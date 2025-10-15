# Tools to compute contextual similarity between patents and PRODCO codes.
# Provides an OpenAI embedding-based likelihood estimator and a doc2vec fallback.

library(doc2vec)
library(stringi)
library(httr)
library(jsonlite)

default_preprocess <- function(text) {
  text <- iconv(text, from = "", to = "UTF-8", sub = "")
  text <- stringi::stri_trans_tolower(text)
  text <- stringi::stri_replace_all_regex(text, "[[:punct:]]+", " ")
  text <- stringi::stri_replace_all_regex(text, "\\s+", " ")
  stringi::stri_trim_both(text)
}

compose_text_fields <- function(df, text_cols = NULL, code_col = NULL, include_code = FALSE) {
  df_local <- as.data.frame(df, stringsAsFactors = FALSE)

  if (!is.null(text_cols)) {
    missing_cols <- setdiff(text_cols, names(df_local))
    if (length(missing_cols) > 0) {
      stop(
        sprintf(
          "The following text columns are missing: %s",
          paste(missing_cols, collapse = ", ")
        )
      )
    }
  } else {
    text_cols <- names(df_local)[vapply(df_local, function(x) is.character(x) || is.factor(x), logical(1))]
  }

  text_cols <- text_cols[text_cols %in% names(df_local)]

  if (length(text_cols) == 0) {
    texts <- rep("", nrow(df_local))
  } else {
    texts <- apply(df_local[, text_cols, drop = FALSE], 1, function(row) {
      parts <- trimws(vapply(row, function(x) {
        if (is.na(x)) return("")
        stringi::stri_trans_general(as.character(x), "Any-Latin; Latin-ASCII")
      }, character(1)))
      parts <- parts[!is.na(parts) & nzchar(parts)]
      paste(parts, collapse = ". ")
    })
  }

  if (include_code && !is.null(code_col) && code_col %in% names(df_local)) {
    codes <- as.character(df_local[[code_col]])
    codes[is.na(codes)] <- ""
    prefix <- ifelse(nzchar(codes), paste0("PRODCOM code ", codes, ": "), "")
    texts <- trimws(paste0(prefix, texts))
  }

  texts[texts == ""] <- NA_character_
  texts
}

softmax_rows <- function(mat, temperature = 1) {
  if (!is.matrix(mat)) mat <- as.matrix(mat)
  if (nrow(mat) == 0 || ncol(mat) == 0) return(mat)
  if (temperature <= 0) stop("temperature must be positive.")

  scaled <- mat / temperature
  row_max <- apply(scaled, 1, function(x) if (all(is.na(x))) 0 else max(x, na.rm = TRUE))
  stable <- sweep(scaled, 1, row_max, FUN = "-")
  exp_mat <- exp(stable)
  exp_mat[!is.finite(exp_mat)] <- 0

  row_sum <- rowSums(exp_mat)
  zero_rows <- row_sum == 0
  if (any(zero_rows)) {
    exp_mat[zero_rows, ] <- 1
    row_sum[zero_rows] <- ncol(exp_mat)
  }

  exp_mat / row_sum
}

cosine_similarity <- function(a, b) {
  if (!is.matrix(a)) a <- as.matrix(a)
  if (!is.matrix(b)) b <- as.matrix(b)
  if (!is.numeric(a) || !is.numeric(b)) stop("Inputs must be numeric matrices.")

  a_norm <- sqrt(rowSums(a^2))
  b_norm <- sqrt(rowSums(b^2))
  a_norm[a_norm == 0] <- NA_real_
  b_norm[b_norm == 0] <- NA_real_

  sim <- a %*% t(b)
  sim <- sim / (a_norm %*% t(b_norm))
  sim[is.na(sim)] <- 0
  sim
}

openai_embed_texts <- function(
    texts,
    model = "text-embedding-3-large",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    batch_size = 16,
    max_retries = 3,
    sleep = 1,
    verbose = TRUE) {
  if (!nzchar(api_key)) stop("OpenAI API key not found. Set OPENAI_API_KEY in your environment.")

  texts <- as.character(texts)
  texts[is.na(texts)] <- "Description unavailable."
  n <- length(texts)
  if (n == 0) return(matrix(numeric(0), nrow = 0))

  embeddings <- vector("list", n)
  idx <- 1

  while (idx <= n) {
    end_idx <- min(idx + batch_size - 1L, n)
    chunk <- texts[idx:end_idx]

    attempt <- 1
    repeat {
      response <- tryCatch(
        POST(
          url = "https://api.openai.com/v1/embeddings",
          add_headers(
            Authorization = paste("Bearer", api_key),
            `Content-Type` = "application/json"
          ),
          body = list(
            model = model,
            input = chunk,
            encoding_format = "float"
          ),
          encode = "json",
          timeout(60)
        ),
        error = function(e) e
      )

      if (inherits(response, "error")) {
        if (attempt >= max_retries) stop("Failed to fetch embeddings: ", response$message)
      } else if (response$status_code >= 200 && response$status_code < 300) {
        break
      } else if (response$status_code >= 500 && attempt < max_retries) {
        if (verbose) message("OpenAI API error ", response$status_code, ", retrying...")
      } else {
        stop("OpenAI API request failed: ", content(response, "text", encoding = "UTF-8"))
      }

      attempt <- attempt + 1L
      Sys.sleep(sleep * attempt)
    }

    parsed <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    batch_embeddings <- lapply(parsed$data, function(item) as.numeric(item$embedding))

    if (verbose) {
      usage <- parsed$usage
      if (!is.null(usage)) {
        message(
          sprintf(
            "Embedded %d texts (prompt_tokens: %s)",
            length(batch_embeddings),
            usage$prompt_tokens
          )
        )
      }
    }

    embeddings[idx:end_idx] <- batch_embeddings
    idx <- end_idx + 1L
    if (idx <= n) Sys.sleep(sleep)
  }

  do.call(rbind, embeddings)
}

compute_patent_prodcom_likelihood <- function(
    patents,
    prodcom,
    method = c("openai_embeddings", "doc2vec"),
    patent_text_cols = c("title", "abstract"),
    prodcom_text_cols = NULL,
    code_col = "code",
    openai_model = "text-embedding-3-large",
    temperature = 0.07,
    batch_size = 16,
    max_retries = 3,
    sleep = 1,
    verbose = TRUE,
    doc2vec_dim = 100,
    doc2vec_iter = 30) {
  method <- match.arg(method)

  if (!is.data.frame(patents)) stop("patents must be a data.frame or data.table")
  if (!is.data.frame(prodcom)) stop("prodcom must be a data.frame or data.table")

  patent_texts <- compose_text_fields(patents, patent_text_cols, include_code = FALSE)
  prodcom_texts <- compose_text_fields(prodcom, prodcom_text_cols, code_col = code_col, include_code = TRUE)

  patent_ids <- if ("patent_id" %in% names(patents)) as.character(patents$patent_id) else sprintf("patent_%s", seq_len(nrow(patents)))
  prodcom_ids <- if (!is.null(code_col) && code_col %in% names(prodcom)) as.character(prodcom[[code_col]]) else sprintf("code_%s", seq_len(nrow(prodcom)))

  if (method == "openai_embeddings") {
    patent_emb <- openai_embed_texts(
      texts = patent_texts,
      model = openai_model,
      batch_size = batch_size,
      max_retries = max_retries,
      sleep = sleep,
      verbose = verbose
    )

    prodcom_emb <- openai_embed_texts(
      texts = prodcom_texts,
      model = openai_model,
      batch_size = batch_size,
      max_retries = max_retries,
      sleep = sleep,
      verbose = verbose
    )
  } else {
    doc_ids <- c(
      patent_ids,
      sprintf("code_%s", prodcom_ids)
    )

    texts <- c(patent_texts, prodcom_texts)
    texts[is.na(texts)] <- ""
    texts_clean <- vapply(texts, default_preprocess, FUN.VALUE = character(1), USE.NAMES = FALSE)

    word_counts <- stringi::stri_count_words(texts_clean)
    valid <- word_counts > 0 & word_counts < 1000

    if (!all(valid)) {
      doc_ids <- doc_ids[valid]
      texts_clean <- texts_clean[valid]
    }

    doc_data <- data.frame(doc_id = doc_ids, text = texts_clean, stringsAsFactors = FALSE)

    model <- paragraph2vec(
      x = doc_data,
      type = "PV-DBOW",
      dim = doc2vec_dim,
      iter = doc2vec_iter,
      min_count = 2,
      lr = 0.05,
      threads = 2
    )

    embeddings <- as.matrix(model, which = "docs")
    n_pat <- length(patent_ids)
    patent_emb <- embeddings[1:n_pat, , drop = FALSE]
    prodcom_emb <- embeddings[(n_pat + 1):nrow(embeddings), , drop = FALSE]
  }

  similarity <- cosine_similarity(patent_emb, prodcom_emb)
  likelihood <- softmax_rows(similarity, temperature = temperature)

  rownames(likelihood) <- patent_ids
  colnames(likelihood) <- prodcom_ids

  attr(likelihood, "similarity") <- similarity
  likelihood
}
