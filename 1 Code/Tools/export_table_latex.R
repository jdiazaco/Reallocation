export_table_latex<-function(table, output_dir, output_name, auto=F){
  capture.output(print(xtable(table, auto=auto), include.rownames=F, include.colnames=T),
                 file=paste0(output_dir, output_name))
}
