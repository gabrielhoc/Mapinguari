unformula<-function(formula){
  i<-as.list(formula)
  j<-rapply(i,as.list,how="replace",classes='call')
  if(identical(i,j)){
    return(i)
  } else {
    unlist(formula_tree(j))
  }
}

for(i in 1:length(as.list(formula3)[[3]])){print(as.list(formula4[[3]][i]))}

unformula2<-function(formula){

variables_vector<-unlist(strsplit(toString(formula),"~",fixed=T))
response_var<-variables_vector[1]
independent_vars<-unlist(strsplit(variables_vector[2],"+",fixed=T))

smooth_index<-grep("s\\(|te\\(|ti\\(|t2\\(", independent_vars)
random_index<-grep("\\|", independent_vars)

smooth_vars<-independent_vars[smooth_index]
random_vars<-independent_vars[random_index]
linear_vars<-independent_vars[-c(smooth_index,random_index)]

#smooth_vars_names<-all.vars(formula)[smooth_index]
#random_vars_names<-all.vars(formula)[random_index]
#linear_vars_names<-all.vars(formula)[-c(smooth_index,random_index)]

return(list(response=response_var,
            smooth=smooth_vars,
            random=random_vars,
            linear=linear_vars))

}