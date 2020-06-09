#' @title Run CHDWILSON Model
#' @description This function calls the CHDWILSON model.
#' @param model_input A list/json object with gender, age,
#' TChol, LDL,HDL,SBP,DBP,diabetes and smoker
#' @return Return a dataset with two columns: CHDprediction_use_TChol, CHDprediction_use_ldl

model_run<-function(model_input = NULL)
{

  input<-unflatten_list(model_input)
  results <- predictchd (age       = model_input$age,
                         gender    = model_input$gender,
                         TChol     = model_input$TChol,
                         LDL       = model_input$LDL,
                         HDL       = model_input$HDL,
                         SBP       = model_input$SBP,
                         DBP       = model_input$DBP,
                         diabetes  = model_input$diabetes,
                         smoker    = model_input$smoker)
  return(as.list(results))
}


get_default_input <- function() {
  model_input <- list(age         = 55,
                      gender      = 1,
                      TChol       = 250,
                      LDL         = 120,
                      HDL         = 39,
                      SBP         = 146,
                      DBP         = 88,
                      diabetes    = 0,
                      smoker      =1
  )
  return((flatten_list(model_input)))
}


#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }

  for(i in 1:length(lst))
  {
    nm<-names(lst[i])

    message(nm)

    if(prefix!="")  nm<-paste(prefix,nm,sep=".")

    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}



#Gets a hierarchical named list and flattens it; updating names accordingly
unflatten_list<-function(lst)
{
  if(is.null(lst)) return(lst)
  out<-list()

  nms<-names(lst)

  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }

  return(out)
}
