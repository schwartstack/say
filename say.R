say<-function(n){
  if(n==0){
    return("zero")
  }else{
    if(n<0){
      string="negative"
      n=-1*n
    }else{
      string=""
    }
    scale=c("","thousand","million","billion","trillion","quadrillion","quintillion","sextillion","septillion","octillion","nonillion","decillion","undecillion","duodecillion","tredecillion","quattuordecillion","quindecillion","sexdecillion","septendecillion","octodecillion","novemdecillion","vigintillion")
    tens=c("ten","twenty","thirty","fourty","fifty","sixty","seventy","eighty","ninety")
    ones=c("one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen")
    
    s=as.character(n)
    v=strsplit(s,"")[[1]]
    v=as.numeric(v)
    
    if(length(v)%%3!=0){
      while(length(v)%%3!=0){
        v=c(0,v)
      }
    }
    m=matrix(v,ncol=3,byrow=T)
    
    for(r in 1:nrow(m)){
      if(m[r,1]>0){
        string=paste(string,ones[m[r,1]],"hundred")
      }
      if(m[r,2]>1){
        string=paste(string,tens[m[r,2]],ones[m[r,3]])
      }else{
        string=paste(string,ones[10*m[r,2]+m[r,3]])
      }
      string=paste(string,rev(scale[1:nrow(m)])[r])
    }
    return(trimws(string))
  }
}