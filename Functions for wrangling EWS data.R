inter.plot <- function(var){
  model = glm(Event6Hr~ews_grp[[var]]+ews_grp[["Group"]]+ews_grp[[var]]:ews_grp[["Group"]],data = ews_grp,family = 'binomial')
  p.val = round(summary(model)$coefficients[4,4],digits = 5)
  if(length(levels(ews_grp[[var]]))==2){
    l.orl = coef(model)[2]
    l.orh = coef(model)[2]+coef(model)[4]
    s.err1 = compute_std(model = model,i=1,j=3)
    s.err2 = compute_std(model = model,i=1)
    df = data.frame(l.odds = c(l.orh,l.orl),
                    grp = c("High","Low"),
                    se =c(s.err1,s.err2) )
    
    g<- ggplot(df ,aes(x = grp,y = exp(l.odds)))+
      geom_point(aes(color = grp),size = 4)+
      geom_errorbar(aes(ymin = exp(l.odds-1.96*se),ymax = exp(l.odds + 1.96*se)),width = .05)+
      coord_trans(y = "log")+
      labs(x = "Group",y = "Odds Ratio",title = var)+
      geom_hline(yintercept = 1)+
      annotate("text",label=paste("p-value =",p.val),Inf,Inf,hjust=1,vjust= 2,size=3.5)
    
    return(g)
  }
  else if(length(levels(ews_grp[[var]]))==3){
    l.orh1 = coef(model)[2]+coef(model)[5]
    l.orh2 = coef(model)[3]+coef(model)[6]
    l.orl1 = coef(model)[2]
    l.orl2 = coef(model)[3]
    s.err1 =compute_std(model = model,i=1,j=4)
    s.err2 =compute_std(model = model,i=2,j=5)
    s.err3 =compute_std(model = model,i=1)
    s.err4 =compute_std(model = model,i=2)
    class.1 = levels(ews_grp[[var]])[2]
    class.2 = levels(ews_grp[[var]])[3]
    p.val1 = round(summary(model)$coefficients[5,4],digits = 5)
    p.val2 = round(summary(model)$coefficient[6,4],digits = 5)
    df = data.frame(l.odds = c(l.orh1,l.orh2,l.orl1,l.orl2),
                    grp = c(rep("High",2),rep("Low",2)),
                    se =c(s.err1,s.err2,s.err3,s.err4),
                    class = c(class.1,class.2,class.1,class.2))
    
    g <- ggplot(df ,aes(x = grp,y = exp(l.odds),group=class))+
      geom_point(aes(colour = class),position = position_dodge(width = 0.3), size = 4)+
      geom_errorbar(aes(x=grp,ymin = exp(l.odds-1.96*se),ymax = exp(l.odds + 1.96*se)),position = position_dodge(width = 0.3),width = 0.1)+
      coord_trans(y = "log")+
      labs(x = "Group",y = "Odds Ratio",title = var)+
      geom_hline(yintercept = 1)+
      annotate("text",label = paste("p-value for",levels(ews_grp[[var]])[2],"=",p.val1),Inf,Inf,hjust = 1,vjust = 2,size=3.5)+
      annotate("text",label = paste("p-value for",levels(ews_grp[[var]])[3],"=",p.val2),Inf,Inf,hjust = 1,vjust = 4,size=3.5)
    
    return(g)
  }
  else{
    g<- ggplot(ews_grp,aes(x = ews_grp[[var]], y = Event6Hr, group = Group))+
      geom_smooth(aes(colour = Group),method = "glm",se = TRUE)+
      labs(x = var,y = "Odds",title = var)+
      annotate("text",label=paste("p-value =",p.val),Inf,Inf,hjust=1,vjust= 2,size=3.5)
    return(g)
  }
}



compute_std <- function(model,i,j = 0){
  smry = summary(model)
  beta = smry$coefficient[,1]
  std = smry$coefficient[,2]
  s.variance = `^`(std,2)
  co.variance = s.variance[i+1]+s.variance[j+1]+2*smry$cov.unscaled[i+1,j+1]
  se = ifelse(j == 0,std[i+1],sqrt(co.variance))
  return(se)
}


creat_tb <- function(var){
  model = glm(Event6Hr~ews_grp[[var]]+ews_grp[["Group"]]+ews_grp[[var]]:ews_grp[["Group"]],data = ews_grp,family = 'binomial')
  if(length(levels(ews_grp[[var]]))==2){
    l.orl = coef(model)[2]
    l.orh = coef(model)[2]+coef(model)[4]
    tb = matrix(nrow = 1,ncol = 2)
    colnames(tb) = c("High","Low")
    rownames(tb) <- paste(var,levels(ews_grp_reord[[var]])[2])
    tb[1,1] = l.orl
    tb[1,2] = l.orh
  }
  if(length(levels(ews_grp[[var]]))==3){
    l.orh1 = coef(model)[2]+coef(model)[5]
    l.orh2 = coef(model)[3]+coef(model)[6]
    l.orl1 = coef(model)[2]
    l.orl2 = coef(model)[3]
    tb = matrix(nrow = 2,ncol = 2)
    colnames(tb) = c("High","Low")
    rownames(tb) <- c(paste(var,levels(ews_grp_reord[[var]])[2]),paste(var,levels(ews_grp_reord[[var]])[3]))
    tb[1,1] = l.orh1
    tb[1,2] = l.orl1
    tb[2,1] = l.orh2
    tb[2,2] = l.orl2
  }
  return(exp(tb))
}