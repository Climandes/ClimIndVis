




plot_tacho<-function(filename,pos,perc_cat,colors=colors,skill=NULL,skillmin,noskill,maxskill,breaks,r1=0,r3=1.1,line.ens=FALSE,pos.ens=NA,tacholabs,lab.place,scale.labs=NA,scale.place=NA,message_text=NULL,title=""){

if (missing (lab.place)) lab.place <- (breaks[1:(length(breaks)-1)] + breaks[2:(length(breaks))])/2
if (missing (tacholabs)) tacholabs=1:(length(breaks)-1)
if (missing(skillmin)) skillmin=noskill
if( is.na(skill)) skill=noskill
if (skill <= skillmin) {
  skill=noskill
  colors2=paste0(colors,"30")
} else colors2=paste0(colors,"80")
  p <- ggplot2::ggplot()
  for (i in 1:(length(breaks)-1)){
  # p <- p + geom_polygon(data=get.poly(breaks[i],breaks[i+1], r1),aes(x,y),fill=colors[i])
  r2<-perc_cat[i]/100
  p <- p + ggplot2::geom_polygon(data=get.poly(breaks[i],breaks[i+1], r1,r2=1),ggplot2::aes(x,y),fill=NA,colour=colors[i])
  p <- p + ggplot2::geom_polygon(data=get.poly(breaks[i],breaks[i+1], r1,r2),ggplot2::aes(x,y),fill=colors2[i],colour=colors2[i])
  }

  if (line.ens==TRUE){
    if (all(is.na(pos.ens))==FALSE){
      p <- p + geom_segment(ggplot2::aes(x = 0, y = 0, xend = (skill-noskill)/(maxskill-noskill)*cos(pi*(1-pos.ens/100)), yend = (skill-noskill)/(maxskill-noskill)*sin(pi*(1-pos.ens/100))), arrow = ggplot2::arrow(length = grid::unit(0.2, "cm")),
        col = "grey35", lwd = 0.3)
    }}

 if(skill>noskill & pos>0){
  p <- p + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = (skill-noskill)/(maxskill-noskill)*cos(pi*(1-pos/100)), yend =  (skill-noskill)/(maxskill-noskill)*sin(pi*(1-pos/100))),
    arrow = ggplot2::arrow(length = grid::unit(0.5, "cm")), lwd = 1, alpha = 1, col = "black")
 }

  p <- p +  ggplot2::geom_text(data=as.data.frame(tacholabs), size=3, fontface="bold", vjust=0,
                      ggplot2::aes(x=(r3+0.2)*cos(pi*(1-lab.place/100)),y=r3*sin(pi*(1-lab.place/100))), label = tacholabs)
  if (all(is.na(scale.labs))==FALSE) {
    if (missing (scale.place)) scale.place <-seq(0,100,100/(length(scale.labs)-1))
  p <- p +  ggplot2::geom_text(data=as.data.frame(scale.labs), size=3, fontface="bold", vjust=0,
    ggplot2::aes(x=r3*cos(pi*(1-scale.place/100)),y=r3*sin(pi*(1-scale.place/100))), label = scale.labs)
  }


  if (!all(is.na(title))){
    title=title[!sapply(title,is.null)]
    for(ll in 1:length(title)){
      p <- p +  ggplot2::annotate("text",x=ifelse(ll==1,0,-1.2),y=1.4-(ll-1)*0.05,label=title[[ll]],size=ifelse(ll==1,3,2), hjust= ifelse(ll==1,0.5,0), fontface=ifelse(ll==1,"bold","plain"))
    } }

  if( !is.null(message_text)){
    lt<-nchar(message_text)
  p <- p +  ggplot2::annotate("label",x=0,y=0.5,label=message_text,size=100/lt,label.padding=grid::unit(1.5,"lines"))
  }



  #@annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
  p <- p +   ggplot2::coord_fixed()
  p <- p +   ggplot2::theme_bw()
  p <- p +   ggplot2::theme(axis.text=ggplot2::element_blank(),
                   axis.title=ggplot2::element_blank(),
                   axis.ticks=ggplot2::element_blank(),
                   panel.grid=ggplot2::element_blank(),
                   panel.border=ggplot2::element_blank())
  p

if (filename==""){
  dev.new()
 print(p)
} else {
  ggplot2::ggsave(filename, plot = ggplot2::last_plot(),width= par("din")[1],height= par("din")[2]-3 )
}



}


get.poly <- function(a,b,r1,r2=1) {
  th.start <- pi*(1-a/100)
  th.end   <- pi*(1-b/100)
  th       <- seq(th.start,th.end,length=100)
  x        <- c(r1*cos(th),rev(r2*cos(th)))
  y        <- c(r1*sin(th),rev(r2*sin(th)))
  return(data.frame(x,y))
}
