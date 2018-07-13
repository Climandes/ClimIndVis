#' function to plot data points on map of gridded data
#' @param g_lon,g_lat: arrays of longitudes and latidudes  of dimension nx,ny ( or matrix of dim nx x ny if grid is not regular).
#' @param g_dat: matrix of gridded data of dimension nx x ny
#' @param g_col: color scale for plotting gridded data as character of function
#' @param g_breaks (g_nlev): breaks for plotting gridded data. If not provided, g_nlev needs to be specified to calculate breaks. If g_nlev is provided, the pretty() function is used to define the breakpoints and therefore the number of breaks might differ from the g_nlev.
#'
#' @param p_lon,p_lat: array of longitudes and latitudes of points of dimension np=number of points
#' @param p_dat: array of values for data for points of dimension p (needed for p_col_info = "same" or "cbar")
#' @param p_col_info: type of coloring for points
#' \itemize{
#'  \item "same": same colorbar as for gridded data, p_col and p_nlev do not need to be specified and no extra legend is plotted for points but p_dat is needed
#'  \item "cbar": colorbar specified in p_col but different from g_col. Either predefined colors bars such as these: link to grDevices::Palettes and gplots::colorpanel and RColorBrewer or given in "" (e.g. cbar="gplots::bluered")
#'  \item "cpoints": discrete colors specified for each point as array in p_col (p_dat is not needed in this case)
#'  }
#' @param p_col: colors for points, see p_col_info
#' @param p_breaks (p_nlev): breaks for plotting point data. If p_breaks is not provided, p_nlev needs to be specified to calculate breaks. If p_col_info = "same" or "cpoints" values are taken from g_breaks (g_nlev). If p_nlev is provided, the pretty() function is used to define the breakpoints and therefore the number of breaks might differ from the p_nlev.
#' @param p_pch: pch of the points (see par). Values between 21:25 are implemented, default is 21.
#' @param p_cex: cex of points (number or array), default is 2
#' @param p_legend: legend to be drawn for point data. Default is no legend. Legend is not drawn if p_col_info="same".
#'\itemize{
#'  \item "cbar": colorbar, additional argumten p_nlev or p_breaks is needed
#'  \item "cpoints": legend of points of unique colors of p_col, p_legend_text should be specified
#'  }
#'@param p_pch_s: x and y limits of the plot. If not specified taken from g_lon and g_lat
#'@param xlims,ylims: x and y limits of the plot. If not specified taken from g_lon and g_lat
#'@param mask: plot land-sea mask of data
#'@param mask_col: color of mask values
#'@param fc_plot -> forecast plot, shows probabilities for all ncat forecast categories
# in optional Argumets weitergeben - fpr fc_plot=TRUE
#'@param leg_labs: legend labs (for fc_plot=TRUE)
#'@param fc_catnames: title for categories (for fc_plot=TRUE)
#'@param leg_title title of legend
#' @param output: needs to be specified if file output is wanted, currently only "png" is implemented (default=NA)
#' @param outfile: character, outputfile (only if output is not NULL). final file will be of type outfile.output
#' @param plot_title (optional), character string for plot title
#' @param pwidth width of plotting region of output (as input for jpeg, png, ...). height is calculated in functions based on plot layout
#' @param lwidth width of legend
#' @param plwidth width of legend for points
#' @param topo Information for contour lines added to the plot. Named list with following three elements: \emph{alt}=array of altitudes of dimension nlonXnlat, \emph{lon}: array of longitudes, \emph{lat}: array of latitudes. Default=NULL.
#' @param graphic_device arguments for graphics devices (in addition to <<filename>>, <<width>> and <<height>> ), depending on the output chosen see \code{\link[grDevices]{png}} or \code{\link[grDevices]{pdf}}.
#' @keywords internal


plot_map_grid_points <- function(g_dat=NULL,g_lon=NULL,g_lat=NULL,g_col=rainbow(10),g_col_center=FALSE,g_breaks,g_nlev=10,
  p_lon=NULL,p_lat=NULL,p_col_info="same",p_col=rainbow(10),p_col_center=FALSE,p_dat,p_legend=0,legend_center=FALSE,
  xlims,ylims,zlims=NULL,p_zlims,mask=NULL,mask_NA=NULL,ratio=1.0,p_nlev=10,p_breaks=NA,p_text,p_pch=21,p_cex=1,p_pch_s=NA,p_col_sig,
  output=NULL,outfile,pwidth=5,graphic_device,plot_title,lwidth=1,plwidth=1,trend=FALSE,topo=NULL,text_cex=1,mask_col="lightgrey",NA_col="white",fc_plot=FALSE,outliers=c(FALSE,FALSE),units="",...) {

opargs<-list(...)
  # 1.checks ----------------------------------------------------------------
  grid <- ifelse (missing(g_dat) ,0,1)
  points<-ifelse (missing(p_dat) & missing(p_col),0,1)
  if(grid==0 & points==0) stop("either grid or point data needs to be specified")
  if (grid==1) check_arguments_function(list(g_lon,g_lat))
  if (points==1) check_arguments_function(list(p_lon,p_lat))

  #fehlt: checken ob lons und lats die gleiche Groesse haben

  if(points==1 & p_col_info=="same" & missing(p_dat)) stop("p_dat needs to be specified if <<p_col_info==same>>")

  #check if lons are in same "format" [0,360] vs. [-180,180]
  if (grid==1 & points == 1){
    if(any(p_lon > 180) & any(g_lon < 0)) p_lon <- p_lon-360
    if(any(p_lon < 0) & any(g_lon > 180)) p_lon <- p_lon+360
  }

# default limits from lon and lat if not specified
  if (missing(xlims)) xlims <-range(g_lon,p_lon)
  if (missing(ylims)) ylims <-range(g_lat,p_lat)
  if(grid==1) ylims=ylims+(diff(g_lon)[1]/2 *c(-1,1))

  limcalc <- "quant" # for test purpose and put switch later

# default z-limits
  if (missing(zlims) & grid==1){
    if (points==1 & p_col_info=="same" ) {
      zlims_g <- get_lims(list(g_dat,p_dat))
    } else
      zlims_g <-get_lims(g_dat)
  } else {
    zlims_g=list()
    zlims_g$lim=zlims
  }

  if(points==1){
  if (missing(p_zlims)) {
    if (p_col_info=="same"){
      zlims_p=list()
       zlims_p$lims<-zlims_g
    } else if (p_col_info == "cbar") {
      #p_zlims <-range(p_dat,na.rm=TRUE)
       zlims_p <- get_lims(p_dat)

      if(!all(is.finite(zlims_p$lim))) zlims_p$lim <- c(0,0)
    } # wenn p_col_info="cpoints" braucht man keine p_zlims da diese nur zur Aufteilung der Farbskala gebraucht werden und diese dann in p_col definiert sind
  } else {
    zlims_p=list()
    zlims_p$lim=p_zlims
  }

  # check if all arguments for p_legend are given

  if (p_legend=="cpoints") {
    if(missing(p_pch)) p_pch <- 16
    if (missing(p_text)) {
      message("No text specified for legend points (p_text)")
    }
  }
  }

  # default color scales ----------------------------------------------------
    #grid
  if (grid==1 ) {
    if(missing(g_breaks)){
      g_br = get_breaks(g_dat,zlims_g,g_nlev+1, center=legend_center)
      g_breaks = g_br$breaks
      outliers <- g_br$outliers
    }
    if (g_col_center){
      if (!is.element(0,g_breaks)){
        if (!all(g_breaks>0))neg=tail(which(g_breaks<0),1)
        g_breaks=c(g_breaks[1:neg],0,g_breaks[(neg+1):length(g_breaks)])
      }
      if (all(g_breaks>=0)) {
        t_col=colorRampPalette(g_col)(length(g_breaks)*2-1)[length(g_breaks): (length(g_breaks)*2-1)]
      } else if (all(g_breaks<=0)){
        t_col=colorRampPalette(g_col)(length(g_breaks)*2-1)[1:(length(g_breaks)-1)]
      } else {
        step=diff(zlims_g$lim)/(length(g_breaks)-1)
        nzl=c(max(abs(zlims_g$lim))*c(-1,1))
        g_br_help=pretty(nzl,diff(nzl)/step)
        sel=which(is.element(round(g_br_help,digits=4),round(g_breaks,digits=4)))
        g_col = colorRampPalette(g_col)(length(g_br_help)-1)[head(sel,-1)]
      }
    } else {
      if (length(g_col) != (length(g_breaks)-1)){
      g_col = colorRampPalette(g_col)(length(g_breaks)-1)
      }
    }

    if (outliers[1]) g_dat[g_dat<head(g_breaks,1)]=head(g_breaks,1)
    if (outliers[2]) g_dat[g_dat>tail(g_breaks,1)]=tail(g_breaks,1)

  }
  # points
  if (points==1){
    if (p_col_info == "cpoints") {
      if(length(p_col) != length(p_lon)) stop("length of p_col needs to be of same number as p_lon and p_lat")
      col_points = p_col
    } else {
    if (p_col_info =="same"){
      p_col<-g_col
      p_breaks<-g_breaks
      p_legend=0
    } else if (p_col_info == "cbar"){
      if (is.na(p_breaks)[1]){
        p_br <-get_breaks(p_dat, zlims_p, p_nlev, center=legend_center)
          p_breaks <- p_br$breaks
          outliers <- p_br$outliers
      }
      if (p_col_center){
        if (!is.element(0,p_breaks)){
          if (!all(p_breaks>0))neg=tail(which(p_breaks<0),1)
          p_breaks=c(p_breaks[1:neg],0,p_breaks[(neg+1):length(p_breaks)])
        }
        if (all(p_breaks>=0)) {
          t_col=colorRampPalette(p_col)(length(p_breaks)*2-1)[length(p_breaks): (length(p_breaks)*2-1)]
        } else if (all(p_breaks<=0)){
          t_col=colorRampPalette(p_col)(length(p_breaks)*2-1)[1:(length(p_breaks)-1)]
        } else {
          step=diff(zlims_p$lim)/(length(p_breaks)-1)
          nzl=c(max(abs(zlims_p$lim))*c(-1,1))
          g_br_help=pretty(nzl,diff(nzl)/step)
          sel=which(is.element(round(g_br_help,digits=4),round(p_breaks,digits=4)))
          p_col = colorRampPalette(p_col)(length(g_br_help)-1)[head(sel,-1)]
        }
      } else {
        if (length(p_col) != (length(p_breaks)-1)){
          p_col= colorRampPalette(p_col)(length(p_breaks)-1)
        }
      }
    }


      if (outliers[1]) p_dat[p_dat<head(p_breaks,1)]=head(p_breaks,1)
      if (outliers[2]) p_dat[p_dat>tail(p_breaks,1)]=tail(p_breaks,1)


      if(all(unlist(lapply(p_dat,is.na)))){
        col_points <- NA
      } else{
        col_points <- p_col[cut(p_dat, p_breaks,right=TRUE,include.lowest = TRUE)]
        col_points[is.na(p_dat)]=NA_col
      }
        }

  }

# setup plot and plot -----------------------------------------------------
# calculate aspect ratio of length of lons and lats within x- and ylims
  #if (is.matrix(g_lon)==FALSE){
    nx <- diff(xlims) #length(is.in.range(g_lon,xlims))
    ny <- diff(ylims)  #length(is.in.range(g_lat,ylims))
  #} else {
   # print("not implemented yet for irregular grid")
    #return;}
  ratio<-ny/nx
  if (!is.null(output)){

    if(missing(graphic_device)) graphic_device=list()
    if (missing(outfile)) stop("no outfile specified")
    filename=paste0(outfile,".",output)
    pheight=ifelse(p_legend==0 & !fc_plot, ratio*5/6*pwidth+(pwidth/5),(ratio*5/6+1/6)*pwidth+(pwidth/5))
    if (is.element(output,c("png","jpeg","bmp","tiff"))){
      if(is.null(graphic_device$res)) graphic_device$res=200 # res=pres,units="in"
      if(is.null(graphic_device$units)) graphic_device$units="in"
      do.call(output,c(list(filename=filename,height=pheight,width=pwidth+(pwidth/5)),graphic_device))
    } else if (output == "pdf"){
      do.call(output,c(list(file=filename,height=pheight,width=pwidth+(pwidth/5)),graphic_device))
    } else if (output=="dev.new" ) dev.new()
  }

  if (p_legend==0 & grid==0) {
    layout(mat=matrix(c(1,2),nrow=1,byrow=TRUE),heights=c(ratio*5),widths=c(5),respect=TRUE)
  } else if ((p_legend==0 & grid==1) | (p_legend!=0 & grid==0) | fc_plot) {
    layout(mat=matrix(c(1,2),nrow=1,byrow=TRUE),heights=c(ratio*5),widths=c(5,1),respect=TRUE)
  } else layout(mat=matrix(c(1,2,3,3),nrow=2,byrow=TRUE),heights=c(ratio*5,2),widths=c(5,1))

  # define type of coords
   coords=ifelse(any(xlims> 180), FALSE, TRUE)
   coords=ifelse(any(xlims< 0), TRUE, FALSE)

  #if (fc_plot) { par(oma=c(1,1,5,1),mar=c(1,1,0.5,1))
  #} else {
    ifelse(!all(is.na(plot_title)),
           par(oma=c(1.5,1.5,(length(plot_title)+0.5)*text_cex,1),mar=c(1,1,0.5,1)),
           par(oma=c(1.5,1.5,1,1),mar=c(1,1,1,1)))
    #}




   #muss evtl angepasst werden an Groesse?

# plot gridded data or else generate empty map plot

  if (grid==1){
  image(g_lon,g_lat,g_dat,xlim=xlims, ylim=ylims,zlim=zlims_g$lim,col=g_col,breaks=as.numeric(g_breaks),cex=3*text_cex,xlab="",ylab="",cex.axis=1)

  if(!is.null(mask_NA) ){
     if(!is.null(mask)) mask_NA[mask==1]=NA
      if(!all(is.na(mask_NA))) image(g_lon,g_lat,mask_NA,xlim=xlims, ylim=ylims,col=NA_col,add=TRUE)
  }
   } else {
    image(xlims[1]:xlims[2],ylims[1]:ylims[2],array(0,dim=c(diff(xlims)+1,diff(ylims)+1)),col=NULL,cex=3*text_cex,xlab="",ylab="",cex.axis=1)

  }
   if(!is.null(mask) ){
    if (!all(is.na(mask))) image(g_lon,g_lat,mask,xlim=xlims, ylim=ylims,col=mask_col,add=TRUE)
   }
   maps::map(ifelse(coords==TRUE,'world',"world2"), add=T, lwd=2)
   maps::map('lakes', add=T, lwd=2)
  #plot a grid on top of the plot
  all_lons <- seq(xlims[1],xlims[2],1)
  all_lats <- seq(ylims[1],ylims[2],1)

  abline(v= all_lons[which((all_lons %% 5)==0)], lwd=1, lty=2, col="gray66")
  abline(h= all_lats[which((all_lats %% 5)==0)], lwd=1, lty=2, col="gray66")

  box()

  if (!is.null(topo)){
    contour(topo$lon, topo$lat, topo$alt, nlevels=3, col="gray55", add=TRUE)
  }


#plot points
  if (points==1){
  points(p_lon,p_lat,col="black",bg=col_points,pch=p_pch,cex=p_cex,lwd=1)
    }


  # plot title
  if (!all(is.na(plot_title))){
    plot_title=plot_title[!sapply(plot_title,is.null)]
    for(ll in 1:length(plot_title)){
      mtext(side=3,line=((length(plot_title)-0.5):0.5)[ll]*0.6*text_cex, plot_title[[ll]], adj=ifelse(names(plot_title)[ll]=="utitle",NA,0), cex=ifelse(ll==1,1,0.6)*text_cex)
    } }

  test<-par()
  par(fig=c(0.8,0.99,0.01,0.95),pty="m",new=TRUE)

  if (grid == 1 | p_col_info=="same"){
    if(!length(g_breaks)==1){
      if(fc_plot){
        image_scale(breaks=g_breaks,col=g_col,axis.pos=4,xlab=opargs$leg_labs,
                    add.axis=FALSE,fc_axis=opargs$fc_catnames,equidist=TRUE)

      } else {
        image_scale(breaks=g_breaks,col=g_col,axis.pos=4,scale_lab=units, add.axis=TRUE,key.extend=outliers)
      }
    }

  }

 #legend
  if(p_legend!=0){
    if (p_legend == "cbar" & fc_plot){
      image_scale(breaks=p_breaks,col=p_col,axis.pos=4,xlab=opargs$leg_labs,
        add.axis=FALSE,fc_axis=opargs$fc_catnames,equidist=TRUE)
    } else {
      if (grid==1) par(fig=c(0,1,0,test$fig[3]),new=TRUE)

      if (p_legend == "cbar") {
        if(!length(p_breaks)==1){
          horiz= ifelse(grid==0,4,1)
          if (horiz==1)  {
            par(mar=c(2,1,1,1))
          } else par(mar=c(1,1,1,2))
          image_scale(breaks=p_breaks,col=p_col,axis.pos=horiz,scale_lab=units, add.axis=TRUE,key.extend=outliers)

        }

      }else if (p_legend == "cpoints") {
        plot(0,type="n",axes=FALSE)
        legend("topleft",pch=p_pch,legend=p_text,col=unique(p_col),cex=1,bty="n")

      }
    }
  }

if (!is.null(output)){
  if (output!="dev.new") dev.off()
}

} #end function


