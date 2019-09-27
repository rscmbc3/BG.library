#'@title makeLayout
#'@description creates the character string to execute for plot_ly layout \\cr \\cr
#'@param xDomain character string to exectute setting the domain of xaxis (format, "domain = c(0,1),")
#'@param xaxisStr character string to execute to set specifications for xaxis
#'@param yaxisStr character string to execute to set specifications for y axes
#'@return `layoutStr` character string to execute for plot_ly layout

makeLayout<-function(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange){
  if (addGoodRange){
    shapeStr<-"shapes = list(list(type = 'rect',
                                fillcolor = 'green',
                                line = list(color = 'green'),
                                opacity = 0.2,
                                x0 = 0, x1 = 23,
                                y0 = 80, y1 = 150))"
  }else{
    shapeStr<-"shapes = list()"
  }
  

  
  ##make layoutstr
  layoutStr<-paste0("p <- p %>% layout( showlegend=T, legend = list(orientation = 'h',   # show entries horizontally
                    xanchor = 'center',  # use center of legend as anchor
                    x = 0.5,
                    y = legendInset),",
                    xaxisStr,",",
                    yaxisStr,",",
                    shapeStr,",
                    margin = list(r = 100),
                    title = list(text = '",titleStr,"'))")
  return(layoutStr)
}