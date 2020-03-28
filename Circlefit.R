
#test data
#x_x<-c(0,0.5, 1, 1.5, 2, 2.5, 3)
#y_x<-c(0,.25, 1, 2.25, 4, 6.25, 9)
#df<-as.data.frame(cbind(x_x, y_x))


circlefit<-function (df){
  names(df)<-c("X", "Y")
  
  #find mean x y so we can cacluate the difference of each X, Y from its respective         mean
  xmean<-mean(df$X)
  ymean<-mean(df$Y)
  #adds needed columns for summations required to perform least squares
  mat2<-df%>%
    mutate(a=X-xmean)%>%
    mutate(b=Y-ymean)%>%
    mutate(aa=a^2)%>%
    mutate(ab=a*b)%>%
    mutate(bb=b^2)%>%
    mutate(aaa=a^3)%>%
    mutate(abb=a*b^2)%>%
    mutate(baa=b*a^2)%>%
    mutate(bbb=b^3)
  #column sums for construction of linear system of equations
  Saa<-sum(mat2$aa)
  Sab<-sum(mat2$ab)
  Sbb<-sum(mat2$bb)
  Saaa<-sum(mat2$aaa)
  Sbbb<-sum(mat2$bbb)
  Sabb<-sum(mat2$abb)
  Sbaa<-sum(mat2$baa)
  #linear stystem of equations
  sums_row1<-c(Saa, Sab)
  sums_row2<-c(Sab, Sbb)
  
  sum_mat<-as.matrix(rbind(sums_row1, sums_row2), nrow=2)
  #gauss elimination ratio
  gauss_ratio<-sum_mat[1,2]/sum_mat[1,1]
  #new eliminated row
  elim_row2<-sums_row2-(sums_row1*gauss_ratio)
  
  #initial (A,B)
  Ac<-0.5*(Saaa+Sabb)
  Bc<-0.5*(Sbbb+Sbaa)
  
  
  #result of Bc after elimination
  elim_Bc<-Bc-(gauss_ratio*Ac)
  
  #final deviation of (A, B) from mean
  fin_Bc<-elim_Bc/elim_row2[2]
  fin_Ac<-(Ac-(fin_Bc*sum_mat[1,2]))/sum_mat[1,1]
  
  #center of least squares fit of circle (xc,yc)
  Xc<-xmean+fin_Ac
  yc<-ymean+fin_Bc
  
  alpha<-fin_Ac^2+fin_Bc^2+((Saa+Sbb)/nrow(mat2))
  
  #radius of circle
  radius<-sqrt(alpha)
  
  #temporarily stores circle parameters, names them and then puts them in the globalEnv
  circle_parms<-c(Xc, yc, radius)
  names(circle_parms)<-c("Xc", "Yc", "Radius")
  circle_parms<<-circle_parms
  
  #generates a ggplot of the the input data and the approximated circle; puts plot in the globalEnv as circleplot
  circleplot<<-ggplot(df, aes(x=X, y=Y))+geom_point()+
    geom_point(aes(x=Xc, y=yc), color="Red", size=4)+theme(aspect.ratio = 1)
  
  #defines a circle fit function such that it can be added to the circleplot above; this function is available in globalEnv
  gg_circle <<- function(r, xc, yc, color="blue", fill=NA, ...) {
    x <- xc + r*cos(seq(0, pi, length.out=100))
    ymax <- yc + r*sin(seq(0, pi, length.out=100))
    ymin <- yc + r*sin(seq(0, -pi, length.out=100))
    annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
  }
  #adds the fit circle to the data.
  circleplot+gg_circle(r=radius, xc=Xc, yc=yc)
} 