#引数はなし　2009,03,37
stop.watch<-function(){
  t<-system.time(readline("Enter で stop"))[3]
  names(t)<-NULL
  print(t)
}
