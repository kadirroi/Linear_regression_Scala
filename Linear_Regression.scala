package se
//linear regression two independent variables

object Linear_Regression {

  def main(args: Array[String]): Unit = {

    val list_points = List((1.0, 2.0), (2.0, 1.0),(4.0,3.0))
    val linear_Regression= new LinearRegression(list_points)
    linear_Regression.calc_m()

  }

}

class LinearRegression(list: List[(Double, Double)]) {
  var this.list = list
  var mean_x:Double = 0
  var mean_y:Double=0
  var mean_x_y:Double=0
  var x:Double=0
  var len=list.length
  var m:Double=0
  var b:Double=0
  println("Len : "+len)
  def calc_m(): Unit =
    {
      list.foreach { i => mean_x += i._1 
        mean_y+=i._2
        mean_x_y+=i._1*i._2
        x+=i._1*i._1
      }
      mean_x=mean_x/len
      println("Mean x: "+mean_x)
      mean_y=mean_y/len
      println("Mean y: "+mean_y)
      mean_x_y=mean_x_y/len
      println("Mean xy: "+mean_x_y)
      m=(mean_x_y-(mean_x*mean_y))/((x/len)-(mean_x*mean_x))
      println("m: "+m)
      b=mean_y-(m*mean_x)
      println("b: "+b)
      println("x :"+x)
      println("-----------Equation---------")
      println("y="+m+"x+"+b)
    }
 

}