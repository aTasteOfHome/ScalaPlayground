object FxDataStructs {

	sealed trait myList[+A]	
	case object Nil extends myList[Nothing]
	case class Cons[+A](head: A, tail: myList[A]) extends myList[A]

	object myList {

		def apply[A](elems: A*): myList[A] = {
			
			if(elems.length <= 0){
				Nil
			}else{
				@annotation.tailrec
				var list: myList[A] = Cons(elems(0), myList.apply(elems.slice(1,elems.length): _*))
				list	
			}
		}

		def sum(vals: myList[Int]): Int = vals match {
			case Nil => 0
			case Cons(head, tail) => head + sum(tail)
		}

		def tail[A](list: myList[A]): myList[A] = list match {
			case Cons(h, t) => t
			case _ => Nil
		}

		def setHead[A](a: A, list: myList[A]): myList[A] = list match {
			case Nil => Cons(a, Nil)
			case Cons(h, t) => Cons(a, t)
		}

		def drop[A](n: Int, list: myList[A]): myList[A] = {
			if(n<=0){
				list
			} else list match {
				case Nil => list
				case Cons(h, t) => drop(n-1, t)
			}
		}

		def dropWhile[A](list: myList[A], f: A => Boolean): myList[A] = list match{
			case Nil => Nil
			case Cons(h,t) => {
				if(f(h)){
					dropWhile(t, f)
				} else {
					list
				}
			}
		}

		def toString[A](list: myList[A]): String = {

			def genString[A](l: myList[A]): String = l match {
				case Cons(h, Nil) => h + ""
				case Nil => ""
				case Cons(h, t) => h + ", " + genString(t)
			}	
			"(" + genString(list) + ")"
		}
	}
	def main(args: Array[String]): Unit = { 

		def printAll(strings: String*) = {
		  strings.foreach(println)
		}

		var list : myList[Int] = myList(0,1,2,3,4,5,6,7,8,9)
    	println("Hello everyone!")

    	println("sum test: "+ myList.sum(list))
    	println("toString: " + myList.toString(list));

    	println("tail: " + myList.toString(myList.tail(list)))
    	println("setHead: " + myList.toString(myList.setHead(32,list)))

    	println("drop: " + myList.toString(myList.drop(4,list)))

    	println("dropWhile: " 
    		+ myList.toString(
    			myList.dropWhile(list, (i: Int) => i<=6)
    		)
    	)
  	}
}