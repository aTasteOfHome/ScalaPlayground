object FxFun {

	def isSorted[A] (arr: Array[A], ordered: (A,A) => Boolean): Boolean = {
		@annotation.tailrec
		def loop(n: Int): Boolean = {
			if (n >= arr.length) {
				true
			} else if(!ordered(arr(n-1), arr(n))) {
				false
			} else {
				loop(n+1)
			}
		}

		if(arr.length<=1){
			true
		}else{
			loop(1)
		}
	}

	//implementing a curry function that "curries" a function
	//currying converts a function that takes 2 params and outputs a third into a 
	//	new function that takes in 1 param, and returns a function that takes the second param
	//	to return the output.
	def curry[A,B,C] (f: (A,B) => C): A => (B => C) = {
		(a: A) => (b: B) => f(a,b)
	}

	def uncurry[A,B,C] (f: A => B => C): (A,B) => C = {
		(a: A, b: B) => f(a)(b)
	}

	//functions to be used for tests


	def main(args: Array[String]): Unit = { 
    	println("Hello everyone!")

    	def inOrder(x: Int, y: Int) = x<=y
    	println("isSorted test: " + isSorted(Array(1,2,3,4,5,6), inOrder))

    	def curryTest = curry(inOrder)

    	println( "curry test: " + curryTest(2)(3) )

    	def uncurryTest = uncurry(curryTest)

    	println("uncurryTest: " + uncurryTest(2,3))
  	}
}