object FibonacciFx {
	def fib(n: Int): Int = {
		@annotation.tailrec
		def fibHelper(prev: Int, cur: Int, stepsLeft: Int): Int = {
			if(prev < 0 || cur < 0 || (cur==0 && prev==0)){
				-1
			}else if(stepsLeft <= 0){
				cur
			} else {
				fibHelper(cur, cur + prev, stepsLeft - 1)
			}
		}
		
		fibHelper(0, 1, n-1)
	}

	def main(args: Array[String]): Unit = {  
    	println(fib(5));
  	}
}