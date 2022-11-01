import Array._
import scala.annotation.tailrec
import scala.util.control.Breaks._

object App {

  def task_one (): Unit = {
    var phrase: String = "Hello, Scala!"
    // Task 1 - print Hello, Scala!
    println(phrase)
    // Task 2 - reverse and print initial phrase
    println(phrase.reverse)
    // Task 3 - to lower
    phrase = phrase.toLowerCase()
    println(phrase)
    // task 4 - remove symbol '!'
    phrase = phrase.replaceAll("!", "")
    println(phrase)
    // task 5 add 'and goodbye python!' to tail
    phrase = phrase + " and goodbye python!"
    println(phrase)
  }

  class EmployeeDB() {
    var data = new Array[Double](0)

    def addEmployeeSalary(salary: Double): Unit = {
      data = concat(data, Array(salary))
    }

    def currentSalary(): Array[Double] = {
      data
    }

    def mediumSalary(): Double = {
      var result: Double = 0


      for (x <- data) {
        result += x
      }

      result / data.length
    }

    def percentageDeviation(): Array[Double] = {
      val medium = mediumSalary()
      val onePercent = medium / 100
      val result = new Array[Double](data.length)
      var idx = 0
      for (x <- data) {
        val delta = x - medium
        result(idx) = delta / onePercent
        idx = idx + 1
      }
      result
    }

    def correctSalary(employeeIdx: Int, delta: Double): Unit = {
      data(employeeIdx) += delta
    }

    def salary(employeeIdx: Int): Double = {
      data(employeeIdx)
    }

    def sortedSalary(): Array[Double] = {
      data.sorted
    }

    def middleIndexes(from : Double, to: Double) : Array[Int] = {
      var res = Array[Int]()
      for (idx <- data.indices) {
        if (data(idx) >= from && data(idx) <= to) {
          res = res :+ idx
        }
      }
      res
    }

    def indexSalary(percent : Double): Unit = {
      for (idx <- data.indices) {
        data(idx) += data(idx) / 100 * percent
      }
    }
  }

  def printArray[A](arr: Array[A]): Unit = {
    var first = true
    for (x <- arr) {
      if (first) {
        print(x)
        first = false
      } else {
        print(", " + x)
      }
    }
    print("\n")
  }

  def calculateMonthlySalary(annualSalary: Double, bonusInPercent: Double, foodCompensation: Double,
                             taxInPercent: Double): Double = {
    var res = annualSalary * (1 + bonusInPercent / 100) + foodCompensation
    res -= res * (taxInPercent / 100)
    res / 12
  }

  def powerOfTwo(v: Int): BigInt = {
    @tailrec
    def loop(v: Int, acc: BigInt = 1): BigInt = {
      if (v == 0) {
        return acc
      }
      loop(v - 1, acc * 2)
    }

    loop(v)
  }


  def main(args: Array[String]): Unit = {
    task_one()
    val db = new EmployeeDB()
    val salary = calculateMonthlySalary(1000, 30, 80, 13)
    println("monthly salary: " + salary)
    db.addEmployeeSalary(salary)
    for (x <- Array(100.0, 150.0, 200.0, 80.0, 120.0, 75.0)) {
      db.addEmployeeSalary(x)
    }
    print("current salary: ")
    printArray(db.currentSalary())
    println("medium salary: " + db.mediumSalary())
    print("percentage deviation: ")
    printArray(db.percentageDeviation())

    val empId = 4
    println(s"current salary of Employee $empId: ${db.salary(empId)}")
    val delta = 10
    db.correctSalary(empId, delta)
    println(s"current salary of Employee $empId after : ${db.salary(empId)}")
    db.addEmployeeSalary(350)
    db.addEmployeeSalary(90)
    print("Sorted salary: ")
    val sorted = db.sortedSalary()
    printArray(sorted)
    val newEmployeeSalary = 130
    breakable {
      for (idx <- sorted.indices) {
        if (sorted(idx) >= newEmployeeSalary) {
          var newSalary = new Array[Double](sorted.length + 1)

          def copy(fromArr: Array[Double], fromArrIdx: Int, toArr: Array[Double], toArrIdx: Int, len: Int): Array[Double] = {
            val res = toArr
            for (idx <- 0 to len) {
              res(toArrIdx + idx) = fromArr(fromArrIdx + idx)
            }
            res
          }
          newSalary = copy(sorted, 0, newSalary, 0, idx)
          newSalary(idx) = newEmployeeSalary
          newSalary = copy(sorted, idx, newSalary, idx + 1, sorted.length - idx - 1)
          print("After added new employee salary: ")
          printArray(newSalary)
          break
        }
      }
    }
    db.addEmployeeSalary(newEmployeeSalary)
    // получаем индексы middle сотрудников
    print("Middle indexes: ")
    printArray(db.middleIndexes(100, 120))

    print("salary before indexing: ")
    printArray(db.data)
    db.indexSalary(7)
    print("salary after indexing: ")
    printArray(db.data)
    val v = 54
    println(s"power of two of $v = ${powerOfTwo(v)}")
  }
}
