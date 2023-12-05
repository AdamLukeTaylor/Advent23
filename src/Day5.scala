object Day5 {
  case class Function(start: Long, end: Long, adjust: Long)
  def part1(input: String): Unit = {
    println("-----------Part 1---------------")
    val maps = input.split("\n\n")
    val mapSeedToSoil = populateFunction(maps(1))
    val mapSoilToFert = populateFunction(maps(2))
    val mapFertToWater = populateFunction(maps(3))
    val mapWaterToLight = populateFunction(maps(4))
    val mapLightToTemp = populateFunction(maps(5))
    val mapTempToHumid = populateFunction(maps(6))
    val mapHumidToLocation = populateFunction(maps(7))
    println(mapSeedToSoil)
    val seeds = maps(0).substring(7).split(" ").map(_.toLong).toList
    val locations = seeds.map { seed =>
      val soil = lookUp(mapSeedToSoil, seed)
      val fert = lookUp(mapSoilToFert, soil)
      val water = lookUp(mapFertToWater, fert)
      val light = lookUp(mapWaterToLight, water)
      val temp = lookUp(mapLightToTemp, light)
      val humid = lookUp(mapTempToHumid, temp)
      val location = lookUp(mapHumidToLocation, humid)
      println(s"$seed -> $soil -> $fert -> $water -> $light -> $temp ->$humid -> $location")
      location
    }
    println(locations.min)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def part2(input: String): Unit = {
    println("-----------Part 2---------------")
    val maps = input.split("\n\n")
    val mapSeedToSoil = populateFunction(maps(1))
    val mapSoilToFert = populateFunction(maps(2))
    val mapFertToWater = populateFunction(maps(3))
    val mapWaterToLight = populateFunction(maps(4))
    val mapLightToTemp = populateFunction(maps(5))
    val mapTempToHumid = populateFunction(maps(6))
    val mapHumidToLocation = populateFunction(maps(7))
    val seedsRaw = maps(0).substring(7).split(" ").map(_.toLong).toList
    println(mapSeedToSoil)
    println(mapSoilToFert)
    println(s"Seedsraw $seedsRaw")
    val netLocations = seedsRaw.grouped(2).map { plant =>
      println(s"Plant $plant")
      var locationMin = Long.MaxValue
      for( a<- plant.head to plant.head + plant(1)) {
          val soil = lookUp(mapSeedToSoil, a)
          val fert = lookUp(mapSoilToFert, soil)
          val water = lookUp(mapFertToWater, fert)
          val light = lookUp(mapWaterToLight, water)
          val temp = lookUp(mapLightToTemp, light)
          val humid = lookUp(mapTempToHumid, temp)
          val location = lookUp(mapHumidToLocation, humid)
//          println(s"$a -> $soil -> $fert -> $water -> $light -> $temp ->$humid -> $location")
          if(location< locationMin) {
            locationMin = location
          }
      }
      locationMin
    }.toList
    println(netLocations)
    println(netLocations.min)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def lookUp(functions: List[Function], item: Long): Long = {
    val returns = functions.flatMap { func =>
      if (item >= func.start && item < func.end) Some(item + func.adjust)
      else None
    }
    returns.length match {
      case 0 => item
      case 1 => returns.head
      case _ => ???
    }
  }
  
//  def combineFunctions(before: List[Function], after: List[Function]): List[Function] = {
//    after.map{aft =>
//      before.foreach{bef =>
//        if((bef.start <= aft.start && bef.end > aft.start)||(bef.start <= aft.end && bef.end > aft.end)){
//          //over lap
//          List(Function(start = Math.min(bef.start, aft.start), end = ???, adjust = ???))
//        }
//
//      }
//    }
//  }

  def populateFunction(input: String): List[Function] = {
    input.split("\n").flatMap { line =>
      if (line.contains("map")) {
        None
      } else {
        // in out switched
        val chunks = line.split(" ")
        val range = chunks(2).toLong
        val inStart = chunks(1).toLong
        val inEnd = inStart + range
        val outStart = chunks(0).toLong
        val outEnd = outStart + range
        Some(Function(inStart, inEnd, -inStart + outStart))
      }
    }.toList
  }

  def populateMap(map: Map[Long, Long], input: String): Map[Long, Long] = {
    val lists = input.split("\n").flatMap { line =>
      if (line.contains("map")) {
        None
      } else {
        // in out switched
        val chunks = line.split(" ")
        val range = chunks(2).toLong
        val inStart = chunks(0).toLong
        val inEnd = inStart + range
        val outStart = chunks(1).toLong
        val outEnd = outStart + range
        val in = inStart until inEnd
        val out = outStart until outEnd
        Some((out.toList zip in.toList).toMap)
      }
    }.toList
    lists.fold(Map.empty)(_ ++ _)
  }
}
