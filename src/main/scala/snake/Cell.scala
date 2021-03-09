package snake

sealed trait Cell
case object SnakePart extends Cell
case object Wall extends Cell
case object EmptyCell extends Cell
case object FoodCell extends Cell
case object ObstacleCell extends Cell