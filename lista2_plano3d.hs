import Data.List (foldl')

data Mov = Z Double | X Double | Y Double
  deriving (Eq, Show, Read)

type Point3D = (Double, Double, Double)
type Object3D = [Point3D]

toRad :: Double -> Double
toRad angle = angle * (pi / 180.0)

transladaObjeto :: Object3D -> [Mov] -> Object3D
transladaObjeto object movements = map translatePoint object
  where
    translatePoint (x, y, z) =
      let
        newX = x + sum [value | X value <- movements]
        newY = y + sum [value | Y value <- movements]
        newZ = z + sum [value | Z value <- movements]
      in
        (newX, newY, newZ)

rotatePoint :: Double -> Double -> Double -> Point3D -> Point3D
rotatePoint angleX angleY angleZ (x, y, z) =
  let
    radX = toRad angleX
    radY = toRad angleY
    radZ = toRad angleZ

    newX = x * cos radY * cos radZ - y * cos radY * sin radZ + z * sin radY
    newY = x * (cos radX * sin radZ + sin radX * sin radY * cos radZ) + y * (cos radX * cos radZ - sin radX * sin radY * sin radZ) - z * cos radY * sin radX
    newZ = x * (sin radX * sin radZ - cos radX * sin radY * cos radZ) + y * (sin radX * cos radZ + cos radX * sin radY * sin radZ) + z * cos radY * cos radX
  in
    (newX, newY, newZ)

rotacionaObjeto :: Double -> Double -> Double -> Object3D -> Maybe Object3D
rotacionaObjeto angleX angleY angleZ object
  | all (\angle -> angle >= 0 && angle <= 360) [angleX, angleY, angleZ] =
      Just $ map (rotatePoint angleX angleY angleZ) object
  | otherwise = Nothing

main :: IO ()
main = do
  coord <- getLine
  mov <- getLine
  angX <- getLine
  angY <- getLine
  angZ <- getLine

  let object = read coord :: Object3D
  let movements = read mov :: [Mov]
  let angleX = read angX :: Double
  let angleY = read angY :: Double
  let angleZ = read angZ :: Double

  let transladado = transladaObjeto object movements
  let rotacionado = rotacionaObjeto angleX angleY angleZ transladado

  print rotacionado
