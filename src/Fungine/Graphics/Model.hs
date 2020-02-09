module Fungine.Graphics.Model where

import Fungine.Prelude
import Fungine.Graphics.Geometry
import Fungine.Graphics.Texture
import Numeric.LinearAlgebra

type BoneId = Word16
data Bone = Bone Int [Bone]

data MeshVertex = MeshVertex { vertexBones :: [(BoneId,Float)]
                             , vertex :: Point3d
                             , vertexUVCoord :: Point2d
                             , vertexNormal :: Point3d
                             }

data Model2d = Model2d { m2dRectangle :: Rectangle
                     , m2dTexture :: Texture
                     , m2dUVCoord :: Rectangle
}
                     
data Model3d = Model3d   { m3dBones :: Map BoneId Bone
                       , m3dMesh :: Vector MeshVertex
                       , m3dTexture :: Texture
                       }
