{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RayTracing.Object.Shape (
  module RayTracing.Object.Shape.Class,
  module RayTracing.Object.Shape.StdShape,
  module RayTracing.Object.Shape.Plane,
  module RayTracing.Object.Shape.Box,
  module RayTracing.Object.Shape.Sphere,
  module RayTracing.Object.Shape.Translate,
  module RayTracing.Object.Shape.Rotate,
) where

import RayTracing.Object.Shape.Box
import RayTracing.Object.Shape.Class
import RayTracing.Object.Shape.Plane
import RayTracing.Object.Shape.Rotate
import RayTracing.Object.Shape.Sphere
import RayTracing.Object.Shape.StdShape
import RayTracing.Object.Shape.Translate
