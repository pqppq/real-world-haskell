myDumbExample xs =
  -- list does not store its own length explicitly
  -- to know the length of list, need to walk through!
  if length xs > 0
    then head xs
    else 'Z'

mySmartExample xs =
  -- better way
  if not (null xs)
    then head xs
    else 'Z'
