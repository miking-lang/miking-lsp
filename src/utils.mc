let optionSnoc = lam x. lam y.
  match y with Some e then snoc x e else x

let optionInvert = lam opt.
  match opt with None () then
    Some (None ())
  else match opt with Some (None ()) then
    None ()
  else opt

let optionInvertMap = lam f. compose optionInvert (optionMap f)

let optionMapM = lam f. lam l.
  foldr (lam x. lam o. optionBind (f x) (lam y. optionMap (cons y) o))
        (Some [])
        l
