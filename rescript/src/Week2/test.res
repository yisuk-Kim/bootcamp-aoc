let check = str => {
  //   let rule = %re("/^#[a-z0-9+]{6}$/")
  let rule = %re("/^0*[0-9+]{9}$/")
  Js.Re.test_(rule, str)
}

// "#123abc"->checkValidityHcl->Js.log
// "#123456"->checkValidityHcl->Js.log
// "#defabc"->checkValidityHcl->Js.log

// "123abc"->checkValidityHcl->Js.log
// "#123abcd"->checkValidityHcl->Js.log

"012345678"->check->Js.log
"000000008"->check->Js.log
"012345670"->check->Js.log
"112345678"->check->Js.log

"1112345678"->check->Js.log
"1123456a78"->check->Js.log

let m = Belt.Map.String.fromArray([("a", 1), ("b", 2)])
Js.String.slice(~from=0, ~to_=-2, "134cm")->Js.log
let rule = %re("/^#[a-z0-9+]{6}$/")

let hcl = None
switch hcl->Belt.Option.map(x => Js.Re.test_(rule, x)) {
| Some(true) => hcl
| Some(false) => Some("2")
| None => Some("1")
}->Js.log
(hcl <= Some(1))->Js.log

let isInValidRange = (data, min, max) => {
  let num = data->Belt.Int.fromString

  switch num->Belt.Option.map(x => (x >= min, x <= max)) {
  | Some((true, true)) => true
  | Some(_) | None => false
  }
}
"====="->Js.log
"3"->isInValidRange(1, 2)->Js.log
