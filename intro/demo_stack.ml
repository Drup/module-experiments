module StackCalc = struct

  type t = int Stack.t
  
  let un_op f st =
    let x = Stack.pop st in
    Stack.push (f x) st

  let neg = un_op (~-)
  
  let bin_op f st =
    let x = Stack.pop st in
    let y = Stack.pop st in
    Stack.push (f x y) st

  let plus = bin_op ( + )
  let mult = bin_op ( * )
  
  let swap st = 
    let x = Stack.pop st in
    let y = Stack.pop st in
    Stack.push x st;
    Stack.push y st

end
