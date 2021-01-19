import macros


macro x(a: untyped = 4): untyped =
  var x = quote:
    self.ping(`a`)
  echo x.astGenRepr
  newEmptyNode()

x()

# macro tmp() =
#   nnkStmtList.newTree(
#     nnkProcDef.newTree(
#       nnkPostfix.newTree(
#         newIdentNode("*"),
#         newIdentNode("fsym")
#       ),
#       newEmptyNode(),
#       newEmptyNode(),
#       nnkFormalParams.newTree(
#         newIdentNode("emacs_value"),
#         nnkIdentDefs.newTree(
#           newIdentNode("env"),
#           nnkPtrTy.newTree(
#             newIdentNode("emacs_env")
#           ),
#           newEmptyNode()
#         ),
#         nnkIdentDefs.newTree(
#           newIdentNode("nargs"),
#           newIdentNode("ptrdiff_t"),
#           newEmptyNode()
#         ),
#         nnkIdentDefs.newTree(
#           newIdentNode("args"),
#           nnkPtrTy.newTree(
#             nnkBracketExpr.newTree(
#               newIdentNode("array"),
#               nnkInfix.newTree(
#                 newIdentNode(".."),
#                 newLit(0),
#                 newIdentNode("max_args")
#               ),
#               newIdentNode("emacs_value")
#             )
#           ),
#           newEmptyNode()
#         ),
#         nnkIdentDefs.newTree(
#           newIdentNode("data"),
#           newIdentNode("pointer"),
#           newEmptyNode()
#         )
#       ),
#       nnkPragma.newTree(
#         newIdentNode("exportc"),
#         nnkExprColonExpr.newTree(
#           newIdentNode("extern"),
#           nnkInfix.newTree(
#             newIdentNode("&"),
#             nnkInfix.newTree(
#               newIdentNode("&"),
#               newLit("nimEmacs_"),
#               nnkDotExpr.newTree(
#                 newIdentNode("self"),
#                 newIdentNode("libName")
#               )
#             ),
#             newLit("_$1")
#           )
#         )
#       ),
#       newEmptyNode(),
#       nnkStmtList.newTree(
#         nnkTryStmt.newTree(
#           nnkStmtList.newTree(
#             newIdentNode("body")
#           ),
#           nnkExceptBranch.newTree(
#             newIdentNode("Exception"),
#             nnkStmtList.newTree(
#               nnkCall.newTree(
#                 nnkDotExpr.newTree(
#                   newIdentNode("env"),
#                   newIdentNode("mk_call")
#                 ),
#                 newLit("intern"),
#                 nnkPrefix.newTree(
#                   newIdentNode("@"),
#                   nnkBracket.newTree(
#                     newLit("nil")
#                   )
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   )
