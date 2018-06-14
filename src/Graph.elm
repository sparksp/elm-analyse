module Graph exposing (Edge, Graph, Node, NodeContext, edges, fromNodesAndEdges, get, isEmpty, nodes)

import Dict exposing (Dict)


type Graph a b
    = Graph a b


fromNodesAndEdges : a -> b -> c
fromNodesAndEdges i g =
    Debug.todo "TODO"


type alias Node n =
    { id : NodeId
    , label : n
    }


type alias Edge e =
    { from : NodeId
    , to : NodeId
    , label : e
    }


type alias NodeId =
    Int


edges : a -> b
edges a =
    Debug.todo "todo"


get : NodeId -> Graph a b -> c
get i g =
    Debug.todo "TODO"


isEmpty : Graph a b -> Bool
isEmpty a =
    Debug.todo "TODO"


nodes : Graph a b -> List c
nodes a =
    Debug.todo "TODO"


type alias NodeContext n e =
    { node : Node n
    , incoming : Adjacency e
    , outgoing : Adjacency e
    }


type alias Adjacency e =
    Dict Int e
