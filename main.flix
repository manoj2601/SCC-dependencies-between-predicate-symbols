//list of inverted edges
def getTranspose(edges: List[(String, String)]): List[(String, String)] =
    match edges {
        case (x1, x2)::xs => List.append((x2, x1)::Nil, getTranspose(xs))
        case Nil => Nil
    }

//map of nodes and adjency lists
def createAdjList(edges: List[(String, String)]): Map[String, List[String]] = 
    match edges {
    case (y,x)::xs => {
        let m = createAdjList(xs);
        let temp = Map.get(x, m);
        match temp {
        case None => {let temp = List.append(y::Nil, Nil);
            Map.insert(x, temp, m)}
        case Some(l) => Map.insert(x, List.append(y::Nil, l), m)
    }}
    case Nil => Map.empty()
}

//stack, visited
def dfsHelper(adjlist: Map[String, List[String]], stack: List[String], visited: Set[String], node: String): (List[String], Set[String]) =
    let visited2 = Set.insert(node, visited);
    let child = Map.get(node, adjlist);
    match child {
    case Some(l) => {
        let t = dfs(adjlist, stack, visited2, l);
        match t {
        case (stk, visited2) => {
            let stak = List.append(node::Nil, stk);
            (stak, visited2)
        }
        case _ => (stack, visited2)
        }}
    case None => {
        let stack = List.append(node::Nil, stack);
        (stack, visited2)
    }}

//stack, visited
def dfs(adjlist: Map[String, List[String]], stack: List[String], visited: Set[String], nodes: List[String]): (List[String], Set[String]) =
    match nodes {
    case x::xs => {
        let t = Set.find(a -> a==x, visited);
        match t {
        case None => {
            let t2 = dfsHelper(adjlist, stack, visited, x); //stack, visited
            match t2 {
            case (stack, visited) => {
                dfs(adjlist, stack, visited, xs)
        }}}
        case Some(_v) => dfs(adjlist, stack, visited, xs)
    }}
    case Nil => (stack, visited)
    }

//scc, visited 
def dfs2helper(adjList: Map[String, List[String]], nodes: List[String], visitedold: Set[String], mySCC: Set[String]) : (Set[String], Set[String]) =
    match nodes {
    case x::xs => {
        let t1 = Set.find(a-> a==x, visitedold);
        match t1 {
            case Some(_v) => {
                dfs2helper(adjList, xs, visitedold, mySCC)
            }
            case None => {
                let t = dfs2(adjList, x, visitedold, mySCC);
                match t {
                case (scc, visited1) => dfs2helper(adjList, xs, visited1, scc)
        }}}
    }
    case _ => (mySCC, visitedold)
    }

//scc, visited
def dfs2(adjList: Map[String, List[String]], node: String, visitedold: Set[String], mySCCold: Set[String]): (Set[String], Set[String]) =
    let visited = Set.insert(node, visitedold);
    let mySCC = Set.insert(node, mySCCold);
    let child = Map.get(node, adjList);
    match child {
        case Some(l) => dfs2helper(adjList, l, visited, mySCC)
        case None => (mySCC, visited)
    }

//list of sccs
def phaseTwo(adjList: Map[String, List[String]], stack: List[String], visited: Set[String]): List[Set[String]] =
    match stack {
    case x::xs =>{
        let t = Set.find(a-> a==x, visited);
        match t {
        case None => {
            let newSCC = Set.empty();
            let s = dfs2(adjList, x, visited, newSCC);
            match s {
            case (scc, vst) => {
                let l = phaseTwo(adjList, xs, vst);
                List.append(scc::Nil, l)
        }}}
        case Some(_v) => phaseTwo(adjList, xs, visited)
    }}
    case Nil => Nil
    }

//list of all different nodes for given edges
def allNodes(edges: List[(String, String)]): List[String] =
    match edges {
    case x::xs => {
        let l = allNodes(xs);
        match x {
        case (x1, x2) => {
            let r1 = List.find(a -> a==x1, l);
            match r1 {
            case None =>{
                let l = List.append(x1::Nil, l);
                let r2 = List.find(a -> a==x2, l);
                match r2 {
                case None => List.append(x2::Nil, l)
                case Some (_v) => l
            }}
            case Some(_v) =>{
                let r2 = List.find(a -> a==x2, l);
                match r2 {
                case None => List.append(x2::Nil, l)
                case Some (_u) => l
                }
        }}}}
    }
    case _ => Nil
    }

// list of sccs
def scc(edges: List[(String, String)]) : List[Set[String]] =
    let adjlist = createAdjList(edges);
    let stack = Nil;
    let visited = Set.empty();
    let nodes = allNodes(edges);
    let t = dfs(adjlist, stack, visited, nodes);
    match t {
        case (stk, _visited) => {
            let edgesT = getTranspose(edges);
            phaseTwo(createAdjList(edgesT), stk, Set.empty())
        }
    }

def main(): List[Set[String]] =
    let edges = ("A", "B")::("B", "A")::("B", "D")::("C", "A")::("C", "B")::Nil;
    scc(edges)
