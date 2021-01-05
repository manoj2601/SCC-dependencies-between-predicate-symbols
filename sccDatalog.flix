/// Manoj Kumar
/// www.cse.iitd.ac.in/~cs5180411/

/// Declaration of two predicate symbols
rel Reachable(x: String, y: String)
rel Edge(x: String, y: String)

/// helper function for scc
/// it returns a list of all different nodes for given edges
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
    
/// It returns a variable with all facts for edges    
def createFacts(l : List[(String, String)]) : #{ Edge, Reachable} =
    match l {
        case (y,x)::xs => {
            let g = #{Edge(x,y).};
            solve createFacts(xs) <+> g
        }
        case _ => #{}
    }

/// It finds the scc in which node n presents.
/// it returns a tuple f (scc, visited)
def getSCC(nodes: List[String], m: #{Edge, Reachable}, n: String, visited: Set[String]) : (Set[String], Set[String]) =
    match nodes {
    case x::xs => {
        let t1 = (m |= Reachable(x, n).);
        let t2 = (m |= Reachable(n, x).);
        match (t1,t2) {
        case (true,true) => {
            let vst = Set.insert(x, visited);
            let t = getSCC(xs, m, n, vst);
            match t {
                case (s, v) => (Set.insert(x, s), v)
            }}
        case (_,_) => getSCC(xs, m, n, visited)
        }}
    case _ => (Set.singleton(n), visited)
    }

/// It returns all SCCs of the nodes 'nodes'
/// It returns a tuple of (List of SCCs, visited)
def getSCCs(nodes: List[String], m: #{Edge, Reachable}, visited: Set[String]): (List[Set[String]], Set[String]) =
    match nodes {
    case x::xs => {
        let isVisited = Set.find(a->a==x, visited);
        match isVisited {
        case None => {
            let vst = Set.insert(x, visited);
            let t = getSCC(xs, m, x, vst);
            match t {
            case (scc, v) => {
                let r = getSCCs(xs, m, v);
                match r {
                    case (l, v1) => (List.append(scc::Nil, l), v1)
                }}}}
        case _ => getSCCs(xs, m, visited)
        }}
    case _ => (Nil, visited)
    }

/// Function to find a list of all SCCs where each SCC is a set of Strings.
///  input : list of edges (dependencies) edge (X, Y) means X <- Y
def scc(edges: List[(String, String)]): List[Set[String]] = 
    let nodes = allNodes(edges);
    let r = #{
        Reachable(x, y) :- Edge(x, y).
        Reachable(x, y) :- Reachable(x, z), Edge(z, y).
    };
    let p = createFacts(edges);
    let m = solve p<+>r;
    let a = getSCCs(nodes, m, Set.empty());
    match a {
        case (s, _) => s
        case _ => Nil
    }

    
def main(): List[Set[String]] =
    let edges = ("A", "B")::("B", "A")::("B", "D")::("C", "A")::("C", "B")::Nil;
    scc(edges)