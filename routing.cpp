#include <iostream>
#include <string>
#include <cstdio>
#include <cstring>
#include <vector>
#include <map>
#include <bitset>
#include <algorithm>
#include <utility>
#include <queue>
#include <boost/optional.hpp>
#include <random>

using namespace std;
using namespace boost;
typedef vector<int> vi;
typedef pair<int, int> ii;
typedef vector<ii> vii;
typedef vector<bool> vb;
typedef map<int, int> mii;
typedef long long ll;
struct edge_info;
typedef pair<int, edge_info> ie;

int x_shift[9] = {-1,0,1,1,1,0,-1,-1};
int y_shift[9] = {-1, -1, -1, 0, 1, 1, 1, 0};


#define DFS_WHITE -1
#define DFS_BLACK 1
#define INF 1000000000

#define REP(i, n) for(int i=0; i < n; ++i)

#define N 1000010

struct edge_info
{
  ll cost;
  ll length;
};

template <class Out>
Out& operator<<(Out& out, const edge_info& edge)
{
  return out << "("<< edge.cost << "," << edge.length << ")" << std::endl;
}

vector<vii> graph;
vector<edge_info> streets;
vector<vi> vpath;

struct coord_t { 
  double x,y; 
  coord_t(){}
  coord_t(double x, double y):x(x),y(y){}
};

vi visited;

ll num_intersect;
ll num_streets;
ll max_time;
ll num_vehicles;
ll start_node;
vector<coord_t> coords;

vector<coord_t> directions = {
  coord_t(-90,45),
  coord_t(0,90),
  coord_t(90,45),
  coord_t(180,0),
  coord_t(90,-45),
  coord_t(0,-90),
  coord_t(-90,-45),
  coord_t(-180,0)
};

std::random_device rd;
std::uniform_int_distribution<int> ddir(1,1);

std::uniform_int_distribution<int> droad(1,80);

double dist(const coord_t& c1, const coord_t& c2)
{
  return sqrt((c2.x - c1.x)*(c2.x - c1.x) + (c2.y - c1.y)*(c2.y -c1.y));
}

int veh;

int goto_free_point(const vii& g, int& t){
  vi bfs;
  vi res;
  vi revisited;
  revisited.resize(num_intersect);

  for(int i=0; i < num_intersect; ++i)
    revisited[i] = 0;

  for(int i=0; i < (int)g.size(); ++i)
    bfs.push_back(g[i].first);
  int go;
  while(!bfs.empty()){
    go = bfs.back();
    bfs.pop_back();
    if(visited[go] && !revisited[go]){
      res.push_back(go);
      revisited[go] = 1;
      for(int i=0; i < (int)g.size(); ++i)
        if(!revisited[go])
          bfs.push_back(graph[go][i].first);
    }
    else{
      break;
    }
  }
        for(int i=0; i < (int)res.size(); ++i)
        vpath[veh].push_back(res[i]);
      return go;
}

int get_road(int ridx, const vii& g, int &t){
  coord_t dir = directions[ridx];
  double min_dist = 800000000.0;
  int res = -1;
  for(int i = 0; i < (int)g.size(); ++i){
    double min_dist2= dist(dir, coords[g[i].first]);
    if(visited[g[i].first] < 2 && min_dist > min_dist2){
      min_dist = min_dist2;
      res = i;
    }
  }
  // if(res == -1) return goto_free_point(g, t);
  // return res;
  return (res == -1)?droad(rd)%g.size():res;
}

int force_roll(int ridx)
{
  return (ridx+ddir(rd))%directions.size();
}

vii access_node_in_time(int start, int t)
{
  vii subg;
  for(int i = 0; i < (int)graph[start].size(); ++i)
    if(streets[graph[start][i].second].cost < max_time - t)
      subg.push_back(graph[start][i]);
  return subg;
}

int kinit = 20;
int kmax = kinit;

void travel(int k, int start, int ridx, int t)
{
  if(t < max_time){
    ++visited[start];

    vpath[veh].push_back(start);

    vii subg = access_node_in_time(start, t);
    if(subg.size() > 0){

      if(k%kmax==0){ if(kmax > 6) kmax-=1; else { kmax = 12;}ridx = force_roll(ridx);}
      else ++k;
      int road = get_road(ridx, subg, t);
      travel(k, subg[road].first, ridx, t+streets[subg[road].second].cost);
    }
  }
}

bool sorter(const ii& l, const ii& r)
{
  return streets[l.second].length < streets[r.second].length;
}

int main()
{
  cin >> num_intersect >> num_streets >> max_time >> num_vehicles >> start_node;

  vpath.resize(num_vehicles);

  visited.resize(num_intersect);

  graph.resize(num_intersect);
  coords.resize(num_intersect);
  for(int i=0; i < num_intersect; ++i){
    cin >> coords[i].y >> coords[i].x;
  }

  streets.resize(num_streets);
  for(int i=0; i < num_streets; ++i){
    int s, e, sens;
    cin >> s >> e >> sens >> streets[i].cost >> streets[i].length;
    graph[s].push_back(ii(e, i));
    if(sens == 2) {
      graph[e].push_back(ii(s, i));
    }
  }

  for(int i=0; i < (int)graph.size(); ++i)
    std::sort(graph[i].begin(), graph[i].end(), sorter);

    for(int i=0; i < num_intersect; ++i)
      visited[i] = 0;
  for(int i=0; i < num_vehicles; ++i)
  {
    veh = i;
    travel(1, start_node, i%directions.size(), 0);
  }

  std::cout << vpath.size() << std::endl;
  for(int i=0; i < (int)vpath.size(); ++i)
  {
    std::cout << vpath[i].size() << std::endl;
    for(int j=0; j < (int)vpath[i].size(); ++j)
    {
      std::cout << vpath[i][j] <<std::endl;
    }
  }
  return 0;
}
