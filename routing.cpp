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

vector<vector<ii>> graph;

vector<edge_info> streets;

ll num_intersect;
ll num_streets;
ll max_time;
ll num_vehicles;
ll start_node;

int main()
{
  cin >> num_intersect >> num_streets >> max_time >> num_vehicles >> start_node;

  graph.resize(num_intersect);
  for(int i=0; i < num_intersect; ++i){
    double ign;
    cin >> ign >> ign;
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

  for(int i=0; i < num_intersect; ++i)
  {
    for(int j = 0; j < (int)graph[i].size(); ++j)
    {
      std::cout << i << " to " << graph[i][j].first << " : " << streets[graph[i][j].second] << std::endl;
    }
  }
  return 0;
}
