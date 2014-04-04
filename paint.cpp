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

using namespace std;
typedef vector<int> vi;
typedef pair<int, int> ii;
typedef vector<ii> vii;
typedef map<int, int> mii;
typedef long long ll;

#define DFS_WHITE -1
#define DFS_BLACK 1
#define INF 1000000000

#define REP(i, n) for(int i=0; i < n; ++i)

#define N 1000010

int value[N];

int process(int n)
{
  if(n == 1) return 1;

  return process((n%2 == 0) ? n/2: n*3+1) + 1;
}

int main()
{
  int h,l;
  scanf("%d%d\n", &h, &l);
  vector<vi> m;
  vector<ii> mm;
  // m.resize(h);
  // for(int i=0; i < h; ++i) m[i].resize(l);

  for(int i=0; i < h; ++i)
  {
    for(int j=0; j < l; ++j)
    {
      // m[i][j] = (getchar() == '.'? 0:1);
      if(getchar() == '#')
        mm.push_back(ii(i,j));
    }
    getchar(); // \n
  }
  std::cout << mm.size() << std::endl;
  for(int i=0; i < mm.size(); ++i)
    std::cout << "PAINTSQ " << mm[i].first << " " << mm[i].second << " 0\n";
  // int min_y = INF;
  // int min_x = INF;
  // int max_y = -1;
  // int max_x = -1;
  // for(int i=0; i < h; ++i)
  //   for(int j=0; j < l; ++j)
  //   {

  //     if(m[i][j] == 1){
  //       min_y = min(min_y, i);
  //       max_y = max(max_y, i);
  //       min_x = min(min_x, j);
  //       max_x = max(max_x, j);
  //     }
  //   }

  // std::cout << min_y << " " << max_y << " " << min_x << " " << max_x << std::endl;
  return 0;
}
