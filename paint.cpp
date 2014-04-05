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
typedef vector<bool> vb;
typedef map<int, int> mii;
typedef long long ll;


int x_shift[9] = {-1,0,1,1,1,0,-1,-1};
int y_shift[9] = {-1, -1, -1, 0, 1, 1, 1, 0};


#define DFS_WHITE -1
#define DFS_BLACK 1
#define INF 1000000000

#define REP(i, n) for(int i=0; i < n; ++i)

#define N 1000010

void print_delete(int i, int j){
  std::cout << "ERASECELL " << i << " " << j << std::endl;
}

void print_square(int i, int j, int s)
{
  std::cout << "PAINTSQ " << i << " " << j << " " << s << std::endl;
}

int main()
{
  int h,l;
  scanf("%d%d\n", &h, &l);
  vector<vi> m;
  vector<vb> draws;
  m.resize(h);
  draws.resize(h);
  for(int i=0; i < h; ++i) {
    m[i].resize(l);
    draws[i].resize(l);
  }

  for(int i=0; i < h; ++i)
  {
    for(int j=0; j < l; ++j)
    {
      m[i][j] = (getchar() == '.'? 0:1);
      draws[i][j] = false;
    }
    getchar(); // \n
  }

  for(int i=1; i < h-1; ++i)
    for(int j=1; j < l-1; ++j)
    {
      int takens = 0;
      for(int kkk=0; kkk < 9; ++kkk){
        int y = i+y_shift[kkk];
        int x = j+x_shift[kkk]; 
        if(m[y][x] == 1 && !draws[y][x]) ++takens;
      }
      int takens2 = 0;
      if(i > 1 && i < h-2 && j > 1 && j < l-1){
      for(int kkk=0; kkk < 9; ++kkk){
        for(int g=1; g <= 2; ++g){
        int y = i+y_shift[kkk]*g;
        int x = j+x_shift[kkk]*g; 
        if(m[y][x] == 1 && !draws[y][x]) ++takens2;
      }}}

      if(takens2 > 25/2){
                print_square(i, j, 2);
        for(int g=1; g <= 2; ++g){
        int y = i+y_shift[h]*g;
        int x = j+x_shift[h]*g; 
          draws[y][x] = true;
        }
      }

      else if(takens > 9/2){
        print_square(i, j, 1);
        for(int h=0; h < 9; ++h){
          int y = i+y_shift[h];
          int x = j+x_shift[h];
          draws[y][x] = true;
        }
      }
    }

  for(int i=0; i < h; ++i)
    for(int j=0; j < l; ++j)
    {
      if(draws[i][j] && m[i][j] == 0) print_delete(i, j);
      if(!draws[i][j] && m[i][j] == 1) print_square(i, j, 0);
    }

  return 0;
}
