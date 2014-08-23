#include <iostream>
#include <queue>
#include <list>
#include <utility>
using namespace std;

typedef pair<int, int> pii;

list<pii> graph[10000];
int dist[10000];
int N, M;

void bfs(){
    priority_queue<pii> q;

    q.push(pii(0,0));
    
    while(!q.empty()){
        pii curr = q.top();
        q.pop();


        if(dist[curr.second]!=-1) continue;

        dist[curr.second] = -curr.first;

        for(auto it = graph[curr.second].begin(); it != graph[curr.second].end(); it++){
            if(dist[it->first]==-1){
                q.push(pii(curr.first-it->second, it->first));
            }
        }
    }
}

int main(){
    cin >> N >> M;
    
    int a, b, c;
    for(int i=0;i<M;i++){
        cin >> a >> b >> c;
        graph[a-1].push_back(pii(b-1,c));
    }

    for(int i=0;i<N;i++) dist[i] = -1;

    bfs();

    for(int i=0;i<N;i++) cout << dist[i] << " ";
    cout << endl;

    return 0;
}


