#include<iostream>
#include<vector>
#include<algorithm>
#include<queue>
#include<map>
#include<iterator>
#include <cstdlib>

using namespace std;

typedef vector<int> face;



int printface(face f){
    for (int v : f){
        cout<<v<<" ";
    }
    return 0;
}


int printvectorf(vector<face> vect){
    for(face f: vect){
            printface(f);
            cout<<"\n";
        }
    return 0;
}


/// @brief We get the next subset of the vector facet 
/// @return a face sg, which vertices are in the increasing order
int nextface(face facet, int i, int n, face &newface){
    newface.clear();
    for(int j=0;j<n;j++){
        if((i>>j)&1)    //is the element in the subset?
            newface.push_back(facet[j]);
    }
    //printface(newface);
    //cout<<"\n";
    return 0;
}

int computefaces(vector<face> &facets, vector<face> &faces){
    faces.clear();
    face sigma;
    int n;
    for(face facet : facets){
        n=facet.size();
        for(int i=0;i<(1<<n);i++){
            nextface(facet, i, n, sigma);
            if(find(faces.begin(), faces.end(), sigma) == faces.end()){
                faces.push_back(sigma);
            }
        }
    }
    return 0;
}


/// @brief Put the face sg minus sgk in the ref priv. Vertices of every face in increasing order. Linear time
int getminus(face sg, face sgk, face &priv){
    priv.clear();
    int i =0, j=0;
    int n=sg.size(), m=sgk.size();
    while(i<n){
        if(j>m-1 || sg[i]<sgk[j]){
            //cout<<"case1\n";
            priv.push_back(sg[i]);
            i++;
        }
        else{
            if(sg[i]==sgk[j]){
                i++;
                //cout<<"case2\n";
            }
            //cout<<"case23\n";
            j++;
        }
        //cout<<"i="<<i<<" j="<<j<<"\n";
    }
    return 0;
}


int computepriv(vector<face> &facets, vector<face> &faces, map<face,map<face,face>> &priv){
    priv.clear();
    map<face,face> privsg;
    face privsgsgk;
    for(face sg : faces){
        for(face sgk:facets){
            getminus(sg,sgk,privsgsgk);
            privsg[sgk]=privsgsgk;
        }
        priv[sg]=privsg;
    }
    return 0;
}




/// @brief 
/// @param sg a face sigma
/// @param facets the ordered list of the facets
/// @return |M<F(sigma)|
int sizeMes(face sg, vector<face> facets, map<face,map<face,face>> &privmap){
    int m=0;
    int mini;
    face priv;
    bool endfacet;
    priority_queue<int, vector<int>, greater<int> > mes;  //Min-heap (the top is the mimimum)

    for (face sgk : facets){
        priv=privmap[sg][sgk];
        //If sg is a subset of sgk
        //cout<<"priv=";
        //printface(priv);
        //cout<<"\n";
        if(priv.size()==0){
            break;
        }
 
        priority_queue<int,vector<int>,greater<int> > mescopy(mes); //copy of mes
        endfacet=false;
        while(!mescopy.empty() && !endfacet){
            mini=mescopy.top();
            mescopy.pop();
            //If mini is in sg\sgk, we are in case 1
            if(binary_search (priv.begin(), priv.end(), mini)){
                endfacet=true;
            }
        }
        //If we are in case 2:
        if(!endfacet){
            mes.push(priv[0]); //priv[0] is the minimum
            m++;
        }
    }
    //cout<<"for the face sigma="<<printface(sg)<<", we have M(sg) = "<<print(mes)<<"\n";
    return m;
}





/// @param facets the ordered vector of the facets of our simplicial complex.
/// It is equivalent to taking a simplicial complex X and an order <F in input.
/// We suppose the vertices in a face are in increasing order
/// @return d<F(X) 
int dmes(vector<face> &facets, vector<face> &faces, map<face,map<face,face>> &priv,int dwanted){
    int szmes;
    int d=0;
    vector<face> thefaces; //to get the faces that are interesting
    //For each facet
    for(face sigma : faces){
        szmes=sizeMes(sigma, facets, priv);
        //cout<<"face=";
        //printface(sigma);
        //cout<<", sizeM="<<szmes<<"\n";
        
        //Here we could also memorize the face if needed
        if(szmes>d){
            d=szmes;
            if(szmes==dwanted){
                thefaces.push_back(sigma);
            }
        }
    }

    /*if(d==dwanted){
        cout<<"the faces = ";
        printvectorf(thefaces);
        cout<<"\n"; 
    }  */
    return d;
}



/// @brief computes the m.e.s number of a simplicial complex
/// @param simpl our simplicial complex
/// @param all if all=0, it computes only with the ordering given by simpl, 
/// if all=1, it returns the min for all possible orderings
/// @param dwanted 
/// @return the minimal exclusion sequence number of a simplicial complex
int bestorder(vector<face> simpl, int all, int dwanted){
    vector<face> faces;
    computefaces(simpl, faces);

    map<face,map<face,face>> priv; //priv[sg][sgk] is sg\sgk
    computepriv(simpl,faces,priv);

    int d=dmes(simpl,faces,priv,dwanted);
    if(! all){
        return d;
    }
    
    int dt;
    vector<face> bestorder=simpl;
    do{
        //cout<<d<<"\n";
        dt=dmes(simpl,faces,priv,dwanted);
        if(dt<d){
            d=dt;
            //cout<<"not the first!\n";
            bestorder=simpl;
        }
        /*if (dt<4){
            cout<<"dt="<<dt<<" THE END\n";
            break;
        }*/
    }while(next_permutation(begin(simpl), end(simpl)));

    //cout<<"\nBest order for T1(K):\n";
    //printvectorf(bestorder);

    return(d);
}


face merge2faces(face f1, face f2){
    face mrg;
    int i=0, j=0;
    int n1=f1.size(), n2=f2.size();
    while(i!=n1 || j!=n2){
        if(j==n2 || (i<n1 && f1[i]<f2[j])){
            mrg.push_back(f1[i]);
            i++;
        }
        else{
            mrg.push_back(f2[j]);
            j++;
        }
    }
    return mrg;
}



//Creating a simplicial complex
int create_complex(vector<face> &facets, int n, int nb_facets, face &graph){
   
    int m;
    face sigma;
    face nfacet;
    face priv, overset;
    map<face,int> possible;
    //iterator item; 

    facets.clear();

    for(int i=0;i<(1<<n);i++){
        nextface(graph, i, n, sigma);   
        possible[sigma]=1;          
        
    }

    while(nb_facets>0 && !possible.empty()){
        nb_facets--;
        auto item= possible.begin();
        advance(item, rand() % (possible.size()) );
        nfacet=item->first;

        facets.push_back(nfacet);
        m=nfacet.size();

        //remove all the subset of the facet fom the list of the possibilities
        for(int j=0;j<(1<<m);j++){
            nextface(nfacet, j, m, sigma);
            possible.erase(sigma); 
        }

        //remove all the faces which have facet as a subset
        getminus(graph, nfacet,priv);
        for(int j=0; j<(1<<priv.size());j++){
            nextface(priv, j, n-m, sigma);
            overset=merge2faces(sigma,nfacet);
            possible.erase(overset);
        }


    }
    return 0;
}


/// @brief from a simplicial complex X, create T1(X) the 1-tolerance complex
/// @param simpl our simplicial complex X
/// @param tsimpl T1(X)
/// @param graph the set of vertices V on which X is based 
/// @return 0
int extends1(vector<face> &simpl, vector<face> &tsimpl, face graph){
    tsimpl.clear();
    face complem;
    face newface;
    face diff;
    bool isfacet;
    for(face facet : simpl){
        getminus(graph, facet, complem);
        for(int v : complem){
            newface=merge2faces(facet,{v});
            isfacet=true;
            for(int i=0; i<tsimpl.size(); i++){
                //If newface is a subset of tsimpl[i], it is not a facet, we don't add it
                getminus(newface, tsimpl[i], diff);
                if(diff.empty()){
                    isfacet=false;
                    break;
                }
                //If f is a subset of newface, tsimpl[i] is not a facet : we remove it from our new simplicial complex
                getminus(tsimpl[i], newface, diff);
               
                if(diff.empty()){
                    tsimpl.erase(tsimpl.begin()+i);
                    i--;
                }
                
            }
            if(isfacet){
                tsimpl.push_back(newface);
            }
        }
    }
    return 0;
}



//For 100 tests on only 5 vertices (these can be changed), we create a random simplicial complex K, 
//then compute its 1-tolerance complex T1(K). If it is too big, we skip this test.
//Else we compute both the minimal exlusion sequence number of K (d1) and T1(K) (d2)
//Depending on the parameters we give to "bestorder", we can compute this number for the given 
//linear ordering on the facets, or on all possible orderings on the facet (more interesting but it also takes
//WAY MORE time!)
//We keep for each d1 the maximum corresponding d2 
int main()
{
    int n;
    int nb_facets;

    map<int,int> results;

    int test=100;
    while(test){
        cout<<"\ntest nb="<< test <<"\n";
        test--;

        n=5 ; 
        nb_facets= rand()%(10*n+1); 
    
        cout<<"n="<<n<<" nb_facets="<<nb_facets<<"\n";
        int d1,d2;
        vector<face> simpl;
        vector<face> tsimpl;
        face graph;


        for (int v=1; v<n+1; v++){
            graph.push_back(v);
        }


        create_complex(simpl,n,nb_facets,graph);
        extends1(simpl,tsimpl, graph);

        if(tsimpl.size()>8){ //8 takes a few seconds (like 5 sec), but 9 is too much
            cout<<"TK too big, nb of facets of TK="<<tsimpl.size()<<"\nAbort this test\n";
            continue;
        }

        random_shuffle(tsimpl.begin(), tsimpl.end());
      
        cout<<"\nSimplicial complex K de base:\n";
        printvectorf(simpl);


        cout<<"\nT1(K):\n";
        printvectorf(tsimpl);
        cout<<"\n\n";

        
        d1= bestorder(simpl,0,1);
        d2 = bestorder(tsimpl,1,3);

        if(results.find(d1) == results.end()){
            results[d1]=d2;
        }
        else{
            if(results[d1]<d2){
                results[d1]=d2;
            }
        }
        cout<<"\nd1="<<d1<<" d2="<<d2<<"\n";
        
    }

    cout<<"\nthe end\n\n";

    for(auto& r:results){
        cout<<"d1="<<r.first<<" d2="<<r.second<<"\n";
    }
    
    return 0;
}
    