//Codificado por: sAfOrAs
//Matricula
using namespace std;
int main()
{
    int ncredisp, ncred, ncredmat=0, ncursos=0,ciclo;
    char i;
    string nom;
    cout<<"Nombre del alumno: ";cin>>nom;
    IngreseCiclo:
    cout<<"Ingrese ciclo: ";cin>>ciclo;
     
    switch(ciclo)
    {
        case 1: case 4: case 7:
        {
            ncredisp=20;
            cout<<"Creditos disponibles: "<<ncredisp<<endl;
             
            do
            {
                IngreseCreditos:
                do
                {
                    cout<<"Ingrese la cantidad creditos del curso: ";cin>>ncred;
                    if(ncred<=0||ncred>ncredisp)
                    {
                         
                        cout<<"La cantidad de creditos debe ser mas de 0 y menor-igual que el total de creditos disponibles"<<endl;
                        sn:
                        cout<<"Desea ingresar un valor valido (Si:S/s) (No:N/n): ";cin>>i;
                        decide:
                        switch(i)
                        {
                        case 's':case'S': goto IngreseCreditos;
                        case 'n':case'N':   {if(ncredmat>=12) goto notificar;
                                            else exit(0);
                                            }break;
                        default: cout<<"Invalido\n";goto sn;
                        }
                    }
                    ncredisp=ncredisp-ncred;
                     
                    cout<<"Credito  Restante: "<<ncredisp<<endl;
                    ncredmat=ncredmat+ncred;
                    ncursos++;
                }while(ncredmat<12);
                 
                while(ncredisp>0)
                {       
                cout<<"Desea matricularse en algun curso mas (Si:S/s) (No:N/n): ";cin>>i;
                goto decide;
                }
            }while(ncred<0&&ncred>ncredisp);
        }break;
        case 2: case 3: case 8:
            {
                ncredisp=22;
                cout<<"Creditos disponibles: "<<ncredisp<<endl;
                goto IngreseCreditos;
 
            }break;
        case 5: case 6: case 9: case 10:
            {
                ncredisp=24;
                cout<<"Creditos disponibles: "<<ncredisp<<endl;
                goto IngreseCreditos;
 
            }break;
        default:
                {
                    cout<<"Desea ingresar un valor valido s/S n/N: ";cin>>i;
                    switch(i)
                    {
                        case 's': case 'S':goto IngreseCiclo;
                        case 'N': case 'n':exit(0);
                        default: cout<<"respuesta invalida"<<endl;
                    }
                }
     
    }
    notificar:
    cout<<"\n\nREPORTE\n\n";
    cout<<"Nombre del alumno: "<<nom<<endl;
    cout<<"Ciclo del alumno: "<<ciclo<<endl;
    cout<<"Total de creditos matriculados: "<<ncredmat<<endl;
    cout<<"Total de creditos disponibles: "<<ncredisp<<endl;
    cout<<"Total de cursos matriculados: "<<ncursos<<endl;
 
cin.ignore(); return 0;
}