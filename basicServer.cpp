#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <cstring>
#include <signal.h>
#include <vector>
#include <pthread.h>
#include <algorithm>

using namespace std;

vector<int> clientSockets; // Lista de sockets de clientes conectados
pthread_mutex_t clientSocketsMutex = PTHREAD_MUTEX_INITIALIZER; // Mutex para acceso seguro a la lista de sockets
/*
Básicamente lo que hace mutex es impedir que dos usarios/clientes actualicen la lista al mismo tiempo,
esto es especialmente útil cuando se manejan inserciones de datos desde múltiples threads
*/

// Función para enviar un mensaje a todos los clientes
// Esta función se encarga de 
void broadcastMessage(const string &message, int senderSocket) {
    pthread_mutex_lock(&clientSocketsMutex);
    for (int clientSocket : clientSockets) { // Recorrerá hasta el último elemento de clientSockets
        if (clientSocket != senderSocket) { // No enviar el mensaje al cliente que lo envió
            send(clientSocket, message.c_str(), message.length(), 0);
        }
    }
    pthread_mutex_unlock(&clientSocketsMutex);
}

// Función que maneja la comunicación con un cliente específico
void handleClient(int clientSocket) {
    char buffer[1024];
    string message;

    while (true) {
        memset(buffer, 0, sizeof(buffer)); // Limpiar buffer

        // Recibir mensaje del cliente
        int bytesReceived = recv(clientSocket, buffer, sizeof(buffer) - 1, 0);
        // bytesReceived almacena la cantidad de bytes recibidos y sizeof(buffer) - 1 se utiliza para dejar espacio para el terminador nulo
        // 0 indica que no se está utilizando ninguna bandera especial
        if (bytesReceived <= 0) {
            cout << "Cliente desconectado." << endl;
            break;
        }

        message = string(buffer);
        cout << "Mensaje del cliente: " << message << endl;

        // Si el mensaje es "Cerrar", cerrar la conexión
        if (message == "Cerrar") {
            cout << "Cliente pidió cerrar la conexión." << endl;
            break;
        }

        // Enviar el mensaje a todos los clientes
        broadcastMessage(message, clientSocket);
    }

    // Eliminar el cliente de la lista y cerrar la conexión
    pthread_mutex_lock(&clientSocketsMutex);
    auto client = find(clientSockets.begin(), clientSockets.end(), clientSocket);
    if (client != clientSockets.end()) {
        clientSockets.erase(client);
    }
    pthread_mutex_unlock(&clientSocketsMutex);

    close(clientSocket);
}

// Función que maneja la conexión con el cliente
void *clientHandler(void *arg) {
    int clientSocket = *(int *)arg;
    free(arg); // Liberar memoria del argumento pasado al hilo
    handleClient(clientSocket);
    return nullptr;
}

int main() {
    signal(SIGCHLD, SIG_IGN); // Ignorar señales de procesos hijos terminados
    // Crear el socket del servidor
     /*
     Para crear un socket se utiliza socket(), los parámetros que se pasaron 
     tienen diferentes funcionalidades AF_INET especifica que se utilizará el
     protocolo IPv4 (para conexiones de dispositivos con internet), y SOCK_STREAM
     define un servidor TCP, de manera general se recomienda usar un servidor UDP,
     pero considero que TCP es más simple y encontré más información acerca de
     */
    int serverSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (serverSocket == -1) {
        cerr << "Error al crear el socket del servidor" << endl;
        return 1;
    }

    sockaddr_in serverAddress; // sockaddr_in tipo de dato para guardar direcciones de sockets (socket address in)
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_port = htons(8080); // Convierte enteros a bytes de red, se utiliza para definir el puerto
    serverAddress.sin_addr.s_addr = INADDR_ANY; // Usado para no vincular el socket a una sola IP

    // Asociar el socket al puerto
    if (bind(serverSocket, (struct sockaddr *)&serverAddress, sizeof(serverAddress)) == -1) {
        cerr << "Error en bind()" << endl;
        close(serverSocket);
        return 1;
    }

    // Escuchar conexiones entrantes
    if (listen(serverSocket, 5) == -1) {
        cerr << "Error en listen()" << endl;
        close(serverSocket);
        return 1;
    }

    cout << "Servidor esperando conexiones en el puerto 8080..." << endl;
    /*
     Ciclo principal del servidor que acepta conexiones de clientes y crea un proceso hijo por cada cliente para manejarlas.
     */
    while (true) {
        // Aceptar un cliente
        int clientSocket = accept(serverSocket, nullptr, nullptr);
        if (clientSocket == -1) {
            cerr << "Error al aceptar conexión" << endl;
            continue;
        }

        // Añadir el nuevo cliente a la lista
        pthread_mutex_lock(&clientSocketsMutex);
        clientSockets.push_back(clientSocket);
        pthread_mutex_unlock(&clientSocketsMutex);

        cout << "Cliente conectado!" << endl;

        // Crear un hilo para manejar al cliente
        pthread_t clientThread;
        int *clientSocketPtr = (int *)malloc(sizeof(int));
        *clientSocketPtr = clientSocket;
        pthread_create(&clientThread, nullptr, clientHandler, clientSocketPtr);
        pthread_detach(clientThread); // Desprender el hilo para que se limpie automáticamente
    }

    close(serverSocket);
    return 0;
}

