#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <cstring>

using namespace std;

int main()
{
    // Creación del socket
    int serverSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (serverSocket == -1) {
        cerr << "Error al crear el socket" << endl;
        return 1;
    }

    // Especificar la dirección del servidor
    sockaddr_in serverAddress;
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_port = htons(8080);
    serverAddress.sin_addr.s_addr = INADDR_ANY;

    // Asociar el socket a la dirección y puerto
    if (bind(serverSocket, (struct sockaddr*)&serverAddress, sizeof(serverAddress)) == -1) {
        cerr << "Error al hacer bind()" << endl;
        close(serverSocket);
        return 1;
    }

    // Escuchar conexiones entrantes
    if (listen(serverSocket, 5) == -1) {
        cerr << "Error en listen()" << endl;
        close(serverSocket);
        return 1;
    }

    cout << "Esperando conexiones en el puerto 8080..." << endl;

    // Aceptar conexión de un cliente
    int clientSocket = accept(serverSocket, nullptr, nullptr);
    if (clientSocket == -1) {
        cerr << "Error al aceptar conexión" << endl;
        close(serverSocket);
        return 1;
    }

    // Recibir datos del cliente
    char buffer[1024];
    memset(buffer, 0, sizeof(buffer)); // Inicializar buffer
    int bytesReceived = recv(clientSocket, buffer, sizeof(buffer), 0);
    if (bytesReceived == -1) {
        cerr << "Error en recv()" << endl;
    } else {
        cout << "Mensaje del cliente: " << buffer << endl;
    }

    // Enviar respuesta al cliente
    const char* response = "Mensaje recibido por el servidor";
    send(clientSocket, response, strlen(response), 0);

    // Cerrar los sockets
    close(clientSocket);
    close(serverSocket);

    return 0;
}