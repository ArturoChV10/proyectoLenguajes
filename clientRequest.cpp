#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <cstring>

using namespace std;

int main()
{
    // Crear el socket del cliente
    int clientSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (clientSocket == -1) {
        cerr << "Error al crear el socket del cliente" << endl;
        return 1;
    }

    // Configurar la dirección del servidor
    sockaddr_in serverAddress;
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_port = htons(8080);

    // Convertir la IP del servidor
    if (inet_pton(AF_INET, "127.0.0.1", &serverAddress.sin_addr) <= 0) {
        cerr << "Dirección IP no válida o no compatible" << endl;
        close(clientSocket);
        return 1;
    }

    // Enviar solicitud de conexión
    if (connect(clientSocket, (struct sockaddr*)&serverAddress, sizeof(serverAddress)) == -1) {
        cerr << "Error al conectar con el servidor" << endl;
        close(clientSocket);
        return 1;
    }

    cout << "Conectado al servidor" << endl;

    // Enviar datos al servidor
    const char* message = "Hello, server!";
    if (send(clientSocket, message, strlen(message), 0) == -1) {
        cerr << "Error al enviar mensaje" << endl;
        close(clientSocket);
        return 1;
    }

    cout << "Mensaje enviado al servidor" << endl;

    // Recibir respuesta del servidor
    char buffer[1024];
    memset(buffer, 0, sizeof(buffer));
    int bytesReceived = recv(clientSocket, buffer, sizeof(buffer), 0);
    if (bytesReceived == -1) {
        cerr << "Error al recibir datos del servidor" << endl;
    } else {
        cout << "Respuesta del servidor: " << buffer << endl;
    }

    // Cerrar el socket del cliente
    close(clientSocket);

    return 0;
}