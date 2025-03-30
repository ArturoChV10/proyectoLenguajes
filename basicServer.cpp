#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <cstring>
#include <signal.h>

using namespace std;


void handleClient(int clientSocket) {
    while (true) {
        char buffer[1024];
        memset(buffer, 0, sizeof(buffer));

        int bytesReceived = recv(clientSocket, buffer, sizeof(buffer) - 1, 0);
        if (bytesReceived <= 0) {
            cout << "Cliente desconectado." << endl;
            break;
        }

        cout << "Mensaje del cliente: " << buffer << endl;

        if (strcmp(buffer, "Cerrar") == 0) {
            cout << "Cerrando conexión con cliente." << endl;
            break;
        }

        const char* response = "Mensaje recibido por el servidor";
        send(clientSocket, response, strlen(response), 0);
    }

    close(clientSocket);
}
int main() {
    // Crear el socket del servidor
    /*
    Para crear un socket se utiliza socket(), los parámetros que se pasaron 
    tienen diferentes funcionalidades AF_INET especifica que se utilizará el
    protocolo IPv4 (para conexiones de dispositivos con internet), y SOCK_STREAM
    define un servidor TCP, de manera general se recomienda usar un servidor UDP,
    pero considero que TCP es más simple y encontré más información acerca de
    */
   signal(SIGCHLD, SIG_IGN);

    int serverSocket = socket(AF_INET, SOCK_STREAM, 0);

    // Validación de creación
    if (serverSocket == -1) {
        cerr << "Error al crear el socket" << endl;
        return 1;
    }

    // Configurar la dirección del servidor
    sockaddr_in serverAddress; // sockaddr_in tipo de dato para guardar direcciones de sockets (socket address in)
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_port = htons(8080); // Convierte enteros a bytes de red, se utiliza para definir el puerto
    serverAddress.sin_addr.s_addr = INADDR_ANY; // Para no vincular el socket a una sola IP

    // Asociar el socket al puerto
    if (bind(serverSocket, (struct sockaddr*)&serverAddress, sizeof(serverAddress)) == -1) {
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
    La idea del doble while fue gpt, tenemos que considerar si es necesario, ya
    que recurrí en busca de como mantener el servidor abierto para múltiples mensajes
    */
    while (true) {
        // Aceptar la conexión de un cliente
        // Esto tenemos que verlo, porque debe aceptar a N clientes
        int clientSocket = accept(serverSocket, nullptr, nullptr);
        if (clientSocket == -1) {
            cerr << "Error al aceptar conexión" << endl;
            continue;
        }

        cout << "Cliente conectado!" << endl;

        // Crear un proceso hijo para manejar al cliente
        pid_t pid = fork();

        if (pid == 0) {
            // Proceso hijo
            close(serverSocket); // El hijo no necesita el socket del servidor
            handleClient(clientSocket);
            return 0; // Termina el hijo después de atender al cliente

        } else if (pid > 0) {
            // Proceso padre
            close(clientSocket); // El padre no atiende directamente al cliente
            
        } else {
            cerr << "Error al hacer fork()" << endl;
        }
    }

    close(serverSocket);
    return 0;
}