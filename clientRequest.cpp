#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <cstring>
#include <thread>

using namespace std;

// Función para recibir mensajes del servidor
void receiveMessages(int clientSocket) {
    while (true) {
        char buffer[1024];
        memset(buffer, 0, sizeof(buffer));

        int bytesReceived = recv(clientSocket, buffer, sizeof(buffer), 0);
        if (bytesReceived == -1) {
            cerr << "Error al recibir datos del servidor" << endl;
        } else if (bytesReceived == 0) {
            cout << "El servidor ha cerrado la conexión." << endl;
            break;
        } else {
            cout << "\n" << "Servidor: " << buffer << endl;
        }
    }

    // Cerrar el socket del cliente
    close(clientSocket);
}

int main() {
    // Crear el socket del cliente
    int clientSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (clientSocket == -1) {
        cerr << "Error al crear el socket del cliente" << endl;
        return 1;
    }

    // Configurar la dirección del servidor
    sockaddr_in serverAddress;
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_port = htons(8080); // Puerto del servidor

    // Convertir la IP del servidor
    if (inet_pton(AF_INET, "127.0.0.1", &serverAddress.sin_addr) <= 0) {
        cerr << "Dirección IP no válida o no compatible" << endl;
        close(clientSocket);
        return 1;
    }

    // Conectar al servidor
    if (connect(clientSocket, (struct sockaddr*)&serverAddress, sizeof(serverAddress)) == -1) {
        cerr << "Error al conectar con el servidor" << endl;
        close(clientSocket);
        return 1;
    }

    cout << "Conectado al servidor" << endl;

    // Crear un hilo para recibir mensajes del servidor
    thread receiveThread(receiveMessages, clientSocket);

    // Pedir nombre de usuario
    string username;
    cout << "Ingrese su nombre de usuario: ";
    getline(cin, username);

    // Bucle para enviar mensajes al servidor
    while (true) {
        // Almacena el nombre de usuario del usuario al que desea enviar mensaje
        string receiver;
        cout << "Receptor del mensaje: ";
        getline(cin, receiver);

        // Almacena el mensaje que se desea enviar
        string message;
        cout << username << ": ";
        getline(cin, message);

        // Formatear el mensaje con el nombre del usuario
        string fullMessage = username + " TO " + receiver + ": " + message;

        // Enviar el mensaje al servidor
        if (send(clientSocket, fullMessage.c_str(), fullMessage.length(), 0) == -1) {
            cerr << "Error al enviar mensaje" << endl;
            break;
        }

        // Verificar si el usuario quiere cerrar el servidor
        if (message == "Cerrar") {
            cout << "Solicitaste cerrar la conexión." << endl;
            break;
        }
    }

    // Unir el hilo de recepción para esperar que termine
    receiveThread.join();

    // Cerrar el socket del cliente
    close(clientSocket);
    return 0;
}
