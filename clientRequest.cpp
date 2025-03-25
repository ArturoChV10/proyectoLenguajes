#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <cstring>

using namespace std;

int main() {
    // Crear el socket del cliente
    /*
    Para crear un socket se utiliza socket(), los parámetros que se pasaron 
    tienen diferentes funcionalidades AF_INET especifica que se utilizará el
    protocolo IPv4 (para conexiones de dispositivos con internet), y SOCK_STREAM
    define un servidor TCP, de manera general se recomienda usar un servidor UDP,
    pero considero que TCP es más simple y encontré más información acerca de
    */
    int clientSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (clientSocket == -1) {
        cerr << "Error al crear el socket del cliente" << endl;
        return 1;
    }

    // Configurar la dirección del servidor
    sockaddr_in serverAddress; // sockaddr_in tipo de dato para guardar direcciones de sockets (socket address in)
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_port = htons(8080); // Convierte enteros a bytes de red, se utiliza para definir el puerto

    // Convertir la IP del servidor
    if (inet_pton(AF_INET, "127.0.0.1", &serverAddress.sin_addr) <= 0) { // inet_pton convierte una dirección IPv4 es texto
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

    /*
    Se solicita nombre de usuario, la idea de esto es simular el uso de usuarios,
    de manera que cuando validemos usuarios solo debamos enviar esta variable 
    username y una variable password a revisar 
    */
    string username;
    cout << "Ingrese su nombre de usuario: ";
    getline(cin, username);

    while (true) { // Ciclo para solicitar N mensajes
        // Pedir mensaje al usuario
        string message;
        cout << username << ": ";
        getline(cin, message);

        // Formatear el mensaje con el nombre del usuario
        string fullMessage = username + ": " + message;

        // Enviar mensaje al servidor
        if (send(clientSocket, fullMessage.c_str(), fullMessage.length(), 0) == -1) {
            cerr << "Error al enviar mensaje" << endl;
            break;
        }

        // Verificar si el usuario quiere cerrar el servidor
        if (message == "Cerrar") {
            cout << "Solicitaste cerrar el servidor." << endl;
            break;
        }

        // Recibir respuesta del servidor
        char buffer[1024];
        /*
        Antes de manera el buffer se "reserva" la memoria estableciendo 0
        */
        memset(buffer, 0, sizeof(buffer)); 
        int bytesReceived = recv(clientSocket, buffer, sizeof(buffer), 0);
        if (bytesReceived == -1) {
            cerr << "Error al recibir datos del servidor" << endl;
        } else {
            cout << "Servidor: " << buffer << endl;
        }
    }

    // Cerrar el socket del cliente
    close(clientSocket);
    return 0;
}
