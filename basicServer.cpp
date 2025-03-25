#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <cstring>

using namespace std;

int main() {
    // Crear el socket del servidor
    /*
    Para crear un socket se utiliza socket(), los parámetros que se pasaron 
    tienen diferentes funcionalidades AF_INET especifica que se utilizará el
    protocolo IPv4 (para conexiones de dispositivos con internet), y SOCK_STREAM
    define un servidor TCP, de manera general se recomienda usar un servidor UDP,
    pero considero que TCP es más simple y encontré más información acerca de
    */
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
        /*
        Este es el ciclo while que permite enviar N mensajes
        */
        while (true) {
            char buffer[1024];
            memset(buffer, 0, sizeof(buffer)); // Limpiar el buffer

            // Recibir mensaje del cliente
            int bytesReceived = recv(clientSocket, buffer, sizeof(buffer) - 1, 0); // recv() es la función que recibe mensajes del cliente
            if (bytesReceived <= 0) {
                cout << "Cliente desconectado." << endl;
                break;
            }

            // Mostrar mensaje recibido
            cout << "Mensaje del cliente: " << buffer << endl;

            // Verificar si el mensaje es "Cerrar" y terminar el servidor
            if (strcmp(buffer, "Cerrar") == 0) { //strcmp (string compare)
                cout << "El servidor se está cerrando..." << endl;
                close(clientSocket);
                close(serverSocket);
                return 0;
            }

            // Enviar confirmación al cliente
            const char* response = "Mensaje recibido por el servidor";
            /*
            Envia un mensaje al usuario, esto podemos aprovecharlo, enviando un
            mensaje con la estructura <receptor><-<remiente>:<mensaje>, lo cual
            nos permite enviar un mensaje a todos los usuarios y luego verificar
            quien cumple las condiciones para recibirlo (obviamente esto no es
            para nada eficiente, es como que whatsapp mande el mismo mensaje a
            2000 millones de personas y luego valide para que solo se muestre a
            una, pero es una buena manera de cubrir el problema)
            */
            send(clientSocket, response, strlen(response), 0);
        }

        // Cerrar la conexión con el cliente y esperar otro
        close(clientSocket);
    }

    return 0;
}
