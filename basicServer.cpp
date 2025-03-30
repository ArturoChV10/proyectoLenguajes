#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <cstring>
#include <signal.h>

using namespace std;

/*
 * Función que maneja la comunicación con un cliente específico.
 * Se ejecuta en un proceso hijo creado por fork().
 */
void handleClient(int clientSocket) {
    while (true) {
        char buffer[1024];
        memset(buffer, 0, sizeof(buffer)); //limpiar buffer

        // Espera para recibir mensajes del cliente
        // recv() recibe el mensaje del cliente y lo almacena en el buffer
        int bytesReceived = recv(clientSocket, buffer, sizeof(buffer) - 1, 0); 
        // bytesReceived almacena la cantidad de bytes recibidos y sizeof(buffer) - 1 se utiliza para dejar espacio para el terminador nulo
        // 0 indica que no se está utilizando ninguna bandera especial
        if (bytesReceived <= 0) {
            cout << "Cliente desconectado." << endl;
            break;
        }

        // Muestra el mensaje recibido
        cout << "Mensaje del cliente: " << buffer << endl;

        // Si alguno de los clientes escribe Cerrar, se cierra la conexión
        if (strcmp(buffer, "Cerrar") == 0) {
            cout << "Cerrando conexión con cliente." << endl;
            break;
        }

        // Confirmacion del mensaje recibido
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
   signal(SIGCHLD, SIG_IGN); // Ignorar señales de procesos hijos terminados

   // Crear el socket del servidor
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
    Ciclo principal del servidor que acepta conexiones de clientes y crea un proceso hijo por cada cliente para manejarlas.
    */
    while (true) {
        // Aceptar la conexión de un cliente
        int clientSocket = accept(serverSocket, nullptr, nullptr);
        if (clientSocket == -1) {
            cerr << "Error al aceptar conexión" << endl;
            continue;
        }

        cout << "Cliente conectado!" << endl;

        // Con ayuda de gpt se implementa fork(), aqui se crea un proceso hijo para manejar al cliente
        pid_t pid = fork();

        if (pid == 0) {
            // Proceso hijo
            close(serverSocket); // El hijo no necesita el socket del servidor ya que no acepta mas conexiones
            handleClient(clientSocket); // logica de recepción y envío de mensajes al cliente
            return 0; // Termina el hijo después de atender al cliente

        } else if (pid > 0) {
            // Proceso padre
            close(clientSocket); // El padre no atiende directamente al cliente, por ende se cierra el socket

        } else {
            cerr << "Error al hacer fork()" << endl;
        }
    }

    close(serverSocket);
    return 0;
}