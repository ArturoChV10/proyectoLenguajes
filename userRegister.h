/* Fernando Sánchez Hidalgo - 2022218688
* Lenguajes de programación S1.2025
*/

#ifndef USERREGISTER_H
#define USERREGISTER_H

#include <iostream>
#include <fstream>
#include <functional>
#include <string>

using namespace std;

// Declaración de funciones
bool validateUser(string username);
bool validatePassword(string pass);
int saveFile(string username, string password);
int registerUser();
bool LoginUser(string username, string password);

// Definición de funciones
bool validateUser(string username) {
    //valida que el nombre de usuario no exista en la base de datos
    ifstream file("users.txt");
    string line, storedUser;

    if (file.is_open()) {
        while (getline(file, line)) {
            size_t pos = line.find(";"); // encuentra el ; que separa usuario;contraseña
            if (pos != string::npos) {
                storedUser = line.substr(0, pos); // obtiene el nombre de usuario
                if (storedUser == username) {
                    cout << "Ese nombre de usuario ya existe, intentelo de nuevo \n";
                    return false;
                }
            }
        }
        file.close();
    }
    return true;
}

bool validatePassword(string pass){
    // valida que la contraseña sea de al menos 8 caracteres, contenga una mayuscula y un digito
    bool isLong = true;
    if (pass.length() < 8) {
        isLong =  false;
    }
    
    bool hasUpper = false, hasDigit = false;
    
    for (char c : pass) {
        if (isupper(c)) hasUpper = true;
        if (isdigit(c)) hasDigit = true;
    }

    if (!(isLong && hasUpper && hasDigit)){
        cout << "La contrasena debe ser de al menos 8 caracteres, contener una mayuscula y un digito \n";
    }

    return isLong && hasUpper && hasDigit; // si alguno da falso, la funcion retorna falso
}

int saveFile(string username, string password){
    //guarda el registro en users.txt en el formato usuario;contraseña. La contraseña es guardada hasheada
    ofstream file("users.txt", ios::app); 

    hash<string> hasher;
    string hPassword = to_string(hasher(password));

    if (file.is_open()) {
        file << username << ";" << hPassword << endl;
        file.close();
        cout << "Usuario registrado exitosamente. \n";
    }
    else {
        cout << "Error al registrar el usuario, intentlo de nuevo. \n";
    }
    return 1;
}

/* Funcion que maneja el registro de usuarios */
int registerUser(){
    string user[2];

    bool validUser;
    do {
        cout << "Cree un nombre de usuario: ";
        cin >> user[0];
        validUser = validateUser(user[0]);
    } while (!validUser);
    
    
    bool validPass;
    do {
        cout << "Cree una contrasena: ";
        cin >> user[1];
        validPass = validatePassword(user[1]);
    } while (!validPass);

    saveFile(user[0], user[1]);
    cout << "Usuario: " << user[0] << " Contrasena: " << user[1];
    return 1;
}

bool LoginUser(string username, string password) {
    //recibe un usuario y contraseña, retorna true si el usuario y contraseña coinicen, retorna false si no coinciden el usuario y la contraseña
    ifstream file("users.txt");
    string line, storedUser, storedPass;

    hash<string> hasher;
    string hPassword = to_string(hasher(password)); //hashea la contraseña ingresada para comparar con la contraseña guardada

    if (file.is_open()) {
        while (getline(file, line)) {
            size_t pos = line.find(";");
            if (pos != string::npos) {
                storedUser = line.substr(0, pos);
                storedPass = line.substr(pos + 1);

                if (storedUser == username && storedPass == hPassword) {
                    cout << "Bienvenido, " << username << "!\n";
                    return true;
                }
            }
        }
        file.close();
    }
    cout << "Nombre de usuario o contraseña incorrectos. \n";
    return false;
}

#endif