#include <iostream>
#include <string>

using namespace std;

// Tamaño del array de usuarios
const int arraySize = 5;

struct user {
	int ID;
	string username;
	string password;
}usersArray[arraySize];

// Function used in testing phase, allow us to initialize the array in an simple way
void initializeUsers() {
	cout << "Ingrese los datos de " << arraySize << " usuarios:" << endl;
	for (int i = 0; i < arraySize; i++) {
		cout << "Usuario " << i + 1 << ":\n";
		usersArray[i].ID = i + 1;
		cout << "Nombre de usuario: ";
		cin >> usersArray[i].username;
		cout << "Contraseña: ";
		cin >> usersArray[i].password;
	}
}

// Checks if an user is registered in the array
bool checkUsers(string username, string password) {
	int index = 0;
	while(index < arraySize) {
		if (username == usersArray[index].username && password == usersArray[index].password) {
			return true;
		}
		index++;
	}
	return false;
}

int main() {
	/*
	initializeUsers();
	if (checkUsers("Marco", "Marco")) {
		cout << "Funciona" << endl;
	}
	else {
		cout << "No" << endl;
	}
    */
   cout << "test" << endl;
	return 0;
}