package org.psug.usi.rest

/*
 * Login
 *
 *  * URI : .../api/login
 *  * méthode POST
 *  * Paramètres d'entrée 
 *
 * {
 *   "email" : "string",
 *   "password" : "string"
 * }
 * 
 *  * Codes de retour
 *        o OK : CREATED 201
 *        o Si l'utilisateur n'existe pas ou si l'utilisateur est déjà loggé : 400 
 *        
 *  * Commentaire : lorsque le code de retour est OK, un cookie de session non persistant, nommé 
 *    'session_key' est placé dans l'entête HTTP. La valeur de ce cookie est une clé générée pour 
 *    authentifier l'utilisateur lors des différents appels faits pendant la suite du jeu.
 */


class LoginApi {
  
  //TODO
  
}