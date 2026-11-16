// Sauvegarde de la session dans le stockage local du navigateur.
//
// Le serveur ne conserve rien : il envoie ici, à chaque modification, le
// contenu complet de la session (le même JSON que le fichier exporté). À la
// connexion, la page annonce au serveur si une session s'y trouve ; elle n'en
// transmet le contenu que lorsque l'utilisateur choisit de la reprendre.
//
// `window.lcaSessionNs` (préfixe des inputs du module d'accueil) est posé par
// app_ui() avant le chargement de ce fichier.
(function () {
  'use strict';

  var ns = window.lcaSessionNs || '';
  // Clé propre à l'adresse de l'application : sur un serveur qui héberge
  // plusieurs applications sous le même domaine, le stockage local est commun.
  var prefixe = 'lca-validate:' + window.location.pathname + ':';
  var fermee = false;

  function lire(cle) {
    try {
      return window.localStorage.getItem(prefixe + cle);
    } catch (e) {
      return null;
    }
  }

  // Navigation privée, stockage désactivé ou bloqué par une politique.
  function stockageDisponible() {
    try {
      window.localStorage.setItem(prefixe + 'test', '1');
      window.localStorage.removeItem(prefixe + 'test');
      return true;
    } catch (e) {
      return false;
    }
  }

  function metaEnregistree() {
    if (!lire('session')) return null;
    try {
      return JSON.parse(lire('meta'));
    } catch (e) {
      return {};
    }
  }

  function fuseau() {
    try {
      return Intl.DateTimeFormat().resolvedOptions().timeZone || '';
    } catch (e) {
      return '';
    }
  }

  $(document).on('shiny:sessioninitialized', function () {
    Shiny.setInputValue(ns + 'navigateur', {
      disponible: stockageDisponible(),
      meta: metaEnregistree(),
      fuseau: fuseau()
    });
  });

  Shiny.addCustomMessageHandler('lca-session-enregistrer', function (message) {
    if (fermee) return;
    var maintenant = new Date();
    var ok = true;
    try {
      var meta = message.meta || {};
      meta.date = maintenant.toLocaleString('fr-FR', { dateStyle: 'short', timeStyle: 'short' });
      window.localStorage.setItem(prefixe + 'session', message.json);
      window.localStorage.setItem(prefixe + 'meta', JSON.stringify(meta));
    } catch (e) {
      // Stockage plein ou indisponible : le serveur en informe l'utilisateur.
      ok = false;
    }
    Shiny.setInputValue(ns + 'sauvegarde', {
      ok: ok,
      heure: maintenant.toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })
    }, { priority: 'event' });
  });

  Shiny.addCustomMessageHandler('lca-session-lire', function (message) {
    Shiny.setInputValue(ns + 'contenu_navigateur', lire('session') || '', { priority: 'event' });
  });

  Shiny.addCustomMessageHandler('lca-session-effacer', function (message) {
    try {
      window.localStorage.removeItem(prefixe + 'session');
      window.localStorage.removeItem(prefixe + 'meta');
    } catch (e) { /* rien à effacer */ }
    if (message.recharger) {
      // Plus aucune écriture ne doit suivre l'effacement avant le rechargement.
      fermee = true;
      window.location.reload();
    }
  });
})();

// Accusé de réception d'un chargement de question (Comparaison) : renvoie au
// serveur le numéro reçu. Le message est traité après les mises à jour
// d'inputs de la même réponse du serveur : quand l'accusé arrive, les champs
// sont ceux de la question chargée (cf. saisie_a_jour(), mod_comparaison.R).
Shiny.addCustomMessageHandler('lca-accuse', function (message) {
  Shiny.setInputValue(message.id, message.valeur, { priority: 'event' });
});
