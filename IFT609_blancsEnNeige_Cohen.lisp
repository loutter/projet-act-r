;===============================;
;   Johana Cohen                ;
;   Sandra De La Purification   ;
;   Leo Pecault                 ;
;   Farah Rebiha                ;
;   Aurelien Vauthier           ;
;===============================;

(defvar *window*)

(defvar coquillesGlobal)
(defvar jaunesGlobal)
(defvar succesGlobal)

(defun experiment ()
  (setf coquillesGlobal 0)
  (setf jaunesGlobal 0)
  (setf succesGlobal nil)

  (setf *window* (open-exp-window "Choice Experiment" :visible nil))
    
  (install-device *window*)
  (allow-event-manager *window*)  

  (add-button-to-exp-window :x 20  :y 20 :height 24 :width 24 :text "0" :action 'oeuf-casse)
  (add-button-to-exp-window :x 60  :y 20 :height 24 :width 24 :text "0" :action 'oeuf-casse)
  (add-button-to-exp-window :x 100 :y 20 :height 24 :width 24 :text "0" :action 'oeuf-casse)
  (add-button-to-exp-window :x 140 :y 20 :height 24 :width 24 :text "0" :action 'oeuf-casse)

  (proc-display)  

  (goal-focus-fct (car (define-chunks-fct `((isa butAvoirBlancsEnNeige statut startExperiment)))))
  (define-chunks-fct `((isa butAvoirBlancs nbBlancs 0 nbJaunes 0 nbCoquilles 0 statut startCassage)))
  (define-chunks-fct `((isa butObtenirTexture couleurActuelle 0 volumeActuel 0 vitesseActuelle 0 energie 1100 statut startFouettage)))

  (run-full-time 10)

  
  (list (/ coquillesGlobal 4) (/ jaunesGlobal 4) succesGlobal)
)

(defun learning (n)
  (reset)
  (let (res)    
    (dotimes (i n (reverse res))
      (push (experiment) res)
    )
    ;; (write res)
  )
)

(defun show-learning (n)
  (let ((data nil))
    (dotimes (i n)
      (if (null data)
        (setf data (learning 20))
        (setf data (mapcar (lambda (x y) (mapcar '+ x y)) data (learning 20)))
      )
    )
    (let ((percentages (mapcar (lambda (experience) (mapcar (lambda (x) (/ x n)) experience)) data) ))
      (draw-graph percentages)
    )
  )
)

(defun draw-graph (stats)
  (dotimes (statElement 3)
    (let (points)
      (dotimes (i 20)
        (push (nth statElement (nth i stats)) points)
      )
      (setf points (reverse points))

      (setf graphTitles '("Coquilles" "Jaunes" "Succes"))
      (let ((w (open-exp-window (nth statElement graphTitles) :width 550 :height 460 :visible t)))
        (allow-event-manager w)
        (add-line-to-exp-window '(50 0) '(50 420) :color 'white :window (nth statElement graphTitles))
        (dotimes (i 11)
          (add-text-to-exp-window :x 5 :y (+ 5 (* i 40)) :width 35 :text (format nil "~3,1f" (- 1 (* i .1))) :window (nth statElement graphTitles))
          (add-line-to-exp-window (list 45 (+ 10 (* i 40))) (list 550 (+ 10 (* i 40))) :color 'white :window (nth statElement graphTitles))
        )
        
        (let ((x 50))
          (mapcar 
            (lambda (a b) 
              (add-line-to-exp-window (list x (floor (- 410 (* a 400) ) ) ) (list (incf x 25) (floor (- 410 (* b 400)))) :color 'blue :window (nth statElement graphTitles))
            )
            (butlast points) (cdr points)
          )
        )
        (allow-event-manager w)
      )
    )
  )
)

(defun oeuf-casse (button)
  (remove-items-from-exp-window button)
  (proc-display)  
)

(clear-all)

(define-model oeufs-neige-model 

  (sgp :v nil :show-focus t :trace-detail medium :ul t :ult t)

  ;; do not change these parameters
  (sgp :esc t :egs 0.5 :bll .5 :ol t :er t :lf 0)
  
  ;; adjust these as needed
  (sgp :ans .2 :mp 10.0 :rt -60)
    
  (chunk-type butAvoirBlancsEnNeige statut)
  (chunk-type butAvoirBlancs nbBlancs nbJaunes nbCoquilles statut)
  (chunk-type butObtenirTexture couleurActuelle volumeActuel vitesseActuelle energie statut)

  ;; Chunk modélisant l'oeuf réel, non utilisé dans cette version du code mais permettra une éventuelle adaptation future
  ;;(chunk-type oeuf blanc jaune coquille)
  
  ;; Chunk modélisant une couleur, non utilisé dans cette version du code mais permettra une éventuelle adaptation future
  ;;(chunk-type couleur nom valeur)
  
  ;; Chunk modélisant la texture des blancs 
  (chunk-type texture couleur volume)
  
  ;; Chunk modélisant l'apprentissage du modèle pour casser des oeufs selon une force appliquée, non utilisé dans cette version du code mais permettra une éventuelle adaptation future
  ;;(chunk-type experienceCassage forceActuelle resultat)
  
  ;; Chunk modélisant l'apprentissage du modèle pour fouetter correctement la préparation et monter les blancs en neige
  (chunk-type experienceFouettage couleurActuelle volumeActuel vitesseMouvement resultat)

  (add-dm
    ;; But principal : obtenir des blancs en neige
    (goalAvoirBlancsEnNeige ISA butAvoirBlancsEnNeige statut startExperiment)
   
    ;; Premier sous-but : obtenir uniquement des blancs
    (goalAvoirBlancs ISA butAvoirBlancs nbBlancs 0 nbJaunes 0 nbCoquilles 0 statut startCassage)
   
    ;; Second sous-but : obtenir une texture en neige
    (goalObtenirTexture ISA butObtenirTexture couleurActuelle 0 volumeActuel 0 vitesseActuelle 0 energie 1100 statut startFouettage)

    ;; Exemple de chunk oeuf pour une évolution future
    ;;(oeuf ISA oeuf blanc 1 jaune 1 coquille 1)

    ;; Exemples de chunks couleur pour une évolution future
    ;;(couleur1 ISA couleur nom transparent valeur 0)
    ;;(couleur2 ISA couleur nom blanc valeur 200)

    ;; Chunk de texture parfaite
    (textureParfaite ISA texture couleur 200 volume 100)
   
    (transparent isa chunk) 
    (blanc isa chunk)
    (startExperiment isa chunk) 
    (startCassage isa chunk) 
    (startFouettage isa chunk) 
    (presenceCoquille isa chunk)
    (presenceJaune isa chunk)

    (succes isa chunk) 
    (echec isa chunk) 
    (manqueEnergie isa chunk) 
    (chercherOeuf isa chunk) 
    (oeufRecupere isa chunk) 
    (oeufCasse isa chunk) 
    (oeufSepare isa chunk)
    (oeufAttrape isa chunk)
    (pasDeCoquille isa chunk)
    (blancsObtenus isa chunk)
    (move-mouse isa chunk)
    (recupererVitesse isa chunk)
    (recupererVitesseManqueEnergie isa chunk)
    (augmenterVitesse isa chunk)
    (diminuerVitesse isa chunk)
    (fouetter isa chunk)
    (verifierTexture isa chunk)
    (comparerTexture isa chunk)
    (verifierEnergie isa chunk)
  )

  (start-hand-at-mouse)

  (goal-focus goalAvoirBlancsEnNeige)

  (P start ;; Démarrage du programme : la première chose à faire et de trouver un oeuf pour le casser
    =goal>
      isa         butAvoirBlancsEnNeige
      statut      startExperiment
    ==>
    !bind! =but (goal-focus goalAvoirBlancs)
    =goal>
      statut      startCassage
  )

;;
;; Début du but premier sous-but : Obtenir uniquement des blancs d'oeufs
;;
  
  (P startCassage ;; Préparation de la recherche visuelle d'un oeuf dans la fenêtre graphique
    =goal>
      isa         butAvoirBlancs
      statut      startCassage
    ==>
    +visual-location>
      isa         visual-location
      kind        text
    =goal>
      statut      chercherOeuf
  )

  (P attraperOeuf ;; Cherche visuellement un oeuf dans la fenêtre graphique pour l'attraper
    =goal>
      statut      chercherOeuf
    =visual-location>
    ?visual>
        state      free
    ?manual>
        state      free
    ==>
    =visual-location>
    +visual>
      isa        move-attention
      screen-pos =visual-location
    +manual>
      isa        move-cursor
      loc        =visual-location
    =goal>
      statut      oeufAttrape
  )

 (P casserOeufSansCoquille ;; Modélise le cassage de l'oeuf sans avoir mis de coquille dans la préparation
    =goal>
      statut      oeufAttrape
      nbCoquilles   =nombreCoquilles

    ?manual>
      state  free
    ==>
    +manual>
      isa         click-mouse
    =goal>
      statut      oeufCasse
  )

  (P casserOeufAvecCoquille ;; Modélise le cassage de l'oeuf en ayant fait tomber des coquilles dans la préparation
    =goal>
      statut      oeufAttrape
      nbCoquilles   =nombreCoquilles

    ?manual>
      state  free
    ==>
    !bind! =nouveauNombreCoquille 1
    !bind! =command1 (setf coquillesGlobal (+ coquillesGlobal =nouveauNombreCoquille))
    +manual>
      isa         click-mouse
    =goal>
      statut      oeufCasse
      nbCoquilles   =nouveauNombreCoquille
    !output!        =nouveauNombreCoquille
  )

  (P separerOeufSansJaune ;; Modélise la séparation du blanc et du jaune sans faire tomber de jaune dans la préparation
    =goal>
      statut      oeufCasse
      nbBlancs    =nombreBlancs
      nbJaunes    =nombresJaunes

    ==>
   !bind! =nombre   (+ =nombreBlancs 1)
    =goal>
      nbBlancs    =nombre
      statut      oeufSepare
  )

   (P separerOeufAvecJaune ;; Modélise la séparation du blanc et du jaune en faisant tomber du jaune dans la préparation
    =goal>
      statut      oeufCasse
      nbBlancs    =nombreBlancs
      nbJaunes    =nombresJaunes
    ==>
   !bind! =nombre   (+ =nombreBlancs 1)
   !bind! =nouveauNombreJaunes  (+ =nombresJaunes 1)
   !bind! =command2 (setf jaunesGlobal (+ jaunesGlobal =nouveauNombreJaunes))
    =goal>
      nbJaunes    =nouveauNombreJaunes
      nbBlancs    =nombre
      statut      oeufSepare
  )
  
  (P verifierPasCoquille ;; Indique qu'il n'y a pas de coquille dans la préparation
    =goal>
      statut      oeufSepare
      nbCoquilles =nombreCoquilles
      !eval!  (< =nombreCoquilles 1)
    ==>
    =goal>
      statut      pasDeCoquille

  )
  (P verifierAvecCoquille ;; Indique la présence de coquilles dans la préparation
    =goal>
      statut      oeufSepare
      nbCoquilles =nombreCoquilles
      !eval!  (> =nombreCoquilles 0)
    ==>
    =goal>
      statut      presenceCoquille
  )

  (P retirerCoquille ;; Action de retirer les coquilles de la préparation
    =goal>
    statut presenceCoquille
    nbCoquilles =nombreCoquilles
    > nbCoquilles 0
    ==>
    !bind! =nouveauNombreCoquille   (- =nombreCoquilles 1)
    =goal>
    nbCoquilles =nouveauNombreCoquille
    )

    (P retirerCoquilleTermine ;; Indique qu'il n'y a plus de coquille à retirer pour passer à la suite (séparation du blanc et du jaune)
      =goal>
      statut presenceCoquille
      nbCoquilles 0
      ==>
      =goal>
      statut pasDeCoquille
      )

  (P verifierPasJaune ;; Indique qu'il n'y a pas de jaune dans la préparation
    =goal>
    statut      pasDeCoquille
    nbJaunes    =nombreJaunes
      !eval!  (< =nombreJaunes 1)
    ==>
    =goal>
    statut      startCassage
    -visual>
    -manual>
  )
  
  (P verifierAvecJaune ;; Indique la présence de jaunes dans la préparation
    =goal>
      statut      pasDeCoquille
      nbJaunes    =nombreJaunes
      !eval!  (> =nombreJaunes 0)
    ==>

    =goal>
      statut      presenceJaune
    -visual>
    -manual>
  )

  (P retirerJaune ;; Action de retirer les jaunes de la préparation
    =goal>
    statut presenceJaune
    nbJaunes =nombreJaunes
    > nbJaunes 0
    ==>
    !bind! =nouveauNombreJaunes   (- =nombreJaunes 1)
    =goal>
    nbJaunes =nouveauNombreJaunes
    )

  (P retirerJauneTermine ;; Indique qu'il n'y a plus de jaune à retirer pour passer à la suite (fouettage des blancs)
    =goal>
    statut presenceJaune
    nbJaunes 0
    ==>
    =goal>
    statut      startCassage
    )

  (P passerAuFouettage ;; Permet le changement de but lorsque tous les oeufs sont cassés et séparés
    =goal>
      isa         butAvoirBlancs
      statut      chercherOeuf
    ?visual-location>
      buffer      failure
    ==>
    !bind! =but (goal-focus goalAvoirBlancsEnNeige)
    =goal>
      statut      blancsObtenus
  )
  
  (P demarrerFouettage
    =goal>
      ISA         butAvoirBlancsEnNeige
      statut      blancsObtenus
    ==>
    !bind! =but (goal-focus goalObtenirTexture)
    =goal>
      statut      startFouettage
    -visual>
    -manual>
    -visual-location>
  )

;;
;; Début du second sous-but : Obtenir une texture en neige
;;

  (P recupererVitesseSucces ;; Récupère la vitesse d'une précédente tentative en succès comme référence pour cette run 
    =goal>
      ISA         butObtenirTexture
      statut      startFouettage
    ?retrieval>
      state       free
      buffer      empty
    ==>
    =goal>
      statut      recupererVitesse
    +retrieval>
      ISA         experienceFouettage
      resultat    succes
  )

  (P recupererVitesseSuccesOK ;; Rappel de la vitesse avec la précédente run étant un succès réussi
    =goal>
      ISA         butObtenirTexture
    =retrieval>
      ISA         experienceFouettage
      resultat    succes
      vitesseMouvement =vitesse
    ==>
    =goal>
      statut      fouetter
      vitesseActuelle =vitesse
    @retrieval> ;; Vider le buffer sans impacter l'apprentissage
  )

  (P recupererVitesseSuccesKO ;; Echec du rappel de la vitesse avec la précédente run étant un succès
    =goal>
      ISA         butObtenirTexture
      statut      recupererVitesse
    ?retrieval>
      buffer      failure
    ==>
    =goal>
      statut      recupererVitesseManqueEnergie
    +retrieval>
      ISA         experienceFouettage
      resultat    manqueEnergie 
  )

  (P recupererVitesseManqueEnergieOK ;; Rappel de la vitesse avec la précédente run étant un manque d'énergie réussi
    =goal>
      ISA         butObtenirTexture
    =retrieval>
      ISA         experienceFouettage
      resultat    manqueEnergie
      vitesseMouvement =vitesse
    ==>
    =goal>
      statut      diminuerVitesse
      vitesseActuelle =vitesse
    @retrieval> ;; Vider le buffer sans impacter l'apprentissage
  )

  (P recupererVitesseManqueEnergieKO ;; Echec du rappel de la vitesse, on choisit une vitesse par défaut aléatoire
    =goal>
      ISA         butObtenirTexture
      statut      recupererVitesseManqueEnergie
    ?retrieval>
      buffer      failure
    ==>
    !bind! =vitesse (+ 15 (act-r-random 10))
    =goal>
      statut      fouetter
      vitesseActuelle =vitesse
  )

  (P baisserVitesse ;; Adapte la vitesse d'une précédente run échouée par manque d'énergie en la diminuant légèrement
    =goal>
      ISA         butObtenirTexture
      statut      diminuerVitesse
      vitesseActuelle =vitesse
    ==>
    !bind! =nouvelleVitesse (- =vitesse 3)
    =goal>
      statut      fouetter
      vitesseActuelle =nouvelleVitesse
  )

  (P augmenterVitesse ;; Perfectionne la vitesse d'une précédente run réussie en l'augmentant un peu
    =goal>
      ISA         butObtenirTexture
      statut      augmenterVitesse
      vitesseActuelle =vitesse
    ==>
    !bind! =nouvelleVitesse (+ =vitesse 1)
    =goal>
      statut      fouetter
      vitesseActuelle =nouvelleVitesse
  )

  (P fouetterClassique ;; Foutte de façon classique
    =goal>
      ISA         butObtenirTexture
      statut      fouetter
      vitesseActuelle =vitesse
      volumeActuel    =volume
      couleurActuelle =couleur
      energie         =energie
    ==>
    !bind! =nouveauVolume (+ =volume =vitesse)
    !bind! =nouvelleCouleur (+ =couleur (* =vitesse 2))
    !bind! =nouvelleEnergie (- =energie (* =vitesse =vitesse))
    =goal>
      statut      verifierTexture
      volumeActuel  =nouveauVolume
      couleurActuelle =nouvelleCouleur
      energie         =nouvelleEnergie
  )

  (P verifierTexture ;; Récupère la texture parfaite pour comparer avec la texture actuelle
    =goal>
      ISA         butObtenirTexture
      statut      verifierTexture
      volumeActuel    =volume
      couleurActuelle =couleur
    ==>
    =goal>
      statut      comparerTexture
    +retrieval>
      textureParfaite
  )

  (P comparerTextureOK ;; Rappel de la texture parfaite réussi et comparaison avec la texture actuelle : similaire
    =goal>
      ISA         butObtenirTexture
      statut      comparerTexture
      volumeActuel    =volume
      couleurActuelle =couleur
      vitesseActuelle =vitesse
      energie         =ener
    =retrieval>
      ISA         texture
      volume      =volumeParfait
      couleur     =couleurParfaite
    !eval! (< (- =volumeParfait =volume) 8)
    !eval! (< (- =couleurParfaite =couleur) 15)
    ?imaginal>
      state       free
    ==>
    !bind! =but (goal-focus goalAvoirBlancsEnNeige)
    =goal>
      statut      succes
    +imaginal>
      ISA experienceFouettage
      couleurActuelle   =couleur
      volumeActuel      =volume
      vitesseMouvement  =vitesse
      resultat          succes
  )

  (P comparerTextureKO ;; Rappel de la texture parfaite réussi et comparaison avec la texture actuelle : différente
    =goal>
      ISA         butObtenirTexture
      statut      comparerTexture
      volumeActuel    =volume
      couleurActuelle =couleur
      vitesseActuelle =vitesse
    =retrieval>
      ISA         texture
      volume      =volumeParfait
      couleur     =couleurParfaite
    !eval! (>= (- =volumeParfait =volume) 8)
    !eval! (>= (- =couleurParfaite =couleur) 15)
    ==>
    =goal>
      statut      verifierEnergie
  )

  (P verifierEnergieKO ;; Le modèle n'a plus d'énergie, arrêt du fouettage
    =goal>
      ISA         butObtenirTexture
      statut      verifierEnergie
      couleurActuelle =couleur
      volumeActuel    =volume
      vitesseActuelle =vitesse
      <= energie     0
    ?imaginal>
      state       free
    ==>
    !bind! =but (goal-focus goalAvoirBlancsEnNeige)
    =goal>
      statut      echec
    +imaginal>
      ISA experienceFouettage
      couleurActuelle   =couleur
      volumeActuel      =volume
      vitesseMouvement  =vitesse
      resultat          manqueEnergie
  )

  (P verifierEnergieOK ;; Il reste encore de l'énergie au modèle, on continue à fouetter
    =goal>
      ISA         butObtenirTexture
      statut      verifierEnergie
      > energie     0
    ==>
    =goal>
      statut      fouetter
  )

  (P finirExperienceEchec ;; On termine l'expérience en vidant le buffer imaginaire pour finaliser l'apprentissage
    =goal>
      ISA         butAvoirBlancsEnNeige
      statut      echec
    ?imaginal>
       state free
       buffer full
     ==>
     -imaginal>
    !bind! =command3 (setf succesGlobal 0)
  )

  (P finirExperienceSucces ;; On termine l'expérience en vidant le buffer imaginaire pour finaliser l'apprentissage
    =goal>
      ISA         butAvoirBlancsEnNeige
      statut      succes
    ?imaginal>
       state free
       buffer full
     ==>
     -imaginal>
    !bind! =command3 (setf succesGlobal 1)
  )

  (spp casserOeufSansCoquille :reward 3)
  (spp separerOeufSansJaune :reward 3)
  (spp casserOeufAvecCoquille :reward 1)
  (spp separerOeufAvecJaune :reward 1)
)
