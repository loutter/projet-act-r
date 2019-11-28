;===============================;
;   Johana Cohen                ;
;   Sandra De La Purification   ;
;   Leo Pecault                 ;
;   Farah Rebiha                ;
;   Aurelien Vauthier           ;
;===============================;

(defvar *window*)

(defun show-learning (n &optional (graph t) (game 'game0))
  (let ((data nil))
    (dotimes (i n)
      (reset)
      (if (null data)
          (setf data (experiment))
        (setf data (mapcar (lambda (x y) (mapcar '+ x y)) data (run-blocks 20 5)))
      )
    )
    (let ((percentages (mapcar (lambda (x) (/ (car x) (* n 5.0))) data)))
      (when graph
        (draw-graph percentages)
      )
      (list (list (/ (apply '+ (subseq percentages 0 5)) 5)
                  (/ (apply '+ (subseq percentages 5 10)) 5)
                  (/ (apply '+ (subseq percentages 10 15)) 5)
                  (/ (apply '+ (subseq percentages 15 20)) 5))
                  percentages
      )
    )
  )
)

(defun draw-graph (points)
  (let ((w (open-exp-window "Data" :width 550 :height 460 :visible t)))
    (allow-event-manager w)
    (add-line-to-exp-window '(50 0) '(50 420) :color 'white :window "Data")
    (dotimes (i 11)
      (add-text-to-exp-window :x 5 :y (+ 5 (* i 40)) :width 35 :text (format nil "~3,1f" (- 1 (* i .1))) :window "Data")
      (add-line-to-exp-window (list 45 (+ 10 (* i 40))) (list 550 (+ 10 (* i 40))) :color 'white :window "Data")
    )
    
    (let ((x 50))
      (mapcar (lambda (a b) (add-line-to-exp-window (list x (floor (- 410 (* a 400))))
                                                  (list (incf x 25) (floor (- 410 (* b 400))))
                                                    :color 'blue :window "Data"))
        (butlast points) (cdr points)
      )
    )
    (allow-event-manager w)
  )
)

(defun experiment ()

  (setf *window* (open-exp-window "Choice Experiment" :visible t))
    
  (install-device *window*)
  (allow-event-manager *window*)  

  (add-button-to-exp-window :x 20   :y 20  :height 24 :width 24 :text "0"     :action 'oeuf-casse)
  (add-button-to-exp-window :x 60   :y 20  :height 24 :width 24 :text "0"     :action 'oeuf-casse)
  (add-button-to-exp-window :x 100  :y 20  :height 24 :width 24 :text "0"     :action 'oeuf-casse)
  (add-button-to-exp-window :x 140  :y 20  :height 24 :width 24 :text "0"     :action 'oeuf-casse)

  (proc-display)  

  (goal-focus-fct (car (define-chunks-fct `((isa butAvoirBlancsEnNeige statut startExperiment)))))
  (define-chunks-fct `((isa butAvoirBlancs nbBlancs 0 nbJaunes 0 nbCoquilles 0 statut startCassage)))
  (define-chunks-fct `((isa butObtenirTexture couleurActuelle 0 volumeActuel 0 vitesseActuelle 0 energie 1100 statut startFouettage)))

  (run-full-time 100)
)

(defun oeuf-casse (button)
  (remove-items-from-exp-window button)
  (proc-display)  
)

(clear-all)

(define-model oeufs-neige-model 

  (sgp :v t :show-focus t :trace-detail medium :ult t)

    ;; do not change these parameters
  (sgp :esc t :bll .5 :ol t :er t :lf 0)
  
  ;; adjust these as needed
  (sgp :ans .2 :mp 10.0 :rt -60)
    
  (chunk-type butAvoirBlancsEnNeige statut)
  (chunk-type butAvoirBlancs nbBlancs nbJaunes nbCoquilles statut)
  (chunk-type butObtenirTexture couleurActuelle volumeActuel vitesseActuelle energie statut)

  (chunk-type oeuf blanc jaune coquille)
  (chunk-type couleur nom valeur)
  (chunk-type texture couleur volume)
  (chunk-type experienceCassage forceActuelle resultat)
  (chunk-type experienceFouettage couleurActuelle volumeActuel vitesseMouvement resultat)

  (add-dm
    (goalAvoirBlancsEnNeige ISA butAvoirBlancsEnNeige statut startExperiment)
    (goalAvoirBlancs ISA butAvoirBlancs nbBlancs 0 nbJaunes 0 nbCoquilles 0 statut startCassage)
    (goalObtenirTexture ISA butObtenirTexture couleurActuelle 0 volumeActuel 0 vitesseActuelle 0 energie 1100 statut startFouettage)

    (oeuf1 ISA oeuf blanc 1 jaune 1 coquille 1)

    (couleur1 ISA couleur nom transparent valeur 0)
    (couleur2 ISA couleur nom blanc valeur 200)

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
    (blancsEnNeigeObtenus isa chunk)
  )

  (start-hand-at-mouse)

  (goal-focus goalAvoirBlancsEnNeige)

  (P start
    =goal>
      isa         butAvoirBlancsEnNeige
      statut      startExperiment
    ==>
    !bind! =but (goal-focus goalAvoirBlancs)
    =goal>
      statut      startCassage
  )

  (P startCassage
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

  (P passerAuFouettage
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

  (P attraperOeuf
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

 (P casserOeufSansCoquille
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

  (P casserOeufAvecCoquille
    =goal>
      statut      oeufAttrape
      nbCoquilles   =nombreCoquilles

    ?manual>
      state  free
    ==>
    !bind!  =nouveauNombreCoquille (+ =nombreCoquilles (act-r-random 4))
    +manual>
      isa         click-mouse
    =goal>
      statut      oeufCasse
      nbCoquilles   =nouveauNombreCoquille

  )

  (P separerOeufAvecJaune
    =goal>
      statut      oeufCasse
      nbBlancs    =nombreBlancs
      nbJaunes    =nombresJaunes
    ==>
   !bind! =nombre   (+ =nombreBlancs 1)
   !bind! =nouveauNombreJaunes  (+ =nombresJaunes 1)
    =goal>
      nbJaunes    =nouveauNombreJaunes
      nbBlancs    =nombre
      statut      oeufSepare
  )

  (P separerOeufSansJaune
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




  (P verifierPasCoquille
    =goal>
      statut      oeufSepare
      nbCoquilles =nombreCoquilles
      !eval!  (< =nombreCoquilles 1)
    ==>
    =goal>
      statut      pasDeCoquille

  )
  (P verifierAvecCoquille
    =goal>
      statut      oeufSepare
      nbCoquilles =nombreCoquilles
      !eval!  (> =nombreCoquilles 0)
    ==>
    =goal>
      statut      presenceCoquille
  )

  (P retirerCoquille
    =goal>
    statut presenceCoquille
    nbCoquilles =nombreCoquilles
    > nbCoquilles 0
    ==>
    !bind! =nouveauNombreCoquille   (- =nombreCoquilles 1)
    =goal>
    nbCoquilles =nouveauNombreCoquille
    )

    (P retirerCoquilleTermine
      =goal>
      statut presenceCoquille
      nbCoquilles 0
      ==>
      =goal>
      statut pasDeCoquille
      )


  (P verifierAvecJaune
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

  (P verifierPasJaune
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


  (P retirerJaune
    =goal>
    statut presenceJaune
    nbJaunes =nombreJaunes
    > nbJaunes 0
    ==>
    !bind! =nouveauNombreJaunes   (- =nombreJaunes 1)
    =goal>
    nbJaunes =nouveauNombreJaunes
    )

  (P retirerJauneTermine
    =goal>
    statut presenceJaune
    nbJaunes 0
    ==>
    =goal>
    statut      startCassage
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
;; Début du but secondaire : Obtenir une texture en neige
;;

  (P recupererVitesseSucces ;; récupérer la vitesse d'une précédente tentative en succès comme référence pour cette run 
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

  (P recupererVitesseSuccesOK ;; rappel de la vitesse avec la précédente run étant un succès réussi
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
    @retrieval> ;; pour vider le buffer sans impacter l'apprentissage
  )

  (P recupererVitesseSuccesKO ;; échec du rappel de la vitesse avec la précédente run étant un succès
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

  (P recupererVitesseManqueEnergieOK ;; rappel de la vitesse avec la précédente run étant un manque d'énergie réussi
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
    @retrieval> ;; pour vider le buffer sans impacter l'apprentissage
  )

  (P recupererVitesseManqueEnergieKO ;; échec du rappel de la vitesse, on choisi une vitesse par défaut aléatoire
    =goal>
      ISA         butObtenirTexture
      statut      recupererVitesseManqueEnergie
    ?retrieval>
      buffer      failure
    ==>
    !bind! =vitesse (+ 5 (act-r-random 15))
    =goal>
      statut      fouetter
      vitesseActuelle =vitesse
  )

  (P baisserVitesse ;; Adapter la vitesse d'une précédente run échoué par manque d'énergie en la diminuant un peu
    =goal>
      ISA         butObtenirTexture
      statut      diminuerVitesse
      vitesseActuelle =vitesse
    ==>
    !bind! =nouvelleVitesse (- =vitesse 5)
    =goal>
      statut      fouetter
      vitesseActuelle =nouvelleVitesse
  )

  (P augmenterVitesse ;; Perfectionner la vitesse d'une précédente run réussi en l'augmentant un peu
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

  (P fouetterClassique ;; foutter de façon classique
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

  (P verifierTexture ;; récupère la texture parfaite pour comparer avec la texture actuelle
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

  (P comparerTextureOK ;; rappel de la texture parfaite réussi et la texture actuelle y est similaire
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
      statut      blancsEnNeigeObtenus
    +imaginal>
      ISA experienceFouettage
      couleurActuelle   =couleur
      volumeActuel      =volume
      vitesseMouvement  =vitesse
      resultat          succes
    !output! (=ener)
  )

  (P comparerTextureKO ;; rappel de la texture parfaite réussi mais la texture actuelle y est différente
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

  (P verifierEnergieKO ;; il ne reste plus d'énergie au model, on arrête
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
      statut      blancsEnNeigeObtenus
    +imaginal>
      ISA experienceFouettage
      couleurActuelle   =couleur
      volumeActuel      =volume
      vitesseMouvement  =vitesse
      resultat          manqueEnergie
  )

  (P verifierEnergieOK ;; il reste encore de l'énergie au model, on continue
    =goal>
      ISA         butObtenirTexture
      statut      verifierEnergie
      > energie     0
    ==>
    =goal>
      statut      fouetter
  )

  (P finirExperiance ;; on termine l'expériance en vidant le buffer imaginaire pour finaliser l'apprentissage
    =goal>
      ISA         butAvoirBlancsEnNeige
      statut      blancsEnNeigeObtenus
    ?imaginal>
       state free
       buffer full
     ==>
     -imaginal>
  )
)
