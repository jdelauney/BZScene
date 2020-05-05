
# BZScene Multimedia 2D, 3D & audio library for Lazarus and FPC [VERSION ALPHA]
# BZScene Bibliothèque multimédia 2D, 3D & audio pour Lazarus et FPC [VERSION ALPHA]

**BZScene** est la solution Audio, 2D et 3D multiplateforme pour Lazarus &amp; FPC 32 et 64 bits

**BZScene** est en partie basé sur le moteur de rendu  [GLScene](https://sourceforge.net/projects/glscene/ "GLScene"). Communauté dont je fais partie.

**BZScene** est une Bibliothèque pour développer des applications graphiques multiplateformes 2D et ou 3D (Software, OpenGL, Vulkan,...)

Petite histoire :
-----------------

[GLScene](https://sourceforge.net/projects/glscene/ "GLScene"). est le fruits de centaines de développeurs à travers le monde. 
Son histoire débute dans les années 199(5) avec [Mike Lischke](http://www.lischke-online.de/ "Soft Gems - Mike Lischke") et ces bibliothèques pour delphi "[GraphicEx](https://github.com/mike-lischke/GraphicEx "GraphicEx")", et de GLScene. Puis le flambeau de GLScene à été passé à Eric Grange. Depuis la communauté est toujours là et plus ou moins active. Cette une bibliothèque robuste et optimisée. Mais le code est "vieillissant"

**BZScene** reprend donc certaines bases de GLScene et apporte de nouvelles améliorations et méthodes :

- Compatible : **Windows 64bits**, **Linux 64bits** (_OS 32bits et MacOs 64bits : non testé_)
- Une gestion des Bitmaps **optimisée et totalement indépendante de FPC et Lazarus**.
- Une bilbiothèque mathématique vectorielle et matricielle **optimisée en assembleur avec les instruction SIMD SSE 2/3/4 (AVX)**
- Une gestion pour jouer et maninuler des échantillons sonores avec OpenAL comme moteur par défaut 
	- _(le support de bibliothèques externe comme BASS, FMod, PortAudio..., peuvent être ajouter facilement)_
   -  **Formats de fichier audio supportés en lecture seule** : 
		- wav, mp3, ogg, amiga modules (mod,xm, s3m...)
	
- Des classes uniques dans la gestion et manipulation de données de diverses formats et types
- Un moteur de rendu 2D et 3D **évolutif et facilement adaptable**		
- Un code totalement commenté en **français** (for one time sorry, for english, and all non-french people, but name of methods are in a "pseudo" english)
- Des dizaines applications de démonstration

https://github.com/jdelauney/BZScene-Samples
https://github.com/jdelauney/BZScene-Demoscene-samples
https://github.com/jdelauney/OldSchoolIsBack
https://github.com/jdelauney/LittleAlchemist

et plein d'autres chose encore.....
		
----------

## Les capacités générales de BZScene :
### 2D

- **Manipulation des bitmaps uniquement en 32Bits** au format de couleur RGBA ou BGRA  (Linux / Windows) afin d'optimiser les performances d'affichage
- **Affichage avec ou sans transparance** en fonction du sytsteme et de l'interface (gtk, qt, carbon,...)
- **Convertion des couleurs dans plusieurs formats(de 1 à 64 bits par pixel)** :  
	- rgba, bgra, hsl, hsv, (_les modes cmyk, cie, yuv, lab, lch... seront supporté dans le futur_) 
- **Formats d'image supportés en lecture seulement** :
	- JPEG, PNG, TGA, PCX, XPM, PGM, PBM, PGM, GIF, WEBP 
	- **Autres formats supportés dans le futur**  : , APNG, MNG, ICO, CUR, DDS, KTX, TIFF, PCD, PSD, XCF,IFF/LBM, FLI/FLC, CUT..._
	
- **Formats d'image supportés en lecture et écriture** :		
	- BMP, TGA et JPEG
	- _Autres formats supportés dans le futur_  : PNG
  
- Acces aux pixels optimisé (très rapide)
- **Capacité de dessiner des formes diverses** :  
	- lignes, rectangles, cercles, polygones, textes, avec ou sans adoucissement
- **Plusieurs méthodes de remplissage** : 
	- couleur unie,  dégradé, ou par placage de texture
- **Nombreux filtres sur les couleurs (+ de 50) disponibles** :
	- Luminosité, Saturation, Inversion, Niveaux de gris, ...
- **Filtres de segmentation, détection des contours** :  
  - par seuillage, Canny, Marr Hildreth, Gradient, Boolean...
- **Appliquer des effets spéciaux** :
	-  Twirl, Transformation polaire, sinuosidale...
- **Plusieurs modes de transformations** :
	- Stretch, StretSmooth, wnSample, Resample, rotation, mirroir...
- **Mode de fusion** :
  - Normal, Add, Sub, Mul, Difference, Average, Overlay, Screen...
- **filtres d'interpolations supportés** : 
  - Box, Hermit, Cubic, Lanscoz, Mitchell, Hamming, Kayser...
	
### 3D
Pour le moment pas de 3D, c'est en cours de développement

### Audio
- **Moteur audio supporté** :
  - OpenAL
- **Formats d'image supportés en lecture seulement** :
  - Wav, mp3, ogg, module (mod, xm, s3m...)
  
  Merci d'avance de votre soutien et pour vos contributions future à ce projet
  
  BeanzMaster
