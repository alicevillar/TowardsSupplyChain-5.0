
 
<h1>Towards Supply Chain 5.0: redesigning supply chains as resilient, sustainable, and human-centric systems in a post-pandemic world </h1>

 
 <br> 
 
 
>This is the official repository for the paper: "Towards Supply Chain 5.0: redesigning supply chains as resilient, sustainable, and human-centric systems in a post-
>pandemic world". This article performs a bibliometric study followed by a systematic literature review to analyze the published literature landscape and presents a >study of how Industry 5.0 can enable supply chain evaluation and optimization in manufacturing processes, helping to enhance supply chain resilience.

 
<h1>Table of Contents</h1>

<!-- TOC -->
- [1. Overview](#1-overview)
- [2. Architecture](#2-architecture) 
    - [2.1. Monolithic Architecture](#21-monolithic-architecture)
    - [2.2. Microservice Architecture](#22-microservice-architecture)
- [3. Project Interface](#3-project-interface)
    - [3.1. How to use SFA](#31-how-to-use-sfa)
    - [3.2. Activity Diagrams](#32-activity-diagrams)
- [4. Development timeline](#4-development-timeline)
- [5. Project Structure](#5-project-structure)
- [6. Project Files](#6-project-files)
- [7. Python Dependencies](#7-python-dependencies) 
- [8. Tools](#8-tools) 
- [9. Installation](#9-installation)
- [10. Quick Start](#10-quick-start)
- [11. Authentication Details](#11-authentication-details)
    - [11.1. Request Structure](#111-request-structure)
    - [10.2. API Rate Limits](#102-api-rate-limits) 
- [12. OWASP Proactive Controls](#12-owasp-proactive-controls)
- [13. Automated Testing](#13-automated-testing)
- [14. Project Roadmap](#14-project-roadmap)
- [15. Useful Links](#15-useful-links)

<!-- /TOC -->
 
## 1. Overview 

This API, called SFA (Space Fan Art), is an API created with Flask REST-Plus with two Prototypes: a monolithic architecture and a microservice architecture. [Astronomy Picture Of The Day (APOD)](https://api.nasa.gov/), which is a NASA open API that returns the picture of the day, has been used as a model thoughout the development of both Prototypes:  
 
:arrow_forward: In the monolithic architecture, APOD was used as an API model in various aspects, such as user authorisation key and rate limits. 
<br>
:arrow_forward:In the microservice architecture, SFA-API is connected with APOD. Thus, our microservive returns a picture that comes directly from APOD. 

<br> 
 
## 2. Architecture 
