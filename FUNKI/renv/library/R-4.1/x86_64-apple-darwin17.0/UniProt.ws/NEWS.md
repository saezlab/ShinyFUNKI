Changes VERSION 2.27.0
----------------------

BUG FIX

    o (2.27.1) Fix bug when selecting column GENEID. The mapping mapped both
    GENEID and ENTREZ_GENE to P_ENTREZGENE. When returning columsn used match to
    identify but would only pick up first match which was ENTREZ_GENE entry.


Changes VERSION 1.2.0
--------------------

PKG FEATURES

    o UniProt.ws creates an object that talks directly to the web
    services at UniProt.  As such, it provides access to a tremendous
    library of IDs etc. directly from UniProt.

    o When the package loads there will be acces to a Uniprot.ws
    object, this object has select, keys, cols and keytypes methods
    that behave the way these methods normally do for the other
    annotation packages.  

    o One important difference from other packages is that the user
    must use the species method to set the species to their organism
    of choice (by default it is set for humans).  This is because the
    web resource is too big to return values for all these species at
    the same time.  Uniprot currently has support for over 21,000
    different species.  

    o Please see the manual pages and associated vignette for more
    detailed information about how to use this resource.


