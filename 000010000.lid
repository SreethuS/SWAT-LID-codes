Low Impact Development practices (.lid): The first 17 lines for a Green Roof, the next *** for a Rain Gargen, the next *** for a CiStern, and the following *** for a Porous paVement
URMD    | Urban land use name
0    | 0=the green roof is inactive (no simulation), 1=active
1    | Month when the green roof became operational (1-12)
1990    | Year when the wet pond became operational (e.g. 1980)
1    | Fraction of impervious areas where green roofs are installed
0    | 0=Characteristics (FC, WP, and Ksat) of the amended soil are identical to those of the native HRU soil, 1=read user input
0    | Evapotranspiration coefficient
0.4    | Field capacity of the amended soil (mm/mm)
0.15    | Wilting point of the amended soil (mm/mm)
50    | Saturated hydraulic conductivity of the amended soil layer (mm/hr)
0.5    | Porosity of the amended soil layer (mm/mm)
0.5    | Hydraulic efficiency factor (considering clogging up and anisotropy ratio) (0-1)
0.5    | Depth of the amended soil (m)
0    | A line prepared for additional parameters of the green roof
0    | A line prepared for additional parameters of the green roof
0    | A line prepared for additional parameters of the green roof
0    | A line prepared for additional parameters of the green roof
0    | A line prepared for additional parameters of the green roof
0    | 0=the rain garden is inactive (no simulation), 1=active
1    | Month when the rain garden became operational (1-12)
1990    | Year when the wet pond became operational (eg 1980)
1    | Fraction of impervious areas draining water to the rain garden
0    | 0=Characteristics (FC, WP, and Ksat) of the amended soil are identical to those of the native HRU soil, 1=read user input
0    | Evapotranspiration coefficient
0.4    | Field capacity of the amended soil (mm/mm)
0.15    | Wilting point of the amended soil (mm/mm)
50    | Saturated hydraulic conductivity of the amended soil layer (mm/hr)
0.5    | Porosity of the amended soil layer (mm/mm)
0.5    | Hydraulic efficiency factor (considering clogging up and anisotropy ratio) (0-1)
0.5    | Depth of the amended soil (m)
0    | Options for determining the size of the surface storage of the rain garden: 0=the storage dimension is determined based on volume requirement estimated from the drainage areas of the rain garden, assuming the storage depth of 0.1 m, 1=read user input (dim
1    | Fractional area of the rain garden storage surface to the impervious area of an urban HRU (0-1)
0    | (not used yet) Runoff volume required to fill the pool (storage of the rain garden on the soil surface), m3
1    | Depth of the rain garden storage (m)
1    | Diameter of the surface area of the rain garden (m; frustum of a circular cone)
1    | Diameter of the bottom of the rain garden (m; frustum of a circular cone)
0.5    | Slope of a slant of the rain garden storage (surface side slopes) (m; frustum of a circular cone)
0    | 0=the orifice drainage is inactive (no simulation), 1=active
0    | Height of the orifice from the bottom of the storage (m)
0    | Diameter of the orifice pipe (m)
0    | A line prepared for additional parameters for the rain garden
0    | A line prepared for additional parameters for the rain garden
0    | A line prepared for additional parameters for the rain garden
0    | A line prepared for additional parameters for the rain garden
0    | A line prepared for additional parameters for the rain garden
0    | 0=the cistern is inactive (no simulation), 1=active
1    | Month when the cistern became operational (1-12)
1990    | Year when the cistern became operational (e.g. 1980)
0    | 0=the cistern is not connected to the corresponding green roof located in the same HRU, 1=the cistern is connected to the corresponding green roof located in the same HRU
1    | Fraction of impervious areas draining water to the cistern
0    | Runoff volume to fill the cistern (storage of the cistern), m3; If CS_VOL is zero, SWAT calculates the cistern capacity with CS_RDEPTH.
25    | Rainfall depth generated in treated area to fill the cistern (storage of the cistern) (mm); If CS_RDEPTH is zero, SWAT assumes the cistern capacity as 5 m3
0    | A line prepared for additional parameters for the cistern
0    | A line prepared for additional parameters for the cistern
0    | A line prepared for additional parameters for the cistern
0    | A line prepared for additional parameters for the cistern
0    | A line prepared for additional parameters for the cistern
0    | 0=the porous pavement is inactive (no simulation), 1=active
1    | Month when the porous pavement became operational (1-12)
1990    | Year when the porous pavement became operational (e.g. 1980)
1    | Fraction of impervious areas where the porous pavement is installed
500    | Depth of the gravel bed of porous pavement (mm)
0.5    | Porosity of the gravel bed of porous pavement (mm/mm)
0    | 0=Characteristics (FC, WP, and Ksat) of the amended soil are identical to those of the native HRU soil, 1=read user input
0    | Drainage coefficient
0.4    | Field capacity of the amended soil  (mm/mm)
0.15    | Wilting point of the amended soil (mm/mm)
50    | Saturated hydraulic conductivity of the amended soil layer (mm/hr)
0.5    | Porosity of the amended soil layer
0.5    | Hydraulic efficiency factor (considering clogging up and anisotropy ratio)
0    | A line prepared for additional parameters for the porous pavement
0    | A line prepared for additional parameters for the porous pavement
0    | A line prepared for additional parameters for the porous pavement
0    | A line prepared for additional parameters for the porous pavement
0    | A line prepared for additional parameters for the porous pavement
0    | MLGA biocell on or off (1=on, 0=off)
0.05 | Fraction of impervious area where bioretention cell (BC) is installed
0.5 | Fraction of runoff from the impervious area that is routed to bio-retention cell
100    | Free board thickness (surface layer)
0.5    | Void fraction in the surface layer
400    | Thickness of the BC soil layer (mm)
45    | Saturated hydraulic conductivity of the BC soil layer (mm/hr)
0.10    | Initial moisture content of the BC soil layer (mm/mm)
0.41    | Moisture content at saturation (or porosity) of the BC soil layer (mm/mm)
200    | Suction head of the BC soil layer (mm)
0.057    | Wilting point of the BC soil layer (mm/mm)
600    |  Thickness of the BC storage layer (mm)
120    | Saturated hydraulic conductivity of the BC storage layer (mm/hr)
0.11    | Initial moisture content of the BC storage layer (mm/mm)
0.43    | Moisture content at saturation (or porosity) of the BC storage layer (mm/mm)
180    | Suction head of the BC storage layer (mm)
0.045   | Wilting point of the BC storage layer (mm/mm)
0.0   |  Diameter of the under drain pipe openings (m)
5    | Number of drain pipe openings
0    | Offset height of the under drain from the bottom of LID (mm)
0    | MLGA RAIN GARDEN on or off (1=on, 0=off)
0.05 | Fraction of impervious area where RAIN GARDEN (RG) is installed
0.5 | Fraction of runoff from the impervious area that is routed to RAIN GARDEN
150    | Free board thickness (surface layer)
0.5    | Void fraction in the surface layer
600    | Thickness of the RG soil layer (mm)
45    | Saturated hydraulic conductivity of the RG soil layer (mm/hr)
0.10    | Initial moisture content of the RG soil layer (mm/mm)
0.41    | Moisture content at saturation (or porosity) of the RG soil layer (mm/mm)
200    | Suction head of the RG soil layer (mm)
0.045    | Wilting point of the RG soil layer (mm/mm)
0.00    |  Diameter of the under drain pipe openings (m)
5    | Number of drain pipe openings
0    | Offset height of the under drain from the bottom of LID (mm)
0    | MLGA ROOF GARDEN on or off (1=on, 0=off)
0.05 | Fraction of impervious area where ROOF GARDEN (GR) is installed
150    | Free board thickness (surface layer)
0.5    | Void fraction in the surface layer
500    | Thickness of the GR soil layer (mm)
90    | Saturated hydraulic conductivity of the GR soil layer (mm/hr)
0.10    | Initial moisture content of the GR soil layer (mm/mm)
0.41    | Moisture content at saturation (or porosity) of the GR soil layer (mm/mm)
200    | Suction head of the GR soil layer (mm)
0.057    | Wilting point of the GR soil layer (mm/mm)
50    |  Thickness of the GR drainage layer (mm)
500    | Saturated hydraulic conductivity of the GR drainage layer (mm/hr)
0.11    | Initial moisture content of the GR drainage layer (mm/mm)
0.43    |  porosity of the GR drainage layer (mm/mm)
0.00    |  Diameter of the under drain pipe openings (m)
5    | Number of drain pipe openings
0    | Offset height of the under drain from the bottom of LID (mm)
0    | MLGA INFILTRATION TRENCH on or off (1=on, 0=off)
0.05 | Fraction of impervious area where INFILTRATION TRENCH (IT) is installed
0.5 | Fraction of runoff from the impervious area that is routed to IT
150    | Free board thickness (surface layer)
1.0    | Void fraction in the surface layer
600    | Thickness of the IT soil layer (mm)
145    | Saturated hydraulic conductivity of the IT storage layer (mm/hr)
0.11    | Initial moisture content of the IT storage layer (mm/mm)
0.45    | Moisture content at saturation (or porosity) of the IT storage layer (mm/mm)
200    | Suction head of the IT storage  layer (mm)
0.045    | Wilting point of the IT storage  layer (mm/mm)
0.0    |  Diameter of the under drain pipe openings (m)
5    | Number of drain pipe openings
0    | Offset height of the under drain from the bottom of LID (mm)
1    | MLGA PERMEABLE PAVEMENT on or off (1=on, 0=off)
0.05 | Fraction of impervious area where PERMEABLE PAVEMENT (PP) is installed
0.5 | Fraction of runoff from the impervious area that is routed to PERMEABLE PAVEMENT cell
50    | Free board thickness (surface layer)
0.5    | Void fraction in the surface layer
600    | Thickness of the PERMEABLE PAVEMENT soil layer (mm)
90     | Saturated hydraulic conductivity of the PERMEABLE PAVEMENT soil layer (mm/hr)
0.10    | Initial moisture content of the PERMEABLE PAVEMENT soil layer (mm/mm)
0.41    | Moisture content at saturation (or porosity) of the PP soil layer (mm/mm)
200    | Suction head of the PP soil layer (mm)
0.057    | Wilting point of the PP soil layer (mm/mm)
400    |  Thickness of the PP storage layer (mm)
145    | Saturated hydraulic conductivity of the PP storage layer (mm/hr)
0.11    | Initial moisture content of the PP storage layer (mm/mm)
0.43    | Moisture content at saturation (or porosity) of the PP storage layer (mm/mm)
180    | Suction head of the PP storage layer (mm)
0.090    | Wilting point of the PP storage layer (mm/mm)
100    | Thickness of the pavement layer (mm)
500    | Hydraulic conductivity of the pavement layer (mm/h)
0.5    | Previous fraction in the pavement layer
0.0    |  Diameter of the under drain pipe openings (m)
5    | Number of drain pipe openings
0    | Offset height of the under drain from the bottom of LID (mm)

