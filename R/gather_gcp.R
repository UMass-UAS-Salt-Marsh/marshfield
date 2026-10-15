# Gather 2026 GCPs for ESX site

if(FALSE)
   gcp <- gather_rtk(path = 'C:/Work/saltmarsh/data/uas2026/field/GCP',
                     result = 'C:/Work/saltmarsh/data/uas2026/field/GCP/gcp.gpkg',
                     rename_file = file.path('C:/Work/saltmarsh/data/uas2026/field/pars', 'rtk_names.txt'),
                     validate = FALSE)
