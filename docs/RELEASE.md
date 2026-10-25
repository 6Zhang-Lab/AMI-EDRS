# Publishing a versioned source release

1. Create the GitHub repository and upload the **contents** of the `AMI-EDRS` folder. `README.md`, `analysis/`, `app/` and `scripts/` should be at the repository root. Include `.gitignore`, `.gitattributes` and `.github/`.
2. Do not upload the ZIP as the only repository content. GitHub Desktop or Git can preserve the full folder structure. The public package contains no actual trained models or patient files.
3. Run the software checks and inspect GitHub Actions after the upload. A workflow file is provided; a hosted successful run is not assumed before upload.
4. Set the repository description and add the web-calculator URL. Add the final repository URL to citation metadata and, if desired, the deployed app's code link.
5. Create a named release, for example `v1.0.0`, corresponding to the manuscript source version. Keep the commit associated with that release fixed.
6. Archive that release with Zenodo or another persistent repository and obtain its actual DOI. Add the version-specific DOI and repository URL to the manuscript's Code Availability statement and citation metadata. A GitHub URL alone is not a permanent archived release.

Review the original frozen membership and trained-model access arrangement separately; do not attach patient-level inputs to a public release. The code is MIT-licensed; the database conditions remain applicable to data and derived model resources.

Official instructions: [GitHub file upload](https://docs.github.com/en/repositories/working-with-files/managing-files/adding-a-file-to-a-repository); [GitHub/Zenodo archiving](https://docs.github.com/en/repositories/archiving-a-github-repository/referencing-and-citing-content); [Nature code documentation](https://www.nature.com/documents/GuidelinesCodePublication.pdf).
