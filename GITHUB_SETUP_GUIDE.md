# Step-by-Step Guide: Putting This Project on GitHub

## Prerequisites

1. **Create a GitHub account** (if you don't have one): https://github.com/signup
2. **Install Git** on your computer:
   - **Mac**: Open Terminal and run `git --version`. If not installed, it will prompt you to install.
   - **Windows**: Download from https://git-scm.com/download/win
3. **Configure Git** (one-time setup):
   ```bash
   git config --global user.name "Your Name"
   git config --global user.email "your.email@example.com"
   ```

---

## Option A: GitHub Desktop (Easiest for Beginners)

### Step 1: Download GitHub Desktop
- Go to https://desktop.github.com/
- Download and install
- Sign in with your GitHub account

### Step 2: Create a New Repository
1. Open GitHub Desktop
2. Click **File → New Repository**
3. Fill in:
   - **Name**: `float-analysis` (or `floatation-rest-analysis`)
   - **Description**: "Analysis code for floatation-REST RCT"
   - **Local Path**: Choose where your project folder is (or will be)
   - **Initialize with README**: Uncheck (we already have one)
   - **Git Ignore**: Select "R"
4. Click **Create Repository**

### Step 3: Add Your Files
1. Copy all the project files into the repository folder
2. GitHub Desktop will show all the new files as "changes"
3. At the bottom left:
   - **Summary**: "Initial commit: analysis pipeline"
   - **Description** (optional): "Complete analysis code for float study"
4. Click **Commit to main**

### Step 4: Publish to GitHub
1. Click **Publish repository** (top right)
2. Choose:
   - **Keep this code private**: Check this if your data/study isn't public yet
   - Uncheck if you want it public
3. Click **Publish Repository**

### Step 5: Done!
Your code is now at: `https://github.com/YOUR-USERNAME/float-analysis`

---

## Option B: Command Line (More Control)

### Step 1: Create Repository on GitHub Website
1. Go to https://github.com/new
2. Fill in:
   - **Repository name**: `float-analysis`
   - **Description**: "Analysis code for floatation-REST RCT"
   - **Public** or **Private**: Your choice
   - **DO NOT** initialize with README, .gitignore, or license (we have these)
3. Click **Create repository**
4. You'll see a page with setup instructions — keep this open

### Step 2: Initialize Local Repository
Open Terminal (Mac) or Git Bash (Windows) and navigate to your project folder:

```bash
# Navigate to your project folder
cd /path/to/float-analysis

# Initialize git repository
git init

# Add all files
git add .

# Make first commit
git commit -m "Initial commit: complete analysis pipeline"
```

### Step 3: Connect to GitHub and Push
Copy the commands from the GitHub page, or use these (replace YOUR-USERNAME):

```bash
# Add the remote repository
git remote add origin https://github.com/YOUR-USERNAME/float-analysis.git

# Rename branch to main (if needed)
git branch -M main

# Push to GitHub
git push -u origin main



```

### Step 4: Authenticate
- If prompted for credentials, enter your GitHub username
- For password, you'll need a **Personal Access Token** (GitHub no longer accepts passwords):
  1. Go to https://github.com/settings/tokens
  2. Click **Generate new token (classic)**
  3. Give it a name, select **repo** scope
  4. Copy the token and use it as your password

---

## After Setup: Making Changes

### When you update your code:

**GitHub Desktop:**
1. Open GitHub Desktop
2. You'll see changed files listed
3. Write a commit message (e.g., "Fix FDR correction in mediation")
4. Click **Commit to main**
5. Click **Push origin**

**Command Line:**
```bash
git add .
git commit -m "Fix FDR correction in mediation"
git push
```

---

## Recommended: Add Data Instructions

Since your raw data shouldn't be on GitHub (privacy + file size), add this to your README:

```markdown
## Data Access

Raw data files are not included in this repository. To reproduce analyses:

1. Obtain data files from [PI name/lab/OSF link]
2. Place in the `data/` folder:
   - `raw_data.csv`
   - `only_complete_data.csv`
3. Run `source("run_all.R")`
```

---

## Optional: Add a License

For academic code, common choices:

1. **MIT License** (very permissive — anyone can use/modify)
2. **CC-BY 4.0** (requires attribution)

To add:
1. Go to your repo on GitHub
2. Click **Add file → Create new file**
3. Name it `LICENSE`
4. Click **Choose a license template** on the right
5. Select MIT or your preferred license

---

## Folder Structure Check

Before pushing, make sure your folder looks like this:

```
float-analysis/
├── .gitignore          ✓ Prevents data files from being uploaded
├── README.md           ✓ Project description
├── GITHUB_SETUP_GUIDE.md  (you can delete this after setup)
├── run_all.R           ✓ Master script
├── data/
│   └── (empty or .gitkeep — CSV files ignored)
├── output/
│   └── tables/
│       └── (empty — PNG files ignored)
└── R/
    ├── 00_setup.R
    ├── 01_data_cleaning.R
    ├── 02_anova_11dasc.R
    ├── 02_anova_5dasc.R
    ├── 02_anova_intero.R
    ├── 02_anova_clinical.R
    ├── 02_anova_panas.R
    ├── 03_mediation_simple.R
    ├── 03_mediation_ob_followup.R
    ├── 03_mediation_phenom.R
    ├── 04_ttests_prepost.R
    └── utils/
        ├── anova_functions.R
        ├── mediation_functions.R
        └── table_functions.R
```

---

## Troubleshooting

### "Permission denied" error
- Make sure you're using a Personal Access Token, not your password
- Check that you have write access to the repository

### "Repository not found" error
- Double-check the repository URL
- Make sure the repo exists on GitHub

### Files not showing up
- Check `.gitignore` — your data files are intentionally excluded
- Run `git status` to see what Git is tracking

### Large file errors
- GitHub has a 100MB file limit
- If you accidentally committed large files, you'll need to remove them from history

---

## Quick Reference Card

| Action | GitHub Desktop | Command Line |
|--------|---------------|--------------|
| See changes | Automatic | `git status` |
| Stage files | Automatic | `git add .` |
| Commit | Bottom left panel | `git commit -m "message"` |
| Push to GitHub | "Push origin" button | `git push` |
| Pull updates | "Fetch origin" then "Pull" | `git pull` |

---

## Questions?

- GitHub Docs: https://docs.github.com/en/get-started
- Git basics: https://git-scm.com/book/en/v2/Getting-Started-Git-Basics
- R + Git guide: https://happygitwithr.com/
