# Graph Report - .  (2026-06-06)

## Corpus Check
- Corpus is ~1,509 words - fits in a single context window. You may not need a graph.

## Summary
- 54 nodes · 52 edges · 17 communities (7 shown, 10 thin omitted)
- Extraction: 90% EXTRACTED · 10% INFERRED · 0% AMBIGUOUS · INFERRED: 5 edges (avg confidence: 0.81)
- Token cost: 0 input · 0 output

## Community Hubs (Navigation)
- [[_COMMUNITY_AI & Core Packages|AI & Core Packages]]
- [[_COMMUNITY_Refactoring Audit & Stability|Refactoring Audit & Stability]]
- [[_COMMUNITY_Keymap & Git Workflow|Keymap & Git Workflow]]
- [[_COMMUNITY_Completion Stack|Completion Stack]]
- [[_COMMUNITY_LSP & Variable Config|LSP & Variable Config]]
- [[_COMMUNITY_Keymap Architecture|Keymap Architecture]]
- [[_COMMUNITY_Org Mode|Org Mode]]
- [[_COMMUNITY_Completion-at-Point|Completion-at-Point]]
- [[_COMMUNITY_Evil Commentary|Evil Commentary]]
- [[_COMMUNITY_Evil Surround|Evil Surround]]
- [[_COMMUNITY_Evil Goggles|Evil Goggles]]
- [[_COMMUNITY_Evil TeX|Evil TeX]]
- [[_COMMUNITY_Avy Navigation|Avy Navigation]]
- [[_COMMUNITY_Flymake Diagnostics|Flymake Diagnostics]]
- [[_COMMUNITY_Project Management|Project Management]]
- [[_COMMUNITY_Snippets|Snippets]]
- [[_COMMUNITY_Shell Path|Shell Path]]

## God Nodes (most connected - your core abstractions)
1. `zeemacs.d Emacs Configuration` - 12 edges
2. `lisp/core/keybinds.el (new file)` - 7 edges
3. `Technical Audit Summary` - 5 edges
4. `vertico` - 4 edges
5. `Hardcoded Paths Issue` - 4 edges
6. `Variable Inconsistency Issue` - 4 edges
7. `Skeleton & Injection Keymap Pattern` - 4 edges
8. `zeds/leader-map` - 4 edges
9. `evil (Vim Emulation)` - 3 edges
10. `org (Org mode)` - 3 edges

## Surprising Connections (you probably didn't know these)
- `which-key` --semantically_similar_to--> `Skeleton & Injection Keymap Pattern`  [INFERRED] [semantically similar]
  README.md → refactoring_plan.md
- `evil (Vim Emulation)` --references--> `zeds/leader-map`  [INFERRED]
  README.md → refactoring_plan.md
- `Technical Audit Summary` --references--> `zeemacs.d Emacs Configuration`  [EXTRACTED]
  refactoring_plan.md → README.md
- `zeds/eglot-python-workspace-config (silent failure)` --references--> `eglot (LSP client)`  [EXTRACTED]
  refactoring_plan.md → README.md
- `zeds/git-map` --references--> `magit`  [EXTRACTED]
  refactoring_plan.md → README.md

## Hyperedges (group relationships)
- **Vertico/Corfu Completion Stack** — zeemacs_d_readme_vertico, zeemacs_d_readme_corfu, zeemacs_d_readme_cape, zeemacs_d_readme_orderless, zeemacs_d_readme_marginalia, zeemacs_d_readme_consult, zeemacs_d_readme_embark [EXTRACTED 0.95]
- **Skeleton & Injection Keymap Architecture** — zeemacs_d_refactoring_plan_skeleton_injection, zeemacs_d_refactoring_plan_keybinds_el, zeemacs_d_refactoring_plan_zeds_leader_map, zeemacs_d_refactoring_plan_use_package_bind, zeemacs_d_refactoring_plan_defvar_keymap [EXTRACTED 0.95]
- **4-Phase Refactoring Plan** — zeemacs_d_refactoring_plan_phase1, zeemacs_d_refactoring_plan_phase2, zeemacs_d_refactoring_plan_phase3, zeemacs_d_refactoring_plan_phase4 [EXTRACTED 1.00]

## Communities (17 total, 10 thin omitted)

### Community 0 - "AI & Core Packages"
Cohesion: 0.22
Nodes (9): aidermacs, copilot, copilot-chat, doom-themes, zeemacs.d Emacs Configuration, evil (Vim Emulation), evil-collection, vterm (+1 more)

### Community 1 - "Refactoring Audit & Stability"
Cohesion: 0.33
Nodes (7): Technical Audit Summary, core/base.el, core/functions.el, early-init.el, gc-cons-threshold Memory Leak, Hardcoded Paths Issue, Phase 1: Critical Stability Fixes

### Community 2 - "Keymap & Git Workflow"
Cohesion: 0.38
Nodes (7): magit, lisp/core/keybinds.el (new file), zeds/buffer-map, zeds/code-map, zeds/file-map, zeds/git-map, zeds/leader-map

### Community 3 - "Completion Stack"
Cohesion: 0.33
Nodes (6): consult, embark, embark-consult, marginalia, orderless, vertico

### Community 4 - "LSP & Variable Config"
Cohesion: 0.40
Nodes (6): eglot (LSP client), core/variables.el, zeds/eglot-python-workspace-config (silent failure), init.el, Phase 4: Cleanup & Optimization, Variable Inconsistency Issue

### Community 5 - "Keymap Architecture"
Cohesion: 0.40
Nodes (5): defvar-keymap (Emacs 29+), Phase 2: Core Keybinds Infrastructure, Phase 3: Migration & Distribution, Skeleton & Injection Keymap Pattern, use-package :bind injection

### Community 6 - "Org Mode"
Cohesion: 0.67
Nodes (3): evil-org, org (Org mode), org-roam

## Knowledge Gaps
- **29 isolated node(s):** `aidermacs`, `copilot`, `copilot-chat`, `evil-collection`, `evil-commentary` (+24 more)
  These have ≤1 connection - possible missing edges or undocumented components.
- **10 thin communities (<3 nodes) omitted from report** — run `graphify query` to explore isolated nodes.

## Suggested Questions
_Questions this graph is uniquely positioned to answer:_

- **Why does `zeemacs.d Emacs Configuration` connect `AI & Core Packages` to `Refactoring Audit & Stability`, `Keymap & Git Workflow`, `Completion Stack`, `LSP & Variable Config`, `Org Mode`?**
  _High betweenness centrality (0.441) - this node is a cross-community bridge._
- **Why does `Technical Audit Summary` connect `Refactoring Audit & Stability` to `AI & Core Packages`, `LSP & Variable Config`?**
  _High betweenness centrality (0.222) - this node is a cross-community bridge._
- **Why does `vertico` connect `Completion Stack` to `AI & Core Packages`?**
  _High betweenness centrality (0.139) - this node is a cross-community bridge._
- **Are the 3 inferred relationships involving `vertico` (e.g. with `consult` and `marginalia`) actually correct?**
  _`vertico` has 3 INFERRED edges - model-reasoned connections that need verification._
- **What connects `aidermacs`, `copilot`, `copilot-chat` to the rest of the system?**
  _29 weakly-connected nodes found - possible documentation gaps or missing edges._