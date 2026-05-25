% ============================================================================
% CONSTRAINT STORY: html_compatibility_matrix
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_html_compatibility_matrix, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: html_compatibility_matrix
 *   human_readable: HTML Compatibility Matrix: Browser Vendor Control and Web Standard Fragmentation
 *   domain: technology/standards/web_infrastructure
 *
 * SUMMARY:
 *   The HTML compatibility matrix represents a structural constraint where
 *   web developers must navigate divergent browser implementations of web
 *   standards to reach a global audience. This constraint exhibits classical
 *   Tangled Rope characteristics: genuine coordination function (browsers
 *   implementing shared standards enables a universal web platform) layered
 *   with asymmetric extraction (browser vendors extract developer labor
 *   through compatibility work and lock-in, while developers bear the cost).
 *   The constraint's extractiveness has declined over the interval (0.68 to
 *   0.52) as W3C interoperability initiatives, automated testing frameworks,
 *   and native JavaScript features have reduced the most egregious
 *   compatibility gaps. However, theater ratio has increased (0.52 to 0.65),
 *   indicating performative elements: legacy polyfills, CSS vendor prefixes,
 *   and compatibility layers persist through institutional inertia despite
 *   reduced necessity. The constraint exhibits all six types from different
 *   structural positions: powerless web developers see pure extraction
 *   (Snare), browser vendors see coordination benefit (Rope), non-Chromium
 *   vendors experience mixed extraction (Tangled Rope), organized
 *   standardization bodies see a temporary problem with a sunset path
 *   (Scaffold), legacy tooling persists as degraded mechanisms (Piton), and
 *   naive observers risk naturalizing contingent vendor choices as immutable
 *   laws.
 *
 * KEY AGENTS:
 *   - Web Developers: Primary victims (powerless/trapped) — must support multiple browser implementations; no exit from web platform if targeting broad audience
 *   - Browser Vendors (Chromium-dominated): Primary beneficiaries (institutional/arbitrage) — benefit from developer lock-in and market share; arbitrage through proprietary extensions
 *   - Non-Chromium Browser Vendors (Firefox, Safari): Secondary beneficiaries with extraction (powerful/constrained) — benefit from web coordination but constrained by Chromium dominance
 *   - Web Accessibility Community: Secondary victims (powerless/constrained) — must work around inconsistent accessibility feature implementation across browsers
 *   - W3C Standards Body and Interoperability Coalition: Organized actors (organized/constrained) — working to reduce fragmentation; see sunset path through standardization
 *   - Small/Independent Web Projects: Implicit victims — lack resources for extensive cross-browser testing; most vulnerable to extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing vendor competitive choices as inherent platform limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(html_compatibility_matrix, 0.52).
domain_priors:suppression_score(html_compatibility_matrix, 0.68).
domain_priors:theater_ratio(html_compatibility_matrix, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(html_compatibility_matrix, extractiveness, 0.52).
narrative_ontology:constraint_metric(html_compatibility_matrix, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(html_compatibility_matrix, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(html_compatibility_matrix, tangled_rope).
narrative_ontology:human_readable(html_compatibility_matrix, "HTML Compatibility Matrix: Browser Vendor Control and Web Standard Fragmentation").
narrative_ontology:topic_domain(html_compatibility_matrix, "technology/standards/web_infrastructure").

domain_priors:requires_active_enforcement(html_compatibility_matrix).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(html_compatibility_matrix, browser_vendors).
narrative_ontology:constraint_beneficiary(html_compatibility_matrix, web_platform_incumbents).
narrative_ontology:constraint_victim(html_compatibility_matrix, web_developers).
narrative_ontology:constraint_victim(html_compatibility_matrix, small_web_projects).
narrative_ontology:constraint_victim(html_compatibility_matrix, web_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEB DEVELOPER (SNARE) — Trapped in the matrix. Must support multiple browser vendors, each with partial HTML/CSS/JavaScript compliance and divergent implementation details. Cannot exit: building for the web requires compatibility across browsers. No alternative platform offers equivalent reach. Faces full extraction: time investment in cross-browser testing, maintenance burden for compatibility layers, career risk from specification whiplash.
constraint_indexing:constraint_classification(html_compatibility_matrix, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WEB ACCESSIBILITY COMMUNITY (SNARE) — Constrained by browser implementation variance in ARIA, semantic HTML, keyboard navigation, and screen reader support. Accessibility standards exist but are interpreted inconsistently across vendors. Must invest extra labor to work around browser bugs and incomplete implementations. High suppression: small community has minimal leverage over vendor behavior; must accept whatever accessibility features each browser chooses to implement.
constraint_indexing:constraint_classification(html_compatibility_matrix, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: BROWSER VENDOR — CHROMIUM-BASED (ROPE) — Experiences the matrix as a coordination mechanism: supporting HTML standards enables the web platform to function as a distribution channel. Benefits from compatibility: more developers write for the web, growing the user base. Can arbitrage: proprietary extensions (Chrome-specific APIs) create vendor lock-in while maintaining baseline compatibility. Net beneficiary — extraction flows toward them.
constraint_indexing:constraint_classification(html_compatibility_matrix, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-CHROMIUM BROWSER VENDOR (TANGLED ROPE) — Mixed position. Benefits from web standard coordination (users expect consistent web experience). But constrained by Chromium dominance: must maintain compatibility with Chromium-biased web content while differentiating on other dimensions. Extraction: developers increasingly target Chromium first, leaving non-Chromium browsers as secondary targets. Active enforcement required: must maintain parity with Chromium innovations to avoid users abandoning the platform.
constraint_indexing:constraint_classification(html_compatibility_matrix, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: W3C STANDARDS BODY AND INTEROPERABILITY COALITION (SCAFFOLD) — Organized actors (Mozilla, Apple, Microsoft, Google, W3C) committed to reducing fragmentation through shared standards work. Sees the compatibility matrix as a temporary problem with sunset logic: Interop 2023/2024 initiatives, Baseline Web Platform compatibility tracking, and WebDX metrics aim to reduce implementation gaps. Low effective extraction because the coalition has agency and a clear exit path (increased interoperability). Has sunset clause: as vendors align on standard behaviors, the extractive asymmetry should decline.
constraint_indexing:constraint_classification(html_compatibility_matrix, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY COMPATIBILITY LAYERS AND POLYFILLS (PITON) — Once-critical tools for bridging browser gaps (jQuery, Babel transpilers, CSS prefixes, polyfills) persist through institutional inertia despite much reduced necessity. Theater ratio high: many projects still import jQuery for syntax convenience despite native querySelector being standard; CSS prefix usage remains even though vendor prefixes are deprecated. The ecosystem maintains these tools because they're already in place and developers trained on them, not because they solve current problems. These are degraded coordination mechanisms.
constraint_indexing:constraint_classification(html_compatibility_matrix, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, the compatibility matrix appears immutable: you cannot eliminate browser diversity without eliminating vendor choice, and you cannot have vendor choice without fragmentation. The incompatibility is presented as inherent to competitive browser markets. However, structural data contradicts this — the fragmentation is substantially engineered through vendor-specific extensions, implementation delays, and strategic incompleteness. This perspective naturalizes what is partially contingent.
constraint_indexing:constraint_classification(html_compatibility_matrix, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(html_compatibility_matrix_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(html_compatibility_matrix, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(html_compatibility_matrix, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(html_compatibility_matrix, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(html_compatibility_matrix, TR),
    TR >= 0.70.

:- end_tests(html_compatibility_matrix_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Browser vendors extract significant developer labor through forced compatibility work, testing overhead, and maintenance burden for divergent implementations. However, extraction is not maximal (0.68 → 0.52 trend) because shared standards do provide genuine coordination benefit — the web platform's universality is a real collective good. Suppression (0.68): High. Developers are suppressed by lack of alternatives: building for the web requires browser compatibility; no viable competitor platform offers equivalent reach without its own compatibility matrices. Suppression mechanisms include: career dependence on web skills, network effects from global web audience, lack of practical alternatives, information asymmetry (developers learn vendor-specific quirks through painful iteration). Theater ratio (0.65): Moderate-high. Legacy tooling (jQuery, Babel, polyfills) is maintained as ritual despite reduced necessity; CSS vendor prefixes persist in codebases; compatibility testing frameworks add procedural overhead beyond genuine necessity. Theater increased over interval as interoperability improved — the remaining compatibility work is increasingly performative rather than functionally necessary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival disagreement. The web developer classifies as Snare (extraction with minimal coordination benefit visible from their position). The browser vendor classifies as Rope (genuine coordination with net benefit to them). The W3C coalition classifies as Scaffold (temporary problem with sunset). The legacy ecosystem classifies as Piton (degraded vestigial functions). The analytical observer risks Mountain classification (natural law of competitive markets) but structural data reveals engineered fragmentation. This gap is diagnostic: it reveals that 'the HTML compatibility matrix' is not a unified natural constraint but an engineered asymmetry maintained through vendor choices and developer lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness is computed from their structural position. Web developers derive high d (high directionality toward victimhood) from powerless status + trapped exit + victim classification, yielding high f(d) and high χ. Browser vendors derive low d from institutional status + arbitrage exit + beneficiary classification, yielding low/negative f(d) and low χ (they experience the constraint as beneficial). Non-Chromium vendors derive medium d from powerful status + constrained exit + mixed victim/beneficiary position, yielding moderate χ. The global scope (σ=1.2) amplifies extractiveness across all perspectives: browser incompatibilities matter more when they affect billions of users. Suppression is not scope-scaled — it remains a raw structural property (0.68) reflecting the genuine barriers developers face.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED through perspectival multiplicity: The constraint is validly Tangled Rope from the analytical observer's position (genuine coordination function + asymmetric extraction), but developers experience pure Snare (extraction without perceivable coordination benefit). The gap between these perspectives reveals the extraction mechanism: the coordination genuinely benefits vendors and enables the web platform, but developers bear the costs while vendors reap benefits. The W3C Interop initiatives represent institutional recognition that the asymmetry is unsustainable and explicit resolution mechanism. The theater ratio increase indicates the constraint's function is shifting: as technical incompatibilities decline, performative elements (legacy tooling, unnecessary polyfills, ritual testing) increase — the constraint is beginning to degrade from Tangled Rope toward Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vendor_strategic_incompleteness,
    'To what extent is HTML/CSS/JavaScript incompatibility the result of vendor strategic choices (prolonging supplier lock-in) versus genuine technical difficulty in coordinating implementations?',
    'Analysis of specification clarity vs implementation divergence; correlation between spec maturity and cross-browser variance; examination of deliberate vendor delays in implementing others'' innovations',
    'If strategic: incompatibility is extractive mechanism (Snare) that vendors enforce. If technical: incompatibility is coordination problem (Rope). Mixed assessment likely — determine proportions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_strategic_incompleteness, empirical, 'Whether incompatibility is strategic or technical').

omega_variable(
    interop_initiative_effectiveness,
    'Does the W3C Interop initiative (Interop 2023/2024) actually reduce developer extraction, or does it merely reduce performative vendor differentiation while maintaining structural asymmetries?',
    'Longitudinal measurement of compatibility test pass rates across browsers; developer self-reported time spent on cross-browser fixes; correlation between Interop focus areas and actual developer pain points',
    'If effective: Scaffold classification confirmed, sunset is real. If ineffective: Interop is theater maintaining the Tangled Rope without reducing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interop_initiative_effectiveness, empirical, 'Effectiveness of W3C Interoperability initiatives').

omega_variable(
    monoculture_risk_trade_off,
    'What is the actual relationship between browser market concentration (Chromium dominance) and total developer extraction? Does monoculture reduce extraction by eliminating compatibility work, or increase it by eliminating competitive pressure?',
    'Comparison of developer time investment across periods of high vendor diversity (2010-2015) vs Chromium dominance (2020-2026); analysis of feature innovation velocity and accessibility improvements correlated with market share',
    'If monoculture reduces extraction: Chromium dominance is stabilizing. If increases: current fragmentation is unstable equilibrium and competitive pressure necessary for platform health.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monoculture_risk_trade_off, empirical, 'Relationship between browser market concentration and developer extraction').

omega_variable(
    platform_exit_feasibility,
    'Are non-web platforms (React Native, Flutter, native mobile apps) genuine exits from the HTML compatibility matrix, or alternative platforms with their own extractive matrices?',
    'Comparative analysis of cross-platform testing burden, vendor fragmentation, and developer lock-in across platforms; measurement of platform switching costs',
    'If genuine exits: web developers trapped; web has higher extraction than alternatives. If alternative matrices: HTML constraint is not a trap but a trade-off among equally extractive platforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_exit_feasibility, empirical, 'Whether alternative platforms offer genuine exit from extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(html_compatibility_matrix, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(htmlcompat_tr_t0, html_compatibility_matrix, theater_ratio, 0, 0.52).
narrative_ontology:measurement(htmlcompat_tr_t5, html_compatibility_matrix, theater_ratio, 5, 0.59).
narrative_ontology:measurement(htmlcompat_tr_t10, html_compatibility_matrix, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(htmlcompat_be_t0, html_compatibility_matrix, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(htmlcompat_be_t5, html_compatibility_matrix, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(htmlcompat_be_t10, html_compatibility_matrix, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(html_compatibility_matrix, information_standard).
narrative_ontology:affects_constraint(html_compatibility_matrix, chromium_market_dominance).
narrative_ontology:affects_constraint(html_compatibility_matrix, web_developer_skill_lock_in).
narrative_ontology:affects_constraint(html_compatibility_matrix, accessibility_implementation_variance).

% DUAL FORMULATION NOTE:
% The HTML compatibility matrix can be decomposed into three structurally distinct constraints: (1) specification incompleteness and ambiguity (ε ≈ 0.25, Mountain — inherent to complex standards), (2) vendor strategic divergence in implementation (ε ≈ 0.65, Snare — engineered fragmentation), (3) legacy tooling persistence (ε ≈ 0.35, Piton — institutional inertia). This story captures the aggregate effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(html_compatibility_matrix, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
