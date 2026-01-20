% ============================================================================
% CONSTRAINT STORY: ai_professional_displacement
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Financial Times (Jan 18, 2026), TFR83 (2025), Shaw & Nave (2026)
% ============================================================================

:- module(constraint_ai_professional_displacement, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ai_professional_displacement
 * human_readable: The Displacement of Entry-Level Professional Pathways
 * domain: economic/technological
 * temporal_scope: 2022 - 2026+ (Post-ChatGPT era)
 * spatial_scope: Global (UK/US Focused)
 * * SUMMARY:
 * As AI (System 3) becomes embedded in professional services, entry-level 
 * "grunt work" is being automated. While not yet causing widespread 
 * mass layoffs, AI is reshaping labor markets by reducing graduate hiring 
 * rates, as one bot can do the work of several junior employees. 
 * This creates a structural barrier for graduates seeking the "financial 
 * stability" once promised by these professions.
 * * KEY AGENTS:
 * - Recent Graduates (Powerless): Individuals who have "done everything they 
 * were told to do" but find entry-level jobs vulnerable.
 * - CEOs/Investors (Institutional): Deploying AI to "create efficiencies" 
 * with the "objective of cutting labor costs".
 * - Policymakers (Institutional): Focused on "winning the AI race" rather 
 * than protecting workers or managing fallout.
 * * NARRATIVE ARC:
 * The "jobs apocalypse" fears of 2022 have transitioned into a visible 
 * reshaping of professional services in 2026. The narrative moves 
 * from general anxiety to a specific "structural" threat to the graduate 
 * middle class, where the path to professional expertise is being "short-
 * circuited" by automated cognition[cite: 5, 4].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(ai_labor_shift_2026, 2022, 2030).
narrative_ontology:constraint_claim(ai_professional_displacement, noose).

% Base extractiveness score: Moderate-High (0.70)
% Rationale: Firms extract "AI-driven efficiencies" and cost savings at the 
% expense of entry-level human capital development and graduate wages.
domain_priors:base_extractiveness(ai_professional_displacement, 0.70).

% Suppression score: High (0.80)
% Rationale: System 3 (AI) "suppresses System 2" deliberation and replaces 
% early-career training tasks with automated outputs. 
% Government focus on "winning the race" suppresses labor protections.
domain_priors:suppression_score(ai_professional_displacement, 0.80).

% Enforcement: Requires active enforcement
% Rationale: Driven by institutional "objectives of cutting labor costs" 
% and the deployment of generative AI across tech and finance.
domain_priors:requires_active_enforcement(ai_professional_displacement).

% Beneficiaries and Victims
constraint_beneficiary(ai_professional_displacement, [investors, tech_firms, large_employers]).
constraint_victim(ai_professional_displacement, [recent_graduates, professional_services_workforce]).

narrative_ontology:constraint_metric(ai_professional_displacement, extractiveness, 0.70).
narrative_ontology:constraint_metric(ai_professional_displacement, suppression_requirement, 0.80).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: RECENT GRADUATE - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (Entry-level workers with zero bargaining power)
   WHEN: immediate (Facing a "hiring downturn" in 2026)
   WHERE: trapped (Traditional "safe" paths are disappearing)
   SCOPE: global (The professional services market shift)
   
   WHY THIS CLASSIFICATION:
   For graduates, AI is a Noose. It is an extractive mechanism that removes 
   their career entry points while offering no immediate alternatives for the 
   skills they were told to acquire.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_professional_displacement,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: CHIEF EXECUTIVE OFFICER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (In charge of company deployment and strategy)
   WHEN: biographical (Short-to-medium term efficiency targets)
   WHERE: mobile (Can shift hiring, build bots, or reinvest in higher-value roles)
   SCOPE: national (U.S. or UK labor strategy)
   
   WHY THIS CLASSIFICATION:
   For the CEO, AI is a Rope—a functional tool for "creating efficiencies" 
   and "cutting labor costs" to improve performance and competitiveness.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_professional_displacement,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: MACRO-ECONOMIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observing structural labor market trends)
   WHEN: historical (The long-run evolution of automation)
   WHERE: analytical (Studying global job postings and trends)
   SCOPE: global (The "Tri-System" shift in human reasoning [cite: 4])
   
   WHY THIS CLASSIFICATION:
   From an analytical view, this is a Mountain. It is a structural shift in 
   the "cognitive ecology" where System 3 becomes an active participant 
   that naturally supplants low-complexity human tasks.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_professional_displacement,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(ai_labor_tests).

test(graduate_vs_ceo_gap) :-
    % Graduates are trapped (Noose), CEOs are mobile (Rope).
    constraint_indexing:constraint_classification(ai_professional_displacement, noose, context(individual_powerless, immediate, trapped, _)),
    constraint_indexing:constraint_classification(ai_professional_displacement, rope, context(institutional, _, mobile, _)).

test(structural_permanence) :-
    % Economists see it as a structural law (Mountain).
    constraint_indexing:constraint_classification(ai_professional_displacement, mountain, context(analytical, historical, _, _)).

:- end_tests(ai_labor_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. PERSPECTIVAL GAP:
 * The most significant finding is the "underestimated transformation." 
 * Institutions see a Rope (efficiency), while graduates see a Noose (trapped). 
 * The gap is exacerbated by governments who view the "AI Race" as a 
 * "Mountain-climbing" exercise rather than a "Noose-cutting" task.
 * * 2. INTEGRATION WITH SYSTEM 3:
 * I linked the FT article to Shaw & Nave's (2026) "System 3" theory. 
 * The "grunt work" being displaced is effectively "Cognitive Surrender" at 
 * the institutional level—delegating entry-level reasoning to AI agents[cite: 4].
 * * 3. TFR83 ALIGNMENT:
 * TFR83 warns of "underinvestment in strategically important areas"[cite: 1]. 
 * The FT article highlights that governments have not yet done enough to 
 * "protect those who may lose out," suggesting a strategic failure in 
 * human capital investment during the technology race[cite: 5, 1].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_wage_link,
    "Will AI-driven productivity gains eventually feed through to higher wages and living standards?",
    resolution_mechanism("Monitor real wage growth vs. productivity indices for professional services through 2028."),
    impact("If yes, the Noose eventually becomes a Rope for the survivors. If no, the Noose tightens."),
    confidence_without_resolution(low)
).

omega_variable(
    graduate_path_reinvention,
    "Can professions 'wholesome reinvent' entry pathways as Kinder suggests?",
    resolution_mechanism("Monitor the adoption of AI-scaffolded training models in law and finance."),
    impact("If reinvented, it remains a Rope (coordinated change). If not, it is a Mountain (dead end)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: "Verify-then-Adopt" (Cognitive Offloading)
 * Viability: Using AI to *scaffold* junior reasoning rather than replacing it[cite: 4].
 * Suppression: Suppressed by CEOs' "objective of cutting labor costs" 
 * which prioritizes immediate savings over long-term skill development.
 * * ALTERNATIVE 2: Targeted Reskilling/Training
 * Viability: Investing in "critical thinking to spot hallucination".
 * Suppression: "Companies are also behind the curve" and failing to invest 
 * in these complementary skills.
 * * CONCLUSION:
 * The rejection of these alternatives in favor of "efficiency" confirms the 
 * Noose classification for current graduates.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% ?- constraint_indexing:multi_index_report(ai_professional_displacement).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
