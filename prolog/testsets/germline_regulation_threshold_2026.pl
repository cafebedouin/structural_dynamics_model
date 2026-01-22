% ============================================================================
% CONSTRAINT STORY: germline_regulation_threshold_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "We can rewrite our genetic code" by Michael Le Page
% ============================================================================

:- module(germline_regulation_threshold_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 * * constraint_id: germline_regulation_threshold_2026
 * human_readable: International Germline Regulatory Threshold
 * domain: political/technological
 * temporal_scope: 2026-2040
 * spatial_scope: Global (International Governance)
 * * SUMMARY:
 * This constraint focuses on the regulatory barrier preventing the routine use 
 * of CRISPR for germline (inheritable) editing. While somatic editing is 
 * approved, germline editing is currently restricted because it 
 * "isn't yet safe enough to attempt". The "threshold" represents 
 * the scientific and political consensus point where "safe enough" is reached.
 * * KEY AGENTS:
 * - Global Regulator: (Institutional) - Enforces the moratorium until safety 
 * benchmarks are met.
 * - Biotech Innovator: (Individual Powerful) - Views the threshold as a 
 * bottleneck for "better versions of ourselves".
 * - Future Progeny Advocate: (Analytical/Powerless) - Views the threshold 
 * as a protective shield against "errors".
 * * NARRATIVE ARC:
 * The success of somatic treatments like Victoria Gray's puts 
 * pressure on the germline threshold. As clinical trials increase, 
 * regulators must decide if the "rewrite" capability should extend to 
 * future generations.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(germline_regulation_threshold_2026, 0, 10).

% Base extractiveness score: 0.5 (Moderate)
% Rationale: Regulation extracts time and capital from developers to ensure 
% safety, but also prevents "errors" from being hard-coded into 
% the human lineage, which is a form of protection.
domain_priors:base_extractiveness(germline_regulation_threshold_2026, 0.5).

% Suppression score: 0.8 (High)
% Rationale: Non-compliant germline research is actively suppressed or 
% de-funded globally until the "safety threshold" is met.
domain_priors:suppression_score(germline_regulation_threshold_2026, 0.8).

% Enforcement: Requires active enforcement
% Rationale: Regulatory bodies must actively monitor and approve CRISPR 
% therapies.
domain_priors:requires_active_enforcement(germline_regulation_threshold_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(germline_regulation_threshold_2026, human_genetic_integrity).
constraint_victim(germline_regulation_threshold_2026, radical_enhancement_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: GLOBAL REGULATOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power)
   WHEN: generational (Thinking about "altering our children's genomes")
   WHERE: mobile (Can adjust thresholds as safety data arrives)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the regulator, the threshold is a "Rope"—a coordination mechanism 
   used to guide the technology safely toward a future where it is 
   "routinely used".
   
   NARRATIVE EVIDENCE:
   The technology "isn't yet safe enough," implying a deliberate 
   hold-and-release strategy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    germline_regulation_threshold_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: BIOTECH INNOVATOR - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerful (Significant influence and capital)
   WHEN: biographical (Seeking ROI on "extraordinarily powerful tools")
   WHERE: constrained (Must wait for safety approvals)
   SCOPE: national/regional
   
   WHY THIS CLASSIFICATION:
   For the innovator, the threshold acts as a "Noose." It strangles the 
   commercialization of germline editing even though the "rewrite" capability 
   already exists.
   
   NARRATIVE EVIDENCE:
   "It does seem likely that in the future, CRISPR will be routinely used," 
   yet the current "not safe enough" status prevents immediate use.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    germline_regulation_threshold_2026,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE UNBORN PROGENY - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (No agency in their own genome editing)
   WHEN: civilizational (Permanent changes to genetic code)
   WHERE: trapped (No exit from their own DNA)
   SCOPE: local (Biological self)
   
   WHY THIS CLASSIFICATION:
   For the subject being edited, the regulatory threshold is a "Mountain." 
   It is the only unchangeable barrier protecting their biological 
   blueprint from "errors" until absolute safety is proven.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    germline_regulation_threshold_2026,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(germline_threshold_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(germline_regulation_threshold_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(germline_regulation_threshold_2026, noose, context(agent_power(individual_powerful), _, _, _)),
    constraint_indexing:constraint_classification(germline_regulation_threshold_2026, mountain, context(agent_power(individual_powerless), _, _, _)).

test(safety_as_enforcement) :-
    % High suppression score (0.8) indicates that the "not safe enough" 
    % claim is the primary tool for active regulation.
    domain_priors:suppression_score(germline_regulation_threshold_2026, S),
    S > 0.7.

:- end_tests(germline_threshold_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE GAP: The conflict lies between the Innovator (who sees 
 * the threshold as an arbitrary Noose) and the Progeny (who sees it as 
 * a protective Mountain).
 * 2. REGULATORY ANALYSIS: The Omega variable (safety threshold) is what 
 * prevents the Rope from becoming a Noose for society at large. 
 * If the threshold is set too low, it becomes a "Noose" (permanent errors); 
 * if set too high, it becomes a "Mountain" (denied cures).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - THE SAFETY THRESHOLD
   ========================================================================== */

omega_variable(
    germline_editing_safety_threshold,
    "At what off-target mutation rate does 'not safe enough' become 'routinely safe'?",
    resolution_mechanism("Meta-analysis of hundreds of clinical trials to establish parity with natural mutation rates"),
    impact("If resolved low: Rapid transition to a global Rope. If resolved high: Remains an institutional Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    regulatory_arbitrage_risk,
    "Will 'bio-haven' nations lower the threshold early, creating a Noose for international harmonization?",
    resolution_mechanism("Monitoring of national approval timelines for the first germline therapies"),
    impact("If arbitrage occurs: The international 'Rope' breaks, leading to a fragmented Mountain/Noose landscape."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Permanent Global Ban
 * Viability: Historically the default for germline editing.
 * Suppression: Being eroded by the "likely" future routine use mentioned 
 * in the source.
 * * ALTERNATIVE 2: Somatic-Only Limitation
 * Viability: Current status quo.
 * Conclusion: The "Safety Threshold" Omega is the only factor determining 
 * if the world stays at Alternative 2 or moves to full germline "rewriting".
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [germline_regulation_threshold_2026].
% Report: ?- multi_index_report(germline_regulation_threshold_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
