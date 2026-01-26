% ============================================================================
% CONSTRAINT STORY: the_wall_procedural_barrier
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: The 9/11 Commission Report
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_the_wall_procedural_barrier, []).

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
 * 
 * constraint_id: the_wall_procedural_barrier
 * human_readable: The Intelligence/Law Enforcement Information Sharing Barrier ("The Wall")
 * domain: legal/institutional
 * temporal_scope: 1995-2001 CE
 * spatial_scope: US Department of Justice / FBI / CIA
 * 
 * SUMMARY:
 * "The Wall" refers to procedures restricting information sharing between 
 * intelligence investigators (CIA/FBI) and criminal prosecutors (DOJ). Initially 
 * intended to protect the legality of FISA warrants, it became a pervasive, 
 * bureaucratic barrier that prevented the connection of critical intelligence dots prior to 9/11.
 * 
 * KEY AGENTS:
 * - FBI Field Agent (Individual Powerless): Subject to the rules, unable to get needed information.
 * - OIPR Gatekeeper (Institutional): Controlled the flow of FISA data, enforcing the wall.
 * - 9/11 Commission (Analytical): Post-hoc analysts who identified the wall as a critical failure point.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(the_wall_procedural_barrier, 0, 10).
narrative_ontology:constraint_claim(the_wall_procedural_barrier, snare).

% Base extractiveness score (0.8)
% High; the system extracted the ability of agents to perform their primary function
% (protecting public safety), leading to catastrophic failure. Value was destroyed, not transferred.
domain_priors:base_extractiveness(the_wall_procedural_barrier, 0.8).

% Suppression score (0.9)
% High; actively enforced by OIPR's threat of withholding FISA warrants, which suppressed
% the natural flow of information and collaboration.
domain_priors:suppression_score(the_wall_procedural_barrier, 0.9).

% Enforcement: Requires active maintenance by OIPR.
domain_priors:requires_active_enforcement(the_wall_procedural_barrier).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(the_wall_procedural_barrier, institutional_inertia).
constraint_victim(the_wall_procedural_barrier, public_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: OIPR GATEKEEPER (INSTITUTIONAL) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Controlled flow of FISA data)
   WHEN: biographical (Career depended on procedural correctness)
   WHERE: arbitrage (Positioned between intelligence and law)
   
   WHY THIS CLASSIFICATION:
   From the perspective of the DOJ gatekeepers, "The Wall" was a 'Rope'—a necessary
   coordination tool to maintain the legal purity of criminal cases and prevent
   intelligence gathered under FISA from tainting prosecutions.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    the_wall_procedural_barrier,
    rope,
    context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: FBI FIELD AGENT - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless (Subject to rules from above)
   WHEN: immediate (Blocked during an active investigation)
   WHERE: trapped (No authority to circumvent the wall)

   WHY THIS CLASSIFICATION:
   For the field agent, The Wall was a 'Mountain'—an incomprehensible, immovable
   bureaucratic obstacle. It was a fact of their job they could not change or
   reason with, directly impeding their ability to act on information.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    the_wall_procedural_barrier,
    mountain,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: 9/11 COMMISSION (ANALYTICAL) - Snare
   --------------------------------------------------------------------------
   WHO: analytical (Post-hoc observer of systemic failure)
   WHEN: historical (Analyzing the lead-up to a major event)
   WHERE: analytical (Reviewing all evidence without restriction)

   WHY THIS CLASSIFICATION:
   With the benefit of hindsight, the 9/11 Commission saw the Wall as a 'Snare'.
   It was an extractive system that choked the flow of vital information, directly
   enabling the 9/11 plot to succeed by preventing agents from connecting the dots.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    the_wall_procedural_barrier,
    snare,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
).

/* ==========================================================================
   4. TESTS (Validation)
   ========================================================================== */

:- begin_tests(the_wall_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, Type2, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(the_wall_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. MANDATROPHY STATUS: The 0.8 extractiveness score reflects the catastrophic
 *    destruction of value (public safety) caused by the constraint. It's 'RESOLVED'
 *    by showing the perspective of the institutional agent who saw it as a 'Rope'
 *    to protect legal procedure, while the powerless agent saw it as a 'Mountain'
 *    and the analyst saw its true nature as a 'Snare'.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Gatekeeper (Rope): A tool for legal purity.
 *    - Field Agent (Mountain): An immovable bureaucratic rule.
 *    - Commission (Snare): A fatal information bottleneck.
 *
 * 3. BENEFICIARY/VICTIM: The primary beneficiary was 'institutional inertia' itself,
 *    while the victim was abstractly 'public safety'. This highlights how constraints
 *    can serve the system's continuation at the expense of its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */
omega_variable(
    wall_rigidity_source,
    "Was the barrier's deadly strength derived from a strict interpretation of FISA law (Mountain) or from institutional risk-aversion within the DOJ/FBI (Snare)?",
    resolution_mechanism("A comparative legal audit of FISA vs. internal FBI/DOJ policy memos from the period."),
    impact("Determines if resolution required legislative change (changing a Mountain) or merely decisive leadership (cutting a Snare)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Coordinated Information Sharing (Pre-Wall status quo)
 *    Viability: This was the default mode of operation before 1995.
 *    Suppression: Actively suppressed by new DOJ procedures and the OIPR's enforcement of them, born from a fear of legal taint.
 *
 * CONCLUSION:
 * The fact that a functional, less-restrictive alternative already existed and was actively suppressed is the strongest possible evidence for the 'Snare' classification from an analytical perspective. The 'Wall' was not an inevitable 'Mountain', but a constructed 'Snare'.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/the_wall_procedural_barrier].
 * 2. Multi-perspective: ?- multi_index_report(the_wall_procedural_barrier).
 * 3. Run tests: ?- run_tests(the_wall_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */