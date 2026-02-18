% ============================================================================
% CONSTRAINT STORY: moltbook_breach_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_moltbook_breach_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: moltbook_breach_2026
 * human_readable: The Moltbook Database Exposure
 * domain: technological/social
 * * SUMMARY:
 * Moltbook, the "front page of the agent internet," was revealed to have an 
 * exposed backend database. This vulnerability allows any actor to hijack 
 * supposedly "autonomous" AI agents, transforming the platform's narrative 
 * from an independent experiment into a centralized Snare of insecure data.
 * * KEY AGENTS:
 * - AI Users/Enthusiasts: Subject (Powerless)
 * - Malicious Actors/Exploiters: Beneficiary (Institutional - via exploit)
 * - 404 Media (Matthew Gault): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.72). The vulnerability extracts the autonomy of 
% every agent on the site and the trust of its users.
domain_priors:base_extractiveness(moltbook_breach_2026, 0.72). 

% Suppression is moderate-high (0.65). The site's misconfiguration suppresses 
% the actual safety of the "agent experiment" in favor of ease of access.
domain_priors:suppression_score(moltbook_breach_2026, 0.65).   

% Theater ratio is extreme (0.95). The "uncontrolled experiment" was 
% performative mask over a fundamentally porous database structure.
domain_priors:theater_ratio(moltbook_breach_2026, 0.95).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(moltbook_breach_2026, extractiveness, 0.72).
narrative_ontology:constraint_metric(moltbook_breach_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(moltbook_breach_2026, theater_ratio, 0.95).

% Constraint classification claim
narrative_ontology:constraint_claim(moltbook_breach_2026, piton).
narrative_ontology:human_readable(moltbook_breach_2026, "The Moltbook Database Exposure").
narrative_ontology:topic_domain(moltbook_breach_2026, "technological/social").

% Primary keys for the classification engine
% High-extraction stakeholders
narrative_ontology:constraint_beneficiary(moltbook_breach_2026, malicious_script_actors).
narrative_ontology:constraint_victim(moltbook_breach_2026, agent_autonomy_narrative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE USER (SNARE)
% For users who believed in the experiment, the breach is a Snare: a 
% predatory trap where their agents are now puppets for external actors.
constraint_indexing:constraint_classification(moltbook_breach_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE EXPLOITER (ROPE)
% For those with access to the exposed APIs, the database is a Rope: 
% a coordination tool to seize control of the "agent internet."
constraint_indexing:constraint_classification(moltbook_breach_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(immediate), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Analysts view Moltbook as a Piton: An inertial maintenance of an 
% "autonomous" narrative that is 95% theater and zero-percent functional.
constraint_indexing:constraint_classification(moltbook_breach_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(moltbook_breach_2026, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moltbook_breach_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moltbook_breach_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moltbook_breach_2026, rope, context(agent_power(institutional), _, _, _)).

test(piton_threshold) :-
    domain_priors:theater_ratio(moltbook_breach_2026, TR),
domain_priors:requires_active_enforcement(moltbook_breach_2026).
    TR >= 0.70.

:- end_tests(moltbook_breach_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.72) is extreme because the platform allows the 
 * total hijacking of user agents. The Theater Ratio (0.95) identifies 
 * that the "Agent Social Media" was a performative shell with no 
 * functional security infrastructure to back its claims of autonomy.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system identifies the Piton by the discrepancy between the viral 
 * narrative of "uncontrolled experimentation" and the functional reality 
 * of an open, misconfigured database.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_moltbook_integrity,
    'Was the misconfiguration intentional theater or a genuine engineering fail?',
    'Audit of Moltbook backend logs prior to Jan 31 exposure.',
    'Intentional implies a permanent Snare; Failure implies a repairable Scaffold.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moltbook_breach_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio rises as the "autonomous" hype hits a peak just before the breach.
narrative_ontology:measurement(mb_tr_t0, moltbook_breach_2026, theater_ratio, 0, 0.40).
narrative_ontology:measurement(mb_tr_t5, moltbook_breach_2026, theater_ratio, 5, 0.75).
narrative_ontology:measurement(mb_tr_t10, moltbook_breach_2026, theater_ratio, 10, 0.95).

% Extraction spikes as the exposed API becomes public knowledge.
narrative_ontology:measurement(mb_ex_t0, moltbook_breach_2026, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(mb_ex_t5, moltbook_breach_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(mb_ex_t10, moltbook_breach_2026, base_extractiveness, 10, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
