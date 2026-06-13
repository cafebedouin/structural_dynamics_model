% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This story analyzes the remonstrance right from the perspective of the
 *   Crown and its supporters, who viewed it as an illegitimate minoritarian
 *   veto that protected particularist privileges at the expense of national
 *   unity and fiscal stability. The constraint is framed as a snare,
 *   extracting resources and authority from the central government to benefit
 *   local elites and magistrates. This is one reading of the
 *   'remonstrance_authority' kernel, contrasting with a 'magistrate_reading'
 *   that would frame it as a legitimate constitutional mechanism.
 *
 * KEY AGENTS:
 *   - royal_fiscal_authority: Primary target (institutional/constrained) — bears extraction
 *   - particularist_privilege_holders: Primary beneficiary (organized/identity_locked) — benefits from constraint
 *   - local_magistrates: Agenda setter (powerful/constrained) — administers the veto
 *   - national_interest: Victim (analytical/analytical) — abstract concept harmed by fragmentation
 *   - loyalist_parliamentarians: Excluded (organized/constrained) — voices for central authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.85).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.7).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, 'ca46cbd3-8195-4d0b-a6ea-89f7de306749').
narrative_ontology:cs_kernel_codification('ca46cbd3-8195-4d0b-a6ea-89f7de306749', formalized).
narrative_ontology:cs_authority_grounding('ca46cbd3-8195-4d0b-a6ea-89f7de306749', extraction).
narrative_ontology:cs_interpretation_layer_present('ca46cbd3-8195-4d0b-a6ea-89f7de306749').
narrative_ontology:cs_reading_relation('ca46cbd3-8195-4d0b-a6ea-89f7de306749', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('ca46cbd3-8195-4d0b-a6ea-89f7de306749', foundational, royal_sovereignty_is_indivisible).
narrative_ontology:cs_axiom_status(royal_sovereignty_is_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('ca46cbd3-8195-4d0b-a6ea-89f7de306749', royal_sovereignty_is_indivisible, deontological).
narrative_ontology:cs_axiom('ca46cbd3-8195-4d0b-a6ea-89f7de306749', foundational, local_privileges_subordinate_to_national_law).
narrative_ontology:cs_axiom_status(local_privileges_subordinate_to_national_law, holdable).
narrative_ontology:cs_axiom_grounding('ca46cbd3-8195-4d0b-a6ea-89f7de306749', local_privileges_subordinate_to_national_law, conventional).
narrative_ontology:cs_reference_frame('ca46cbd3-8195-4d0b-a6ea-89f7de306749', unified_royal_sovereignty).
narrative_ontology:cs_drift_state('ca46cbd3-8195-4d0b-a6ea-89f7de306749', late_ancien_regime, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ca46cbd3-8195-4d0b-a6ea-89f7de306749', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, particularist_privilege_holders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, local_magistrates).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, national_interest).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the remonstrance directly diverts significant fiscal resources and administrative power from the Crown. Suppression (0.7) is also high, as the Crown's attempts to bypass or abolish the remonstrance were met with strong, often violent, resistance, requiring active enforcement to maintain the local veto. Theater ratio is low (0.2) because the remonstrance, from this perspective, is a direct and effective tool for obstruction, not a performative one. Resistance is high (0.8) from the Crown's side, reflecting its continuous struggle against the remonstrance.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's perspective, the remonstrance is a snare, actively extracting from its authority. From the local magistrates' perspective (the 'magistrate_reading'), it is a legitimate rope or scaffold, coordinating the defense of ancient liberties. The engine's classification will reflect this divergence based on the declared beneficiary/victim structure and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The royal_fiscal_authority is a clear target (payer), bearing the costs of blocked edicts and lost revenue. Particularist_privilege_holders and local_magistrates are beneficiaries, gaining local autonomy and fiscal exemptions. The national_interest is an abstract victim, suffering from the fragmentation. Loyalist_parliamentarians are excluded, unable to influence the local veto process.
 *
 * MANDATROPHY ANALYSIS:
 *   The Crown's reading asserts that the original mandate of the remonstrance (protecting against arbitrary royal power) is 'dead' and that the mechanism now serves purely extractive ends for local elites. This prevents mislabeling it as a legitimate coordination mechanism (rope) when its function has atrophied into a tool for particularist obstruction (snare). The 'dead' founding problem status combined with 'world_rearranges' disappearance verdict signals a zombie constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remonstrance_legitimacy_ambiguity,
    'Is the remonstrance right a legitimate constitutional check on royal power, or an illegitimate obstruction of national governance?',
    'Analysis of historical outcomes: if it consistently protected fundamental rights against genuine tyranny, it leans legitimate; if it consistently protected particularist interests against necessary national reforms, it leans illegitimate.',
    'If legitimate, the Crown''s extraction is justified as the cost of constitutional balance; if illegitimate, the extraction by local magistrates is a snare on national authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remonstrance_legitimacy_ambiguity, conceptual, 'Ambiguity over the constitutional legitimacy of the remonstrance right.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''crown_reading'' of the ''remonstrance_authority'' kernel. How would the classification change under the ''magistrate_reading''?',
    'The ''magistrate_reading'' would likely classify the constraint as a ''rope'' or ''scaffold'', with local magistrates as beneficiaries and the Crown as a payer, and significantly lower extractiveness and suppression, reflecting its role as a legitimate check on power.',
    'A shift in reading would invert the beneficiary/victim structure and drastically alter the computed classification, highlighting the perspectival nature of constitutional interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'The classification is highly dependent on the chosen reading of the remonstrance authority kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1600, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1600, remonstrance_authority__crown_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(remo_tr_t1650, remonstrance_authority__crown_reading, theater_ratio, 1650, 0.25).
narrative_ontology:measurement(remo_tr_t1700, remonstrance_authority__crown_reading, theater_ratio, 1700, 0.22).
narrative_ontology:measurement(remo_tr_t1750, remonstrance_authority__crown_reading, theater_ratio, 1750, 0.21).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__crown_reading, theater_ratio, 1789, 0.2).

% Extraction over time
narrative_ontology:measurement(remo_be_t1600, remonstrance_authority__crown_reading, base_extractiveness, 1600, 0.6).
narrative_ontology:measurement(remo_be_t1650, remonstrance_authority__crown_reading, base_extractiveness, 1650, 0.7).
narrative_ontology:measurement(remo_be_t1700, remonstrance_authority__crown_reading, base_extractiveness, 1700, 0.78).
narrative_ontology:measurement(remo_be_t1750, remonstrance_authority__crown_reading, base_extractiveness, 1750, 0.82).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__crown_reading, base_extractiveness, 1789, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1600, remonstrance_authority__crown_reading, suppression_requirement, 1600, 0.5).
narrative_ontology:measurement(remo_su_t1650, remonstrance_authority__crown_reading, suppression_requirement, 1650, 0.58).
narrative_ontology:measurement(remo_su_t1700, remonstrance_authority__crown_reading, suppression_requirement, 1700, 0.65).
narrative_ontology:measurement(remo_su_t1750, remonstrance_authority__crown_reading, suppression_requirement, 1750, 0.68).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__crown_reading, suppression_requirement, 1789, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
