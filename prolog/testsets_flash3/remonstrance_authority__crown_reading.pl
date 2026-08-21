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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance as Illegitimate Minoritarian Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This constraint story analyzes the 'remonstrance right' from the
 *   perspective of the Crown, viewing it as an illegitimate minoritarian veto
 *   that obstructs royal authority and protects particularist privileges. The
 *   Crown's reading frames the remonstrance as a snare, extracting power and
 *   resources from the central government for the benefit of entrenched
 *   judicial bodies (parlements) and their allies. The metrics reflect this
 *   perspective, showing high extractiveness and suppression required to
 *   overcome this obstruction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.85).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.7).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance as Illegitimate Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8').
narrative_ontology:cs_kernel_codification('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', formalized).
narrative_ontology:cs_authority_grounding('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', lineage).
narrative_ontology:cs_interpretation_layer_present('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8').
narrative_ontology:cs_reading_relation('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', foundational, royal_prerogative_unimpeded).
narrative_ontology:cs_axiom_status(royal_prerogative_unimpeded, holdable).
narrative_ontology:cs_axiom_grounding('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', royal_prerogative_unimpeded, deontological).
narrative_ontology:cs_axiom('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', foundational, particularist_privilege_illegitimate).
narrative_ontology:cs_axiom_status(particularist_privilege_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', particularist_privilege_illegitimate, conventional).
narrative_ontology:cs_reference_frame('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', absolute_monarchy_unimpeded).
narrative_ontology:cs_drift_state('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', late_ancien_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8f7a01fd-947d-4f01-95d5-d9aa4ab2adf8', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, crown_royal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, parliamentary_factions).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, magistrates_of_parlements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, royal_treasury).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The monarch and their ministers, who view the remonstrance as an obstruction to necessary fiscal and administrative reforms. They benefit from its suppression and from the unimpeded exercise of royal prerogative, which the remonstrance challenges.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_royal_authority, agenda_setter,
    institutional, generational, constrained, national).

% The legislative bodies whose edicts and fiscal measures are subject to the remonstrance. They bear the cost of delayed or blocked legislation and the erosion of their authority by a minoritarian veto. Their power is diluted by the remonstrance's effect.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, parliamentary_factions, payer,
    powerful, biographical, constrained, national).

% The judicial bodies (parlements) that exercise the right of remonstrance. From the Crown's perspective, they are illegitimate obstructors of royal will, using an archaic privilege to protect their own particularist interests and tax exemptions. They are targeted by royal efforts to suppress the remonstrance.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, magistrates_of_parlements, payer,
    organized, biographical, identity_locked, regional).

% The financial arm of the Crown, which benefits from the unimpeded collection of taxes and implementation of fiscal policies. The remonstrance directly impedes its function, making its suppression a direct benefit.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_treasury, beneficiary,
    institutional, immediate, constrained, national).

% The general populace, whose views on the legitimacy of royal authority versus judicial checks are swayed by the ongoing contest. They are not direct actors but their sentiment influences the political climate.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, public_opinion, observer,
    moderate, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the Crown's perspective, the constraint coordinates the implementation of royal edicts and fiscal policy across the realm, ensuring uniform application of law and revenue collection without obstruction.
% TRANSFER_FUNCTION: Transfers legislative and fiscal authority from the parlements to the Crown, and ensures the flow of revenue to the royal treasury by preventing judicial obstruction.
% ABSENT_VOICES: The broader populace, whose interests are often framed as being served by either the Crown's reforms or the parlements' defense of tradition, but who have no direct voice in the remonstrance mechanism itself.
% DISAPPEARANCE_RATIONALE: If the remonstrance right and its associated authority vanished overnight, the Crown's legislative and fiscal power would be significantly enhanced, leading to a rapid implementation of reforms and a shift in the balance of power between the monarchy and judicial bodies. The political landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The Crown's need for unimpeded authority to govern and raise revenue, often in the face of local and particularist resistance from entrenched elites.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and its historians consistently attest to the ongoing necessity of royal authority to overcome local resistance for the good of the realm. This view is contested by historical accounts from parliamentary factions and some independent scholars, who frame the remonstrance as a legitimate check on arbitrary power.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the remonstrance, from the Crown's view, diverts significant royal authority and fiscal resources. Suppression (0.70) is substantial, as the Crown frequently resorted to 'lits de justice' and other coercive measures to overcome judicial resistance. Theater ratio (0.40) indicates that while the remonstrance had a genuine historical basis, its continued exercise was increasingly seen by the Crown as a performative act of obstruction rather than a legitimate constitutional check. Resistance is high (0.75) due to the persistent opposition from the parlements.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's perspective (this reading) sees the remonstrance as an extractive snare, while the magistrate's reading (a sibling constraint) views it as a legitimate rope or even a mountain protecting ancient liberties. The engine's per-seat classification will highlight this divergence, with the Crown as a victim and the magistrates as beneficiaries in this reading, reversing in the sibling reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and its treasury are structural beneficiaries of the constraint's suppression, as it allows them to implement policies and collect revenue. The parliamentary factions and magistrates of parlements are the targets, as their legislative and judicial authority is undermined by the Crown's efforts to suppress the remonstrance. The magistrates, in particular, are identity-locked into their role as defenders of tradition, making their exit options constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   From the Crown's perspective, the original mandate of the remonstrance (if any legitimate one existed) had atrophied into a mechanism for particularist obstruction. The classification as a snare prevents mislabeling this as a legitimate coordination mechanism, highlighting the Crown's view of it as pure extraction of royal authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_remonstrance,
    'Is the remonstrance right a legitimate constitutional check on royal power, or an illegitimate minoritarian veto protecting particularist privileges?',
    'Analysis of historical outcomes: did remonstrances consistently protect broader public interest or primarily entrenched elite privileges? Examination of contemporary legal theory on constitutional checks and balances.',
    'If legitimate, the constraint would reclassify towards a Rope or even a Mountain (from the magistrate''s perspective); if illegitimate, its Snare classification (from the Crown''s perspective) is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_remonstrance, conceptual, 'Ambiguity regarding the constitutional legitimacy and function of the remonstrance right.').

omega_variable(
    fiscal_necessity_vs_royal_prerogative,
    'Were the Crown''s fiscal and administrative reforms genuinely necessary for the public good, or were they primarily aimed at expanding royal prerogative and centralizing power?',
    'Economic analysis of the state''s financial condition and the impact of proposed reforms, compared with the actual outcomes of royal policy.',
    'If reforms were genuinely necessary, the Crown''s suppression of remonstrance might be seen as a necessary (though still extractive) means to a public good; if primarily self-serving, the extraction is more clearly illegitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_necessity_vs_royal_prerogative, empirical, 'The underlying motivation for the Crown''s actions and its impact on the public good.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1600, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1600, remonstrance_authority__crown_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(remo_tr_t1650, remonstrance_authority__crown_reading, theater_ratio, 1650, 0.35).
narrative_ontology:measurement(remo_tr_t1700, remonstrance_authority__crown_reading, theater_ratio, 1700, 0.38).
narrative_ontology:measurement(remo_tr_t1750, remonstrance_authority__crown_reading, theater_ratio, 1750, 0.4).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__crown_reading, theater_ratio, 1789, 0.4).

% Extraction over time
narrative_ontology:measurement(remo_be_t1600, remonstrance_authority__crown_reading, base_extractiveness, 1600, 0.7).
narrative_ontology:measurement(remo_be_t1650, remonstrance_authority__crown_reading, base_extractiveness, 1650, 0.75).
narrative_ontology:measurement(remo_be_t1700, remonstrance_authority__crown_reading, base_extractiveness, 1700, 0.8).
narrative_ontology:measurement(remo_be_t1750, remonstrance_authority__crown_reading, base_extractiveness, 1750, 0.83).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__crown_reading, base_extractiveness, 1789, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1600, remonstrance_authority__crown_reading, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(remo_su_t1650, remonstrance_authority__crown_reading, suppression_requirement, 1650, 0.6).
narrative_ontology:measurement(remo_su_t1700, remonstrance_authority__crown_reading, suppression_requirement, 1700, 0.65).
narrative_ontology:measurement(remo_su_t1750, remonstrance_authority__crown_reading, suppression_requirement, 1750, 0.68).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__crown_reading, suppression_requirement, 1789, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'remonstrance_authority' kernel. The 'magistrate_reading' is a sibling constraint that presents an alternative interpretation of the remonstrance's function and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
