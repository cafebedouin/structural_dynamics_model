% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Partial Withdrawal Reading
 *   domain: international_law/diplomacy
 *
 * SUMMARY:
 *   UN Security Council Resolution 242, passed in 1967, calls for the
 *   'withdrawal of Israeli armed forces from territories occupied in the
 *   recent conflict.' This constraint story instantiates the 'partial
 *   withdrawal' reading, which interprets the indefinite article
 *   'territories' (in the English text) as permitting Israel to retain some
 *   strategic territories for secure boundaries, rather than requiring
 *   withdrawal from all territories. This reading converts textual
 *   indefiniteness into negotiating leverage, benefiting the occupying power
 *   and mediators, while placing claimant states at a disadvantage due to the
 *   lack of a fixed enforcement line. The constraint is claimed as a Tangled
 *   Rope because it provides a framework for coordination (negotiation,
 *   secure boundaries) but simultaneously enables asymmetric extraction
 *   (retention of territory, diplomatic leverage) and requires active
 *   diplomatic and political enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.6).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause: Partial Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomacy").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'c871dabd-bcc1-44ad-b359-01b435f772d2').
narrative_ontology:cs_kernel_codification('c871dabd-bcc1-44ad-b359-01b435f772d2', fixed_text).
narrative_ontology:cs_authority_grounding('c871dabd-bcc1-44ad-b359-01b435f772d2', lineage).
narrative_ontology:cs_interpretation_layer_present('c871dabd-bcc1-44ad-b359-01b435f772d2').
narrative_ontology:cs_reading_relation('c871dabd-bcc1-44ad-b359-01b435f772d2', unsc_242_withdrawal_clause__maximal_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('c871dabd-bcc1-44ad-b359-01b435f772d2', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('c871dabd-bcc1-44ad-b359-01b435f772d2', foundational, indefinite_article_permits_discretion).
narrative_ontology:cs_axiom_status(indefinite_article_permits_discretion, holdable).
narrative_ontology:cs_axiom_grounding('c871dabd-bcc1-44ad-b359-01b435f772d2', indefinite_article_permits_discretion, conventional).
narrative_ontology:cs_axiom('c871dabd-bcc1-44ad-b359-01b435f772d2', foundational, security_precedes_full_withdrawal).
narrative_ontology:cs_axiom_status(security_precedes_full_withdrawal, holdable).
narrative_ontology:cs_axiom_grounding('c871dabd-bcc1-44ad-b359-01b435f772d2', security_precedes_full_withdrawal, instrumental).
narrative_ontology:cs_reference_frame('c871dabd-bcc1-44ad-b359-01b435f772d2', negotiated_withdrawal_framework).
narrative_ontology:cs_drift_state('c871dabd-bcc1-44ad-b359-01b435f772d2', contemporary_diplomatic_stalemate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c871dabd-bcc1-44ad-b359-01b435f772d2', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, claimant_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupied_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the ambiguity of the withdrawal clause, using it to justify the retention of strategic territories and control the pace of any withdrawal. It leverages diplomatic and military power to maintain its interpretation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    institutional, generational, arbitrage, regional).

% Seek full withdrawal from all occupied territories but are forced to negotiate within a framework that permits partial withdrawal. They bear the political and economic costs of continued occupation and diplomatic stalemate.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, claimant_states, payer,
    institutional, generational, constrained, regional).

% Gain diplomatic influence and stability by facilitating negotiations based on this interpretation. They act as brokers, often finding this reading a pragmatic basis for dialogue, even if it doesn't fully satisfy all parties.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states, beneficiary,
    institutional, biographical, mobile, global).

% Live under continued occupation, experiencing its daily costs and restrictions. They are largely excluded from the high-level diplomatic negotiations that interpret the resolution, and their demands for full withdrawal are often unheeded.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupied_populations, payer,
    powerless, generational, trapped, local).

% The body that passed the resolution; its members continue to interpret and enforce it, often divided on the scope of withdrawal. Its authority is invoked by all parties, but its capacity to enforce a single interpretation is limited by geopolitical realities.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the legal implications and historical context of the resolution, often critiquing or supporting different interpretations. Their work informs diplomatic positions but does not directly determine policy.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_legal_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for diplomatic negotiations regarding territorial withdrawal and secure boundaries, aiming to prevent immediate, full-scale conflict by offering a basis for phased disengagement and recognition.
% TRANSFER_FUNCTION: Transfers negotiating leverage and de facto control over strategic territories to the occupying power, while requiring some withdrawal. It also transfers the burden of diplomatic compromise to claimant states and occupied populations.
% ABSENT_VOICES: The populations under occupation are not directly represented in the diplomatic negotiations; they would demand immediate and full withdrawal based on the principle of inadmissibility of territory acquisition by war.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the existing diplomatic framework for resolving the conflict would collapse. This would likely lead to renewed conflict, a complete re-evaluation of the resolution's legal standing, and a scramble for a new basis for negotiation, fundamentally reorganizing regional power dynamics.
% FOUNDING_PROBLEM: To establish a framework for peace in the Middle East following the 1967 war, balancing the need for withdrawal from occupied territories with the need for secure and recognized boundaries for all states in the region.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic records, statements from UN officials, and ongoing peace process efforts corroborate the original intent to balance withdrawal with security. However, claimant states and many legal scholars attest that the 'secure boundaries' principle has been over-extended to justify indefinite retention of territory, indicating a contested status of the problem's current manifestation.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is moderate because while it permits retention of territory, it still mandates some withdrawal and provides a basis for negotiation, preventing outright annexation. Suppression (0.60) is moderate-high, as diplomatic and political pressure is consistently applied to maintain this interpretation and limit alternatives for claimant states. The theater ratio (0.20) is relatively low, as this interpretation is actively used in ongoing diplomatic efforts, even if progress is slow. Accessibility collapse (0.45) is moderate, as it limits the options for claimant states but does not completely eliminate their ability to pursue their claims, albeit through a constrained framework. Resistance (0.50) is moderate, reflecting the ongoing diplomatic and legal challenges from claimant states and their allies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the occupying power, this reading is a legitimate and necessary interpretation for national security and regional stability. From the perspective of claimant states and occupied populations, it is a legal justification for continued occupation and a mechanism for extracting territorial concessions, undermining international law principles.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power is a primary beneficiary (low d) as it gains diplomatic leverage and the ability to retain strategic territories. Mediating states also benefit (low d) by having a pragmatic framework for engagement and conflict management. Claimant states and occupied populations are the primary targets (high d), bearing the costs of continued occupation and constrained diplomatic options. The UN Security Council, as an agenda-setter, experiences a more symmetric directionality (near 0.5), as it seeks to coordinate peace but also faces internal divisions and external pressures regarding the resolution's interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading, while enabling extraction, maintains a coordination function by providing a framework for negotiation, preventing the constraint from becoming a pure Snare. However, if the 'withdrawal' aspect of the resolution becomes purely performative without substantive territorial changes, and the 'secure boundaries' principle is used indefinitely to justify occupation, the constraint risks drifting towards a Snare or Piton, where the original mandate for peace and withdrawal has atrophied into a mechanism for maintaining the status quo.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefinite_article_intent,
    'Was the indefinite English article ''territories'' in UNSC Resolution 242 truly intended by the drafters to allow for partial withdrawal, or was it a diplomatic compromise that has been reinterpreted to serve political ends?',
    'Analysis of declassified diplomatic cables, drafting session minutes, and testimonies from original drafters (if available) to ascertain the explicit intent behind the wording.',
    'If definitive evidence shows intent for full withdrawal, this reading''s legitimacy would collapse, shifting the constraint towards a maximal withdrawal interpretation. If intent for partial withdrawal is confirmed, this reading''s foundation would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_article_intent, empirical, 'Ambiguity regarding the drafters'' original intent for the indefinite article in Resolution 242.').

omega_variable(
    secure_boundaries_vs_territorial_integrity,
    'How does the ''secure boundaries'' principle, central to this reading, balance against the fundamental international law principle of the inadmissibility of the acquisition of territory by war?',
    'Judicial review by an international court (e.g., ICJ) or a consensus-based re-affirmation by the UN Security Council on the hierarchical relationship between these two principles in the context of Resolution 242.',
    'If territorial integrity is deemed paramount, this reading''s justification for retaining strategic territories would be significantly weakened, increasing pressure for full withdrawal. If secure boundaries are given equal or greater weight, this reading would be further entrenched.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secure_boundaries_vs_territorial_integrity, conceptual, 'Conceptual tension between the ''secure boundaries'' principle and the inadmissibility of territory acquisition by war.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(unsc_tr_t2023, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1995, 0.53).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(unsc_be_t2023, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2023, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(unsc_su_t2023, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2023, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, middle_east_peace_process).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_law_of_occupation).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UNSC Resolution 242 withdrawal clause kernel. This reading focuses on the discretionary scope of withdrawal, contrasting with the maximal withdrawal reading and the interpretive authority structure reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
