% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Revelation of Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the radical religious reforms initiated by
 *   Pharaoh Akhenaten, who declared Aten (the sun disk) to be the sole,
 *   exclusive deity, and himself as Aten's only legitimate interpreter. This
 *   involved the active suppression of all other traditional Egyptian gods,
 *   particularly the powerful cult of Amun, and the dismantling of their
 *   temple economies. The constraint is a reading of how divine legitimacy
 *   was structured during this brief, tumultuous period.
 *
 * KEY AGENTS:
 *   - pharaoh_akhenaten: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - atenist_priesthood: Secondary beneficiary (organized/constrained)
 *   - amun_priesthood: Primary target/victim (institutional/trapped)
 *   - traditional_cults: Secondary target/victim (organized/trapped)
 *   - common_people: Diffuse target/victim (powerless/constrained)
 *   - military: Enforcement arm (institutional/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.9).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.95).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, snare).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Revelation of Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '9a34624c-d2e6-4f6d-8f72-5f687f6947c5').
narrative_ontology:cs_kernel_codification('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', formalized).
narrative_ontology:cs_authority_grounding('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', extraction).
narrative_ontology:cs_interpretation_layer_present('9a34624c-d2e6-4f6d-8f72-5f687f6947c5').
narrative_ontology:cs_reading_relation('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', foundational, aten_sole_creator_god).
narrative_ontology:cs_axiom_status(aten_sole_creator_god, holdable).
narrative_ontology:cs_axiom_grounding('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', aten_sole_creator_god, theological).
narrative_ontology:cs_axiom('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', foundational, pharaoh_sole_divine_interpreter).
narrative_ontology:cs_axiom_status(pharaoh_sole_divine_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', pharaoh_sole_divine_interpreter, conventional).
narrative_ontology:cs_reference_frame('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', pharaonic_atenist_monotheism).
narrative_ontology:cs_drift_state('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', post_akhenaten_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('9a34624c-d2e6-4f6d-8f72-5f687f6947c5', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, atenist_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_cults).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, common_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sole interpreter and conduit of Aten's divine will, centralizing all religious and political power. Benefits from the dismantling of rival power centers and the establishment of a new theological basis for absolute rule.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, agenda_setter,
    institutional, civilizational, arbitrage, national).

% A newly established priestly class loyal to Akhenaten, gaining wealth, status, and influence from the new religious order. Their existence is entirely dependent on the pharaoh's decree.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, atenist_priesthood, beneficiary,
    organized, generational, constrained, national).

% The traditional, powerful priesthood of Amun, whose temples are closed, wealth confiscated, and names erased. They are the primary target of the Atenist reforms, losing all power and resources.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    institutional, generational, trapped, national).

% Local and regional cults dedicated to various traditional deities, whose practices are outlawed and whose adherents are forced to abandon their ancestral worship. They face suppression and loss of community identity.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_cults, payer,
    organized, generational, trapped, local).

% Forced to abandon traditional religious practices and adopt the Atenist cult, which is less accessible and mediated solely through the pharaoh. They bear the social and spiritual costs of forced conversion and the disruption of daily life.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, common_people, payer,
    powerless, biographical, constrained, local).

% The enforcement arm of the pharaoh, tasked with dismantling traditional temples, erasing names of old gods, and suppressing dissent. They gain stability and direct patronage from the pharaoh.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, military, agenda_setter,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes religious authority and state power under a single deity (Aten) and a single interpreter (the pharaoh), aiming to unify the kingdom's spiritual and political life.
% TRANSFER_FUNCTION: Transfers immense religious authority, economic resources (temple lands, offerings), and political influence from the diverse traditional priesthoods and cults to the pharaoh and the newly established Atenist priesthood.
% ABSENT_VOICES: The dispossessed Amun priesthood, local cult leaders, and common people whose ancestral practices are suppressed. They would object to the theological exclusivity, the destruction of their heritage, and the concentration of power, but their voices are actively silenced.
% DISAPPEARANCE_RATIONALE: If the Atenist decree and its enforcement vanished overnight, the entire religious, political, and economic structure of Egypt would immediately revert to its traditional polytheistic forms, with the Amun priesthood regaining its power and influence. The pharaoh's authority would be severely challenged.
% FOUNDING_PROBLEM: The pharaoh perceived a fragmentation of divine authority, the excessive power and wealth of the Amun priesthood challenging royal control, and a need for a new, more direct theological basis for pharaonic absolute rule.
% FOUNDING_PROBLEM_CORROBORATION: Primarily attested by Akhenaten's own royal decrees, hymns, and inscriptions. Historical and archaeological evidence from outside the benefiting parties (e.g., the rapid post-Akhenaten restoration of traditional cults) suggests the 'problem' was largely a political justification for a power grab, rather than a universally acknowledged spiritual crisis.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) due to the complete expropriation of wealth and power from traditional religious institutions. Suppression is extremely high (0.95) as the state actively persecuted other cults, erased names, and enforced the new monotheism. Theater ratio is low (0.1) because Akhenaten's reforms were a genuine, albeit short-lived, attempt to establish a new religious order, not merely performative maintenance of an existing one. Accessibility collapse is high (0.9) as alternatives were actively outlawed. Resistance is high (0.7) due to the deep-seated nature of traditional beliefs and the power of the dispossessed priesthoods, even if overt resistance was suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From Akhenaten's perspective, this was a divine revelation and a necessary purification of religious truth, leading to a more unified and legitimate state. From the perspective of the Amun priesthood and traditional cults, it was a tyrannical imposition, a sacrilege, and a direct assault on their power, wealth, and spiritual heritage. The common people experienced it as a disruptive and alienating force.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh Akhenaten and the newly formed Atenist priesthood are the clear beneficiaries, gaining absolute religious and political authority and control over resources. The Amun priesthood, other traditional cults, and the common people are the victims, suffering expropriation, suppression, and forced conversion. The military acts as the enforcement arm, benefiting from direct royal patronage.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the typical sense, as it was a revolutionary imposition rather than an atrophied function. Its persistence depended entirely on the pharaoh's active enforcement and belief. The question of its 'mandate' is tied directly to the pharaoh's reign and theological conviction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_political_motivation,
    'Was Akhenaten''s Atenist reform primarily a genuine religious conviction or a political maneuver to consolidate power and break the Amun priesthood''s influence?',
    'Further archaeological discoveries of Akhenaten''s personal writings or contemporary accounts from outside the royal court, or re-evaluation of existing evidence through new historical methodologies.',
    'If primarily political, the extractiveness and suppression metrics are more clearly ''snare-like'' and less justifiable by a ''coordination'' function. If primarily religious, the constraint might be seen as a ''mountain'' of revealed truth by its adherents, though still extractive for others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_vs_political_motivation, conceptual, 'Ambiguity of Akhenaten''s primary motivation for the Atenist reforms.').

omega_variable(
    suppression_effectiveness_vs_underground_adherence,
    'How completely did the Atenist regime suppress traditional beliefs, versus driving them underground or into private practice?',
    'Archaeological evidence of hidden shrines, private religious artifacts, or non-Atenist burial practices during Akhenaten''s reign.',
    'If traditional beliefs persisted widely underground, the effective suppression was lower than the overt measures suggest, indicating a less complete ''accessibility collapse'' for alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_effectiveness_vs_underground_adherence, empirical, 'The true extent of suppression of traditional religious practices.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the `atenist_monotheistic_reading` of the `divine_legitimacy_substrate` kernel. What structural changes would a sibling reading (e.g., `amun_polytheistic_reading` or `folk_syncretistic_reading`) entail?',
    'Analysis of historical periods dominated by these sibling readings, comparing their institutional structures, power distributions, and religious practices.',
    'The `amun_polytheistic_reading` would feature a powerful, wealthy Amun priesthood as a primary beneficiary and agenda-setter, with the pharaoh''s power constrained by religious tradition. The `folk_syncretistic_reading` would distribute religious authority more diffusely, with local cults and household rituals as primary beneficiaries, and less centralized extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural differences between kernel readings of divine legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(divi_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.1).
narrative_ontology:measurement(divi_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(divi_tr_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.1).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.78).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.85).
narrative_ontology:measurement(divi_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.88).
narrative_ontology:measurement(divi_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.9).
narrative_ontology:measurement(divi_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.85).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.9).
narrative_ontology:measurement(divi_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.93).
narrative_ontology:measurement(divi_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.95).
narrative_ontology:measurement(divi_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
