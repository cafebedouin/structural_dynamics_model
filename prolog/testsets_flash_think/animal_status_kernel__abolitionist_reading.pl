% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Abolitionist Reading of Animal Status: Right Not to Be Property
 *   domain: Moral Philosophy/Animal Ethics/Legal Theory
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of the
 *   'animal_status_kernel', which posits that animals are moral persons with
 *   a basic right not to be property. From this perspective, property status
 *   itself is the fundamental injustice, rendering all use of animals
 *   categorically impermissible, regardless of welfare conditions. This
 *   reading directly challenges the prevailing legal and social order,
 *   identifying animals as victims of a deeply entrenched snare.
 *
 * KEY AGENTS:
 *   - Animals: Primary target (powerless/trapped) — bear full extraction.
 *   - Abolitionist Advocates: Agenda-setter (organized/constrained) — actively challenge the constraint.
 *   - Human Users of Animals: Primary beneficiary (powerful/mobile) — benefit from animal property status.
 *   - Welfarist Advocates: Excluded (organized/constrained) — their approach is seen as incompatible with abolitionist goals.
 *   - Legal Systems: Agenda-setter (institutional/identity_locked) — enforce animal property status.
 *   - Analytical Philosophers: Observer (analytical/analytical) — analyze the ethical foundations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.98).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Abolitionist Reading of Animal Status: Right Not to Be Property").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "Moral Philosophy/Animal Ethics/Legal Theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '646e9613-1ba6-4fc1-9021-29305018963d').
narrative_ontology:cs_kernel_codification('646e9613-1ba6-4fc1-9021-29305018963d', formalized).
narrative_ontology:cs_authority_grounding('646e9613-1ba6-4fc1-9021-29305018963d', expertise).
narrative_ontology:cs_interpretation_layer_present('646e9613-1ba6-4fc1-9021-29305018963d').
narrative_ontology:cs_reading_relation('646e9613-1ba6-4fc1-9021-29305018963d', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('646e9613-1ba6-4fc1-9021-29305018963d', animal_status_kernel__welfare_reading, forecloses).
narrative_ontology:cs_axiom('646e9613-1ba6-4fc1-9021-29305018963d', foundational, animal_personhood_axiom).
narrative_ontology:cs_axiom_status(animal_personhood_axiom, holdable).
narrative_ontology:cs_axiom_grounding('646e9613-1ba6-4fc1-9021-29305018963d', animal_personhood_axiom, deontological).
narrative_ontology:cs_axiom('646e9613-1ba6-4fc1-9021-29305018963d', foundational, property_is_injustice_axiom).
narrative_ontology:cs_axiom_status(property_is_injustice_axiom, holdable).
narrative_ontology:cs_axiom_grounding('646e9613-1ba6-4fc1-9021-29305018963d', property_is_injustice_axiom, deontological).
narrative_ontology:cs_reference_frame('646e9613-1ba6-4fc1-9021-29305018963d', species_egalitarian_justice).
narrative_ontology:cs_drift_state('646e9613-1ba6-4fc1-9021-29305018963d', contemporary_speciesist_society, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('646e9613-1ba6-4fc1-9021-29305018963d', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, human_users_of_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legally classified as property, animals are subject to human ownership, use, and exploitation across all domains (food, research, entertainment, clothing, labor). They bear the full cost of this property status, including loss of autonomy, bodily integrity, and life, with no legal recourse.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals, payer,
    powerless, immediate, trapped, universal).

% Actively campaign for the legal personhood of animals and the complete abolition of their property status. They challenge existing legal frameworks and societal norms, advocating for a paradigm shift in human-animal relations. Their efforts face significant institutional and economic resistance.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, agenda_setter,
    organized, generational, constrained, global).

% Benefit directly and extensively from the legal status of animals as property, enabling their use for food, scientific research, entertainment, companionship, and labor. This status underpins vast economic industries and cultural practices.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, human_users_of_animals, beneficiary,
    powerful, generational, mobile, global).

% Seek to improve animal conditions and minimize suffering within the existing property framework. From the abolitionist perspective, their focus on welfare reforms, while well-intentioned, implicitly legitimizes property status and delays the fundamental shift to animal personhood.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfarist_advocates, excluded,
    organized, biographical, constrained, global).

% Codify, enforce, and perpetuate the legal status of animals as property. These systems provide the foundational framework for human dominion over animals, defining their lack of rights and facilitating their instrumental use. Changing this status would require fundamental legal and constitutional reform.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_systems, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Analyze the philosophical implications of animal personhood, property status, and speciesism. They contribute to the intellectual discourse surrounding animal ethics, often providing the theoretical underpinnings for abolitionist arguments, but do not directly enforce or benefit from the constraint.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, analytical_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, human_users_of_animals).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the abolitionist perspective, the constraint coordinates human exploitation of animals by legally defining them as property, thereby removing moral and legal barriers to their instrumental use across diverse human activities.
% TRANSFER_FUNCTION: Transfers all rights, autonomy, and bodily integrity from animals to humans, enabling humans to appropriate animal bodies and lives as resources. This transfer underpins vast economic value and cultural practices.
% ABSENT_VOICES: Animals themselves are structurally absent from the legal and moral discourse that defines their status. Future generations of humans, who might reject speciesism, are also absent from the historical formation of this constraint.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, entire industries (e.g., animal agriculture, vivisection, entertainment involving animals) would collapse. Legal systems would require fundamental overhaul to accommodate animal personhood, and human-animal relations would be radically transformed, leading to a profound reorganization of society.
% FOUNDING_PROBLEM: The historical problem of how to manage, control, and exploit non-human animals for human benefit, without moral or legal constraint, to secure resources and labor.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal codes, philosophical texts justifying human dominion, and economic analyses of animal agriculture corroborate the historical problem. Abolitionist scholars and ethicists provide counter-narratives, arguing that the 'problem' was always one of unjust exploitation, not legitimate resource management.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the constraint fundamentally denies animals their most basic rights, treating them as resources. Suppression is near maximal (0.98) due to the pervasive legal and social enforcement of property status, which offers animals no exit or recourse. Theater ratio is very low (0.05) because this reading is about fundamental structural change, not performative maintenance of a degraded function; the constraint's function (enabling exploitation) is fully active. Accessibility collapse is high (0.85) as the legal and economic system makes alternatives to property status extremely difficult to realize. Resistance is high (0.75) due to the active and growing abolitionist movement.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from both the property and welfare readings. While the property reading sees animals as resources and the welfare reading seeks to mitigate suffering within that framework, the abolitionist reading views the property framework itself as the source of injustice. This creates a deep perspectival gap where what one reading considers 'normal' or 'reform' another considers 'fundamental oppression'.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the full targets (d=1.0) as they bear all costs and have no agency. Human users of animals are the full beneficiaries (d=0.0) as they derive immense benefit from animal property status. Legal systems are agenda-setters, enforcing the constraint. Abolitionist advocates are actively resisting, while welfarist advocates are structurally excluded from the core abolitionist goal.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare, not a piton, because its primary function (enabling animal exploitation) is fully active and highly extractive, benefiting identifiable parties (human users of animals). There is no significant atrophy of function; rather, there is active enforcement and substantial resistance against it. The classification prevents mislabeling a deeply extractive and actively maintained system as merely inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is the ''animal_status_kernel'' a fundamental moral truth or a contested philosophical and legal construct?',
    'Resolution depends on the adoption of a universal ethical framework or a global legal consensus on animal personhood.',
    'If a fundamental moral truth, the abolitionist reading gains universal normative force; if a construct, its persistence depends on ongoing advocacy and legal reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity regarding the inherent nature vs. constructed status of animal property.').

omega_variable(
    welfare_reform_impact,
    'Do incremental welfare reforms (as advocated by the welfare reading) ultimately advance or delay the goal of animal abolition?',
    'Longitudinal sociological and legal studies tracking the impact of welfare legislation on public perception, industry practices, and the progress of abolitionist movements.',
    'If reforms delay abolition, the abolitionist reading''s ''forecloses'' relation to the welfare reading is strengthened. If reforms are found to incrementally advance abolition, the relation might shift to ''influences'' or ''coexists_with'' under a different strategic framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_impact, empirical, 'Strategic tension between welfare reforms and abolitionist goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__abolitionist_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(anim_tr_t60, animal_status_kernel__abolitionist_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(anim_tr_t80, animal_status_kernel__abolitionist_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement(anim_tr_t100, animal_status_kernel__abolitionist_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__abolitionist_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.93).
narrative_ontology:measurement(anim_be_t60, animal_status_kernel__abolitionist_reading, base_extractiveness, 60, 0.94).
narrative_ontology:measurement(anim_be_t80, animal_status_kernel__abolitionist_reading, base_extractiveness, 80, 0.95).
narrative_ontology:measurement(anim_be_t100, animal_status_kernel__abolitionist_reading, base_extractiveness, 100, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__abolitionist_reading, suppression_requirement, 20, 0.92).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.94).
narrative_ontology:measurement(anim_su_t60, animal_status_kernel__abolitionist_reading, suppression_requirement, 60, 0.96).
narrative_ontology:measurement(anim_su_t80, animal_status_kernel__abolitionist_reading, suppression_requirement, 80, 0.97).
narrative_ontology:measurement(anim_su_t100, animal_status_kernel__abolitionist_reading, suppression_requirement, 100, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'animal_status_kernel', each with different ε values and structural properties. This abolitionist reading focuses on the right not to be property, contrasting with the property-centric and welfare-centric sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
