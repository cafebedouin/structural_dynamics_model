% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Principle: Distributed Maintenance of Cosmic Order
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint represents the 'distributed maintenance' reading of the
 *   Ma'at principle in ancient Egypt, where cosmic order is sustained through
 *   the proper conduct of all actors, from the Pharaoh to the commoner, each
 *   fulfilling their station. This reading emphasizes collective
 *   responsibility and adherence to ethical principles as the foundation of
 *   stability, rather than solely divine decree or reciprocal exchange. It is
 *   claimed as a Rope due to its genuine coordination function and low
 *   inherent extraction from this specific perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.15).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.4).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Principle: Distributed Maintenance of Cosmic Order").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__distributed_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '1db013cf-d253-419f-b98a-f79ec053c6d3').
narrative_ontology:cs_kernel_codification('1db013cf-d253-419f-b98a-f79ec053c6d3', formalized).
narrative_ontology:cs_authority_grounding('1db013cf-d253-419f-b98a-f79ec053c6d3', practice).
narrative_ontology:cs_interpretation_layer_present('1db013cf-d253-419f-b98a-f79ec053c6d3').
narrative_ontology:cs_reading_relation('1db013cf-d253-419f-b98a-f79ec053c6d3', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('1db013cf-d253-419f-b98a-f79ec053c6d3', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('1db013cf-d253-419f-b98a-f79ec053c6d3', foundational, individual_conduct_sustains_cosmic_order).
narrative_ontology:cs_axiom_status(individual_conduct_sustains_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('1db013cf-d253-419f-b98a-f79ec053c6d3', individual_conduct_sustains_cosmic_order, deontological).
narrative_ontology:cs_axiom('1db013cf-d253-419f-b98a-f79ec053c6d3', foundational, legitimacy_from_adherence_not_status).
narrative_ontology:cs_axiom_status(legitimacy_from_adherence_not_status, holdable).
narrative_ontology:cs_axiom_grounding('1db013cf-d253-419f-b98a-f79ec053c6d3', legitimacy_from_adherence_not_status, conventional).
narrative_ontology:cs_reference_frame('1db013cf-d253-419f-b98a-f79ec053c6d3', collective_moral_stewardship).
narrative_ontology:cs_drift_state('1db013cf-d253-419f-b98a-f79ec053c6d3', historical_peak_of_egyptian_civilization, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1db013cf-d253-419f-b98a-f79ec053c6d3', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, all_of_egyptian_society).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, priests_and_scribes).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, commoners).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, cosmic_harmony_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, social_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the highest earthly authority, the Pharaoh is responsible for embodying Ma'at and ensuring its principles are upheld throughout the land. His conduct is seen as crucial for cosmic stability, but his authority is also derived from his adherence to Ma'at, not solely from divine right. He benefits from the stability Ma'at provides but is also bound by its demands.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, generational, constrained, national).

% Interpret Ma'at, educate the populace, and perform rituals to maintain cosmic balance. They benefit from their role as custodians of knowledge and ritual, holding significant social status, but their legitimacy depends on their own adherence to Ma'at's principles and their ability to guide society in its maintenance.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, priests_and_scribes, beneficiary,
    organized, biographical, constrained, national).

% Sustain Ma'at through daily proper conduct, honesty, and fulfilling their social roles. They bear the cost of self-regulation and social pressure to conform, but also benefit from the stability and justice Ma'at is believed to bring. Their identity is deeply intertwined with their place in the cosmic order, making deviation unthinkable.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, commoners, payer,
    powerless, immediate, identity_locked, local).

% Collectively benefits from the cosmic and social harmony that Ma'at is believed to ensure, including agricultural prosperity, political stability, and a predictable afterlife. This collective benefit is the primary coordination function of the constraint.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, all_of_egyptian_society, beneficiary,
    organized, generational, constrained, national).

% The abstract principle of truth, justice, and cosmic balance that all actions are meant to uphold. It is the ultimate referent for all conduct and the state that is maintained by collective effort, rather than an active agent.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, cosmic_order, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(maat_order_principle__distributed_maintenance_reading, cosmic_order).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a stable, just, and predictable social and cosmic order by coordinating the conduct of all individuals according to shared principles of truth, balance, and righteousness, thereby preventing chaos (Isfet).
% TRANSFER_FUNCTION: Transfers the responsibility for cosmic and social stability to all individuals through their proper conduct, and in return, transfers perceived blessings and order from the cosmos to society.
% ABSENT_VOICES: Those who would seek to centralize power solely in the Pharaoh, or those who would prioritize individual gain over collective cosmic balance, are implicitly excluded by the pervasive social and religious framework that emphasizes distributed responsibility. Their voices would challenge the very foundation of this reading of Ma'at.
% DISAPPEARANCE_RATIONALE: If the principle of Ma'at and its distributed maintenance vanished, the entire social, political, and religious fabric of ancient Egypt would collapse. The legitimacy of the Pharaoh, the role of the priesthood, and the daily conduct of commoners would lose their grounding, leading to chaos and perceived cosmic imbalance. The society would fundamentally reorganize or cease to exist in its recognized form.
% FOUNDING_PROBLEM: To establish and maintain a stable, just, and harmonious society and cosmos, preventing chaos (Isfet) and ensuring the prosperity and continuity of Egypt.
% FOUNDING_PROBLEM_CORROBORATION: Ancient Egyptian religious texts, wisdom literature, and historical records consistently attest to the centrality of Ma'at and the ongoing struggle against Isfet, corroborating that the problem of maintaining order was always live and required continuous effort from all levels of society. This is attested by non-Pharaonic sources and common religious practice.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.15) because the primary 'cost' is behavioral conformity and self-regulation, which is seen as a necessary contribution to a universally beneficial cosmic order, not a material transfer to a specific party. Suppression is moderate (0.4) as it relies heavily on internalized norms, social pressure, and religious belief, rather than overt coercion for every infraction. Theater ratio is low (0.1) because the belief in the efficacy of proper conduct for maintaining Ma'at was deeply ingrained and genuinely functional. Accessibility collapse is moderate (0.4) as alternatives to proper conduct exist (e.g., crime, rebellion) but carry severe social, religious, and existential costs. Resistance is low (0.2) because the principle was widely accepted and internalized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the commoner, the constraint involves significant behavioral costs and social pressure, making it feel more like a payer role. However, from the perspective of the entire society, including the Pharaoh and priests, the distributed maintenance of Ma'at is a collective good that ensures stability and prosperity for all, making it a beneficial coordination mechanism. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   All of Egyptian society, including the Pharaoh and priests, are beneficiaries of the cosmic and social order maintained by Ma'at. Commoners are primarily payers through their daily conduct and adherence to norms. The distributed nature of responsibility means that while everyone contributes, the 'extraction' is diffuse and primarily behavioral, aimed at maintaining a collective good.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of Ma'at is not subject to mandatrophy in the traditional sense, as its 'mandate' (maintaining cosmic order) is considered perpetually live and essential. The distributed responsibility ensures continuous 'maintenance' by all participants, preventing atrophy of function. The low extraction and high perceived benefit from this reading prevent it from being mislabeled as pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''distributed maintenance'' reading of the Ma''at principle, distinct from other interpretations?',
    'Comparative textual analysis of ancient Egyptian religious and wisdom literature, focusing on explicit statements regarding individual vs. pharaonic responsibility for Ma''at.',
    'If this reading is found to be less prominent than others, its classification as a Rope (low extraction, high coordination) might be less representative of the overall Ma''at principle, potentially shifting the aggregate classification of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific interpretation of the Ma''at kernel being modeled.').

omega_variable(
    natural_vs_constructed_ambiguity,
    'To what extent was Ma''at perceived as an immutable natural law of the cosmos versus a socially constructed and maintained ethical framework?',
    'Analysis of theological texts and daily practices: if deviations from Ma''at were attributed to human failing rather than cosmic instability, it suggests a stronger social construction; if cosmic disasters were directly linked to human transgression, it suggests a stronger ''natural law'' perception.',
    'If more strongly a natural law, the constraint would lean towards Mountain (even lower extraction, higher accessibility collapse); if more strongly constructed, it reinforces its classification as a Rope (social coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, empirical, 'Ambiguity of Ma''at''s ontological status as natural law or social construct.').

omega_variable(
    actual_extraction_level_from_commoners,
    'Despite the low base extractiveness, does the pervasive social and religious pressure to conform to Ma''at''s demands constitute a higher effective extraction from commoners due to their limited exit options and identity lock?',
    'Detailed anthropological and historical analysis of commoner daily life, including instances of non-conformity and their consequences, to quantify the ''cost'' of adherence beyond simple behavioral regulation.',
    'If effective extraction from commoners is significantly higher, it would push their per-seat classification towards a Tangled Rope or Snare, even if the overall constraint remains a Rope from a societal perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_extraction_level_from_commoners, empirical, 'Assesses the true burden of Ma''at''s demands on commoners given their structural position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__distributed_maintenance_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__distributed_maintenance_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__distributed_maintenance_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__distributed_maintenance_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Ma'at order principle' kernel, each representing a distinct structural claim about how cosmic order is maintained in ancient Egypt. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
