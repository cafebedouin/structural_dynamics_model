% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention: Expansive Humanitarian Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates an 'expansive humanitarian reading' of the
 *   1951 Refugee Convention, which interprets 'well-founded fear' to include
 *   generalized violence and non-state persecution, and 'particular social
 *   group' to encompass gender, LGBTQ+, and clan-based persecution. It views
 *   the Convention as an unbendable humanitarian mandate requiring broad
 *   protection, and considers interdiction and offshore processing as
 *   refoulement violations. The metrics reflect the high extraction and
 *   suppression experienced by asylum seekers under current state practices,
 *   as assessed by this reading's own lights, despite the reading's claim
 *   that the Convention *should be* a Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.85).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.9).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention: Expansive Humanitarian Reading").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '3337f654-8ec4-4114-88aa-a5cdb2d5514b').
narrative_ontology:cs_kernel_codification('3337f654-8ec4-4114-88aa-a5cdb2d5514b', fixed_text).
narrative_ontology:cs_authority_grounding('3337f654-8ec4-4114-88aa-a5cdb2d5514b', lineage).
narrative_ontology:cs_interpretation_layer_present('3337f654-8ec4-4114-88aa-a5cdb2d5514b').
narrative_ontology:cs_reading_relation('3337f654-8ec4-4114-88aa-a5cdb2d5514b', refugee_convention_text__restrictive_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('3337f654-8ec4-4114-88aa-a5cdb2d5514b', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('3337f654-8ec4-4114-88aa-a5cdb2d5514b', foundational, non_refoulement_absolute).
narrative_ontology:cs_axiom_status(non_refoulement_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3337f654-8ec4-4114-88aa-a5cdb2d5514b', non_refoulement_absolute, deontological).
narrative_ontology:cs_axiom('3337f654-8ec4-4114-88aa-a5cdb2d5514b', foundational, protection_extends_to_non_state_actors_and_generalized_violence).
narrative_ontology:cs_axiom_status(protection_extends_to_non_state_actors_and_generalized_violence, holdable).
narrative_ontology:cs_axiom_grounding('3337f654-8ec4-4114-88aa-a5cdb2d5514b', protection_extends_to_non_state_actors_and_generalized_violence, conventional).
narrative_ontology:cs_reference_frame('3337f654-8ec4-4114-88aa-a5cdb2d5514b', universal_human_rights_framework).
narrative_ontology:cs_drift_state('3337f654-8ec4-4114-88aa-a5cdb2d5514b', contemporary_migration_crises, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3337f654-8ec4-4114-88aa-a5cdb2d5514b', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, refugee_advocates).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, humanitarian_ngos).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, refugee_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, border_enforcement_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and defend an expansive interpretation of the Refugee Convention, pushing for broad protection criteria and challenging restrictive state practices. Their mandate is strengthened by this reading.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugee_advocates, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, refugee_advocates, beneficiary).

% Bear the direct costs of restrictive interpretations, facing detention, refoulement, and denial of basic rights. Their lives and safety depend on the Convention's protective scope.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Provide direct assistance and legal aid to asylum seekers, relying on the Convention's humanitarian mandate to justify their work. An expansive reading legitimizes and facilitates their operations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, humanitarian_ngos, beneficiary,
    organized, biographical, constrained, global).

% Are signatories to the Convention and thus nominally bound by it. However, many actively implement restrictive policies that contradict this expansive reading, often citing national security or economic concerns. They are the primary enforcers of the overall system.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, states_parties, agenda_setter,
    institutional, generational, constrained, global).

% Interpret international law, including the Refugee Convention, and can issue rulings that influence its application. They are often a forum for challenging restrictive state practices.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_courts, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, international_courts, agenda_setter).

% Implement state policies at borders, often involving interdiction, detention, and expedited removal, which are seen by this reading as violations of non-refoulement. They bear the operational costs of these policies.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, border_enforcement_agencies, payer,
    institutional, immediate, constrained, national).

% Suffer family separation and prolonged uncertainty due to restrictive interpretations of the Convention, which limit their ability to seek asylum together or reunite.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugee_families, payer,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common international framework for states to coordinate the protection of individuals fleeing persecution, ensuring a minimum standard of humanitarian treatment and burden-sharing.
% TRANSFER_FUNCTION: Transfers the burden of protection from individuals fleeing persecution to states, requiring states to provide asylum, legal status, and basic rights, rather than leaving individuals stateless and vulnerable.
% ABSENT_VOICES: Individuals and communities in transit or in regions without effective state protection are often excluded from the formal mechanisms of the Convention, their voices unheard in policy debates, despite being most directly affected.
% DISAPPEARANCE_RATIONALE: If this expansive reading of the Convention vanished, states would likely revert to more restrictive, sovereignty-driven approaches, leading to increased refoulement, humanitarian crises at borders, and a collapse of international cooperation on refugee protection. The global system for managing forced migration would fundamentally reorganize.
% FOUNDING_PROBLEM: The post-WWII displacement crisis, where millions were stateless and vulnerable, lacking international legal protection against return to persecution.
% FOUNDING_PROBLEM_CORROBORATION: Refugee advocates, humanitarian NGOs, and international bodies like UNHCR consistently attest that the core problem of forced displacement and persecution remains live, citing ongoing conflicts and human rights abuses. While states acknowledge displacement, many contest the extent of their obligations under the Convention, particularly regarding non-state persecution and generalized violence.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the severe costs borne by asylum seekers due to restrictive state practices that deny protection despite clear humanitarian need. Suppression (0.90) is high due to active state enforcement of border controls, interdiction, and legal barriers designed to limit access to asylum. The theater ratio (0.60) indicates that while states maintain the appearance of compliance with international law, a significant portion of their actions are performative, masking a departure from the Convention's humanitarian spirit. Accessibility collapse (0.75) is high as states actively close off pathways to asylum. Resistance (0.70) is substantial from NGOs, advocates, and asylum seekers themselves.
 *
 * PERSPECTIVAL GAP:
 *   The claimed type of 'rope' reflects the ideal function of the Convention as a coordination mechanism for humanitarian protection, as envisioned by this expansive reading. However, the high extractiveness and suppression metrics reflect the reality of state practices, which often diverge sharply from this ideal. This gap highlights the contest between the Convention's intended humanitarian purpose and its often restrictive application by states.
 *
 * DIRECTIONALITY LOGIC:
 *   From the perspective of this reading, refugee advocates and humanitarian NGOs are beneficiaries, as their core mission aligns with and is strengthened by a broad interpretation of the Convention. Asylum seekers and refugee families are the primary targets/payers, bearing the costs of non-protection. States Parties, while nominally bound, often act as agents of extraction by implementing restrictive policies, making them targets of critique from this reading's perspective. International courts and border enforcement agencies play institutional roles, with the former potentially acting as a check and the latter as an instrument of suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This expansive reading actively combats mandatrophy by insisting that the Convention's original humanitarian mandate remains live and must be applied broadly to contemporary forms of persecution. It prevents mislabeling by exposing how state practices, often justified by 'sovereignty' or 'security,' are in fact extractive and suppressive, rather than genuine coordination or necessary enforcement of a humanitarian framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_well_founded_fear,
    'To what extent does ''well-founded fear of persecution'' genuinely encompass generalized violence, non-state actors, and gender/LGBTQ+ based persecution under international customary law?',
    'Further jurisprudence from international courts, state practice evolution, and scholarly consensus on the interpretation of ''persecution'' in contemporary contexts.',
    'If these elements are widely recognized, the expansive reading gains stronger legal grounding, increasing pressure on states to comply. If not, the reading''s claims may be seen as an overreach, weakening its legal force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_well_founded_fear, conceptual, 'Ambiguity regarding the scope of ''well-founded fear'' in the Refugee Convention.').

omega_variable(
    refoulement_vs_interdiction,
    'Is interdiction at sea or offshore processing, without full individual assessment, a violation of the principle of non-refoulement, or a legitimate exercise of state border control?',
    'Binding rulings from international tribunals (e.g., ECtHR, ICJ) specifically addressing the extraterritorial application of non-refoulement in interdiction contexts, or a new international protocol clarifying these practices.',
    'If deemed refoulement, state practices would be reclassified as severe violations, increasing the measured suppression and extractiveness. If deemed legitimate, the expansive reading''s critique of these practices would be weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refoulement_vs_interdiction, empirical, 'Contest over whether interdiction and offshore processing constitute refoulement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(refu_tr_t2005, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2005, 0.5).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1951, 0.4).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(refu_be_t2005, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2015, 0.83).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(refu_su_t2005, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2015, 0.87).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_law_framework).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, state_border_control_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'refugee_convention_text' kernel, alongside 'restrictive_sovereignty_reading' and 'procedural_integrity_reading'. Each reading instantiates a distinct constraint with its own ε and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
