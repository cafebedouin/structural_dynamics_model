% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention Text: Procedural Integrity Reading
 *   domain: legal/international/migration
 *
 * SUMMARY:
 *   The 1951 Refugee Convention and its 1967 Protocol constitute a contested
 *   kernel in international law. This constraint instantiates the
 *   procedural_integrity_reading: the Convention as a procedural safeguard
 *   mandating fair individualized assessment, where protection thresholds
 *   remain flexible but process integrity is non-negotiable and outcome is
 *   secondary to procedure. Sibling readings include the
 *   expansive_humanitarian_reading (broad substantive protection) and the
 *   restrictive_sovereignty_reading (maximum sovereign discretion). Under
 *   this reading, states gain flexibility to narrow definitions but cannot
 *   eliminate substantive review; offshore processing is permissible only
 *   with full procedural guarantees. The victim set depends on procedural
 *   access rather than outcome. The story treats the standing arrangement
 *   under this reading as the referent.
 *
 * KEY AGENTS:
 *   - asylum_seekers: Primary beneficiary (powerless/trapped) â receive procedural protection
 *   - states_asylum_authorities: Dual-positioned payer/beneficiary (institutional/constrained) â bear compliance costs and sovereignty constraints while gaining substantive flexibility
 *   - domestic_judiciary: Agenda-setter (institutional/analytical) â interprets and enforces procedural requirements
 *   - unhcr: Observer (institutional/analytical) â monitors compliance and guides interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.52).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.6).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention Text: Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "legal/international/migration").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '407a823e-df96-4bb7-9b18-3bb059842aaf').
narrative_ontology:cs_kernel_codification('407a823e-df96-4bb7-9b18-3bb059842aaf', fixed_text).
narrative_ontology:cs_authority_grounding('407a823e-df96-4bb7-9b18-3bb059842aaf', lineage).
narrative_ontology:cs_interpretation_layer_present('407a823e-df96-4bb7-9b18-3bb059842aaf').
narrative_ontology:cs_reading_relation('407a823e-df96-4bb7-9b18-3bb059842aaf', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('407a823e-df96-4bb7-9b18-3bb059842aaf', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('407a823e-df96-4bb7-9b18-3bb059842aaf', foundational, outcome_secondary_to_process).
narrative_ontology:cs_axiom_status(outcome_secondary_to_process, holdable).
narrative_ontology:cs_axiom_grounding('407a823e-df96-4bb7-9b18-3bb059842aaf', outcome_secondary_to_process, conventional).
narrative_ontology:cs_axiom('407a823e-df96-4bb7-9b18-3bb059842aaf', foundational, procedural_floor_non_negotiable).
narrative_ontology:cs_axiom_status(procedural_floor_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('407a823e-df96-4bb7-9b18-3bb059842aaf', procedural_floor_non_negotiable, conventional).
narrative_ontology:cs_reference_frame('407a823e-df96-4bb7-9b18-3bb059842aaf', postwar_procedural_sovereignty_compromise).
narrative_ontology:cs_drift_state('407a823e-df96-4bb7-9b18-3bb059842aaf', contemporary_securitized_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('407a823e-df96-4bb7-9b18-3bb059842aaf', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, asylum_seekers).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, states_asylum_authorities).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, states_asylum_authorities).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, procedural_fairness_as_non_derogable).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, state_sovereignty_flexible_substance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals fleeing persecution who depend on the Convention's procedural guarantees to obtain a fair individualized assessment of their claims. Their access to protection hinges on whether states maintain non-negotiable process integrity, regardless of how substantive protection thresholds are calibrated.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers, beneficiary,
    powerless, biographical, trapped, global).

% State agencies responsible for asylum determination that gain flexibility to narrow substantive protection definitions under this reading, but must bear the administrative, legal, and sovereignty costs of maintaining fair procedures and individualized review. They cannot eliminate substantive review entirely and face legal and reputational barriers to exiting the treaty regime.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_asylum_authorities, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, states_asylum_authorities, beneficiary).

% National courts that interpret the Convention's procedural requirements, review executive asylum decisions, and enforce the non-negotiability of process integrity against restrictive state policies.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, domestic_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% The UN refugee agency monitors state compliance, issues authoritative guidelines on procedural standards, and intervenes in litigation to uphold the integrity of asylum processes worldwide.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates states around a common procedural floor for asylum determination, preventing arbitrary exclusion and ensuring that individual claims receive fair assessment regardless of how substantive protection thresholds are calibrated.
% TRANSFER_FUNCTION: Moves administrative and legal obligation from states toward procedural infrastructure (interview, appeal, legal representation) that asylum seekers can invoke; simultaneously transfers flexibility on substantive protection definitions to states in exchange for binding process commitments.
% ABSENT_VOICES: Asylum seekers with claims based on generalized violence or non-state persecution that would be recognized under the expansive humanitarian reading but lack individualized persecution proof under this reading are structurally disadvantaged by the prioritization of procedure over outcome. States seeking to eliminate substantive review entirely are constrained by this reading but remain active in the discourse.
% DISAPPEARANCE_RATIONALE: If the procedural integrity reading vanished, states would accelerate summary rejection regimes, offshore processing without individualized review, and definitional narrowing unchecked by judicial oversight; asylum seekers would lose the procedural leverage that compels states to assess claims individually; the international asylum architecture would fragment into bilateral discretion or unilateral deterrence.
% FOUNDING_PROBLEM: Post-WWII displacement crisis requiring an international framework to prevent arbitrary state rejection of refugees while preserving state sovereignty over migration control.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and regional human rights courts attest the problem remains live through ongoing mass displacement and judicial caseloads. However, an increasing number of state governments argue the procedural framework is exploited by irregular migrants and that the founding problem has mutated into border management rather than refugee protection per se; this view is corroborated by state submissions to the UN Global Compact process and restrictive legislative reforms, though these sources are not neutral.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate: the constraint genuinely coordinates asylum governance by preventing arbitrary exclusion, but it extracts significant compliance costs and sovereignty limitations from states. Suppression (0.60) reflects the active enforcement required through domestic and international litigation, UNHCR pressure, and human rights monitoring to prevent states from abandoning procedural review. Theater ratio (0.32) captures growing performative compliance where formal hearings are maintained but meaningful access to counsel or evidence is eroded. Accessibility collapse (0.40) is moderate: alternatives such as summary rejection exist but face sustained legal challenge. Resistance (0.50) is substantial: states regularly resist through pushbacks, offshore processing, and procedural acceleration. The claimed type is tangled_rope because the constraint simultaneously coordinates (predictable procedural floor) and extracts (state sovereignty and resources).
 *
 * PERSPECTIVAL GAP:
 *   From the asylum seeker seat, the constraint is protective coordination; from the state seat, it is a sovereignty cost with compensating flexibility on substance; from the judicial seat, it is an interpretive mandate. The engine computes these divergences from structural data: identical power atoms (institutional) with different directionalities due to beneficiary versus payer roles and differentiated exit options (trapped versus constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers are structural beneficiaries (directionality near the beneficiary end) because the constraint subsidizes their access to fair procedure. States are genuinely dual-positioned: they benefit from substantive flexibility but pay through procedural compliance; the net structural position is mixed, but the victim declaration in base_properties drives the extraction signal for effective extraction computation. Domestic judiciary and UNHCR are near-analytical (exit: analytical) with directionality near symmetric. No directionality overrides are necessary because the structural derivation chain captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The procedural reading resists mandatrophy because its founding problemâpreventing arbitrary state rejection while preserving sovereigntyâremains contested but live. Were the problem dead, the constraint might decay into a piton (theatrical compliance without function). However, active resistance and ongoing enforcement demonstrate that the constraint still performs genuine coordination work, distinguishing it from a snare (which would lack the coordination function) and from a rope (which would lack the asymmetric extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    offshore_processing_procedural_integrity,
    'Can offshore processing ever satisfy the procedural integrity reading''s requirement for fair individualized assessment, or does geographic externalization inherently undermine process guarantees?',
    'Comparative case-law review of offshore regimes against procedural fairness criteria including access to counsel, independent review, and effective remedy.',
    'If offshore processing inherently undermines procedural integrity, the reading forecloses a major state strategy and raises effective extraction from states; if it can be compliant, the reading permits greater state flexibility and lowers extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_procedural_integrity, conceptual, 'Whether offshore processing is structurally compatible with procedural integrity.').

omega_variable(
    state_compliance_theater,
    'Is the procedural compliance observed in state asylum systems sincere operationalization of the reading, or increasingly performative theater masking substantive restriction?',
    'Empirical audit of asylum procedures measuring legal representation rates, interview duration, appeal success rates, and correlation between procedural form and protection outcomes.',
    'High theater would reclassify the constraint toward piton, indicating the coordination function has decayed into performative maintenance; low theater supports the current tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_compliance_theater, empirical, 'Whether state procedural compliance is genuine or performative.').

omega_variable(
    flexibility_as_hollowing,
    'Does the reading''s flexibility on substantive protection thresholds amount to a gradual hollowing of the Convention''s protective purpose, or a sustainable sovereignty-protection balance?',
    'Longitudinal outcome analysis comparing protection rates under this reading versus the expansive humanitarian reading, controlling for country of origin and claim type.',
    'If flexibility systematically hollows out protection, the beneficiary set shrinks and the constraint shifts toward extraction; if it sustains equilibrium, the current classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(flexibility_as_hollowing, conceptual, 'Whether substantive flexibility undermines the Convention''s protective core.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__procedural_integrity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(refu_tr_t6, refugee_convention_text__procedural_integrity_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(refu_tr_t12, refugee_convention_text__procedural_integrity_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(refu_tr_t18, refugee_convention_text__procedural_integrity_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(refu_tr_t24, refugee_convention_text__procedural_integrity_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(refu_tr_t30, refugee_convention_text__procedural_integrity_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(refu_be_t6, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(refu_be_t12, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(refu_be_t18, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement(refu_be_t24, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(refu_be_t30, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(refu_su_t6, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(refu_su_t12, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(refu_su_t18, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(refu_su_t24, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(refu_su_t30, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the refugee_convention_text kernel family. It decomposes the colloquial label 'Refugee Convention' into structurally distinct readings: procedural_integrity_reading (process-first), expansive_humanitarian_reading (substance-first), and restrictive_sovereignty_reading (sovereignty-first). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked because they compete to interpret the same fixed text and their operative effects are mutually exclusive in application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
