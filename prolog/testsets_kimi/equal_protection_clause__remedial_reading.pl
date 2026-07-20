% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Remedial Reading â Race-Conscious Remediation Mandate
 *   domain: constitutional law / political philosophy / education policy
 *
 * SUMMARY:
 *   This constraint story instantiates the remedial reading of the Equal
 *   Protection Clause: the claim that the Clause requires race-conscious
 *   governmental remediation of historical group subordination to achieve
 *   substantive equality. Under this reading, the Constitution mandates
 *   temporary, narrowly tailored preferences for historically marginalized
 *   racial groups and imposes corresponding costs on non-preferred
 *   individuals. The constraint is claimed as a scaffold (transitional
 *   remedy) but carries high extractiveness and significant resistance. It is
 *   one reading of the equal_protection_clause kernel; siblings are the
 *   colorblind reading and the diversity reading.
 *
 * KEY AGENTS:
 *   - historically_marginalized_groups (beneficiary / identity-locked)
 *   - non_preferred_individuals (payer / identity-locked)
 *   - public_institutions (agenda_setter / administrator)
 *   - federal_judiciary (agenda_setter / interpreter)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.78).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.65).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Remedial Reading â Race-Conscious Remediation Mandate").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional law / political philosophy / education policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, 'dec9c432-dda9-40eb-954e-8e0a7b8c1bdd').
narrative_ontology:cs_kernel_codification('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', fixed_text).
narrative_ontology:cs_authority_grounding('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', lineage).
narrative_ontology:cs_interpretation_layer_present('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd').
narrative_ontology:cs_reading_relation('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', foundational, substantive_equality_mandate).
narrative_ontology:cs_axiom_status(substantive_equality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', substantive_equality_mandate, deontological).
narrative_ontology:cs_axiom('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', foundational, group_remedial_entitlement).
narrative_ontology:cs_axiom_status(group_remedial_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', group_remedial_entitlement, deontological).
narrative_ontology:cs_reference_frame('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', remedial_constitutional_order).
narrative_ontology:cs_drift_state('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('dec9c432-dda9-40eb-954e-8e0a7b8c1bdd', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_marginalized_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, non_preferred_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive race-conscious remedial preferences in education, employment, and public contracting intended to overcome historical group subordination. Their access to these benefits is tied to group membership; exit from the identity category is not socially or legally available, making the constraint's benefits identity-locked.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_marginalized_groups, beneficiary,
    organized, generational, identity_locked, national).

% Bear the costs of race-conscious remedial programs through reduced access to competitive admissions slots, employment, and contracts. Their racial classification is treated as a liability within the remedial framework, and they cannot exit the constraint by changing identity.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, non_preferred_individuals, payer,
    moderate, biographical, identity_locked, national).

% Administer race-conscious admissions, hiring, and contracting programs to satisfy the remedial constitutional mandate. They design narrowly tailored policies, collect compliance data, and defend against litigation from both beneficiary and payer sides.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, public_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the Equal Protection Clause to require, permit, or forbid race-conscious remediation. Its precedents directly expand or contract the constraint's scope, and it sits outside the benefit-cost structure as the authoritative interpreter.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, historically_marginalized_groups).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the distribution of public educational and economic opportunities to overcome accumulated group disadvantage and move toward a substantively equal society.
% TRANSFER_FUNCTION: Moves competitive admissions slots, public contracts, and employment opportunities from individuals in non-preferred racial groups to historically marginalized racial groups, administered by public institutions under judicial oversight.
% ABSENT_VOICES: Individuals who would have gained access under a colorblind regime but are denied under the remedial framework are present in litigation but structurally outvoted in constitutional interpretation; colorblind constitutional theorists are heard in dissent but rarely in majority opinions upholding remedial mandates.
% DISAPPEARANCE_RATIONALE: If the remedial mandate vanished overnight, public institutions would dismantle race-conscious admissions and contracting programs, the distribution of opportunities would shift toward formal neutrality, and historically marginalized groups would lose a structural mechanism for addressing disparities â the social arrangement of opportunity would reorganize around colorblind criteria.
% FOUNDING_PROBLEM: Persistent racial subordination and formal equality's failure to produce substantive equality in the wake of slavery and segregation.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations and some legal historians attest the problem remains live. Colorblind advocates and empirical skeptics attest the problem is either solved or not amenable to race-conscious remedy; corroboration from neutral longitudinal audit studies is mixed and politically contested. No fully external corroborator exists â the dispute over the founding problem's status is itself the axis of political conflict.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint structurally transfers competitive opportunities from one racial category to another through state action. Suppression (0.65) reflects the active judicial and institutional enforcement needed to maintain race-conscious allocations against colorblind alternatives. Theater ratio (0.40) captures the growth of performative compliance (diversity statements, cosmetic outreach) that substitutes for genuine remediation. Resistance is high (0.80) due to persistent legal and political opposition. Accessibility collapse is moderate (0.50) because the colorblind alternative remains legally and politically visible, though the remedial framework suppresses it within covered institutions.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat, the constraint is corrective justice restoring stolen opportunity; from the payer seat, it is state-sponsored racial discrimination. The agenda-setter seats (institutions and judiciary) experience it as a compliance and interpretation burden. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized groups sit at low directionality (beneficiaries of the transfer). Non-preferred individuals sit at high directionality (targets of the transfer). Both are identity_locked, which amplifies effective extraction for payers and dampens it for beneficiaries only modestly because the benefit is tied to the same locked identity. Public institutions and the federal judiciary sit near symmetric or analytical â they administer and interpret the transfer without being its primary beneficiaries or victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling this as permanent extraction (snare) or pure coordination (rope). The sunset clause is structurally required: the reading's own justification is transitional remediation. If the sunset never triggers, the classification drifts toward tangled_rope or piton. The temporal measurements show extraction accumulation peaking alongside a recent decline in suppression requirement, signaling potential mandatrophy if the sunset clause is not honored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_sunset_uncertainty,
    'Will the sunset clause ever trigger, or has the remedial scaffold become a permanent transfer mechanism?',
    'Longitudinal tracking of program termination dates against socioeconomic parity metrics; if programs persist without narrowing disparity gaps, the sunset is theoretical.',
    'If permanent, the constraint reclassifies from scaffold to tangled_rope or snare; the temporary justification collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_uncertainty, empirical, 'Whether the scaffold''s sunset is operational or nominal.').

omega_variable(
    colorblind_foreclosure_validity,
    'Does the remedial reading''s core premise logically foreclose the colorblind reading entirely, or can both coexist in a federalist or sectoral split?',
    'Jurisdictional fragmentation analysis: if some government entities adopt remedial programs while others adopt colorblind regimes without constitutional crisis, the foreclosure is partial.',
    'If foreclosure is partial, the network edge to colorblind_reading weakens from forecloses to influences or coexists_with, altering contamination propagation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_foreclosure_validity, conceptual, 'Logical exclusivity of remedial and colorblind constitutional readings.').

omega_variable(
    remedial_empirical_premise,
    'Do race-conscious remedial programs actually produce the substantive equality they promise, or do they generate theater without closing disparity gaps?',
    'Controlled longitudinal studies of cohort outcomes in education and employment across race-conscious and race-neutral regimes.',
    'If the empirical premise fails, the coordination story weakens and extraction dominates, pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_empirical_premise, empirical, 'Empirical efficacy of race-conscious remediation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epr_tr_t0, equal_protection_clause__remedial_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(epr_tr_t10, equal_protection_clause__remedial_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(epr_tr_t20, equal_protection_clause__remedial_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(epr_tr_t30, equal_protection_clause__remedial_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(epr_tr_t40, equal_protection_clause__remedial_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(epr_tr_t50, equal_protection_clause__remedial_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(epr_tr_t60, equal_protection_clause__remedial_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(epr_be_t0, equal_protection_clause__remedial_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(epr_be_t10, equal_protection_clause__remedial_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(epr_be_t20, equal_protection_clause__remedial_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(epr_be_t30, equal_protection_clause__remedial_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(epr_be_t40, equal_protection_clause__remedial_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(epr_be_t50, equal_protection_clause__remedial_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement(epr_be_t60, equal_protection_clause__remedial_reading, base_extractiveness, 60, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(epr_su_t0, equal_protection_clause__remedial_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(epr_su_t10, equal_protection_clause__remedial_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(epr_su_t20, equal_protection_clause__remedial_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(epr_su_t30, equal_protection_clause__remedial_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(epr_su_t40, equal_protection_clause__remedial_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(epr_su_t50, equal_protection_clause__remedial_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(epr_su_t60, equal_protection_clause__remedial_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equal_protection_clause kernel. The remedial reading requires race-conscious remediation; the colorblind reading forbids all racial classifications; the diversity reading permits race-consciousness for educational diversity. Each reading instantiates a different constraint with distinct beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
