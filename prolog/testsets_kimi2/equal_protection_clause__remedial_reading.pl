% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Equal Protection Remedial Reading: Race-Conscious Remediation Mandate
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint instantiates the remedial_reading of the
 *   equal_protection_clause kernel: the interpretation that the Equal
 *   Protection Clause of the Fourteenth Amendment requires, rather than
 *   merely permits, race-conscious governmental action to remedy historical
 *   group subordination and achieve substantive equality. Under this reading,
 *   historically marginalized racial groups are beneficiaries of remediation
 *   policies, while individual members of non-preferred groups bear the
 *   extraction costs through reduced access to selective opportunities. The
 *   constraint is structurally temporaryâits justification is transitional,
 *   with a claimed sunset when remediation is completeâbut the endpoint is
 *   contested and potentially indeterminate. It is actively enforced by
 *   courts and administrative bodies, and faces sustained resistance from the
 *   colorblind reading and political majorities in several jurisdictions.
 *
 * KEY AGENTS:
 *   - historically_marginalized_groups: Primary beneficiary (organized/constrained) â receives race-conscious allocations of opportunity.
 *   - non_preferred_individuals: Primary target (moderate/identity_locked) â bears extraction through racially disfavored treatment; no exit from classification.
 *   - remedial_administrators: Agenda setter (institutional/constrained) â interprets and enforces the constitutional mandate through policy design.
 *   - colorblind_advocates: Excluded voice (organized/constrained) â opposes all racial classification but is overridden by this reading.
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â maps doctrinal conflict and empirical outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.75).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.68).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Remedial Reading: Race-Conscious Remediation Mandate").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '97de1a83-9f60-4101-923c-425272b204b7').
narrative_ontology:cs_kernel_codification('97de1a83-9f60-4101-923c-425272b204b7', fixed_text).
narrative_ontology:cs_authority_grounding('97de1a83-9f60-4101-923c-425272b204b7', lineage).
narrative_ontology:cs_interpretation_layer_present('97de1a83-9f60-4101-923c-425272b204b7').
narrative_ontology:cs_reading_relation('97de1a83-9f60-4101-923c-425272b204b7', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('97de1a83-9f60-4101-923c-425272b204b7', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('97de1a83-9f60-4101-923c-425272b204b7', foundational, group_remediation_constitutionally_required).
narrative_ontology:cs_axiom_status(group_remediation_constitutionally_required, holdable).
narrative_ontology:cs_axiom_grounding('97de1a83-9f60-4101-923c-425272b204b7', group_remediation_constitutionally_required, deontological).
narrative_ontology:cs_axiom('97de1a83-9f60-4101-923c-425272b204b7', foundational, substantive_equality_over_formal_equality).
narrative_ontology:cs_axiom_status(substantive_equality_over_formal_equality, holdable).
narrative_ontology:cs_axiom_grounding('97de1a83-9f60-4101-923c-425272b204b7', substantive_equality_over_formal_equality, deontological).
narrative_ontology:cs_reference_frame('97de1a83-9f60-4101-923c-425272b204b7', substantive_equality_framework).
narrative_ontology:cs_drift_state('97de1a83-9f60-4101-923c-425272b204b7', post_sffa_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('97de1a83-9f60-4101-923c-425272b204b7', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_marginalized_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, non_preferred_individuals).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, group_based_remedial_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive race-conscious allocations of educational, employment, and contracting opportunities intended to remedy cumulative historical disadvantage. Their access to selective institutions and economic positions is explicitly increased by the constraint's operation. Exit from the group category is not structurally available.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_marginalized_groups, beneficiary,
    organized, generational, constrained, national).

% Bear the extraction costs of race-conscious remediation, including reduced statistical access to selective admissions, scholarships, and contracts allocated by group membership. Their race is treated as a disfavored category; exit from the classification is impossible, making the cost inescapable for individuals in the disfavored groups.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, non_preferred_individuals, payer,
    moderate, biographical, identity_locked, national).

% State universities, agencies, and federal courts that design, administer, and enforce race-conscious remediation policies under the constitutional mandate. They balance compliance with judicial precedent against political and legal opposition, and their institutional authority derives from interpreting the Equal Protection Clause.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, remedial_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for the position that all governmental racial classifications are constitutionally forbidden. Under the remedial reading, their preferred framework is overridden and they are structurally excluded from policy design, though they mount legal challenges and ballot initiatives.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the doctrinal tension between remedial, diversity, and colorblind readings of the Equal Protection Clause. They track precedent, empirically evaluate remediation outcomes, and assess the logical coherence of competing constitutional frameworks.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, historically_marginalized_groups).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Remedies durable structural inequalities produced by centuries of state-sanctioned racial subordination through temporary, group-targeted allocation of opportunities, aiming to reach a baseline of substantive equality.
% TRANSFER_FUNCTION: Moves selective educational admissions, public contracts, and employment opportunities from non-preferred individuals to historically marginalized groups on the basis of race, as a corrective transfer.
% ABSENT_VOICES: Colorblind constitutionalists and individual-rights advocates who oppose all racial classification are structurally excluded from the policy design under this reading; their arguments are treated as legally irrelevant rather than merely politically defeated.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, universities and state agencies would revert to facially neutral criteria that reproduce existing disparities; the demographic composition of selective institutions would shift away from historically marginalized groups, and the legal justification for current race-conscious programs would collapse.
% FOUNDING_PROBLEM: Historical state-sanctioned racial subordination (slavery, Jim Crow, de jure segregation) created structural disadvantages that persist despite facially neutral law.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists outside the beneficiary groups attest to persistent wealth, education, and health disparities linked to historical discrimination; conservative legal scholars and some federal judges contest whether these disparities are traceable to state action or remediable through race-conscious means.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.75, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.75) is authored as high because the mandate structurally reallocates scarce opportunities (admissions, contracts) on the basis of race, imposing direct costs on a defined payer group. Suppression (0.68) reflects the legal foreclosure of colorblind alternatives: under this reading, facially neutral policies that fail to actively remediate are constitutionally insufficient, suppressing the colorblind alternative. Theater ratio (0.40) captures the increasing share of performative compliance (diversity rhetoric, symbolic programming) relative to genuine structural redistribution, especially as legal challenges intensify. Resistance (0.72) records robust political and legal opposition, including successful ballot initiatives banning race-conscious action and the Supreme Court's movement toward colorblindness. Accessibility collapse (0.55) is moderate: colorblind alternatives are legally disfavored but still politically and doctrinally live. The temporal series show extraction rising through the interval's middle decades as programs expanded, then slightly declining as judicial repudiation mounts, while suppression and theater rise to maintain the constraint against growing resistance.
 *
 * PERSPECTIVAL GAP:
 *   The historically marginalized groups experience the constraint as corrective justice and delayed equalization; the non-preferred individuals experience it as extraction legitimated by group membership; the remedial administrators experience it as a constitutional duty. The engine will compute divergent per-seat classifications from these structural positions: the payer seat should see high effective extraction amplified by identity-locked exit, while the beneficiary seat sees subsidy.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized groups are declared beneficiaries with constrained exit (low d, low or negative Ï). Non-preferred individuals are declared payers with identity-locked exit (high d, high Ï amplified by scope and immobility). Remedial administrators are agenda setters who enforce but do not personally collect; their d is intermediate. No override is needed because the structural derivation chain captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims scaffold status with a sunset clause (remediation complete). It resists classification as pure snare because of this internal temporariness and its genuine coordination function (remedying a documented collective-action problem of historical injustice). However, mandatrophy risk is present: if the remedial endpoint is indeterminate, the sunset is nominal and the constraint becomes a permanent extraction mechanism. The rising theater ratio in the measurement series suggests some drift toward performative maintenance, which would support a future piton or snare reclassification if the coordination function atrophies while the structure persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_endpoint_indeterminacy,
    'Can the remedial endpoint (substantive equality) be objectively determined such that the scaffold''s sunset clause is triggered?',
    'Longitudinal socio-economic parity metrics across domains where remediation was applied, or a judicial declaration that the founding problem is resolved.',
    'If no determinate endpoint exists, the scaffold''s temporariness is nominal and it functions as a permanent extraction mechanism, inviting reclassification toward tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_endpoint_indeterminacy, conceptual, 'Whether the sunset condition is practically determinable.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the remedial reading''s core premise logically foreclose the colorblind reading within a single constitutional framework?',
    'Supreme Court adoption of one reading to the exclusion of the other; doctrinal analysis of whether requiring race-consciousness is logically compatible with forbidding all racial classifications.',
    'If foreclosed, the kernel stabilizes under one reading and the constraint''s enforcement becomes more uniform; if the readings merely coexist, the constraint remains cyclically contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relation between remedial and colorblind readings.').

omega_variable(
    race_neutral_alternative_efficacy,
    'Can race-neutral alternatives (e.g., class-based preferences) achieve comparable remediation outcomes without racial classification?',
    'Empirical comparison of race-neutral and race-conscious policies in university admissions and contracting outcomes.',
    'If race-neutral means are comparably effective, the extraction cost borne by non-preferred individuals under this reading is unnecessary, undermining the remedial necessity claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_neutral_alternative_efficacy, empirical, 'Whether race-consciousness is empirically necessary for remediation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__remedial_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(equa_tr_t10, equal_protection_clause__remedial_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(equa_tr_t20, equal_protection_clause__remedial_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(equa_tr_t30, equal_protection_clause__remedial_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(equa_tr_t40, equal_protection_clause__remedial_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(equa_tr_t50, equal_protection_clause__remedial_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__remedial_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(equa_be_t10, equal_protection_clause__remedial_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(equa_be_t20, equal_protection_clause__remedial_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(equa_be_t30, equal_protection_clause__remedial_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(equa_be_t40, equal_protection_clause__remedial_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(equa_be_t50, equal_protection_clause__remedial_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__remedial_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(equa_su_t10, equal_protection_clause__remedial_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(equa_su_t20, equal_protection_clause__remedial_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(equa_su_t30, equal_protection_clause__remedial_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(equa_su_t40, equal_protection_clause__remedial_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(equa_su_t50, equal_protection_clause__remedial_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the equal_protection_clause constraint family. The kernel (the Equal Protection Clause text) decomposes into structurally distinct readings due to divergent normative axioms and empirical premises. Each reading has a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
