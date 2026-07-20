% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportional Balancing Standard
 *   domain: international_law/security/human_rights
 *
 * SUMMARY:
 *   This constraint story captures the proportionality_balancing reading of
 *   the humane_treatment_standard kernel instantiated by Common Article 3 of
 *   the Geneva Conventions. Under this reading, CA3 does not absolutely
 *   prohibit harsh treatment (the absolute_prohibition sibling reading) nor
 *   does it defer entirely to state security claims (the contextual_necessity
 *   sibling reading); rather, it installs courts as gatekeepers who balance
 *   detainee dignity against security needs case-by-case. The constraint
 *   coordinates international humanitarian expectations by providing a
 *   workable legal standard for non-international armed conflicts, but it
 *   asymmetrically extracts from detained persons by legitimizing suffering
 *   when judicial balancing favors security, and from security interrogators
 *   through procedural burdens. It is actively enforced through domestic and
 *   international judicial review. The structural claim is tangled_rope:
 *   genuine coordination function layered with extraction. The metrics are
 *   authored independently and show moderate extractiveness rising over time
 *   as proportionality jurisprudence matured, moderate suppression of
 *   absolute-prohibition alternatives, and growing theater as procedural
 *   compliance ritualized.
 *
 * KEY AGENTS:
 *   - detaining_states: Primary beneficiary (institutional/constrained) â collect legal flexibility and legitimacy
 *   - adjudicating_courts: Agenda-setter (institutional/constrained) â capture gatekeeping authority through case-by-case enforcement
 *   - detained_persons: Primary target (powerless/trapped) â bear physical and psychological costs when balance tips to security
 *   - security_interrogators: Secondary target (moderate/constrained) â bear procedural and operational compliance burdens
 *   - human_rights_advocates: Excluded voice (organized/constrained) â structurally marginalized by proportionality framework
 *   - legal_scholars: Analytical observer (analytical/analytical) â document interpretive drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.65).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.45).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.65).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportional Balancing Standard").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_law/security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '5d8757eb-b339-4ac3-b66d-8494e1274d3f').
narrative_ontology:cs_kernel_codification('5d8757eb-b339-4ac3-b66d-8494e1274d3f', formalized).
narrative_ontology:cs_authority_grounding('5d8757eb-b339-4ac3-b66d-8494e1274d3f', lineage).
narrative_ontology:cs_interpretation_layer_present('5d8757eb-b339-4ac3-b66d-8494e1274d3f').
narrative_ontology:cs_reading_relation('5d8757eb-b339-4ac3-b66d-8494e1274d3f', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('5d8757eb-b339-4ac3-b66d-8494e1274d3f', humane_treatment_standard__contextual_necessity, influences).
narrative_ontology:cs_axiom('5d8757eb-b339-4ac3-b66d-8494e1274d3f', foundational, proportionality_governs_humanitarian_minimum).
narrative_ontology:cs_axiom_status(proportionality_governs_humanitarian_minimum, holdable).
narrative_ontology:cs_axiom_grounding('5d8757eb-b339-4ac3-b66d-8494e1274d3f', proportionality_governs_humanitarian_minimum, conventional).
narrative_ontology:cs_reference_frame('5d8757eb-b339-4ac3-b66d-8494e1274d3f', balanced_humanitarian_minimum).
narrative_ontology:cs_drift_state('5d8757eb-b339-4ac3-b66d-8494e1274d3f', post_9_11_security_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d8757eb-b339-4ac3-b66d-8494e1274d3f', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detaining_states).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, adjudicating_courts).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detained_persons).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, security_interrogators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a legally recognized framework that permits limited interrogation and security practices under judicial oversight, avoiding both the political cost of absolute prohibition and the pariah status of unlimited brutality.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detaining_states, beneficiary,
    institutional, generational, constrained, national).

% Serve as gatekeepers between detainee dignity claims and state security assertions, developing balancing tests and procedural safeguards that determine permissible treatment on a case-by-case basis.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, adjudicating_courts, agenda_setter,
    institutional, generational, constrained, national).

% Bear the bodily and psychological costs when judicial balancing finds security imperatives sufficient to override dignity interests; receive partial protection compared to absolute prohibition but remain vulnerable to state-defined necessity.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detained_persons, payer,
    powerless, immediate, trapped, local).

% Operate under procedural constraints and judicial review requirements that limit their discretionary methods; must document and justify treatment choices against proportionality standards, bearing the operational burden of compliance.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, security_interrogators, payer,
    moderate, immediate, constrained, national).

% Advance absolute prohibition positions that are structurally excluded from proportionality-balancing proceedings; their voices are rendered irrelevant when courts treat dignity as a variable in a cost-benefit analysis rather than an inviolable floor.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the divergence between the treaty text's plain language and judicial innovations in proportionality, documenting how the standard drifts across jurisdictions without formal amendment.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, detaining_states).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared legal standard for treating detainees in non-international armed conflicts, enabling states to maintain international legitimacy while conducting security operations through a judicially overseen balancing framework.
% TRANSFER_FUNCTION: Moves discretionary authority over treatment permissibility from military and political commanders to judicial gatekeepers, and moves the risk of harsh treatment from states to detainees on a case-by-case basis.
% ABSENT_VOICES: Advocates of absolute prohibition and detainee communities themselves are largely absent from the judicial balancing process; their exclusion is structural because the proportionality framework treats dignity as negotiable rather than inviolable.
% DISAPPEARANCE_RATIONALE: If the proportional balancing standard disappeared, detaining states would lose their legal framework for legitimate interrogation, courts would no longer serve as gatekeepers between security and dignity, and detainees would face either unchecked state violence or unworkable absolute standards â the current equilibrium depends on this specific constraint.
% FOUNDING_PROBLEM: How to regulate treatment of detainees in internal conflicts where states reject full Geneva Convention applicability but international legitimacy still demands some humanitarian limit.
% FOUNDING_PROBLEM_CORROBORATION: International Court of Justice and regional human rights courts attest the need for balancing mechanisms from outside the direct state-beneficiary circle; however, humanitarian NGOs contest that the founding problem required proportionality rather than absolute minimum standards.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects that the standard permits significant suffering to be legitimized through judicial balancing; suppression (0.45) captures the structural marginalization of absolute-prohibition alternatives without overt coercion; theater_ratio (0.35) marks the ritualization of procedural safeguards that often ratify rather than prevent harm. Accessibility_collapse is high (0.70) because legal discourse increasingly treats proportionality as the only viable framework, while resistance (0.50) reflects sustained contestation from human rights actors and some states. The temporal series track one shared grid from the standard's crystallization through post-9/11 jurisprudential expansion.
 *
 * PERSPECTIVAL GAP:
 *   Detaining states and courts experience the constraint as a legitimate coordination mechanism that preserves international standing and operational capacity. Detained persons experience the same structure as an extractive arrangement in which their dignity is treated as negotiable. Security interrogators experience procedural burden without commensurate clarity. The engine computes this divergence from the structural data; the authored claim does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Detaining states and adjudicating courts sit toward the beneficiary end: the former gain legal cover and flexibility, the latter gain gatekeeping authority and institutional relevance. Detained persons sit at the full-target end (trapped, powerless, bodily costs). Security interrogators sit toward the target end but less extreme (constrained exit, procedural costs, moderate power). Human rights advocates are excluded rather than directly targeted; their exclusion is the mechanism by which the constraint suppresses alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than rope prevents misidentifying the procedural safeguards as purely protective; the detainee costs are structurally necessary to the coordination, not accidental. Classifying it as tangled_rope rather than snare prevents ignoring the genuine coordination function it provides to the international legal order and the real constraint it places on unlimited state violence. The absence of a concentrated capturer of monetary rents further distinguishes it from snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the proportional balancing reading represent the authentic kernel of Common Article 3, or has it displaced an originally absolute prohibition through interpretive drift?',
    'Historical-interpretive analysis of the 1949 negotiating record and subsequent state practice to determine whether the original kernel encoded proportionality or minimum absolute standards.',
    'If the kernel was originally absolute, this reading is a false summit (tangled_rope masquerading as evolved doctrine) and should be reclassified toward snare; if originally proportional, the reading is a more faithful rope-tending reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether proportional balancing is authentic kernel or interpretive drift').

omega_variable(
    proportionality_as_legitimation,
    'Does the proportionality standard function as genuine coordination between humanitarian and security imperatives, or as a legitimization mechanism for state violence that would otherwise be prohibited?',
    'Cross-jurisdictional outcome analysis: compare treatment permissibility rates and judicial outcomes in jurisdictions applying strict balancing versus those under absolute prohibition regimes.',
    'If outcomes show systematic tilt toward security claims, the coordination story is cover and extraction dominates; if outcomes show genuine equipoise, the tangled_rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_legitimation, empirical, 'Whether proportionality balances genuinely or legitimizes state violence').

omega_variable(
    judicial_gatekeeper_bias,
    'Are adjudicating courts structurally independent balancers, or are they captured by state security framings in proportionality proceedings?',
    'Quantitative analysis of judicial outcomes in CA3 cases: rates of state success, deference doctrines used, and evidentiary standards applied to security claims versus dignity claims.',
    'If courts show systematic deference to state security assertions, the beneficiary structure is misdeclared: courts are not neutral agenda_setters but captured beneficiaries, and directionality shifts toward detainees as sole payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_gatekeeper_bias, empirical, 'Structural bias in judicial balancing of security and dignity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.1).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__proportionality_balancing, theater_ratio, 10, 0.12).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__proportionality_balancing, theater_ratio, 20, 0.18).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__proportionality_balancing, theater_ratio, 30, 0.22).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__proportionality_balancing, theater_ratio, 40, 0.28).
narrative_ontology:measurement(huma_tr_t50, humane_treatment_standard__proportionality_balancing, theater_ratio, 50, 0.32).
narrative_ontology:measurement(huma_tr_t60, humane_treatment_standard__proportionality_balancing, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__proportionality_balancing, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__proportionality_balancing, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__proportionality_balancing, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(huma_be_t50, humane_treatment_standard__proportionality_balancing, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(huma_be_t60, humane_treatment_standard__proportionality_balancing, base_extractiveness, 60, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__proportionality_balancing, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__proportionality_balancing, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__proportionality_balancing, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(huma_su_t50, humane_treatment_standard__proportionality_balancing, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(huma_su_t60, humane_treatment_standard__proportionality_balancing, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the humane_treatment_standard kernel, which also emits absolute_prohibition and contextual_necessity readings. Each reading has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
