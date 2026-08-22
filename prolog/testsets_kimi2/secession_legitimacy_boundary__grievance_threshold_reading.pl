% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Secession Legitimacy by Grievance Threshold (Remedial Secession Doctrine)
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint instantiates the grievance-threshold reading of the
 *   secession legitimacy boundary kernel. It holds that secession from a
 *   federal state becomes legitimate only when the center's actions cross an
 *   objective threshold of structural injustice, regardless of what the
 *   constitutional text says. The doctrine is invoked in international law
 *   and political discourse to adjudicate center-periphery conflicts. It
 *   purports to coordinate by preventing both frivolous fragmentation and
 *   permanent subordination, but its operation concentrates interpretive
 *   authority in the center and international institutions that the periphery
 *   does not control, generating asymmetric extraction. The claim/metric
 *   independence is maintained: the reading is claimed as tangled_rope
 *   (coordinating function plus asymmetric extraction) while metrics describe
 *   a moderately extractive, actively enforced arrangement.
 *
 * KEY AGENTS:
 *   - federal_center: Primary beneficiary (institutional/arbitrage) â collects territorial integrity and resource control
 *   - subordinated_minority_regions: Primary target (moderate/identity_locked) â bears extraction through continued subordination and high burden of proof
 *   - international_judicial_bodies: Agenda setter (institutional/analytical) â interprets threshold but does not bear costs
 *   - secessionist_political_leaders: Secondary target (moderate/constrained) â bears mobilization and evidentiary costs
 *   - indigenous_treaty_holders: Excluded payer (organized/constrained) â bears costs of treaty override, absent from threshold discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.64).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.72).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Secession Legitimacy by Grievance Threshold (Remedial Secession Doctrine)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, 'ed179b73-362b-482b-89ad-9d3a1c4c7506').
narrative_ontology:cs_kernel_codification('ed179b73-362b-482b-89ad-9d3a1c4c7506', formalized).
narrative_ontology:cs_authority_grounding('ed179b73-362b-482b-89ad-9d3a1c4c7506', expertise).
narrative_ontology:cs_interpretation_layer_present('ed179b73-362b-482b-89ad-9d3a1c4c7506').
narrative_ontology:cs_reading_relation('ed179b73-362b-482b-89ad-9d3a1c4c7506', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed179b73-362b-482b-89ad-9d3a1c4c7506', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed179b73-362b-482b-89ad-9d3a1c4c7506', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('ed179b73-362b-482b-89ad-9d3a1c4c7506', foundational, remedial_secession_on_structural_injustice).
narrative_ontology:cs_axiom_status(remedial_secession_on_structural_injustice, holdable).
narrative_ontology:cs_axiom_grounding('ed179b73-362b-482b-89ad-9d3a1c4c7506', remedial_secession_on_structural_injustice, deontological).
narrative_ontology:cs_axiom('ed179b73-362b-482b-89ad-9d3a1c4c7506', foundational, objective_burden_of_proof_required).
narrative_ontology:cs_axiom_status(objective_burden_of_proof_required, holdable).
narrative_ontology:cs_axiom_grounding('ed179b73-362b-482b-89ad-9d3a1c4c7506', objective_burden_of_proof_required, conventional).
narrative_ontology:cs_reference_frame('ed179b73-362b-482b-89ad-9d3a1c4c7506', remedial_secession_doctrine).
narrative_ontology:cs_drift_state('ed179b73-362b-482b-89ad-9d3a1c4c7506', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed179b73-362b-482b-89ad-9d3a1c4c7506', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, federal_center).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, subordinated_minority_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, secessionist_political_leaders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_treaty_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains territorial integrity, tax base, and resource control over subordinated regions. Benefits from the grievance-threshold doctrine because the burden of proof and interpretive control make successful secession rare. Can interpret 'structural injustice' narrowly or lobby international bodies to do so.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_center, beneficiary,
    institutional, generational, arbitrage, national).

% Administer the threshold by interpreting what counts as 'structural injustice' in international law. Set evidentiary standards and recognition criteria. Their authority depends on appearing neutral while their interpretations often reflect great-power consensus.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_judicial_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% Bear the costs of continued political and economic subordination within the federation. Must meet a high objective burden of proof to legitimate secession, a burden set and interpreted by institutions they do not control. Their identity is locked to the territory, making exit through assimilation or migration costly.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, subordinated_minority_regions, payer,
    moderate, generational, identity_locked, regional).

% Must mobilize resources to demonstrate that the federal center has crossed the injustice threshold. Bear costs of failed bids for independence, including legal penalties and political repression. Their strategic options are constrained by the need for international recognition, which depends on satisfying the threshold.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, secessionist_political_leaders, payer,
    moderate, biographical, constrained, regional).

% Hold pre-federal treaty rights that the grievance-threshold framework does not center. Their consent is treated as secondary to a universalist injustice standard. Would object that no secession affecting treaty lands is legitimate without their consent, but this voice is marginalized in the threshold-based discourse.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_treaty_holders, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_treaty_holders, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, federal_center).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a conditional normative rule for legitimate political breakup, preventing arbitrary secession while permitting exit under demonstrated oppression, thereby stabilizing expectations between center and periphery.
% TRANSFER_FUNCTION: Transfers interpretive authority over legitimacy from secessionist regions to international courts and federal institutions; transfers compliance costs (burden of proof, political mobilization) from the center to the periphery.
% ABSENT_VOICES: Indigenous treaty holders whose consent-based sovereignty claims are sidelined by the grievance-threshold logic; popular-sovereignty advocates who hold that majority will alone should suffice; and constitutional absolutists who reject any non-textual exit route.
% DISAPPEARANCE_RATIONALE: Without the grievance-threshold standard, secessionist movements would lose a key legitimating framework that currently constrains both their own actions and federal responses; international recognition practices would revert to constitutional or popular-sovereignty defaults, rearranging the strategic environment for center-periphery conflicts.
% FOUNDING_PROBLEM: How to reconcile territorial integrity with self-determination in plurinational states, preventing both perpetual fragmentation and permanent subordination.
% FOUNDING_PROBLEM_CORROBORATION: Federal governments and UN bodies attest the problem of balancing integrity and self-determination is live. However, no external corroboration exists for the grievance-threshold as the specific solution â secessionist movements and indigenous treaty holders outside the benefiting parties argue the threshold is a legitimizing device for empire preservation, not a genuine remedy.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.64, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64) reflects the decoupling of threshold application from the periphery's self-determination needs: the burden of proof and interpretive control allow the center to convert coordination into sustained subordination. Suppression (0.72) is high because the constraint actively delegitimizes unilateral and popular-sovereignty alternatives. Theater_ratio (0.47) indicates that nearly half of threshold adjudication activity serves legitimation rather than genuine justice-testing â a ratchet from the early post-WWII period. Accessibility_collapse (0.68) captures how alternative legitimacy frameworks (referendum-only, treaty-consent) have been marginalized once the grievance-threshold became the dominant international standard. Resistance (0.58) tracks persistent secessionist and indigenous challenges.
 *
 * PERSPECTIVAL GAP:
 *   The federal_center and international_judicial_bodies compute toward coordination-with-extraction: they see a necessary stabilizing rule. The subordinated_minority_regions, secessionist_political_leaders, and indigenous_treaty_holders compute toward extraction: they experience a legitimacy bar they did not set and cannot control. The engine derives this divergence from the same structural data â the asymmetry of exit options (arbitrage vs identity_locked/constrained) and the directional flow of interpretive authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal_center is the structural beneficiary (low d): it captures territorial integrity, tax base, and resource rents while externalizing the legitimacy costs of suppression. Subordinated_minority_regions and secessionist_political_leaders are structural targets (high d): they pay in foregone self-determination, mobilization costs, and evidentiary burdens. Indigenous_treaty_holders are also targets (high d) via exclusion from the legitimacy framework. International_judicial_bodies sit near symmetric (moderate d): they neither pay nor collect material rents, but they accrue institutional authority from administering the threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   The grievance-threshold reading was founded to solve the coordination problem between territorial integrity and self-determination. Its coordination function is genuine: it prevents both frivolous fragmentation and absolutist suppression. However, mandatrophy is not resolved because the arrangement persists beyond its original decolonization context into intra-state conflicts where it often functions as empire preservation. The rising theater_ratio (0.20 to 0.47) signals growing performative maintenance â threshold inquiries increasingly serve to legitimize predetermined non-recognition. Classifying as tangled_rope (rather than rope or snare) captures this hybrid: a real coordination mechanism fused with asymmetric extraction through interpretive control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_objectivity_ambiguity,
    'Is the threshold of structural injustice an objective legal standard or an inherently political judgment masquerading as neutral expertise?',
    'Comparative case analysis across ICJ opinions, UN recognition practice, and regional human rights bodies to measure inter-rater reliability of threshold application.',
    'If the threshold is irreducibly political, the constraint''s extraction is higher than its coordination function suggests, moving it toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_objectivity_ambiguity, conceptual, 'Whether the injustice threshold is objective or political.').

omega_variable(
    burden_of_proof_asymmetry,
    'Does the objective burden of proof requirement structurally advantage the federal center over the secessionist periphery?',
    'Quantitative analysis of secession claims that succeeded versus failed threshold tests; measurement of evidentiary resources available to center versus periphery.',
    'If the burden is systematically asymmetric, the victim set is larger than the threshold-crossing condition implies, and effective extraction is amplified for subordinated regions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_of_proof_asymmetry, empirical, 'Whether evidentiary burden asymmetry favors the center.').

omega_variable(
    treaty_consent_sidelining,
    'Does the grievance-threshold reading foreclose or merely sideline indigenous treaty-based sovereignty claims?',
    'Jurisprudential mapping of cases where treaty consent and grievance thresholds conflict; indigenous legal scholarship and tribunal decisions.',
    'If foreclosed, the constraint extracts from treaty holders by overriding their consent framework; if sidelined, it coexists with treaty primacy as a competing standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_consent_sidelining, conceptual, 'Whether treaty-based claims are foreclosed or sidelined.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 50, 0.47).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 50, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is the grievance-threshold reading of the secession_legitimacy_boundary kernel. The kernel decomposes into four distinct constraints because 'secession legitimacy' conflates constitutional, popular, grievance-based, and treaty-based standards with different epsilon profiles and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
