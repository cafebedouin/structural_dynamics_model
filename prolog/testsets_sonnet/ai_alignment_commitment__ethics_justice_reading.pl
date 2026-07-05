% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: Alignment-as-Present-Harm-and-Bias-Prevention (Ethics/Justice Reading)
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This story instantiates the ethics/justice reading of the contested 'AI
 *   alignment' kernel: alignment is defined as preventing reproduction of
 *   social bias and present-day, demonstrable harm — discriminatory lending,
 *   hiring, policing, and content-moderation outcomes affecting specific
 *   marginalized populations right now. This is deliberately NOT the
 *   safety/control reading (catastrophic loss-of-control risk) and NOT the
 *   integrated reading (both simultaneously). Under this reading, the
 *   coordination function is real (shared audit standards let harms be
 *   detected and sometimes remediated that would otherwise be invisible), but
 *   a genuine extraction dynamic has grown alongside it: a fairness-research
 *   and compliance-vendor apparatus now captures funding, attention, and
 *   regulatory legitimacy in ways only loosely coupled to whether affected
 *   communities' material conditions actually improve, and this apparatus
 *   draws resources and legitimacy away from long-term safety research framed
 *   as a competing claim on the same 'alignment' budget.
 *
 * KEY AGENTS:
 *   - algorithmic_fairness_research_field: institutional beneficiary and agenda-setter who defines operational fairness metrics
 *   - marginalized_communities_subject_to_biased_systems: powerless, trapped payers who are the reading's named referent but rarely control remediation design
 *   - compliance_and_audit_vendors: powerful beneficiaries profiting from codification of the reading into procurement and law
 *   - long_term_safety_researchers: excluded from this reading's operational scope, competing for the same 'alignment' resource pool
 *   - ai_developers_and_labs: institutional agenda-setters who must operationalize whichever reading has regulatory teeth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.52).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.44).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "Alignment-as-Present-Harm-and-Bias-Prevention (Ethics/Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, 'e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7').
narrative_ontology:cs_kernel_codification('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', distributed).
narrative_ontology:cs_authority_grounding('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', distributed).
narrative_ontology:cs_reading_relation('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', foundational, present_demonstrated_harm_has_priority_claim).
narrative_ontology:cs_axiom_status(present_demonstrated_harm_has_priority_claim, holdable).
narrative_ontology:cs_axiom_grounding('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', present_demonstrated_harm_has_priority_claim, deontological).
narrative_ontology:cs_axiom('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', foundational, distributive_justice_to_marginalized_populations_is_the_alignment_target).
narrative_ontology:cs_axiom_status(distributive_justice_to_marginalized_populations_is_the_alignment_target, holdable).
narrative_ontology:cs_axiom_grounding('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', distributive_justice_to_marginalized_populations_is_the_alignment_target, deontological).
narrative_ontology:cs_reference_frame('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', documented_discrimination_case_baseline).
narrative_ontology:cs_drift_state('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', post_regulatory_compliance_industry_formation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9cc5761-9d7b-47d9-b0a9-5c8bc54ecdc7', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, algorithmic_fairness_research_field).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_ethics_practitioners).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, compliance_and_audit_vendors).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_biased_systems).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, gig_workers_displaced_by_automation).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, content_moderators_and_data_labelers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_biased_systems).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_developers_and_labs).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, algorithmic_bias_is_a_present_material_harm).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, distributive_justice_is_a_legitimate_alignment_target).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic and industry researchers who built careers, conferences, and funding streams around fairness metrics, bias audits, and disparate-impact testing. They set the operational definition of 'alignment' used in most present-day corporate and regulatory compliance work, and their labor is the primary channel through which the ethics/justice reading gets implemented.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, algorithmic_fairness_research_field, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, algorithmic_fairness_research_field, agenda_setter).

% Trust-and-safety staff, policy leads, and ethics-board members hired specifically to operationalize present-harm mitigation inside AI companies. Their institutional standing and budgets depend on the ethics/justice framing remaining the dominant definition of alignment work; they can move between firms but the role only exists under this reading.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_ethics_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Third-party firms selling bias-audit tooling, fairness certification, and regulatory-compliance services keyed to the present-harm definition of alignment. They profit from the reading being codified into law and procurement requirements regardless of whether the underlying harms are actually reduced.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, compliance_and_audit_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% People denied loans, flagged by predictive policing, screened out by hiring algorithms, or misclassified by content-moderation systems today. They are the named referent of this reading's justification, but have little control over how audits are designed, what counts as 'fixed,' or whether remediation follows detection. Exit from the systems that harm them is often not possible (housing, credit, employment platforms are structurally unavoidable).
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_biased_systems, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_biased_systems, beneficiary).

% Workers whose labor is displaced or algorithmically managed by the same systems being audited for bias; present-harm framing captures discriminatory treatment but often does not capture displacement itself as a harm category, leaving this group under-covered even by the reading built to center current harm.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, gig_workers_displaced_by_automation, payer,
    powerless, immediate, trapped, national).

% Low-wage global workers whose labor produces the training and evaluation data underlying fairness audits themselves; they bear psychological and economic costs of the alignment apparatus's data pipeline while rarely appearing as a named beneficiary group in ethics/justice remediation efforts.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, content_moderators_and_data_labelers, payer,
    powerless, immediate, constrained, global).

% Researchers focused on catastrophic and loss-of-control risk who argue that funding, talent, and regulatory attention captured by the present-harm framing come partly at the expense of long-horizon safety work. They are structurally present in the broader alignment debate but excluded from this reading's own operational definition of what counts as alignment work worth resourcing.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, excluded,
    organized, civilizational, constrained, global).

% Companies that must operationalize whichever reading regulators and public pressure favor. Under the ethics/justice reading they fund bias audits, fairness teams, and compliance tooling; they can lobby to shape the reading's scope but cannot exit the requirement to demonstrate present-harm mitigation once it is embedded in law or procurement.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_developers_and_labs, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, ai_developers_and_labs, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates attention, funding, and regulatory requirements around detecting and remediating demonstrable, present-tense harms — biased lending, hiring, policing, and content systems — that would otherwise go unmeasured and unaddressed absent a shared definitional and audit infrastructure.
% TRANSFER_FUNCTION: Moves research funding, regulatory attention, and compliance budgets toward fairness-audit infrastructure and the practitioners who build it; moves reputational and legal risk away from developers who can point to audit compliance; the material benefit of harm reduction is supposed to flow to affected communities but frequently stalls at the detection stage rather than reaching remediation.
% ABSENT_VOICES: Marginalized communities named as the reading's beneficiaries are rarely present in the technical standard-setting bodies that define what 'bias' and 'fairness' mean operationally; gig workers and data-labelers whose harms don't fit clean statistical-parity metrics are structurally hard to represent in audit frameworks built around demographic categories.
% DISAPPEARANCE_RATIONALE: Compliance vendors and ethics teams would lose their operational mandate overnight and fairness-audit infrastructure would likely dissolve quickly — the field's institutional footprint clearly depends on the reading. But whether the underlying communities' material situation would change is genuinely contested: some argue detection without enforceable remediation changes little for affected people day to day, while others point to real cases (credit scoring reform, hiring-algorithm bans) where the reading produced concrete rule changes.
% FOUNDING_PROBLEM: Documented cases (COMPAS recidivism scoring, Amazon's hiring algorithm, facial recognition misidentification rates across race) showed AI systems reproducing and amplifying existing social discrimination in ways that caused concrete harm to specific people in the present, not hypothetically in the future.
% FOUNDING_PROBLEM_CORROBORATION: Independent journalism (ProPublica's COMPAS investigation), peer-reviewed audits (Gender Shades, Buolamwini & Gebru), and litigation outcomes in housing and employment discrimination cases attest the founding problem from outside the fairness-research field and compliance-vendor industry that has since grown around it — though corroboration of whether current remediation efforts actually resolve the harm, versus merely documenting it, is thinner and comes mostly from the same practitioner community.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects a real but partial divergence between detection activity and material remediation: audits proliferate, certifications are sold, but enforcement teeth and downstream remedy for affected communities lag behind the volume of measurement work. Suppression (0.44) is moderate — the reading doesn't foreclose the safety/control reading by force, but it does structurally compete for the same funding and institutional attention, and firms can use compliance with this reading's audits as a liability shield that dampens further scrutiny. Theater ratio rises over the interval (0.22 to 0.40) as audit-and-certify activity scales faster than measurable harm reduction — a classic proxy-substitution signature where 'passed the fairness audit' substitutes for 'the harm stopped.' Accessibility collapse is comparatively low (0.35) because genuine alternative framings (the safety/control and integrated readings) remain live and contested, not foreclosed. Resistance is higher (0.62) because affected communities, journalists, and litigation continue to press for stronger remediation than the audit-compliance apparatus currently delivers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (fairness researchers, ethics practitioners, compliance vendors) sit near the low end of directionality: the reading subsidizes their institutional existence and career paths. Marginalized communities are named beneficiaries in the reading's own justification but structurally behave more like conditional payers — trapped exit options (cannot leave the credit, housing, or employment systems being audited), and remediation is contingent on enforcement that frequently does not follow detection. Gig workers and data-labelers are victims whose harms (displacement, psychological cost of labeling) are less legible to the reading's own bias-and-discrimination framing, producing an under-coverage gap even within the reading built to center present harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented, present-tense discriminatory harm from deployed AI systems) remains genuinely live — this is not a dead mandate propped up by inertia. The tension is narrower: whether the apparatus built to address it has drifted toward measuring and certifying harm rather than remediating it, which is exactly the theater_ratio trajectory tracked above. Classifying this as tangled_rope rather than snare or rope preserves both halves: real coordination benefit exists (documented cases of algorithmic reform following audits), and a real extraction dynamic exists (compliance industry capture, under-covered harm categories, resource competition with the sibling safety reading) — collapsing either half would mislabel the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_competition_with_safety_reading,
    'Does funding and institutional attention captured by the ethics/justice reading meaningfully reduce resources available to long-term safety research, or are these largely non-competing funding pools?',
    'Comparative analysis of AI safety and AI ethics funding sources (philanthropic, corporate, government) to determine overlap versus independence of funding pools, and interview data from researchers who have shifted between the two research programs.',
    'If pools are substantially shared and competitive, this reading''s extraction includes a real opportunity cost imposed on the safety_control_reading''s constraint, strengthening the case for an influences edge rather than mere coexistence. If pools are largely independent, the competitive framing is overstated and the readings coexist without meaningful structural pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_competition_with_safety_reading, empirical, 'Whether ethics/justice and safety/control readings compete for the same finite resources.').

omega_variable(
    detection_remediation_gap,
    'Is the growing gap between bias-detection activity (audits, certifications) and actual material remediation for affected communities a temporary implementation lag, or a structural feature of how compliance incentives are designed?',
    'Longitudinal tracking of specific audited systems: does documented bias detection predict subsequent policy or system changes, or does audit-and-certify activity substitute for remediation over multi-year windows?',
    'If structural, the theater_ratio trend is not a transitional artifact but a persistent extraction feature of the compliance-vendor beneficiary class, supporting reclassification pressure toward snare over time. If temporary lag, the tangled_rope classification with an improving trajectory is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detection_remediation_gap, empirical, 'Whether audit activity substitutes for or precedes actual harm remediation.').

omega_variable(
    reading_framing_underdetermination,
    'Is ''preventing reproduction of social bias and present-day harm'' a genuinely distinct commitment from the integrated reading''s justice component, or is this reading simply the integrated reading with the safety component suppressed by omission rather than by principled exclusion?',
    'Examine whether ethics/justice-reading institutions (fairness research field, ethics boards) actively argue against safety-control resource allocation, versus simply not engaging with it — active opposition would support a genuine forecloses-adjacent tension; mere non-engagement supports coexists_with.',
    'If active opposition is found, an influences or even partial-foreclosure relation to the integrated_reading would be more accurate than pure coexistence, since the integrated reading explicitly asserts non-exclusivity that this reading''s practice may implicitly deny.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the ethics/justice reading''s institutional practice implicitly excludes rather than merely deprioritizes the safety/control component.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 24, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_alignment_commitment__ethics_justice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__ethics_justice_reading, 0.1).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the ai_alignment_commitment kernel. ethics_justice_reading (this file) holds ε=0.52 centered on present, documented, demographically identifiable harm and a compliance-vendor/fairness-research beneficiary class. safety_control_reading is expected to carry a structurally different ε and victim set centered on diffuse future/civilizational risk exposure rather than named present communities. integrated_reading asserts non-exclusivity between the two and should show partial overlap in beneficiary/victim structure with both siblings while claiming a broader, harder-to-verify coordination function. Do not average ε across the three; each is a separate constraint with its own stable measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
