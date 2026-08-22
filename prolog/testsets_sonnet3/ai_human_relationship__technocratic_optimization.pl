% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__technocratic_optimization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__technocratic_optimization, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_human_relationship__technocratic_optimization
 *   human_readable: Technocratic Optimization Reading of the AI-Human Relationship
 *   domain: political theology / technology ethics / labor
 *
 * SUMMARY:
 *   This story instantiates the technocratic optimization reading of the
 *   AI-human relationship kernel: AI is treated as a neutral instrument whose
 *   purpose is to maximize measurable efficiency, and human value is assessed
 *   by productivity and optimization potential. Under this reading, persons
 *   are reduced to data profiles, populations that score poorly on the
 *   relevant metrics are functionally excluded from work, credit, or care,
 *   power concentrates in the hands of those who design and own the scoring
 *   infrastructure, and human work rhythms are subordinated to machine-set
 *   pace. This is a distinct constraint from the incarnational_humanism
 *   reading (which holds AI must serve integral human development and treats
 *   the person as irreducible to optimization) and the
 *   instrumental_subsidiarity reading (which treats AI as a governable
 *   neutral tool). Those are separate stories with their own ε values and
 *   stakeholder structures, linked here via network edges; this file does not
 *   average across them or describe the contest between them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__technocratic_optimization, 0.81).
domain_priors:suppression_score(ai_human_relationship__technocratic_optimization, 0.68).
domain_priors:theater_ratio(ai_human_relationship__technocratic_optimization, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, extractiveness, 0.81).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__technocratic_optimization, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__technocratic_optimization, "Technocratic Optimization Reading of the AI-Human Relationship").
narrative_ontology:topic_domain(ai_human_relationship__technocratic_optimization, "political theology / technology ethics / labor").

domain_priors:requires_active_enforcement(ai_human_relationship__technocratic_optimization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__technocratic_optimization, '0e64abde-4a26-4b36-8a87-97e33e89c812').
narrative_ontology:cs_kernel_codification('0e64abde-4a26-4b36-8a87-97e33e89c812', distributed).
narrative_ontology:cs_authority_grounding('0e64abde-4a26-4b36-8a87-97e33e89c812', extraction).
narrative_ontology:cs_interpretation_layer_present('0e64abde-4a26-4b36-8a87-97e33e89c812').
narrative_ontology:cs_reading_relation('0e64abde-4a26-4b36-8a87-97e33e89c812', ai_human_relationship__incarnational_humanism, forecloses).
narrative_ontology:cs_reading_relation('0e64abde-4a26-4b36-8a87-97e33e89c812', ai_human_relationship__instrumental_subsidiarity, influences).
narrative_ontology:cs_axiom('0e64abde-4a26-4b36-8a87-97e33e89c812', foundational, human_value_is_productivity_measurable).
narrative_ontology:cs_axiom_status(human_value_is_productivity_measurable, holdable).
narrative_ontology:cs_axiom_grounding('0e64abde-4a26-4b36-8a87-97e33e89c812', human_value_is_productivity_measurable, instrumental).
narrative_ontology:cs_axiom('0e64abde-4a26-4b36-8a87-97e33e89c812', foundational, efficiency_maximization_is_the_proper_telos_of_technology).
narrative_ontology:cs_axiom_status(efficiency_maximization_is_the_proper_telos_of_technology, holdable).
narrative_ontology:cs_axiom_grounding('0e64abde-4a26-4b36-8a87-97e33e89c812', efficiency_maximization_is_the_proper_telos_of_technology, instrumental).
narrative_ontology:cs_reference_frame('0e64abde-4a26-4b36-8a87-97e33e89c812', pre_digital_administrative_discretion).
narrative_ontology:cs_drift_state('0e64abde-4a26-4b36-8a87-97e33e89c812', contemporary_algorithmic_governance_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0e64abde-4a26-4b36-8a87-97e33e89c812', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__technocratic_optimization, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, platform_owners).
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, algorithmic_management_vendors).
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, efficiency_optimized_institutions).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, gig_platform_workers).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, disabled_and_elderly_populations).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, informal_sector_workers).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, low_scoring_credit_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and deploy optimization systems that score, rank, and route human labor and attention according to measured productivity. They set the metrics, own the models, and capture the surplus generated by treating workers and users as inputs to be tuned rather than persons to be served. They can exit any particular market while the optimization logic itself persists across their portfolio.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, platform_owners, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__technocratic_optimization, platform_owners, beneficiary).

% Sell scoring, routing, and productivity-measurement infrastructure to platforms, employers, and lenders. Their business model depends on efficiency maximization being treated as the legitimate frame for evaluating human activity; every domain reframed as an optimization problem is a new market for their tools.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, algorithmic_management_vendors, beneficiary,
    organized, biographical, mobile, global).

% Hospitals, insurers, and firms that adopt optimization metrics to allocate scarce resources (staffing, credit, care) benefit from apparent objectivity and reduced administrative discretion, even as the metrics displace judgment about need, dignity, or desert.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, efficiency_optimized_institutions, beneficiary,
    institutional, generational, constrained, national).

% Are scored continuously on delivery speed, acceptance rate, and customer rating; the algorithm sets pace and can deactivate them with no appeal. Their labor is subordinated to machine-set targets; leaving one platform typically means facing the same scoring logic on the next.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, gig_platform_workers, payer,
    powerless, immediate, trapped, local).

% Are systematically scored as low-productivity or high-cost by optimization systems used in hiring, insurance underwriting, and care allocation. Their claim on resources is discounted precisely because they do not fit the productivity profile the system optimizes for; there is no exit from being measured this way when the systems govern access to employment or care.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, disabled_and_elderly_populations, payer,
    powerless, biographical, trapped, national).

% Fall outside the data profiles the optimization systems recognize at all — their labor and needs are invisible to the metrics that allocate credit, aid, and formal-sector opportunity, effectively excluding them by omission rather than low score.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, informal_sector_workers, payer,
    powerless, immediate, trapped, regional).

% Are denied credit, housing, or employment based on algorithmic risk and productivity scores derived from proxies they cannot see or contest. The system treats their reduced access to capital or documentation history as evidence of lower value rather than as a structural condition.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, low_scoring_credit_applicants, payer,
    powerless, biographical, constrained, national).

% Would argue that productivity metrics strip workers of bargaining power and dignity, but are structurally excluded from the design of the scoring systems and often from legal standing to challenge algorithmic management decisions directly.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, labor_organizers, excluded,
    moderate, biographical, constrained, national).

% Analyze the technocratic optimization frame against the tradition's insistence that human dignity is not reducible to economic output, naming the reduction of persons to data profiles as a structural harm rather than a neutral technical choice.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, catholic_social_teaching_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Optimization systems do solve real coordination problems — routing scarce labor supply to demand, allocating credit under uncertainty, triaging care under resource constraints — by replacing costly case-by-case discretion with scalable, auditable-looking metrics.
% TRANSFER_FUNCTION: Moves discretion and bargaining power from persons (workers, applicants, patients) to the owners and designers of the scoring systems, and moves the economic surplus from efficiency gains disproportionately to platform owners and vendors rather than to those whose labor or need is being measured.
% ABSENT_VOICES: Workers subject to algorithmic management, disabled and elderly people scored as low-productivity, and informal-sector populations invisible to the metrics are not present when scoring criteria are designed; labor organizers who would object are structurally denied standing or access to the models.
% DISAPPEARANCE_RATIONALE: If the technocratic optimization frame were abandoned overnight, platforms would need to restore human discretion or negotiated standards for labor allocation, credit underwriting, and care triage; the institutions and revenue models built on treating productivity-scoring as the legitimate universal frame would have to justify allocation decisions on other grounds — dignity, need, solidarity — which would materially change who gets work, credit, and care.
% FOUNDING_PROBLEM: Institutions faced genuine problems of scale: matching labor supply to fluctuating demand, allocating credit under information asymmetry, and triaging care under real resource scarcity, where manual discretion was slow, inconsistent, or captured by bias.
% FOUNDING_PROBLEM_CORROBORATION: Platform owners and vendors attest the scaling problem remains live and justifies continued optimization. Catholic social teaching theorists, labor organizers, and disability-rights advocates — outside the beneficiary set — attest that the original coordination problem has been overtaken by a totalizing metric that now excludes populations and subordinates persons to machine pace, corroborated by documented algorithmic-deactivation disputes and disparate-impact findings in credit and hiring litigation.
narrative_ontology:disappearance_verdict(ai_human_relationship__technocratic_optimization, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__technocratic_optimization, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__technocratic_optimization, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_human_relationship__technocratic_optimization, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__technocratic_optimization, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__technocratic_optimization_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__technocratic_optimization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__technocratic_optimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.81 over the interval as optimization logic diffuses from narrow scheduling applications into credit, hiring, and care allocation, each expansion converting a domain previously governed by discretion or need into one governed by score. Suppression (0.68) reflects the active enforcement machinery — deactivation algorithms, opaque scoring, denial of appeal — required to keep workers and applicants compliant with metrics they cannot contest; it is authored as a raw structural property, not scaled by scope, per the engine's computation rules. Theater ratio (0.42) captures the substantial genuine coordination value optimization systems provide (real scheduling and underwriting problems solved) alongside a growing performative layer of 'fairness' and 'transparency' dashboards that do not change scoring outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (platform owners), this reads as a rational, even benevolent, allocation mechanism solving real scarcity problems. From the payer seats (gig workers, disabled populations, informal-sector workers, low-scoring applicants) the identical structure operates as exclusion by design — their value is measured against a metric built without them and used to deny them access. The engine's per-seat computation should surface this divergence directly from the declared power/exit asymmetries; the claim of technocratic 'neutrality' is precisely what the payer seats' structural position falsifies.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform owners and algorithmic management vendors sit near the full-beneficiary end: they set the metrics, capture the surplus, and can exit any single deployment while the underlying logic persists across their portfolio. Gig workers, disabled and elderly populations, informal-sector workers, and low-scoring credit applicants sit near the full-target end: they are trapped or constrained, cannot see or contest the scoring criteria, and bear the cost of exclusion when the metric denies them work, credit, or care. Efficiency-optimized institutions occupy an intermediate position — they benefit from apparent administrative objectivity but are also constrained by competitive pressure to adopt the same metrics as peers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine scaling and allocation problems that manual discretion handled poorly — was real and remains partially live for some institutions, which is why founding_problem_status is authored as contested rather than dead. But the metric that solved the scaling problem has been extended, largely unchallenged, into domains (dignity, care, personhood) where it was never fit for purpose, and the extension serves concentrated beneficiaries (platform owners, vendors) far more than it serves the populations it now excludes. Classifying this as tangled_rope rather than snare preserves the fact that a real coordination function exists (efficient labor and credit matching) while still naming the asymmetric extraction and enforcement that keeps the arrangement standing — collapsing it to pure extraction would erase the genuine problem it once solved; calling it a pure rope would erase the documented exclusion of the powerless seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Is the disagreement between the technocratic_optimization reading and its siblings (incarnational_humanism, instrumental_subsidiarity) located in what AI IS (a neutral tool vs. an inherently totalizing logic) or in what human value IS (productivity-measurable vs. irreducible to measurement)?',
    'Trace where each reading''s proponents locate their strongest objection: instrumental_subsidiarity objects to inadequate governance of an otherwise neutral tool; incarnational_humanism objects to the anthropology (what a person is) that technocratic_optimization presupposes. These are different loci of disagreement and cannot be resolved by better regulation alone if the anthropological premise is the real fault line.',
    'If the disagreement is anthropological, no amount of instrumental_subsidiarity-style governance reform resolves the harms this reading produces — the scoring logic itself, not merely its unregulated deployment, would need to be abandoned for excluded populations to be re-included.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel contest is about tool-neutrality or about the definition of human value.').

omega_variable(
    reversibility_of_metric_capture,
    'Once institutions (hospitals, lenders, employers) have restructured their allocation processes around optimization metrics, can the founding coordination problem be resolved by non-metric means at comparable cost, or has metric-dependence become structurally irreversible?',
    'Comparative case studies of institutions that have reintroduced human discretion or appeals processes after algorithmic deployment: measure cost, speed, and outcome-equity changes.',
    'If reversal is cheap, the tangled_rope classification''s coordination component is genuinely available without the extraction; if reversal is prohibitively costly, the arrangement has calcified toward snare-like lock-in despite the nominal coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_metric_capture, empirical, 'Whether de-optimization is a live institutional option or foreclosed by sunk infrastructure.').

omega_variable(
    exclusion_by_omission_vs_low_score,
    'Is the harm to informal-sector workers (invisible to the metrics) structurally the same extraction mechanism as the harm to low-scoring credit applicants (visible but penalized), or are these two distinct harms bundled under one victim category?',
    'Compare remedy pathways: contestability of a low score versus contestability of total invisibility to the system; different remedies may be required.',
    'If distinct, this story may itself warrant decomposition per the ε-invariance principle into an ''exclusion by omission'' constraint and an ''exclusion by penalty'' constraint with different ε profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_by_omission_vs_low_score, conceptual, 'Whether omission-based and score-based exclusion are one mechanism or two.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__technocratic_optimization, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__technocratic_optimization, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_h_tr_t4, ai_human_relationship__technocratic_optimization, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ai_h_tr_t8, ai_human_relationship__technocratic_optimization, theater_ratio, 8, 0.3).
narrative_ontology:measurement(ai_h_tr_t12, ai_human_relationship__technocratic_optimization, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_h_tr_t16, ai_human_relationship__technocratic_optimization, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ai_h_tr_t20, ai_human_relationship__technocratic_optimization, theater_ratio, 20, 0.4).
narrative_ontology:measurement(ai_h_tr_t24, ai_human_relationship__technocratic_optimization, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__technocratic_optimization, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_h_be_t4, ai_human_relationship__technocratic_optimization, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(ai_h_be_t8, ai_human_relationship__technocratic_optimization, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(ai_h_be_t12, ai_human_relationship__technocratic_optimization, base_extractiveness, 12, 0.69).
narrative_ontology:measurement(ai_h_be_t16, ai_human_relationship__technocratic_optimization, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(ai_h_be_t20, ai_human_relationship__technocratic_optimization, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(ai_h_be_t24, ai_human_relationship__technocratic_optimization, base_extractiveness, 24, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__technocratic_optimization, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ai_h_su_t4, ai_human_relationship__technocratic_optimization, suppression_requirement, 4, 0.51).
narrative_ontology:measurement(ai_h_su_t8, ai_human_relationship__technocratic_optimization, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(ai_h_su_t12, ai_human_relationship__technocratic_optimization, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(ai_h_su_t16, ai_human_relationship__technocratic_optimization, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(ai_h_su_t20, ai_human_relationship__technocratic_optimization, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(ai_h_su_t24, ai_human_relationship__technocratic_optimization, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__technocratic_optimization, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__technocratic_optimization, 0.1).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, ai_human_relationship__incarnational_humanism).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, ai_human_relationship__instrumental_subsidiarity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings decomposed from the natural-language label 'AI as instrument of efficiency maximization vs. AI ordered to human dignity' (the ai_human_relationship kernel). incarnational_humanism authors AI's proper end as integral human development with near-zero legitimate extraction under its own premises; instrumental_subsidiarity authors AI as governable neutral infrastructure with moderate, correctable extraction; this story (technocratic_optimization) authors AI's operative telos as efficiency maximization itself, producing high, actively enforced extraction against populations that score poorly against productivity metrics. Each reading has its own stable ε and stakeholder structure and is not to be averaged with the others; they are linked here to preserve the contest's structure for downstream contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
