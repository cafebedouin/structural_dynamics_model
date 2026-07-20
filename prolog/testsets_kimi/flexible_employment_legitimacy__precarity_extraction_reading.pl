% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Structural Precarity and Platform Surplus Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint is the precarity_extraction_reading of the
 *   flexible_employment_legitimacy kernel. It models the legal and cultural
 *   framework that legitimizes flexible employment arrangementsâgig work,
 *   independent contracting, platform laborâas a snare: a structure whose
 *   coordination narrative (efficient matching, worker autonomy,
 *   entrepreneurial freedom) serves as cover for the systematic extraction of
 *   surplus value through risk externalization, algorithmic labor discipline,
 *   and social protection cost-shifting. The constraint persists through
 *   active enforcement of employment classification boundaries and
 *   suppression of reclassification alternatives.
 *
 * KEY AGENTS:
 *   - gig_platforms: Agenda-setter (institutional/arbitrage) â designs terms, captures surplus, evades regulation
 *   - platform_workers: Primary target (powerless/constrained) â bears extraction directly through risk-shifting and algorithmic control
 *   - consumer_users: Secondary beneficiary (moderate/mobile) â receives subsidized prices externalized from labor costs
 *   - labor_regulators: Analytical observer (institutional/analytical) â captured authority with latent power to reclassify
 *   - traditional_labor_unions: Excluded voice (organized/constrained) â blocked from platform-mediated workplaces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.82).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.78).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, snare).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Structural Precarity and Platform Surplus Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '11ec6961-5f10-459e-8411-755e82da04d4').
narrative_ontology:cs_kernel_codification('11ec6961-5f10-459e-8411-755e82da04d4', formalized).
narrative_ontology:cs_authority_grounding('11ec6961-5f10-459e-8411-755e82da04d4', extraction).
narrative_ontology:cs_interpretation_layer_present('11ec6961-5f10-459e-8411-755e82da04d4').
narrative_ontology:cs_reading_relation('11ec6961-5f10-459e-8411-755e82da04d4', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('11ec6961-5f10-459e-8411-755e82da04d4', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('11ec6961-5f10-459e-8411-755e82da04d4', foundational, economic_reality_test_governs_classification).
narrative_ontology:cs_axiom_status(economic_reality_test_governs_classification, holdable).
narrative_ontology:cs_axiom_grounding('11ec6961-5f10-459e-8411-755e82da04d4', economic_reality_test_governs_classification, instrumental).
narrative_ontology:cs_axiom('11ec6961-5f10-459e-8411-755e82da04d4', foundational, social_protection_non_waivable).
narrative_ontology:cs_axiom_status(social_protection_non_waivable, holdable).
narrative_ontology:cs_axiom_grounding('11ec6961-5f10-459e-8411-755e82da04d4', social_protection_non_waivable, deontological).
narrative_ontology:cs_reference_frame('11ec6961-5f10-459e-8411-755e82da04d4', standard_employment_relation).
narrative_ontology:cs_drift_state('11ec6961-5f10-459e-8411-755e82da04d4', contemporary_platform_economy_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('11ec6961-5f10-459e-8411-755e82da04d4', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, gig_platforms).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, consumer_users).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, platform_workers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, independent_contractor_autonomy_doctrine).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, platform_efficiency_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design algorithmic management systems, set commission rates, and lobby for flexible employment classifications that externalize operational risks and social insurance costs to workers. Can restructure across jurisdictions to evade regulatory reclassification.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_platforms, agenda_setter,
    institutional, generational, arbitrage, global).

% Accept algorithmically dispatched tasks, bear direct costs of equipment, fuel, insurance, and income volatility, and lack social protections due to independent contractor classification. Exit is constrained by labor market slack and sunk platform-specific reputational capital.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_workers, payer,
    powerless, immediate, constrained, national).

% Receive lower-cost, on-demand services enabled by platform scale and suppressed labor costs. Do not directly bear the risks of income volatility or social protection gaps.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, consumer_users, beneficiary,
    moderate, biographical, mobile, national).

% Administrative and legislative bodies tasked with employment classification. They mediate between platform lobbying and worker advocacy, with authority to reclassify workers but face political capture and jurisdictional constraints.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% Seek to organize platform workers and challenge flexible employment classifications. Structurally excluded from platform-mediated workplaces designed around individual contractor relationships and decentralized work sites.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, traditional_labor_unions, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, gig_platforms).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches fragmented labor supply with on-demand demand in real time, reducing search and transaction costs for workers and consumers.
% TRANSFER_FUNCTION: Moves surplus value from platform workers to platform operators by shifting risksâequipment, insurance, income volatility, and social insuranceâonto workers while capturing platform fees and data rents. Transfers cost savings to consumers through suppressed labor costs.
% ABSENT_VOICES: Platform workers in jurisdictions without labor protections, would-be formal employers displaced by platform monopolization, and future social protection systems bearing accumulated precarity costs are absent from platform governance and pricing conversations.
% DISAPPEARANCE_RATIONALE: If the legitimacy of flexible employment vanished overnight and platforms were compelled to classify workers as employees with full protections, platform revenue models would collapse or restructure, consumer prices would rise to internalize labor costs, workers would gain stability at the cost of schedule flexibility, and labor markets would reorganize around formal employment channels.
% FOUNDING_PROBLEM: Rigid labor market regulations and high transaction costs in matching irregular labor demand with available supply left workers unemployed and consumers underserved in specific temporal and spatial niches.
% FOUNDING_PROBLEM_CORROBORATION: International Labor Organization data documents high informal employment rates in emerging economies, but independent labor economists and worker advocacy organizations outside the platform beneficiary set attest that in developed economies the constraint now persists beyond the founding scarcity conditions, functioning primarily as a mechanism for regulatory arbitrage rather than genuine coordination.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because platform revenue decouples from genuine service provision costs and captures rents from information asymmetry, algorithmic pacing, and classification arbitrage. Suppression is high (0.78) because the constraint depends on active legal classification enforcement, arbitration clauses, deactivation threats, and lobbying against collective bargaining. Theater ratio is substantial (0.50) due to the elaborate entrepreneurial narrative, flexibility rhetoric, and rating systems that perform autonomy while practicing discipline. Accessibility collapse (0.72) reflects the crowding-out of formal employment alternatives in platform-dominated sectors. Resistance (0.55) captures ongoing litigation, sporadic strikes, and regulatory investigations that have not yet overcome platform structural power.
 *
 * PERSPECTIVAL GAP:
 *   The platform operator seat computes the constraint as coordination it built and maintains; the worker seat computes it as coerced extraction. Consumer users experience a subsidized service. The engine derives this divergence from structural dataâbeneficiary status, constrained exit, and power asymmetryâwithout requiring the claim to adjudicate the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   gig_platforms are structural beneficiaries with global arbitrage exit (d near 0.0). platform_workers are declared victims with constrained exit and powerless positioning (d near 1.0). consumer_users are incidental beneficiaries with mobile exit (low d, low chi). labor_regulators sit at analytical remove. The directionality chain amplifies effective extraction for workers and damps it for platforms and consumers.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by insisting that the coordination functionâmatching supply and demandâdoes not redeem the constraint because the matching mechanism is inseparable from the extraction mechanism. The risk-shifting is not a side effect; it is the business model. Were the constraint to lose its extraction function but retain matching, it would be a different constraint (the market_efficiency_reading). The precarity_extraction_reading therefore resists collapsing into tangled_rope by asserting that the coordination story is cover, not complement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is flexible employment best understood as structural precarity enabling extraction, as a legitimate market mechanism, or as a transitional developmental form?',
    'Triangulation across longitudinal studies of worker net income after risk adjustment, comparative regulatory outcomes across jurisdictions with different classification regimes, and transition rates from flexible to formal employment.',
    'Resolving which reading is dominant would reclassify the constraint from snare to rope or scaffold, and reverse the beneficiary-victim structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the flexible employment kernel is structurally accurate').

omega_variable(
    worker_consent_as_suppression,
    'To what extent does worker participation in flexible employment reflect genuine preference versus internalized precarity and lack of alternative options?',
    'Exit-trajectory studies observing whether workers leave when formal alternatives become available, and retrospective reclassification of prior situations as constrained.',
    'If consent is largely internalized suppression, effective extraction is higher than structural measures suggest; if genuine preference, the constraint coordinates more than it extracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_consent_as_suppression, empirical, 'Whether worker participation reflects suppressed alternatives or genuine choice').

omega_variable(
    platform_cost_externalization_quantification,
    'What proportion of platform revenue derives from cost externalization to workers and public social protection systems rather than from genuine coordination efficiencies?',
    'Regulatory audit requiring platforms to account for full worker costs including imputed social insurance, equipment depreciation, and income risk premiums.',
    'A high proportion would confirm the snare classification; a low proportion would suggest tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_cost_externalization_quantification, empirical, 'Quantifying extraction versus coordination in platform revenue').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(flex_tr_t25, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 25, 0.5).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(flex_be_t25, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(flex_su_t25, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the flexible_employment_legitimacy kernel. The precarity_extraction_reading views the kernel as a snare of surplus extraction; the market_efficiency_reading views it as a rope of market coordination; the developmental_state_reading views it as a scaffold toward formalization. They are structurally distinct constraints with different epsilon values and must not be averaged into a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
