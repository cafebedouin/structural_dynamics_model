% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Flexible Employment as Structural Precarity and Surplus Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the precarity_extraction_reading of
 *   the flexible_employment_legitimacy kernel. The reading diagnoses flexible
 *   employment arrangementsâparticularly platform-mediated gig workâas
 *   structural precarity that externalizes employment risks onto workers
 *   while extracting surplus value through algorithmic control and social
 *   security cost-shifting. The constraint is the standing arrangement of
 *   contractor classification, algorithmic dispatch, and de-integrated social
 *   protection. The claim is snare: the coordination narrative (flexibility,
 *   entrepreneurship, market-clearing) is cover for pure extraction. Metrics
 *   are authored descriptively and independently of the claim.
 *
 * KEY AGENTS:
 *   - app_based_workers (powerless/trapped): Bear extraction through risk-shifting and algorithmic discipline
 *   - casualized_service_providers (moderate/constrained): Bear platform fees and benefit gaps with limited exit
 *   - gig_platforms (institutional/arbitrage): Set algorithmic terms, capture surplus, and enforce contractor classification
 *   - platform_shareholders (powerful/arbitrage): Collect returns from labor cost minimization
 *   - service_consumers (organized/mobile): Receive subsidized convenience; indirect beneficiaries
 *   - competition_regulators (institutional/analytical): Observers evaluating reclassification
 *   - labor_advocates (organized/constrained): Excluded from formal bargaining structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.82).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.78).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, snare).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Structural Precarity and Surplus Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '65f7fbcd-ad31-4708-b0c6-f80953e62fcf').
narrative_ontology:cs_kernel_codification('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', distributed).
narrative_ontology:cs_authority_grounding('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', extraction).
narrative_ontology:cs_interpretation_layer_present('65f7fbcd-ad31-4708-b0c6-f80953e62fcf').
narrative_ontology:cs_reading_relation('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', foundational, labor_risk_externalization_constitutes_surplus_extraction).
narrative_ontology:cs_axiom_status(labor_risk_externalization_constitutes_surplus_extraction, holdable).
narrative_ontology:cs_axiom_grounding('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', labor_risk_externalization_constitutes_surplus_extraction, empirically_contingent).
narrative_ontology:cs_axiom('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', foundational, algorithmic_control_is_labor_discipline_not_neutral_matching).
narrative_ontology:cs_axiom_status(algorithmic_control_is_labor_discipline_not_neutral_matching, holdable).
narrative_ontology:cs_axiom_grounding('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', algorithmic_control_is_labor_discipline_not_neutral_matching, empirically_contingent).
narrative_ontology:cs_reference_frame('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', genuine_autonomous_self_employment).
narrative_ontology:cs_drift_state('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', platform_economy_maturity, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('65f7fbcd-ad31-4708-b0c6-f80953e62fcf', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, gig_platforms).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_shareholders).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, service_consumers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, app_based_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, casualized_service_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set algorithmic management rules, classify workers as independent contractors, lobby against labor reclassification, and capture surplus via commission structures, data rents, and risk externalization. Can restructure across jurisdictions to preserve the contractor model.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_platforms, agenda_setter,
    institutional, generational, arbitrage, global).

% Capture returns from platform growth and labor cost minimization; their valuations depend on maintaining the contractor classification and suppressing employment protections.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_shareholders, beneficiary,
    powerful, biographical, arbitrage, global).

% Receive below-cost or convenient services subsidized by worker risk-bearing and social security gaps; their demand sustains the platform model but they do not set terms.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, service_consumers, beneficiary,
    organized, immediate, mobile, national).

% Accept algorithmically dispatched tasks, bear vehicle and equipment costs, lack social insurance and deactivation protections, and face income volatility shifted entirely onto them. Exit to formal employment is blocked by reference gaps and skill atrophy.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, app_based_workers, payer,
    powerless, immediate, trapped, local).

% Perform task-based labor through digital platforms with nominally greater schedule control but still bear platform fees, rating risk, and benefit gaps. Slightly more mobile than app-based workers but still dependent on the platform channel for market access.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, casualized_service_providers, payer,
    moderate, biographical, constrained, regional).

% Evaluate whether to reclassify platform workers as employees; caught between platform lobbying and labor advocacy; their rulings could alter the constraint's enforcement.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, competition_regulators, observer,
    institutional, generational, analytical, national).

% Would bargain collectively and press for social insurance inclusion but are structurally excluded by the independent contractor classification and anti-union platform practices.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, gig_platforms).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches variable labor demand from consumers with available workers on a per-task basis, replacing fixed employment contracts with on-demand dispatch and algorithmic queue management.
% TRANSFER_FUNCTION: Moves surplus value from app-based workers and casualized service providers to gig platforms and shareholders by externalizing employment risks (insurance, downtime, equipment depreciation, social security contributions) onto workers; simultaneously transfers below-market service prices and convenience to consumers.
% ABSENT_VOICES: Undocumented platform workers, workers in jurisdictions lacking reclassification pathways, future social insurance claimants facing depleted funds due to non-contribution, and traditional sector employers competing against cost structures built on risk externalization.
% DISAPPEARANCE_RATIONALE: If the contractor classification, algorithmic risk-shifting, and social security gaps vanished overnight, platforms would face immediate labor cost repricing toward formal employment levels, service prices would rise to reflect true labor costs, consumer demand would shift, and the entire platform business model would reorganize around direct employment or genuine self-employment with full risk-bearing compensation.
% FOUNDING_PROBLEM: Seasonal and episodic labor demand that formal employment contracts cannot efficiently match, alongside worker desire for schedule autonomy outside traditional shift structures.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists and sociologists outside the platform beneficiary set attest that digital dispatch technology and portable benefit schemes could solve the matching problem without contractor classification or risk-shifting; platform-affiliated economists attest the problem remains live. Independent empirical studies on wage floors and portable benefits in the EU and UK provide external corroboration that formalization does not eliminate the coordination function.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.82) is high because the arrangement systematically transfers employment risks and social insurance costs to workers while platforms capture fees and data rents. Suppression (0.78) is high because persistence depends on actively enforcing contractor classification, deactivating organizing workers, and lobbying against reclassification. Theater_ratio (0.45) reflects that the 'flexibility' and 'entrepreneurship' narratives are increasingly performativeâreal matching occurs, but a growing share of platform activity is devoted to maintaining the precarity frame rather than improving coordination. Accessibility_collapse (0.72) is high because once workers are embedded in the platform ecosystem, exit to formal employment is blocked by skill atrophy, reference gaps, and social insurance discontinuity. Resistance (0.55) is moderate because worker organizing and regulatory challenges are active but fragmented.
 *
 * PERSPECTIVAL GAP:
 *   The gig_platform seat should compute as near-beneficiary (low d): from their perspective, the constraint is a legitimate business model solving coordination problems. The app_based_workers seat should compute as near-target (high d): from their perspective, the same structure is enforced extraction with trapped exit. Service_consumers sit near symmetric but slightly toward beneficiary: they gain convenience and price subsidies but do not capture the extraction directly. The engine computes this divergence from structural data; the reading does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (gig_platforms, platform_shareholders, service_consumers) are structurally positioned to receive the surplus transfer or its secondary effects, deriving low directionality and damped effective extraction. Victims (app_based_workers, casualized_service_providers) bear the risk shift and algorithmic discipline, deriving high directionality and amplified effective extraction. The spatial scope (global for platforms, local for workers) further amplifies the asymmetry: platforms operate with arbitrage-grade exit across jurisdictions while workers are trapped in local labor markets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmatching episodic labor demandâhas been technologically solved and no longer requires contractor classification or risk-shifting to function. The constraint persists because it rearranges the world (extracts surplus), not because the founding problem remains unsolved. This prevents mislabeling the arrangement as a rope (genuine coordination) or scaffold (transitional support). The reading insists that any coordination benefit is severable from the extraction mechanism; removing extraction would not eliminate the matching function, only its monopolistic, risk-asymmetric form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the precarity_extraction_reading of kernel flexible_employment_legitimacy. How would classification change under sibling readings?',
    'Compare against market_efficiency_reading (lower extractiveness, coordination-framed) and developmental_state_reading (scaffold with sunset).',
    'Market efficiency reading would reclassify as rope or tangled_rope; developmental state reading as scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position of this reading within the kernel''s sibling set').

omega_variable(
    risk_transfer_empirical_boundary,
    'To what extent do platform worker earnings, net of expenses and risk-bearing, fall below equivalent formal employment compensation when accounting for social insurance gaps?',
    'Comprehensive matched-panel studies comparing platform worker net income with counterfactual formal employment in the same locality and skill category, including imputed value of benefits.',
    'If net earnings are at or above formal parity, the extraction claim weakens toward tangled_rope; if substantially below, snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_transfer_empirical_boundary, empirical, 'Whether empirical wage data supports the surplus extraction claim').

omega_variable(
    suppression_structural_vs_internalized,
    'Is worker immobility structural (deactivation risk, lack of alternative jobs, social insurance lock-in) or internalized (workers adopting entrepreneurial identity that masks dependency)?',
    'Post-exit trajectory studies: if workers who leave platforms continue to accept precarious conditions and underprice risk, suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measures and the constraint operates more like identity_coordination; if purely structural, enforcement is external and extraction is more transparent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism in platform labor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(flex_tr_t25, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(flex_be_t25, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(flex_su_t25, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
