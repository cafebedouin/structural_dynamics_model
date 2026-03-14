% ============================================================================
% CONSTRAINT STORY: social_credit_scoring_logic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_credit_scoring_logic, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: social_credit_scoring_logic
 *   human_readable: Social Credit Scoring Logic Systems
 *   domain: governance/digital_surveillance/behavioral_control
 *
 * SUMMARY:
 *   Social credit scoring systems rank individuals and entities by behavioral
 *   compliance, financial reliability, and social trustworthiness using
 *   algorithmic assessment of digital traces, financial records, legal
 *   violations, and behavioral reports. These systems exist in multiple
 *   institutional contexts — from state-mandated national scoring (China's
 *   Sesame Credit) to corporate reputation scoring (online marketplaces,
 *   ride-sharing platforms) to financial credit assessment (traditional
 *   credit bureaus). The constraint story focuses on state-mandated
 *   comprehensive social credit as the primary form, which exhibits the
 *   fullest extraction mechanism and highest suppression. The core structural
 *   tension is between the genuine coordination function (identifying fraud,
 *   monitoring compliance, allocating scarce resources) and the asymmetric
 *   extraction mechanism (concentration of power, opaque penalties,
 *   behavioral control). The system's extractiveness has increased over time
 *   (0.35 → 0.68) as algorithmic precision has improved and integration
 *   across domains has deepened. The theater ratio (0.65) reflects that the
 *   system is presented as rational, scientific, and meritocratic, while
 *   operating largely as a tool of behavioral compliance and dissent
 *   suppression. The system classifies as Snare from the perspective of
 *   powerless, trapped individuals; as Tangled Rope from moderate cohorts and
 *   organized critics; as Rope from the state apparatus; and as a false
 *   summit from civilizational analytical perspectives that risk naturalizing
 *   contingent surveillance architecture as inherent governance.
 *
 * KEY AGENTS:
 *   - Scored Population: Primary victims (powerless/trapped) — bears extraction through social and economic penalties with no meaningful exit or appeal
 *   - Low-Score Cohorts: Victims (powerless/trapped) — face cascading penalties, employment discrimination, travel restrictions, educational barriers, child disadvantage
 *   - Mid-Tier Score Population: Mixed (moderate/constrained) — experiences coordination benefit (fraud reduction, fraud prevention incentives) alongside asymmetric compliance pressure
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — captures behavioral compliance, governance efficiency, and dissent monitoring; can exit system costlessly
 *   - Surveillance Infrastructure Operators: Beneficiaries (institutional/arbitrage) — profit from data collection, algorithmic processing, and system expansion
 *   - Civil Society Monitors: Organized critics (organized/constrained) — face reputational penalties for public criticism but also benefit from concrete accountability targets
 *   - International Community: Observer (analytical/analytical) — risks treating social credit as inevitable governance evolution rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_credit_scoring_logic, 0.68).
domain_priors:suppression_score(social_credit_scoring_logic, 0.72).
domain_priors:theater_ratio(social_credit_scoring_logic, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_credit_scoring_logic, extractiveness, 0.68).
narrative_ontology:constraint_metric(social_credit_scoring_logic, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(social_credit_scoring_logic, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_credit_scoring_logic, snare).
narrative_ontology:human_readable(social_credit_scoring_logic, "Social Credit Scoring Logic Systems").
narrative_ontology:topic_domain(social_credit_scoring_logic, "governance/digital_surveillance/behavioral_control").

domain_priors:requires_active_enforcement(social_credit_scoring_logic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_credit_scoring_logic, state_apparatus).
narrative_ontology:constraint_beneficiary(social_credit_scoring_logic, surveillance_infrastructure_operators).
narrative_ontology:constraint_victim(social_credit_scoring_logic, scored_populations).
narrative_ontology:constraint_victim(social_credit_scoring_logic, low_score_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCORED INDIVIDUAL (SNARE) — Subject to opaque algorithmic scoring with no meaningful exit or appeal. Trapped by national identity and economic integration into state systems. Bears extraction (travel bans, lending denial, job disqualification, social stigma) with no capacity to exit the constraint. Suppression is maximal: the scoring logic is proprietary, appeal mechanisms are theatrical, and alternatives are unavailable.
constraint_indexing:constraint_classification(social_credit_scoring_logic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GENERATIONAL LOW-SCORE COHORT (SNARE) — Intergenerational extraction via inherited disadvantage and reputational stigma. Children of low-score individuals face pre-built discrimination. The constraint operates across generations with no mechanism for rehabilitation or escape. Maximum suppression at generational scale.
constraint_indexing:constraint_classification(social_credit_scoring_logic, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-TIER SCORE COHORT (TANGLED ROPE) — Experiences mixed extraction and coordination. The scoring system does provide some coordination function (monitoring fraud, identifying bad-faith actors, incentivizing prosocial behavior). However, this coordination coexists with asymmetric extraction: the mid-tier experiences constant compliance pressure and marginal risk of score degradation. Exit is theoretically possible but highly constrained by economic integration and family obligations.
constraint_indexing:constraint_classification(social_credit_scoring_logic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE APPARATUS (ROPE) — Perceives the scoring system as a pure coordination mechanism: monitoring compliance, identifying risks, allocating resources efficiently. Experiences no extraction cost — the system subsidizes state capacity. The state has full arbitrage: it can modify, abandon, or weaponize the system at will. Extraction flows toward the state, not away.
constraint_indexing:constraint_classification(social_credit_scoring_logic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL SOCIETY MONITORS (TANGLED ROPE) — Organized critics and watchdog groups experience the constraint as both a threat (chilled speech, surveillance of activism) and an opportunity (documentation and exposure of injustice creates evidence for reform). They have constrained exit — they can criticize but face reputational and economic penalties. They also benefit from having concrete, measurable targets for accountability advocacy.
constraint_indexing:constraint_classification(social_credit_scoring_logic, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL OBSERVER (PITON) — Social credit systems are often presented as sophisticated governance innovations that reduce corruption and fraud. From an international perspective, the theater ratio reveals this framing as largely performative: the coordination benefits (fraud reduction) could be achieved through transparent, auditable mechanisms; instead, the opaque algorithmic logic serves primarily to concentrate power and control dissent. The innovation theater persists because it appears modern and scientific.
constraint_indexing:constraint_classification(social_credit_scoring_logic, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT RISK (MOUNTAIN) — From a civilizational perspective, one might argue that population monitoring is inherent to large-scale coordination: you cannot allocate resources, enforce contracts, or prevent fraud without surveillance. This perspective risks naturalizing the social credit system as an immutable law of governance. However, this is a false summit: many large-scale societies coordinate without comprehensive algorithmic scoring. The mountain classification wrongly treats a contingent institutional choice (comprehensive surveillance scoring) as a structural necessity.
constraint_indexing:constraint_classification(social_credit_scoring_logic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_credit_scoring_logic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_credit_scoring_logic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_credit_scoring_logic, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_credit_scoring_logic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_credit_scoring_logic, TR),
    TR >= 0.70.

:- end_tests(social_credit_scoring_logic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The system extracts significant behavioral compliance and social control benefits from scored populations. The extraction is concentrated: benefits flow to the state and surveillance operators, costs flow to low-score cohorts. The 0.68 value reflects that genuine coordination benefit (fraud reduction) coexists with significant illegitimate extraction (political control, dissent suppression, social segregation). The measurement trajectory (0.35 → 0.68) reflects historical evolution from limited fraud-prevention tool to comprehensive behavioral control system. Suppression (0.72): Very high. Multiple suppression mechanisms operate: algorithmic opacity prevents understanding or challenging scores, appeal processes are theatrical and rarely successful, economic integration makes exit functionally impossible, spatial monopoly (national scope) eliminates geographic alternatives. The constraint exhibits structural suppression (external barriers) that is not internalized — agents perceive the system as coercive, not natural. Theater ratio (0.65): Moderate-high. The system is presented as scientific, meritocratic, and necessary for governance. This theater masks the core extraction mechanism: behavioral control masquerades as fraud prevention, political suppression masquerades as rule enforcement, and power concentration masquerades as coordination. The theater has increased over time (0.40 → 0.65) as systems have become more sophisticated and integrated.
 *
 * PERSPECTIVAL GAP:
 *   The snare classification from the powerless perspective differs sharply from the rope classification from the state perspective. The gap reveals asymmetric information and power. The state genuinely experiences coordination benefit — the system works as intended for governance efficiency. The powerless individual experiences pure extraction — the system produces costs with no visible benefit. Both descriptions are accurate from their respective positions. The mandatrophy is resolved by recognizing that the indexical position determines the constraint's apparent function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are determined by structural position: who benefits and who bears costs. The state apparatus and surveillance operators are beneficiaries with arbitrage exit — they can modify or abandon the system at will, costs flow away from them, benefits flow toward them. Directionality for this group derives to ~0.05-0.15 (full beneficiary). Low-score individuals are victims with no exit — they bear costs and receive no coordination benefit, directionality derives to ~0.95 (full target). Mid-tier individuals are both beneficiaries (of fraud prevention) and victims (of compliance pressure), with constrained exit — their directionality derives to ~0.55 (balanced). Civil society monitors are victims (of surveillance) with constrained exit, but also positioned to benefit from advocacy success — their directionality derives to ~0.65-0.70 (net victim but with agency). The engine computes effective extractiveness (χ) from base extractiveness (ε=0.68) scaled by f(d) for each agent. For the powerless trapped victim, f(d≈0.95) produces maximum effective extraction. For the state, f(d≈0.10) produces negative effective extraction (the system subsidizes state capacity).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The social credit system's mandatrophy is fully resolved through indexical analysis. From the state's perspective, the system is Rope — it solves genuine coordination problems (fraud detection, compliance monitoring, resource allocation). From the powerless individual's perspective, the system is Snare — it extracts behavioral compliance and social control with no coordination benefit to them. Both perspectives are structurally correct. The apparent contradiction (is it coordination or extraction?) is not resolved by choosing one perspective over another, but by recognizing that the system IS both coordination and extraction, and which appears dominant depends on the observer's structural position within it. The analytical falsely-summit risk is that civilizational perspectives might naturalize the system as 'inherent to large-scale governance' when in fact it is a contingent institutional choice. Transparent, auditable alternatives exist — traditional credit scoring, fraud investigation, compliance monitoring without comprehensive behavioral tracking. The social credit system is not inevitable; it is chosen because it concentrates power. This choice appears as necessity only from perspectives that benefit from the concentration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_mechanism,
    'Is the opacity of scoring algorithms a fundamental technical necessity or a deliberate design choice to prevent accountability?',
    'Comparison with transparent scoring systems in other domains (credit scores, insurance ratings); analysis of whether proprietary claims are justified by genuine trade secrets or by suppression intent',
    'If technical necessity: suppression rating reduces to 0.50-0.55, classification shifts toward Tangled Rope. If deliberate design: suppression remains at 0.72, Snare classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_mechanism, empirical, 'Whether algorithmic opacity is technical necessity or suppression mechanism').

omega_variable(
    appeal_mechanism_functionality,
    'Do formal appeal processes for score disputes constitute meaningful exit opportunities or are they theatrical with predetermined outcomes?',
    'Longitudinal tracking of appeal success rates, analysis of appeal outcomes vs. initial scores, interviews with appeal decision-makers, structural analysis of appeals process design',
    'If appeals are effective: exit_options for powerless agents upgrade from ''trapped'' to ''constrained'', extractiveness drops to 0.50-0.55. If theatrical: trapped status confirmed, extractiveness remains at 0.68.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appeal_mechanism_functionality, empirical, 'Whether appeal mechanisms provide genuine exit or are theatrical').

omega_variable(
    coordination_vs_extraction_ratio,
    'What proportion of the system''s function is legitimate fraud prevention/coordination vs. illegitimate behavioral control/extraction?',
    'Empirical measurement of fraud cases prevented by scoring vs. cases of incorrect penalization; analysis of score degradation patterns (are they correlated with documented violations or with political/demographic targeting?)',
    'If coordination-dominant (70%+): system reclassifies as Tangled Rope with extractiveness ~0.35. If extraction-dominant (70%+): Snare classification confirmed at 0.68+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ratio, empirical, 'Ratio of legitimate coordination to illegitimate extraction function').

omega_variable(
    intergenerational_transmission_mechanism,
    'Do children of low-score individuals face actual formal disadvantage from inherited scores, or does the harm operate solely through internalized stigma and social discrimination?',
    'Analysis of official policy (does the system formally penalize children?) vs. de facto implementation (do schools, employers, lenders treat children differently based on parent scores?)',
    'If formal: generational snare classification is structurally correct. If de facto only: harm is real but classification should reflect social rather than systemic mechanism — modify commentary but not metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Whether intergenerational harm is formal policy or social implementation').

omega_variable(
    state_capacity_dependency,
    'Is the state''s reliance on comprehensive social credit systems so deep that removing the system would create genuine governance collapse, or is the system fundamentally substitutable?',
    'Analysis of state coordination mechanisms in pre-social-credit era and in systems without comprehensive scoring; stress-testing of core state functions (tax collection, fraud prevention, resource allocation) under scenarios of score system removal',
    'If dependent: state''s arbitrage exit options are overestimated, institutional perspective shifts toward constrained. If substitutable: arbitrage characterization is confirmed — state can exit costlessly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_dependency, empirical, 'Whether state capacity is dependent on social credit system').

omega_variable(
    behavioral_compliance_coercion,
    'Does the scoring system achieve behavioral change through internalized norm adoption (light coercion) or through material penalty threat (heavy coercion)?',
    'Measurement of behavioral compliance patterns pre- and post-score change; analysis of whether compliance correlates with score knowledge; interviews with compliers on motivations',
    'If norm-internalized: suppression rating drops to 0.55-0.60 (agents perceive constraint as natural). If penalty-threatened: suppression remains at 0.72 (agents perceive constraint as coercive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_compliance_coercion, empirical, 'Whether compliance is norm-internalized or penalty-threatened').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_credit_scoring_logic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scs_tr_t0, social_credit_scoring_logic, theater_ratio, 0, 0.4).
narrative_ontology:measurement(scs_tr_t3, social_credit_scoring_logic, theater_ratio, 3, 0.5).
narrative_ontology:measurement(scs_tr_t6, social_credit_scoring_logic, theater_ratio, 6, 0.58).
narrative_ontology:measurement(scs_tr_t10, social_credit_scoring_logic, theater_ratio, 10, 0.65).
narrative_ontology:measurement(scs_tr_t1, social_credit_scoring_logic, theater_ratio, 1, 0.42).

% Extraction over time
narrative_ontology:measurement(scs_be_t0, social_credit_scoring_logic, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scs_be_t3, social_credit_scoring_logic, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(scs_be_t6, social_credit_scoring_logic, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(scs_be_t10, social_credit_scoring_logic, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(scs_be_t1, social_credit_scoring_logic, base_extractiveness, 1, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_credit_scoring_logic, enforcement_mechanism).
narrative_ontology:affects_constraint(social_credit_scoring_logic, behavioral_compliance_coercion).
narrative_ontology:affects_constraint(social_credit_scoring_logic, algorithmic_decision_opacity).
narrative_ontology:affects_constraint(social_credit_scoring_logic, dissent_suppression_infrastructure).
narrative_ontology:affects_constraint(social_credit_scoring_logic, financial_surveillance_integration).
narrative_ontology:affects_constraint(social_credit_scoring_logic, reputational_commons_contamination).

% DUAL FORMULATION NOTE:
% Social credit scoring can be decomposed into structurally distinct constraints: (1) fraud_detection (ε≈0.15, Rope) — genuine coordination to identify non-compliant actors; (2) behavioral_compliance_enforcement (ε≈0.72, Snare) — extraction mechanism for behavioral control; (3) political_dissent_suppression (ε≈0.85, Snare) — pure extraction for suppression of organized opposition. The aggregate system story treats these as unified, but they have different ε values and different perspectives. This story focuses on the aggregate; decomposed stories handle specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_credit_scoring_logic, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
