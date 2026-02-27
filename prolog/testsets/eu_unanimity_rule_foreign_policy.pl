% ============================================================================
% CONSTRAINT STORY: eu_unanimity_rule_foreign_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_unanimity_rule_foreign_policy, []).

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
 *   constraint_id: eu_unanimity_rule_foreign_policy
 *   human_readable: EU Unanimity Requirement for Foreign Policy and Financial Decisions
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The EU unanimity requirement for foreign policy and financial decisions
 *   creates a structural conflict between formal sovereign equality
 *   (requiring consensus) and material geopolitical urgency (demanding rapid
 *   response). This constraint simultaneously enables small-state voice and
 *   paralyzes collective action, generating extraction mechanisms hidden
 *   within coordination rhetoric. The constraint exhibits classic Tangled
 *   Rope structure: it provides genuine coordination benefits (preventing
 *   dominant-state hegemony) while enabling severe extraction through veto
 *   threats, slowness-induced advantage, and asymmetric costs of delay. Over
 *   the past 30 years, extractiveness has increased as geopolitical crises
 *   (military intervention, sanctions regimes, refugee flows) have
 *   accelerated while unanimity procedures have not adapted, creating a
 *   widening gap between decision-making speed requirements and institutional
 *   capacity. Theater ratio has risen as the gap between formal unanimity and
 *   actual practice (constructive abstention, enhanced cooperation
 *   workarounds) has grown, indicating the rule is increasingly maintained
 *   through performative amendments rather than functional enforcement.
 *
 * KEY AGENTS:
 *   - Small Member States (Malta, Cyprus, Slovakia, Estonia): Formal beneficiaries (powerless/trapped) — hold veto right but cannot exercise it without retaliation; experience constraint as snare despite nominal power
 *   - Major EU States (France, Germany, Italy): Primary beneficiaries (institutional/arbitrage) — capture veto slowness as negotiating leverage; can pursue bilateral foreign policy outside EU when unanimity fails
 *   - Medium Member States (Poland, Spain, Netherlands): Mixed position (organized/constrained) — nominally equal in veto power but face real diplomatic costs for exercising it; experience tangled_rope
 *   - EU Commission & Parliament: Reformist coalition (organized/constrained) — pursue treaty reform with sunset logic; see current unanimity as temporary obstacle to qualified majority voting
 *   - Sanctioned States / Crisis-Affected Regions: Victims (powerless/trapped) — bear costs of delayed EU response; no voice in EU decision-making, no exit option
 *   - EU Collective Action Capacity: Abstract victim — ability to respond to external crises is extracted by unanimity requirement's slowness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_unanimity_rule_foreign_policy, 0.58).
domain_priors:suppression_score(eu_unanimity_rule_foreign_policy, 0.65).
domain_priors:theater_ratio(eu_unanimity_rule_foreign_policy, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_unanimity_rule_foreign_policy, tangled_rope).
narrative_ontology:human_readable(eu_unanimity_rule_foreign_policy, "EU Unanimity Requirement for Foreign Policy and Financial Decisions").
narrative_ontology:topic_domain(eu_unanimity_rule_foreign_policy, "geopolitical").

domain_priors:requires_active_enforcement(eu_unanimity_rule_foreign_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_unanimity_rule_foreign_policy, small_member_states).
narrative_ontology:constraint_beneficiary(eu_unanimity_rule_foreign_policy, veto_holding_coalitions).
narrative_ontology:constraint_beneficiary(eu_unanimity_rule_foreign_policy, sovereigntist_governments).
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, urgent_policy_response).
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, eu_collective_action_capacity).
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, target_states_under_sanctions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SANCTIONED STATE / CRISIS REGION (SNARE) — Bears full cost of delayed EU coordination responses. Crisis timing (humanitarian, military, economic) does not await unanimity procedures. Trapped by geography and geopolitical circumstance; cannot exit EU's protracted decision-making. Maximum extraction of temporal and strategic advantage by actors who can afford delay.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAJOR EU STATES (ROPE) — Experience unanimity as coordination mechanism enabling negotiation leverage. Veto threat is a bargaining tool, not a coercive constraint. Can trade sanctions packages against other foreign policy priorities (Nord Stream, China trade access). Exit option (bilateral foreign policy outside EU framework) is real and available. Net beneficiary from coordination structure that amplifies their voice.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MEDIUM MEMBER STATES (TANGLED ROPE) — Experience mixed extraction and coordination benefit. Unanimity rule gives formal veto power (coordination benefit), but exercising it incurs diplomatic costs and EU funding retaliation (extraction mechanism). Constrained exit — cannot exit EU without massive cost, but can exercise veto with visible consequences. Requires active enforcement of social sanctions against vetoing states.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SMALL MEMBER STATES (SNARE) — Hold formal veto power but cannot meaningfully exercise it without severe diplomatic and financial retaliation. Trapped by EU dependence and asymmetric bargaining power. The veto right is performative — use triggers punishment. Cannot exit; cannot escape consequences of exercising formal rights.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: EU REFORMIST COALITION (SCAFFOLD) — European Parliament and Commission view unanimity as temporary obstacle being overcome through treaty reform, qualified majority voting expansion, and constructive abstention norm-building. Sees exit pathway through institutional redesign with sunset logic: unanimity sunset as treaty revision proceeds. Theater high because current reform efforts are performative (appear to change rules without actually changing them for years).
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TREATY FRAMEWORK / FORMAL LEGAL STRUCTURE (PITON) — Unanimity requirement persists from Maastricht/Nice treaties despite decades of recognized dysfunction. The legal form (treaty text requiring unanimity) persists through inertia while actual practice has partially evolved (constructive abstention, coordinated defaults, enhanced cooperation). Theater ratio high because the formal rule is maintained while its functional enforcement has degraded. Original coordination function (protecting small-state interests) has atrophied into performative sovereignty symbolism.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN CLAIM — FALSE SUMMIT) — From civilizational/universal frame, unanimity appears as immutable constraint: sovereign states cannot be bound without their consent (natural law of sovereignty). However, this naturalizes a contingent institutional design — unanimity was a policy choice, not a law of nature. The engine's false summit detector will identify this as naturalization of a contingent arrangement.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_unanimity_rule_foreign_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_unanimity_rule_foreign_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_unanimity_rule_foreign_policy, TR),
    TR >= 0.70.

:- end_tests(eu_unanimity_rule_foreign_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. The constraint extracts value from crisis-response urgency. When rapid sanctions or aid decisions are required (military intervention, humanitarian crisis, financial contagion), unanimity procedures guarantee delay. Actors able to benefit from delay (major states pursuing other agendas, states with conflicting interests) capture this temporal extraction. The growth from 0.35 to 0.58 reflects acceleration of geopolitical crises: Ukraine (2022), migration crises (2015+), financial instability (2008+, 2020+) have all exposed unanimity's inability to respond within operative timeframes. Suppression (0.65): Moderate-high. Alternative decision pathways are suppressed: qualified majority voting requires treaty reform (unanimity to change unanimity — structural lock); bilateral foreign policy faces strong normative and institutional pressure to route through EU. Small states face career and financial retaliation for unilateral vetoes. Theater ratio (0.62): Moderate-high, increasing. The gap between formal unanimity rule and actual practice has widened: constructive abstention, enhanced cooperation, and coordinated defaults now enable ~40% of major decisions to proceed without achieving formal unanimity. The ritual of unanimity negotiation persists even when outcomes are predetermined. Claimed type (Tangled Rope): Genuine coordination function (protecting small-state veto) coupled with asymmetric extraction (slowness benefits delay-advantaged actors; veto exercises incur retaliation costs).
 *
 * PERSPECTIVAL GAP:
 *   Major divergence between structural beneficiary/victim classifications. Major EU states (France, Germany) experience unanimity as Rope — they can exercise veto, pursue bilateral alternatives, and use veto threat as leverage in negotiations. Small states nominally hold equal veto power but experience Snare — exercising veto incurs retaliation and isolation, making formal rights performative rather than functional. The Analytical Observer risks seeing a Mountain (unanimity as immutable requirement of sovereign equality) but this is a false summit — unanimity is a policy choice, not a law of nature. The Treaty Framework itself (Piton) shows degradation: formal unanimity persists while actual enforcement has evolved into workarounds, indicating functional atrophy masked by legal persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position in the extraction flow. Major states (institutional/arbitrage): low d (~0.10-0.20) because they hold real exit options (bilateral policy) and can leverage veto as negotiating tool rather than being constrained by it. Small states (powerless/trapped): high d (~0.90-0.95) because they hold formal veto power but cannot exercise it without retaliation; trapped by EU dependence and asymmetric costs of sovereignty assertion. Medium states (organized/constrained): moderate d (~0.55-0.65) because veto power is real but exercising it triggers predictable diplomatic costs; some agency but constrained exit. Crisis-affected regions (powerless/trapped): maximum d (~0.98) because they have no voice in unanimity process and no exit from its consequences. The engine computes chi from these d values through f(d), capturing how much extraction each agent actually experiences despite their formal position in the institutional structure.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY — extractiveness 0.58 does not yet exceed 0.70 mandate threshold, but the constraint exhibits classic mandatrophy symptoms: (1) Coordination function (protecting small-state equality) legitimizes extraction mechanism (slowness-induced bargaining advantage); (2) Formal consensus rule masks asymmetric outcomes (major states benefit disproportionately from delay); (3) Reformist coalition (EU Parliament, Commission) recognizes dysfunction but reform itself requires unanimity, creating reflexive lock. If extractiveness rises above 0.70 (likely in high-crisis scenarios), mandatrophy would become acute — the coordination justification would become obviously false, requiring explicit resolution through treaty reform or institutional redesign. Current status (0.58) places constraint in high-risk zone where perspectival gap is widening: sanctioned states and crisis regions increasingly view unanimity as pure extraction, while major states can still invoke coordination logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_bargaining_extraction_vs_coordination,
    'Does veto power function as genuine coordination leverage (Rope) or as extractive mechanism hiding within formal consensus (Snare disguised as Rope)?',
    'Empirical analysis of veto exercises: track instances where veto was threatened/exercised; measure outcomes (actor receives concessions vs actor experiences retaliation and isolation). If outcomes cluster around actor receiving policy concessions, veto is coordination leverage. If outcomes cluster around retaliation and isolated vetoing state backing down, veto is extractive.',
    'If genuine coordination: rope classification correct. If extractive: snare/tangled_rope classification elevated; small states'' formal rights are theater, not function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_bargaining_extraction_vs_coordination, empirical, 'Whether veto power functions as coordination or extraction').

omega_variable(
    unanimity_functional_replacement,
    'Has unanimity requirement been functionally replaced by evolved practices (constructive abstention, enhanced cooperation, coordinated defaults) such that the formal rule is now theater rather than enforcement mechanism?',
    'Content analysis of EU Council decisions 2015-2026: measure proportion of decisions formally achieving unanimity vs those where members abstained, used enhanced cooperation clause, or defaulted to prior framework. If >40% of major decisions proceed without formal unanimity, unanimity rule is piton (theater maintenance), not functional constraint.',
    'If functionally replaced: piton classification confirmed. If still enforced: tangled_rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_functional_replacement, empirical, 'Whether unanimity is functionally enforced or theater-maintained').

omega_variable(
    treaty_reform_feasibility,
    'Is qualified majority voting reform achievable within 10-20 year timeline, or does unanimity lock itself in place (making sunset clause aspirational rather than structural)?',
    'Probabilistic analysis: track treaty reform attempts (Nice, Lisbon, post-Lisbon); measure convergence toward majority voting; assess whether initial unanimity defenders have veto over their own replacement. If treaty reform cycles are >15 years apart and unanimity blocks its own reform, sunset is not structural.',
    'If achievable: scaffold classification confirmed. If locked in: piton classification elevated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_reform_feasibility, empirical, 'Whether unanimity reform is structurally feasible').

omega_variable(
    small_state_actual_preference,
    'Do small member states genuinely prefer unanimity (as coordination protection) or are they coerced into defending it by larger states who benefit from slowness?',
    'Survey data and deliberative analysis: small-state stated preferences on unanimity reform vs. veto use patterns. If small states'' rhetoric favors unanimity but voting patterns show they abstain or accept majority when threatened, preference is coerced (extraction). If small states actively exercise veto and defend unanimity in treaty negotiations, preference is genuine (coordination).',
    'If genuine: unanimity legitimacy confirmed. If coerced: small-state snare classification elevated; large states are using small-state veto rights as regulatory capture tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_state_actual_preference, empirical, 'Whether small states genuinely prefer or are coerced into unanimity defense').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_unanimity_rule_foreign_policy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_unan_tr_t0, eu_unanimity_rule_foreign_policy, theater_ratio, 0, 0.4).
narrative_ontology:measurement(eu_unan_tr_t15, eu_unanimity_rule_foreign_policy, theater_ratio, 15, 0.55).
narrative_ontology:measurement(eu_unan_tr_t30, eu_unanimity_rule_foreign_policy, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(eu_unan_be_t0, eu_unanimity_rule_foreign_policy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eu_unan_be_t15, eu_unanimity_rule_foreign_policy, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(eu_unan_be_t30, eu_unanimity_rule_foreign_policy, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_unanimity_rule_foreign_policy, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_unanimity_rule_foreign_policy, eu_regulatory_capture_by_individual_states).
narrative_ontology:affects_constraint(eu_unanimity_rule_foreign_policy, nato_consensus_requirement).
narrative_ontology:affects_constraint(eu_unanimity_rule_foreign_policy, un_security_council_veto_system).

% DUAL FORMULATION NOTE:
% The unanimity requirement operates across two distinct structural contexts: (1) Foreign policy coordination among equal sovereign states (coordination-preservation logic); (2) Financial/budgetary decisions where fiscal capacity is asymmetric (extraction-enabling logic). These could be decomposed into separate constraints with different epsilon values, but are currently unified because EU treaty framework treats them identically. A future decomposition should separate 'unanimity_foreign_policy_coordination' (lower epsilon, more rope-like) from 'unanimity_budgetary_control' (higher epsilon, more snare-like).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_unanimity_rule_foreign_policy, institutional, 0.15).
constraint_indexing:directionality_override(eu_unanimity_rule_foreign_policy, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
