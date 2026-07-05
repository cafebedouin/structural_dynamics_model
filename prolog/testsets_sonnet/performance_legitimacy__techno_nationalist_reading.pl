% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Performance Legitimacy (Strategic Industry Self-Sufficiency)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   In this reading, the regime's legitimacy is grounded not in delivering
 *   growth rates, quality-of-life improvements, or 'high-quality development'
 *   metrics, but specifically in achieving technological self-sufficiency and
 *   dominance in strategic industries — semiconductors, AI, aerospace,
 *   advanced materials — framed as the material precondition for national
 *   security and great-power status. This reading directs massive state
 *   capital toward designated strategic sectors largely independent of market
 *   signals, deploys export controls and supply-chain resilience mandates,
 *   and treats capital, land, and credit reallocation away from consumer and
 *   SME sectors as an acceptable, even necessary, cost. The coordination
 *   function (solving the collective-action problem of underinvestment in
 *   decades-long, capital-intensive strategic technology) is real; the
 *   extraction (systematic reallocation of resources away from
 *   consumer-facing sectors and ordinary savers toward politically designated
 *   national champions, protected from market discipline) is also real and
 *   grows over the measured interval as the security framing entrenches. This
 *   is a distinct constraint from the quantitative_growth_reading (which
 *   would extract from labor/wages to sustain headline growth rates), the
 *   qualitative_development_reading (which would extract from
 *   ecological/social costs to fund efficiency transitions), and the
 *   livelihood_security_reading (which would extract fiscal capacity from
 *   long-term investment toward immediate consumption transfers) — the
 *   beneficiary and victim sets differ structurally across all four, which is
 *   why they are authored as four separate constraint stories in the same
 *   kernel family rather than one story with a measurement parameter.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.61).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy (Strategic Industry Self-Sufficiency)").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '4e63f809-ddbb-44fb-9153-fad1c32b8358').
narrative_ontology:cs_kernel_codification('4e63f809-ddbb-44fb-9153-fad1c32b8358', distributed).
narrative_ontology:cs_authority_grounding('4e63f809-ddbb-44fb-9153-fad1c32b8358', extraction).
narrative_ontology:cs_interpretation_layer_present('4e63f809-ddbb-44fb-9153-fad1c32b8358').
narrative_ontology:cs_reading_relation('4e63f809-ddbb-44fb-9153-fad1c32b8358', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('4e63f809-ddbb-44fb-9153-fad1c32b8358', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e63f809-ddbb-44fb-9153-fad1c32b8358', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('4e63f809-ddbb-44fb-9153-fad1c32b8358', foundational, strategic_autonomy_supersedes_allocative_efficiency).
narrative_ontology:cs_axiom_status(strategic_autonomy_supersedes_allocative_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('4e63f809-ddbb-44fb-9153-fad1c32b8358', strategic_autonomy_supersedes_allocative_efficiency, instrumental).
narrative_ontology:cs_axiom('4e63f809-ddbb-44fb-9153-fad1c32b8358', foundational, great_power_status_requires_technological_non_dependency).
narrative_ontology:cs_axiom_status(great_power_status_requires_technological_non_dependency, holdable).
narrative_ontology:cs_axiom_grounding('4e63f809-ddbb-44fb-9153-fad1c32b8358', great_power_status_requires_technological_non_dependency, conventional).
narrative_ontology:cs_reference_frame('4e63f809-ddbb-44fb-9153-fad1c32b8358', developmental_state_security_primacy).
narrative_ontology:cs_drift_state('4e63f809-ddbb-44fb-9153-fad1c32b8358', contemporary_supply_chain_decoupling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e63f809-ddbb-44fb-9153-fad1c32b8358', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sector).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_planning_apparatus).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, domestic_semiconductor_industry).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_goods_sector).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, small_and_medium_enterprises).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, retail_investors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, downstream_consumers).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, national_security_primacy_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, great_power_status_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers industrial policy directing capital toward semiconductors, AI, aerospace, and advanced manufacturing. Sets procurement mandates, subsidy criteria, and export-control compliance rules. Justifies allocation decisions by national security and strategic autonomy rather than market return, and can redefine what counts as 'strategic' at will.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_planning_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Receive preferential financing, guaranteed procurement, land, and regulatory shelter as designated strategic-sector leaders. Their scale and survival are substantially decoupled from market discipline; state backing lets them absorb losses that would sink an unsheltered competitor and compete globally on subsidized terms.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champion_firms, beneficiary,
    powerful, generational, arbitrage, global).

% Chip designers, advanced materials firms, and dual-use technology producers whose valuations and R&D budgets are inflated by directed state investment justified as security necessity. Their institutional position is now structurally tied to the continuation of the self-sufficiency framing.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sector, beneficiary,
    powerful, generational, arbitrage, national).

% Competes for capital, land, and skilled labor against strategic-sector allocation priorities. Credit is rationed toward designated industries, leaving consumer-facing manufacturing and services with tighter financing and lower relative state support even where they generate more employment per unit of capital.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_goods_sector, payer,
    moderate, biographical, constrained, national).

% Bear the brunt of credit rationing and local-government fiscal strain, since municipal resources are redirected toward hosting strategic-industry projects (land grants, infrastructure, tax holidays). Have no lobbying access to the planning apparatus and cannot relocate their customer base or supply chains easily.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, small_and_medium_enterprises, payer,
    powerless, biographical, trapped, regional).

% Channel household savings, often via state-encouraged equity and bond markets, into strategic-sector vehicles marketed as patriotic and high-potential investments. Bear the downside when subsidized firms overinvest in capacity that cannot clear the market, while gains are concentrated among early institutional insiders.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, retail_investors, payer,
    powerless, biographical, constrained, national).

% Pay through higher prices, narrower consumer choice, and delayed availability of goods when import substitution mandates favor domestically produced but less competitive strategic-sector output over cheaper or better imported alternatives. Have no voice in the classification of which goods count as strategic.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, downstream_consumers, payer,
    powerless, biographical, trapped, national).

% Economists, private-sector allocators, and reform-minded technocrats who would argue capital should flow to comparative-advantage sectors regardless of strategic labeling. Structurally excluded from agenda-setting once security framing dominates the legitimacy discourse; raising market-efficiency objections risks being read as insufficiently patriotic.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_allocation_advocates, excluded,
    moderate, biographical, constrained, national).

% Trading partners and multinational firms affected by export controls, reciprocal tariffs, and supply-chain decoupling driven by this legitimacy framing. Not part of the domestic legitimacy conversation but structurally shape and are shaped by it through retaliation and parallel decoupling.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, foreign_trading_partners, excluded,
    powerful, generational, constrained, global).

% Assess whether self-sufficiency investment yields genuine technological parity or overcapacity and malinvestment. Track subsidy flows, patent output, and supply-chain resilience metrics; their findings feed international policy debate but do not control domestic resource allocation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, independent_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, diffuse).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce capital, talent, and regulatory attention on a defined set of strategic technologies (semiconductors, AI, aerospace, advanced materials) faster than a purely market-driven allocation would, addressing a genuine collective-action problem: no individual firm has the incentive to bear the front-loaded, decades-long capital cost of building an independent supply chain against entrenched foreign incumbents.
% TRANSFER_FUNCTION: Moves capital, land, credit allocation priority, and regulatory shelter from consumer-facing and small-enterprise sectors toward defense-adjacent and nationally designated 'strategic' firms, justified by the national-security and great-power-status framing rather than by comparative return.
% ABSENT_VOICES: Market-allocation advocates and economists who would argue for comparative-advantage-based capital deployment are structurally sidelined once security framing dominates — dissent reads as disloyalty. Foreign trading partners affected by export controls and supply-chain decoupling have no seat in the domestic legitimacy conversation at all, despite bearing large structural consequences.
% DISAPPEARANCE_RATIONALE: The planning apparatus and national champions would argue the world rearranges catastrophically — supply-chain vulnerability, loss of strategic leverage, and diminished great-power standing. Consumer-sector actors and independent analysts would argue capital reallocates efficiently to comparative-advantage industries and much of the 'crisis' framing dissolves; the disagreement over what disappears is itself evidence the legitimacy claim is doing political work beyond its stated coordination function.
% FOUNDING_PROBLEM: Dependence on foreign suppliers for chips, aerospace components, and advanced materials was judged an existential vulnerability — a rival power could choke off critical inputs during conflict or coercive diplomacy, and technological subordination was read as incompatible with claimed great-power status.
% FOUNDING_PROBLEM_CORROBORATION: The state planning apparatus and national champion firms attest the vulnerability remains acute, citing recent export-control episodes as vindication. Independent policy analysts and some foreign trade economists attest that certain sub-sectors have already achieved adequate resilience and that continued massive directed investment now substantially exceeds what security logic alone would justify, functioning increasingly as industrial patronage; no fully disinterested party outside the security and industrial-policy establishment has corroborated the founding problem's continued acuity at current investment scale.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, contested).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.42) reflecting an early phase where strategic-sector investment still tracked plausible security gaps, and rises to 0.68 by the interval's end as investment scale outpaces demonstrable security necessity and increasingly functions as entrenched industrial patronage for designated champions. Theater ratio rises in parallel (0.22 to 0.42) as an increasing share of 'strategic' designation activity becomes performative — firms and localities relabel ordinary commercial activity as strategic to capture preferential treatment, diluting the term's original security content. Suppression rises (0.38 to 0.61) as dissent from the security framing becomes harder to voice: raising market-efficiency objections increasingly reads as insufficiently patriotic, and the enforcement apparatus (export-control compliance, procurement mandates, credit-allocation directives) hardens over the interval. All three series share one time grid across seven points.
 *
 * DIRECTIONALITY LOGIC:
 *   National champion firms and the defense-adjacent tech sector sit near the full-beneficiary end: they receive directed capital, procurement guarantees, and regulatory shelter, and their institutional survival is now partly decoupled from market performance. The state planning apparatus is the agenda-setter with analytical exit (it can redefine the strategic-sector boundary at will). Consumer-goods firms, SMEs, retail investors, and downstream consumers sit toward the target end: they bear credit rationing, tax and fiscal reallocation, price effects from import-substitution mandates, and investment risk in patriotically-marketed but sometimes overcapacity-prone vehicles, with limited ability to exit these costs given constrained or trapped exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (foreign chokepoint vulnerability) is genuinely live in narrow terms — certain critical technologies remain foreign-dependent — but the corroboration is asymmetric: only the benefiting apparatus and firms attest to continued acuity at the CURRENT investment scale, while independent analysts suggest some sub-sectors have already achieved adequate resilience. This is the classic mandatrophy signature: a genuine founding problem that partially persists being used to justify continued extraction at a scale exceeding what the residual problem requires. The tangled_rope classification (rather than snare) reflects that the coordination function has not fully atrophied — some strategic capacity genuinely would not exist without directed investment — but the asymmetric extraction from consumer and SME sectors, sustained by active enforcement (export controls, procurement mandates, credit directives), is real and growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_vs_industrial_patronage_boundary,
    'At what point does directed strategic-sector investment stop tracking genuine national-security necessity and become self-perpetuating industrial patronage justified by the same security language?',
    'Independent technical audit comparing current foreign-dependency risk in each designated ''strategic'' sub-sector against the marginal security benefit of continued investment scale; compare against counterfactual resilience achievable at lower investment intensity.',
    'If most designated sectors have already crossed adequate-resilience thresholds, the residual investment functions predominantly as extraction from consumer/SME sectors dressed in security language, pushing the classification toward snare; if security gaps remain wide, the tangled_rope''s coordination function is more substantial than the extraction component suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_vs_industrial_patronage_boundary, empirical, 'Whether directed investment still tracks genuine security necessity or has become self-justifying patronage.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the techno-nationalist framing the regime''s actual dominant legitimacy claim, or one of several simultaneously-deployed readings whose relative weight shifts opportunistically depending on which sector needs cover at a given moment?',
    'Track official rhetoric, budget allocation justifications, and propaganda emphasis across the interval; if techno-nationalist framing consistently dominates resource-allocation justifications relative to growth, quality-development, or livelihood framings, the reading is structurally load-bearing rather than merely rhetorical.',
    'If the techno-nationalist reading is genuinely dominant, this constraint carries the primary extraction burden in the kernel family; if it is one rotating justification among several, the extraction may be more evenly distributed across the sibling readings than this story alone suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is the load-bearing legitimacy claim or one of several rotating justifications.').

omega_variable(
    national_champion_market_discipline_erosion,
    'Have national champion firms genuinely achieved competitive technological parity, or has state protection allowed persistent inefficiency to accumulate behind the security justification?',
    'Compare designated national champions'' unsubsidized cost structure, patent quality (not just quantity), and export competitiveness against unprotected domestic and foreign competitors over the same interval.',
    'Genuine parity supports the coordination-function reading (temporary protection achieving real capability); persistent inefficiency behind protection would indicate the beneficiary relationship has become extractive rent-collection independent of any remaining coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_champion_market_discipline_erosion, empirical, 'Whether protected national champions have achieved genuine competitiveness or accumulated protected inefficiency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__techno_nationalist_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__techno_nationalist_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__techno_nationalist_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__techno_nationalist_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__techno_nationalist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__techno_nationalist_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 24, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the performance_legitimacy kernel, each authored as a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle. The techno_nationalist_reading directs capital toward defense-adjacent strategic sectors at the expense of consumer/SME allocation; the quantitative_growth_reading would extract from labor/wage suppression to sustain headline GDP figures; the qualitative_development_reading would extract from short-term social/ecological costs to fund efficiency transitions; the livelihood_security_reading would extract fiscal capacity from long-horizon investment to fund immediate consumption transfers. These readings can be simultaneously invoked by the same regime for different sectors or moments, which is why they are linked structurally rather than merged into one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
