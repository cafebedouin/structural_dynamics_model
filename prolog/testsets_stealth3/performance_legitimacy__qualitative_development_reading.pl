% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: High-Quality Development Legitimacy Standard (Qualitative Reading)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   Since the 19th Party Congress (2017) the party-state's
 *   performance-legitimacy standard has been re-specified from headline
 *   growth to 'high-quality development': innovation, coordination, green,
 *   open, and shared development, operationalized as innovation KPIs, energy
 *   and emissions caps, deleveraging rules, and strategic-industry lists.
 *   This file instantiates the qualitative_development_reading of the
 *   performance_legitimacy kernel as a single epsilon-invariant constraint:
 *   the standing arrangement under contest is the legitimacy standard itself
 *   — the rule that elite careers, credit, and policy priority flow to
 *   whoever delivers structural transformation rather than raw expansion. Its
 *   beneficiaries are the high-tech sectors and the state-backed innovation
 *   ecosystem; its payers are traditional export manufacturing,
 *   property-dependent local governments, and the workers of retired
 *   capacity. Sibling readings (quantitative growth, techno-nationalist
 *   self-sufficiency, livelihood security) are separate constraint files
 *   linked through network.affects_constraints; their epsilon values differ
 *   because they condition legitimacy on different observables with different
 *   loser sets. Claim/metric independence holds: tangled_rope is my
 *   structural judgment; the metrics describe operation as observed. KEY
 *   AGENTS (by structural relationship): - central_planning_authorities:
 *   Agenda setter (institutional/arbitrage) — defines the standard, weights
 *   the KPIs, directs credit, evaluates careers - state_innovation_ecosystem:
 *   Primary beneficiary (institutional/identity_locked) — administers and
 *   captures the innovation budget; institutionally fused with the mission -
 *   high_tech_sectors: Primary beneficiary (powerful/mobile) — receives
 *   subsidized credit, procurement preference, regulatory patience -
 *   property_dependent_local_governments: Primary target (powerful/trapped) —
 *   lost the land-finance base under deleveraging rules -
 *   traditional_export_manufacturers: Target (moderate/mobile) — compliance
 *   costs, credit rationing, offshoring exit - legacy_sector_workers: Diffuse
 *   target (powerless/constrained) — displacement with thin retraining and
 *   registration-bound mobility - provincial_cadre_apparatus: Dual-positioned
 *   (powerful/constrained) — pays in fiscal stress, collects in promotion
 *   competition - private_venture_capital: Beneficiary with reversal exposure
 *   (powerful/arbitrage) — guided into hard tech, hedged abroad -
 *   sunset_region_residents: Excluded voice (powerless/constrained) — bears
 *   regional decline without a seat - independent_development_economists:
 *   Analytical observer (analytical/analytical) — audits the gap between
 *   reported and realized transformation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "High-Quality Development Legitimacy Standard (Qualitative Reading)").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '803fe44a-313a-4df5-8da4-778587c7b221').
narrative_ontology:cs_kernel_codification('803fe44a-313a-4df5-8da4-778587c7b221', formalized).
narrative_ontology:cs_authority_grounding('803fe44a-313a-4df5-8da4-778587c7b221', extraction).
narrative_ontology:cs_interpretation_layer_present('803fe44a-313a-4df5-8da4-778587c7b221').
narrative_ontology:cs_reading_relation('803fe44a-313a-4df5-8da4-778587c7b221', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('803fe44a-313a-4df5-8da4-778587c7b221', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('803fe44a-313a-4df5-8da4-778587c7b221', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('803fe44a-313a-4df5-8da4-778587c7b221', foundational, structural_quality_over_growth_rate).
narrative_ontology:cs_axiom_status(structural_quality_over_growth_rate, holdable).
narrative_ontology:cs_axiom_grounding('803fe44a-313a-4df5-8da4-778587c7b221', structural_quality_over_growth_rate, instrumental).
narrative_ontology:cs_axiom('803fe44a-313a-4df5-8da4-778587c7b221', secondary, slower_headline_growth_tolerance).
narrative_ontology:cs_axiom_status(slower_headline_growth_tolerance, holdable).
narrative_ontology:cs_axiom_grounding('803fe44a-313a-4df5-8da4-778587c7b221', slower_headline_growth_tolerance, instrumental).
narrative_ontology:cs_reference_frame('803fe44a-313a-4df5-8da4-778587c7b221', innovation_led_structural_transformation).
narrative_ontology:cs_drift_state('803fe44a-313a-4df5-8da4-778587c7b221', post_property_crisis_adjustment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('803fe44a-313a-4df5-8da4-778587c7b221', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_innovation_ecosystem).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_export_manufacturers).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, legacy_sector_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, provincial_cadre_apparatus).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, private_venture_capital).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, provincial_cadre_apparatus).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, private_venture_capital).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, middle_income_trap_thesis).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, innovation_driven_growth_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the five-year plan targets, innovation KPIs, energy and emissions caps, and credit-direction guidance through which the high-quality development standard operates; runs the cadre evaluations and disciplinary inspections that tie official careers to delivery; allocates national science budgets and strategic industry funds. Can redefine indicators when outcomes disappoint, as when growth-support measures were layered back in after the property downturn.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_planning_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% National laboratories, ministry-affiliated research institutes, university systems, and state guidance funds that administer the innovation budget. Their staffing, status, and continuation depend on the transformation mission remaining the organizing priority; they translate the standard into fundable programs and evaluate the proposals that claim to serve it. Dissolving the mission would dissolve the institutions themselves.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_innovation_ecosystem, beneficiary,
    institutional, generational, identity_locked, national).

% Semiconductor, AI, electric vehicle, battery, and biotech firms that receive subsidized credit, procurement preferences, talent programs, and regulatory patience. They operate in global markets and can list, incorporate, or offshore R&D abroad if domestic conditions sour, though scale and supply-chain depth anchor most of them at home.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    powerful, generational, mobile, global).

% City and county governments whose budgets, platform-company collateral, and staff compensation were built on land-sale revenue. Deleveraging rules and the property contraction removed that base while mandated spending and transfer obligations continued; they cannot repudiate the standard, cannot legally create substitute money, and cannot quickly rebuild a replacement revenue engine.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    powerful, biographical, trapped, regional).

% Low-margin assembly, textile, furniture, and processing-trade firms facing energy-consumption caps, environmental compliance costs, wage and land appreciation, and thinner bank-credit access than strategic-list peers. Larger firms have relocated lines to Southeast Asia or automated; smaller firms lack the capital to move or upgrade and sell or shutter instead.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_export_manufacturers, payer,
    moderate, biographical, mobile, global).

% Workers in closed or downsized plants, construction, coal and steel overcapacity, and property-chain trades. Reemployment paths run through age-limited gig work or migration constrained by household-registration ties; compensation and retraining arrive late and thin relative to displacement.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, legacy_sector_workers, payer,
    powerless, immediate, constrained, regional).

% Provincial and municipal officials whose promotions now hinge on innovation, environmental, and efficiency metrics rather than headline GDP. They close or relocate legacy plants, chase showcase projects, and absorb the fiscal hole left by shrinking land sales; simultaneously they compete for the new resources the standard distributes, and some regions have converted the pivot into genuine new industry clusters.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, provincial_cadre_apparatus, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, provincial_cadre_apparatus, beneficiary).

% Domestic and dollar-denominated funds that the standard courted into hard-tech, semiconductors, and green industry through guidance-fund co-investment and listing-channel preferences. They gained deal flow and exit windows in favored sectors, and learned from adjacent crackdowns that favored status can reverse with a policy document, keeping part of their book deployable overseas.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, private_venture_capital, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, private_venture_capital, payer).

% Residents of northeastern and other legacy-industrial regions whose towns depend on the factories and mines the transition retires. They have no seat in planning consultations, their tax base leaves with the firms, and household registration and family obligations make the growing southern service cities a costly, partial escape.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, sunset_region_residents, excluded,
    powerless, biographical, constrained, regional).

% Academic and multilateral researchers who audit the gap between reported innovation output and measured productivity, publish on debt sustainability and land-finance exposure, and provide the outside attestations the founding-problem record relies on. They observe without a vote in target-setting.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, independent_development_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, state_innovation_ecosystem).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a continental-scale economy's capital, energy, and bureaucratic attention away from an exhausted investment-property-export model toward productivity-raising activity — basic research, strategic manufacturing, green capacity — that dispersed actors underprice because returns are long, uncertain, and partly public.
% TRANSFER_FUNCTION: Moves bank credit, fiscal transfers, energy quotas, land-use approvals, and cadre promotion slots from property development, low-end export manufacturing, and land-finance-dependent local budgets toward high-tech sectors, the state innovation apparatus, and officials who deliver upgrading metrics.
% ABSENT_VOICES: Legacy-sector workers and sunset-region residents bear displacement without representation in target-setting; small private manufacturers outside the strategic lists have no consultative channel; foreign holders of property-sector debt were never consulted on the deleveraging design that impaired them.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, cadre evaluation would snap back to headline growth, credit guidance would return to property and export channels, land sales would be re-monetized, and the innovation funding apparatus would lose its mandate — the economy would reorganize around the pre-2017 growth model within a planning cycle.
% FOUNDING_PROBLEM: The investment-and-property growth model was exhausting itself: total-debt ratios climbing faster than output, land-finance dependency making local budgets hostage to a property bubble, environmental ceilings binding, and the risk of stagnating in middle-income status before catching up.
% FOUNDING_PROBLEM_CORROBORATION: IMF Article IV assessments and BIS debt statistics corroborate the debt-overhang diagnosis; independent total-factor-productivity research corroborates the efficiency slowdown; municipal bond spreads and rating-agency actions on local-government financing vehicle credits corroborate land-finance fragility — none of these sources sits inside the beneficiary set.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the standard's operation redirects bank credit, energy quotas, land-use approval, and fiscal attention away from identifiable groups — land-finance-dependent local governments lost their principal revenue instrument under the deleveraging rules, low-end exporters face compliance costs and credit rationing relative to strategic-list peers, and displaced workers absorb the adjustment — while the gains concentrate in the state innovation apparatus and favored sectors. Suppression (0.62) is a raw structural property, unscaled by scope or directionality in the engine's arithmetic: enforcement runs through cadre-evaluation career stakes, disciplinary inspection, credit window guidance, and target-linked accountability rather than mass coercion; the temporal series shows this machinery building sharply through 2022 and decaying afterward as the property downturn forced support measures. Theater (0.34) is real but minority: patent-count inflation, showcase laboratories, and rebranded projects coexist with genuine reallocation — battery, solar, and EV capacity is competitively real. Accessibility_collapse 0.5: alternatives persist (offshoring, automation, diversification, successful regional pivots) but narrow sharply for actors without capital — small manufacturers, older workers, fiscally trapped counties. Resistance 0.55: concealed local-government debt kept accumulating against the rules, statistical falsification episodes surfaced, industry associations lobbied for relief, and capital quietly re-routed. The claim (tangled_rope) is authored independently of these metrics; the engine computes per-seat types from the structural data, and any divergence from the claim is the datum the corpus exists to take. All three tracked metric series share one time grid (annual, 2017–2026) so no row substitutes an end-state scalar for an earlier value; the 2026 column carries projected basis pending full-year observation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the standard as the solution to a real trap-risk problem and reads its costs as transition prices; property-dependent local governments experience the same instruments as confiscation of their fiscal base with no compensating mandate; high-tech incumbents experience it as an opportunity structure; the innovation apparatus experiences it as constitutive mission. Same-nominal-power divergence: county governments and provincial cadres both hold powerful regional standing, yet the former are trapped (no legal route around the revenue loss) while the latter retain constrained maneuver (showcase selection, indicator negotiation) — the difference is fiscal structure, not rank. Coalition potential among powerless displaced workers exists demographically but is organizationally suppressed by the absence of independent representation; if the engine evaluates coalition-adjusted extraction for powerless seats, that absence is the binding fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: state_innovation_ecosystem and high_tech_sectors sit near the beneficiary pole (d low); the ecosystem's identity_locked exit pushes it deeper toward the subsidy side than its funding alone would. Property-dependent local governments sit nearest the full-target pole: trapped exit, concentrated loss. Traditional exporters carry high d moderated by mobile exit (offshoring arbitrage dampens effective extraction). Legacy workers combine powerless power with constrained exit for high d. Provincial cadres straddle: payer on fiscal flows, beneficiary on promotion competition — the engine reads the dual role. Private venture capital sits near symmetric with arbitrage-grade exit damping extraction. Receipt: extracted flows (directed credit, fiscal science budgets, quota rents) demonstrably accrue through the state innovation apparatus, which allocates them onward to favored firms — hence gain_flow names that seat rather than 'diffuse'. Fixing is prohibitive because removal would strand the sunk innovation infrastructure, contradict the leadership's signature doctrine, and leave the founding problems unaddressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification keeps both halves visible: reading the standard as pure rope would erase the named payers — land-finance governments, legacy workers — whose losses are structural, not incidental; reading it as pure snare would erase the real coordination achievement, since industrial upgrading in batteries, solar, and EVs is measurable and internationally competitive. Mandatrophy is not declared: the founding problem — debt overhang, property dependence, productivity slowdown — remains live by outside attestation (IMF and BIS debt data, independent productivity research), so the arrangement has not outlived its function. The risk to watch is the opposite drift: enforcement decay converting an enforced hybrid into a theatrically maintained shell if targets persist while instruments hollow out. The suppression_requirement series is the early-warning surface for that conversion, and the enforcement_post_crisis_trajectory omega records the open question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the performance_legitimacy kernel; the four readings assign legitimacy to mutually different observables, so which reading is operative determines who counts as delivering legitimate performance — what fixes this story to the qualitative reading rather than a blend?',
    'Track which observables actually gate cadre promotion and credit allocation in a given period (target documents, KPI weightings, inspection priorities); if headline growth-rate targets regain gating force, the operative reading has shifted toward the quantitative sibling.',
    'If the quantitative reading becomes operative again, this constraint''s beneficiary/victim structure dissolves into the growth-target structure and this file''s epsilon no longer describes the standing arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel-membership ambiguity: which reading of performance legitimacy is operative.').

omega_variable(
    quality_technonationalism_boundary,
    'Where does the qualitative reading end and the techno-nationalist sibling begin, given that self-sufficiency programs are funded through this reading''s innovation apparatus?',
    'Classify individual programs by their gating criterion: programs gated on productivity, efficiency, or sustainability outcomes belong to this reading; programs gated on import-substitution shares and strategic-autonomy metrics belong to the sibling file.',
    'If most funded programs are gated on self-sufficiency rather than efficiency, a large share of this story''s extraction belongs to the techno-nationalist file and this reading''s epsilon is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_technonationalism_boundary, conceptual, 'Boundary ambiguity between the quality agenda and the self-sufficiency agenda inside shared funding channels.').

omega_variable(
    transition_price_vs_durable_extraction,
    'Are the costs borne by traditional manufacturers, land-finance-dependent governments, and legacy workers a bounded transition price that ends with reabsorption, or durable extraction that persists after the transition completes?',
    'Cohort tracking: whether displaced workers reach comparable employment within a decade, whether land-finance governments acquire stable replacement revenues, whether exited manufacturers reappear in upgraded form or permanently exit.',
    'Bounded costs support the coordination-dominant reading of the standard; persistent immiseration shifts the balance toward extraction dominance and eventual snare-side classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_price_vs_durable_extraction, empirical, 'Whether transition costs are temporary or structural.').

omega_variable(
    innovation_output_realism,
    'How much of measured innovation progress (patent counts, R&D intensity, showcase clusters) reflects real productivity transformation versus statistical and theatrical production?',
    'Firm-level total-factor-productivity audits and input-output analysis cross-checked against export market share in genuinely competitive products; patent-citation quality distributions.',
    'A high theater share would push theater_ratio above 0.5, date a piton-drift warning, and undercut the coordination-function leg the tangled-rope classification rests on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_output_realism, empirical, 'Realism of innovation metrics under KPI pressure.').

omega_variable(
    enforcement_post_crisis_trajectory,
    'Does the post-2022 easing of enforcement (property support measures, softer deleveraging tone) mark durable decay of the standard''s suppressive machinery or a tactical pause before re-intensification?',
    'Watch target documents and disciplinary-inspection priorities through the next plan cycle; renewed red-line-style instruments would signal re-intensification.',
    'Durable decay moves the constraint toward inertial maintenance (standard retained, enforcement hollowing); re-intensification restores the enforced profile with higher suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_post_crisis_trajectory, empirical, 'Future trajectory of enforcement capacity after the property downturn.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 2017, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2017, performance_legitimacy__qualitative_development_reading, theater_ratio, 2017, 0.16).
narrative_ontology:measurement_basis(perf_tr_t2017, observed).
narrative_ontology:measurement(perf_tr_t2018, performance_legitimacy__qualitative_development_reading, theater_ratio, 2018, 0.19).
narrative_ontology:measurement_basis(perf_tr_t2018, observed).
narrative_ontology:measurement(perf_tr_t2019, performance_legitimacy__qualitative_development_reading, theater_ratio, 2019, 0.22).
narrative_ontology:measurement_basis(perf_tr_t2019, observed).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__qualitative_development_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement_basis(perf_tr_t2020, observed).
narrative_ontology:measurement(perf_tr_t2021, performance_legitimacy__qualitative_development_reading, theater_ratio, 2021, 0.29).
narrative_ontology:measurement_basis(perf_tr_t2021, observed).
narrative_ontology:measurement(perf_tr_t2022, performance_legitimacy__qualitative_development_reading, theater_ratio, 2022, 0.31).
narrative_ontology:measurement_basis(perf_tr_t2022, observed).
narrative_ontology:measurement(perf_tr_t2023, performance_legitimacy__qualitative_development_reading, theater_ratio, 2023, 0.33).
narrative_ontology:measurement_basis(perf_tr_t2023, observed).
narrative_ontology:measurement(perf_tr_t2024, performance_legitimacy__qualitative_development_reading, theater_ratio, 2024, 0.36).
narrative_ontology:measurement_basis(perf_tr_t2024, observed).
narrative_ontology:measurement(perf_tr_t2025, performance_legitimacy__qualitative_development_reading, theater_ratio, 2025, 0.35).
narrative_ontology:measurement_basis(perf_tr_t2025, observed).
narrative_ontology:measurement(perf_tr_t2026, performance_legitimacy__qualitative_development_reading, theater_ratio, 2026, 0.34).
narrative_ontology:measurement_basis(perf_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(perf_be_t2017, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement_basis(perf_be_t2017, observed).
narrative_ontology:measurement(perf_be_t2018, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2018, 0.47).
narrative_ontology:measurement_basis(perf_be_t2018, observed).
narrative_ontology:measurement(perf_be_t2019, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement_basis(perf_be_t2019, observed).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement_basis(perf_be_t2020, observed).
narrative_ontology:measurement(perf_be_t2021, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2021, 0.63).
narrative_ontology:measurement_basis(perf_be_t2021, observed).
narrative_ontology:measurement(perf_be_t2022, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2022, 0.67).
narrative_ontology:measurement_basis(perf_be_t2022, observed).
narrative_ontology:measurement(perf_be_t2023, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2023, 0.69).
narrative_ontology:measurement_basis(perf_be_t2023, observed).
narrative_ontology:measurement(perf_be_t2024, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2024, 0.7).
narrative_ontology:measurement_basis(perf_be_t2024, observed).
narrative_ontology:measurement(perf_be_t2025, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2025, 0.69).
narrative_ontology:measurement_basis(perf_be_t2025, observed).
narrative_ontology:measurement(perf_be_t2026, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(perf_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2017, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2017, 0.32).
narrative_ontology:measurement_basis(perf_su_t2017, observed).
narrative_ontology:measurement(perf_su_t2018, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2018, 0.44).
narrative_ontology:measurement_basis(perf_su_t2018, observed).
narrative_ontology:measurement(perf_su_t2019, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2019, 0.48).
narrative_ontology:measurement_basis(perf_su_t2019, observed).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement_basis(perf_su_t2020, observed).
narrative_ontology:measurement(perf_su_t2021, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2021, 0.71).
narrative_ontology:measurement_basis(perf_su_t2021, observed).
narrative_ontology:measurement(perf_su_t2022, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2022, 0.74).
narrative_ontology:measurement_basis(perf_su_t2022, observed).
narrative_ontology:measurement(perf_su_t2023, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement_basis(perf_su_t2023, observed).
narrative_ontology:measurement(perf_su_t2024, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2024, 0.66).
narrative_ontology:measurement_basis(perf_su_t2024, observed).
narrative_ontology:measurement(perf_su_t2025, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(perf_su_t2025, observed).
narrative_ontology:measurement(perf_su_t2026, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(perf_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% 'Performance legitimacy' is a colloquial label covering at least four structurally distinct legitimacy standards — growth-rate, quality/transformation, self-sufficiency, livelihood delivery — with different epsilon values, different beneficiaries, and different victims. Per the epsilon-invariance principle the kernel decomposes into four constraint files; this file instantiates the qualitative reading. Lineage: the quantitative reading historically grounded the others (growth delivered the resources each agenda spends); this reading's funding apparatus now resources the techno-nationalist program, which is why the influence edge runs from this file to that sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
