% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy: Qualitative Development Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   The Chinese Communist Party's legitimacy framework shifted at the 19th
 *   Party Congress (2017) from 'high-speed growth' to 'high-quality
 *   development' (高质量发展) as the primary performance criterion. This reading
 *   restructures the performance_legitimacy kernel around innovation,
 *   sustainability, and efficiency gains rather than raw GDP expansion. The
 *   constraint operates through industrial policy (Made in China 2025, dual
 *   circulation), financial repression directed at strategic sectors, and a
 *   venture capital/M&A infrastructure that channels capital toward 'hard
 *   tech' (semiconductors, AI, new energy). Traditional manufacturing and
 *   property-dependent local governments bear the adjustment costs: credit
 *   tightening, environmental compliance, land revenue erosion, and mandated
 *   upgrading without commensurate support. The state (agenda_setter)
 *   enforces this through planning targets, credit guidance, and cadre
 *   evaluation metrics that now weight 'quality' indicators. The constraint
 *   is presented as a necessary structural transformation; critics read it as
 *   managing a growth slowdown while protecting strategic rents.
 *
 * KEY AGENTS:
 *   - central_party_state: Primary agenda_setter (institutional/biographical) — sets targets, directs credit, evaluates cadres on quality metrics
 *   - high_tech_sectors: Primary beneficiary (powerful/generational) — receives directed credit, subsidies, market protection, talent allocation
 *   - state_backed_innovation_ecosystem: Primary beneficiary (institutional/generational) — national labs, state VC funds, university commercialization arms capture resource flows
 *   - traditional_manufacturing: Primary victim/payer (organized/biographical) — faces credit withdrawal, compliance costs, upgrading mandates without transition support
 *   - property_dependent_local_governments: Primary victim/payer (institutional/biographical) — land finance base erodes, debt service rises, no replacement revenue authorized
 *   - industrial_workers_in_declining_sectors: Excluded (powerless/immediate) — job losses, wage arrears, no voice in transition design
 *   - international_institutions_and_analysts: Observer (analytical/analytical) — assess compliance with WTO norms, debt sustainability, innovation system efficiency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.58).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.45).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy: Qualitative Development Reading").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '470aa81d-bbf1-4065-a191-2b4e56ec5e53').
narrative_ontology:cs_kernel_codification('470aa81d-bbf1-4065-a191-2b4e56ec5e53', formalized).
narrative_ontology:cs_authority_grounding('470aa81d-bbf1-4065-a191-2b4e56ec5e53', extraction).
narrative_ontology:cs_interpretation_layer_present('470aa81d-bbf1-4065-a191-2b4e56ec5e53').
narrative_ontology:cs_reading_relation('470aa81d-bbf1-4065-a191-2b4e56ec5e53', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('470aa81d-bbf1-4065-a191-2b4e56ec5e53', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('470aa81d-bbf1-4065-a191-2b4e56ec5e53', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('470aa81d-bbf1-4065-a191-2b4e56ec5e53', foundational, high_quality_development_legitimacy).
narrative_ontology:cs_axiom_status(high_quality_development_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('470aa81d-bbf1-4065-a191-2b4e56ec5e53', high_quality_development_legitimacy, conventional).
narrative_ontology:cs_axiom('470aa81d-bbf1-4065-a191-2b4e56ec5e53', secondary, innovation_driven_upgrading).
narrative_ontology:cs_axiom_status(innovation_driven_upgrading, holdable).
narrative_ontology:cs_axiom_grounding('470aa81d-bbf1-4065-a191-2b4e56ec5e53', innovation_driven_upgrading, empirically_contingent).
narrative_ontology:cs_reference_frame('470aa81d-bbf1-4065-a191-2b4e56ec5e53', post_19th_congress_legitimacy_framework).
narrative_ontology:cs_drift_state('470aa81d-bbf1-4065-a191-2b4e56ec5e53', post_zero_covid_policy_pivot, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('470aa81d-bbf1-4065-a191-2b4e56ec5e53', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, high_quality_development_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, innovation_driven_growth_model).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, structural_transformation_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the 'high-quality development' targets through Five-Year Plans, Politburo study sessions, and cadre evaluation reform. Directs credit via policy banks and window guidance. Controls the legitimacy narrative through propaganda apparatus. Can shift the constraint by redefining 'quality' indicators or reverting to growth targets. Holds all structural cards but bears political risk if transition stalls.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_party_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives directed credit, tax breaks, land allocation, talent recruitment support, and domestic market protection (procurement preferences, standards setting). Includes semiconductors, AI, new energy vehicles, biotech, aerospace. Dependent on state favor — policy shifts can redirect flows overnight. Global market access provides some exit option but core demand is domestic and policy-mediated.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    powerful, generational, constrained, global).

% Comprises national laboratories, state venture capital guidance funds (国家引导基金), university technology transfer offices, SOE R&D institutes. Captures the bulk of innovation funding allocation. Career paths for scientists and administrators are fused to this structure — identity_locked at the institutional level. Extracts rents via project selection, overhead rates, and commercialization mandates.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    institutional, generational, constrained, national).

% Faces credit tightening (green finance taxonomy excludes 'low-end' manufacturing), rising compliance costs (environmental, safety, energy intensity), forced upgrading mandates (Industrial Internet, smart factory pilots) without subsidies. Sunk capital in specific locations and supply chains prevents relocation. Workforce skills are industry-specific. Some firms capture 'upgrading' subsidies but most are net payers.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing, payer,
    organized, biographical, trapped, national).

% Land conveyance revenue (40-50% of local fiscal income) collapses as property market cools and central policy restricts land supply for industrial/residential use. Debt service on LGFV bonds rises. No authorized replacement revenue (property tax pilot stalled, VAT sharing favors center). Cadre promotion still tied to 'development performance' — now redefined as 'quality' but without fiscal tools. Identity_locked: the local state's self-concept and legitimacy are constituted through land-led development; they cannot conceive an alternative fiscal model.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    institutional, biographical, identity_locked, regional).

% Face layoffs, wage arrears, pension shortfalls as traditional factories close or relocate. No collective bargaining, no transition programs funded at scale. Hukou system limits geographic mobility. Would object to the transition's pace and distributional impact but have no channel. Their exclusion is structural — the constraint's coordination function (upgrading) explicitly de-prioritizes their sector.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, industrial_workers_in_declining_sectors, excluded,
    powerless, immediate, trapped, local).

% IMF, OECD, WTO, and independent analysts assess whether 'high-quality development' delivers TFP growth, respects trade rules, and manages debt sustainably. Their judgments affect capital costs, market access, and technology transfer — but they do not set the constraint. They see the full structure: coordination claims, extraction flows, victim exclusion.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, international_institutions_and_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of directing capital, talent, and policy attention toward long-horizon, high-uncertainty innovation (semiconductors, foundational AI, green tech) that private markets under-invest in due to spillovers, time horizons, and coordination failures across firms and regions.
% TRANSFER_FUNCTION: Moves credit, fiscal resources, land, and regulatory favor from traditional manufacturing, property development, and local government land finance toward the state-backed innovation ecosystem (national labs, guidance funds, strategic SOEs, 'hard tech' private firms) and the central state's legitimacy account.
% ABSENT_VOICES: Industrial workers in declining sectors (excluded stakeholder) would object to job losses without transition support. Rural populations dependent on land lease income (not directly represented) would object to land revenue collapse. Small private firms in traditional sectors (not organized) would object to credit discrimination. These voices are absent because the constraint's enforcement operates through cadre evaluation and credit guidance — no legislative or consultative channel exists for them.
% DISAPPEARANCE_RATIONALE: If the qualitative development constraint vanished overnight: credit would flood back to property and traditional manufacturing; local government fiscal stress would ease but structural upgrading would stall; the innovation ecosystem would lose its protected resource base and many 'strategic' projects would fail; the central state would lose its primary legitimacy narrative and revert to growth targeting. The political economy would reorganize around the pre-2017 model.
% FOUNDING_PROBLEM: Escaping the middle-income trap: by 2015-2017, China's growth model (investment-heavy, export-led, property-driven) hit diminishing returns — overcapacity in steel/cement, rising debt, environmental crisis, and technology dependence. The founding problem was how to sustain convergence to high-income status without a financial crisis or political rupture.
% FOUNDING_PROBLEM_CORROBORATION: The central state attests the problem is live (2023-2024 Central Economic Work Conferences still frame 'high-quality development' as the solution to incomplete upgrading). Independent economists (World Bank China 2030 follow-ups, Lardy/Pettis analyses) corroborate that the middle-income trap dynamics persist — TFP growth has not sustainably recovered, technology bottlenecks remain. However, they contest whether this specific constraint arrangement is solving it or managing its symptoms.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.58) reflects the structural transfer from traditional sectors to the innovation ecosystem via directed credit, land allocation, and regulatory preference. The rate is moderate because the coordination function (guiding structural upgrading) is real and partially succeeds — China leads in EVs, batteries, solar, high-speed rail. Suppression (0.45) is moderate: enforcement operates through cadre evaluation and credit guidance rather than overt coercion, but exit for local governments is structurally blocked (no bankruptcy framework, no revenue autonomy). Theater ratio (0.42) is significant: 'high-quality development' rhetoric exceeds measurable TFP gains; many 'strategic' projects are subsidy farms. Accessibility collapse (0.55) — alternatives (market-led upgrading, fiscal federalism) are institutionally foreclosed. Resistance (0.52) — local governments resist through hidden debt, land hoarding; traditional firms resist through regulatory arbitrage, but neither can shift the central agenda.
 *
 * PERSPECTIVAL GAP:
 *   From the central state's seat, this is a rope: genuine coordination solving the middle-income trap via structural upgrading. From traditional manufacturing's seat, it is a snare: extraction via credit starvation and compliance costs with no transition path. From property-dependent local governments' seat, it is a piton: the land-finance model atrophied but the constraint (growth-dependent legitimacy) persists theatrically — they administer the old model while the center demands the new. The engine computes per-seat types from power/exit/beneficiary structure; this divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Central state (agenda_setter, institutional, biographical, arbitrage) — d ≈ 0.15 (beneficiary: controls resource allocation, captures legitimacy gains). High-tech sectors (beneficiary, powerful, generational, constrained) — d ≈ 0.25 (net recipient of directed flows, but dependent on state favor). State innovation ecosystem (beneficiary, institutional, generational, constrained) — d ≈ 0.20 (institutionalized capture of innovation rents). Traditional manufacturing (payer, organized, biographical, trapped) — d ≈ 0.85 (credit withdrawal, compliance costs, no exit — cannot relocate easily, sunk capital). Property-dependent local governments (payer, institutional, biographical, identity_locked) — d ≈ 0.90 (fiscal model fused with cadre identity; no authorized alternative revenue; 'identity_locked' because the local state's self-concept is constituted through land-led development). Industrial workers (excluded, powerless, immediate, trapped) — d ≈ 0.95 (bear job losses with no voice). Observers (analytical) — d = 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (escaping the middle-income trap via structural upgrading) remains live — China has not yet achieved high-income status via innovation. However, the arrangement shows mandatrophy signs: the 'quality' metrics have become the new ritual (theater rising), the innovation ecosystem captures increasing rents (extraction rising), and the victim set (traditional sectors, local governments) has no viable exit. The constraint persists not because the transition is complete, but because the legitimacy framework cannot admit the transition may fail or stall. Classification as tangled_rope captures this: coordination function real but degrading, extraction rising, enforcement active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested performance_legitimacy kernel, and does the qualitative_development_reading instantiate a structurally distinct constraint from its siblings?',
    'Compare ε, beneficiary/victim structure, and classification across all four declared readings of the performance_legitimacy kernel. If ε values differ significantly and beneficiary/victim sets are disjoint, the kernel decomposes into multiple constraints.',
    'If confirmed, the qualitative_development_reading must be authored as a separate constraint story with its own ε, stakeholders, and classification — linked via network.affects_constraints to siblings. This is the ε-invariance principle applied to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel/reading decomposition structural identity').

omega_variable(
    growth_tolerance_credibility,
    'Is the declared tolerance for lower GDP growth rates a genuine structural shift in the legitimacy criterion, or rhetorical cover for managing an unavoidable growth slowdown?',
    'Track official growth targets vs. actual outcomes, and measure whether legitimacy discourse shifts when targets are missed. If missed targets trigger no legitimacy crisis but quality metrics are cited, tolerance is structural; if quality metrics are invoked only when growth misses, it is rhetorical.',
    'If rhetorical, the constraint''s coordination function is overstated — extraction (protecting incumbent interests while managing decline) dominates. Classification shifts toward snare. If structural, the tangled_rope coordination/extraction hybrid holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_tolerance_credibility, empirical, 'Whether growth tolerance is a real legitimacy shift or rhetorical adaptation').

omega_variable(
    innovation_ecosystem_capture,
    'Does the state-backed innovation ecosystem genuinely coordinate frontier innovation, or does it capture rents through directed credit, subsidy allocation, and market access privileges?',
    'Compare TFP growth and genuine innovation metrics (patent quality, citation impact, commercialization rates) in state-backed sectors vs. private counterparts. Track subsidy intensity per unit of innovation output. If subsidy-to-output ratio is high and innovation metrics low, capture dominates.',
    'If capture dominates, the beneficiary declaration overstates coordination — the constraint extracts from traditional sectors to fund a rent-seeking innovation complex. Effective extraction rises; classification pressure toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_ecosystem_capture, empirical, 'Whether the innovation ecosystem is a coordination mechanism or a rent-capture vehicle').

omega_variable(
    local_government_fiscal_collapse_risk,
    'Will property-dependent local governments face fiscal collapse as land finance erodes, and does the constraint provide a viable transition path or merely extract their remaining capacity?',
    'Monitor local government debt ratios, land revenue shares, and central transfer dependency over the interval. Assess whether VAT reform, property tax pilot, or new revenue-sharing mechanisms materialize at scale.',
    'If no transition path emerges and fiscal stress intensifies without relief, the victim structure hardens — local governments become trapped payers with no exit. This increases effective extraction and suppression, pushing classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_government_fiscal_collapse_risk, empirical, 'Fiscal viability of property-dependent local governments under qualitative development').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pl_qdr_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(pl_qdr_tr_t0, observed).
narrative_ontology:measurement(pl_qdr_tr_t3, performance_legitimacy__qualitative_development_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement_basis(pl_qdr_tr_t3, observed).
narrative_ontology:measurement(pl_qdr_tr_t6, performance_legitimacy__qualitative_development_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(pl_qdr_tr_t6, observed).
narrative_ontology:measurement(pl_qdr_tr_t9, performance_legitimacy__qualitative_development_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement_basis(pl_qdr_tr_t9, observed).
narrative_ontology:measurement(pl_qdr_tr_t12, performance_legitimacy__qualitative_development_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(pl_qdr_tr_t12, observed).
narrative_ontology:measurement(pl_qdr_tr_t15, performance_legitimacy__qualitative_development_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(pl_qdr_tr_t15, observed).
narrative_ontology:measurement(pl_qdr_tr_t18, performance_legitimacy__qualitative_development_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement_basis(pl_qdr_tr_t18, projected).

% Extraction over time
narrative_ontology:measurement(pl_qdr_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(pl_qdr_be_t0, observed).
narrative_ontology:measurement(pl_qdr_be_t3, performance_legitimacy__qualitative_development_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement_basis(pl_qdr_be_t3, observed).
narrative_ontology:measurement(pl_qdr_be_t6, performance_legitimacy__qualitative_development_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(pl_qdr_be_t6, observed).
narrative_ontology:measurement(pl_qdr_be_t9, performance_legitimacy__qualitative_development_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement_basis(pl_qdr_be_t9, observed).
narrative_ontology:measurement(pl_qdr_be_t12, performance_legitimacy__qualitative_development_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(pl_qdr_be_t12, observed).
narrative_ontology:measurement(pl_qdr_be_t15, performance_legitimacy__qualitative_development_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(pl_qdr_be_t15, observed).
narrative_ontology:measurement(pl_qdr_be_t18, performance_legitimacy__qualitative_development_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement_basis(pl_qdr_be_t18, projected).

% Suppression requirement over time
narrative_ontology:measurement(pl_qdr_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(pl_qdr_su_t0, observed).
narrative_ontology:measurement(pl_qdr_su_t3, performance_legitimacy__qualitative_development_reading, suppression_requirement, 3, 0.35).
narrative_ontology:measurement_basis(pl_qdr_su_t3, observed).
narrative_ontology:measurement(pl_qdr_su_t6, performance_legitimacy__qualitative_development_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(pl_qdr_su_t6, observed).
narrative_ontology:measurement(pl_qdr_su_t9, performance_legitimacy__qualitative_development_reading, suppression_requirement, 9, 0.4).
narrative_ontology:measurement_basis(pl_qdr_su_t9, observed).
narrative_ontology:measurement(pl_qdr_su_t12, performance_legitimacy__qualitative_development_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(pl_qdr_su_t12, observed).
narrative_ontology:measurement(pl_qdr_su_t15, performance_legitimacy__qualitative_development_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(pl_qdr_su_t15, observed).
narrative_ontology:measurement(pl_qdr_su_t18, performance_legitimacy__qualitative_development_reading, suppression_requirement, 18, 0.45).
narrative_ontology:measurement_basis(pl_qdr_su_t18, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, industrial_upgrading_credit_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, local_government_debt_restructuring).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, venture_capital_state_guidance_funds).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel decomposes into four constraint stories with disjoint beneficiary/victim structures and divergent ε. This reading (qualitative_development) has ε=0.58, beneficiaries=high-tech/innovation ecosystem, victims=traditional manufacturing/property-dependent local govts. The quantitative_growth_reading would have lower ε (~0.25), beneficiaries=broad employment/construction, victims=fiscal discipline. The techno_nationalist_reading has higher ε (~0.65), beneficiaries=defense/strategic SOEs, victims=consumer sectors/foreign dependence. The livelihood_security_reading has moderate ε (~0.40), beneficiaries=urban service sectors/rural migrants, victims=export-oriented manufacturing. All four share the kernel_id but are ε-invariant distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__qualitative_development_reading, institutional, 0.9).
constraint_indexing:directionality_override(performance_legitimacy__qualitative_development_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
