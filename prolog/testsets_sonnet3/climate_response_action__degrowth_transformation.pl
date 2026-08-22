% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Degrowth Transformation Reading of Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth_transformation reading of the
 *   climate_response_action kernel: the claim that meeting planetary
 *   boundaries requires abandoning GDP growth as an organizing principle in
 *   favor of sufficiency, redistribution, and reduced throughput, rather than
 *   betting on technological substitution or carbon markets within a
 *   growth-preserving frame. As authored, this reading requires active
 *   enforcement (working-time caps, consumption rationing mechanisms,
 *   redirected fiscal architecture) to move consumption headroom and resource
 *   claims from wealthy, high-throughput populations to Global South
 *   development claimants and future generations. It coordinates a genuine
 *   collective problem (staying within a shrinking carbon and resource
 *   budget) while imposing a real, non-consensual transfer on specific, named
 *   payers — hence tangled_rope rather than rope. This is a different
 *   constraint from mitigation_priority (which keeps growth and bets on
 *   decarbonization technology and carbon markets) and from
 *   adaptation_priority (which accepts warming and invests in resilience);
 *   each of the three kernel readings has its own beneficiary/victim
 *   structure, its own ε, and is authored as its own separate story, linked
 *   only through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - post_growth_policy_coalition: sets the agenda for this reading (organized/analytical) — drafts the transformation but lacks enforcement power
 *   - global_south_development_claimants: primary intended beneficiary (moderate/constrained) — receives redistributed throughput budget
 *   - future_generations: primary intended beneficiary (powerless/trapped) — inherits the biophysical trajectory, has no voice
 *   - global_north_high_consumption_households: primary payer (moderate/constrained) — mandated consumption reduction
 *   - fossil_and_extractive_capital_holders: primary payer (powerful/mobile) — asset devaluation, can resist via capital mobility
 *   - gdp_dependent_national_treasuries: institutional payer (institutional/trapped) — fiscal architecture threatened, largely excluded from design
 *   - export_dependent_developing_economy_workers: caught between payer and beneficiary roles (powerless/trapped) — employment risk precedes redistribution benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.68).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.58).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Degrowth Transformation Reading of Climate Response").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, '2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595').
narrative_ontology:cs_kernel_codification('2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595', distributed).
narrative_ontology:cs_authority_grounding('2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595', distributed).
narrative_ontology:cs_reading_relation('2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595', climate_response_action__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595', foundational, growth_is_not_the_organizing_principle).
narrative_ontology:cs_axiom_status(growth_is_not_the_organizing_principle, holdable).
narrative_ontology:cs_axiom_grounding('2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595', growth_is_not_the_organizing_principle, empirically_contingent).
narrative_ontology:cs_axiom('2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595', foundational, intergenerational_and_north_south_equity_precede_efficiency).
narrative_ontology:cs_axiom_status(intergenerational_and_north_south_equity_precede_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595', intergenerational_and_north_south_equity_precede_efficiency, deontological).
narrative_ontology:cs_created_at('2a09d67b-8c4b-4cc1-8ac5-e4fb60df8595', '').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_south_development_claimants).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, ecologically_precarious_communities).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, post_growth_policy_coalition).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, global_north_high_consumption_households).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, fossil_and_extractive_capital_holders).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, gdp_dependent_national_treasuries).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, export_dependent_developing_economy_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, export_dependent_developing_economy_workers).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, planetary_boundaries_thesis).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, throughput_decoupling_skepticism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academics, NGOs, and a minority of sub-national governments who advocate restructuring national accounts and firm ownership rules around sufficiency and universal basic services rather than GDP maximization. They set the agenda for this reading — drafting model legislation, staffing degrowth-aligned ministries in a handful of jurisdictions — but hold no enforcement power at the scale the transformation requires; their leverage is persuasive and electoral, not coercive.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, post_growth_policy_coalition, agenda_setter,
    organized, generational, analytical, global).

% Governments and civil society coalitions in lower-income, historically low-emitting countries who would receive expanded atmospheric and resource budget under a degrowth reallocation, plus direct transfers and technology access currently withheld under a growth-maintenance framework. They cannot compel this redistribution themselves; their gain depends entirely on Global North political will shifting.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_development_claimants, beneficiary,
    moderate, generational, constrained, global).

% People not yet born who inherit whatever biophysical and institutional trajectory current policy sets. Under this reading they are the primary intended beneficiary of reduced throughput; they have no vote, no representation, and no capacity to negotiate the terms of the transfer being made on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Middle- and upper-income households in wealthy countries whose consumption levels this reading targets directly through working-time reduction, resource caps, and redistribution of consumption headroom to the Global South. They experience the transformation as a mandated reduction in material living standard and autonomy over consumption choices, with limited individual exit short of emigration.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_north_high_consumption_households, payer,
    moderate, biographical, constrained, national).

% Owners of fossil fuel reserves, extractive infrastructure, and growth-dependent financial assets whose asset values this reading treats as illegitimate claims on a shrinking throughput budget. They have substantial capital mobility and lobbying capacity to resist, delay, or redirect the transformation, unlike the households above.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, fossil_and_extractive_capital_holders, payer,
    powerful, biographical, mobile, global).

% Finance ministries and central banks whose debt servicing, pension obligations, and tax base are structured around continuous GDP growth. Abandoning growth as an organizing principle directly threatens fiscal solvency models they did not choose and cannot easily unwind; their institutional voice in this debate is largely absent from degrowth policy design despite bearing acute transition costs.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, gdp_dependent_national_treasuries, payer,
    institutional, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, gdp_dependent_national_treasuries, excluded).

% Workers in manufacturing and resource-export sectors in middle-income countries whose employment depends on Global North consumption demand. A sufficiency-driven contraction in Northern consumption could remove their income source before Southern development transfers or new employment structures materialize, even though they belong to the broad category the reading intends to benefit.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, export_dependent_developing_economy_workers, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, export_dependent_developing_economy_workers, beneficiary).

% The sibling reading of the same kernel that holds temperature targets can be met through decarbonization technology and carbon markets while preserving growth. Its advocates are not absent from the broader climate conversation but are structurally excluded from THIS reading's policy design, which treats growth-preservation as incompatible with adequate throughput reduction.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, mitigation_priority_reading, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__degrowth_transformation, diffuse).
narrative_ontology:fixing_cost_class(climate_response_action__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a deliberate, planned contraction and redistribution of material and energy throughput across countries and income classes, intended to keep cumulative emissions and resource extraction within planetary boundaries without relying on unproven negative-emissions technology to do the work growth-preservation would otherwise require.
% TRANSFER_FUNCTION: Moves consumption headroom, atmospheric budget, and resource claims from current high-consuming Global North populations and growth-dependent capital toward Global South development claims and toward future generations, financed by working-time reduction, wealth-based rationing, and reallocation of currently GDP-counted activity toward non-market provisioning (care, ecological restoration, universal basic services).
% ABSENT_VOICES: Export-dependent developing-economy workers and GDP-dependent treasury technocrats are named in the redistribution's intended beneficiary class or bear its fiscal shock, respectively, but neither group has meaningfully shaped the policy architecture; the mitigation_priority reading's advocates are excluded by design since their premise (growth-compatible decarbonization) is treated as insufficient rather than debated on its merits within this reading's own framework.
% DISAPPEARANCE_RATIONALE: If the degrowth reading vanished as a live political claim, the growth-preserving mitigation framework and adaptation-triage framework would occupy the entire policy space uncontested; carbon budgets would be allocated through market and technology mechanisms alone, Global South development claims would lose their strongest redistributive framing, and future-generations' interests would be represented only through discount-rate assumptions in cost-benefit mitigation models rather than through direct throughput constraints.
% FOUNDING_PROBLEM: Standard growth-plus-decarbonization pathways were judged, by this reading's proponents, structurally incapable of meeting the remaining carbon budget in time, because they rely on continuous efficiency improvements outpacing rebound effects and on speculative-scale carbon removal that does not yet exist at the required scale — while also leaving historical emitters' consumption levels and Global South development deficits untouched.
% FOUNDING_PROBLEM_CORROBORATION: Degrowth economists and a subset of IPCC working-group contributors attest the decoupling-at-scale premise underlying growth-compatible mitigation remains empirically unproven, citing historical throughput-emissions correlation data. Mainstream economic institutions, mitigation-priority advocates, and most G20 treasury departments dispute the founding problem's severity, arguing decoupling evidence is improving and that degrowth's political infeasibility makes it a non-solution regardless of its diagnosis; no unaffiliated cross-institutional body has adjudicated between these corroborating and disputing sources.
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__degrowth_transformation, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 by interval end because the reading requires a genuine, mandated transfer of consumption capacity and resource claims away from named payers (Global North households, fossil capital, treasuries) rather than a voluntary reallocation — this is what the tangled_rope classification's asymmetric-extraction gate requires. Suppression is set lower than extractiveness (0.58) because, unlike an established extractive institution, this reading has not yet built durable enforcement infrastructure anywhere at national scale; what suppression exists is concentrated in the handful of jurisdictions experimenting with working-time or consumption caps, and is rising as the coalition's political footprint grows, not because coercion has hardened uniformly. Resistance is authored high (0.81) because fossil capital, growth-dependent treasuries, and consumption-habituated households have strong incentive and capacity to resist a redistribution that directly reduces their claims — this is the mark of a genuinely contested transfer, not a settled arrangement. Accessibility collapse is moderate (0.42): growth-preserving alternatives (mitigation_priority, adaptation_priority) remain fully available as competing policy paths, so alternatives have not collapsed the way they would under an entrenched extractive constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the post_growth_policy_coalition's seat, this is coordination in the purest form: a rational response to a shrinking planetary budget that no other reading adequately addresses. From the seat of global_north_high_consumption_households or fossil_and_extractive_capital_holders, the same structure computes as extraction — a mandated reduction in living standard or asset value imposed without their consent, justified by a claim (throughput decoupling skepticism) they may not accept. The engine computes both seats' types from the same structural data; the divergence is expected and is the point of a tangled_rope classification rather than a defect to be smoothed over.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South development claimants and future generations are declared beneficiaries and sit near the low end of directionality because the reading's entire redistributive logic routes throughput and consumption headroom toward them, even though neither group holds power to enforce the transfer themselves — their low d comes from the structural benefit, not from agency. Global North high-consumption households, fossil capital holders, and GDP-dependent treasuries are declared victims/payers and sit near the high end of directionality because the reading's mechanism (working-time reduction, consumption rationing, abandonment of growth-linked fiscal design) extracts directly from their current claims. Export-dependent developing-economy workers get a dual role deliberately: they are nominally inside the beneficiary class (Global South) but structurally exposed as payers in the near term, since the redistribution mechanism that would eventually benefit them removes their current income source before compensating transfers arrive — this sequencing risk is real and is why they carry secondary_role beneficiary rather than pure beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that growth-compatible decarbonization cannot meet the remaining carbon budget without relying on unproven large-scale carbon removal — is authored as contested rather than resolved, precisely because both sides (degrowth proponents and mainstream/mitigation economists) dispute whether decoupling evidence is improving fast enough. This keeps the classification honest: if the founding problem were later shown clearly dead (decoupling proven robust at required scale) while the degrowth apparatus persisted and continued extracting from the same payer set, that mismatch (status=dead, verdict=world_rearranges) would flag a capture pattern. As authored now, the founding problem remains live by the reading's own lights and corroborated partially from outside its own advocates (a subset of IPCC contributors), so no mandatrophy is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    throughput_decoupling_empirical_status,
    'Can GDP growth be sufficiently decoupled from resource throughput and emissions at the pace and scale required to meet remaining carbon budgets, making the degrowth reading''s founding premise obsolete?',
    'Longitudinal cross-national data on absolute (not merely relative) decoupling rates, compared against required emissions trajectories; resolvable in principle over the next one to two decades as more high-income economies attempt simultaneous growth and absolute emissions decline.',
    'If robust absolute decoupling is demonstrated at the required scale, the degrowth reading''s founding problem becomes substantially weaker and the constraint would trend toward founding_problem_status: dead while political apparatus built around it would risk a mandatrophy flag if it persisted unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(throughput_decoupling_empirical_status, empirical, 'Whether decoupling evidence resolves or sustains the degrowth reading''s core empirical premise.').

omega_variable(
    sequencing_risk_for_intended_beneficiaries,
    'Do export-dependent developing-economy workers experience net harm before compensating Global South development transfers materialize, given plausible political and administrative lag in implementing redistribution?',
    'Modeling and, where pilot programs exist, empirical tracking of transition timelines between Global North consumption contraction and Global South transfer disbursement.',
    'If lag is long and transfers are politically fragile, a nominally intended beneficiary group functions as a near-term victim, which would argue for reclassifying export_dependent_developing_economy_workers with a higher directionality value via override rather than relying on the derived beneficiary-leaning value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sequencing_risk_for_intended_beneficiaries, empirical, 'Whether the redistribution''s sequencing imposes real near-term costs on a nominal beneficiary class.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice to treat mitigation_priority, adaptation_priority, and degrowth_transformation as mutually exclusive competing readings correct, or could a hybrid framework combine elements of all three without instantiating a fourth distinct constraint?',
    'Policy analysis of jurisdictions that formally blend degrowth-adjacent measures (e.g. working-time reduction) with growth-preserving decarbonization investment and adaptation infrastructure, to see whether the blend behaves structurally as one of the three readings or as a genuinely novel fourth kernel reading.',
    'If hybrids are common and stable, the three-reading decomposition may be incomplete and a fourth story may be warranted per the ε-invariance principle rather than treating hybrids as noisy instances of one of the three.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the three-way kernel decomposition exhaustively covers observed policy hybrids.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__degrowth_transformation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t5, climate_response_action__degrowth_transformation, theater_ratio, 5, 0.12).
narrative_ontology:measurement(clim_tr_t10, climate_response_action__degrowth_transformation, theater_ratio, 10, 0.14).
narrative_ontology:measurement(clim_tr_t15, climate_response_action__degrowth_transformation, theater_ratio, 15, 0.16).
narrative_ontology:measurement(clim_tr_t20, climate_response_action__degrowth_transformation, theater_ratio, 20, 0.18).
narrative_ontology:measurement(clim_tr_t25, climate_response_action__degrowth_transformation, theater_ratio, 25, 0.2).
narrative_ontology:measurement(clim_tr_t30, climate_response_action__degrowth_transformation, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__degrowth_transformation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t5, climate_response_action__degrowth_transformation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clim_be_t10, climate_response_action__degrowth_transformation, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t15, climate_response_action__degrowth_transformation, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(clim_be_t20, climate_response_action__degrowth_transformation, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(clim_be_t25, climate_response_action__degrowth_transformation, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(clim_be_t30, climate_response_action__degrowth_transformation, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__degrowth_transformation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t5, climate_response_action__degrowth_transformation, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(clim_su_t10, climate_response_action__degrowth_transformation, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(clim_su_t15, climate_response_action__degrowth_transformation, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(clim_su_t20, climate_response_action__degrowth_transformation, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(clim_su_t25, climate_response_action__degrowth_transformation, suppression_requirement, 25, 0.56).
narrative_ontology:measurement(clim_su_t30, climate_response_action__degrowth_transformation, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).

% DUAL FORMULATION NOTE:
% This story is one of three linked members of the climate_response_action constraint family, decomposed per the ε-invariance principle because the natural-language label 'climate response' conflates structurally distinct claims about mechanism, beneficiary structure, and burden allocation. mitigation_priority (growth-preserving decarbonization via technology and carbon markets) and adaptation_priority (accepting warming, investing in resilience) are separate constraint files with their own ε values and stakeholder sets. This file's ε (0.68) reflects the mandated transfer this reading's mechanism requires; it should not be averaged or reconciled with the siblings' ε values — each reading is evaluated on its own structural terms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
