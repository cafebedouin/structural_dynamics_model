% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation Reading of Climate Response Legitimacy
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   A political-economic program holds that no climate response is legitimate
 *   unless it dismantles the growth imperative in wealthy nations through
 *   universal basic services, working-time reduction, and democratized firm
 *   ownership. The program reallocates real income and capital returns from
 *   current developed-world populations toward reduced cumulative emissions,
 *   whose benefits accrue mostly to future generations and the global south.
 *   The coordination function (staying within a shrinking global carbon
 *   budget without relying on unproven negative-emissions technology) is
 *   genuine; the extraction (concentrated present-day income and capital
 *   costs imposed on wage earners, shareholders, and pensioners who did not
 *   choose the framework and often cannot exit it) is also genuine and
 *   asymmetric across the payer seats.
 *
 * KEY AGENTS:
 *   - current_developed_world_wage_earners: primary cost-bearer (moderate/constrained) — absorbs income and structural disruption
 *   - capital_owning_shareholders: primary cost-bearer with high exit (powerful/mobile) — can relocate capital to avoid transformation
 *   - growth_dependent_pension_holders: trapped cost-bearer (moderate/trapped) — locked into growth-dependent commitments
 *   - future_generations: primary beneficiary, no voice (powerless/trapped) — benefits without agency
 *   - global_south_populations: primary beneficiary, no voice (powerless/trapped) — benefits without a policy seat
 *   - degrowth_policy_coalition: agenda setter (organized/analytical) — authors and advocates the framework without bearing its costs
 *   - climate_science_and_policy_analysts: analytical observer (analytical/analytical) — assesses feasibility and equity across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.61).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.52).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.61).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation Reading of Climate Response Legitimacy").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '060eb03c-d091-4d55-b9b6-28cde34b2b14').
narrative_ontology:cs_kernel_codification('060eb03c-d091-4d55-b9b6-28cde34b2b14', distributed).
narrative_ontology:cs_authority_grounding('060eb03c-d091-4d55-b9b6-28cde34b2b14', distributed).
narrative_ontology:cs_reading_relation('060eb03c-d091-4d55-b9b6-28cde34b2b14', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('060eb03c-d091-4d55-b9b6-28cde34b2b14', climate_response_legitimacy__adaptation_priority, influences).
narrative_ontology:cs_axiom('060eb03c-d091-4d55-b9b6-28cde34b2b14', foundational, growth_imperative_incompatible_with_carbon_budget).
narrative_ontology:cs_axiom_status(growth_imperative_incompatible_with_carbon_budget, holdable).
narrative_ontology:cs_axiom_grounding('060eb03c-d091-4d55-b9b6-28cde34b2b14', growth_imperative_incompatible_with_carbon_budget, empirically_contingent).
narrative_ontology:cs_axiom('060eb03c-d091-4d55-b9b6-28cde34b2b14', foundational, present_generation_owes_structural_sacrifice_to_future).
narrative_ontology:cs_axiom_status(present_generation_owes_structural_sacrifice_to_future, holdable).
narrative_ontology:cs_axiom_grounding('060eb03c-d091-4d55-b9b6-28cde34b2b14', present_generation_owes_structural_sacrifice_to_future, deontological).
narrative_ontology:cs_reference_frame('060eb03c-d091-4d55-b9b6-28cde34b2b14', postwar_growth_consensus).
narrative_ontology:cs_drift_state('060eb03c-d091-4d55-b9b6-28cde34b2b14', post_paris_agreement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('060eb03c-d091-4d55-b9b6-28cde34b2b14', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, ecosystem_stability_interests).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, current_developed_world_wage_earners).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, capital_owning_shareholders).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, growth_dependent_pension_holders).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, growth_imperative_is_not_natural_law).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, intergenerational_equity_requires_present_sacrifice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face proposed working-time reduction and restructuring of firms toward democratic ownership models, alongside a shift away from GDP-growth-linked wage bargaining. Some gain leisure and services (universal basic services offset income loss) but many experience real income compression in transition, with no individual exit from a nationally legislated transformation short of emigration.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, current_developed_world_wage_earners, payer,
    moderate, biographical, constrained, national).

% Hold equity and ownership stakes threatened by mandated shifts to democratic firm governance and reduced growth expectations, which compress return on capital. Can shift capital across jurisdictions to avoid the transformation, giving them far more effective exit than wage earners bound to national labor markets.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, capital_owning_shareholders, payer,
    powerful, biographical, mobile, global).

% Depend on pension funds whose actuarial assumptions and payout structures are built on continued economic growth and capital returns. A structural degrowth transition threatens fund solvency and payout levels; they are largely locked into commitments made decades earlier and cannot renegotiate their exposure.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, growth_dependent_pension_holders, payer,
    moderate, biographical, trapped, national).

% Stand to inherit a climate system with substantially reduced warming and less reliance on unproven negative-emissions technology if current growth imperatives are dismantled now. Have no voice, vote, or bargaining position in present decisions; their benefit is conditional entirely on choices current cost-bearers make on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Bear disproportionate climate impacts from wealthy-nation emissions while having contributed least to cumulative carbon budgets. Benefit if wealthy-nation degrowth reduces the remaining carbon budget consumed, but have no formal seat in wealthy-nation domestic economic policy debates that determine whether this transformation occurs.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Academics, movement organizations, and allied legislators who author and advocate the UBS/working-time-reduction/democratic-ownership platform, framing it as the only legitimate climate response. They administer the proposal's design and public case but do not themselves bear the income or capital losses the transformation would impose on others.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_policy_coalition, agenda_setter,
    organized, generational, analytical, national).

% Mainstream economic policy institutions and growth-dependent industrial interests who would object that decoupling and technological mitigation preserve welfare gains this reading forecloses, but operate largely outside the degrowth coalition's framing of what counts as a legitimate response and are treated by this reading as defenders of an illegitimate premise rather than a good-faith alternative.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, growth_economists_and_incumbent_industry, excluded,
    institutional, biographical, mobile, national).

% Assess carbon budgets, feasibility of decoupling, and social equity outcomes across all three legitimacy readings without being cost-bearers or direct beneficiaries themselves; their modeling determines how contested the empirical premises (decoupling limits, feasibility of low-growth welfare) actually are.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_science_and_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__degrowth_transformation, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a national-scale economic restructuring away from GDP growth as the organizing objective, replacing it with universal basic services, shorter working hours, and democratized firm ownership, in order to reduce material and energy throughput fast enough to stay within remaining carbon budgets without depending on unproven large-scale carbon removal.
% TRANSFER_FUNCTION: Moves income, capital returns, and consumption capacity from current wage earners, shareholders, and pension holders in wealthy nations toward reduced aggregate emissions whose benefit accrues to future generations and to populations in less-developed, more climate-vulnerable regions.
% ABSENT_VOICES: Future generations and global south populations who stand to benefit have no vote in the wealthy-nation legislatures deciding whether to adopt this transformation. Growth economists and industry incumbents who dispute the premise that growth and emissions cannot be decoupled are cast as illegitimate objectors within this reading's own framing rather than heard as a live empirical dispute.
% DISAPPEARANCE_RATIONALE: If this reading's transformation program vanished, the degrowth coalition insists remaining carbon budgets would be exceeded and future/global-south harms would compound; mainstream economists and industry dispute this, holding that decoupling and carbon pricing achieve equivalent emissions outcomes without the income and pension disruption — the parties dispute what the counterfactual world without this transformation actually looks like.
% FOUNDING_PROBLEM: Continued GDP growth in wealthy, high-consuming economies appears structurally incompatible with staying within remaining global carbon budgets absent speculative-scale negative emissions technology; this reading was built to solve that incompatibility by removing growth as a policy objective rather than betting on decoupling.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists and some IPCC working-group contributors outside the core degrowth advocacy movement corroborate that decoupling at the pace and scale required remains empirically unproven at a global level. However, mainstream growth economists and technology-sector modelers, also external to the degrowth coalition, corroborate the opposing claim that historical decoupling trends in several wealthy economies are accelerating — the founding problem's severity and the transformation's necessity are contested by credentialed outside parties on both sides, not settled by anyone external to advocacy.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, contested).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61 at interval end) reflects the real, non-trivial income and capital-return reduction imposed on current developed-world populations by design, not as a side effect — the program's own theory of change requires this transfer to reduce throughput. Suppression (0.52) is moderate: this reading requires legislative and social mobilization to override entrenched growth-oriented institutions and incentive structures, but does not (yet) rest on coercive enforcement comparable to authoritarian climate measures; it depends on democratic majorities being built and sustained. Resistance is high (0.82) because growth economists, industry, and much of the current electorate in wealthy democracies actively contest both the premise and the program. Accessibility collapse is moderate-low (0.38): alternative legitimacy framings (mitigation, adaptation) remain fully live and are being actively pursued by other political coalitions — this reading has not foreclosed them in practice, only in its own normative claim. Theater ratio (0.40) reflects that some implemented pilot programs (four-day-week trials, local UBS pilots) function partly as proof-of-concept demonstration rather than the full-scale transformation the reading claims is necessary, and that gap tends to widen as the political coalition matures without achieving full-scale enactment.
 *
 * PERSPECTIVAL GAP:
 *   From the degrowth_policy_coalition's agenda-setting seat, the program reads as coordination: a rational, necessary transformation to preserve a livable climate. From the wage-earner and pension-holder payer seats, the same structure reads as extraction: a mandated reduction in living standards imposed by a coalition that does not itself bear the costs. The engine should compute these divergently from the same structural data — the coalition's institutional/analytical exit position versus the payers' constrained/trapped exit position is the load-bearing asymmetry, not any difference in stated values.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and global south populations are declared beneficiaries with the lowest directionality — they receive the climate benefit without bearing the transformation's costs, and their powerless/trapped structural position (no vote, no exit, no seat) means the derivation correctly pushes their d toward full-beneficiary despite having no agency to secure that benefit. Current developed-world wage earners, capital owners, and pension holders are declared victims with directionality driven toward the target end, but their exit options differ sharply: shareholders have mobile capital and can arbitrage across jurisdictions (dampening their effective extraction relative to their nominal victim status), while wage earners and especially pension holders are constrained or trapped, concentrating effective extraction on the seats least able to escape it. This is the central seat-divergence the story is built to expose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (growth incompatible with remaining carbon budgets absent speculative-scale carbon removal) is authored as contested rather than resolved-dead or resolved-live, because credentialed analysts outside the advocacy coalition disagree about whether decoupling trends already underway resolve the incompatibility without requiring degrowth. This prevents the classification from either mislabeling the program as pure extraction (it does address a genuine, unresolved coordination problem under one empirically live reading of decoupling limits) or as pure coordination (it also imposes a real, uncompensated, asymmetric cost structure that the coalition itself does not bear, which is exactly the tangled_rope signature: real coordination function plus real asymmetric extraction, both present simultaneously).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_contested,
    'Can wealthy economies decouple GDP growth from emissions fast enough to meet remaining carbon budgets without dismantling the growth objective, or is decoupling at the required pace and scale empirically unachievable?',
    'Longitudinal tracking of absolute (not merely relative) decoupling rates in the highest-emitting wealthy economies against required emissions trajectories for 1.5C/2C budgets, adjudicated by climate-economics literature outside both the degrowth and mainstream-growth advocacy communities.',
    'If decoupling is empirically achievable at required scale, this reading''s foundational premise (growth imperative must be dismantled) loses its coordination justification and the constraint collapses toward pure extraction relative to the mitigation_priority sibling reading. If decoupling is empirically unachievable, the coordination function is vindicated and the extraction is better understood as a necessary transfer rather than a gratuitous one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_feasibility_contested, empirical, 'Whether absolute decoupling can resolve the founding problem without structural degrowth.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice among the three legitimacy readings (mitigation, degrowth, adaptation) a genuinely empirical dispute resolvable by climate and economic data, or an irreducible values dispute about acceptable intergenerational and intragenerational tradeoffs that no amount of data resolves?',
    'Track whether convergence occurs among analysts as decoupling and carbon-budget data accumulate over the next decade, versus persistent disagreement conditional on identical data — the latter would indicate the dispute is values-driven rather than empirical.',
    'If the dispute is empirical and resolves toward one reading, the sibling readings should be expected to lose adherents and political traction. If it is an irreducible values dispute, all three readings persist indefinitely as coexisting legitimate framings regardless of further data, which is the structural relationship this reading''s cs_structure declares toward its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether reading selection within the kernel is empirically resolvable or a standing values disagreement.').

omega_variable(
    political_feasibility_barrier,
    'Is the program''s low political feasibility in wealthy democracies (given that its cost-bearers are current voters and its beneficiaries cannot vote) a temporary implementation barrier that could shift with mobilization, or a structural feature that makes the reading permanently non-implementable regardless of its normative merits?',
    'Compare electoral outcomes and policy adoption rates in jurisdictions that have implemented partial elements (four-day week trials, universal basic services pilots) against the coalition''s mobilization efforts over a multi-decade horizon.',
    'If structurally non-implementable, the reading functions mainly as a normative critique of the other readings rather than an actionable alternative, which would push its practical classification toward symbolic/theatrical rather than operative — raising the theater_ratio finding''s weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_barrier, empirical, 'Whether the political feasibility barrier is temporary or structural to this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t6, climate_response_legitimacy__degrowth_transformation, theater_ratio, 6, 0.27).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__degrowth_transformation, theater_ratio, 12, 0.31).
narrative_ontology:measurement(clim_tr_t18, climate_response_legitimacy__degrowth_transformation, theater_ratio, 18, 0.34).
narrative_ontology:measurement(clim_tr_t24, climate_response_legitimacy__degrowth_transformation, theater_ratio, 24, 0.37).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__degrowth_transformation, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t6, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(clim_be_t18, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(clim_be_t24, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 30, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t6, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 6, 0.36).
narrative_ontology:measurement(clim_su_t12, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(clim_su_t18, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 18, 0.45).
narrative_ontology:measurement(clim_su_t24, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__degrowth_transformation, 0.12).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language claim 'legitimate climate response' per the ε-invariance principle. The mitigation_priority reading claims growth-preserving decoupling suffices for legitimacy (lower present-generation extraction, higher technological dependency risk). The adaptation_priority reading claims warming trajectory acceptance plus resilience investment suffices (redirects the cost-bearer set toward the currently vulnerable rather than wealthy-nation wage earners). Each reading has its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged into one observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
