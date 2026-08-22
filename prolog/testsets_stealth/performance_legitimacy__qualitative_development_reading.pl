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
 *   human_readable: High-Quality Development Legitimacy Mandate (Structural Transformation Reading)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   A developmental state re-founds its performance legitimacy on structural
 *   transformation rather than aggregate expansion: cadre evaluations are
 *   rewritten around innovation, environmental, and debt indicators; credit
 *   is steered from property toward designated strategic sectors; regulatory
 *   campaigns enforce the pivot against property leverage, speculative
 *   platforms, and low-end overcapacity. The arrangement solves a real
 *   coordination problem — long-horizon innovation and a system-wide fiscal
 *   transition that no locality could attempt alone — while imposing
 *   concentrated costs on the actors who prospered under the previous
 *   standard. This file instantiates ONE reading of the
 *   performance_legitimacy kernel, the qualitative_development_reading, as a
 *   single epsilon-invariant constraint: the standing arrangement under
 *   contest is the transformation mandate itself, assessed as it operates.
 *   Sibling readings (aggregate-growth, techno-nationalist,
 *   livelihood-security) are separate constraint files with their own
 *   beneficiary/victim structures and epsilon values; the contest between
 *   readings is routed to omega variables and the kernel_context note, never
 *   averaged into this file's metrics. Family members are linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - central_planning_authority: Agenda-setter (institutional/arbitrage) — defines the transformation standard and enforces it through cadre evaluation and credit allocation
 *   - state_innovation_apparatus: Primary beneficiary (institutional/identity_locked) — administers the mandate and receives the redirected resources; institutionally fused with the mission
 *   - strategic_high_tech_firms: Secondary beneficiary (powerful/constrained) — collects subsidized capital and protected demand conditional on milestones
 *   - property_dependent_local_governments: Primary target (organized/trapped) — lost the fiscal engine the center itself once authorized, must now fund the transition
 *   - traditional_manufacturers: Target (moderate/mobile) — bear credit discrimination and escalating compliance costs; partial exit via offshoring
 *   - displaced_industrial_workers: Target (powerless/trapped) — bear the employment shock with the fewest buffers
 *   - urban_middle_class_households: Dual-positioned (moderate/constrained) — amenity and employment gains against housing-wealth losses and captive savings
 *   - foreign_technology_suppliers: Excluded party (powerful/arbitrage) — substituted out of the market they formerly served
 *   - independent_policy_analysts: Analytical observer (analytical/analytical) — outside check on officially reported transformation metrics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.59).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.59).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "High-Quality Development Legitimacy Mandate (Structural Transformation Reading)").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, 'ff6763d4-008a-46f5-9df2-38b3040e3679').
narrative_ontology:cs_kernel_codification('ff6763d4-008a-46f5-9df2-38b3040e3679', formalized).
narrative_ontology:cs_authority_grounding('ff6763d4-008a-46f5-9df2-38b3040e3679', practice).
narrative_ontology:cs_interpretation_layer_present('ff6763d4-008a-46f5-9df2-38b3040e3679').
narrative_ontology:cs_reading_relation('ff6763d4-008a-46f5-9df2-38b3040e3679', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('ff6763d4-008a-46f5-9df2-38b3040e3679', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('ff6763d4-008a-46f5-9df2-38b3040e3679', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('ff6763d4-008a-46f5-9df2-38b3040e3679', foundational, legitimacy_through_structural_transformation).
narrative_ontology:cs_axiom_status(legitimacy_through_structural_transformation, holdable).
narrative_ontology:cs_axiom_grounding('ff6763d4-008a-46f5-9df2-38b3040e3679', legitimacy_through_structural_transformation, instrumental).
narrative_ontology:cs_axiom('ff6763d4-008a-46f5-9df2-38b3040e3679', secondary, ecological_ceiling_binding_on_development).
narrative_ontology:cs_axiom_status(ecological_ceiling_binding_on_development, holdable).
narrative_ontology:cs_axiom_grounding('ff6763d4-008a-46f5-9df2-38b3040e3679', ecological_ceiling_binding_on_development, empirically_contingent).
narrative_ontology:cs_reference_frame('ff6763d4-008a-46f5-9df2-38b3040e3679', structural_transformation_standard).
narrative_ontology:cs_drift_state('ff6763d4-008a-46f5-9df2-38b3040e3679', contemporary_post_property_correction, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff6763d4-008a-46f5-9df2-38b3040e3679', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_innovation_apparatus).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, strategic_high_tech_firms).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturers).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, displaced_industrial_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, urban_middle_class_households).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, urban_middle_class_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the national development agenda through five-year plans and central economic work conferences. Rewrote cadre evaluation weights away from regional output totals toward research intensity, environmental targets, and debt containment. Directs the largest state banks' lending priorities and decides which sectors receive patient credit. Its own standing now depends on demonstrating that the transformation is real, which makes it the most committed defender of the standard it created.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_planning_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% National laboratories, ministry research programs, and state-guided investment funds that administer the innovation mandate. Receives multi-year funding commitments, talent-recruitment budgets, and first call on directed credit. Staffed by officials and institute directors whose careers and institutional identities are bound to the mission's continuation; abandoning the mission would dissolve the institutions themselves, so exit is unthinkable from inside.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_innovation_apparatus, beneficiary,
    institutional, generational, identity_locked, national).

% Champions in semiconductors, electric vehicles, batteries, and solar equipment. Receive subsidized credit, protected procurement, land grants, and regulatory forbearance conditioned on hitting technology milestones. Their supply chains, order books, and listing venues increasingly depend on continued designation; walking away from designated status would forfeit the support their capital intensity requires.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, strategic_high_tech_firms, beneficiary,
    powerful, biographical, constrained, global).

% Provincial and municipal governments that financed themselves for two decades through land-lease sales to developers. Now face falling land revenue, hard caps on off-book borrowing, and evaluation criteria that reward innovation parks and environmental compliance they must fund from shrinking balances. They cannot resume the old fiscal engine because the center actively suppresses it, and they cannot default without triggering a crisis they would personally answer for.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    organized, biographical, trapped, regional).

% Low-margin exporters in furniture, apparel, assembly, and basic components. Face rising labor and environmental compliance costs, tighter credit than designated sectors receive, and steady pressure to automate, relocate inland, or move production to Southeast Asia. Some upgrade successfully; many relocate or close. Access to bank credit now depends on fitting categories the plan recognizes.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturers, payer,
    moderate, biographical, mobile, global).

% Assembly-line and construction workers in regions losing property and low-end manufacturing employment. Receive retraining vouchers and social-insurance transfers of uneven quality. Their skills are location- and age-specific; moving to hub cities means unaffordable housing, so most stay and absorb the adjustment as informal work and reduced income.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, displaced_industrial_workers, payer,
    powerless, immediate, trapped, regional).

% Salariat households in major cities. Gain cleaner air, metro expansion, and prestige employment in technology sectors; lost paper wealth as apartment prices flattened and some pre-sold projects stalled. Household savings sit largely in state banks whose lending follows plan priorities rather than depositors' preferences, so their savings finance the redirection whether or not they approve of it.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, urban_middle_class_households, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, urban_middle_class_households, payer).

% Foreign equipment, software, and chip vendors that originally supplied the upgrade drive. Substitution targets and procurement preferences progressively exclude them from segments they once dominated. They retain other global markets but have no seat in the domestic planning conversation that is reallocating their former customers.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, foreign_technology_suppliers, excluded,
    powerful, biographical, arbitrage, global).

% Economists in universities, think tanks, and international institutions who track total-factor productivity, patent quality, and local-debt sustainability. Publish within limits; several lines of measurement critical of allocation efficiency circulate mainly in restricted channels. Their assessments are the principal outside check on officially reported transformation metrics.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, independent_policy_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, state_innovation_apparatus).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates credit, talent, procurement, and research infrastructure on activities with long payoffs and learning spillovers that dispersed capital markets underfund; coordinates a simultaneous fiscal transition away from land finance across hundreds of local governments; internalizes environmental costs that individual jurisdictions would otherwise race to avoid bearing alone.
% TRANSFER_FUNCTION: Moves bank credit, fiscal subsidies, land quotas, and university talent from property development and low-margin export manufacturing toward designated strategic sectors; moves career advancement inside the bureaucracy toward officials delivering innovation and environmental indicators; moves household deposits, through the state banking channel, into directed lending at administratively set rates.
% ABSENT_VOICES: Traditional manufacturers had consultative channels during the export boom that thinned as evaluation criteria shifted; displaced workers hold no formal seat and register position through localized disputes rather than agenda access; foreign suppliers are entirely outside the conversation; heterodox economists who question state allocation itself publish under constraint. Apparent consensus about the transformation's desirability reflects who holds agenda access, not consent among those bearing its costs.
% DISAPPEARANCE_RATIONALE: If the transformation mandate and its enforcement vanished overnight, cadre incentives would revert to measurable regional output, state credit would flow back to property and infrastructure, land finance would resume wherever land still has buyers, and the innovation apparatus would contract to grant maintenance. The property-and-local-government coalition that anchored the previous era would reconstitute within a few budget cycles.
% FOUNDING_PROBLEM: A catch-up growth model built on cheap labor, heavy investment, and property-led local finance was visibly exhausting itself: returns on infrastructure spending falling, the demographic dividend closing, environmental damage compounding, and core technologies — above all advanced semiconductors — sourced from potential adversaries. The arrangement was built to answer: what sustains legitimacy and security once convergence growth ends?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: multilateral growth-accounting work documenting diminishing returns to the old model, published demographic projections showing workforce contraction, trade statistics quantifying semiconductor import dependence, and the observable growth slowdown itself. The corroboration covers the problem's reality, not the chosen remedy — analysts outside the state sector dispute whether administrative allocation is an efficient answer to it.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
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
 *   Extraction is high (0.68 at interval end) because the directed transfers are large and receipt is imperfectly coupled to verified performance: subsidy and credit flows continue to designated sectors even where milestone verification is weak, while the paying seats — land-finance-starved localities, credit-rationed manufacturers, displaced workers — receive no compensating claim on the upside. Suppression (0.59) is administrative rather than overtly coercive: KPI rewriting, credit denial, inspection campaigns, and personnel consequences; the temporal series documents deliberate enforcement-capacity change — a sharp build-up through the 2021 campaign peak, then routinization at a plateau — which is why suppression_requirement is tracked on the shared grid rather than left static. Theater (0.47) reflects documented metric gaming: patent-count inflation, relabeled high-tech enterprise certifications, showcase facilities built to satisfy evaluation rather than production; roughly half of measurable innovation activity responds to the indicator rather than the mission. Accessibility_collapse (0.52): alternatives persist — offshoring, the informal economy, residual property activity — but the sanctioned path narrows decisively once actors understand the mandate. Resistance (0.48): quiet noncompliance by localities, capital relocation by manufacturers, household consumption retrenchment; open resistance is rare because agenda control is centralized. All three series run on one shared nine-point annual grid (2017-2025) so every metric is authored at every examined time point; endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute different classifications from identical structural facts. From the property-dependent local governments' position, the mandate is an unfunded liability swap: the center authorized land finance for two decades, then repriced it as a violation while demanding innovation spending from the wreckage. From the state innovation apparatus's position, the same arrangement is overdue patient-capital coordination that markets refused to provide. From the planning center's position, it is existential necessity. Traditional manufacturers split internally by mobility: the relocatable experience friction, the immobile experience something closer to confiscation-by-regulation. The engine derives these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: the innovation apparatus receives first call on resources (d near 0.0, amplified by identity lock — it cannot exit without dissolving itself), and strategic firms receive protected demand (d low, dampened further by their constrained-but-real option to internationalize). Urban households sit mildly beneficiary with an offsetting payer position the dual role records. Targets sit near the full-target end: local governments are trapped (no exit amplifies their effective burden), workers are trapped and powerless, and manufacturers — though genuinely targeted — carry mobile exit that damps their effective extraction below what their treatment alone would imply. Foreign suppliers are excluded rather than coordinated; their exclusion is the enforcement object on the substitution front and sits outside the d computation proper.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sustaining legitimacy and security after convergence growth ends — is live, so the mandate has not outlived its function and mandatrophy is not resolved. The live risk is Goodhart accumulation rather than obsolescence: the theater series rises monotonically across the interval, and if measured transformation fully substitutes for real transformation, the arrangement drifts toward administered performance maintained by evaluation machinery alone. The status-live x world-rearranges pairing is internally consistent today (no zombie flag), and the metric-gaming omega is the designated tripwire: resolution showing a dominant gaming share would force either metric reform or a credibility-driven reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_metric_selection_ambiguity,
    'Which dimension of performance legitimately grounds authority — structural transformation, aggregate growth, technological self-sufficiency, or daily-life security? This constraint exists only under the transformation answer; the kernel''s other readings instantiate different constraints.',
    'Observe the binding constraints in practice: what cadres are actually promoted or dismissed for, what credit stops flowing for, what the center tolerates failing. Revealed enforcement reveals which reading is operative regardless of stated doctrine.',
    'If a sibling reading becomes operative, beneficiaries and victims reassign wholesale and epsilon moves — a livelihood-security reversion would redirect transfers toward welfare delivery and convert the innovation apparatus from beneficiary to payer; a growth reversion would rehabilitate the property coalition this reading suppresses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_metric_selection_ambiguity, conceptual, 'Kernel-level ambiguity over what counts as deliverable performance; this story is one reading of four.').

omega_variable(
    transformation_output_vs_metric_gaming,
    'What fraction of measured innovation output represents real technological capability versus metric-responsive activity (patent inflation, certification arbitrage, showcase construction)?',
    'Citation-weighted patent-quality audits, firm-level total-factor-productivity studies, and third-party verification of designated-enterprise qualifications against actual production.',
    'A high gaming share raises theater_ratio further, accelerates drift toward administered performance, and forces eventual metric reform or credibility collapse; a low share confirms the coordination function is substantive and stabilizes the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_output_vs_metric_gaming, empirical, 'Real-versus-performed composition of the transformation the mandate measures.').

omega_variable(
    transition_cost_permanence,
    'Are the costs borne by property-chain actors, traditional manufacturers, and displaced workers a one-time structural adjustment or a recurring extraction stream?',
    'Longitudinal regional income and employment tracking, plus fiscal-transfer data for localities that lost land revenue: do affected cohorts and regions converge back, or does the differential persist?',
    'Permanence supports reclassification toward pure-extraction dynamics with the innovation apparatus as capturer; transience confirms the costs are the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_permanence, empirical, 'Whether payer-seat costs are transitional or structural.').

omega_variable(
    slowdown_tolerance_durability,
    'Can the transformation-based legitimacy frame survive a prolonged period of below-trend growth, or does sustained underdelivery automatically re-import the quantitative growth reading?',
    'Observe target-setting language, personnel decisions, and credit allocation after consecutive growth misses: does the center double down on transformation metrics or quietly restore output targets?',
    'Reversion would restore growth-reading beneficiaries (the property-local-government coalition), invert this file''s victim structure, and date a reading-level transition in the kernel family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slowdown_tolerance_durability, conceptual, 'Durability of the reading''s legitimacy claim under adverse delivery conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 2017, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2017, performance_legitimacy__qualitative_development_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement(perf_tr_t2018, performance_legitimacy__qualitative_development_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(perf_tr_t2019, performance_legitimacy__qualitative_development_reading, theater_ratio, 2019, 0.31).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__qualitative_development_reading, theater_ratio, 2020, 0.34).
narrative_ontology:measurement(perf_tr_t2021, performance_legitimacy__qualitative_development_reading, theater_ratio, 2021, 0.39).
narrative_ontology:measurement(perf_tr_t2022, performance_legitimacy__qualitative_development_reading, theater_ratio, 2022, 0.42).
narrative_ontology:measurement(perf_tr_t2023, performance_legitimacy__qualitative_development_reading, theater_ratio, 2023, 0.44).
narrative_ontology:measurement(perf_tr_t2024, performance_legitimacy__qualitative_development_reading, theater_ratio, 2024, 0.46).
narrative_ontology:measurement(perf_tr_t2025, performance_legitimacy__qualitative_development_reading, theater_ratio, 2025, 0.47).

% Extraction over time
narrative_ontology:measurement(perf_be_t2017, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2017, 0.44).
narrative_ontology:measurement(perf_be_t2018, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2018, 0.47).
narrative_ontology:measurement(perf_be_t2019, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2019, 0.51).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2020, 0.54).
narrative_ontology:measurement(perf_be_t2021, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2021, 0.61).
narrative_ontology:measurement(perf_be_t2022, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement(perf_be_t2023, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2023, 0.66).
narrative_ontology:measurement(perf_be_t2024, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2024, 0.67).
narrative_ontology:measurement(perf_be_t2025, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2017, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2017, 0.36).
narrative_ontology:measurement(perf_su_t2018, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2018, 0.41).
narrative_ontology:measurement(perf_su_t2019, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2020, 0.49).
narrative_ontology:measurement(perf_su_t2021, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2021, 0.57).
narrative_ontology:measurement(perf_su_t2022, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2022, 0.6).
narrative_ontology:measurement(perf_su_t2023, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2023, 0.61).
narrative_ontology:measurement(perf_su_t2024, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement(perf_su_t2025, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2025, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% Performance legitimacy decomposes into four structurally distinct constraints — one per reading of what counts as deliverable performance. The aggregate-growth, self-sufficiency, daily-livelihood, and structural-transformation readings carry different beneficiary/victim sets and different epsilon values; merging them yields an observable-dependent epsilon in violation of epsilon-invariance, so each is a separate story. This file is the transformation reading, currently dominant in official target-setting, and therefore exerts structural pressure on the growth and self-sufficiency siblings' operating environments (softened output targets; securitized innovation budgets) while coexisting with the livelihood reading as a rival held by different factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
