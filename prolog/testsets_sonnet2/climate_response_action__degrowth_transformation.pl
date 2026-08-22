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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Degrowth Transformation Reading of Climate Response (GDP-Growth Rejection)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth_transformation reading of the
 *   contested climate_response_action kernel: the claim that adequate climate
 *   response requires abandoning GDP growth as the organizing metric of
 *   economic policy in favor of sufficiency, equity, and reduced material
 *   throughput, with the adjustment burden placed on current high-consumption
 *   populations rather than deferred to future generations or offshored to
 *   speculative carbon removal. As a reading it is authored on its own terms
 *   — it is not a hedge across the mitigation_priority or adaptation_priority
 *   readings, and its extraction figure describes the standing
 *   growth-oriented economic order as this reading's own advocates
 *   characterize it (ongoing throughput extraction from the biosphere and
 *   from the Global South, deferred onto the powerless and the future), not
 *   the sufficiency arrangement the reading would install. The sibling
 *   readings are separate constraints, evaluated in separate files, sharing
 *   only the kernel identity and the network link declared below.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.71).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.62).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.71).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Degrowth Transformation Reading of Climate Response (GDP-Growth Rejection)").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, '87cd96e2-70fc-4be4-bc9d-0fd91498e85e').
narrative_ontology:cs_kernel_codification('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', distributed).
narrative_ontology:cs_authority_grounding('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', distributed).
narrative_ontology:cs_reading_relation('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_axiom('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', foundational, growth_orientation_is_extractive_not_neutral).
narrative_ontology:cs_axiom_status(growth_orientation_is_extractive_not_neutral, holdable).
narrative_ontology:cs_axiom_grounding('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', growth_orientation_is_extractive_not_neutral, empirically_contingent).
narrative_ontology:cs_axiom('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', foundational, present_wealthy_populations_bear_adjustment_burden).
narrative_ontology:cs_axiom_status(present_wealthy_populations_bear_adjustment_burden, holdable).
narrative_ontology:cs_axiom_grounding('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', present_wealthy_populations_bear_adjustment_burden, deontological).
narrative_ontology:cs_reference_frame('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', growth_as_organizing_principle).
narrative_ontology:cs_drift_state('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', post_paris_agreement_decoupling_gap_recognition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('87cd96e2-70fc-4be4-bc9d-0fd91498e85e', '').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_south_development_claimants).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, ecological_systems_stewards).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, global_north_consumer_classes).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, fossil_capital_shareholders).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, growth_dependent_wage_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic economists, movement organizers, and a minority of sympathetic legislators who author the sufficiency-and-redistribution program: working time reduction, universal basic services, democratic firm ownership, caps on material throughput. They administer no state and enforce nothing directly yet — their leverage is discursive and electoral, seeking to displace GDP growth as the organizing metric of policy success.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, degrowth_policy_coalition, agenda_setter,
    organized, generational, identity_locked, global).

% Populations and governments in historically low-emitting nations who would receive an enlarged carbon and development budget under a framework that caps Global North consumption rather than allocating remaining emissions space proportionally to current GDP. Their situation improves structurally under this reading but they have limited power to compel Global North adoption of it; they remain dependent on negotiated transfers that have not yet materialized at scale.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_development_claimants, beneficiary,
    moderate, generational, constrained, global).

% Cannot participate in present negotiations. This reading is explicitly built to shift the burden of adjustment from them (via unproven future carbon removal and continued warming) onto currently wealthy populations now. They have no voice, no vote, and no mechanism to enforce the transfer that is nominally made on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Middle and working classes in high-consumption nations who would face reduced material throughput, working time restructuring, and constrained consumption patterns under this program. They did not individually design the growth-dependent economy they inherited, but the reading assigns them a large share of the adjustment cost. Exit is limited to political resistance or migration; most cannot simply opt out of the economy they live inside.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_north_consumer_classes, payer,
    organized, biographical, constrained, national).

% Owners of extraction-dependent and growth-dependent capital who stand to lose asset value directly and immediately if throughput reduction and sufficiency policy displace growth as the governing objective. Unlike consumer classes, they retain meaningful exit through capital mobility, diversification, and political influence to resist or dilute the transformation.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, fossil_capital_shareholders, payer,
    powerful, biographical, mobile, global).

% Workers in extraction, manufacturing, and growth-sector employment whose jobs are structurally tied to the throughput this reading seeks to reduce. Universal basic services and working-time reduction are offered as compensating mechanisms, but transition sequencing is uncertain and many bear displacement risk before any replacement income structure is operative.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, growth_dependent_wage_workers, payer,
    powerless, biographical, trapped, national).

% Non-human ecological systems and the biophysical limits (planetary boundaries) this reading treats as binding constraints rather than externalities to be priced. Listed for completeness as the entity whose stability the sufficiency framing claims to protect; it is not an agent capable of asserting its own interest.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, ecological_systems_stewards, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_action__degrowth_transformation, ecological_systems_stewards).

% Hold the dominant institutional position within central banks, treasuries, and multilateral lenders. They would object that abandoning GDP growth as an organizing principle risks employment collapse, debt-service crises, and loss of the fiscal capacity needed to fund the very transition programs this reading proposes. Their objections are marginalized in movement literature but dominate actual policy-making institutions — the reverse of the exclusion pattern in most constraint stories.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, mainstream_growth_economists, excluded,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__degrowth_transformation, diffuse).
narrative_ontology:fixing_cost_class(climate_response_action__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shift in the metric by which economic success and climate adequacy are judged, replacing an output-maximization criterion (GDP growth) with a throughput-and-equity criterion, so that policy, investment, and labor allocation can be redirected toward sufficiency rather than expansion.
% TRANSFER_FUNCTION: Moves consumption capacity, working hours, and capital claims from currently high-throughput Global North populations and growth-dependent capital toward Global South development budgets and toward reduced pressure on future generations and ecological systems; also redistributes economic security from wage labor toward universal basic services.
% ABSENT_VOICES: Mainstream growth economists and multilateral financial institutions are structurally marginal within the degrowth policy coalition's own discourse even though they hold the actual levers (central bank mandates, sovereign debt terms, IMF conditionality) that would determine whether the transformation is fiscally survivable. Growth-dependent wage workers are nominally centered but have little direct voice in designing the compensating UBS/working-time mechanisms meant to protect them.
% DISAPPEARANCE_RATIONALE: If this reading vanished from the policy conversation, the mitigation_priority and adaptation_priority readings would continue to organize climate policy largely unchanged in the near term — GDP-growth-compatible carbon markets and resilience spending do not depend on the degrowth argument's survival. But the degrowth coalition argues its disappearance would foreclose the only pathway that keeps warming within survivable bounds without relying on unproven negative-emissions technology, making the long-run world materially different. Whether the world 'rearranges' depends on which empirical claim about technological substitution feasibility is correct, which is itself contested.
% FOUNDING_PROBLEM: Global emissions reduction has not tracked with GDP growth despite decades of efficiency gains and green investment (relative decoupling without adequate absolute decoupling), and the burden of both warming impacts and mitigation costs falls disproportionately on populations who contributed least to cumulative emissions.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 Working Group III assessments and independent ecological economics literature (outside the degrowth movement's own advocacy organizations) corroborate that absolute decoupling of GDP growth from resource throughput at the pace required has not been empirically observed at global scale, which is the core empirical premise this reading depends on. Mainstream growth economists dispute the inference drawn from that same evidence, arguing decoupling is achievable with adequate policy and does not require abandoning growth as an organizing principle — corroboration of the underlying decoupling-gap fact exists outside the coalition; corroboration of the growth-abandonment inference does not.
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, contested).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__degrowth_transformation, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.71 by interval end) because this reading's own diagnosis holds that continued GDP-growth orientation constitutes an extractive transfer — from the Global South's remaining carbon budget, from future generations who inherit warming and speculative-removal risk, and from ecological systems treated as externalities. Suppression is moderate-high (0.62) and rising, reflecting the reading's own account that maintaining growth-as-organizing-principle requires increasingly active ideological and institutional defense (central bank mandates, GDP-indexed political legitimacy, resistance to alternative metrics) as ecological strain becomes harder to ignore. Theater ratio is moderate (0.40): a meaningful share of climate policy activity under the status quo (voluntary corporate net-zero pledges, green growth rhetoric) is, by this reading's lights, performative substitution for the throughput reduction it says is actually required. Accessibility collapse is moderate (0.45) — alternatives to growth-as-organizing-principle are contested and visible, not fully foreclosed, which is why resistance (0.78) is high: growth-dependent capital and mainstream economic institutions actively resist the reframing.
 *
 * PERSPECTIVAL GAP:
 *   From the degrowth_policy_coalition's agenda-setting seat, the arrangement is coordination toward long-overdue redistribution and biophysical realism. From the mainstream_growth_economists' excluded seat, the same proposal reads as an untested, potentially destabilizing rejection of the fiscal machinery (growth-financed debt service, employment absorption) that any transition — including their own — depends on. The engine should compute these as genuinely different per-seat classifications rather than the story adjudicating between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South development claimants, future generations, and ecological systems are coded as beneficiaries because the reading's explicit redistributive logic routes remaining consumption and emissions space toward them. Global North consumer classes, fossil capital shareholders, and growth-dependent wage workers are coded as payers because the reading assigns them the adjustment cost — consumption constraint, asset devaluation, and employment transition risk respectively — though their exit options differ sharply: fossil capital retains mobility and political leverage (arbitrage-adjacent), while wage workers are largely trapped in place, which the engine should register as differentiated effective extraction despite a shared 'payer' role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (decoupling gap between GDP growth and required absolute emissions reduction) is coded live, corroborated by IPCC assessment outside the degrowth coalition itself — this blocks a mandatrophy reading where the constraint is authored dead but institutionally persisting. The genuine dispute is not whether the problem exists but whether this reading's remedy (abandoning growth as organizing principle) is the correct or only structurally available response, which is precisely the inter-reading contest the kernel framework is built to hold open rather than resolve inside a single constraint file.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_ambiguity,
    'Is absolute decoupling of GDP growth from resource throughput and emissions achievable at the pace and scale required, making mitigation_priority sufficient, or does the empirical decoupling gap validate the degrowth reading''s premise that growth-abandonment is structurally necessary?',
    'Longitudinal cross-national data on absolute (not merely relative) decoupling rates against required emissions trajectories; independent ecological economics and mainstream growth economics currently interpret the same IPCC data to opposite policy conclusions.',
    'If decoupling proves technically feasible at required speed, the degrowth reading''s foreclosure of mitigation_priority as insufficient collapses and the constraint becomes a preference-driven equity claim rather than a physically necessitated one; if decoupling proves infeasible, mitigation_priority''s growth-preserving premise is undermined instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility_ambiguity, empirical, 'Whether absolute GDP-throughput decoupling is achievable, which is the load-bearing empirical premise separating this reading from mitigation_priority.').

omega_variable(
    political_feasibility_vs_structural_necessity,
    'Is the political infeasibility of degrowth transformation (documented resistance from growth-dependent institutions) evidence that the reading is impractical, or evidence that the institutions defending growth-as-organizing-principle are themselves the extractive structure the reading identifies?',
    'Track whether resistance to sufficiency policy tracks material interest (capital devaluation risk) versus genuine feasibility concern (fiscal/employment collapse risk); requires distinguishing self-interested defense from disinterested technical objection among mainstream_growth_economists.',
    'If resistance is predominantly interest-driven, it corroborates this reading''s claim that growth-orientation is maintained by active suppression rather than superior function, raising confidence in the tangled_rope/snare-adjacent classification of the status quo it opposes. If resistance is predominantly technical (genuine fiscal instability risk), the reading''s transition sequencing is under-specified and its own extraction claim is weaker.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_vs_structural_necessity, conceptual, 'Whether institutional resistance to degrowth reflects captured interest or genuine feasibility constraint.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does adopting the degrowth_transformation reading logically foreclose mitigation_priority (they cannot coexist because one requires growth and the other requires its abandonment), or can a jurisdiction hold elements of both (e.g., pursue carbon markets while separately pursuing sufficiency policy) such that they merely compete for policy priority rather than logically excluding each other?',
    'Examine actual multi-track climate policy portfolios (e.g., the EU''s simultaneous pursuit of carbon markets and selected sufficiency measures like the Green Deal''s material footprint targets) to see whether the two commitments are jointly held in practice.',
    'If jointly holdable, the reading_relations edge to mitigation_priority should be coexists_with rather than a foreclosure; if the growth-abandonment premise is genuinely incompatible with growth-preserving carbon markets at the level of organizing principle (not merely policy instrument), the relation would need reassessment toward a stronger contradiction, though the framework asks us to reserve forecloses for the rare case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the degrowth and mitigation readings are logically incompatible or merely competing policy emphases within overlapping frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2015, climate_response_action__degrowth_transformation, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(clim_tr_t2015, observed).
narrative_ontology:measurement(clim_tr_t2019, climate_response_action__degrowth_transformation, theater_ratio, 2019, 0.3).
narrative_ontology:measurement_basis(clim_tr_t2019, observed).
narrative_ontology:measurement(clim_tr_t2023, climate_response_action__degrowth_transformation, theater_ratio, 2023, 0.35).
narrative_ontology:measurement_basis(clim_tr_t2023, observed).
narrative_ontology:measurement(clim_tr_t2027, climate_response_action__degrowth_transformation, theater_ratio, 2027, 0.38).
narrative_ontology:measurement_basis(clim_tr_t2027, projected).
narrative_ontology:measurement(clim_tr_t2031, climate_response_action__degrowth_transformation, theater_ratio, 2031, 0.39).
narrative_ontology:measurement_basis(clim_tr_t2031, projected).
narrative_ontology:measurement(clim_tr_t2035, climate_response_action__degrowth_transformation, theater_ratio, 2035, 0.4).
narrative_ontology:measurement_basis(clim_tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2015, climate_response_action__degrowth_transformation, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement_basis(clim_be_t2015, observed).
narrative_ontology:measurement(clim_be_t2019, climate_response_action__degrowth_transformation, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement_basis(clim_be_t2019, observed).
narrative_ontology:measurement(clim_be_t2023, climate_response_action__degrowth_transformation, base_extractiveness, 2023, 0.6).
narrative_ontology:measurement_basis(clim_be_t2023, observed).
narrative_ontology:measurement(clim_be_t2027, climate_response_action__degrowth_transformation, base_extractiveness, 2027, 0.67).
narrative_ontology:measurement_basis(clim_be_t2027, projected).
narrative_ontology:measurement(clim_be_t2031, climate_response_action__degrowth_transformation, base_extractiveness, 2031, 0.7).
narrative_ontology:measurement_basis(clim_be_t2031, projected).
narrative_ontology:measurement(clim_be_t2035, climate_response_action__degrowth_transformation, base_extractiveness, 2035, 0.71).
narrative_ontology:measurement_basis(clim_be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2015, climate_response_action__degrowth_transformation, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(clim_su_t2015, observed).
narrative_ontology:measurement(clim_su_t2019, climate_response_action__degrowth_transformation, suppression_requirement, 2019, 0.38).
narrative_ontology:measurement_basis(clim_su_t2019, observed).
narrative_ontology:measurement(clim_su_t2023, climate_response_action__degrowth_transformation, suppression_requirement, 2023, 0.48).
narrative_ontology:measurement_basis(clim_su_t2023, observed).
narrative_ontology:measurement(clim_su_t2027, climate_response_action__degrowth_transformation, suppression_requirement, 2027, 0.55).
narrative_ontology:measurement_basis(clim_su_t2027, projected).
narrative_ontology:measurement(clim_su_t2031, climate_response_action__degrowth_transformation, suppression_requirement, 2031, 0.6).
narrative_ontology:measurement_basis(clim_su_t2031, projected).
narrative_ontology:measurement(clim_su_t2035, climate_response_action__degrowth_transformation, suppression_requirement, 2035, 0.62).
narrative_ontology:measurement_basis(clim_su_t2035, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__degrowth_transformation, 0.15).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_response_action kernel (mitigation_priority, adaptation_priority, degrowth_transformation). Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure: mitigation_priority centers technological substitution and carbon markets while preserving growth (lower ε, coordination-dominant framing likely); adaptation_priority centers resilience investment accepting continued warming (distinct victim set: currently vulnerable populations rather than future generations); degrowth_transformation (this file) centers structural transformation away from growth as organizing principle, with the highest authored extraction because it characterizes the standing growth-oriented order itself as the extractive arrangement under contest. The three are linked via affects_constraints rather than merged, per the ε-invariance principle — averaging their ε values would produce a fourth, fictional constraint with no coherent referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
