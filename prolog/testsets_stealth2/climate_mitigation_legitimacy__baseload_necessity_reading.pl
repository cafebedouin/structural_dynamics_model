% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Doctrine in Decarbonization Policy
 *   domain: energy/climate/governance
 *
 * SUMMARY:
 *   The doctrine that reliable decarbonization requires dispatchable baseload
 *   power renewables cannot provide at scale functions as a gatekeeper in
 *   climate-mitigation policy: it determines which resources count toward
 *   adequacy, which receive capacity payments and credits, and which pathways
 *   are dismissed as inadequate before they are costed. It is presented as
 *   physics ('the sun sets') but is enforced through planning conventions,
 *   accreditation rules, procurement floors, and subsidy statutes — human
 *   artifacts with identifiable administrators. Beneath it sits a genuine
 *   reliability problem; on top of it sit concentrated receipts. This file
 *   instantiates ONE reading of the climate_mitigation_legitimacy kernel (the
 *   baseload_necessity_reading); the sibling readings are separate
 *   constraints linked via network.affects_constraints, per the
 *   epsilon-invariance principle. The epsilon referent is the standing policy
 *   arrangement under contest — the capacity-market and credit regime this
 *   doctrine sustains — assessed by this reading's own lights: the reading
 *   concedes real costs borne beyond service value (socialized overruns,
 *   capacity payments above scarcity value) while holding the bulk of
 *   firm-capacity revenue as justified reliability insurance. The
 *   claim/metric gap is deliberate: claimed_type is authored from structure,
 *   metrics from description; the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - incumbent_nuclear_operators: Primary beneficiary (institutional/identity_locked) — collects the capacity payments, credits, and life extensions the doctrine justifies; co-administers its justification
 *   - reactor_vendors_and_epc_firms: Secondary beneficiary (organized/constrained) — sells the capital projects the doctrine sizes
 *   - fossil_generation_owners: Opportunistic beneficiary (powerful/constrained) — rides the dispatchability premise to defer retirements
 *   - electricity_ratepayers: Primary target (moderate/trapped) — bears capacity charges, credit surcharges, and stranded-cost recovery
 *   - federal_taxpayers: Diffuse target (powerless/trapped) — backstops loan guarantees and demonstration budgets
 *   - renewable_storage_developers: Contested target (organized/constrained) — pathways discounted by accreditation before costing
 *   - independent_system_operators: Agenda setter (institutional/constrained) — translates the doctrine into binding adequacy parameters
 *   - national_energy_ministries: Agenda setter (institutional/constrained) — legislates credits and capacity mandates
 *   - community_energy_projects: Excluded voice (powerless/trapped) — aggregated flexibility lacks standing in the planning docket
 *   - climate_policy_analysts: Analytical observer (analytical/analytical) — models pathways outside the procurement process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.48).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Doctrine in Decarbonization Policy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy/climate/governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '1d23b4f2-f6e9-468b-a09c-44e7acf50176').
narrative_ontology:cs_kernel_codification('1d23b4f2-f6e9-468b-a09c-44e7acf50176', formalized).
narrative_ontology:cs_authority_grounding('1d23b4f2-f6e9-468b-a09c-44e7acf50176', expertise).
narrative_ontology:cs_interpretation_layer_present('1d23b4f2-f6e9-468b-a09c-44e7acf50176').
narrative_ontology:cs_reading_relation('1d23b4f2-f6e9-468b-a09c-44e7acf50176', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('1d23b4f2-f6e9-468b-a09c-44e7acf50176', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('1d23b4f2-f6e9-468b-a09c-44e7acf50176', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('1d23b4f2-f6e9-468b-a09c-44e7acf50176', foundational, firm_dispatchable_capacity_irreplaceable_at_scale).
narrative_ontology:cs_axiom_status(firm_dispatchable_capacity_irreplaceable_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('1d23b4f2-f6e9-468b-a09c-44e7acf50176', firm_dispatchable_capacity_irreplaceable_at_scale, empirically_contingent).
narrative_ontology:cs_axiom('1d23b4f2-f6e9-468b-a09c-44e7acf50176', secondary, resource_adequacy_requires_dedicated_capacity_procurement).
narrative_ontology:cs_axiom_status(resource_adequacy_requires_dedicated_capacity_procurement, holdable).
narrative_ontology:cs_axiom_grounding('1d23b4f2-f6e9-468b-a09c-44e7acf50176', resource_adequacy_requires_dedicated_capacity_procurement, conventional).
narrative_ontology:cs_reference_frame('1d23b4f2-f6e9-468b-a09c-44e7acf50176', vertically_integrated_baseload_grid_norm).
narrative_ontology:cs_drift_state('1d23b4f2-f6e9-468b-a09c-44e7acf50176', contemporary_high_vre_grids, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1d23b4f2-f6e9-468b-a09c-44e7acf50176', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, reactor_vendors_and_epc_firms).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_generation_owners).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_storage_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, federal_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the existing reactor fleet and collects capacity payments, zero-emission credits, production tax credits, and life-extension approvals whose justification runs through the necessity doctrine. Co-authors resource-adequacy testimony and staffs reliability councils. The fleet, its workforce towns, and its regulatory compact are fused with the doctrine's continuation; plant closure means community collapse, so defending the arrangement is existential rather than discretionary.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_operators, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_operators, agenda_setter).

% Sells reactors, major forgings, and engineering-procurement-construction contracts whose pipeline size follows from the doctrine's buildout premise. Revenue depends on government-backed procurement and export finance; few alternative customers exist for the specialized supply chain.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, reactor_vendors_and_epc_firms, beneficiary,
    organized, biographical, constrained, continental).

% Invokes the same dispatchability premise to win reliability-must-run designations, capacity payments, and deferred retirement dates for unabated gas fleets. The benefit is opportunistic: it holds only insofar as 'dispatchable' is read to include their assets, and a strict low-carbon reading of the same doctrine would eventually displace them.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_generation_owners, beneficiary,
    powerful, biographical, constrained, national).

% Pays capacity charges, non-bypassable surcharges financing nuclear credits, and stranded-cost recovery on overran projects through monthly bills. Service territories are monopoly-franchised, so no supplier choice exists; objections surface only through occasional consumer-intervention in rate cases.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_ratepayers, payer,
    moderate, immediate, trapped, regional).

% Backstops loan guarantees, funds demonstration budgets, and absorbs defaults on megaprojects. The exposure is diffuse and per-project voice is nil; losses are socialized after the fact while the decision was made elsewhere.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, federal_taxpayers, payer,
    powerless, generational, trapped, national).

% Proposes storage-hybrid and wind/solar portfolios that capacity-accreditation models discount, and faces procurement floors requiring 'dispatchable' qualifications their products cannot meet. Capital is already committed to the clean-energy sector, so exit means shifting geography or technology, not leaving decarbonization markets. Actively litigates accreditation methodologies in every planning cycle.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_storage_developers, payer,
    organized, biographical, constrained, global).

% Rooftop and community solar-plus-storage aggregations would diversify the adequacy resource pool, but integrated-resource plans are dominated by utility-scale filings in which these projects lack standing. Their objection — that aggregated flexibility is systematically undervalued by the planning constructs — never reaches the docket with equal weight.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, community_energy_projects, excluded,
    powerless, biographical, trapped, local).

% Designs capacity accreditation, reserve margins, and loss-of-load-expectation standards, translating the necessity doctrine into binding planning parameters. Professionally liable for blackouts, which biases the machinery toward firm-capacity conservatism regardless of the technology mix.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, independent_system_operators, agenda_setter,
    institutional, generational, constrained, regional).

% Legislates nuclear credits, capacity-market mandates, and strategic-reserve mechanisms, balancing climate targets against security-of-supply politics. Reversing course invites simultaneous incumbent backlash and blackout-blame, so the doctrinal frame is politically sticky from the administering seat.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, national_energy_ministries, agenda_setter,
    institutional, generational, constrained, national).

% Models decarbonization pathways across the full technology space, documents where capacity constructs diverge from engineering reality, and publishes outside the procurement process. Holds no position in the flows the doctrine moves.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_operators).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of resource adequacy: ensuring the grid serves load through multi-day low-wind and low-sun periods and seasonal extremes, which no single generator or consumer can secure alone. Provides the shared planning standard (loss-of-load-expectation, reserve margins) around which investors coordinate sixty-year capital commitments.
% TRANSFER_FUNCTION: Moves revenue streams — capacity payments, zero-emission credits, production tax credits, guaranteed cost recovery, loan-guarantee backstops — from ratepayers and taxpayers to owners of firm and dispatchable generation, disproportionately nuclear operators and retained fossil assets; and moves procurement access away from variable-renewable-only portfolios, which are classified as inadequate before costing.
% ABSENT_VOICES: Storage-and-flexibility researchers, community energy advocates, and industrial demand-response providers stand outside most resource-adequacy proceedings, which are dominated by incumbent generators and system operators. Their objection — that adequacy constructs systematically undervalue storage duration, aggregation, and flexibility — enters the record late and with lesser standing, so the unanimity behind the doctrine partly reflects who was in the room when the accreditation rules were written.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand capacity-market revenue streams, void zero-emission credit statutes, reopen integrated-resource plans, and redirect capital toward storage, transmission, and demand flexibility; nuclear life-extension cases and gas retirement schedules would be re-litigated within a single regulatory cycle. Arrangements visibly depend on the doctrine's continuation.
% FOUNDING_PROBLEM: Unit-commitment-era reliability engineering faced uncontrollable demand and inflexible thermal units; the baseload-peaking hierarchy solved that. As decarbonization advanced, the operative question became whether intermittent sources could keep the lights on at all, and the doctrine consolidated as the answer: reliable decarbonization requires dispatchable baseload that renewables cannot provide at scale.
% FOUNDING_PROBLEM_CORROBORATION: The weak form is corroborated from outside the beneficiary set: NERC-style reliability assessments and the peer-reviewed firm-capacity literature produced by systems-modeling groups unaffiliated with nuclear vendors attest that multi-day lulls and seasonal deficits are real and that some firm low-carbon resource lowers system cost. No party outside the benefiting coalition attests the strong categorical claim that renewables cannot provide firm capacity at scale — that premise rests on this reading's own modeling tradition and is precisely what the sibling readings deny.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48: moderate by this reading's own lights — it concedes that socialized overrun costs, capacity payments decoupled from scarcity value, and credit programs exceeding incremental reliability value are real transfers, while holding most firm-capacity revenue as the price of reliability insurance; hence elevated but not snare-grade. Suppression 0.58: structural, not coercive — accreditation discounts, procurement floors, and planning conventions close the alternative pathway without banning it; suppression is a raw property and is deliberately NOT scaled by power or scope in this authoring. Theater_ratio 0.30: the reliability concern is real, but a growing share of advocacy is performative defense of revenue (blackout-scare campaigns against accreditation reform), and the ratio tracks that growing share. Accessibility_collapse 0.40: alternatives remain visible and increasingly demonstrated in high-VRE jurisdictions; they do not vanish once the doctrine is understood. Resistance 0.62: sustained industry, NGO, and academic contestation in every integrated-resource-plan cycle. Claim/metric independence: tangled_rope is claimed from structure (genuine adequacy coordination + asymmetric receipts + active enforcement machinery); the metrics are authored descriptively; where computed per-seat types diverge from the claim, that divergence is the datum. All three temporal series run on one shared grid {0,5,10,15,20,25}; the interval maps to roughly 2000-2025, spanning capacity-market formalization, state zero-emission credit programs, megaproject overruns, and the post-crisis reliability turn. Receipt-surface notes: gains demonstrably accrue to the operator seat (named in gain_flow); fixing is classed prohibitive because unwinding credit statutes and redesigning accreditation carries incumbent mobilization and blackout-blame costs exceeding what any single agenda-setter would capture from fixing.
 *
 * PERSPECTIVAL GAP:
 *   From the operator and vendor seats the arrangement computes as coordination they built, staffed, and are existentially fused with — necessity, not extraction. From the ratepayer and storage-developer seats the same accreditation machinery computes as enforced exclusion with concentrated receipts and no supplier choice. The system-operator seat sits between: engineering sincerity coupled with liability-driven conservatism that happens to favor incumbents. The ministry seat experiences the doctrine as political shelter — it converts a contested technology bet into an engineering imperative. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: operators sit nearest the beneficiary end (identity_locked exit amplifies retention of benefit), vendors are transactional collectors, fossil owners are partial and opportunistic — the derived d for fossil owners would sit lower than their true position, but the override axis is power-level, too coarse to isolate one seat, so the imprecision is documented here rather than overridden. Targets derive high directionality: ratepayers and taxpayers are trapped with no offsetting receipt, and storage developers combine paid opportunity cost with constrained exit. Administrators (operators, ministries) sit near symmetric. National and continental instrument scopes raise verification difficulty, modestly amplifying effective extraction on the trapped seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is partially live: multi-day lulls and seasonal deficits are real, so the mandate is not dead — but the strong categorical form (renewables cannot provide firm capacity at scale) is contested and may be dying as storage costs fall. The mandatrophy guard: if seasonal storage crosses cost thresholds, the mandate dies while the arrangement (capacity markets, credit statutes) persists — the classic dead-mandate-with-world_rearranges mismatch the R5 consumer flags, cross-checked against the theater path. The tangled_rope classification prevents mislabeling in both directions: calling the doctrine a pure snare erases the genuine adequacy function that would survive reform; calling it a rope launders concentrated receipts as coordination overhead. Piton risk emerges only on a specific future path — enforcement decaying into theatrical compliance while receipts continue — which the theater_ratio series monitors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the climate_mitigation_legitimacy kernel — what would change structurally if a sibling reading were adopted instead?',
    'Comparative instantiation: generate the sibling stories (renewable_primacy, portfolio_pragmatism, degrowth_sufficiency) and diff their beneficiary/victim sets, epsilon values, and computed types against this file.',
    'Under renewable_primacy, storage firms enter the beneficiary set and nuclear operators migrate toward the target side; under degrowth_sufficiency, the entire generation-expansion apparatus becomes the target and this reading''s beneficiaries dissolve; under portfolio_pragmatism, extraction diffuses across all subsidized technologies and the necessity claim softens into preference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story''s position as one reading of the climate_mitigation_legitimacy kernel and the structural deltas sibling adoptions would produce.').

omega_variable(
    firm_capacity_substitutability,
    'Can multi-day and seasonal storage, demand flexibility, and overbuild-plus-curtailment substitute for dispatchable firm capacity at the scales deep decarbonization requires?',
    'Storage cost trajectories crossing seasonal-adequacy thresholds, demonstrated high-VRE systems maintaining loss-of-load-expectation targets through adverse weather years, and accreditation reforms admitting duration-based resources at full value.',
    'If substitution succeeds at scale, the doctrine''s coordination core collapses into rent defense and the constraint shifts snare-ward; if it fails at scale, the coordination component dominates and the measured extraction reads largely as the price of reliability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_capacity_substitutability, empirical, 'Whether the doctrine''s factual premise — categorical incapacity of renewables at scale — survives storage and flexibility evidence.').

omega_variable(
    physics_vs_planning_construct,
    'Is ''renewables cannot provide firm capacity'' a fact of grid physics or an artifact of planning conventions (loss-of-load-expectation standards, capacity accreditation, reserve-margin definitions) written when thermal units defined the fleet?',
    'Cross-jurisdictional comparison of jurisdictions running different adequacy constructs at comparable reliability and high VRE shares; counterfactual re-accreditation modeling of the same physical fleet under duration-based rules.',
    'If constructed, the constraint loses any mountain-like immunity entirely, its suppression becomes contestable regulation rather than engineering necessity, and the resistance metric predicts successful reform; if physical, part of the measured extraction is irreducible coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physics_vs_planning_construct, conceptual, 'Natural-law versus constructed-character ambiguity in the necessity claim.').

omega_variable(
    fossil_opportunism_boundary,
    'Do fossil generation owners benefit from THIS reading specifically, or from a sibling rhetorical variant (''renewables are unreliable'') that the nuclear-centered necessity claim does not strictly entail?',
    'Flow tracing: separate capacity-payment and reliability-must-run revenues attributable to firm-low-carbon mandates from those attributable to gas-retention designations, and trace which doctrinal citations justify each.',
    'If fossil rents ride on this reading, effective extraction widens beyond the nuclear beneficiary set and the payer seats'' burden rises accordingly; if separable, this reading''s extraction confines to nuclear-support flows and the fossil beneficiary declaration overstates its reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_opportunism_boundary, empirical, 'Boundary of the beneficiary set: whether fossil owners are inside this reading''s benefit structure or riding an adjacent variant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 25, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what decarbonization requires' decomposes into four structurally distinct readings of the climate_mitigation_legitimacy kernel, each with its own epsilon, beneficiary/victim structure, and type. This file is the baseload_necessity_reading; the sibling files instantiate renewable_primacy, portfolio_pragmatism, and degrowth_sufficiency. The upstream/downstream structure runs from this reading outward: when enacted, it changes resource availability and legitimacy conditions for the portfolio debate (influences) and is logically incompatible with the renewable-primacy capability premise (forecloses), while remaining merely rivalrous with the degrowth reading (coexists_with). Family members are linked exclusively via network.affects_constraints; no reading's epsilon is averaged or hedged across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
