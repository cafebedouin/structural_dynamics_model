% ============================================================================
% CONSTRAINT STORY: comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_comparative_risk_dominant, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: comparative_risk_dominant
 *   human_readable: Comparative Risk Framework: Nuclear Acceptability Contingent on Fossil Fuel Alternatives
 *   domain: energy_policy/risk_assessment/public_safety
 *
 * SUMMARY:
 *   The comparative-risk framework for nuclear energy acceptability
 *   subordinates absolute safety thresholds to relative risk evaluation:
 *   nuclear is acceptable if and only if its risk profile is superior to the
 *   alternatives it displaces (primarily coal-fired generation and climate
 *   catastrophe). This reading of the acceptable_risk_for_energy kernel
 *   creates a tangled-rope structure in which near-term climate urgency
 *   (coordination function) is leveraged to impose multi-millennial waste
 *   stewardship burdens on future populations (asymmetric extraction). The
 *   constraint exhibits significant theater: the comparison logic, while
 *   originally addressing a genuine coordination problem (how to weigh
 *   incommensurable risk types), has become institutionalized as a
 *   naturalizing practice that forecloses questioning the waste-allocation
 *   asymmetry. Extractiveness has risen from 0.35 (1990s, when
 *   comparative-risk was novel and actively justified) to 0.58 (present),
 *   indicating increasing suppression of intergenerational justice concerns.
 *   The theater ratio (0.65) reflects that the comparison has become
 *   performative—climate and nuclear risks are presented as comparable
 *   through institutional ritual rather than transparent analytical
 *   reframing.
 *
 * KEY AGENTS:
 *   - Nuclear Energy Industry & Carbon-Constrained Sector: Primary beneficiary (institutional/arbitrage) — extracts legitimacy and market advantage by positioning as climate solution
 *   - Intergenerational Waste Bearers: Primary victim (powerless/trapped) — inherits multi-millennial stewardship burden imposed by comparative logic privileging near-term urgency
 *   - Climate-Vulnerable Populations: Secondary victim/beneficiary (moderate/constrained) — experience both genuine climate-emergency coordination benefit and asymmetric risk allocation (waste storage concentration in peripheral regions)
 *   - Nuclear-Host Nation: Secondary actor (powerful/constrained) — extracts energy/decarbonization benefit but concentrated waste-storage burden constrains exit; negotiates compensation but underlying asymmetry persists
 *   - Advanced Clean Technology Coalition: Exit-pathway actor (organized/arbitrage) — sees comparative-risk as temporary, building renewable + storage alternatives with 20-30 year sunset horizon
 *   - Risk Governance Apparatus: Institutional enforcer (institutional/arbitrage) — maintains comparative-risk framework through regulatory inertia; piton perspective shows degraded function as genuine alternatives emerge
 *   - Analytical Observer: Civilizational assessor (analytical/analytical) — identifies tangled-rope structure: genuine coordination (climate urgency) entangled with asymmetric extraction (intergenerational burden)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(comparative_risk_dominant, 0.58).
domain_priors:suppression_score(comparative_risk_dominant, 0.72).
domain_priors:theater_ratio(comparative_risk_dominant, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(comparative_risk_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(comparative_risk_dominant, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(comparative_risk_dominant, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(comparative_risk_dominant, "Comparative Risk Framework: Nuclear Acceptability Contingent on Fossil Fuel Alternatives").
narrative_ontology:topic_domain(comparative_risk_dominant, "energy_policy/risk_assessment/public_safety").

domain_priors:requires_active_enforcement(comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(comparative_risk_dominant, 'c31ddf57-7225-4aec-9213-b8fdc0bc88f3').
narrative_ontology:cs_created_at('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', '').
narrative_ontology:cs_kernel_codification('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', distributed).
narrative_ontology:cs_authority_grounding('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', extraction).
narrative_ontology:cs_interpretation_layer_present('c31ddf57-7225-4aec-9213-b8fdc0bc88f3').
narrative_ontology:cs_kernel_id(comparative_risk_dominant, acceptable_risk_for_energy).
narrative_ontology:cs_reading_relation('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', foundational, temporal_urgency_over_intergenerational_equity).
narrative_ontology:cs_axiom_status(temporal_urgency_over_intergenerational_equity, holdable).
narrative_ontology:cs_axiom_grounding('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', temporal_urgency_over_intergenerational_equity, empirically_contingent).
narrative_ontology:cs_axiom('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', foundational, comparative_risk_suffices_for_acceptability).
narrative_ontology:cs_axiom_status(comparative_risk_suffices_for_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', comparative_risk_suffices_for_acceptability, instrumental).
narrative_ontology:cs_reference_frame('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', energy_decarbonization_imperative).
narrative_ontology:cs_drift_state('c31ddf57-7225-4aec-9213-b8fdc0bc88f3', renewable_parity_emergence, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(comparative_risk_dominant, nuclear_energy_industry).
narrative_ontology:constraint_beneficiary(comparative_risk_dominant, carbon_constrained_energy_sector).
narrative_ontology:constraint_victim(comparative_risk_dominant, intergenerational_waste_bearers).
narrative_ontology:constraint_victim(comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(comparative_risk_dominant, decision_transparency_requirement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERGENERATIONAL WASTE BEARERS (SNARE) — Trapped by geological timescales and lack of voice in current risk calculus. Bears concentrated extraction: multi-millennial waste stewardship burden imposed by comparative-risk logic that privileges near-term climate urgency over intergenerational justice. No exit; no contemporaneous compensation mechanism. Maximum suppression: waste security is non-negotiable by future populations.
constraint_indexing:constraint_classification(comparative_risk_dominant, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE-VULNERABLE POPULATIONS (TANGLED ROPE) — Experience genuine coordination benefit: comparative-risk logic frames nuclear as necessary alternative to catastrophic climate outcomes that would displace them. But also experience asymmetric extraction: risk allocation concentrates waste storage in peripheral or colonially-subordinate regions, while near-term climate benefit flows to high-emission wealthy nations. Constrained exits via climate adaptation or nuclear-free development paths blocked by resource availability and financing conditionality.
constraint_indexing:constraint_classification(comparative_risk_dominant, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NUCLEAR INDUSTRY & DECARBONIZATION IMPERATIVE (ROPE) — Primary beneficiary. Comparative-risk framing creates institutional legitimacy: nuclear becomes climate solution rather than existential risk. Industry arbitrages regulatory comparison—extracts market value and policy support by positioning itself as lesser evil relative to coal/gas. Net benefits: licensing acceleration, public acceptance, climate finance access. Coordination function: solves the problem of decarbonizing grids at scale without demand reduction.
constraint_indexing:constraint_classification(comparative_risk_dominant, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NUCLEAR HOST NATION (TANGLED ROPE) — Faces both coordination and extraction. Coordination: national energy security, decarbonization pathway, technological leadership. Extraction: concentrated waste storage burden, long-tail catastrophic risk exposure, limited exit (political economy of waste siting is path-dependent; reversal extremely costly). Powerful nation-state can negotiate waste fees/compensation, but underlying asymmetry remains—storage burden persists across centuries while energy/climate benefits accrue primarily to current generation.
constraint_indexing:constraint_classification(comparative_risk_dominant, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ADVANCED TECHNOLOGY COALITION (SCAFFOLD) — Sees comparative-risk framing as temporary lock-in. Organized actors (renewable energy sector, battery storage developers, distributed generation advocates) position this constraint as sunset-clause mechanism: as renewables + storage costs decline, the 'comparative advantage' of nuclear over coal erodes, unlocking decarbonization without long-tail waste burden. Low effective extraction because this coalition perceives and is building an exit pathway with clear timeline (20-30 years for grid-scale storage maturity).
constraint_indexing:constraint_classification(comparative_risk_dominant, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RISK GOVERNANCE SYSTEM (PITON) — The comparative-risk framework is performing increasingly degraded function. Originally (1970s-1990s) genuinely resolved coordination problem: how to compare incommensurable risk types (radiation vs air pollution vs climate). Now largely theatrical: the comparison has become institutionalized ritual that naturalizes waste burden rather than actively evaluating alternatives. Suppression of absolute thresholds has become enforced norm rather than reasoned practice. Piton: maintained through regulatory inertia despite lower functional verification as alternatives emerge.
constraint_indexing:constraint_classification(comparative_risk_dominant, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the comparative-risk framework exhibits both genuine coordination function (enables risk trade-offs across incommensurable categories) and asymmetric extraction (naturalizes intergenerational burden asymmetry by privileging temporal urgency over spatial/intergenerational justice). Classification: tangled_rope rather than mountain because the framework's legitimacy rests on active institutional enforcement of the comparison logic, not on immutable natural law. The comparison IS contestable—as demonstrated by sibling readings that weight catastrophic tail risk or expected value differently.
constraint_indexing:constraint_classification(comparative_risk_dominant, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(comparative_risk_dominant_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(comparative_risk_dominant, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(comparative_risk_dominant, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(comparative_risk_dominant, TR),
    TR >= 0.70.

:- end_tests(comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The comparative-risk framework legitimizes substantial extraction—the multi-millennial waste burden—by framing it as the lesser evil relative to climate catastrophe. This is genuine extraction (not merely cost-sharing or coordination overhead) because the burden is asymmetrically imposed on agents (future generations, climate-peripheral populations) who do not contemporaneously benefit from the near-term climate advantage. The framework suppresses alternative framings (absolute safety thresholds, intergenerational justice, catastrophic tail-risk dominance) that would expose the extraction. Suppression (0.72): High. Suppression operates through: (1) temporal privileging—near-term climate urgency overrides long-term waste concerns by institutional construction; (2) commensurability assertion—the framework claims incommensurable risks (radiation, air pollution, climate) are comparable, suppressing the legitimacy of separate evaluation; (3) intergenerational exclusion—future populations have no voice in risk allocation; (4) tail-risk minimization—catastrophic containment failure is not centered in public risk calculus. Theater ratio (0.65): Moderate-high. The comparative-risk calculation has become increasingly performative over 45 years. Original (1970s-1980s) analysis genuinely grappled with risk-weighting problems. Current institutional practice (2000s onward) performs the comparison as ritual—the outcome (nuclear acceptability) is predetermined by policy commitments, and the analytical machinery is deployed to justify rather than to genuinely evaluate trade-offs. The performance is especially visible in climate-emergency framing, where comparative risk is asserted rather than re-examined.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence here reveals the core analytical work of the constraint: each agent's classification depends entirely on whether they (a) experience the climate coordination benefit, and (b) bear the intergenerational waste burden. Beneficiaries of near-term climate mitigation without bearing long-term burden see Rope. Those bearing burden without receiving benefit see Snare. Those receiving both benefit and bearing burden (nuclear-host nations, climate-vulnerable populations with storage siting) see Tangled Rope. Those building alternatives see Scaffold. Those maintaining the ritual see Piton. The perspectival gap IS the constraint's structural reality—there is no single 'correct' classification because the framework itself distributes costs and benefits asymmetrically across agents with different time horizons and positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent structural position. Nuclear industry (beneficiary + arbitrage): d≈0.15, f(d)≈-0.01, experienced extraction is negative to near-zero (they perceive coordination benefit). Intergenerational waste-bearers (victim + trapped): d≈0.95, f(d)≈1.42, maximum experienced extraction (no exit, no compensation, no voice). Climate-vulnerable populations (victim + constrained): d≈0.72, f(d)≈1.15, high but not maximal extraction (some agency through adaptation and financing negotiation, some genuine benefit from climate stabilization). Nuclear-host nation (mixed beneficiary + victim, powerful + constrained): d≈0.55, f(d)≈0.75, moderate experienced extraction (receives energy benefits and compensation fees but inherits concentrated storage burden with limited reversal options). Renewable sector (beneficiary + arbitrage in long-term, constrained now): d shifts from 0.65 (current: constrained by fossil-fuel incumbent advantage) to 0.15 (future: when renewables reach price parity, they move to arbitrage). Risk governance (institutional maintaining current regime): d≈0.00, f(d)≈-0.12 (they are beneficiaries of the framework—it consolidates their regulatory authority).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: This constraint resolves the mandatrophy by showing how one reading of an acceptable-risk kernel produces a specific, coherent, but contestable classification structure. The comparative-risk-dominant reading asserts that intergenerational waste burden can be rationalized as the lesser evil relative to climate catastrophe. This legitimizes the tangled-rope classification: genuine coordination (climate urgency + energy policy coupling) entangled with asymmetric extraction (waste burden on powerless future actors). Sibling readings (catastrophic_tail_dominant, expected_value_dominant) would reweight the calculus differently—tail-risk reading would elevate waste-containment failure probability to decision-dominance, shifting intergenerational victims from tangled_rope toward snare. Expected-value reading might weight long-term waste remediation costs more equally with climate benefits, reducing extraction visibility. The mandatrophy is not 'which reading is correct' but 'which axioms ground acceptable risk allocation?'—and this reading makes explicit the axiom (temporal privileging of climate urgency over intergenerational justice) that siblings reject. The tangled_rope classification is diagnostically precise: it captures the entanglement of coordination and extraction that the comparative-risk logic produces. No simpler type fits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_weighting_under_determination,
    'How should decision-making weight near-term climate catastrophe against multi-millennial waste stewardship burden? What is the ethically defensible time-discount rate?',
    'Intergenerational justice framework development; empirical research on long-term waste containment stability vs climate cascade-failure timelines; formalization of non-discounting principles for environmental commons',
    'If present-biased discounting is defensible: comparative-risk logic justified, nuclear acceptability stands. If equal weighting across temporal scales required: intergenerational extraction exposed, nuclear acceptability severely constrained. Currently unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_weighting_under_determination, preference, 'Ethical weighting of near-term climate vs long-term waste burden').

omega_variable(
    waste_containment_longevity_empirical,
    'Can engineered and geological waste containment systems reliably isolate high-level nuclear waste for 10,000+ years?',
    'Long-term geological monitoring data; analogues from natural uranium deposits and archeological records; failure mode analysis of repository design under climate/seismic stress; systems reliability engineering for multi-millennial timescales',
    'If containment demonstrably reliable: waste burden is real but bounded; comparative-risk calculation remains valid. If containment failure probability is non-negligible: catastrophic tail risk becomes endogenous to nuclear choice; extraction visibility increases (trapped future populations face tail risk that current generation imposed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_containment_longevity_empirical, empirical, 'Multi-millennial waste containment reliability').

omega_variable(
    renewable_storage_cost_trajectory,
    'What is the trajectory of renewable energy + grid-scale storage costs relative to nuclear? At what point does comparative advantage shift?',
    'Learning curve analysis; technology readiness assessment; deployment cost data; grid modeling for 50-100% renewable scenarios; empirical validation of storage maturity timelines',
    'If storage costs reach grid-parity <15 years: scaffold perspective confirmed, comparative-risk is transient lock-in (extractiveness decreases, classification shifts toward temporary mechanism). If storage costs plateau above nuclear cost: comparative advantage persists indefinitely, victims face permanent intergenerational asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_storage_cost_trajectory, empirical, 'Renewable + storage cost trajectory vs nuclear').

omega_variable(
    reading_determination_catastrophic_vs_comparative,
    'Which reading of acceptable_risk_for_energy correctly adjudicates the risk calculus: comparative-risk-dominant (this reading), catastrophic_tail_dominant (sibling), or expected_value_dominant (sibling)?',
    'Empirical validation of waste containment reliability (omega_waste_containment_longevity). If containment is reliable, tail-risk reading becomes less salient; comparative logic dominates. If containment failure probabilities are material, tail-risk reading foreclosed comparative logic.',
    'This omega routes to committer-level resolution: if tail-risk reading demonstrates non-negligible failure modes, comparative-risk-dominant loses coherence within a single normative framework and the reading_relation should downgrade from coexists_with to forecloses (asymmetrically—tail-risk forecloses comparative-risk, not vice versa).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_determination_catastrophic_vs_comparative, empirical, 'Interdependence between readings in acceptable_risk kernel').

omega_variable(
    intergenerational_representation_mechanism,
    'Does any institutional mechanism genuinely represent intergenerational interests in current risk decisions, or is intergenerational burden excluded from the decision frame by construction?',
    'Analysis of governance institutions (IAEA, national regulators, climate bodies): do they include intergenerational trustee roles? Do decision-makers face accountability to future populations? Comparison with other long-duration environmental commitments (wetland conservation, archaeological site protection) that do include intergenerational representation.',
    'If representation mechanism exists: extraction is constrained by accountability loop (reduces suppression). If intergenerational interests are systematically excluded: suppression is structural (remains high). Current evidence suggests exclusion—intergenerational burden is real but unrepresented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_representation_mechanism, empirical, 'Institutional representation of intergenerational interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(comparative_risk_dominant, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, comparative_risk_dominant, theater_ratio, 0, 0.45).
narrative_ontology:measurement(comp_tr_t15, comparative_risk_dominant, theater_ratio, 15, 0.58).
narrative_ontology:measurement(comp_tr_t30, comparative_risk_dominant, theater_ratio, 30, 0.65).
narrative_ontology:measurement(comp_tr_t45, comparative_risk_dominant, theater_ratio, 45, 0.68).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, comparative_risk_dominant, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comp_be_t15, comparative_risk_dominant, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(comp_be_t30, comparative_risk_dominant, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(comp_be_t45, comparative_risk_dominant, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(comparative_risk_dominant, resource_allocation).
narrative_ontology:affects_constraint(comparative_risk_dominant, catastrophic_tail_dominant).
narrative_ontology:affects_constraint(comparative_risk_dominant, expected_value_dominant).
narrative_ontology:affects_constraint(comparative_risk_dominant, waste_siting_burden_allocation).
narrative_ontology:affects_constraint(comparative_risk_dominant, climate_urgency_framing_dominance).

% DUAL FORMULATION NOTE:
% The acceptable_risk_for_energy kernel has three structurally distinct constraint readings with different ε values. comparative_risk_dominant (this story, ε=0.58, tangled_rope) privileges temporal urgency. catastrophic_tail_dominant (sibling, ε≈0.72, snare) privileges intergenerational risk. expected_value_dominant (sibling, ε≈0.45, rope) applies formal expected-value calculus. Each reading is a coherent normative choice grounded in different axioms about how to weigh incommensurable harms. They coexist as live policy positions across different institutional actors and jurisdictions. All three share the underlying empirical uncertainty (omega_waste_containment_longevity, omega_renewable_cost_trajectory) but resolve it through different normative weighting schemes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(comparative_risk_dominant, powerless, 0.95).
constraint_indexing:directionality_override(comparative_risk_dominant, moderate, 0.72).
constraint_indexing:directionality_override(comparative_risk_dominant, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
