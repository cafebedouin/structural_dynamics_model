% ============================================================================
% CONSTRAINT STORY: climate_liability_legal_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_liability_legal_standard, []).

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
 *   constraint_id: climate_liability_legal_standard
 *   human_readable: Climate Liability Legal Standard
 *   domain: environmental_law/tort_law/climate_justice
 *
 * SUMMARY:
 *   The climate liability legal standard is a meta-constraint governing how
 *   climate damages are attributed to causal actors and what legal recourse
 *   exists for affected communities. The fragmented, uncertain liability
 *   regime creates a structural gap between climate causation
 *   (well-established: greenhouse gas emissions cause warming) and legal
 *   causation (contested: proving individual defendant responsibility for
 *   specific damages remains unresolved). This gap generates a tangled rope
 *   constraint: there is a genuine coordination function (allowing energy
 *   markets to operate during a disputed liability regime) overlaid with
 *   asymmetric extraction (corporations benefit from legal uncertainty while
 *   climate victims cannot recover). The constraint exhibits all six DR types
 *   depending on perspective. The analytics reveals the key mandatrophy
 *   tension: treating the liability gap as a natural law (mountain)
 *   naturalizes what is actually a contingent choice about burden-of-proof
 *   allocation and causation doctrine. The trajectory shows increasing
 *   theater ratio (traditional tort language applied to unprecedented scale,
 *   obscuring legal dysfunction) and rising extractiveness as climate impacts
 *   accelerate while liability remains unsettled.
 *
 * KEY AGENTS:
 *   - Fossil Fuel and Carbon-Intensive Corporations: Primary beneficiary (institutional/arbitrage) — exploit regulatory fragmentation, lobby for weak standards, capture scientific uncertainty about attribution
 *   - Climate-Affected Subsistence Communities: Primary victim (powerless/trapped) — experience catastrophic damages with no legal recourse; trapped by geography and poverty; no exit options
 *   - Middle-Income Jurisdictions and Insurance Companies: Secondary victim (moderate/constrained) — face uncompensated climate damages and liability uncertainty; constrained by borders and legal precedent
 *   - Climate Justice Litigators and NGO Coalitions: Organized agents (organized/constrained) — building alternative liability standards through precedent and political organizing; see sunset pathway through accumulated case law
 *   - Traditional Legal Institutions: Institutional actor (institutional/arbitrage) — maintain tort framework through inertia; benefit from predictability even though framework is dysfunctional; sustain through theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing legal uncertainty as immanent causation limits rather than institutional choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_liability_legal_standard, 0.58).
domain_priors:suppression_score(climate_liability_legal_standard, 0.65).
domain_priors:theater_ratio(climate_liability_legal_standard, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_liability_legal_standard, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_liability_legal_standard, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_liability_legal_standard, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_liability_legal_standard, tangled_rope).
narrative_ontology:human_readable(climate_liability_legal_standard, "Climate Liability Legal Standard").
narrative_ontology:topic_domain(climate_liability_legal_standard, "environmental_law/tort_law/climate_justice").

domain_priors:requires_active_enforcement(climate_liability_legal_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_liability_legal_standard, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(climate_liability_legal_standard, carbon_intensive_manufacturers).
narrative_ontology:constraint_beneficiary(climate_liability_legal_standard, high_emitting_corporations).
narrative_ontology:constraint_victim(climate_liability_legal_standard, climate_affected_communities).
narrative_ontology:constraint_victim(climate_liability_legal_standard, future_generations).
narrative_ontology:constraint_victim(climate_liability_legal_standard, ecosystem_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-AFFECTED COMMUNITIES (SNARE) — Powerless agents bear catastrophic climate damages (sea-level rise, drought, extreme weather, crop failure, displacement) with no legal recourse under fragmented liability standards. Trapped by geographic dependence and economic vulnerability. The legal system provides no effective exit: damages are diffuse, causation chains are complex, defendant liability is legally uncertain. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(climate_liability_legal_standard, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JURISDICTIONS AND INSURERS (TANGLED ROPE) — Constrained by heterogeneous liability standards across jurisdictions and uncertainty about legal precedent. They benefit from clarity (which a unified standard would provide for risk pricing and settlement) but bear extraction through exposure to large uncompensated climate damages if liability is shifted backward. Some agency through litigation and legislative advocacy, but high suppression from legal uncertainty and coordination problems across borders.
constraint_indexing:constraint_classification(climate_liability_legal_standard, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOSSIL FUEL PRODUCERS (ROPE) — Primary beneficiary (institutional/arbitrage). These actors experience the fragmented liability standard as a coordination mechanism: the absence of clear, unified climate liability law creates regulatory arbitrage opportunities across jurisdictions. They can exit unfavorable jurisdictions, lobby for weaker standards, and benefit from the epistemic uncertainty about causation. The constraint serves a genuine coordination function (allowing energy markets to operate while liability is disputed) alongside asymmetric extraction. Net beneficiary with easy exit options.
constraint_indexing:constraint_classification(climate_liability_legal_standard, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE JUSTICE COALITIONS (SCAFFOLD) — Organized agents (environmental NGOs, climate lawyers, vulnerable-state governments) are actively constructing alternative liability standards through litigation victories (Dutch, German, UK precedents), international agreements (Paris framework), and legislative campaigns. They see the fragmented standard as a temporary coordination failure with a sunset: as case law accumulates and political power shifts, unified liability standards (strict liability for major emitters, damage assessments, loss-and-damage funds) will emerge. This perspective has agency and visibility. Sunset timeline: 15-30 years as climate impacts intensify and political power of affected communities increases.
constraint_indexing:constraint_classification(climate_liability_legal_standard, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL TORT FRAMEWORK (PITON) — The classical legal framework (burden of proof, causation requirements, individual negligence, proximate cause) was designed for localized industrial accidents, not planetary-scale cumulative emissions. Modern climate liability claims require proving causal chains across decades and billions of agents. The traditional framework persists through institutional inertia despite low functional capacity: courts invoke it ritualistically, but it collapses under climate complexity. Theater ratio high (0.68) — considerable performative legal analysis that obscures the framework's inability to handle diffuse causation. The piton is maintained by path dependence in legal institutions and absence of an accepted replacement framework.
constraint_indexing:constraint_classification(climate_liability_legal_standard, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AGENCIES (TANGLED ROPE) — Environmental agencies, insurance regulators, and financial authorities coordinate emissions accounting and risk disclosure (which is a genuine coordination function), but they also enforce standards that protect corporate interests through liability caps, safe-harbor provisions, and evidentiary burdens that suppress climate damages claims. Constrained by political economy and legal precedent. Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(climate_liability_legal_standard, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CAUSATION LIMIT VIEW (MOUNTAIN) — From a universalized perspective, proving individual defendant liability for global climate change faces irreducible epistemic limits: the causal chain from any single emitter's tons to any specific damage is mathematically inseparable from billions of other concurrent emissions. This perspective naturalizes the liability fragmentation as an immanent limit of physical causation, not a contingent institutional arrangement. However, the structural data contradicts this — the constraint is maintained through legal choices (burden-of-proof allocation, proximate-cause doctrine), not physical limits. The engine flags this as a false summit.
constraint_indexing:constraint_classification(climate_liability_legal_standard, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_liability_legal_standard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_liability_legal_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_liability_legal_standard, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_liability_legal_standard, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_liability_legal_standard, TR),
    TR >= 0.70.

:- end_tests(climate_liability_legal_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary extraction flow runs from climate victims to fossil fuel corporations — corporations avoid liability while damages are borne by affected communities. The extractiveness value reflects that this is not total (some corporations are beginning to face liability, some damage claims succeed) but structural (the fragmented standard systematically suppresses full liability). The measurement trajectory (0.38 → 0.58) shows extractiveness increasing as climate impacts multiply while legal standards remain fragmented, creating a growing damages gap. Suppression (0.65): High. Barriers to climate liability recovery are substantial: causation complexity, burden-of-proof requirements, statute-of-limitations restrictions, evidentiary hurdles for quantifying damages, geographic fragmentation of jurisdiction, economic inability of vulnerable communities to finance litigation, and lobbying power of defendants. Theater ratio (0.68): High and increasing. Courts and legislatures invoke traditional tort language (individual negligence, proximate cause, foreseeability) that was designed for localized accidents but is inadequate for planetary-scale cumulative emissions. The performance is high (0.45 at baseline, 0.68 at present) because legal institutions must deploy the traditional framework ritualistically even though it cannot handle the problem. The gap between what the legal language claims to do (assign individual responsibility for damages) and what it actually does (protect defendants from liability through evidentiary impossibility) is growing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why mandatrophy matters. If the analytical observer accepts the 'causation is impossible' view (Mountain), the constraint appears immutable and no legal reform is worthwhile. But the structural data reveals the mountain as false: causation uncertainty is maintained through institutional choices (burden-of-proof allocation, evidentiary standards), not physics. The Snare perspective from climate victims is not a minority view requiring proof — it is the dominant structural reality for 2 billion+ people facing climate damages with no legal recourse. The Rope perspective from corporations is genuine but asymmetric: they experience coordination benefit (market certainty during legal uncertainty) while victims experience pure extraction (no compensation, no deterrent). The Scaffold perspective reveals that litigators are systematically weakening the constraint through precedent (Dutch court 2019, German court 2021, UK courts 2023 onwards) and political organizing. The mandatrophy is resolved by acknowledging that all six perspectives are valid readings but the beneficiary (corporations) is primary beneficiary while the victim (climate-affected communities) is primary victim — the asymmetry is stark and measurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to the extraction flow. Fossil fuel corporations are full beneficiaries with arbitrage options (can exit unfavorable jurisdictions, lobby for weaker standards): d ≈ 0.05, resulting in low/negative experienced extractiveness. Climate victims are full targets with no exit: d ≈ 0.95, resulting in maximum experienced extractiveness. Jurisdictions and insurers occupy a mixed position (both benefit from stability and face exposure to uncompensated damages): d ≈ 0.55-0.65, experiencing moderate-high extraction. The legal framework itself occupies a high-extraction position relative to victims (maintaining a barrier to recovery) but a low-extraction position relative to corporations (providing protection): this agent duality is captured by the Piton perspective (high theater, low functional purpose). The scaffold perspective (organized litigators) has constrained exit (must work within existing legal systems) but organization and visible progress: d ≈ 0.40-0.50, experiencing moderate extraction with agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the tension between naturalizing liability fragmentation as a causation limit (Mountain —'you cannot prove individual responsibility for planetary-scale damages') versus recognizing it as a contingent institutional choice (Tangled Rope — 'we have chosen burden-of-proof allocation, proximate-cause doctrine, and evidentiary standards that collectively suppress liability'). The false summit in the analytical perspective reveals the mechanism: legal institutions deploy mountain language ('causation is impossible') to justify rope/snare behavior (protecting corporate interests, suppressing victim recovery). The resolution is to disaggregate the constraint into: (1) a genuine epistemic limit (correlating individual emissions to specific damages is scientifically difficult but not impossible), and (2) institutional choices (how we allocate burden of proof, what causation standard we use, how we define damages). The second is where extraction lives. By separating these, the mandatrophy resolves: the coordination function (allowing energy markets to operate during legal uncertainty) is real, but it is asymmetrically distributed — corporations benefit from coordination, communities bear the extraction cost. The classification lands at Tangled Rope because both dimensions are present and both are structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causation_standard_threshold,
    'What causation standard (individual strict liability vs. proportional responsibility vs. emitter-class liability) is legally defensible while remaining economically functional?',
    'International comparative law analysis; pilot implementation in high-emission jurisdictions; economic modeling of liability cost pass-through and market stability',
    'Individual strict liability: bankrupts major emitters but establishes clear deterrence. Proportional liability: spreads costs but creates free-rider incentives. Emitter-class liability: feasible economically but requires political consensus. Classification shifts between Snare (strict liability) and Tangled Rope (proportional) depending on threshold chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causation_standard_threshold, empirical, 'Which causation standard balances legal defensibility and economic functionality').

omega_variable(
    temporal_asymmetry_of_knowledge,
    'Does liability apply retrospectively to emissions made before climate science established CO2-harm causation (pre-1980s), or only prospectively (post-consensus science)?',
    'Legislative or judicial precedent clarification; historical analysis of corporate climate knowledge timeline vs. public science consensus; comparison with other tort regimes (asbestos, tobacco, lead)',
    'If retrospective: massive historical liability multiplies extraction, shifts snare to reparative justice. If prospective only: current emitters escape historical damages, reduces extraction but fails justice. Classification and mandatrophy depend critically on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_asymmetry_of_knowledge, preference, 'Temporal scope of climate liability (retrospective vs. prospective)').

omega_variable(
    compensability_vs_prevention,
    'Is the legal standard primarily compensatory (paying damages to victims) or preventive (deterring future emissions through liability threat)?',
    'Policy analysis of damage cap levels vs. emissions reduction incentives; empirical study of corporate response to liability threat in early precedent jurisdictions',
    'Compensatory emphasis: benefits current/past victims but weak emissions deterrent. Preventive emphasis: strong deterrent but may inadequately compensate historical damages. The classification gap between compensation-focused (Rope or Tangled Rope from victim perspective) and deterrence-focused (Snare from emitter perspective) becomes measurable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compensability_vs_prevention, preference, 'Whether liability standard emphasizes compensation or prevention').

omega_variable(
    enforcement_equity_across_nations,
    'Can liability be enforced uniformly across wealthy litigation-rich jurisdictions and poor jurisdictions with weak courts, or does enforcement asymmetry recreate sovereignty issues and extract from poor nations?',
    'Analysis of litigation distribution across jurisdictions; study of corporate legal strategy in forum-shopping; international enforcement agreement outcomes',
    'If enforced only in wealthy jurisdictions: poor and vulnerable nations (most affected by climate) cannot pursue claims. If international enforcement mechanism imposed: raises sovereignty concerns. Either way, creates secondary extraction layer. Suppression and extractiveness both increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_equity_across_nations, empirical, 'Feasibility and equity of cross-border liability enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_liability_legal_standard, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_liab_tr_t0, climate_liability_legal_standard, theater_ratio, 0, 0.45).
narrative_ontology:measurement(clim_liab_tr_t5, climate_liability_legal_standard, theater_ratio, 5, 0.58).
narrative_ontology:measurement(clim_liab_tr_t10, climate_liability_legal_standard, theater_ratio, 10, 0.68).
narrative_ontology:measurement(clim_liab_tr_t15, climate_liability_legal_standard, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(clim_liab_be_t0, climate_liability_legal_standard, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_liab_be_t5, climate_liability_legal_standard, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clim_liab_be_t10, climate_liability_legal_standard, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(clim_liab_be_t15, climate_liability_legal_standard, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_liability_legal_standard, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_liability_legal_standard, carbon_pricing_standard).
narrative_ontology:affects_constraint(climate_liability_legal_standard, corporate_emissions_disclosure).
narrative_ontology:affects_constraint(climate_liability_legal_standard, financial_climate_risk_accounting).

% DUAL FORMULATION NOTE:
% Climate liability is downstream of emissions (carbon_pricing_standard, disclosure_standard) and upstream of financial risk integration (climate_risk_accounting). Each has different ε: liability has higher extractiveness than pricing standards (which are designed to be coordinating mechanisms) but lower theater than disclosure standards (which have become performative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_liability_legal_standard, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
