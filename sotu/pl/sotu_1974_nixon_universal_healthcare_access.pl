% ============================================================================
% CONSTRAINT STORY: sotu_1974_nixon_universal_healthcare_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1974_nixon_universal_healthcare_access, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1974_nixon_universal_healthcare_access
 *   human_readable: Nixon's 1974 Universal Healthcare Access Commitment
 *   domain: healthcare_policy/social_entitlement
 *
 * SUMMARY:
 *   Nixon's 1974 commitment to universal healthcare access restructures the
 *   entire health provision system from market-rationed to guaranteed-access
 *   model. This constraint exhibits the signature structural tensions of
 *   modern entitlement programs: genuine coordination function (population
 *   health planning, preventive care infrastructure) combined with
 *   significant extraction mechanisms (margin compression on providers, tax
 *   burden on high-income workers, market elimination for private insurers).
 *   The constraint's theater ratio reflects the gap between access guarantee
 *   and actual care quality/availability — universal access is announced and
 *   institutionalized, but implementation quality depends on enforcement
 *   capacity and provider willingness. The extractiveness measurement
 *   increases over the interval as the full systemic cost (not just direct
 *   redistribution but also innovation suppression and bureaucratic overhead)
 *   becomes visible. This is a canonical Tangled Rope: the coordination
 *   benefit (universal access, health data integration, population health
 *   planning) is real and substantial; the extraction mechanism (margin
 *   control, tax burden concentration, market elimination) is also real and
 *   substantial. Neither dominates; both coexist in the same institutional
 *   structure.
 *
 * KEY AGENTS:
 *   - Uninsured populations: Primary beneficiary (powerless/trapped) — gain access guarantee but face utilization barriers, provider shortages, and quality rationing
 *   - Underinsured populations: Primary beneficiary (powerless/trapped) — gain comprehensive coverage but transition to assigned providers and utilization review
 *   - Private health insurers: Primary victim (powerful/mobile/constrained) — market role eliminated; extraction despite nominal power because exit option (abandon healthcare entirely) is the only real alternative
 *   - Healthcare providers (physicians, hospitals): Mixed victim/beneficiary (moderate/constrained) — benefit from guaranteed patient flow and elimination of uncompensated care; suffer from margin control and utilization review
 *   - Pharmaceutical manufacturers: Secondary victim (powerful/constrained) — face price controls and margin compression; cannot exit domestic market without abandoning revenue
 *   - Federal health administration: Primary beneficiary (institutional/arbitrage) — gains regulatory authority, data access, coordination mandate; has arbitrage exit and net beneficial position
 *   - Labor unions: Secondary beneficiary (organized/constrained) — benefit from decoupling healthcare from employer leverage; constrained by tax burden and loss of negotiated benefit differentiation
 *   - High-income taxpayers: Secondary victim (powerful/constrained) — bear proportionally higher tax burden; have exit options (jurisdictional arbitrage, political opposition) but constrained by system legitimacy
 *   - Analytical observer: Risks naturalizing political choice as economic law (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1974_nixon_universal_healthcare_access, 0.58).
domain_priors:suppression_score(sotu_1974_nixon_universal_healthcare_access, 0.65).
domain_priors:theater_ratio(sotu_1974_nixon_universal_healthcare_access, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1974_nixon_universal_healthcare_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1974_nixon_universal_healthcare_access, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1974_nixon_universal_healthcare_access, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1974_nixon_universal_healthcare_access, tangled_rope).
narrative_ontology:human_readable(sotu_1974_nixon_universal_healthcare_access, "Nixon's 1974 Universal Healthcare Access Commitment").
narrative_ontology:topic_domain(sotu_1974_nixon_universal_healthcare_access, "healthcare_policy/social_entitlement").

domain_priors:requires_active_enforcement(sotu_1974_nixon_universal_healthcare_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1974_nixon_universal_healthcare_access, uninsured_populations).
narrative_ontology:constraint_beneficiary(sotu_1974_nixon_universal_healthcare_access, underinsured_populations).
narrative_ontology:constraint_beneficiary(sotu_1974_nixon_universal_healthcare_access, preventive_care_accessibility).
narrative_ontology:constraint_victim(sotu_1974_nixon_universal_healthcare_access, private_insurers).
narrative_ontology:constraint_victim(sotu_1974_nixon_universal_healthcare_access, healthcare_providers_margin_pressure).
narrative_ontology:constraint_victim(sotu_1974_nixon_universal_healthcare_access, high_income_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED LOW-INCOME WORKER (SNARE) — Despite guaranteed access promise, individuals lack leverage within the system. They face appointment delays, geographic access barriers, and provider choice restrictions under universal model. Cannot exit or arbitrage; trapped within assigned provider networks. The constraint guarantees access but not quality of experience or responsiveness.
constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIMARY CARE PHYSICIAN (TANGLED ROPE) — Genuine coordination function: universal system enables population health planning, preventive care infrastructure, and clinical coordination across fragmented delivery. BUT also constrained by price controls, utilization review, and income caps. Cannot easily exit (career already invested in practice) but has some mobility (private concierge practice, geographic relocation). Extraction runs alongside coordination benefit.
constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIVATE HEALTH INSURANCE COMPANY (SNARE) — Despite nominal power and mobility, faces complete extraction: market share eliminated, profit margins zeroed, competitive advantage nullified. Can exit only by exiting industry entirely. The constraint is designed to eliminate this agent's role. Maximum experienced extraction despite powerful status because exit options are illusory — cannot stay in healthcare and retain value.
constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL HEALTH ADMINISTRATION (ROPE) — Benefits from new administrative role, expanded regulatory authority, coordination mandate. Experiences constraint as legitimate coordination mechanism: centralizing health data, standardizing protocols, managing population-level health metrics. Has arbitrage exit (can delegate functions, contract implementation) and sees net benefit. Constraint expands institutional capacity.
constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR UNION COALITION (TANGLED ROPE) — Organized agents benefit from decoupling healthcare from employer-based system, reducing management leverage over workforce and reducing union negotiation burden. But also constrained: universal system raises tax burden on members, reduces union-negotiated benefit differentiation, shifts power from workplace negotiation to state policy. Coalition has exit option (oppose and protect employer-based system) but ideology/interests favor reform. Mixed extraction and coordination.
constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PHARMACEUTICAL MANUFACTURING (SNARE) — Faces price controls under universal system, reduced pricing power, margin compression. Despite nominal power and significant capital, cannot exit the domestic market without abandoning entire national revenue stream. Can only constrain impact through regulatory lobbying and supply chain tactics. Extraction through margin control despite powerful nominal status.
constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: PATIENT ADVOCACY COALITION (SCAFFOLD) — Sees universal healthcare as temporary bridge to more complete health equity. Organized agents (disease groups, senior advocacy, disability advocates) experience constraint as scaffolding: system builds infrastructure for care but contains sunset logic — as health outcomes improve and disease burden declines, system may evolve toward lower taxation and reduced state role. Coalition has leverage and sees exit path (achieved health equity reduces need for universal system).
constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: MEDICAL PROFESSIONAL CREDENTIALING (PITON) — Under universal system, licensing and specialty credentialing persist as vestigial gatekeeper mechanisms. Licensed physicians receive guaranteed patient flow under universal system, reducing need for market differentiation. Theater ratio increases: credentialing maintenance persists through institutional inertia (AMA gatekeeping, specialty board certification) despite reduced functional necessity. Arbitrage exit available (credentialing bodies can delegate, reduce requirements) but maintained through professional inertia.
constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From analytical/civilizational perspective, universal healthcare appears to encounter immutable economic limits: finite resources, scarcity of specialized providers, tradeoffs between access/quality/cost. The constraint appears to violate irreducible economic realities. However, structural data reveals this as false summit: beneficiaries exist (uninsured populations gain access), victims exist (private insurers eliminated), and active enforcement required. The 'immutable economic law' naturalizes political choice about who bears systemic costs.
constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1974_nixon_universal_healthcare_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1974_nixon_universal_healthcare_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1974_nixon_universal_healthcare_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1974_nixon_universal_healthcare_access, TR),
    TR >= 0.70.

:- end_tests(sotu_1974_nixon_universal_healthcare_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting that the constraint extracts from multiple victim populations (private insurers face complete elimination; providers face margin pressure; high-income taxpayers face tax burden) while benefiting uninsured/underinsured populations substantially. The value reflects that extraction is asymmetric and partially masked: private insurer elimination is explicit and complete (high extraction), but provider margin pressure is bounded by political pressure (moderate extraction), and tax burden is partially offset by reduced out-of-pocket spending for working-class taxpayers (moderate extraction for high-income earners). Suppression (0.65): Moderate-high. Significant barriers include regulatory enforcement capacity constraints, provider supply inelasticity in specialized fields, geographic access challenges, and political pressure to maintain provider profitability. Victims (especially private insurers and margin-constrained providers) have limited exit options; beneficiaries have no exit (guaranteed access is not optional). Theater ratio (0.48): Moderate, reflecting that the constraint has substantial genuine coordination function (health data integration, preventive care infrastructure, population health planning) but also significant performative elements. Access is guaranteed in law but dependent on enforcement, provider participation, and capacity utilization. Early implementation theater is moderate; likely increases over time as implementation pressure mounts and actual capacity constraints become visible. Baseline measurement at t=0 reflects pre-implementation optimism; increase by t=6 reflects emerging capacity constraints and enforcement difficulty.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Uninsured populations see a Snare (access guaranteed but trapped within rationed system with limited provider choice). Healthcare providers see Tangled Rope (genuine coordination benefit from patient flow guarantee and elimination of uncompensated care, but constrained by margin pressure and utilization review). Private insurers see Snare (complete market elimination despite nominal power — exit is illusory). Federal health administration sees Rope (genuine coordination mechanism with net benefit). Labor unions see Tangled Rope (benefit from decoupling but constrained by tax burden). High-income taxpayers see Snare (burdened by proportionally higher tax while receiving no direct benefit). The analytical observer risks seeing Mountain (immutable economic limits) but structural data reveals false summit: beneficiaries, victims, and active enforcement are all present. The perspectival gaps stem from real differences in structural position: those with exit options (administrators, unions with political voice) see lower effective extraction; those without exit (uninsured populations, private insurers trapped in healthcare market) see higher extraction or complete elimination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Uninsured populations are beneficiaries with trapped exit: low d initially (benefit from access guarantee) but high extraction experienced because they cannot arbitrage — they experience the constraint's negative qualities (rationing, provider shortages) without escape. Private insurers are victims with mobile-but-illusory exit: high d (full target of constraint elimination), f(d) approaches maximum, complete extraction despite nominal power because the exit option (abandon healthcare industry) nullifies their power. Healthcare providers are mixed: beneficiary aspect (guaranteed patient flow, eliminated uncompensated care) with victim aspect (margin control, utilization review) — constrained exit means they experience moderate d and moderate chi. Federal health administration is beneficiary with arbitrage exit: low d, negative chi (institutional benefit from new regulatory role). High-income taxpayers are victims with some arbitrage mobility (jurisdictional relocation, political opposition): moderate d, moderate chi. The pipeline computes these automatically from structural declarations; chi values scale by f(d) × σ(S) with scope modifier for national constraint (σ ≈ 1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely both coordination AND extraction, not one masquerading as the other. Beneficiaries (uninsured populations) perceive pure coordination: the constraint solves their pre-existing exclusion problem. Victims (private insurers) perceive pure extraction: the constraint eliminates their market without coordination benefit. The analytical challenge is that the constraint's legitimacy depends on whether one values access guarantee (coordination frame) over market efficiency (extraction frame), and this is a value choice, not an empirical fact. The mandatrophy is properly resolved by: (1) declaring all three perspectives (beneficiary=Rope, victim=Snare, analytical=Tangled Rope), (2) showing that the beneficiary and victim perspectives are not wrong but partial, (3) recognizing that the constraint genuinely performs a coordination function (health data integration, preventive infrastructure) while genuinely extracting from identifiable victims (private insurers face market elimination, providers face margin pressure, high-income taxpayers face tax burden), (4) noting that the extraction is not hidden or disguised—it is explicit and intentional—which is the key structural signature of Tangled Rope rather than Snare. A Snare hides extraction behind a coordination narrative; a Tangled Rope openly enacts both coordination and extraction and relies on asymmetric power to maintain both. Nixon's universal healthcare commitment does exactly this: announces both the access guarantee (coordination) and the market restructuring (extraction) and enforces both despite victim resistance (private insurers cannot prevent this; providers cannot prevent margin control through political means).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_under_universalization,
    'Does universal access guarantee quality care or merely access to substandard care rationing?',
    'Post-implementation longitudinal tracking of health outcomes by condition severity, comorbidity, age cohort; comparison of wait times and provider availability under universal vs market model; mortality and morbidity rates for conditions sensitive to care delays (cancer diagnosis, cardiac intervention, preventive screening)',
    'If quality maintained: constraint is pure Rope (coordination benefit exceeds extraction cost). If quality degrades significantly: constraint becomes Snare (access guarantee masked by hollowed-out care quality).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quality_under_universalization, empirical, 'Whether universal access maintains care quality or devolves to rationing').

omega_variable(
    innovation_preservation,
    'Does margin compression and price control eliminate pharmaceutical and medical device R&D incentives, creating long-term innovation collapse?',
    'Comparison of drug approval rates, new indications, medical device patents before/after universalization; international R&D spending allocation with price control regimes; measurement of drug price elasticity of R&D investment',
    'If R&D survives robust: extraction is bounded (margin pressure is real but sustainable). If R&D collapses within 10-15 years: hidden systemic cost emerges (future care quality suffers), reclassifying as severe Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_preservation, empirical, 'Whether price controls eliminate pharmaceutical R&D incentives').

omega_variable(
    political_sustainability,
    'Is universal healthcare politically sustainable when middle-class tax burden rises to fund care for populations with lower political power, and immediate medical outcomes improvements are asymmetric by income cohort?',
    'Longitudinal polling of tax burden perception and willingness to fund system; election results in districts with high tax burden but low immediate benefit; measurement of cross-income subsidy acceptance and political backlash; comparing early vs mature system sentiment',
    'If politically fragile: constraint exhibits latent Snare dynamics (extraction of middle-class tax burden creates covert victim population). Potential reclassification from Tangled Rope to Snare. If robust: organizational capacity of health beneficiaries sufficient to maintain coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_sustainability, preference, 'Political sustainability of universal healthcare funding model').

omega_variable(
    enforcement_mechanism_feasibility,
    'Can federal enforcement of universal access overcome institutional capacity constraints in medical training, provider distribution, and geographic access in rural/low-density regions?',
    'Measurement of provider availability by region post-universalization; wait time variances by urban/rural location; medical school capacity expansion requirements; federal budget allocation for enforcement infrastructure; comparison of administrative overhead in universal systems with different enforcement designs',
    'If enforcement successful: constraint is Tangled Rope (coordination + extraction sustainable). If enforcement fails: constraint degrades to theater (universal access guaranteed but geographically unavailable), reclassifying toward Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_feasibility, empirical, 'Feasibility of enforcing universal access across geographic regions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1974_nixon_universal_healthcare_access, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nih74_tr_t0, sotu_1974_nixon_universal_healthcare_access, theater_ratio, 0, 0.28).
narrative_ontology:measurement(nih74_tr_t3, sotu_1974_nixon_universal_healthcare_access, theater_ratio, 3, 0.38).
narrative_ontology:measurement(nih74_tr_t6, sotu_1974_nixon_universal_healthcare_access, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(nih74_be_t0, sotu_1974_nixon_universal_healthcare_access, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nih74_be_t3, sotu_1974_nixon_universal_healthcare_access, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(nih74_be_t6, sotu_1974_nixon_universal_healthcare_access, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1974_nixon_universal_healthcare_access, resource_allocation).
narrative_ontology:affects_constraint(sotu_1974_nixon_universal_healthcare_access, employer_based_health_insurance_lock).
narrative_ontology:affects_constraint(sotu_1974_nixon_universal_healthcare_access, pharmaceutical_pricing_regulation).
narrative_ontology:affects_constraint(sotu_1974_nixon_universal_healthcare_access, hospital_certificate_of_need).

% DUAL FORMULATION NOTE:
% This constraint is an attempt to displace the pre-existing employer-based health insurance constraint. Both constraints regulate healthcare access and pricing, but through different mechanisms: employer-based system uses employment relationship as rationing mechanism; universal system uses centralized resource allocation. They are competing institutional arrangements for the same underlying problem. Network edges link this constraint to downstream constraints that depend on its success (pharmaceutical regulation requires universal system to set prices; certificate-of-need reflects universal system's capacity constraints).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
