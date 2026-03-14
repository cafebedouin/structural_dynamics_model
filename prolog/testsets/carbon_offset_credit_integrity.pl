% ============================================================================
% CONSTRAINT STORY: carbon_offset_credit_integrity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carbon_offset_credit_integrity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: carbon_offset_credit_integrity
 *   human_readable: Carbon Offset Credit Integrity Verification and Market Asymmetries
 *   domain: environmental_economics/climate_finance
 *
 * SUMMARY:
 *   The carbon offset credit system represents a structural attempt to
 *   coordinate global climate finance by creating tradeable credits for
 *   verified emissions reductions. However, the system exhibits persistent
 *   asymmetries: developers face incentives to inflate credit claims,
 *   verification is expensive and unevenly distributed, developing nations
 *   bear disproportionate verification burden, and high-emission corporations
 *   can purchase cheap offsets instead of reducing emissions. The
 *   extractiveness has increased from 0.32 to 0.58 over the measurement
 *   interval as the system has grown and fraud patterns have accumulated. The
 *   theater ratio has risen from 0.48 to 0.72, indicating that verification
 *   methodologies are increasingly performative — complex certification
 *   frameworks create legitimacy impression without preventing systematic
 *   fraud. This constraint exhibits all features of Tangled Rope: genuine
 *   coordination function (mobilizing climate finance for conservation,
 *   renewable energy, efficiency projects in developing nations) combined
 *   with asymmetric extraction (offsetters escape abatement costs, developers
 *   capture rents, verification bodies profit from standard complexity). The
 *   suppression is high (0.65) because verification is technically difficult,
 *   information asymmetries favor developers and offsetters, and developing
 *   nations have limited capacity to audit projects or prove fraud.
 *
 * KEY AGENTS:
 *   - Global Emissions Accounting System: Primary victim (powerless/trapped) — cannot exit or correct systematic credit inflation; bears full cost of phantom offsets
 *   - Developing Nation Forest Programs: Secondary victim and partial beneficiary (moderate/constrained) — constrained by revenue dependency on offset sales and verification cost barriers; also genuinely coordinating reforestation
 *   - Carbon Offset Project Developers: Primary beneficiary (institutional/arbitrage) — capture rents through credit inflation and methodological arbitrage; can shift between markets and regulatory regimes
 *   - High-Emission Corporations: Powerful users (powerful/mobile) — benefit from ability to purchase cheap offsets; also constrained by compliance requirements and reputational pressure
 *   - Verification Standards Bodies: Institutional maintainers (institutional/constrained) — benefit from credentialing role while theater ratio increases; constrained by path dependency of established methodologies
 *   - Climate NGOs: Organized enforcers (organized/mobile) — attempt to enforce integrity standards; both coordinate genuine auditing improvements and extract regulatory legitimacy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as necessarily hybrid because offset markets solve a coordination problem (mobilizing climate finance) that would otherwise be harder to address
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carbon_offset_credit_integrity, 0.58).
domain_priors:suppression_score(carbon_offset_credit_integrity, 0.65).
domain_priors:theater_ratio(carbon_offset_credit_integrity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carbon_offset_credit_integrity, extractiveness, 0.58).
narrative_ontology:constraint_metric(carbon_offset_credit_integrity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(carbon_offset_credit_integrity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carbon_offset_credit_integrity, tangled_rope).
narrative_ontology:human_readable(carbon_offset_credit_integrity, "Carbon Offset Credit Integrity Verification and Market Asymmetries").
narrative_ontology:topic_domain(carbon_offset_credit_integrity, "environmental_economics/climate_finance").

domain_priors:requires_active_enforcement(carbon_offset_credit_integrity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carbon_offset_credit_integrity, offset_project_developers).
narrative_ontology:constraint_beneficiary(carbon_offset_credit_integrity, carbon_credit_traders).
narrative_ontology:constraint_beneficiary(carbon_offset_credit_integrity, high_emission_corporations).
narrative_ontology:constraint_victim(carbon_offset_credit_integrity, climate_mitigation_integrity).
narrative_ontology:constraint_victim(carbon_offset_credit_integrity, global_emissions_accounting).
narrative_ontology:constraint_victim(carbon_offset_credit_integrity, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL EMISSIONS ACCOUNTING — Cannot exit verification failures; bears full cost of phantom credits. The integrity of the global carbon ledger is powerless, trapped in a system where false claims accumulate without remedy. No exit option except through collapse of the credit system itself.
constraint_indexing:constraint_classification(carbon_offset_credit_integrity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION FOREST PROGRAMS (TANGLED ROPE) — Constrained by dependency on offset revenue for conservation funding, but also genuinely coordinating reforestation efforts. Faces extraction: must demonstrate additionality under conditions where verification is expensive and asymmetric. Also benefits from market access and climate finance flows. High suppression due to capacity constraints in monitoring and verification.
constraint_indexing:constraint_classification(carbon_offset_credit_integrity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CARBON OFFSET PROJECT DEVELOPERS (ROPE) — Institutional actors with arbitrage options. Experience the constraint as coordination mechanism: standardized verification, methodologies, and registries enable market participation and finance access. Net beneficiaries during favorable conditions. Can shift between markets, methodologies, and regulatory regimes.
constraint_indexing:constraint_classification(carbon_offset_credit_integrity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-EMISSION CORPORATIONS (TANGLED ROPE) — Powerful agents using offsets for compliance and reputation management. Benefit from ability to purchase cheap credits rather than reducing emissions. Also constrained by regulatory requirements and reputational pressure to demonstrate decarbonization. Experience the constraint as both enabling (market access to offsets) and extracting (costs of verification, market volatility, regulatory risk).
constraint_indexing:constraint_classification(carbon_offset_credit_integrity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: VERIFICATION STANDARDS BODIES (PITON) — Gold Standard, Verra, and similar bodies maintain elaborate certification rituals that primarily demonstrate institutional legitimacy rather than preventing fraud at scale. Theater ratio high: certification methodologies are complex and performative, creating impression of rigor without ensuring credit integrity. Constrained by path dependency — existing methodologies are entrenched, alternatives struggle for adoption.
constraint_indexing:constraint_classification(carbon_offset_credit_integrity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED CLIMATE NGOS (TANGLED ROPE) — Organized actors attempting to enforce integrity standards through monitoring, auditing, and public pressure. Mobile: can shift focus between markets, methodologies, and regulatory landscapes. Both coordinate genuine verification improvements and extract regulatory rents through standard-setting. Genuine coordination function: independent auditing does catch some frauds. Asymmetric extraction: standards bodies benefit from credentialing role.
constraint_indexing:constraint_classification(carbon_offset_credit_integrity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational analysis reveals this as a genuine hybrid: offset markets do coordinate climate finance (beneficiaries and victims both need the mechanism) while simultaneously enabling asymmetric extraction (high-emission actors can purchase cheap credits instead of reducing emissions; developing nations bear verification burden). The constraint persists because it solves a coordination problem (mobilizing climate finance) while creating an extraction problem (shifting abatement costs toward vulnerable populations).
constraint_indexing:constraint_classification(carbon_offset_credit_integrity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carbon_offset_credit_integrity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carbon_offset_credit_integrity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carbon_offset_credit_integrity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carbon_offset_credit_integrity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carbon_offset_credit_integrity, TR),
    TR >= 0.70.

:- end_tests(carbon_offset_credit_integrity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. The baseline value of 0.32 reflected the system's early-stage design intent: coordinate climate finance for development-compatible emissions reductions. The current value (0.58) reflects accumulated evidence that the system enables substantial asymmetric extraction: (1) Additionality is weakly verifiable, creating room for phantom credits; (2) Developers have incentives to maximize credit claims rather than verify rigor; (3) High-emission actors purchase cheap offsets instead of reducing emissions; (4) Developing nations bear verification burden without capacity; (5) Methodological complexity creates arbitrage opportunities. The rise to 0.62 at the end reflects recent scandals revealing large-scale fraud (e.g., forest protection projects that weren't actually protecting forests). Suppression (0.65): High and stable. Verification is technically difficult (counterfactuals are unobservable), information asymmetries favor developers and offsetters, developing nations have limited audit capacity, and the sheer scale of the market (billions of dollars, millions of projects) outpaces monitoring. Theater ratio (0.68, rising to 0.72): Moderate-high. Certification methodologies are elaborate and create legitimate impression without preventing fraud. Complex frameworks (CDM methodologies, Verra standards) require specialized expertise, creating barriers to public scrutiny and developer compliance claims that are technically defensible even when substantially false. The rise reflects increasing complexity of standards as methodologies attempt to close loopholes, which paradoxically increases theater while fraud continues.
 *
 * PERSPECTIVAL GAP:
 *   Perspectival gaps emerge along two dimensions: (1) Power asymmetry: powerful actors (corporations, developers) experience the constraint as coordinating mechanism; powerless agents (accounting integrity) experience it as extraction. (2) Verification burden distribution: high-capacity regions and wealthy offsetters have capacity to navigate complex methodologies; low-capacity developing nations and forest communities bear verification cost without capacity. The gap between the developer's Rope and the global ledger's Snare is the constraint's structural core.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality pipeline computes d from beneficiary/victim status, power level, and exit options. Developers benefit from market access and credit inflation (beneficiary status with arbitrage options → low d → negative effective extraction from their perspective). High-emission corporations benefit from cheap offset purchases (beneficiary status with mobile options → low d). Developing nations are both beneficiaries of climate finance and victims of verification burden (mixed d ≈ 0.55). The global emissions accounting system is a pure victim with no exit (victim status trapped → d ≈ 0.95). This produces the perspectival gap: same ε = 0.58, same suppression = 0.65, but dramatically different classifications depending on observer position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy between 'is this coordination or extraction?' by showing that it is genuinely both. The offset market coordinates global climate finance that would otherwise be difficult to mobilize. Developing nations would have fewer conservation projects without offset revenue. The coordination function is real. Simultaneously, the constraint enables systematic asymmetric extraction: high-emission actors escape abatement costs, developers inflate credit claims, and the global accounting system accumulates phantom offsets. Both are true. The Tangled Rope classification reflects this: χ (effective extraction) = 0.58 × f(d) × σ(S) varies across perspectives because d and the coordination function vary. For developers, the coordination benefit and low d make χ modest or negative. For the global system, the victim status and high d make χ extreme. For developing nations, the mixture of benefit and burden produces moderate χ. The constraint cannot be classified as pure Rope (genuine coordination exists) or pure Snare (extraction does exist). It is Tangled Rope because it solves a real coordination problem (mobilizing climate finance) while creating real extraction asymmetry (shifting abatement burden toward weaker actors).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    additionality_verification_impossibility,
    'Can additionality be verified ex-post when the counterfactual (what would have happened without the project) is inherently unobservable?',
    'Comparison of project-attributed emissions reductions against matched control regions and historical trajectories; detection of systematic bias in additionality claims via statistical comparison of claimed vs actual outcomes',
    'If additionality is fundamentally unverifiable: all credits are phantom to some degree, making the constraint a pure Snare with no genuine coordination function. If verifiable at statistical bounds: the constraint is genuine Tangled Rope with measurable but non-zero fraud margin.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(additionality_verification_impossibility, conceptual, 'Fundamental verifiability of additionality claims').

omega_variable(
    leakage_attribution_boundary,
    'How should leakage (emissions reductions in one location causing increased emissions elsewhere) be attributed and compensated? Where do project boundaries end?',
    'Spatial analysis of emissions flows; detection of systematic underestimation of leakage in existing methodologies; comparison of claimed vs empirical leakage rates across project types',
    'If leakage is systematically underestimated by >20%: base extractiveness should increase to 0.68+ (Snare from more perspectives). If leakage accounting is reasonable: supports Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leakage_attribution_boundary, empirical, 'Proper attribution of leakage across project boundaries').

omega_variable(
    developer_moral_hazard_incentives,
    'Do the financial incentives for offset developers systematically bias them toward claiming credits where verification is weakest and fraud risk is highest?',
    'Analysis of credit distribution across project types and verification rigor; comparison of fraud detection rates across methodologies; tracking of developer revenue correlation with verification difficulty',
    'If systematic bias confirmed: extraction component increases (developers extracting rent via fraud), suppression increases (asymmetric information advantages developers). Classification may shift toward Snare from developer perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developer_moral_hazard_incentives, empirical, 'Moral hazard in offset developer incentives').

omega_variable(
    permanent_vs_reversible_storage_accounting,
    'How should credits for reversible abatement (e.g., forest conservation subject to logging risk, soil carbon subject to disturbance) be accounted relative to permanent abatement (e.g., industrial efficiency)?',
    'Historical analysis of reversal rates for carbon storage projects; empirical comparison of permanence across project types; development of risk-adjusted credit accounting methodology',
    'If reversal rates are substantial and unmeasured: base extractiveness should increase (fake permanence equals fake credits). Theater ratio may be systematically understated if methodologies ignore reversibility risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanent_vs_reversible_storage_accounting, empirical, 'Accounting for reversibility and permanence in carbon storage projects').

omega_variable(
    north_south_verification_asymmetry,
    'Are offset credits from low-capacity developing nations systematically less verifiable than credits from high-capacity regions, creating a structural North-South extraction dynamic?',
    'Comparative analysis of verification rigor, fraud detection rates, and credit invalidation across regions; cost analysis of verification burden relative to project scale; attribution of invalidated credits by developer nation',
    'If asymmetry confirmed: geographic dimension of extraction becomes central; constraint shows North-South extraction pattern. Developing nation perspectives should classify higher on extraction scale. Suppression may reflect capacity barriers rather than choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(north_south_verification_asymmetry, empirical, 'North-South asymmetry in verification capacity and integrity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carbon_offset_credit_integrity, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(offset_tr_t0, carbon_offset_credit_integrity, theater_ratio, 0, 0.48).
narrative_ontology:measurement(offset_tr_t5, carbon_offset_credit_integrity, theater_ratio, 5, 0.58).
narrative_ontology:measurement(offset_tr_t10, carbon_offset_credit_integrity, theater_ratio, 10, 0.68).
narrative_ontology:measurement(offset_tr_t12, carbon_offset_credit_integrity, theater_ratio, 12, 0.72).

% Extraction over time
narrative_ontology:measurement(offset_be_t0, carbon_offset_credit_integrity, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(offset_be_t5, carbon_offset_credit_integrity, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(offset_be_t10, carbon_offset_credit_integrity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(offset_be_t12, carbon_offset_credit_integrity, base_extractiveness, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carbon_offset_credit_integrity, resource_allocation).
narrative_ontology:boltzmann_floor_override(carbon_offset_credit_integrity, 0.18).
narrative_ontology:affects_constraint(carbon_offset_credit_integrity, corporate_decarbonization_theater).
narrative_ontology:affects_constraint(carbon_offset_credit_integrity, developing_nation_adaptation_finance_access).
narrative_ontology:affects_constraint(carbon_offset_credit_integrity, carbon_leakage_displacement).

% DUAL FORMULATION NOTE:
% The carbon offset constraint family decomposes into three structurally distinct claims: (1) whether offset projects genuinely reduce emissions (empirical, offset_project_additionality, ε ≈ 0.45); (2) whether offset markets coordinate climate finance (structural, carbon_offset_credit_integrity, ε ≈ 0.58, this story); (3) whether offset purchases enable corporate climate claims (narrative, corporate_decarbonization_theater, ε ≈ 0.62). Each story has different base properties, different victim groups, and different measurement signatures. Upstream: additionality verification constrains credit legitimacy. Downstream: corporate theater depends on offset market opacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carbon_offset_credit_integrity, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
