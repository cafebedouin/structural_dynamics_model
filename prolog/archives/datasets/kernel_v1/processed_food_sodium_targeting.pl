% ============================================================================
% CONSTRAINT STORY: processed_food_sodium_targeting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_processed_food_sodium_targeting, []).

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
 *   constraint_id: processed_food_sodium_targeting
 *   human_readable: Processed Food Sodium Targeting Coordination and Extraction
 *   domain: public_health/food_industry/regulation
 *
 * SUMMARY:
 *   Processed food sodium targeting exemplifies a constraint that presents
 *   itself as a public health coordination mechanism while operating as
 *   embedded extraction disguised by voluntary industry compliance. The
 *   coordination function is genuine: processed foods enable food security
 *   for low-income populations, and sodium functions as an efficient, cheap
 *   preservative and flavor enhancer that stabilizes supply chains and
 *   delivers affordable calories. However, the constraint exhibits asymmetric
 *   extraction: sodium-related health costs (hypertension, stroke, chronic
 *   kidney disease) concentrate on the populations most dependent on cheap
 *   processed foods — precisely those with lowest exit capacity. The
 *   voluntary compliance framework (industry pledges, soft targets, labeling)
 *   creates performative regulation that gives the appearance of coordination
 *   while permitting continued extraction. Rising theater ratio (0.48 → 0.64
 *   over 20 years) reflects increasing reliance on symbolic compliance rather
 *   than functional reduction. Rising suppression (0.55 → 0.68) reflects
 *   deepening barriers: food cost inflation makes low-income consumers more
 *   dependent on cheap processed foods, regulatory capture prevents binding
 *   standards, and knowledge asymmetry hides sodium in complex serving-size
 *   labeling.
 *
 * KEY AGENTS:
 *   - Low-income consumers with hypertension: Primary victims (powerless/trapped) — food cost constraints force reliance on high-sodium products; bear full health burden with no exit capacity
 *   - Processed food manufacturers: Primary beneficiaries (institutional/arbitrage) — high sodium enables cheap production, extended shelf life, and preservative function; capture regulatory process to prevent binding standards
 *   - Public health systems and epidemiologists: Secondary victim (moderate/constrained) — must enable food security while managing sodium-disease burden; constrained by regulatory gaps and lobbying
 *   - Public health advocacy organizations (WHO, AHA, consumer groups): Organized agents (organized/constrained) — see reformulation and enforcement as solvable problems with sunset; building coalition for stronger standards
 *   - Regulatory agencies (FDA, USDA, state departments of health): Institutional actors (institutional/arbitrage) — maintain voluntary framework despite knowing it is performative; lack political capital for binding standards
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choices (voluntary frameworks, captured regulation) as inherent to food economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(processed_food_sodium_targeting, 0.58).
domain_priors:suppression_score(processed_food_sodium_targeting, 0.68).
domain_priors:theater_ratio(processed_food_sodium_targeting, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(processed_food_sodium_targeting, extractiveness, 0.58).
narrative_ontology:constraint_metric(processed_food_sodium_targeting, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(processed_food_sodium_targeting, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(processed_food_sodium_targeting, tangled_rope).
narrative_ontology:human_readable(processed_food_sodium_targeting, "Processed Food Sodium Targeting Coordination and Extraction").
narrative_ontology:topic_domain(processed_food_sodium_targeting, "public_health/food_industry/regulation").

domain_priors:requires_active_enforcement(processed_food_sodium_targeting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(processed_food_sodium_targeting, processed_food_manufacturers).
narrative_ontology:constraint_beneficiary(processed_food_sodium_targeting, low_income_food_security).
narrative_ontology:constraint_victim(processed_food_sodium_targeting, hypertensive_populations).
narrative_ontology:constraint_victim(processed_food_sodium_targeting, chronic_disease_burden).
narrative_ontology:constraint_victim(processed_food_sodium_targeting, public_health_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME CONSUMER WITH HYPERTENSION (SNARE) — Trapped by food cost constraints and limited retail access. Cheap processed foods are the only affordable option; sodium-loaded products are unavoidable. Bears full health cost (hypertension, stroke, kidney disease) with no exit capacity. Maximum extraction without coordination benefit.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH SYSTEMS (TANGLED ROPE) — Genuine coordination: processed foods enable food security for low-income populations and stabilize supply chains. But asymmetric extraction: sodium-related disease burden (hypertension, CKD, stroke) concentrates on powerless populations. Public health system must both enable food access AND manage sodium-disease consequences. Constrained by regulatory gaps and industry resistance to reformulation.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROCESSED FOOD MANUFACTURERS (ROPE) — Primary beneficiary (institutional/arbitrage). High sodium enables product preservation, enhances flavor perception with minimal cost, extends shelf life, enables cheap production. Experiences the constraint as pure coordination: sodium stabilizes supply chains and delivers affordable calories. Net beneficiary — extraction flows toward this agent. Arbitrage exit: can reformulate (high cost) or lobby for weak standards.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC HEALTH ADVOCACY COALITION (SCAFFOLD) — Organized agents (WHO, American Heart Association, consumer health groups) see sodium targeting as a solvable coordination problem with a sunset: reformulation targets and enforcement can reduce population sodium intake without sacrificing food security. Theater is moderate — regulatory frameworks (voluntary sodium targets, labeling mandates) create measurable goals. Exit path exists: progressive reformulation, investment in low-sodium processing, alternative flavor technologies. Sunset logic: within 15-20 years, lower-sodium production becomes competitive cost through scale.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCIES (PITON) — Performative compliance mechanisms (voluntary sodium reduction targets, industry pledges, labeling requirements) persist through institutional inertia despite minimal enforcement. Theater ratio (0.64) reflects that voluntary frameworks lack teeth: manufacturers miss targets with no penalty, labeling hides sodium in serving sizes, industry self-monitoring replaces independent verification. The regulatory apparatus sees its own degradation — knows voluntary compliance is theater but cannot replace it without industry opposition.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FOOD SYSTEM NATURAL LAW (MOUNTAIN) — From a civilizational perspective, sodium in processed foods appears immutable: preserving cheap calories for food-insecure populations requires sodium or equivalent preservation technology. The constraint looks like a law of food chemistry and economics: low-cost shelf-stable nutrition inherently requires high sodium or high sugar. However, structural data reveals this as false summit — the constraint is contingent on cost structures, regulatory laxity, and industry lobbying, not on physics or chemistry. Reformulation is technologically feasible; the barriers are economic and political.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(processed_food_sodium_targeting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(processed_food_sodium_targeting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(processed_food_sodium_targeting, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(processed_food_sodium_targeting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(processed_food_sodium_targeting, TR),
    TR >= 0.70.

:- end_tests(processed_food_sodium_targeting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Sodium provides genuine coordination benefit (cheap food, food security, supply chain stability) worth approximately 0.15-0.20 in functional coordination value. The remaining 0.38-0.43 is asymmetric extraction concentrated on powerless agents through food cost constraints and regulatory laxity. The measurement trajectory (0.42 → 0.58) reflects increasing extraction as reformulation becomes technically and economically feasible but is prevented by regulatory capture and industry resistance. Suppression (0.68): High. Multiple layers: food cost constraints trap low-income consumers; knowledge asymmetry hides sodium in serving-size labeling; regulatory capture prevents binding standards; industry lobbying creates artificial technical barriers; switching costs (limited retail access, time poverty) prevent seeking alternatives. Theater ratio (0.64): Moderate-high. Voluntary sodium reduction pledges, industry self-monitoring, and labeling requirements create visible compliance activity (0.64 theater) without functional outcome. The gap between pledge and actual reformulation reflects that voluntary frameworks lack enforcement mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The manufacturer's rope perspective sees a functional coordination mechanism: sodium solves preservation and flavor problems, enabling cheap production. The low-income consumer's snare perspective sees inescapable extraction: no affordable alternatives exist, health burden is certain, and regulatory protections are theatrical. The public health system sees tangled_rope: both coordination (food security) and extraction (disease burden) are structural. The advocacy coalition sees scaffold: reformulation is feasible with political will, and enforcement timelines create a sunset. The regulator sees piton: voluntary frameworks are known to be performative but cannot be replaced without industry opposition. The analytical observer risks mountain: 'cheap food requires sodium, therefore sodium is inevitable' — naturalizing a contingent political choice. The perspectival gap between snare (victim) and rope (beneficiary) is maximal: same constraint, opposite structural experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (processed food manufacturers) with arbitrage exit derives d ≈ 0.12, producing negative f(d) ≈ -0.01. Manufacturers experience the constraint as pure coordination — sodium stabilizes their operations. Victims (low-income hypertensive populations) with trapped exit derive d ≈ 0.95, producing high f(d) ≈ 1.42. They experience maximum extraction. The tangled_rope type requires beneficiaries (yes: manufacturers), victims (yes: hypertensive populations), and active enforcement (yes: regulatory bodies, though performative). The constraint's classification as tangled_rope reflects both genuine coordination (food security function) and genuine extraction (health cost asymmetry), with enforcement machinery that is active but degraded (voluntary compliance).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by disambiguating the functional from the extractive components. The genuine coordination function (food security through cheap processed foods) is about 15-20% of extractiveness; the remainder is extraction enabled by regulatory failure. The constraint is NOT intrinsically snare (as pure extraction) nor intrinsically rope (as pure coordination). It is tangled_rope because both functions coexist and neither can be removed without redesigning the constraint. Removing extraction (banning high-sodium products) collapses food security for low-income populations. Removing coordination (no food security function) is false — the coordination is real. The resolution path is not to eliminate one function but to decouple them: maintain food security through alternative mechanisms (direct subsidies, low-sodium reformulation, produce access) while removing the extraction mechanism (binding sodium standards, enforcement, reformulation investment). The false-summit mountain perspective ('sodium is inevitable to food economics') is revealed as political cover for regulatory capture, not as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformulation_cost_feasibility,
    'Is low-sodium reformulation technologically and economically viable at scale without significantly raising consumer food costs?',
    'Historical case studies (UK, Denmark, Chile salt reduction programs); cost-benefit analysis of reformulation vs. premium pricing; market penetration data for low-sodium product lines',
    'If feasible at scale: scaffold sunset is structurally real — coordinated reformulation reduces extraction. If costs remain prohibitive: constraint persists as tangled_rope indefinitely and false-summit mountain classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformulation_cost_feasibility, empirical, 'Feasibility and cost of large-scale reformulation').

omega_variable(
    food_security_decoupling,
    'Can food security for low-income populations be maintained without relying on high-sodium processed foods? Are alternative low-cost, low-sodium pathways (fresh produce access, subsidized whole foods, community kitchens) scalable?',
    'Comparative analysis of food security outcomes in jurisdictions with different support models; cost analysis of direct subsidy vs. cheap processed food provision; replication viability of produce-access programs at national scale',
    'If decoupling possible: snare classification is contingent on current policy structure, not inevitable. New perspectives emerge (e.g., food security coordination without extraction). If decoupling fails: food security and sodium extraction remain structurally coupled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(food_security_decoupling, empirical, 'Whether food security and sodium extraction are structurally coupled').

omega_variable(
    voluntary_compliance_theater_measurement,
    'Do voluntary sodium reduction pledges and targets actually reduce population sodium intake, or do they function primarily as regulatory theater without functional impact?',
    'Time-series analysis of sodium reformulation rates vs. voluntary target commitments; cross-national comparison of voluntary vs. mandatory reduction frameworks; population-level urinary sodium excretion trends post-pledge',
    'If theater confirmed: piton classification validated. If voluntary frameworks show genuine compliance: constraint reclassifies toward rope. If mixed results by segment: tangled_rope perspective confirmed with heterogeneous enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_compliance_theater_measurement, empirical, 'Effectiveness of voluntary sodium reduction pledges').

omega_variable(
    sodium_substitution_safety,
    'Are potassium-based salt substitutes a safe, scalable alternative preservative for processed foods without introducing new health risks (hyperkalemia in CKD populations)?',
    'RCT data on long-term potassium salt substitution; pharmacoepidemiologic analysis of hyperkalemia risk in different populations; feasibility at commercial scale',
    'If safe and scalable: reformulation path is clear, scaffold sunset becomes concrete. If safety concerns persist: manufacturers have legitimate technical barrier to reformulation, constraint persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sodium_substitution_safety, empirical, 'Safety and scalability of sodium-potassium salt substitutes').

omega_variable(
    industry_lobbying_capture,
    'Do food industry lobbying activities and regulatory capture prevent implementation of binding sodium reduction standards that would otherwise be technically and economically feasible?',
    'Documentary analysis of lobbying expenditures, regulatory testimony, and delayed/weakened sodium standards; comparison of regulatory trajectories in captured vs. independent regulatory environments; mechanism analysis of industry veto points',
    'If capture confirmed: constraint is politically contingent, not inevitable. Removing capture mechanisms enables faster transition to scaffold. If capture absent: industry has legitimate technical/cost concerns about reformulation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(industry_lobbying_capture, empirical, 'Extent of industry regulatory capture preventing sodium standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(processed_food_sodium_targeting, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sodium_tr_t0, processed_food_sodium_targeting, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sodium_tr_t10, processed_food_sodium_targeting, theater_ratio, 10, 0.58).
narrative_ontology:measurement(sodium_tr_t20, processed_food_sodium_targeting, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(sodium_be_t0, processed_food_sodium_targeting, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sodium_be_t10, processed_food_sodium_targeting, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(sodium_be_t20, processed_food_sodium_targeting, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sodium_su_t0, processed_food_sodium_targeting, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sodium_su_t10, processed_food_sodium_targeting, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(sodium_su_t20, processed_food_sodium_targeting, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(processed_food_sodium_targeting, resource_allocation).
narrative_ontology:affects_constraint(processed_food_sodium_targeting, food_affordability_and_nutrition_access).
narrative_ontology:affects_constraint(processed_food_sodium_targeting, hypertension_treatment_access).
narrative_ontology:affects_constraint(processed_food_sodium_targeting, regulatory_capture_in_food_safety).

% DUAL FORMULATION NOTE:
% Processed food sodium targeting decomposes into two structurally distinct stories: (1) COORDINATION — cheap processed foods enable food security for low-income populations; genuine supply-chain and cost coordination function with ε ≈ 0.15-0.20. (2) EXTRACTION — sodium-related disease burden concentrates on trapped populations; asymmetric health cost extraction with ε ≈ 0.38-0.43. The tangled_rope type (ε = 0.58 combined) results from their inseparability within current regulatory and economic structures. Decomposition would require separate stories only if the ε values diverged wildly under different measurement bases — they do not. Both stories use the same measurement basis (population-level sodium intake and health outcomes) and yield stable ε values. Keeping them unified as tangled_rope reflects structural reality: food security and extraction are coupled in the current system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(processed_food_sodium_targeting, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
