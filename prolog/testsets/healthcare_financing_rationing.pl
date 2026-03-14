% ============================================================================
% CONSTRAINT STORY: healthcare_financing_rationing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_healthcare_financing_rationing, []).

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
 *   constraint_id: healthcare_financing_rationing
 *   human_readable: Healthcare Financing Rationing
 *   domain: healthcare/political_economy
 *
 * SUMMARY:
 *   Healthcare financing rationing creates a structural tension between
 *   legitimate collective resource management and asymmetric extraction
 *   favoring wealthy patients and institutional gatekeepers. The constraint
 *   operates simultaneously as a coordination mechanism (pooling risk through
 *   insurance), a rationing device (allocating scarce resources), and an
 *   extraction apparatus (concentrating denial and delay on low-income
 *   populations while preserving access for high-income populations). The
 *   rise in extractiveness over the interval (0.45 to 0.62) reflects
 *   increasing use of prior authorization, narrow networks, and out-of-pocket
 *   cost-shifting to concentrate costs on patients rather than spreading
 *   them. The theater ratio rise (0.38 to 0.55) reflects that much rationing
 *   is presented as medical necessity when it reflects political choices
 *   about funding levels. The constraint exhibits all six types from
 *   different structural positions, with organized reform movements offering
 *   a scaffold exit path and prior authorization operating as performative
 *   piton ritual.
 *
 * KEY AGENTS:
 *   - Uninsured Patients: Primary victims (powerless/trapped) — face total exclusion or bankruptcy; no exit options from rationing constraint
 *   - Insured Working-Class Patients: Secondary victims (moderate/constrained) — benefit from insurance coordination but constrained by high deductibles, prior authorization, and restricted networks
 *   - Wealthy Patients: Partial victims (powerful/mobile) — experience genuine insurance coordination but can exit rationing entirely through private medicine and international arbitrage
 *   - Healthcare Administrators and Insurers: Primary beneficiaries (institutional/arbitrage) — define and enforce rationing rules; benefit from arbitrage exit and cost-shifting
 *   - Pharmaceutical and Device Manufacturers: Beneficiaries (institutional/arbitrage) — participate in rationing through pricing power; influence which treatments get rationed through lobbying
 *   - Healthcare Reform Movements: Organized actors (organized/constrained) — offer scaffold exit through alternative systems; constrained by political barriers but have pathway forward
 *   - Prior Authorization System: Institutional ritual (institutional/arbitrage) — maintains degraded gatekeeping function; theater has increased as insurers use it primarily for claim denial rather than clinical assessment
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing political funding choices as inherent scarcity limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(healthcare_financing_rationing, 0.62).
domain_priors:suppression_score(healthcare_financing_rationing, 0.68).
domain_priors:theater_ratio(healthcare_financing_rationing, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(healthcare_financing_rationing, extractiveness, 0.62).
narrative_ontology:constraint_metric(healthcare_financing_rationing, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(healthcare_financing_rationing, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(healthcare_financing_rationing, tangled_rope).
narrative_ontology:human_readable(healthcare_financing_rationing, "Healthcare Financing Rationing").
narrative_ontology:topic_domain(healthcare_financing_rationing, "healthcare/political_economy").

domain_priors:requires_active_enforcement(healthcare_financing_rationing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(healthcare_financing_rationing, healthcare_administrators).
narrative_ontology:constraint_beneficiary(healthcare_financing_rationing, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(healthcare_financing_rationing, insurance_underwriters).
narrative_ontology:constraint_beneficiary(healthcare_financing_rationing, wealthy_patients).
narrative_ontology:constraint_victim(healthcare_financing_rationing, low_income_patients).
narrative_ontology:constraint_victim(healthcare_financing_rationing, chronically_ill_populations).
narrative_ontology:constraint_victim(healthcare_financing_rationing, elderly_populations).
narrative_ontology:constraint_victim(healthcare_financing_rationing, rural_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED PATIENT (SNARE) — No structural exit from the rationing constraint. Faces total exclusion from expensive treatments, bankruptcy from emergency care, or death from untreated conditions. Cannot arbitrage out, cannot pay out-of-pocket, cannot negotiate. Maximum suppression via economic barrier and legal framework. Trapped agents perceive immutable mountain-like constraint; engine computes high chi from trapped exit + victim status.
constraint_indexing:constraint_classification(healthcare_financing_rationing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURED WORKING-CLASS PATIENT (TANGLED ROPE) — Mixed coordination and extraction. Insurance provides genuine coordination benefit (pooled risk, predictable costs) but also enforces extraction through high deductibles, prior authorization requirements, and denial of expensive treatments. Can theoretically exit by changing jobs or paying out-of-pocket, but costs are punitive. Experiences both rope (mutual protection) and snare (forced participation, restricted access) simultaneously.
constraint_indexing:constraint_classification(healthcare_financing_rationing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTHCARE ADMINISTRATORS / INSURERS (ROPE) — Experience the rationing system as coordination mechanism for managing collective resources. Genuine coordination function: without rationing rules, system would collapse under infinite demand. Benefit from arbitrage exit (can shift costs to other payers, shift risk to patients, influence policy). Experience minimal suppression because they help define the constraint. Engine derives low d from institutional power + arbitrage exit + beneficiary status.
constraint_indexing:constraint_classification(healthcare_financing_rationing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTHY PATIENTS (TANGLED ROPE) — Genuine coordination benefit from insurance for routine care, but can exit the rationing constraint entirely for expensive treatments through private medicine, international care arbitrage, or direct payment. Some suppression via regulatory restrictions on direct-pay options, but mobile exit options mean suppression is surmountable. Experience genuine coordination (health insurance system) with optional extraction (can leave when costs get high). Moderately experienced extraction but with exit optionality.
constraint_indexing:constraint_classification(healthcare_financing_rationing, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HEALTHCARE REFORM MOVEMENTS (SCAFFOLD) — Organized agents (patient advocacy groups, public health advocates, labor unions) see rationing as a temporary institutional failure with a sunset. Single-payer systems, universal coverage models, and managed competition represent alternative pathways that reduce rationing through coordination redesign rather than accepting scarcity as inherent. Theater ratio is moderate (rationing is presented as medical necessity, but political choices drive actual allocation). Organized power gives these agents exit pathway even if slow.
constraint_indexing:constraint_classification(healthcare_financing_rationing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PRIOR AUTHORIZATION SYSTEM (PITON) — Substantial portion of the rationing constraint operates through degraded institutional ritual. Prior authorization was designed to ensure medical necessity but has become largely performative: insurers use it primarily to delay treatment and encourage abandonment of claims, not to prevent medically unnecessary care. The ritual persists through institutional inertia despite poor health outcomes. Theater ratio in this sub-mechanism reaches 0.75+. Actors recognize it as theater but maintain it because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(healthcare_financing_rationing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SCARCITY VIEW (MOUNTAIN) — From universal perspective, some health rationing is inherent to scarcity: finite budgets, finite skilled providers, finite equipment mean that not all possible treatments can be provided to all people. This perspective sees rationing as an immutable law of economics — any system must allocate scarce resources somehow. However, the structural data reveals this as false naturalization: most wealthy nations provide vastly more treatment access at lower per-capita cost, suggesting the 'inherent scarcity' framing obscures political choices about funding levels and allocation mechanisms.
constraint_indexing:constraint_classification(healthcare_financing_rationing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(healthcare_financing_rationing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(healthcare_financing_rationing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(healthcare_financing_rationing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(healthcare_financing_rationing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(healthcare_financing_rationing, TR),
    TR >= 0.70.

:- end_tests(healthcare_financing_rationing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint concentrates benefits on administrators and wealthy patients while concentrating costs on low-income and chronically ill populations. The 17-point increase over 15 years reflects deterioration: shift from coverage-based rationing (which spreads costs) to cost-based rationing (which concentrates costs on vulnerable populations through prior authorization, network narrowing, and deductible increases). The constraint is not maximal extraction because some genuine coordination function (insurance pooling, risk spreading) persists. Suppression (0.68): High. Multiple barriers to exit: legal mandate to participate in insurance, regulatory restrictions on direct-pay options, lack of price transparency, information asymmetry about treatment quality, professional licensing restrictions on alternative providers. Suppression is both structural (legal/economic barriers) and internalized (patients have absorbed framing that rationing is medical necessity, not political choice). Theater ratio (0.55): Moderate-high. Prior authorization is presented as clinical gatekeeping but functions primarily as administrative barrier and claims-delay mechanism. Coverage denials are framed as 'not medically necessary' when in fact they reflect cost ceilings. Rationing itself is presented as inherent to healthcare when it actually reflects political choices about funding levels relative to other OECD nations.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that healthcare rationing is not a single constraint with uniform effect, but rather a presheaf structure where classification depends entirely on structural position. From below (uninsured), it is snare. From middle (insured working-class), it is tangled_rope. From above (wealthy), it is rope with exit. From within administration, it is rope pure. From organized reform perspective, it is scaffold (temporary, solvable). From degraded ritual perspective (prior authorization), it is piton. From analytical distance, it appears mountain (resource scarcity) but structural comparison with other OECD nations reveals this as false naturalization of a political choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position: power level, exit options, and beneficiary/victim status. Uninsured patients (powerless + trapped + victim) derive maximum d ≈ 0.95, producing f(d) ≈ 1.42, maximum experienced extraction. Working-class insured (moderate + constrained + victim) derive d ≈ 0.65, producing f(d) ≈ 1.00, high experienced extraction. Wealthy patients (powerful + mobile + partial victim/beneficiary) derive d ≈ 0.45, producing f(d) ≈ 0.55, moderate experienced extraction. Healthcare administrators (institutional + arbitrage + beneficiary) derive d ≈ 0.10, producing f(d) ≈ -0.05, negative experienced extraction (they benefit from the constraint's enforcement). Scope modifier σ(national) = 1.0 applies without amplification. The engine's chi calculation multiplies base extractiveness × f(d) × σ(S) for each perspective: uninsured patient experiences χ ≈ 0.62 × 1.42 × 1.0 = 0.88; administrator experiences χ ≈ 0.62 × (-0.05) × 1.0 = -0.03. Directionality overrides are unnecessary: structural derivation from beneficiary/victim declarations and exit options captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Healthcare financing rationing resolves the mandatrophy by showing that all six types are legitimate readings of the same structural phenomenon. The mandatrophy is not 'is healthcare rationing rope or snare?' but rather 'from which structural position are you measuring?' The false summit occurs when the analytical observer naturalizes political funding choices as inherent scarcity limits (mountain). The correct analytical view is tangled_rope: genuine coordination function (insurance pooling) combined with asymmetric extraction (concentration of rationing burden on low-income populations, wealthy escape options, administrative gatekeeping). The mandatrophy resolution requires showing that (1) the coordination function is real (insurance does pool risk and spread costs better than markets do), (2) the extraction is real (rationing concentrates harm on powerless agents while preserving access for powerful agents), and (3) the theatrical component is real (rationing is presented as medical necessity when it reflects political choices about funding levels). None of these truths contradicts the others. The constraint is correctly classified as tangled_rope at the analytical level because it exhibits both genuine coordination (insurance mechanism) and genuine asymmetric extraction (whose access gets rationed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_level_ambiguity,
    'Is the measured rationing driven by absolute resource scarcity or by political choices about funding levels and allocation mechanisms?',
    'Comparative analysis of healthcare outcomes and spending across OECD nations with similar GDP per capita but different funding models. Cross-national mapping of rationing severity vs total health spending.',
    'If driven by scarcity: mountain classification gains plausibility. If driven by political choice: rationing is contingent institutional arrangement, not law of nature. Changes classification of analytical perspective from mountain to tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_level_ambiguity, empirical, 'Whether rationing reflects absolute scarcity or political funding choices').

omega_variable(
    prior_authorization_functionality,
    'Does prior authorization prevent medically unnecessary care at rates justifying its administrative burden, or does it primarily function as a barrier to access and claims abandonment?',
    'Empirical audit: comparison of denial rates vs clinical guidelines; analysis of appeal success rates; correlation between prior authorization delay duration and treatment abandonment; longitudinal health outcomes for claims initially denied but later approved.',
    'If functionally protective: rationing has legitimate coordination component. If primarily a barrier: prior authorization is extractive theater masquerading as clinical gatekeeping. Affects piton vs snare sub-classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_authorization_functionality, empirical, 'Whether prior authorization provides clinical value or functions as access barrier').

omega_variable(
    identity_locked_patient_compliance,
    'Do patients rationally accept rationing as necessary medical practice, or has rationing framing become internalized such that patients suppress their own demands despite structural mobility to seek alternative care?',
    'Comparative analysis: patient behavior when constraints are transparent vs when framed as medical necessity. Ethnographic study of post-denial care-seeking behavior. International survey: do patients in universal-coverage systems report different perceived necessity of rationing?',
    'If identity-locked: suppression is partially internalized; constraint is harder to disrupt because targets have become invested in the rationing narrative. If rational acceptance: suppression is purely structural (cost barriers); constraint could be disrupted by reducing costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_patient_compliance, empirical, 'Whether patient acceptance of rationing reflects identity fusion or rational cost response').

omega_variable(
    insurance_mandate_necessity,
    'Does mandatory insurance participation solve a genuine coordination problem (adverse selection, cost spreading) or primarily extracts rents by forcing participation and restricting exit?',
    'Natural experiments: compare health outcomes and costs in jurisdictions with vs without insurance mandates; analysis of market outcomes under voluntary insurance with high-quality transparency; study of voluntary participation systems.',
    'If coordination problem real: insurance mandate is justified rope-type constraint. If primarily extractive: mandate is snare-type constraint. Affects baseline classification and directionality for all insured-patient perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_mandate_necessity, empirical, 'Whether insurance mandates solve coordination problems or impose extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(healthcare_financing_rationing, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hfr_tr_t0, healthcare_financing_rationing, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hfr_tr_t5, healthcare_financing_rationing, theater_ratio, 5, 0.47).
narrative_ontology:measurement(hfr_tr_t10, healthcare_financing_rationing, theater_ratio, 10, 0.55).
narrative_ontology:measurement(hfr_tr_t15, healthcare_financing_rationing, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(hfr_be_t0, healthcare_financing_rationing, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hfr_be_t5, healthcare_financing_rationing, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(hfr_be_t10, healthcare_financing_rationing, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(hfr_be_t15, healthcare_financing_rationing, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(healthcare_financing_rationing, resource_allocation).
narrative_ontology:affects_constraint(healthcare_financing_rationing, pharmaceutical_pricing_power).
narrative_ontology:affects_constraint(healthcare_financing_rationing, medical_bankruptcy_debt_trap).
narrative_ontology:affects_constraint(healthcare_financing_rationing, provider_network_narrowing).
narrative_ontology:affects_constraint(healthcare_financing_rationing, health_insurance_mandate_enforcement).

% DUAL FORMULATION NOTE:
% Healthcare financing rationing is a constraint family that decomposes into multiple structurally distinct mechanisms: (1) insurance pooling (genuine coordination, low ε), (2) prior authorization delays (theatrical extraction, high theater ratio), (3) network narrowing (exclusion mechanism, high extraction), (4) cost-shifting via deductibles (direct victim targeting, high extraction). Each mechanism has different ε values and different temporal dynamics. The measured extractiveness (0.62) represents a mix of these mechanisms weighted by their prevalence in typical US healthcare encounters. Decomposition into separate stories would distinguish the coordination function (insurance pooling, ε ≈ 0.10) from the extraction mechanisms (prior authorization, network narrowing, cost-shifting, ε ≈ 0.70-0.85). This story models them as an integrated system because they co-evolve: increases in extraction (prior authorization, cost-shifting) are justified by appeals to coordination necessity (controlling costs to keep insurance viable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
