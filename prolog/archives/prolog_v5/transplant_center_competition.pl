% ============================================================================
% CONSTRAINT STORY: transplant_center_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transplant_center_competition, []).

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
 *   constraint_id: transplant_center_competition
 *   human_readable: Transplant Center Competition and Organ Allocation
 *   domain: healthcare/transplantation/resource_allocation
 *
 * SUMMARY:
 *   Transplant center competition creates a structural tension between
 *   specialization incentives (concentrating organs at high-volume centers
 *   improves outcomes) and equity concerns (concentration leaves rural and
 *   disadvantaged populations with worse access). The United States
 *   transplant system allocates organs through UNOS regional and national
 *   allocation policies that attempt to balance utility (organs go to centers
 *   with best outcomes) against equity (organs distributed to serve all
 *   populations). In practice, high-volume centers in urban areas with strong
 *   surgical programs, robust referral networks, and better patient selection
 *   accumulate a disproportionate share of organs. Low-volume rural centers
 *   struggle to maintain program viability, lose surgical staff to urban
 *   competitors, and deteriorate. Rural patients face longer waitlist times
 *   and lower transplant rates. The constraint exhibits tangled_rope
 *   structure from the analytical view: genuine coordination of expertise
 *   (specialization and outcome improvement are real benefits) paired with
 *   asymmetric extraction (access redistribution from rural to urban, from
 *   disadvantaged to advantaged populations). Theater increases over time as
 *   regulatory frameworks become more elaborate (UNOS policies, allocation
 *   algorithms, performance metrics) while actual allocation patterns
 *   increasingly diverge from stated principles of geographic equity.
 *
 * KEY AGENTS:
 *   - High-Volume Transplant Centers: Primary beneficiary (institutional/arbitrage) — concentrate organ referrals, maintain superior outcomes, attract surgeon talent and referrals in positive feedback loop
 *   - Low-Volume Transplant Centers: Primary victim (moderate/constrained) — struggle to maintain case volume, lose staff to competitors, face program viability challenges while constrained by allocation rules
 *   - Rural Patients: Secondary victim (powerless/trapped) — face geographic barriers to high-volume centers, experience longer waitlist times, lower transplant rates; cannot exit system
 *   - Organ Procurement Organizations: Organized actors (organized/constrained) — coordinate organ procurement and allocation while optimizing for their own performance metrics; constrained by regulatory oversight but incentivized toward concentration
 *   - UNOS Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains allocation policies and oversight; has arbitrage capacity through policy design; framework increasingly performs coordination function rather than executing it
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees dual nature of constraint: genuine specialization coordination paired with extractive access redistribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transplant_center_competition, 0.58).
domain_priors:suppression_score(transplant_center_competition, 0.65).
domain_priors:theater_ratio(transplant_center_competition, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transplant_center_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(transplant_center_competition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(transplant_center_competition, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transplant_center_competition, tangled_rope).
narrative_ontology:human_readable(transplant_center_competition, "Transplant Center Competition and Organ Allocation").
narrative_ontology:topic_domain(transplant_center_competition, "healthcare/transplantation/resource_allocation").

domain_priors:requires_active_enforcement(transplant_center_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transplant_center_competition, high_volume_transplant_centers).
narrative_ontology:constraint_beneficiary(transplant_center_competition, wealthy_geographic_regions).
narrative_ontology:constraint_victim(transplant_center_competition, low_volume_transplant_centers).
narrative_ontology:constraint_victim(transplant_center_competition, rural_patients).
narrative_ontology:constraint_victim(transplant_center_competition, organ_donation_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL PATIENT (SNARE) — Trapped by geography and organ scarcity. Competition between centers for organs creates incentives to concentrate transplants at high-volume centers in urban areas. Rural patients face longer waitlist times and lower transplant rates. No exit option: cannot reasonably relocate or choose alternative healthcare system. Bears full cost of competition-driven concentration.
constraint_indexing:constraint_classification(transplant_center_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-VOLUME CENTER (TANGLED ROPE) — Constrained by organ allocation rules and economic pressure. Competition creates genuine coordination of expertise (centers specialize, maintain high skill levels) but also asymmetric extraction: high-volume centers attract better surgeons, more referrals, better survival outcomes, attracting further referrals in a positive feedback loop. Low-volume centers struggle with case volume, staff retention, and program viability. Some coordination benefit (specialization drives quality) mixed with extractive mechanism (winner-take-most dynamics).
constraint_indexing:constraint_classification(transplant_center_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-VOLUME CENTER (ROPE) — Institutional actor with arbitrage capacity (can accept or refuse organ offers, can recruit surgeons and staff from competitors, can invest in infrastructure). Experiences competition as coordination: the system allocates organs to centers with demonstrated capacity and outcomes. Better outcomes attract referrals, which maintains case volume, which sustains expertise and outcomes. Net beneficiary. For this actor, the competition is pure coordination with minimal experienced extraction.
constraint_indexing:constraint_classification(transplant_center_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPO NETWORK (TANGLED ROPE) — Organized actors (OPOs) coordinate organ procurement and allocation while also competing for performance metrics. Genuine coordination function: OPOs must distribute organs to centers that will use them effectively. But also extractive mechanism: OPOs optimize for their own metrics (procurement rate, utilization rate), creating incentive misalignment. Center competition drives pressure on OPOs to concentrate organs at high-success centers, potentially bypassing lower-volume centers that could serve underserved populations. Constrained by regulatory oversight but also by implicit performance incentives.
constraint_indexing:constraint_classification(transplant_center_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY OVERSIGHT (PITON) — UNOS (United Network for Organ Sharing) maintains historical allocation rules and oversight protocols that persist despite functional degradation. Theater ratio reflects the gap between stated allocation principles (equity, medical urgency, utility) and actual allocation mechanisms (concentrated at high-volume centers). Regulatory framework designed to coordinate allocation but increasingly performative: maintains appearance of impartial organ distribution while actual allocation follows concentration logic. Persists through institutional inertia rather than functional success.
constraint_indexing:constraint_classification(transplant_center_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, transplant center competition coordinates specialization and maintains quality incentives (high-volume centers achieve better outcomes, which is coordinative benefit) while simultaneously extracting value from rural and disadvantaged populations through concentration effects. The constraint exhibits genuine dual nature: coordination of expertise paired with asymmetric extraction of access. Classification reflects both functions are real and both are structural.
constraint_indexing:constraint_classification(transplant_center_competition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transplant_center_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transplant_center_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transplant_center_competition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(transplant_center_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(transplant_center_competition, TR),
    TR >= 0.70.

:- end_tests(transplant_center_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts access from rural and low-volume populations through concentration mechanisms, but the extraction is not total (some organs do reach rural centers) and is partly justified by outcome quality. The trajectory shows increasing extractiveness over 20 years as urban centers accumulate advantages and regulatory frameworks struggle to counteract concentration. Suppression (0.65): High. Significant barriers prevent low-volume centers from competing: geographic patient bases, surgery learning curves, surgeon recruitment costs, infrastructure requirements, regulatory compliance costs. Rural patients face barriers including transportation distance, social ties to local providers, and limited awareness of distant center options. These barriers are not total (patients can relocate, centers can invest) but are substantial. Theater ratio (0.48): Moderate. UNOS allocation policies create theatrical elements (stated equity principles, performance metrics, allocation algorithms) that exceed the actual coordination function. However, theater is not dominant — the allocation system does coordinate real matching of organs to centers, though not according to stated principles. Theater has increased over time as policies have become more elaborate while actual allocation logic has become more concentrated.
 *
 * PERSPECTIVAL GAP:
 *   High-volume centers perceive this constraint as pure coordination (Rope): organs flow to centers with best outcomes, which makes medical sense. Low-volume centers perceive extraction (Tangled Rope leaning toward Snare): they are losing organs and staff to competitors while constrained by allocation rules. Rural patients perceive extraction (Snare): they face barriers to access that seem arbitrary and uncontrollable. OPOs perceive mixed extraction and coordination (Tangled Rope): they are meant to coordinate allocation but are incentivized to concentrate organs. UNOS perceives its own allocation framework as coordinating equity and utility (Rope/Piton hybrid), but empirical allocation increasingly contradicts stated principles. The perspectival gaps reveal that 'specialization improves outcomes' (coordination narrative from beneficiary centers) and 'concentration denies access' (extraction narrative from rural patients) are both structurally true — the constraint is genuinely dual.
 *
 * DIRECTIONALITY LOGIC:
 *   High-volume centers experience low d (beneficiary status + arbitrage exit options → negative effective extraction). Rural patients experience high d (victim status + trapped exit options → high effective extraction). Low-volume centers experience moderate-high d (victim status + constrained exit options → high-moderate effective extraction). OPOs experience moderate d (organized actors with constrained exit but also with internal incentive misalignment → mixed extraction). The analytical observer experiences moderate d (analytical position on extraction/coordination duality). Directionality values reflect structural position within the concentration mechanism: those who benefit from concentration have low d; those trapped by geography or capacity constraints have high d.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through dual classification: the analytical observer correctly identifies tangled_rope because the constraint exhibits both genuine coordination (specialization, outcome improvement) and genuine extraction (access redistribution, barrier creation). The high-volume center's rope classification is their sincere experience from within the coordination function. The rural patient's snare classification is their sincere experience of extraction without exit. Both are correct within their structural position. The tension between them is not resolved by choosing one type — it is resolved by recognizing that the constraint coordinates specialization while extracting access. The regulatory framework's piton classification reflects that UNOS policies maintain theater (elaborate allocation rules) while actual allocation has shifted toward concentration. The mandatrophy is not resolved by picking a single type but by mapping the presheaf: each position sees a real aspect of the constraint structure, and the full picture requires all perspectives together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    concentration_causation,
    'Does high-volume center success drive patient referral concentration, or does patient concentration create high-volume centers?',
    'Historical analysis of organ allocation patterns before/after regionalization; causal inference from exogenous policy changes affecting allocation rules',
    'If causation is volume→success: concentration is efficiency coordination. If causation is referral→volume: concentration is extractive redistribution. If bidirectional: positive feedback mechanism confirms tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concentration_causation, empirical, 'Causal direction between center volume and outcome quality').

omega_variable(
    rural_center_viability,
    'Can low-volume transplant programs maintain acceptable clinical outcomes with current allocation mechanisms, or is concentration necessary for quality?',
    'Comparative outcome analysis: low-volume centers with concentrated organ access vs high-volume centers; identification of minimum volume threshold for acceptable outcomes',
    'If low-volume viability is possible: concentration is extraction that could be reduced. If minimum volume is high: concentration is necessary coordination, reducing snare classification of rural patients.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rural_center_viability, empirical, 'Minimum viable volume for acceptable transplant center outcomes').

omega_variable(
    allocation_rule_effectiveness,
    'Do current UNOS allocation policies actually prevent concentration or merely regulate its speed?',
    'Policy simulation and empirical data on organ allocation flows; comparison of actual allocation to stated principles; impact analysis of allocation policy changes',
    'If policies prevent concentration: regulatory framework is effective coordination. If policies regulate but not prevent: framework is theater (piton classification confirmed). If policies amplify concentration: framework is complicit extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allocation_rule_effectiveness, empirical, 'Effectiveness of current organ allocation policies').

omega_variable(
    rural_demand_elasticity,
    'Would rural patients accept transplant at lower-volume local centers, or do they prefer travel to high-volume centers even with increased transplant delay?',
    'Patient survey data on transplant center preferences; analysis of patient choice behavior when offered local vs distant center options; longitudinal tracking of patient outcomes by choice',
    'If patients prefer travel: concentration reflects patient preference (reduces snare classification). If patients prefer local: concentration overrides patient preference (confirms snare classification). Ambiguity suggests identity_locked mechanism (patients internalize center hierarchy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_demand_elasticity, preference, 'Patient preferences for local vs high-volume transplant centers').

omega_variable(
    surgeon_expertise_path_dependency,
    'Is surgeon expertise concentrated at high-volume centers because volume drives expertise, or because expertise was historically concentrated and volume followed?',
    'Career trajectory analysis; surgeon mobility patterns; retraining programs and their success at redistributing expertise',
    'If volume→expertise: concentration is efficiency mechanism. If expertise→volume: concentration reflects historical path dependency (could be altered by investment). If both: positive feedback is real but possibly interventable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surgeon_expertise_path_dependency, empirical, 'Causation between surgeon expertise and center volume').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transplant_center_competition, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(txplant_tr_t0, transplant_center_competition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(txplant_tr_t10, transplant_center_competition, theater_ratio, 10, 0.42).
narrative_ontology:measurement(txplant_tr_t20, transplant_center_competition, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(txplant_be_t0, transplant_center_competition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(txplant_be_t10, transplant_center_competition, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(txplant_be_t20, transplant_center_competition, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transplant_center_competition, resource_allocation).
narrative_ontology:affects_constraint(transplant_center_competition, organ_donation_supply_scarcity).
narrative_ontology:affects_constraint(transplant_center_competition, transplant_surgery_training_pipeline).
narrative_ontology:affects_constraint(transplant_center_competition, healthcare_geographic_equity).

% DUAL FORMULATION NOTE:
% Transplant center competition is downstream of organ supply scarcity but represents a distinct structural constraint. It creates secondary coordination problems (specialization, outcome improvement) while creating secondary extraction mechanisms (geographic concentration). Linked upstream to donation supply constraints and downstream to healthcare equity patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transplant_center_competition, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
