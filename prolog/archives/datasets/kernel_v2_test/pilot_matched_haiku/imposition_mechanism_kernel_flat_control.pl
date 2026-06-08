% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel_flat_control, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_mechanism_kernel_flat_control
 *   human_readable: Temporal and Sartorial Norm Imposition: State Mandate vs. Cultural Adoption
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   The imposition of temporal and sartorial norms represents a foundational
 *   mechanism of state formation and cultural authority. Whether the
 *   legitimacy of these norms derives from endogenous cultural adoption,
 *   exogenous state mandate, or hybrid symbolic-coercive mechanisms is the
 *   central structural question. This constraint exhibits the full range of
 *   DR classification from different perspectives: the state administrative
 *   apparatus experiences pure coordination (Rope), the dominant cultural
 *   group experiences legitimation (Rope), subordinated populations
 *   experience pure extraction (Snare), assimilating minorities experience
 *   mixed coordination and extraction (Tangled Rope), organized resistance
 *   movements experience constrained coordination (Tangled Rope), the
 *   enforcement bureaucracy maintains performative ritual (Piton), and the
 *   analytical observer risks naturalizing contingent arrangements as
 *   inherent to large-scale coordination (Mountain). The temporal trajectory
 *   shows increasing extractiveness (0.35 → 0.62) and rising theater ratio
 *   (0.45 → 0.72), indicating that enforcement has intensified and become
 *   increasingly performative over the interval. This pattern is consistent
 *   with a constraint that begins as genuine coordination (early adoption,
 *   low enforcement) and gradually transforms into extraction maintained
 *   through ritual (late enforcement, high theater).
 *
 * KEY AGENTS:
 *   - State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — captures coordination benefits and cultural authority without bearing extraction costs
 *   - Dominant Cultural Group: Secondary beneficiary (institutional/arbitrage) — experiences legitimation of their cultural practices as official norms
 *   - Subordinated Populations: Primary victim (powerless/trapped) — bears full cost of cultural conformity with no exit option
 *   - Assimilating Minorities: Secondary victim (moderate/constrained) — experience mixed coordination and extraction; face economic dependency and social penalties
 *   - Resistance Movements: Organized agents (organized/constrained) — coordinate alternative practices while bearing suppression costs
 *   - Enforcement Bureaucracy: Institutional actor (institutional/arbitrage) — maintains performative enforcement; extracts authority and resources from the constraint
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel_flat_control, 0.58).
domain_priors:suppression_score(imposition_mechanism_kernel_flat_control, 0.62).
domain_priors:theater_ratio(imposition_mechanism_kernel_flat_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(imposition_mechanism_kernel_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(imposition_mechanism_kernel_flat_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel_flat_control, "Temporal and Sartorial Norm Imposition: State Mandate vs. Cultural Adoption").
narrative_ontology:topic_domain(imposition_mechanism_kernel_flat_control, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(imposition_mechanism_kernel_flat_control, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel_flat_control, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel_flat_control, dominant_cultural_group).
narrative_ontology:constraint_victim(imposition_mechanism_kernel_flat_control, subordinated_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel_flat_control, cultural_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel_flat_control, assimilating_minorities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel_flat_control, assimilating_minorities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel_flat_control, resistance_movements).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel_flat_control, state_monopoly_on_legitimate_symbolic_authority).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel_flat_control, cultural_homogenization_as_state_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state bureaucracy sets and enforces temporal and sartorial standards across the territory it controls. It benefits from the coordination function (standardized calendars enable tax collection, military conscription, legal administration) without bearing the extraction costs. It has arbitrage options — it can adopt alternative standards if they prove more efficient — but it has no incentive to do so because the current standards serve its interests.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel_flat_control, state_administrative_apparatus, agenda_setter,
    institutional, immediate, arbitrage, national).

% The dominant cultural group experiences the state-mandated norms as legitimation of their own practices. Their calendar and dress code become official, elevating their cultural authority. They bear no extraction costs because conformity requires no change to their existing practices. They benefit from the constraint without paying for it.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel_flat_control, dominant_cultural_group, beneficiary,
    institutional, generational, arbitrage, national).

% Subordinated populations face coercive enforcement of temporal and sartorial norms that conflict with their heritage practices. They cannot adopt alternative calendars or dress codes without legal penalty, social ostracism, or economic exclusion. They have no exit option — emigration is not feasible for most, and resistance is suppressed. They bear the full cost of cultural conformity.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel_flat_control, subordinated_populations, payer,
    powerless, biographical, trapped, national).

% Assimilating minorities experience both coordination and extraction. The state-mandated norms do coordinate economic and administrative life — participation in labor markets, legal proceedings, and civic institutions requires conformity. But conformity also extracts cultural identity and imposes costs on maintaining heritage practices. Exit is theoretically possible (emigration, cultural resistance) but constrained by economic dependency and social penalties.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel_flat_control, assimilating_minorities, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel_flat_control, assimilating_minorities, beneficiary).

% Organized resistance movements (cultural associations, religious communities, nationalist movements) coordinate alternative calendars and dress codes to maintain heritage practices. They bear suppression costs (legal penalties, social exclusion, enforcement pressure) but gain the benefit of preserving cultural identity. The constraint's enforcement creates the very organization it seeks to suppress — resistance becomes more organized as enforcement intensifies.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel_flat_control, resistance_movements, payer,
    organized, generational, constrained, national).

% The enforcement bureaucracy (police, inspectors, school administrators) maintains temporal and sartorial norms through performative enforcement. The constraint has become the bureaucracy's primary beneficiary — enforcement activity extracts authority and resources from the constraint itself. Much enforcement is ritual (dress code inspections, calendar compliance checks) that persists through institutional inertia rather than functional necessity.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel_flat_control, enforcement_bureaucracy, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The analytical observer sits outside the constraint's extraction flow and examines its structure from a civilizational perspective. This position risks naturalizing contingent institutional arrangements as inherent to large-scale coordination — the observer may conclude that temporal and sartorial standardization is a natural law of social organization rather than a political choice maintained through enforcement and theater.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel_flat_control, analytical_observer, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(imposition_mechanism_kernel_flat_control, analytical_observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Temporal and sartorial standardization solves the genuine coordination problem of how to synchronize millions of subjects across dispersed territories. A shared calendar enables synchronized economic activity, legal proceedings, military operations, and administrative functions. Standardized dress codes reduce information asymmetries and facilitate social recognition in anonymous urban contexts. Without some standardization, large-scale coordination becomes more difficult.
% TRANSFER_FUNCTION: The constraint transfers cultural authority from subordinated populations and minorities to the state and dominant cultural group. Subordinated populations lose the ability to practice heritage calendars and dress codes; the state and dominant group gain cultural legitimacy and administrative efficiency. The transfer is asymmetric: the state and dominant group gain coordination benefits and cultural authority; subordinated populations bear extraction costs with minimal coordination benefit.
% ABSENT_VOICES: The voices absent from the constraint's legitimation are those of subordinated populations and cultural minorities who would object to the imposition of norms that conflict with their heritage practices. These populations are not in the room where the constraint is designed and enforced — they are subjects of the constraint rather than participants in its creation. Their absence from the design process is itself a structural feature of the constraint: it is imposed rather than negotiated.
% DISAPPEARANCE_RATIONALE: If temporal and sartorial norms disappeared overnight, the world would partially rearrange itself. The state would lose some administrative efficiency (coordination would require alternative mechanisms), but modern systems demonstrate that large-scale coordination is possible without enforcing cultural conformity. Subordinated populations would gain the ability to practice heritage calendars and dress codes. The dominant cultural group would lose the legitimation of their practices as official norms. The constraint is not inherent to social organization — it is a contingent institutional arrangement that could be replaced with alternative coordination mechanisms.
% FOUNDING_PROBLEM: The founding problem was the coordination challenge of synchronizing millions of subjects across dispersed territories during state consolidation. Early modern states faced genuine coordination problems: how to collect taxes, conscript armies, and administer justice across populations with diverse local practices. Temporal and sartorial standardization was one solution to this problem — it enabled synchronized administrative action.
% FOUNDING_PROBLEM_CORROBORATION: Modern administrative systems (digital coordination, decentralized networks, pluralistic governance) demonstrate that large-scale coordination is possible without enforcing cultural conformity. The founding problem — synchronizing dispersed populations — has been solved by technological and institutional innovations that do not require cultural imposition. The constraint persists despite the founding problem being solved, indicating mandatrophy: the constraint is maintained through institutional inertia and the enforcement bureaucracy's interest in authority rather than because it solves a genuine coordination problem. This assessment is corroborated by comparative analysis of modern pluralistic states that achieve high administrative efficiency without enforcing temporal and sartorial conformity.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED POPULATION (SNARE) — Faces coercive enforcement of temporal and sartorial norms with no exit option. Cannot adopt alternative calendars or dress codes without legal penalty, social ostracism, or economic exclusion. The constraint extracts cultural conformity and erases identity markers. No coordination function is visible from this position — only extraction and suppression.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASSIMILATING MINORITY (TANGLED ROPE) — Experiences both coordination and extraction. The state-mandated calendar and dress code do coordinate economic and administrative life — participation in labor markets, legal proceedings, and civic institutions requires conformity. But conformity also extracts cultural identity and imposes costs on maintaining heritage practices. Exit is theoretically possible (emigration, cultural resistance) but constrained by economic dependency and social penalties.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATIVE APPARATUS (ROPE) — Experiences the constraint as pure coordination. Standardized temporal and sartorial norms enable bureaucratic efficiency, tax collection, military conscription, and legal administration. The state benefits from the coordination function without bearing extraction costs — those costs are borne by subordinated populations. From the state's perspective, the constraint solves a genuine collective-action problem: how to coordinate millions of subjects across dispersed territories.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOMINANT CULTURAL GROUP (ROPE) — Experiences the constraint as legitimation of their own cultural practices. The state-mandated calendar and dress code align with their existing norms, so conformity requires no extraction from them. They experience the constraint as coordination that elevates their culture to official status. The constraint vindicates their cultural authority without imposing costs.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RESISTANCE MOVEMENT (TANGLED ROPE) — Organized agents (cultural associations, religious communities, nationalist movements) experience the constraint as both a coordination problem to solve and an extraction mechanism to resist. They coordinate alternative calendars and dress codes (maintaining heritage practices) while bearing suppression costs (legal penalties, social exclusion). The constraint's enforcement creates the very organization it seeks to suppress.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ENFORCEMENT BUREAUCRACY (PITON) — The institutions tasked with enforcing temporal and sartorial norms (police, inspectors, school administrators) maintain the constraint through performative enforcement. The theater ratio is high: much enforcement activity is ritual (dress code inspections, calendar compliance checks) that persists through institutional inertia rather than functional necessity. The bureaucracy has become the constraint's primary beneficiary, extracting authority and resources from enforcement itself.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some temporal and sartorial standardization appears inherent to large-scale social coordination: any complex society requires shared calendars and dress codes to function. This perspective risks naturalizing what is actually a contingent institutional arrangement — the specific calendar (Gregorian, Islamic, revolutionary) and dress code (Western business attire, national costume, religious garb) are not natural laws but political choices. The false summit detector will identify this as naturalization of extraction.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(imposition_mechanism_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(imposition_mechanism_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts cultural conformity from subordinated populations and minorities, erasing identity markers and imposing costs on maintaining heritage practices. However, the extraction is not total — some coordination function is genuine (the state does solve a real problem of coordinating millions across dispersed territories), and some populations voluntarily adopt the norms. The value reflects the hybrid nature: genuine coordination layered with asymmetric extraction. Suppression (0.62): Moderate-high. Significant barriers to maintaining alternative practices include legal penalties, social ostracism, economic exclusion, and enforcement bureaucracy. But suppression is not total — some populations sustain heritage practices despite enforcement, and enforcement intensity varies across regions and time periods. Theater ratio (0.68): High. Enforcement of temporal and sartorial norms is substantially performative. Much enforcement activity is ritual (dress code inspections, calendar compliance checks) that persists through institutional inertia rather than functional necessity. The rising theater ratio over the interval (0.45 → 0.72) indicates that enforcement has become increasingly performative as the constraint has matured.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from different structural positions. The state administrative apparatus sees pure coordination (Rope) — standardized norms enable bureaucratic efficiency. The dominant cultural group sees legitimation (Rope) — their practices become official. Subordinated populations see pure extraction (Snare) — conformity is coerced with no coordination benefit. Assimilating minorities see mixed coordination and extraction (Tangled Rope) — the norms do coordinate economic life but extract cultural identity. Resistance movements see constrained coordination (Tangled Rope) — they can coordinate alternatives but face suppression. The enforcement bureaucracy sees degraded ritual (Piton) — enforcement persists through inertia. The analytical observer risks seeing natural law (Mountain) — temporal and sartorial standardization appears inherent to large-scale coordination — but the structural data reveals this as a false summit: the specific norms are political choices, not natural necessities.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the extraction flow. The state administrative apparatus and dominant cultural group are beneficiaries with arbitrage options — they experience low or negative effective extraction (d ≈ 0.1-0.2). Subordinated populations are victims with no exit — they experience maximum extraction (d ≈ 0.9-1.0). Assimilating minorities are victims with constrained exit — they experience high extraction (d ≈ 0.7-0.8). Resistance movements are organized agents with constrained exit — they experience moderate extraction (d ≈ 0.5-0.6). The enforcement bureaucracy are beneficiaries with arbitrage options — they experience low extraction (d ≈ 0.2-0.3). The analytical observer sits outside the extraction flow (d ≈ 0.5, symmetric). These directionality values feed the engine's f(d) function to produce experienced extractiveness (χ), which varies across perspectives while the base extractiveness (ε) remains constant.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy: the original mandate (coordinate large-scale societies through standardized norms) has outlived its functional necessity in many contexts. Modern administrative systems can coordinate without enforcing cultural conformity — digital systems, decentralized networks, and pluralistic governance models demonstrate that standardization is not functionally necessary. Yet the constraint persists through institutional inertia and the enforcement bureaucracy's interest in maintaining authority. The rising theater ratio (0.45 → 0.72) indicates that enforcement has become increasingly performative — the constraint is maintained as ritual rather than as a solution to a genuine coordination problem. The mandatrophy is resolved by recognizing that the constraint has transformed from Rope (genuine coordination) to Tangled Rope (mixed coordination and extraction) to Piton (degraded ritual). The analytical observer's mountain classification is a false summit: temporal and sartorial standardization is not inherent to large-scale coordination but a contingent institutional arrangement that persists through enforcement and theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_grounding,
    'Does the legitimacy of temporal and sartorial norms derive from endogenous cultural adoption (the population genuinely prefers the standard) or exogenous state mandate (the state imposes it coercively)?',
    'Historical analysis of adoption patterns: voluntary adoption before state mandate vs. adoption only after enforcement; comparison of compliance rates in high-enforcement vs. low-enforcement regions; ethnographic evidence of internalization vs. performative compliance; measurement of resistance intensity and persistence',
    'If endogenous: constraint reclassifies toward Rope (genuine coordination). If exogenous: constraint remains Tangled Rope or Snare (extraction with coercive enforcement). If hybrid: the proportional mix determines the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_grounding, empirical, 'Whether norm legitimacy derives from cultural adoption or state coercion').

omega_variable(
    symbolic_vs_coercive_mechanism,
    'Does the constraint operate primarily through symbolic authority (the population accepts the norm as legitimate) or coercive enforcement (the population complies from fear of punishment)?',
    'Measurement of compliance rates under different enforcement intensities; analysis of voluntary compliance in low-surveillance contexts; ethnographic documentation of internalization vs. strategic compliance; comparison of norm persistence after enforcement withdrawal',
    'If primarily symbolic: suppression metric should be lower, and the constraint reclassifies toward Rope. If primarily coercive: suppression metric is accurate, and Tangled Rope/Snare classification holds. If hybrid: the proportional mix determines effective suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(symbolic_vs_coercive_mechanism, empirical, 'Whether constraint operates through symbolic authority or coercive enforcement').

omega_variable(
    cultural_minority_heterogeneity,
    'Do different cultural minorities experience the constraint with different extractiveness values, or is the extraction uniform across all subordinated populations?',
    'Comparative analysis of enforcement intensity and compliance costs across different minority groups; measurement of economic penalties, social ostracism, and legal sanctions by group; documentation of differential access to exemptions or accommodations',
    'If heterogeneous: the constraint may decompose into multiple stories with different ε values per minority group. If uniform: single constraint story is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_minority_heterogeneity, empirical, 'Whether extraction is uniform or heterogeneous across minority groups').

omega_variable(
    functional_necessity_vs_domination,
    'Is temporal and sartorial standardization functionally necessary for large-scale coordination, or is it primarily a mechanism for cultural domination and state control?',
    'Comparative analysis of societies with and without standardized norms; measurement of administrative efficiency gains from standardization; documentation of alternative coordination mechanisms that achieve similar efficiency without cultural imposition; historical analysis of whether standardization preceded or followed state consolidation',
    'If functionally necessary: constraint reclassifies toward Rope (genuine coordination problem). If primarily domination: constraint remains Tangled Rope/Snare (extraction mechanism). If both: the proportional mix determines classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_necessity_vs_domination, conceptual, 'Whether standardization is functionally necessary or primarily a domination mechanism').

omega_variable(
    resistance_sustainability,
    'Can subordinated populations sustain alternative temporal and sartorial practices indefinitely under state enforcement, or does enforcement eventually suppress alternatives?',
    'Longitudinal historical analysis of minority practices over generations; measurement of practice persistence rates under varying enforcement intensities; documentation of intergenerational transmission of heritage practices; analysis of whether enforcement creates or suppresses organized resistance',
    'If sustainable: resistance movements may eventually reclassify toward Rope (coordination of alternatives). If suppressed: constraint remains Snare (extraction with no exit). If cyclical: constraint exhibits piton dynamics (periodic enforcement followed by relaxation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_sustainability, empirical, 'Whether alternative practices can be sustained under enforcement').

omega_variable(
    state_mandate_vs_cultural_diffusion,
    'When temporal and sartorial norms spread across populations, is the mechanism primarily state mandate (top-down imposition) or cultural diffusion (horizontal adoption through contact and prestige)?',
    'Historical analysis of adoption patterns in regions with and without state enforcement; measurement of adoption rates in high-prestige vs. low-prestige populations; documentation of whether adoption preceded or followed state mandate; ethnographic evidence of adoption motivations',
    'If primarily state mandate: suppression and extractiveness metrics are accurate. If primarily cultural diffusion: the constraint may reclassify toward Rope (genuine coordination). If hybrid: the proportional mix determines metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_mandate_vs_cultural_diffusion, empirical, 'Whether norm spread is driven by state mandate or cultural diffusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel_flat_control, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impos_tr_t0, imposition_mechanism_kernel_flat_control, theater_ratio, 0, 0.45).
narrative_ontology:measurement(impos_tr_t10, imposition_mechanism_kernel_flat_control, theater_ratio, 10, 0.58).
narrative_ontology:measurement(impos_tr_t20, imposition_mechanism_kernel_flat_control, theater_ratio, 20, 0.68).
narrative_ontology:measurement(impos_tr_t30, imposition_mechanism_kernel_flat_control, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(impos_be_t0, imposition_mechanism_kernel_flat_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(impos_be_t10, imposition_mechanism_kernel_flat_control, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(impos_be_t20, imposition_mechanism_kernel_flat_control, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(impos_be_t30, imposition_mechanism_kernel_flat_control, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(impos_su_t0, imposition_mechanism_kernel_flat_control, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(impos_su_t10, imposition_mechanism_kernel_flat_control, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(impos_su_t20, imposition_mechanism_kernel_flat_control, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(impos_su_t30, imposition_mechanism_kernel_flat_control, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel_flat_control, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel_flat_control, national_identity_construction).
narrative_ontology:affects_constraint(imposition_mechanism_kernel_flat_control, bureaucratic_standardization_imperative).
narrative_ontology:affects_constraint(imposition_mechanism_kernel_flat_control, cultural_assimilation_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint is part of a larger family of state-formation mechanisms. Upstream constraints (national identity construction, bureaucratic standardization imperative) create the conditions for this constraint; downstream constraints (cultural assimilation mechanisms, minority suppression) are enabled by this constraint's enforcement infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel_flat_control, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
