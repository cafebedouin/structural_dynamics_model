% ============================================================================
% CONSTRAINT STORY: sotu_1955_eisenhower_information_program_foreign_advocacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1955_eisenhower_information_program_foreign_advocacy, []).

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
 *   constraint_id: sotu_1955_eisenhower_information_program_foreign_advocacy
 *   human_readable: Federal Foreign Information Program (Eisenhower 1955)
 *   domain: governance/foreign_policy/strategic_communication
 *
 * SUMMARY:
 *   President Eisenhower's 1955 proposal for a vigorous federal information
 *   program represents a critical moment in the institutionalization of
 *   strategic communication as a permanent feature of U.S. governance. The
 *   program frames systematic state messaging about American actions and
 *   intentions as 'truthful disclosure' and a moral response to Communist
 *   propaganda, rather than as propaganda itself. This constraint exhibits
 *   contradictory classifications across perspectives because the same
 *   institutional apparatus simultaneously serves legitimate coordination
 *   functions (informing allies about U.S. commitments) and extractive
 *   functions (shaping foreign populations' receptiveness to U.S. foreign
 *   policy). The program's core ambiguity is whether the distinction between
 *   disclosure and persuasion can be sustained when the state apparatus
 *   designing the disclosure has strategic interests in the audience's
 *   beliefs. The extractiveness metric has risen from 0.35 to 0.52 over the
 *   interval, indicating that resource commitment and bureaucratic expansion
 *   have outpaced the program's defensive justification. Theater ratio has
 *   increased from 0.48 to 0.65, suggesting the apparatus is becoming more
 *   performative—measuring messaging reach rather than verifying actual
 *   impact on foreign population beliefs or policy positions.
 *
 * KEY AGENTS:
 *   - U.S. Executive Branch (institutional/arbitrage): Primary beneficiary — controls messaging content and distribution; captures strategic positioning without accountability for accuracy
 *   - Foreign Civilian Audiences (powerless/trapped): Primary victims — targeted by messaging designed to shape beliefs; no capacity to audit claims or exit information environment
 *   - Global Information Commons (powerless/trapped): Structural victim — epistemic substrate colonized by state-directed persuasion infrastructure; permanent contamination of information environment
 *   - Allied Governments (organized/constrained): Secondary actors — benefit from coordination on U.S. strategy but constrained by dependence and targeted by U.S. messaging intended to shape their domestic populations
 *   - Information Program Bureaucracy (institutional/arbitrage): Institutional implementer — maintains apparatus through budgetary momentum; experiences degrading theater as program becomes ritualized
 *   - Anti-Communist Coalition (powerful/constrained): Temporary coalition — sees program as justified counter to Communist propaganda; constrained by Cold War alliance structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1955_eisenhower_information_program_foreign_advocacy, 0.52).
domain_priors:suppression_score(sotu_1955_eisenhower_information_program_foreign_advocacy, 0.48).
domain_priors:theater_ratio(sotu_1955_eisenhower_information_program_foreign_advocacy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1955_eisenhower_information_program_foreign_advocacy, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1955_eisenhower_information_program_foreign_advocacy, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1955_eisenhower_information_program_foreign_advocacy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1955_eisenhower_information_program_foreign_advocacy, tangled_rope).
narrative_ontology:human_readable(sotu_1955_eisenhower_information_program_foreign_advocacy, "Federal Foreign Information Program (Eisenhower 1955)").
narrative_ontology:topic_domain(sotu_1955_eisenhower_information_program_foreign_advocacy, "governance/foreign_policy/strategic_communication").

domain_priors:requires_active_enforcement(sotu_1955_eisenhower_information_program_foreign_advocacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1955_eisenhower_information_program_foreign_advocacy, u_s_executive_branch).
narrative_ontology:constraint_beneficiary(sotu_1955_eisenhower_information_program_foreign_advocacy, cold_war_strategic_interests).
narrative_ontology:constraint_victim(sotu_1955_eisenhower_information_program_foreign_advocacy, global_information_commons).
narrative_ontology:constraint_victim(sotu_1955_eisenhower_information_program_foreign_advocacy, foreign_populations_as_audiences).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOREIGN CIVILIAN AUDIENCES (SNARE) — Cannot exit the information environment or verify the truthfulness of messaging. Trapped by geographic location and media access constraints. Structured as targets of persuasion with no capacity to audit claims or withdraw consent. Maximum extraction: government messaging presents itself as disclosure but selects what to disclose based on strategic interest, not epistemic completeness.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_information_program_foreign_advocacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL INFORMATION COMMONS (SNARE) — The epistemic substrate itself is victimized. Once state-directed persuasion infrastructure colonizes the information environment, the distinction between disclosure and propaganda becomes unstable across the entire commons. Future audiences cannot recover what portion of historical narrative was shaped by systematic U.S. messaging programs vs. organic reporting. No exit option: the contamination is structural and permanent.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_information_program_foreign_advocacy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ALLIED GOVERNMENTS AND MEDIA (TANGLED ROPE) — Constrained by Cold War alliance structures and economic dependence on U.S. support. Face genuine coordination problem: need information about U.S. intentions and actions to maintain alliance. But also targeted by U.S. messaging designed to shape their own populations' views toward U.S.-preferred policies. Both benefit (reliable strategic information) and bear costs (messaging designed to manipulate their domestic politics).
constraint_indexing:constraint_classification(sotu_1955_eisenhower_information_program_foreign_advocacy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: U.S. EXECUTIVE BRANCH (ROPE) — Pure beneficiary. The information program solves a genuine Cold War coordination problem: conveying U.S. commitment to alliance partners and counter-positioning against Communist narratives. Experiences the constraint as legitimate strategic communication enabling U.S. objectives. No suppression from U.S. perspective — the program executes freely within legal authority. Net beneficiary position with arbitrage exit (can redirect resources, adjust messaging).
constraint_indexing:constraint_classification(sotu_1955_eisenhower_information_program_foreign_advocacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INFORMATION PROGRAM BUREAUCRACY (PITON) — Institutional actors running the program (State Department, USIA nascent functions, diplomatic corps) experience the constraint as a coordination mechanism, but the actual functional differentiation of messaging from truth-telling degrades over time. Theater increases as the program becomes ritualized: reporting on the program's reach becomes performance metric rather than verification of actual persuasion effectiveness. The apparatus persists through institutional inertia and budgetary momentum rather than demonstrated impact.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_information_program_foreign_advocacy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANTI-COMMUNIST COALITION (SCAFFOLD) — Powerful but constrained actors (NATO allies, developing nations seeking U.S. support) see the information program as a temporary counter to Communist propaganda during the acute Cold War phase. Genuine sunset logic: if communism ceases to be the organizing threat, the justification for systematic U.S. foreign information programs evaporates. The coalition sees the program as transitional support for ideological competition with a definite endpoint.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_information_program_foreign_advocacy, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational view, state communication about its own actions appears as an immutable feature of governance: all states have interest in shaping foreign perceptions, and the attempt to distinguish 'truthful disclosure' from 'persuasion' risks naturalization of what is actually a specific institutional design choice. However, the structural data reveals this as a false summit candidate: the program's extractiveness and suppression metrics contradict mountain classification, indicating the arrangement is contingent, not natural law.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_information_program_foreign_advocacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1955_eisenhower_information_program_foreign_advocacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1955_eisenhower_information_program_foreign_advocacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1955_eisenhower_information_program_foreign_advocacy, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1955_eisenhower_information_program_foreign_advocacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1955_eisenhower_information_program_foreign_advocacy, TR),
    TR >= 0.70.

:- end_tests(sotu_1955_eisenhower_information_program_foreign_advocacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The program extracts from foreign audiences by colonizing their information environment with messaging designed to support U.S. strategic interests rather than to maximize foreign audiences' epistemic access to accurate information about U.S. actions. The extraction is moderate (not 0.70+) because the program does contain genuinely informative content—foreign audiences do learn about U.S. actions—but the selection, framing, and presentation are systematically biased toward U.S. strategic interests. The trajectory from 0.35 to 0.52 reflects that as the program expands, the proportion of messaging driven by strategic interest rather than informational value increases. Suppression (0.48): Foreign audiences face barriers to verification (language, geographic access, institutional mediation through their own governments, lack of counter-narratives they can access and evaluate). But suppression is not total (0.60+) because foreign media and governments can access alternative information sources and critically evaluate U.S. claims. The suppression is structural rather than physical—the program does not prevent audiences from seeking information, but the infrastructure favors U.S. messaging. Theater ratio (0.65): The program is substantially performative. Measuring success by 'messages disseminated' and 'estimated reach' rather than by 'actual change in audience beliefs about U.S. policy' or 'accuracy of foreign audience understanding of U.S. intentions' indicates institutional degradation toward theater. The increase from 0.48 to 0.65 reflects that the program has become entrenched and the original Cold War justification (defensive counter-propaganda) has shifted toward permanent ideological competition.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap emerges between the beneficiary's experience (rope: solving a coordination problem) and the victim's experience (snare: extraction through epistemic control). The U.S. government genuinely perceives a coordination problem—how to inform foreign audiences and allies about U.S. commitment to the Cold War struggle against communism—and experiences the information program as solving that problem at reasonable cost. But from the foreign audience perspective, the program is not neutral information provision; it is strategic messaging designed to make U.S. foreign policy more legitimate, regardless of whether the policy is in the audience's interest. The gap cannot be closed by claims of 'truthfulness' because truthful statements can be selectively disclosed to serve strategic interests. The allied government perspective occupies an unstable intermediate position—they are both beneficiaries (receiving strategic information about U.S. intentions) and victims (their domestic populations are being targeted by U.S. messaging designed to make them more supportive of policies their governments might not otherwise choose). The scaffold perspective perceives this as temporary—the Cold War will end, the informational competition will diminish, the program will sunset—but the piton perspective suggests the apparatus will persist through inertia even if the original justification evaporates. The analytical observer risks seeing all this as natural law (states must compete for information dominance), but this naturalizes what is actually a choice: the U.S. could provide higher-quality information to foreign audiences without the strategic bias, but chooses not to because the bias serves U.S. interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. The U.S. executive branch is clearly the beneficiary (d ≈ 0.05-0.15) with nearly total arbitrage exit: they can redirect resources, adjust messaging, or wind down the program at minimal cost. Foreign civilian audiences are clearly victims (d ≈ 0.85-0.95) with no exit: they cannot leave the geographic region, cannot reliably distinguish U.S. messaging from independent information sources, and have no institutional voice in the program's design or operation. Allied governments occupy an intermediate position (d ≈ 0.50-0.60) because they benefit from coordination on Cold War strategy (lowering d) but face domestic manipulation (raising d) and are constrained by alliance dependence (reducing their effective exit options). The information program bureaucracy, from the piton perspective, occupies institutional arbitrage position (d ≈ 0.20) but the effective extraction chi is dampened by the theater gate—the apparatus has become largely self-referential (measuring reach rather than impact) so the functional extraction has degraded even though the institutional position would suggest high chi. These directionality values feed into the chi formula χ = ε × f(d) × σ(S), where the scope modifier σ(S) for global reach (1.2) amplifies extractiveness for all perspectives. The foreign audience perspective experiences the highest chi because their d is highest (0.90) and the global scope amplifier is maximum (1.2), yielding χ ≈ 0.52 × 1.28 × 1.2 ≈ 0.80, which aligns with snare classification (χ ≥ 0.66). The U.S. beneficiary experiences χ ≈ 0.52 × 0.02 × 1.2 ≈ 0.01 (negative effective extraction), which aligns with rope classification (χ ≤ 0.35). The allied government perspective experiences intermediate χ ≈ 0.52 × 0.65 × 1.2 ≈ 0.41, which aligns with tangled rope classification (0.40 ≤ χ ≤ 0.90). These structural derivations explain the perspectival gap without requiring anchoring assumptions about what the program 'really is'—the program is different things from different structural positions, and those differences are measurable in the chi values.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by acknowledging that the program is genuinely multivalent: it is rope for the beneficiary (legitimate strategic communication), tangled rope for the allies (mixed coordination and domestic manipulation), snare for foreign audiences (extraction through epistemic asymmetry), and piton for the bureaucracy (performative ritual replacing genuine function). The resolution does not yield a single classification but rather a presheaf structure over the observation site. The analytical observer's mountain classification is a false summit—the program is not a law of nature but a specific institutional design choice that benefits the U.S. and extracts from global audiences. The false summit is particularly dangerous because framing the program as natural law obscures that alternative designs are possible: foreign audiences could be provided with higher-quality information about U.S. actions without the strategic bias; allies could be informed without domestic manipulation; the information commons could remain less colonized by state messaging. The mandatrophy resolution requires accepting that the program simultaneously solves real coordination problems and creates real extraction mechanisms—not in a way that can be reformed into pure coordination, but in a way that demands explicit acknowledgment of whose interests are served and whose are harmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    truthfulness_verification_impossibility,
    'What standards of ''truthful disclosure'' distinguish this program from propaganda, and how would foreign audiences verify compliance?',
    'Post-program historical audit: comparison of State Department messaging claims with declassified records; identification of systematic omissions or selective framing that constituted strategic deception despite literal truthfulness',
    'If verification possible: program remains rope (coordination with transparency). If verification impossible: program reclassifies toward snare (extraction through epistemic asymmetry). Current ambiguity: the program claims truthfulness but the selection of what to disclose remains non-transparent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(truthfulness_verification_impossibility, empirical, 'Whether ''truthful disclosure'' standard prevents systematic propaganda').

omega_variable(
    communist_propaganda_threat_legitimacy,
    'Is the Communist information threat a genuine coordination problem requiring state response, or does framing it as such justify extraction by the U.S. of the global information commons?',
    'Comparative analysis of Communist vs. U.S. messaging campaigns: did Soviet/Chinese messaging actually reach global audiences at scale comparable to U.S. infrastructure? Were foreign audiences unable to evaluate competing narratives without U.S. intervention?',
    'If threat genuine: program is rope (defensive coordination). If threat exaggerated: program is snare (offensive extraction justified by false necessity). Current ambiguity: the threat is real but possibly overstated to justify expansion of persuasion infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communist_propaganda_threat_legitimacy, empirical, 'Whether Communist information threat justifies U.S. foreign information program').

omega_variable(
    state_interest_vs_audience_interest_alignment,
    'Can a program simultaneously serve U.S. strategic interests AND serve foreign audiences'' epistemic interests in accurate information about U.S. actions?',
    'Structural analysis: identify cases where U.S. strategic interest and foreign audience epistemic interest diverge; determine whether program messaging prioritizes strategic interest when conflicts occur',
    'If alignment possible: program is tangled rope (genuine coordination with asymmetric extraction). If structural divergence is inevitable: program is snare disguised as rope (extraction that cannot be reformed without eliminating the program entirely).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_interest_vs_audience_interest_alignment, conceptual, 'Whether U.S. strategic interest can align with foreign audience epistemic interest').

omega_variable(
    sunset_timeline_ambiguity,
    'Is the Cold War competition temporary (supporting scaffold classification) or permanent (supporting tangled_rope or snare)?',
    'Policy analysis: do program proponents articulate an end-state where the information program is no longer necessary? Or do they present ideological competition as permanent feature of international relations?',
    'If scaffold: suppression should decline over program interval as Cold War ends. If permanent: suppression remains constant or increases, indicating the program has become entrenched independent of original justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_timeline_ambiguity, empirical, 'Whether foreign information program has a sunset horizon').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1955_eisenhower_information_program_foreign_advocacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1955_eisenhower_information_program_foreign_advocacy, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sotu_tr_t5, sotu_1955_eisenhower_information_program_foreign_advocacy, theater_ratio, 5, 0.58).
narrative_ontology:measurement(sotu_tr_t10, sotu_1955_eisenhower_information_program_foreign_advocacy, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1955_eisenhower_information_program_foreign_advocacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_be_t5, sotu_1955_eisenhower_information_program_foreign_advocacy, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sotu_be_t10, sotu_1955_eisenhower_information_program_foreign_advocacy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1955_eisenhower_information_program_foreign_advocacy, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1955_eisenhower_information_program_foreign_advocacy, cold_war_ideological_competition).
narrative_ontology:affects_constraint(sotu_1955_eisenhower_information_program_foreign_advocacy, foreign_policy_legitimacy_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is upstream of broader Cold War ideological competition mechanisms. The foreign information program is a specific institutional apparatus designed to address the larger structural problem of global ideological competition. The extractiveness difference (0.52 here vs. higher for ideological competition generally) reflects that this program is a particular implementation with measurable institutional boundaries, whereas the broader competition is a civilizational-scale constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1955_eisenhower_information_program_foreign_advocacy, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
