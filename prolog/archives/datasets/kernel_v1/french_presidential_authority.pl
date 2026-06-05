% ============================================================================
% CONSTRAINT STORY: french_presidential_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_presidential_authority, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: french_presidential_authority
 *   human_readable: French Presidential Authority Under Cohabitation
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The French Fifth Republic establishes a dual executive combining a
 *   separately elected president with a separately elected parliament whose
 *   leader (prime minister) controls domestic legislation. During periods of
 *   unified government (president and parliamentary majority aligned),
 *   presidential authority operates as pure coordination — constitutional
 *   delegation to the president of foreign policy, defense, and treaty-making
 *   authority aligns with parliamentary will, creating no extraction
 *   asymmetry. During cohabitation (president and parliament from opposing
 *   coalitions), this same authority structure becomes a tangled constraint:
 *   the president retains ceremonial and constitutional foreign policy roles
 *   but experiences systematic extraction of effective domestic policy
 *   authority by the prime minister and parliament. The constraint exhibits
 *   all six classification types from different perspectives, with the key
 *   dynamic being oscillation between state regimes. Theater ratio (0.62)
 *   reflects that cohabitation requires elaborate constitutional theater:
 *   both president and PM must perform legitimacy through formal protocols
 *   while both retain genuine power in specific domains, creating substantial
 *   performative overhead. Suppression (0.48) indicates moderate barriers to
 *   exit: the president cannot resign without losing office, but can dissolve
 *   parliament if it becomes politically untenable; the PM cannot ignore the
 *   president but can constrain them through parliamentary discipline.
 *
 * KEY AGENTS:
 *   - The President: Primary victim during cohabitation (powerless/trapped) — constitutional authority stripped through political means; no exit except constitutional amendment or parliamentary dissolution
 *   - The Prime Minister and Government: Primary beneficiary during cohabitation (institutional/arbitrage) — controls domestic policy, legislation, budgetary authority; can trigger dissolution if threatened
 *   - Parliamentary Coalition: Secondary actor (organized/constrained) — gains legislative control but faces dissolution risk; experiences constraint as mixed coordination-extraction
 *   - Opposition Parties: External observer (moderate/mobile) — experience cohabitation as stable coordination mechanism that ensures regular alternation
 *   - Constitutional Architects and Reform Advocates: Institutional (institutional/arbitrage) — view cohabitation as solvable through electoral alignment (sunset clause logic)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent constitutional choice as inherent to semi-presidentialism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_presidential_authority, 0.52).
domain_priors:suppression_score(french_presidential_authority, 0.48).
domain_priors:theater_ratio(french_presidential_authority, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_presidential_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(french_presidential_authority, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(french_presidential_authority, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_presidential_authority, tangled_rope).
narrative_ontology:human_readable(french_presidential_authority, "French Presidential Authority Under Cohabitation").
narrative_ontology:topic_domain(french_presidential_authority, "political/constitutional").

domain_priors:requires_active_enforcement(french_presidential_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(french_presidential_authority, '0aa51d2d-b559-4b20-b9b0-9426a8836322').
narrative_ontology:cs_kernel_codification('0aa51d2d-b559-4b20-b9b0-9426a8836322', formalized).
narrative_ontology:cs_authority_grounding('0aa51d2d-b559-4b20-b9b0-9426a8836322', lineage).
narrative_ontology:cs_interpretation_layer_present('0aa51d2d-b559-4b20-b9b0-9426a8836322').
narrative_ontology:cs_reading_relation('0aa51d2d-b559-4b20-b9b0-9426a8836322', french_presidential_unified_government, coexists_with).
narrative_ontology:cs_reading_relation('0aa51d2d-b559-4b20-b9b0-9426a8836322', semi_presidentialism_inherent_cohabitation, forecloses).
narrative_ontology:cs_axiom('0aa51d2d-b559-4b20-b9b0-9426a8836322', foundational, dual_election_creates_cohabitation_risk).
narrative_ontology:cs_axiom_status(dual_election_creates_cohabitation_risk, holdable).
narrative_ontology:cs_axiom_grounding('0aa51d2d-b559-4b20-b9b0-9426a8836322', dual_election_creates_cohabitation_risk, empirically_contingent).
narrative_ontology:cs_axiom('0aa51d2d-b559-4b20-b9b0-9426a8836322', foundational, constitutional_authority_divided_by_domain).
narrative_ontology:cs_axiom_status(constitutional_authority_divided_by_domain, holdable).
narrative_ontology:cs_axiom_grounding('0aa51d2d-b559-4b20-b9b0-9426a8836322', constitutional_authority_divided_by_domain, conventional).
narrative_ontology:cs_axiom('0aa51d2d-b559-4b20-b9b0-9426a8836322', secondary, political_power_determines_effective_authority).
narrative_ontology:cs_axiom_status(political_power_determines_effective_authority, holdable).
narrative_ontology:cs_axiom_grounding('0aa51d2d-b559-4b20-b9b0-9426a8836322', political_power_determines_effective_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('0aa51d2d-b559-4b20-b9b0-9426a8836322', constitutional_dual_executive).
narrative_ontology:cs_drift_state('0aa51d2d-b559-4b20-b9b0-9426a8836322', contemporary_normalized_cohabitation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0aa51d2d-b559-4b20-b9b0-9426a8836322', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_presidential_authority, prime_minister_government).
narrative_ontology:constraint_victim(french_presidential_authority, presidential_executive_function).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESIDENT DURING COHABITATION (SNARE) — Trapped by constitutional architecture that strips domestic policy authority during divided government. Exit impossible without constitutional amendment or parliamentary dissolution. Experiences maximum extraction: presidential powers in foreign policy and defense persist, but domestic authority flows to PM. Career and legacy depend on surviving cohabitation; cannot resign without losing office.
constraint_indexing:constraint_classification(french_presidential_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIME MINISTER DURING COHABITATION (ROPE) — Benefits from constitutional authority over domestic policy, legislation, and budgetary control. Exit option available through parliamentary dissolution. Experiences the constraint as coordination: both president and PM need each other for legitimacy and functional government. No asymmetric extraction — both institutions are necessary.
constraint_indexing:constraint_classification(french_presidential_authority, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENTARY COALITION (TANGLED ROPE) — Faces constrained exit: parliamentary dissolution triggers new elections with uncertain outcomes. Genuine coordination function: the system requires coalition discipline to pass legislation under divided authority. But also extraction: the PM can use presidential constraint to marginalize parliament's internal opponents (the president becomes a political liability the PM can blame for blocked initiatives). Constrained exit + mixed benefits and costs.
constraint_indexing:constraint_classification(french_presidential_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: OPPOSITION PARTIES (ROPE) — Mobile exit: can campaign for next election, build alternative coalitions, negotiate constitutional reforms. Sees cohabitation as pure coordination mechanism that regularized alternation in power. The constraint ensures neither the presidency nor a single-party parliament dominates indefinitely. No extraction from opposition perspective — they experience the system as structural stability mechanism.
constraint_indexing:constraint_classification(french_presidential_authority, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM PATHWAY (SCAFFOLD) — The Fifth Republic's dual executive is temporary by constitutional logic: electoral cycles can align the presidency and parliament, eliminating cohabitation structurally. Sunset mechanism: if parliamentary terms align with presidential terms (as reform advocates propose), the oscillation ends. Theater low because the mechanism is transparent (electoral cycles) and reversible (by future constitutional amendment). Beneficiary: institutional architects who see divided government as a solvable coordination problem.
constraint_indexing:constraint_classification(french_presidential_authority, scaffold,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SEMI-PRESIDENTIALISM VIEW (MOUNTAIN) — From a civilizational perspective, cohabitation is an inherent feature of semi-presidential systems where both president and parliament claim popular sovereignty. The constraint is a structural inevitability: any system combining a separately elected president with a separately elected legislature faces periodic misalignment. Exit is impossible without abandoning semi-presidentialism entirely. This perspective risks naturalizing a contingent constitutional choice (Fifth Republic design) as a universal law of hybrid systems.
constraint_indexing:constraint_classification(french_presidential_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_presidential_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_presidential_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_presidential_authority, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_presidential_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(french_presidential_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The president experiences real loss of domestic policy authority during cohabitation, but extraction is not total — foreign policy, defense, and treaty authority persist as genuine presidential prerogatives. The 0.52 value reflects that domestic authority loss is substantial (primary victim perspective) but not absolute (foreign policy remains). Suppression (0.48): Moderate. The president faces significant barriers to exit — resignation means loss of office, parliamentary dissolution is nuclear option and uncertain. But suppression is not maximal: foreign policy authority persists, and dissolution remains available. Theater ratio (0.62): Moderate-high. The constraint requires constant constitutional theater: formal protocols distinguish presidential from PM prerogatives (foreign policy vs. domestic) while both actors must perform legitimacy. The theater increases during tense cohabitations (Mitterrand-Chirac period saw higher theater) as both actors perform constitutional roles to avoid direct conflict. The measurements show increasing theater and extractiveness over time, reflecting accumulation of cohabitation as an established pattern rather than emergency — as the constraint normalizes, its extractive character becomes more visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a state-dependent classification dynamic. During unified government (president and parliament aligned), the same constitutional structure would classify as Rope from most perspectives — both actors have incentives to coordinate because their political fates are linked. During cohabitation, the identical constitutional text produces Snare from the president's perspective (trapped, powerless, maximum extraction) and Rope from the PM's perspective (arbitrage exit, genuine coordination). The perspectival gap reveals that the constraint's type oscillates with political alignment, not with constitutional text change. The parliamentary coalition sees tangled rope because they face constrained exit (dissolution risk) but also benefit from domestic control. The opposition sees rope because cohabitation serves their long-term interest in power alternation. The analytical observer risks false summit by treating the oscillation itself as a natural law of semi-presidentialism, rather than a contingent outcome of the Fifth Republic's specific constitutional design.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiary is the prime minister and parliamentary government during cohabitation: they gain control of domestic legislation, budgetary authority, and civil service direction. Directionality for PM: d ≈ 0.15 (beneficiary with arbitrage exit). The primary victim is the president: they experience authority loss in the domain they expected to control. Directionality for president: d ≈ 0.90 (victim with trapped exit). The derived chi for PM would be low (beneficiary + arbitrage → negative effective extraction); the derived chi for president would be high (victim + trapped → maximum effective extraction). The parliamentary coalition has mixed directionality: they benefit from domestic control but face suppression from the trapped president's potential use of constitutional powers (treaty vetoes, emergency powers). The opposition has neutral directionality: they experience the constraint as structural stability, not extraction — their exit option (electoral cycle) is mobile, and they benefit from the system's regularized alternation norm.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY SIGNATURE: This constraint exhibits mandatrophy because the same constitutional structure produces contradictory classifications depending on political alignment state. During unified government, the constraint appears as Rope (coordination with no asymmetric extraction). During cohabitation, it appears as Tangled Rope or Snare (extraction with formal coordination). The resolution is not 'which classification is correct?' but 'the constraint is state-dependent, and the oscillation between states is itself the structural feature.' The system does not evolve toward equilibrium; it cycles between unified and divided government regimes on timescales determined by electoral cycles and coalition stability. Mandatrophy is resolved by recognizing that the presheaf over observation sites includes both regime states, and the system's behavior cannot be reduced to a single type. The theater ratio and extractiveness measurements capture the regime oscillation: as cohabitation normalizes and becomes politically routine rather than constitutional crisis, the theater ratio and extractiveness both rise. This rising trajectory reflects not that the constraint is becoming more extractive in any absolute sense, but that its extractive character becomes more visible and formalized as actors accept cohabitation as permanent institutional feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_causation_mechanism,
    'Is presidential authority loss during cohabitation caused by constitutional text, institutional convention, or political power dynamics?',
    'Comparative analysis: cohabitation in Portugal, Taiwan, and Russia shows varied outcomes despite similar constitutional texts. Trace specific incidents where presidents attempted to exercise suppressed authority and observe whether constitutional barriers or political opposition stopped them.',
    'If constitutional: mountain classification more defensible. If conventional: tangled_rope classification correct — the constraint is maintained by enforcement choice, not structural law. If political: snare classification from PM perspective is justified; extraction is active, not passive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohabitation_causation_mechanism, empirical, 'Root cause of presidential authority loss during cohabitation').

omega_variable(
    pm_extractive_capacity_during_unified_government,
    'When parliament and presidency align, does the PM''s authority collapse entirely, or do PM and president maintain extractive dynamics under unified government framing?',
    'Institutional history analysis: Chirac-Raffarin period (unified), Macron-Philippe period (unified) vs. Mitterrand-Chirac cohabitations. Track: legislative initiative authorship, budget control, treaty negotiation leadership. Measure whether PM discretion persists under unified government or whether president achieves true delegation.',
    'If PM authority collapses: constraint is state-dependent (Rope during unified, Snare during cohabitation). If PM retains extractive capacity: constraint is tangled_rope regardless of alignment — extraction persists, only visibility changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pm_extractive_capacity_during_unified_government, empirical, 'Whether PM extraction is state-dependent or persistent').

omega_variable(
    cohabitation_terminal_stability,
    'Is cohabitation a stable equilibrium that will persist indefinitely, a temporary phase being eliminated by electoral reform, or an oscillating attractor that returns regardless of reform attempts?',
    'Historical trajectory analysis: electoral reforms (2000 presidential term alignment initiative), constitutional amendment proposals (reducing presidential term to 5 years), demographic/coalition stability analysis showing whether future elections will lock parliament and presidency together.',
    'If temporary: scaffold classification confirmed — sunset is structural. If persistent: piton risk increases — enforcement effort rises as actors normalize the constraint. If oscillating: mountain risk increases — the system may be inherently unable to eliminate misalignment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cohabitation_terminal_stability, empirical, 'Long-term trajectory of cohabitation as constitutional feature').

omega_variable(
    foreign_policy_true_presidential_autonomy,
    'During cohabitation, does the president retain genuine autonomy in foreign policy and defense, or does the PM extract veto power through informal political channels?',
    'Case analysis: specific treaties, defense spending decisions, NATO/EU votes, intelligence agency oversight during cohabitations (Mitterrand-Chirac, Chirac-Jospin). Track whether PM formally approved or blocked presidential initiatives; measure scope of informal PM influence on presidential decisions.',
    'If true autonomy: foreign policy extraction is minimal (ε for that domain ≈ 0.15). If PM veto common: foreign policy extraction is high (ε ≈ 0.55), making cohabitation snare from broader perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_policy_true_presidential_autonomy, empirical, 'Whether presidential foreign policy autonomy is genuine or formally permitted but politically constrained').

omega_variable(
    false_summit_risk_semi_presidentialism,
    'Is the constraint a true natural law of semi-presidential systems (mountain), or a false summit naturalizing a contingent Fifth Republic design that could be eliminated by constitutional choice?',
    'Comparative constitutional analysis: France vs Portugal (cohabitation rare, weaker presidency), vs Romania (cohabitation common, strong presidency), vs hybrid systems (Germany, Italy — no separately elected president). Identify whether the constraint is inherent to the form or specific to French constitutional implementation.',
    'If false summit: the ''naturalness'' framing is a cover story; the constraint is tangled_rope or snare, not mountain. Political actors who frame cohabitation as inevitable law deflect responsibility for constitutional choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_risk_semi_presidentialism, conceptual, 'Whether cohabitation is inherent to semi-presidentialism or specific to Fifth Republic design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_presidential_authority, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frpres_tr_t0, french_presidential_authority, theater_ratio, 0, 0.42).
narrative_ontology:measurement(frpres_tr_t5, french_presidential_authority, theater_ratio, 5, 0.55).
narrative_ontology:measurement(frpres_tr_t10, french_presidential_authority, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(frpres_be_t0, french_presidential_authority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(frpres_be_t5, french_presidential_authority, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(frpres_be_t10, french_presidential_authority, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(frpres_su_t0, french_presidential_authority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(frpres_su_t5, french_presidential_authority, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(frpres_su_t10, french_presidential_authority, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_presidential_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(french_presidential_authority, french_parliamentary_legislative_authority).
narrative_ontology:affects_constraint(french_presidential_authority, french_treaty_ratification_authority).

% DUAL FORMULATION NOTE:
% French presidential authority under cohabitation is decomposable into separate constraints: (1) domestic legislative authority (primarily controlled by PM during cohabitation), (2) foreign policy and treaty authority (retained by president), (3) emergency powers (retained by president under Article 16 but politically constrained during cohabitation). This story models the overall oscillation dynamic. Upstream constraints include specific domain-authority divisions (treaty ratification, legislative initiative) that have their own ε values. Downstream constraints include political party coalition dynamics that determine whether cohabitation occurs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
