% ============================================================================
% CONSTRAINT STORY: personalist_dictatorship__cult_information_pathology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personalist_dictatorship__cult_information_pathology, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: personalist_dictatorship__cult_information_pathology
 *   human_readable: Personality Cult Information Pathology in Personalist Dictatorship
 *   domain: political/comparative/autocracy
 *
 * SUMMARY:
 *   The personality cult information pathology is a structural constraint
 *   within personalist dictatorships where the cult apparatus creates a
 *   systematic suppression of accurate information. When praise becomes
 *   mandatory and dissent becomes existentially threatening, officials and
 *   administrators optimize for pleasing the ruler rather than reporting
 *   facts. The result is that the autocrat increasingly governs a country
 *   that exists only in briefing documents — a fictional state constructed
 *   from filtered, flattering reports disconnected from actual conditions on
 *   the ground. This reading of the personalist dictatorship kernel focuses
 *   specifically on how the regime's authority structure (concentrated in
 *   charismatic personality) creates epistemic degradation that eventually
 *   undermines the regime itself. The constraint exhibits the signature of a
 *   snare for most observers: high suppression (0.82), high extractiveness
 *   (0.68), and high theater (0.85). The beneficiaries are sycophants and
 *   cult administrators who thrive on the mandatory praise extraction; the
 *   victims are decision quality, the ruler's epistemic access to reality,
 *   and state capacity. The measurement trajectory shows extractiveness and
 *   suppression ratcheting upward over time as the pathology deepens — each
 *   iteration of the cult requires more elaborate flattery, which
 *   necessitates more suppression of alternatives, which further distances
 *   the ruler from reality.
 *
 * KEY AGENTS:
 *   - Sycophant Courtiers: Primary beneficiaries (institutional/arbitrage) — profit from mandatory praise extraction; their career advancement and material rewards depend on providing increasingly elaborate flattery
 *   - Subordinate Officials: Primary victims (powerless/trapped) — face career death for truth-telling; trapped in a regime where speaking accurately is existentially threatening
 *   - Security Apparatus: Enforcer and victim (moderate/constrained) — tasked with eliminating dissent and enforcing cult, but also governed by the same information pathology; cannot exit without losing institutional position
 *   - State Bureaucracy: Secondary victim (powerless/trapped) — collectively trapped; cannot perform its function (effective governance) because accurate information is suppressed; faces slow degradation and policy failures
 *   - Ruler: Paradoxical position — nominally in control but actually trapped by his own cult (powerless/trapped in terms of epistemic access, though institutionally powerful); victim of his own authority structure
 *   - Decision Quality and State Capacity: Abstract victim (powerless/trapped) — the regime's decisions become increasingly detached from reality as the cult supplies only pleasing information; infrastructure fails, policy misallocates, crises go unmanaged
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personalist_dictatorship__cult_information_pathology, 0.68).
domain_priors:suppression_score(personalist_dictatorship__cult_information_pathology, 0.82).
domain_priors:theater_ratio(personalist_dictatorship__cult_information_pathology, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personalist_dictatorship__cult_information_pathology, extractiveness, 0.68).
narrative_ontology:constraint_metric(personalist_dictatorship__cult_information_pathology, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(personalist_dictatorship__cult_information_pathology, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personalist_dictatorship__cult_information_pathology, snare).
narrative_ontology:human_readable(personalist_dictatorship__cult_information_pathology, "Personality Cult Information Pathology in Personalist Dictatorship").
narrative_ontology:topic_domain(personalist_dictatorship__cult_information_pathology, "political/comparative/autocracy").

domain_priors:requires_active_enforcement(personalist_dictatorship__cult_information_pathology).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personalist_dictatorship__cult_information_pathology, '7dc36c27-144f-409e-b6e1-9ea87c6d9cc8').
narrative_ontology:cs_kernel_codification('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', implicit).
narrative_ontology:cs_authority_grounding('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', extraction).
narrative_ontology:cs_reading_relation('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', personalist_dictatorship__charisma_routinization_problem, influences).
narrative_ontology:cs_reading_relation('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', personalist_dictatorship__coup_proofing_mechanics, coexists_with).
narrative_ontology:cs_axiom('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', foundational, mandatory_praise_destructive_epistemic_access).
narrative_ontology:cs_axiom_status(mandatory_praise_destructive_epistemic_access, holdable).
narrative_ontology:cs_axiom_grounding('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', mandatory_praise_destructive_epistemic_access, empirically_contingent).
narrative_ontology:cs_axiom('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', foundational, cult_short_term_power_long_term_degradation).
narrative_ontology:cs_axiom_status(cult_short_term_power_long_term_degradation, holdable).
narrative_ontology:cs_axiom_grounding('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', cult_short_term_power_long_term_degradation, empirically_contingent).
narrative_ontology:cs_reference_frame('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', cult_authority_through_isolation).
narrative_ontology:cs_drift_state('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', contemporary, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('7dc36c27-144f-409e-b6e1-9ea87c6d9cc8', '').
narrative_ontology:cs_kernel_id(personalist_dictatorship__cult_information_pathology, personalist_dictatorship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personalist_dictatorship__cult_information_pathology, sycophant_courtiers).
narrative_ontology:constraint_beneficiary(personalist_dictatorship__cult_information_pathology, cult_administrators).
narrative_ontology:constraint_victim(personalist_dictatorship__cult_information_pathology, decision_quality).
narrative_ontology:constraint_victim(personalist_dictatorship__cult_information_pathology, ruler_epistemic_access).
narrative_ontology:constraint_victim(personalist_dictatorship__cult_information_pathology, state_capacity).
narrative_ontology:constraint_victim(personalist_dictatorship__cult_information_pathology, reality_based_governance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE OFFICIAL (SNARE) — Trapped in a regime where truth-telling carries career death risk and silence carries complicity. Mandatory praise creates a ratchet: each report must exceed the last in loyalty performance. Officials cannot exit, cannot speak truth, cannot refuse the extraction of their epistemic integrity. Maximum experienced coercion.
constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SYCOPHANT COURTIER (ROPE) — Primary beneficiary. Thrives on mandatory praise extraction. Access to the ruler, material rewards, and status depend on providing the flattery the cult infrastructure requires. Experiences the constraint as pure coordination: delivering the performance the system demands. Low suppression from this perspective — this agent wants the constraint to persist.
constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SECURITY APPARATUS (SNARE) — Trapped in enforcing the cult and suppressing alternative information flows. Tasked with eliminating dissent and enforcing mandatory praise, but also governed by the same information pathology. Intelligence agencies produce flattery instead of analysis. Cannot exit without losing institutional position; cannot speak truth without being seen as disloyalty or weakness.
constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TECHNOCRATIC REFORMER (TANGLED ROPE) — Senior official with partial power and access to ruler. Sees genuine coordination value in the cult structure (it concentrates authority, enabling rapid decisions) but also bears costs from information suppression (policy failures multiply). Mobile at longer time horizons (can defect, retire, or pivot) but constrained at shorter ones (must maintain favor). Mixed extraction and coordination.
constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE BUREAUCRACY (SNARE) — Collectively trapped. Cannot perform its actual function (effective governance based on accurate information) because accurate information is suppressed by the cult. The institution faces slow degradation as policy failures accumulate due to epistemic suppression. No exit from the regime's information pathology; trapped in a system that prevents institutional learning.
constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: DECISION QUALITY / STATE CAPACITY (SNARE) — Abstract victim. The regime's decisions are increasingly detached from reality as the cult supplies only pleasing information. Infrastructure fails; economic policy misallocates; foreign policy ignores signals; public health crises go unmanaged. The ruler governs a country that exists only in briefing documents, not the territory he controls. Maximum extraction of epistemic integrity from the state itself.
constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: CULT APPARATUS (PITON) — The formal mechanisms of the personality cult (propaganda, censorship, loyalty rituals, mandatory praise) persist through institutional inertia. The apparatus was built to concentrate power and eliminate threats, but its primary function has degraded into theater. The machinery continues to run because it defines the regime's authority structure, not because it effectively concentrates power anymore — the power concentration now rests on information suppression itself, which is the pathology. Theater ratio ≥ 0.85: most cult activity is performative rather than functionally necessary.
constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / IMMUTABLE AUTHORITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, personalist rule is treated as an irreducible feature of certain political systems: authority concentrated in one person creates inevitable information pathologies. The ruler is the ultimate source of truth by definition; alternative information sources are threats to coherence. However, this is a false summit — the structural data (high suppression, high theater, identifiable beneficiaries, specific victims) reveals that the information pathology is a contingent institutional choice, not a law of political nature.
constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personalist_dictatorship__cult_information_pathology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personalist_dictatorship__cult_information_pathology, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personalist_dictatorship__cult_information_pathology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(personalist_dictatorship__cult_information_pathology, TR),
    TR >= 0.70.

:- end_tests(personalist_dictatorship__cult_information_pathology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The cult extracts the epistemic integrity of officials, administrators, and ultimately the ruler himself. Early in a regime's personalist phase, extractiveness is lower (0.42) because truthful reporting may still occur and be tolerated; officials still believe they can influence the ruler through accurate information. As the cult matures, extractiveness rises (0.58, then 0.68) as the incentive structure locks in: career advancement, material rewards, and survival all depend on flattery. The extraction is not primarily material wealth (though sycophants do gain wealth) but epistemic: the regime extracts truth from the state and replaces it with performance. Suppression (0.82): Very high. The regime actively suppresses alternative information sources (independent media, accurate reporting, dissent). No official can speak truth without career death. Intelligence agencies are converted to propaganda machines. The suppression increases over time (0.65 → 0.75 → 0.82) as the cult matures and the ruler becomes increasingly insulated. Theater (0.85): Very high. Most activity in the cult apparatus is performative. Loyalty oaths, parade attendance, slogans, propaganda — these are theatrical performances masquerading as governance. The theater increases over time as the gap between cult narrative and reality widens; more elaborate fiction is needed to maintain the narrative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates high perspectival divergence. The sycophant courtier sees a rope — pure coordination delivering the performance the system demands, with generous rewards. The subordinate official sees a snare — trapped in a system where truth is lethal and flattery is mandatory. The ruler supposedly occupies the apex of power but actually inhabits a snare of his own making — increasingly cut off from reality, governed only by pleasing fiction. The analytical observer at civilizational scope risks naturalizing this as an immutable feature of personalist rule (mountain), but the structural data reveals it as a contingent institutional pathology with specific beneficiaries (sycophants) and specific victims (decision quality, state capacity, the ruler's own epistemic access). The piton perspective recognizes that much of the cult machinery is performative — it persists through institutional inertia rather than functional necessity, yet removing it would require dismantling the regime's authority structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values (d) are derived from beneficiary/victim declarations and exit options. Sycophants have arbitrage-level exit (they can defect to other regimes, seek asylum, or switch allegiances) and are primary beneficiaries, yielding low d and negative effective extraction — they experience the constraint as beneficial. Subordinate officials are trapped with no real exit and are victims, yielding high d and high experienced extraction. The security apparatus is constrained (can move between positions within the regime but not exit) and is both enforcer and victim, yielding moderate-high d. The ruler nominally has institutional power but is actually trapped by his own information structure, yielding paradoxical directionality: institutionally powerful but epistemically powerless (trapped), producing mixed signals. The state bureaucracy as a collective is powerless and trapped, yielding maximum d.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy because it is a pure snare from nearly all perspectives (except the sycophants). The constraint is not misclassified as coordination — it is clearly extractive. The challenge is not mandatrophy resolution but understanding the ruler's paradoxical position: he is nominally the beneficiary and sole authority, but he is actually a victim of his own cult because his information access has been catastrophically suppressed. The cult serves him only in the short term (concentration of power, elimination of threats); in the medium to long term, it destroys his governance capacity by cutting him off from reality. This is not mandatrophy but temporal divergence in interests: the short-run interest (power concentration) conflicts with the long-run interest (effective governance). The ruler would need to recognize and reverse the information pathology, which requires admitting that his courtiers have been deceiving him — an admission that threatens the entire authority structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cult_enforcement_mechanism,
    'Is the mandatory praise enforced primarily by active suppression (punishment of truth-telling, censorship) or by structural incentives (career advancement through flattery, resource allocation to loyalty)?',
    'Analysis of regime enforcement patterns: punishments for dissent vs. rewards for flattery; measurement of relative magnitude of carrot vs. stick mechanisms; case studies of officials who attempted truth-telling vs. those who thrived through sycophancy',
    'If primarily punishment-based (active suppression): suppression metric remains ≥ 0.82, classification stable. If primarily incentive-based (structural): the ''snare'' classification may shift toward ''tangled_rope'' for officials with high-status positions (who see coordination benefits), narrowing the victim set to lower-rank officials and state capacity',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cult_enforcement_mechanism, empirical, 'Relative weight of punishment vs. incentive mechanisms in cult enforcement').

omega_variable(
    ruler_awareness_paradox,
    'Does the ruler knowingly govern through falsified information (aware of the pathology but accepting it), or is the ruler genuinely deceived by his own cult (unaware that reports are systematically flattering)?',
    'Analysis of ruler''s private communications, decision patterns when surprising information breaks through the cult filter, and comparison of public statements vs. confidential directives; case studies of moments when rulers discovered suppressed facts',
    'If knowingly deceived: the cult is maintained as a control mechanism; the extractiveness may be lower (more calculated) or the suppression higher (more deliberate). If genuinely deceived: the ruler is a victim of his own cult, not merely a beneficiary; the classification shifts to include the ruler in the victim set rather than the beneficiary set, potentially lowering extractiveness (the extraction is self-directed pathology rather than strategic extraction). This distinction affects the reading''s coherence — does the cult information pathology serve the ruler or destroy him?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ruler_awareness_paradox, empirical, 'Ruler awareness of systematic information suppression within the cult').

omega_variable(
    comparison_across_regimes,
    'Is the information pathology unique to personalist dictatorships with personality cults, or do bureaucratic authoritarians and single-party states exhibit similar (though structurally different) information degradation?',
    'Cross-regime comparison of information quality, policy failures, and decision-maker epistemic access in: personalist cults (Mobutu, Marcos, Mubarak), bureaucratic authoritarians (South Korea, Taiwan under Park/Chiang), single-party states (Soviet Union, China, Vietnam), and institutional militaries (Brazil 1964-85, Argentina 1976-83)',
    'If pathology is unique to personality cults: this reading is a specific constraint within the personalist dictatorship kernel. If pathology is common across regime types with different structural mechanisms: the constraint should be decomposed into separate stories for each regime type, each with its own extractiveness and suppression values. This affects whether the constraint story is about the personality cult or about information degradation under authoritarianism more broadly',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparison_across_regimes, empirical, 'Whether information pathology is unique to personalist cults or common to multiple regime types').

omega_variable(
    regime_collapse_timing,
    'Does the regime collapse when the information pathology reaches a critical threshold (policy failures become visible, state capacity degrades beyond functionality), or does the cult persist indefinitely despite epistemic collapse?',
    'Historical analysis of regime trajectories: did regimes fall due to state capacity degradation from information pathology (Shah of Iran, Marcos in Philippines, Mubarak in Egypt) or did cults persist despite degradation (North Korea, Zimbabwe under Mugabe, Syria under Assad)? What structural features predict collapse vs. persistence?',
    'If threshold-based collapse: the constraint has a natural sunset (the regime self-destructs when information pathology makes governance impossible). If indefinite persistence: the constraint does not self-correct; suppression must increase continuously to maintain control as failures accumulate. This affects the theater trajectory — does it stabilize at a high level or continue ratcheting upward?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_collapse_timing, empirical, 'Relationship between information pathology severity and regime collapse timing').

omega_variable(
    kernel_reading_contest,
    'Is this reading (cult_information_pathology) the primary structural mechanism of personalist dictatorship, or is it a downstream consequence of the other two readings (charisma_routinization_problem and coup_proofing_mechanics)?',
    'Historical and theoretical analysis of causal primacy: does the cult information pathology drive the need for charisma routinization and coup-proofing, or are charisma routinization and coup-proofing the primary mechanisms that necessitate the information pathology? Can personalist regimes exist with low information pathology but high coup-proofing (counterexample: benevolent autocrat with strong reality-checking)?',
    'If primary mechanism: this reading explains both the regime''s authority structure and its eventual decay — the information pathology is the fundamental constraint. If downstream consequence: this reading describes a symptom of the deeper problems of charisma routinization and coup-proofing, and would be classified as influenced_by rather than coexists_with the other readings. This affects the reading_relations declarations in cs_structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Causal primacy of information pathology vs. other personalist dictatorship mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personalist_dictatorship__cult_information_pathology, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cultic_tr_t0, personalist_dictatorship__cult_information_pathology, theater_ratio, 0, 0.68).
narrative_ontology:measurement(cultic_tr_t5, personalist_dictatorship__cult_information_pathology, theater_ratio, 5, 0.78).
narrative_ontology:measurement(cultic_tr_t10, personalist_dictatorship__cult_information_pathology, theater_ratio, 10, 0.85).

% Extraction over time
narrative_ontology:measurement(cultic_be_t0, personalist_dictatorship__cult_information_pathology, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cultic_be_t5, personalist_dictatorship__cult_information_pathology, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(cultic_be_t10, personalist_dictatorship__cult_information_pathology, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cultic_su_t0, personalist_dictatorship__cult_information_pathology, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(cultic_su_t5, personalist_dictatorship__cult_information_pathology, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(cultic_su_t10, personalist_dictatorship__cult_information_pathology, suppression_requirement, 10, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personalist_dictatorship__cult_information_pathology, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(personalist_dictatorship__cult_information_pathology, 0.35).
narrative_ontology:affects_constraint(personalist_dictatorship__cult_information_pathology, personalist_dictatorship__charisma_routinization_problem).
narrative_ontology:affects_constraint(personalist_dictatorship__cult_information_pathology, personalist_dictatorship__coup_proofing_mechanics).

% DUAL FORMULATION NOTE:
% The personality cult information pathology is one reading of the personalist dictatorship kernel. It is downstream of but structurally distinct from the charisma routinization problem (which addresses succession) and coup-proofing mechanics (which addresses internal security). The three readings together form a coherent kernel contest: different mechanisms by which personalist rule persists and fails. Decomposition is not appropriate here because all three readings are structural features of the SAME regime type — they are competing explanations of how personalist dictatorship operates, not different constraints with different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personalist_dictatorship__cult_information_pathology, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
