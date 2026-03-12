% ============================================================================
% CONSTRAINT STORY: temporal_asymmetry_of_obligation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_asymmetry_of_obligation, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temporal_asymmetry_of_obligation
 *   human_readable: Temporal Asymmetry of Obligation in Gift Exchange
 *   domain: moral_philosophy/social_psychology/economic_anthropology
 *
 * SUMMARY:
 *   The temporal asymmetry of obligation in gift exchange creates a
 *   structural tension between the coordination function of mutual aid
 *   (insurance, resource smoothing, social cohesion) and the extraction
 *   mechanism of compounding, non-clearable debt. When aid is given, the
 *   recipient incurs an obligation that — in many cultural contexts — cannot
 *   be 'settled' through equivalent return. The obligation either persists
 *   indefinitely (creating durable hierarchy) or is 'passed forward' to third
 *   parties (diffusing to the commons). This constraint was hypothesized as a
 *   mountain (universal feature of gift economies) but the structural data
 *   reveals significant extractive overhead (ε=0.42) and suppression (σ=0.48)
 *   beyond pure coordination cost. The analytical observer risks naturalizing
 *   a contingent cultural arrangement as an immutable law of reciprocity. The
 *   constraint exhibits perspectival diversity: chronic recipients experience
 *   identity-locked extraction (snare), reciprocating participants experience
 *   mixed coordination and hierarchy (tangled rope), benefactors experience
 *   status-generating coordination (rope), mutual aid collectives see a
 *   temporary problem with a sunset (scaffold), and exit seekers experience
 *   both coordination loss and extraction relief (tangled rope). The
 *   theater_ratio (0.35) reflects moderate performative content: some
 *   obligation narratives are genuine (internalized moral debt), while others
 *   are strategic (claiming obligation to justify continued network access or
 *   to avoid social penalty for exit).
 *
 * KEY AGENTS:
 *   - Chronic Recipients: Primary victims (powerless/identity_locked) — internalized obligation narrative constitutes identity; cannot exit without self-concept rupture; experience compounding moral debt
 *   - Reciprocating Participants: Mixed position (moderate/constrained) — experience both coordination value (mutual aid insurance) and extraction (status hierarchy through temporal debt asymmetry); can exit at social cost
 *   - Original Benefactors: Primary beneficiaries (institutional/arbitrage) — convert material transfer into durable social capital and status; experience low effective extraction because obligation runs toward them
 *   - Mutual Aid Collectives: Organized agents (organized/mobile) — building formalized 'pay it forward' systems that diffuse obligation to the commons rather than compounding it dyadically; see sunset as norms mature
 *   - Exit Seekers: Secondary victims (moderate/mobile) — attempting to leave obligation network; experience coordination loss and extraction relief simultaneously; mobile but at significant social cost
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the asymmetry as universal feature of gift exchange, missing the contingent institutional and cultural mechanisms that determine whether obligation decays, compounds, or diffuses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_asymmetry_of_obligation, 0.42).
domain_priors:suppression_score(temporal_asymmetry_of_obligation, 0.48).
domain_priors:theater_ratio(temporal_asymmetry_of_obligation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_asymmetry_of_obligation, extractiveness, 0.42).
narrative_ontology:constraint_metric(temporal_asymmetry_of_obligation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(temporal_asymmetry_of_obligation, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temporal_asymmetry_of_obligation, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(temporal_asymmetry_of_obligation, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_asymmetry_of_obligation, tangled_rope).
narrative_ontology:human_readable(temporal_asymmetry_of_obligation, "Temporal Asymmetry of Obligation in Gift Exchange").
narrative_ontology:topic_domain(temporal_asymmetry_of_obligation, "moral_philosophy/social_psychology/economic_anthropology").

domain_priors:requires_active_enforcement(temporal_asymmetry_of_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_asymmetry_of_obligation, original_benefactor).
narrative_ontology:constraint_beneficiary(temporal_asymmetry_of_obligation, community_cohesion).
narrative_ontology:constraint_victim(temporal_asymmetry_of_obligation, chronic_recipients).
narrative_ontology:constraint_victim(temporal_asymmetry_of_obligation, exit_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHRONIC RECIPIENT (SNARE) — Identity-locked by internalized obligation narrative ('I owe everything to those who helped me'). Structurally mobile (could relocate, could decline future aid) but identity is constituted through the debt relationship. Experiences the constraint as inescapable moral burden that compounds rather than clears. The obligation asymmetry extracts agency — the recipient cannot 'settle' because the framing prohibits balanced exchange.
constraint_indexing:constraint_classification(temporal_asymmetry_of_obligation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RECIPROCATING PARTICIPANT (TANGLED ROPE) — Experiences both genuine coordination (mutual aid networks provide real insurance value) and extraction (obligation persists asymmetrically, creating status hierarchy). Can exit at cost (social penalty, loss of network access). The constraint coordinates resource flows while embedding power asymmetry through temporal framing of debt.
constraint_indexing:constraint_classification(temporal_asymmetry_of_obligation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ORIGINAL BENEFACTOR (ROPE) — Experiences the constraint as coordination mechanism. Giving creates durable social bonds and status. Can exit freely (stop giving, move to different community). The temporal asymmetry benefits this agent by converting material transfer into lasting social capital. Low effective extraction because the agent is net beneficiary of the obligation structure.
constraint_indexing:constraint_classification(temporal_asymmetry_of_obligation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MUTUAL AID COLLECTIVE (SCAFFOLD) — Organized groups building explicit 'pay it forward' norms with sunset logic: as the network matures, obligation becomes diffuse (owed to the commons, not to specific benefactors), reducing extraction. Rotating credit associations, time banks, and formalized mutual aid structures are creating alternative pathways where obligation clears through system participation rather than compounding personally. Estimated sunset: 1-2 generations as norms shift from dyadic debt to collective reciprocity.
constraint_indexing:constraint_classification(temporal_asymmetry_of_obligation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, temporal asymmetry of obligation appears as a universal feature of human reciprocity: all gift economies create durable bonds, and the impossibility of exact repayment is what generates social cohesion. This framing treats the asymmetry as an immutable property of gift exchange itself. However, the structural data contradicts this — the constraint's extractiveness (0.42) and suppression (0.48) indicate significant coercive overhead beyond pure coordination. The 'natural law' framing naturalizes what is actually a contingent cultural arrangement. The engine's false summit detector will flag this as naturalization of institutional choice.
constraint_indexing:constraint_classification(temporal_asymmetry_of_obligation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: EXIT SEEKER (TANGLED ROPE) — Agent attempting to leave the obligation network (geographic relocation, cultural assimilation, explicit debt repayment). Experiences both coordination loss (losing mutual aid access) and extraction relief (escaping compounding obligation). Mobile exit options but significant cost (social rupture, loss of identity continuity). The constraint coordinates and extracts simultaneously — the exit seeker sees both functions clearly.
constraint_indexing:constraint_classification(temporal_asymmetry_of_obligation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_asymmetry_of_obligation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_asymmetry_of_obligation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_asymmetry_of_obligation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(temporal_asymmetry_of_obligation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The temporal asymmetry creates genuine extraction through compounding obligation that cannot be cleared, but the extraction is not maximal — some agents benefit (benefactors gain status), and some cultural contexts have developed 'pass forward' norms that reduce dyadic hierarchy. The value reflects that the asymmetry coordinates (mutual aid provides real insurance) while extracting (obligation persists beyond functional necessity, creating status hierarchy). Suppression (0.48): Moderate. Significant barriers to exit include social penalty (ostracism, loss of network access), identity costs (self-concept as 'indebted' or 'loyal'), and material dependency (loss of mutual aid insurance). But suppression is not total — exit is possible through geographic relocation, cultural assimilation, or explicit debt repayment attempts. Theater ratio (0.35): Moderate-low. Some obligation narratives are genuine internalized moral commitments; others are strategic performances to maintain network access or avoid exit costs. The theater has increased slightly over the interval as formalized mutual aid systems (time banks, rotating credit) have made the performative vs genuine distinction more visible. Accessibility collapse (0.52) and resistance (0.48): Both moderate, indicating the constraint is NOT a natural law despite the analytical observer's framing. Alternative coordination mechanisms (formalized reciprocity, market exchange, state redistribution) are accessible, and the constraint faces cultural resistance (individualist norms, 'pay it forward' movements, explicit debt-clearing practices).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — temporal asymmetry in gift obligation — produces five distinct classification types depending on the observer's position. Chronic recipients see a snare (identity-locked extraction with no exit). Reciprocating participants and exit seekers see tangled rope (mixed coordination and extraction). Benefactors see rope (status-generating coordination). Mutual aid collectives see scaffold (temporary problem with sunset as norms mature). The analytical observer sees mountain (universal law of reciprocity) but this is a false summit — the constraint's extractiveness and suppression exceed natural law thresholds, and alternative coordination mechanisms are accessible. The perspectival gap is diagnostic: the beneficiary's rope and the recipient's snare are both structurally accurate readings from their respective positions. The gap reveals that 'is obligation asymmetry natural?' is the wrong question — the right question is 'from which structural position are you measuring?'
 *
 * DIRECTIONALITY LOGIC:
 *   Chronic recipients are victims with identity_locked exit — their directionality is high (d ≈ 0.89) because they bear the obligation asymmetry and cannot exit without identity rupture. The identity-lock is cognitive rather than material: they could structurally leave (relocate, decline aid) but their self-concept is constituted through the debt relationship. Reciprocating participants are mixed — they appear in both beneficiaries (mutual aid coordination) and victims (obligation asymmetry) arrays, with constrained exit. Their directionality is moderate (d ≈ 0.55), reflecting balanced costs and benefits. Original benefactors are pure beneficiaries with arbitrage exit — their directionality is low (d ≈ 0.05) because obligation runs toward them (they gain status) and they can exit freely. Exit seekers are victims with mobile exit — their directionality is moderate-high (d ≈ 0.70) because they bear extraction but have agency to leave at cost. The mutual aid collective is a beneficiary (coordination function) with mobile exit — their directionality is low (d ≈ 0.25) because they experience the constraint as a solvable coordination problem. The analytical observer uses canonical analytical directionality (d ≈ 0.73), but the mountain classification is flagged as a false summit because the structural metrics (ε=0.42, σ=0.48, accessibility_collapse=0.52) contradict natural law thresholds.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that the mountain hypothesis (temporal asymmetry is a universal, immutable feature of gift exchange) is a naturalization of contingent cultural arrangements. The analytical observer's mountain classification fails the false summit test: extractiveness (0.42) exceeds the mountain threshold (≤0.25), suppression (0.48) exceeds the mountain threshold (≤0.05), and accessibility collapse (0.52) falls below the mountain threshold (≥0.85). Alternative coordination mechanisms exist (formalized reciprocity, market exchange, state redistribution), and cultural resistance is significant (individualist norms, 'pay it forward' movements). The constraint is not a law of nature — it is a tangled rope that coordinates mutual aid while embedding status hierarchy through temporal framing of debt. The perspectival diversity (snare for recipients, rope for benefactors, scaffold for organized collectives) confirms that the asymmetry's effects are position-dependent, not universal. The mandatrophy is resolved by recognizing that 'is this a mountain?' naturalizes the question — the presheaf over observation sites IS the answer, and the mountain perspective is one reading among several, not the privileged truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_decay_rate,
    'Does felt obligation decay over time, or does it compound? What is the half-life of unpaid social debt?',
    'Longitudinal ethnographic study tracking obligation intensity over decades; comparison of first-generation vs second-generation immigrant obligation narratives; experimental priming studies measuring obligation salience at different temporal distances from the original gift.',
    'If obligation decays: the asymmetry is temporary coordination mechanism (Rope from more perspectives). If obligation compounds: the asymmetry is extraction mechanism (Snare from more perspectives). If decay rate varies by cultural context: the constraint is not a universal mountain but a contingent institutional arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_decay_rate, empirical, 'Whether felt obligation decays or compounds over time').

omega_variable(
    pass_forward_vs_settle_framing,
    'Do recipients primarily frame their obligation as ''settling with the benefactor'' or ''passing forward to others''? Does the framing affect extraction intensity?',
    'Cross-cultural survey of obligation narratives; comparison of societies with strong ''pay it forward'' norms vs dyadic debt norms; measurement of status hierarchy steepness in each cultural context.',
    'If ''pass forward'' framing dominates: obligation diffuses to the commons, reducing extraction (Scaffold perspective confirmed). If ''settle with benefactor'' framing dominates: obligation remains dyadic and hierarchical, increasing extraction (Snare perspective confirmed). Framing may be culturally contingent rather than universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pass_forward_vs_settle_framing, empirical, 'Whether obligation is framed as dyadic debt or diffuse reciprocity').

omega_variable(
    identity_lock_mechanism,
    'Is the identity-lock for chronic recipients structural (internalized narrative) or performative (strategic claim to avoid exit costs)?',
    'Comparison of private vs public obligation narratives; measurement of obligation intensity in anonymous vs identified contexts; tracking of obligation claims before and after exit becomes materially feasible.',
    'If structural: identity_locked classification is accurate — the agent genuinely cannot exit without identity rupture. If performative: the agent is constrained (high exit cost) rather than identity_locked, and the classification should shift. This affects the directionality computation and the perspectival gap interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether identity-lock is internalized or strategic').

omega_variable(
    coordination_floor_calibration,
    'What proportion of the measured extractiveness (0.42) is inherent coordination cost vs extractive overhead?',
    'Comparison of obligation asymmetry in formalized mutual aid systems (time banks, rotating credit) vs informal gift networks; measurement of status hierarchy steepness in each; identification of minimum asymmetry required to sustain participation.',
    'If coordination floor is high (>0.30): much of the measured extraction is necessary cost, and the constraint is closer to Rope. If coordination floor is low (<0.15): most extraction is overhead, and the constraint is closer to Snare. This affects Boltzmann compliance and the tangled_rope vs snare boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_floor_calibration, empirical, 'Proportion of extractiveness that is coordination cost vs overhead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_asymmetry_of_obligation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, temporal_asymmetry_of_obligation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_mid, temporal_asymmetry_of_obligation, theater_ratio, 15, 0.3).
narrative_ontology:measurement(theater_final, temporal_asymmetry_of_obligation, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(extract_initial, temporal_asymmetry_of_obligation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(extract_mid, temporal_asymmetry_of_obligation, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(extract_final, temporal_asymmetry_of_obligation, base_extractiveness, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_asymmetry_of_obligation, attachment_coordination).
narrative_ontology:boltzmann_floor_override(temporal_asymmetry_of_obligation, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is a single structural phenomenon (temporal asymmetry of obligation) rather than a decomposed family. If future analysis reveals that 'obligation asymmetry' conflates multiple structurally distinct claims with different epsilon values (e.g., material debt vs emotional debt, dyadic vs diffuse obligation), decomposition into separate stories would be warranted per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
