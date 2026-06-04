% ============================================================================
% CONSTRAINT STORY: terror_coincidence__plebiscitary_theater_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_terror_coincidence_plebiscitary_theater_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: terror_coincidence__plebiscitary_theater_reading
 *   human_readable: Plebiscitary Theater: Participation as Spectacle in Terror-Era Constitutional Process
 *   domain: legal/political/doctrinal
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   terror_coincidence: specifically, the PLEBISCITARY_THEATER_READING. The
 *   constraint models the Soviet constitutional process (1936) as a
 *   systematized apparatus for harvesting participation as endorsement while
 *   suppressing genuine structural deliberation. Millions of meetings
 *   convened across the USSR, thousands of amendments reported through
 *   official channels, yet none would alter the regime's core powers. The
 *   participation machinery's extractiveness lies in its conversion of
 *   involvement into legitimation spectacle: the regime claims authorship of
 *   a 'democratic' constitution while the actual constitutional text is
 *   predetermined. The apparatus suppresses structural amendments through
 *   multiple mechanisms: screening by local officials, reporting only
 *   non-threatening proposals, and the systematic administrative capacity to
 *   absorb input without permitting feedback into the final text. Theater
 *   ratio is extremely high (0.91) because the entire process is
 *   performative: the appearance of mass deliberation is the product, not a
 *   side effect. The constraint is Snare from the powerless participant's
 *   perspective (trapped, bearing full cost while the regime extracts
 *   legitimacy) and from the perspective of genuine deliberation itself
 *   (which is systematically suppressed). It is Rope from the regime's
 *   perspective (solving a genuine coordination problem: how to govern
 *   revolutionary transformation while maintaining the appearance of
 *   democratic legitimation). The Analytical Observer sees Tangled Rope: real
 *   coordination function (mobilizing the population) wrapped around
 *   extractive function (harvesting legitimacy without permitting
 *   constraint). This reading coexists with two sibling readings: the
 *   LEGITIMATION_DURING_PURGE_READING (the constitution legitimated the
 *   Terror's prerogatives while the troikas sentenced by quota) and the
 *   SINCERE_BLUEPRINT_READING (some drafters meant structural promises that
 *   the regime later betrayed). The three readings occupy different
 *   institutional positions and temporal vantage points.
 *
 * KEY AGENTS:
 *   - Amendment Submitters (powerless/trapped): Citizens mobilized into participation meetings; input harvested as endorsement regardless of content; no exit
 *   - Local Party Officials (moderate/constrained): Organize and report participation metrics; pressured by reporting quotas; recognize structural amendments are impossible but cannot exit without career damage
 *   - Genuine Deliberation (powerless/trapped): The epistemic commons; systematic suppression of structural amendments; no institutional voice
 *   - Central Authority / Regime Apparatus (institutional/arbitrage): Primary beneficiary; solves legitimation problem through managed spectacle; harvests participation as authority
 *   - The Constitutional Text (institutional/arbitrage): Over time becomes inert ritual; maintained through institutional inertia; the discussion process forgotten
 *   - Analytical Observer (analytical/analytical): Civilizational perspective capturing both coordination and extraction functions simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(terror_coincidence__plebiscitary_theater_reading, 0.68).
domain_priors:suppression_score(terror_coincidence__plebiscitary_theater_reading, 0.82).
domain_priors:theater_ratio(terror_coincidence__plebiscitary_theater_reading, 0.91).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(terror_coincidence__plebiscitary_theater_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(terror_coincidence__plebiscitary_theater_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(terror_coincidence__plebiscitary_theater_reading, theater_ratio, 0.91).

% --- Constraint claim ---
narrative_ontology:constraint_claim(terror_coincidence__plebiscitary_theater_reading, snare).
narrative_ontology:human_readable(terror_coincidence__plebiscitary_theater_reading, "Plebiscitary Theater: Participation as Spectacle in Terror-Era Constitutional Process").
narrative_ontology:topic_domain(terror_coincidence__plebiscitary_theater_reading, "legal/political/doctrinal").

domain_priors:requires_active_enforcement(terror_coincidence__plebiscitary_theater_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(terror_coincidence__plebiscitary_theater_reading, 'b7559db5-e59c-4c5b-a83b-0af4e7eb11ab').
narrative_ontology:cs_kernel_codification('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', fixed_text).
narrative_ontology:cs_authority_grounding('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', extraction).
narrative_ontology:cs_interpretation_layer_present('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab').
narrative_ontology:cs_reading_relation('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', terror_coincidence__legitimation_during_purge_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', terror_coincidence__sincere_blueprint_reading, influences).
narrative_ontology:cs_axiom('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', foundational, participation_spectacle_excludes_structural_input).
narrative_ontology:cs_axiom_status(participation_spectacle_excludes_structural_input, holdable).
narrative_ontology:cs_axiom_grounding('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', participation_spectacle_excludes_structural_input, empirically_contingent).
narrative_ontology:cs_axiom('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', foundational, legitimacy_harvested_without_constraint).
narrative_ontology:cs_axiom_status(legitimacy_harvested_without_constraint, holdable).
narrative_ontology:cs_axiom_grounding('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', legitimacy_harvested_without_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', vanguard_democratic_legitimacy).
narrative_ontology:cs_drift_state('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', contemporary_historical_examination, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b7559db5-e59c-4c5b-a83b-0af4e7eb11ab', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(terror_coincidence__plebiscitary_theater_reading, terror_coincidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(terror_coincidence__plebiscitary_theater_reading, regime_apparatus).
narrative_ontology:constraint_beneficiary(terror_coincidence__plebiscitary_theater_reading, central_authority).
narrative_ontology:constraint_victim(terror_coincidence__plebiscitary_theater_reading, genuine_deliberation).
narrative_ontology:constraint_victim(terror_coincidence__plebiscitary_theater_reading, constituent_voice).
narrative_ontology:constraint_victim(terror_coincidence__plebiscitary_theater_reading, amendment_submitters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE AMENDMENT SUBMITTER (SNARE) — Participates in millions of meetings believing input will shape the constitution. Trapped by the expectation of participation and the social pressure to contribute. No genuine exit: refusal to participate signals disloyalty. Input is harvested as endorsement regardless of content — the regime's extraction mechanism converts participation into legitimation spectacle. Maximum experienced extraction because the submitter bears the cost of mobilization while the regime captures all value.
constraint_indexing:constraint_classification(terror_coincidence__plebiscitary_theater_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE LOCAL PARTY OFFICIAL (SNARE) — Must organize and report participation metrics; constrained by reporting quotas and pressure to demonstrate mobilization. Receives recognition for high participation numbers, but the performance metrics themselves become the extraction target. The official reports thousands of amendments, knowing structural change is impossible, but cannot exit without career consequences. Extraction: their administrative labor and credibility are harvested for legitimation theater.
constraint_indexing:constraint_classification(terror_coincidence__plebiscitary_theater_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GENUINE DELIBERATION / THE EPISTEMIC COMMONS (SNARE) — The constraint's primary victim. Structural amendments that would alter the regime's character are systematically suppressed. The space for genuine deliberation — for input that could actually reshape the constitutional order — is closed. The millions of meetings and thousands of amendments create the illusion of an open deliberative process while the actual constitutional text (preserving the Terror's prerogatives) is predetermined. No exit from the systematic suppression.
constraint_indexing:constraint_classification(terror_coincidence__plebiscitary_theater_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: THE REGIME APPARATUS (ROPE) — Experiences the plebiscitary theater as a coordination mechanism: mobilizing millions of meetings for participation ensures buy-in for a predetermined constitution. The regime solves the legitimation problem (how to govern without consent while claiming democratic authority) through managed spectacle. Net beneficiary: participation is harvested as endorsement; the appearance of authorship accrues to the regime. The regime's effective extraction is high, but from its own perspective, it is solving a genuine coordination problem — how to appear democratic while maintaining total control.
constraint_indexing:constraint_classification(terror_coincidence__plebiscitary_theater_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CONSTITUTIONAL TEXT (PITON) — Over time, the text becomes inert ritual. The discussion process that surrounded its adoption is glossed over; the millions of meetings are forgotten; the thousands of amendments are archived unread. The constitution persists as a performative object — invoked to legitimize regime action but carrying no real constraint on power. Theater ratio is nearly total: the document itself becomes a stage prop maintained through institutional inertia.
constraint_indexing:constraint_classification(terror_coincidence__plebiscitary_theater_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination and pure extraction. The regime does solve a real problem (legitimating authority in a revolutionary state) through managed participation. But the coordination mechanism is fundamentally extractive: it harvests the legitimacy-generating potential of participation without permitting any genuine input. The analytical view captures both functions: coordination disguised as extraction, or extraction disguised as coordination — the same mechanism, read from different institutional positions.
constraint_indexing:constraint_classification(terror_coincidence__plebiscitary_theater_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(terror_coincidence__plebiscitary_theater_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(terror_coincidence__plebiscitary_theater_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(terror_coincidence__plebiscitary_theater_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(terror_coincidence__plebiscitary_theater_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(terror_coincidence__plebiscitary_theater_reading, TR),
    TR >= 0.70.

:- end_tests(terror_coincidence__plebiscitary_theater_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The regime captures legitimacy-generating value from participation without permitting genuine structural input. Citizens invest effort (attending meetings, formulating amendments, reporting through channels) expecting influence; the regime appropriates this legitimacy while delivering no substantive constraint. The extracted value accrues to the regime's claim to democratic authority. Theater ratio (0.91): Extremely high. The entire apparatus is performative. Millions of meetings staged to generate the appearance of mass deliberation. Thousands of amendments reported to create the record of 'public participation.' Official guidance to local officials emphasizes participation numbers over amendment substance. The appearance of authorship ('the most democratic constitution in the world') is the product. Suppression (0.82): Very high. Structural amendments that would limit the regime's prerogatives (on the secret police, the party's monopoly, emergency powers) are systematically filtered. Selection bias in reporting means dangerous proposals never reach official record. Administrative capacity to absorb input without implementing feedback ensures that visible suppression is minimal — the suppression is structural, not visible. The constraint's extractiveness increased over the interval (0.55 → 0.68) as the regime learned to manage the machinery more effectively, concentrating on metrics (number of meetings, amendments reported) rather than on the substance of participation. Theater ratio increased (0.75 → 0.91) as the performative character became more refined and more thoroughly separated from any deliberative function.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reveals the constraint's extractive structure. The amendment submitter experiences maximal extraction (trapped, powerless): their input vanishes into the administrative machinery. The regime apparatus experiences coordination (institutional, arbitrage): the machinery solves the legitimation problem perfectly. Genuine deliberation experiences total suppression (powerless, trapped): there is no deliberative function to coordinate around. The analytical observer sees both functions simultaneously: coordination disguised as extraction, extraction disguised as coordination. The Piton perspective (the constitutional text over time) reveals how the apparatus creates institutional memory loss — the theater is so total that later generations learn the constitution as a formal document, not as the spectacle that produced it. The constrast between the Snare (participant's view) and the Rope (regime's view) is the diagnostic marker: when the same mechanism appears as extraction from one position and coordination from another, the gap reveals that the 'coordination' is actually extraction with institutional consent.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's structural position determines its experienced extractiveness (chi). Amendment submitters: beneficiary status = no (they are targets), exit options = trapped, power = powerless → directionality d ≈ 0.95, f(d) ≈ 1.42 (maximum). The regime apparatus: beneficiary status = yes, exit options = arbitrage, power = institutional → directionality d ≈ 0.05, f(d) ≈ -0.12 (institutional beneficiary receives institutional benefit, experienced as negative extraction). Local officials: beneficiary status = ambiguous (they receive recognition for metrics, but are constrained by quotas they know are fabricated), exit options = constrained, power = moderate → directionality d ≈ 0.60, f(d) ≈ 0.90 (moderate extraction). Genuine deliberation: beneficiary status = no (it is suppressed), exit options = trapped (systematic suppression), power = powerless → directionality d ≈ 0.97, f(d) ≈ 1.42 (maximum). Spatial scope is national for most perspectives, amplifying χ slightly via σ(S) = 1.0. The regime's perspective shows the lowest experienced extraction because the mechanism is working as designed from their position — they are solving a problem, not being extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by rejecting the false dichotomy: the constraint is neither 'genuine coordination' nor 'pure extraction' — it is pure extraction disguised as coordination. The regime's perspective reports experiencing it as coordination (solving the legitimation problem), but this is the extractive apparatus's own self-description, not an independent verification. The analytical observer's Tangled Rope classification captures both the coordination and extraction functions, but the distribution (theater ratio 0.91, extractiveness 0.68) shows that extraction dominates coordination. The mandatrophy is resolved by centering the victim's perspective (amendment submitter, genuine deliberation): from their view, the constraint is unambiguously Snare. The regime's Rope classification is not an alternative reality but a description of how extraction appears from the beneficiary's position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sincerity_of_drafter_intent,
    'Did the constitutional drafters (Bukharin, other core group) intend any genuine structural amendments from public participation, or was the entire plebiscitary process designed as theater from inception?',
    'Archival analysis of drafting records, internal memos, selection criteria for reported amendments, comparison of submitted amendments to final text; analysis of which officials praised high participation vs. structural incorporation',
    'If sincere intent existed at draft stage: constraint may be Tangled Rope from drafters'' perspective (genuine coordination corrupted by terror politics). If designed as theater: confirms Snare classification across all perspectives. This omega encodes the struggle between sincere_blueprint_reading and plebiscitary_theater_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_of_drafter_intent, empirical, 'Whether drafters intended genuine structural incorporation from public participation').

omega_variable(
    participation_suppression_mechanism,
    'What is the primary mechanism suppressing structural amendments: explicit censorship of dangerous proposals, selection bias in reporting (filtering out structural amendments), or systematic reframing (coding structural changes as technical clarifications)?',
    'Comparison of archived amendment proposals to officially reported amendments; analysis of amendment categories and frequency; examination of official guidance to local officials on which amendments to prioritize in reporting',
    'If explicit censorship: suppression metric accurate at 0.82. If selection bias: actual suppression may be higher (0.90+) because genuine proposals never reach the record. If reframing: extractiveness increases because structural intent is appropriated and repackaged. Affects interpretation of how institutional memory falsifies the process.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(participation_suppression_mechanism, empirical, 'Mechanism of structural amendment suppression in reported process').

omega_variable(
    institutional_memory_decay,
    'Does the constitutional text''s Piton character emerge naturally from institutional inertia, or is the forgetting of the plebiscitary process deliberately maintained (e.g., through archives kept closed, oral history suppressed)?',
    'Longitudinal analysis of how the constitution is cited and taught; examination of access to archives and official historical narratives; study of punishment of historians or officials who publicize the theater aspect',
    'If natural decay: Piton classification is stable. If deliberately maintained: the constraint is not degraded but actively enforced oblivion — the Snare''s extraction mechanism includes preventing accurate historical memory. Theater_ratio would increase further if the suppression of memory is itself an active extraction process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_memory_decay, empirical, 'Whether constitutional Piton status reflects natural decay or deliberate memory suppression').

omega_variable(
    regime_learning_from_theater,
    'Did the regime''s participation machinery generate any actionable intelligence for governance, or was the entire apparatus purely legitimation theater with zero feedback into actual policy?',
    'Analysis of regime documents to trace whether amendments or participation patterns influenced subsequent policy; comparison of official rationales for policy choices to amendments submitted; examination of whether regime officials claimed to use participation data',
    'If zero feedback: pure extraction (Snare confirmed). If minimal feedback used selectively: extraction mechanism includes co-optation of participation for selective regime benefit. If substantial feedback: coordination function was genuinely present, constraining classification toward Tangled Rope. This omega encodes the distinction between theater and degraded-but-functional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_learning_from_theater, empirical, 'Whether regime extracted actionable intelligence from participation machinery').

omega_variable(
    reading_contest_foreclosure,
    'Does the plebiscitary_theater_reading logically foreclose the sincere_blueprint_reading, or can both readings coexist as different frameworks held by different parties?',
    'Examination of whether sincere drafters'' intent (axiom of the blueprint reading) is logically incompatible with systematic participation theater (axiom of this reading). If sincere intent can coexist with theater (drafters meant well but were overruled by terror apparatus), readings coexist. If theater was always the design, sincere intent is empirically false but not logically impossible.',
    'If readings foreclose each other: one reading wins and displaces the other in institutional memory. If readings coexist: both remain live — different parties adopt different narratives. This determines whether the kernel contest is resolvable or permanent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Logical relationship between plebiscitary theater and sincere blueprint readings of the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(terror_coincidence__plebiscitary_theater_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_plebi_theater_t0, terror_coincidence__plebiscitary_theater_reading, theater_ratio, 0, 0.75).
narrative_ontology:measurement(terr_plebi_theater_t3, terror_coincidence__plebiscitary_theater_reading, theater_ratio, 3, 0.88).
narrative_ontology:measurement(terr_plebi_theater_t6, terror_coincidence__plebiscitary_theater_reading, theater_ratio, 6, 0.91).

% Extraction over time
narrative_ontology:measurement(terr_plebi_extract_t0, terror_coincidence__plebiscitary_theater_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(terr_plebi_extract_t3, terror_coincidence__plebiscitary_theater_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(terr_plebi_extract_t6, terror_coincidence__plebiscitary_theater_reading, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_plebi_suppress_t0, terror_coincidence__plebiscitary_theater_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(terr_plebi_suppress_t3, terror_coincidence__plebiscitary_theater_reading, suppression_requirement, 3, 0.78).
narrative_ontology:measurement(terr_plebi_suppress_t6, terror_coincidence__plebiscitary_theater_reading, suppression_requirement, 6, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(terror_coincidence__plebiscitary_theater_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(terror_coincidence__plebiscitary_theater_reading, 0.25).
narrative_ontology:affects_constraint(terror_coincidence__plebiscitary_theater_reading, terror_coincidence__legitimation_during_purge_reading).
narrative_ontology:affects_constraint(terror_coincidence__plebiscitary_theater_reading, terror_coincidence__sincere_blueprint_reading).

% DUAL FORMULATION NOTE:
% The terror_coincidence kernel has three constraint readings corresponding to three structural readings of the same historical event. Each reading extracts a different constraint by fixing different observables: this reading focuses on the plebiscitary apparatus and participation harvesting; the purge-legitimation reading focuses on the constitution's authorization of Terror prerogatives; the sincere-blueprint reading focuses on the text's deferred promises. Each story has its own extractiveness value reflecting what is being extracted and from whom. All three are linked through the kernel and represent a constraint family, not three unrelated historical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
