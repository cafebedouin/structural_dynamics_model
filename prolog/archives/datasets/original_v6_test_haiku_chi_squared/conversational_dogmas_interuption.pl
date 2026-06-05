% ============================================================================
% CONSTRAINT STORY: conversational_dogmas_interuption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conversational_dogmas_interuption, []).

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
 *   constraint_id: conversational_dogmas_interuption
 *   human_readable: Conversational Dogmas: Interruption vs. Strong Civility Enforcement
 *   domain: social/technological/communication_norms
 *
 * SUMMARY:
 *   Conversational dogmas around interruption and civility enforcement
 *   represent a hybrid coordination-extraction mechanism that has intensified
 *   over the past two decades. The constraint emerges from genuine
 *   coordination problems (preventing crosstalk in synchronous communication)
 *   but has accumulated extractive layers: it now suppresses urgent speakers,
 *   disproportionately targets marginal voices, and serves as a tool for
 *   selective enforcement of status hierarchies. The rising theater_ratio
 *   (0.35→0.58) reflects increased performative enforcement: organizations
 *   maintain strict civility codes while high-status speakers routinely
 *   violate them with minimal consequence. The constraint benefits
 *   institutional rule-setters and high-status speakers (who can interrupt
 *   selectively) while imposing costs on urgent speakers, neurodiverse
 *   communicators, and marginalized voices. The perspectives range from pure
 *   extraction (silenced urgent speakers, marginal voices trapped by
 *   double-bind: speak with urgency and be labeled uncivil; speak with
 *   civility and be unheard) through mixed coordination-extraction (moderate
 *   peers, high-status beneficiaries) to pure coordination (rule-setters) to
 *   degraded ritual (vestigial etiquette). The analytical risk is
 *   naturalizing the constraint as a law of conversation, when it is actually
 *   a choice of turn-allocation mechanism that advantages those with
 *   pre-existing floor access.
 *
 * KEY AGENTS:
 *   - Silenced Urgent Speakers: Primary victim (powerless/trapped) — subject to enforcement in employment/social contexts, cannot exit, face penalties for speech urgency
 *   - Marginalized Voices: Primary victim (powerless/trapped) — subordinated groups face double-bind: pre-existing speaking barriers plus civility enforcement penalties
 *   - Rapid-Thought Processors (Neurodiverse): Primary victim (powerless/trapped) — turn-taking enforcement targets neurologically typical expression patterns, creating discrimination
 *   - Collaborative Peers: Secondary actor (moderate/constrained) — experience mixed coordination (structured turn-taking helps info integration) and extraction (protected speaking time)
 *   - High-Status Norm Beneficiaries: Beneficiary + powerful/mobile — experience asymmetric enforcement (their interruptions contextual, lower-status interruptions violations)
 *   - Platform Rule Setters/Moderators: Beneficiary + institutional/arbitrage — design enforcement mechanisms, experience as pure coordination
 *   - Anti-Harassment Coalition: Organized actor (organized/constrained) — enforced civility as temporary scaffold against harassment, but mechanism reproduces silencing
 *   - Analytical Observer: Risks naturalizing contingent turn-allocation choice as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conversational_dogmas_interuption, 0.52).
domain_priors:suppression_score(conversational_dogmas_interuption, 0.65).
domain_priors:theater_ratio(conversational_dogmas_interuption, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conversational_dogmas_interuption, extractiveness, 0.52).
narrative_ontology:constraint_metric(conversational_dogmas_interuption, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(conversational_dogmas_interuption, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conversational_dogmas_interuption, tangled_rope).
narrative_ontology:human_readable(conversational_dogmas_interuption, "Conversational Dogmas: Interruption vs. Strong Civility Enforcement").
narrative_ontology:topic_domain(conversational_dogmas_interuption, "social/technological/communication_norms").

domain_priors:requires_active_enforcement(conversational_dogmas_interuption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conversational_dogmas_interuption, institutional_rule_setters).
narrative_ontology:constraint_beneficiary(conversational_dogmas_interuption, high_status_voices).
narrative_ontology:constraint_victim(conversational_dogmas_interuption, marginal_speakers).
narrative_ontology:constraint_victim(conversational_dogmas_interuption, rapid_thought_processors).
narrative_ontology:constraint_victim(conversational_dogmas_interuption, urgent_information_sharers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SILENCED URGENT SPEAKER (SNARE) — Subject to 'don't interrupt' enforcement in synchronous communication systems (meetings, video calls, live chat). Cannot exit: participation is employment/social obligation. Cannot speak with urgency without violation penalty. Trapped in a system where speech rhythm enforcement is asymmetric. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.66.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED VOICE (SNARE) — Subordinated groups (women in male-dominated fields, junior staff, minorities) are enforced to 'wait their turn' even when floor-taking barriers already suppress their participation. Civility enforcement becomes a second-order exclusion: after barriers to speaking, add penalties for speech urgency/intensity. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.70.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: COLLABORATIVE PEER (TANGLED ROPE) — Moderate-power speaker who values both input-gathering (coordination benefit) and maintains status through turn-compliance (extraction benefit). Constrained exit: violating speech norms carries social cost. Experiences constraint as mixed: coordination (hearing others' full thoughts) + extraction (protected speaking time and status). d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM RULE SETTER (ROPE) — Moderators, meeting facilitators, social platform engineers design and enforce 'no interruption' / 'civility' rules. Experience constraint as pure coordination: preventing crosstalk, enabling structured turn-taking. Arbitrage exit: can design alternative protocols. Rules benefit moderators by making their role clear and unambiguous. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HIGH-STATUS NORM BENEFICIARY (TANGLED ROPE) — Established speakers (tenured academics, senior executives, recognizable public figures) experience civility enforcement as coordination (structured turn-taking benefits them more than others) PLUS extraction (asymmetric protection: their interruptions are contextual; lower-status interruptions are norm violations). Mobile exit: can speak in exclusive settings without enforcement. d≈0.38, f(d)≈0.35, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANTI-HARASSMENT COALITION (SCAFFOLD) — Organized movements (workplace harassment prevention, online safety initiatives) enforced civility rules as temporary scaffolding to protect vulnerable speakers. See constraint as having a sunset: as workplace harassment norms mature and inclusive speaking cultures develop, strict civility enforcement becomes less necessary. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.31. Sunset clause: as inclusive cultures mature, need for rule enforcement declines.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: VESTIGIAL ETIQUETTE SYSTEM (PITON) — Formal 'don't interrupt' rules are theatrical enforcement of Victorian-era turn-taking etiquette. Original function was to enforce class hierarchy; modern instantiation claims to enforce inclusivity but reproduces the same silencing patterns. theater_ratio=0.58: many organizations maintain formal civility codes despite widespread norm violation. Rules are performative theater — people interrupt constantly in high-stress contexts, but rule violation creates deniability for selective enforcement. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk perspective: conversational synchronization is a natural law — human communication systems inherently require turn-taking mechanics, and interruption creates information loss. All speech systems must solve turn allocation. However: this perspective naturalizes what is actually a CHOICE of turn-allocation mechanism. Alternative mechanisms (parallel speech channels, asynchronous-first, rapid-response threads) exist. The 'natural law' framing hides the extraction embedded in synchronous turn-taking rules.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conversational_dogmas_interuption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conversational_dogmas_interuption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conversational_dogmas_interuption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conversational_dogmas_interuption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(conversational_dogmas_interuption, TR),
    TR >= 0.70.

:- end_tests(conversational_dogmas_interuption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint does solve genuine coordination problems (crosstalk reduction, structured information flow) but the base extraction reflects significant asymmetric cost: marginalized speakers face double penalties (lower pre-existing floor access + civility enforcement), urgent contexts are constrained by rules that reduce decision quality, and neurodiverse speakers bear biological targeting costs. Rising trajectory (0.28→0.52 over 20 years) shows accumulation as enforcement infrastructure (HR codes, content moderation, meeting facilitator training) has expanded. Suppression (0.65): High. Barriers to deviation include employment rules, social group enforcement, platform enforcement, HR complaint mechanisms. Alternatives are suppressed: rapid-response channels are labeled 'unprofessional,' asynchronous options are treated as optional, and modified turn-taking protocols face resistance. Theater ratio (0.58): Moderate-high and rising. Organizations maintain formal civility codes (performative) while enforcement is highly selective by status and context. High-stress situations show constant norm violation; low-stakes settings show strict compliance theater. Rising ratio indicates increasing gap between stated rule and actual practice — marker of piton degradation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits all six classification types, revealing deep structural conflict. Silenced urgent speakers and marginal voices see pure extraction (Snare) — rules trap them while benefiting others. Collaborative peers see mixed coordination-extraction (Tangled Rope) — genuine benefit from structured turn-taking but also extraction of protected speaking time. High-status speakers see mild extraction wrapped in coordination (Tangled Rope) — asymmetric enforcement that protects them. Rule-setters see pure coordination (Rope) — they are solving turn-allocation problem. Etiquette system sees itself as degraded (Piton) — theater-based, violating constantly in practice. Analytical observer risks seeing natural law (Mountain) — 'conversation requires turn-taking' naturalizes a choice. The perspectival gap reveals that 'civility enforcement' is not a single constraint but a mechanism that *distributes* benefits asymmetrically while claiming universal coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Silenced urgent speakers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit, enforcement is mandatory, costs are individual and severe. Marginal voices: Victim + trapped → d≈0.95, f(d)≈1.42. Super-maximum extraction — pre-existing barriers plus enforcement creates compound silencing. Neurodiverse speakers: Victim + trapped (biological targeting) → d≈0.90, f(d)≈1.35. High-cost extraction targeting neurological variance. Collaborative peers: Mixed (moderate coordination benefit + extraction of protected time) + constrained → d≈0.58, f(d)≈0.72. Moderate effective extraction. High-status speakers: Beneficiary + mobile (can speak in exclusive settings) → d≈0.38, f(d)≈0.35. Low effective extraction due to mobile exit, but still extraction benefit. Rule-setters: Beneficiary + arbitrage (can redesign rules) → d≈0.08, f(d)≈-0.10. Net beneficiary. Anti-harassment coalition: Organized + constrained (building sunset pathway) → d≈0.45, f(d)≈0.50. Low-to-moderate extraction as they work toward alternative protocols. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit risk.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the coordination-extraction ambiguity by showing that civility enforcement IS a hybrid mechanism where the coordination function (preventing crosstalk) is REAL and NECESSARY but the extraction mechanism (asymmetric enforcement, status protection, marginalization of urgent/rapid speakers) is SEPARABLE from it. The mandatrophy is resolved by recognizing that alternative turn-allocation mechanisms (asynchronous-first, parallel channels, rapid-response threads) can achieve equivalent coordination without the extraction. The constraint is NOT 'is this coordination or extraction?' but rather 'which mechanism achieves coordination with minimum extraction?' The forced choice between 'no interruption rules' (current: high extraction) and 'free interruption' (risks crosstalk) is false. The real design space includes: (1) asynchronous-first with structured turn-taking in synchronous only, (2) parallel-channel communication for urgent information, (3) explicit status-blind turn allocation algorithms, (4) neurodiverse-inclusive protocols, (5) context-dependent rules (emergency protocols get different rules than planning meetings). The rising theater_ratio and accumulating extractiveness suggest organizations are maintaining the high-extraction mechanism despite better alternatives being available, indicating institutional capture by status-preserving norms — moving the classification toward Piton (degraded function) in institutional contexts. Anti-harassment movements are correct that some coordination mechanism is needed, but their choice of 'strict civility enforcement' reproduces the same status-based silencing they claim to oppose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interruption_information_value,
    'Does rapid interruption in high-stakes contexts (emergency medical teams, crisis response) actually reduce information flow or does it improve decision speed and save lives?',
    'Comparative analysis of communication patterns in high-stakes teams: measure information integration speed and outcome quality under strict turn-taking vs. free interruption protocols. Analyze emergency medicine, air traffic control, military command research.',
    'If interruption improves outcomes: ''don''t interrupt'' rules are extractive in contexts where they constrain optimal communication. If turn-taking preserves information quality: interruption is a genuine coordination problem. Changes the classification from Snare (for urgent-speakers) to Rope (for genuinely coordinated teams).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interruption_information_value, empirical, 'Whether rapid interruption improves or degrades information flow in high-stakes contexts').

omega_variable(
    civility_enforcement_asymmetry,
    'Is civility rule enforcement actually symmetric — do high-status and low-status speakers face the same enforcement probability for identical speech acts?',
    'Controlled study: record real conversations, extract identical speech acts (same interruption type, same emotional intensity), code speaker status, measure enforcement likelihood. Analyze HR complaint patterns and moderation logs for status-correlated enforcement.',
    'If asymmetric: confirms extraction mechanism (Snare for marginal speakers). If symmetric: suggests rules genuinely coordinate (Rope). Currently high theater_ratio suggests asymmetric enforcement is invisible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civility_enforcement_asymmetry, empirical, 'Whether civility enforcement applies equally to speakers of different status').

omega_variable(
    alternative_coordination_sufficiency,
    'Can asynchronous-first communication (threaded replies, documented decisions, async standup) achieve equivalent information integration without synchronous turn-taking constraints?',
    'Comparative study of team communication outcomes: synchronous-enforced teams vs. async-first teams. Measure: information diversity captured, decision speed, participant satisfaction, voice equity across status levels.',
    'If async achieves equivalent outcomes with higher voice equity: synchronous civility enforcement is constraint-redundant and pure extraction (upgrade all perspectives to Snare). If async degrades coordination: synchronous turn-taking solves a real problem (maintain Tangled Rope hybrid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether asynchronous communication achieves equivalent coordination without turn-taking enforcement').

omega_variable(
    rapid_thought_expression_neurodiversity,
    'Do neurodiverse thinkers (ADHD, autism spectrum, bipolar rapid-cycling, Tourette''s) experience ''no interruption'' rules as equally fair constraints or as targeted suppression of neurologically typical expression patterns?',
    'Qualitative analysis of neurodiverse speakers'' experience; accommodation requests in workplace settings; comparative linguistic analysis of speech intensity in ADHD vs. neurotypical speakers. Identify whether turn-taking rules disproportionately penalize rapid-cycling thought patterns.',
    'If neurodiverse speakers bear disproportionate cost: rule targets biological/neurological variance — becomes explicit discrimination. Strengthens Snare classification for affected populations. If accommodation is feasible: maintains Tangled Rope (coordination + targeted extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rapid_thought_expression_neurodiversity, empirical, 'Whether turn-taking enforcement disproportionately suppresses neurodiverse speakers').

omega_variable(
    historical_civility_doctrine_origin,
    'Did formal ''civility'' and ''no interruption'' rules historically emerge as genuine responses to communication failure or as mechanisms for status preservation and class hierarchy enforcement?',
    'Historical analysis: trace civility doctrine origins (Victorian etiquette, parliamentary procedure, academic meeting conventions). Identify original stated purpose vs. observed effect on speech participation by class/gender/race.',
    'If origin is genuine coordination: maintains Tangled Rope for historical context. If origin is status enforcement: reveals rules as pre-existing extraction mechanism being relabeled as inclusivity. Strengthens Piton perspective (degraded function, theatrical maintenance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_civility_doctrine_origin, conceptual, 'Historical origin of civility doctrine: coordination vs. status preservation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conversational_dogmas_interuption, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(convdogma_tr_t0, conversational_dogmas_interuption, theater_ratio, 0, 0.35).
narrative_ontology:measurement(convdogma_tr_t10, conversational_dogmas_interuption, theater_ratio, 10, 0.48).
narrative_ontology:measurement(convdogma_tr_t20, conversational_dogmas_interuption, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(convdogma_be_t0, conversational_dogmas_interuption, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(convdogma_be_t10, conversational_dogmas_interuption, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(convdogma_be_t20, conversational_dogmas_interuption, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conversational_dogmas_interuption, enforcement_mechanism).
narrative_ontology:affects_constraint(conversational_dogmas_interuption, marginalized_voice_suppression).
narrative_ontology:affects_constraint(conversational_dogmas_interuption, workplace_status_hierarchy).
narrative_ontology:affects_constraint(conversational_dogmas_interuption, meeting_participation_equity).
narrative_ontology:affects_constraint(conversational_dogmas_interuption, neurodiverse_exclusion_infrastructure).

% DUAL FORMULATION NOTE:
% Conversational dogmas decompose into multiple structurally distinct constraints: (1) Turn-allocation coordination problem (genuine need, low ε if solved optimally), (2) Status-preservation extraction mechanism (high ε, embedded in civility enforcement rules), (3) Neurodiverse targeting (high ε, masks as 'communication norms'). This story treats the hybrid as one constraint; the network links show how each decomposition affects downstream institutional practices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(conversational_dogmas_interuption, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
