% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Legitimacy of Imposed Practice: Endogenous Climb Reading
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint models the imposition of state-mandated practices
 *   (calendar reform, dress codes, language standardization) that displaces
 *   community-embedded practices. The endogenous_climb reading asserts that
 *   genuine practice displacement requires internalization driven from within
 *   communities, not mere legal mandate from above. Imposed compliance
 *   without internalization produces theater: public conformity masks
 *   persistent private practice. The historical record shows this pattern
 *   repeatedly: lunar calendars persisted in private use for decades after
 *   legal displacement by Julian/Gregorian calendars; traditional dress codes
 *   continued in domestic and ritual contexts despite state mandates for
 *   'modern' dress; indigenous languages survived as private practice despite
 *   educational and administrative bans. The reading distinguishes between
 *   surface compliance (externally enforced) and genuine adoption (internally
 *   motivated). State enforcement can extract surface compliance but cannot
 *   mandate internalization. The constraint exhibits Tangled Rope structure:
 *   genuine coordination benefits exist (unified administrative calendars
 *   enable tax collection, military coordination) alongside extraction
 *   (communities lose autonomy over culturally constitutive practices). The
 *   extractiveness grows over time (0.18 → 0.41) as enforcement machinery
 *   matures. Theater ratio rises (0.42 → 0.68) as the gap widens between
 *   formally mandated practice and actually lived experience. Suppression
 *   requirement paradoxically falls (0.75 → 0.62) as communities develop
 *   underground networks for practice preservation — the state must maintain
 *   enforcement but cannot prevent private persistence.
 *
 * KEY AGENTS:
 *   - Communities Preserving Autonomy: Primary victim (powerless/trapped) — forced compliance; bear full cost of internalization pressure; benefit only if new practice generates genuine gains (coordination benefit is marginal for communities). Lunar calendar displacement persists for decades in private observance despite legal penalty.
 *   - Centralizing State: Primary beneficiary (institutional/arbitrage) — captures administrative coordination gains; has exit options (can relax enforcement, phase implementation, negotiate). Experiences constraint as coordination problem, not extraction.
 *   - Regional Elites and Urban Populations: Secondary actor (moderate/constrained) — benefit from standardized practices (market access, bureaucratic roles, cultural prestige among state officials). Also bear costs (pressure to abandon identity practices, tension between public compliance and private retention). Constrained exit — can migrate, negotiate, or maintain dual practices at cost.
 *   - Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains performative compliance mechanisms. Theater ratio indicates enforcement activity becomes increasingly ceremonial as private practice proves impossible to fully suppress. Sees own function as degraded (piton signature).
 *   - Underground Practice Communities: Organized resistance (organized/constrained) — coordinate alternative pathways for practice preservation. Maintain practices in private, religious, diaspora, and family contexts. Have constrained exit (legal penalties) but show agency (networks, transmission strategies, temporary spaces). Represent the scaffold mechanism — building alternative structures that eventually render state mandate obsolete.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent pattern as a law of cultural transmission (mountain reading). The engine's false summit detection flags this as contestable framing, not natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.35).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.62).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Legitimacy of Imposed Practice: Endogenous Climb Reading").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040').
narrative_ontology:cs_kernel_codification('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', distributed).
narrative_ontology:cs_authority_grounding('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', distributed).
narrative_ontology:cs_reading_relation('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', foundational, internalization_necessary_for_displacement).
narrative_ontology:cs_axiom_status(internalization_necessary_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', internalization_necessary_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', secondary, state_decree_insufficient_absent_internalization).
narrative_ontology:cs_axiom_status(state_decree_insufficient_absent_internalization, holdable).
narrative_ontology:cs_axiom_grounding('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', state_decree_insufficient_absent_internalization, empirically_contingent).
narrative_ontology:cs_reference_frame('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', community_autonomy_in_cultural_practice).
narrative_ontology:cs_drift_state('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', contemporary_enforcement_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('12aaf7cb-62ce-4a3d-b7fd-8c1bf6dfb040', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT COMMUNITIES (SNARE) — Powerless agents trapped by legal mandate and enforcement machinery. State decree prohibits lunar observance, mandates new calendar. Suppression is high: violating the mandate carries legal penalties, social stigma, and resource penalties (exclusion from bureaucratic roles, education, markets). The constraint extracts compliance without genuine coordination benefit — the new practice serves state interests (standardization, administrative efficiency) but offers no reciprocal benefit to communities. High d value (near 1.0) from victim status + trapped exit → high chi. Snare classification reflects that suppression (0.62) creates a binding mechanism independent of the constraint's coordination function.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__endogenous_climb_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE STATE (ROPE) — State apparatus perceives the constraint as pure coordination: standardizing calendars, dress codes, and observances solves collective action problems (administrative efficiency, unified tax cycles, military readiness). From the state's view, this is solving a coordination problem across heterogeneous regions. The state has arbitrage options — it can modify the mandate, phase implementation, or negotiate with regional authorities. Beneficiary status + arbitrage exit → low d (near 0.2) → negative chi. State experiences constraint as cooperative arrangement, not extraction. The 'coordination' framing naturalizes what is, from the subject communities' perspective, forced compliance.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__endogenous_climb_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL ELITES & URBAN POPULATIONS (TANGLED ROPE) — Moderate power agents with constrained exit. Urban merchant classes and regional elites benefit from standardized practices (access to state markets, bureaucratic roles, cultural prestige). But they also bear costs: pressure to abandon cultural practices, negotiate between state expectations and community attachments, manage the tension between public compliance and private retention. Their exit is costly but possible — they can migrate, negotiate with state authorities, or maintain dual practices. d ≈ 0.50 (symmetric costs and benefits) → moderate chi. The constraint is both coordination mechanism (genuine benefit from standardization) and extraction mechanism (coerced abandonment of identity practices).
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ENFORCEMENT APPARATUS (PITON) — The institutions charged with enforcing the mandate (police, tax collectors, educational bureaucracy) see their own activity as increasingly performative over time. Formal compliance with calendar and dress codes is ritually asserted and administratively recorded, but actual private practice persists in ways the apparatus cannot fully control or prevent. Theater ratio is high (0.58) — much enforcement effort is ceremonial affirmation of state authority rather than effective suppression of the old practice. The constraint persists through institutional inertia (the apparatus continues enforcement because it is what exists) rather than through genuine functional suppression. Over decades, the apparatus 'becomes' the mandate it enforces; its identity fuses with the policy. This is the piton signature: degraded function, maintained through theatrical commitment.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__endogenous_climb_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: UNDERGROUND PRACTICE COMMUNITIES (SCAFFOLD) — Organized resistance groups (secret societies, religious networks, family circles) coordinate alternative pathways for practice preservation. These communities maintain lunar observance privately, transmit practices across generations in hidden forms, and create temporary spaces (festivals, domestic rituals, diaspora networks) where the old practice functions openly. From this perspective, the state mandate is a temporary scaffold — a pressure that is being circumvented, absorbed, and eventually rendered obsolete as the organizing capacity of communities matures. The underground networks have constrained exit (legal penalties for discovered practice) but show agency and clear exit path (sustained private practice, eventual political liberalization). Sunset logic: as state enforcement weakens or social attitudes shift, the scaffold collapses and private practice becomes public again.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__endogenous_climb_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational timescale, the impossibility of displacing internalized practice without endogenous adoption appears as a natural law of cultural transmission. No imposed mandate can permanently displace a practice that is constitutive of group identity without generating internal drive from within the community to adopt the new practice. The framing suggests an immutable principle: cultural change requires internalization; top-down force alone produces compliance theater, not genuine displacement. However, this 'natural law' reading risks naturalizing what is actually a contingent historical pattern. The reading forecloses the exogenous_override premise — it denies that decree alone can achieve displacement. But the engine's false summit detector will identify this as a contestable framing, not a law of nature.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__endogenous_climb_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__endogenous_climb_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__endogenous_climb_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, TR),
    TR >= 0.70.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The state extracts administrative standardization benefits and symbolic control (demonstrating state authority over cultural practices), but the extraction is incomplete — private practice persists, limiting the state's actual command. The metric reflects that surface compliance is achieved at moderate cost to communities (legal penalties, social stigma, resource barriers) without translating to genuine cultural displacement. The value increases over time (0.18 → 0.41) as enforcement infrastructure matures and the state's capacity to extract visible compliance improves, though underlying practice persists. Suppression (0.62): Moderate-high. Legal prohibition, enforcement penalties, and exclusion from state benefits create substantial barriers to openly practicing displaced customs. Communities face real costs to resistance. However, suppression is not total — underground networks allow practice continuation at acceptable (if costly) levels. Suppression falls slightly over 30 years (0.75 → 0.62) as communities develop coping strategies, the state exhausts enforcement capacity, and social norms gradually shift, making the prohibited practice less culturally salient. Theater ratio (0.58): Moderate-high. State enforcement is partly performative: public ceremonies affirming new calendar despite private lunar observance, state examinations testing knowledge of new dress codes despite domestic retention of old garments, official language use in bureaucracy alongside private linguistic practice. The ratio rises (0.42 → 0.68) over time as the gap between formal mandate and lived reality widens — enforcement machinery must work harder to maintain the appearance of compliance as actual displacement fails. The constraint exhibits classic Tangled Rope structure: real coordination function (unified calendars do enable administrative efficiency) alongside real asymmetric extraction (communities lose autonomy over culturally constitutive practices). The beneficiary declaration (communities_preserving_autonomy) may appear counterintuitive — they are the victim group. But from the endogenous_climb reading perspective, preserving autonomy IS the benefit: communities that maintain endogenous drive to adopt new practices can do so; communities that lack that drive retain their autonomy and preserve identity. The victim (state_modernization_timeline) is the state's temporal goal — full displacement requires longer than the state wants to wait. This framing embeds the reading's core premise: successful displacement requires endogenous adoption, not decree alone.
 *
 * PERSPECTIVAL GAP:
 *   The perspective spread demonstrates the full indexical range. Subject communities (powerless/trapped/generational) see Snare: legal suppression with minimal coordination benefit, forcing behavioral compliance without genuine internalization. The state (institutional/arbitrage/immediate) sees Rope: solving a legitimate coordination problem (calendar standardization). Regional elites (moderate/constrained/biographical) see Tangled Rope: both cooperation (benefits from standardization) and extraction (pressure to abandon practices, tension between public and private). Underground communities (organized/constrained/generational) see Scaffold: a temporary pressure being circumvented through organized resistance, with clear sunset as enforcement weakens. The enforcement apparatus (institutional/arbitrage/civilizational) sees Piton: a degraded ritual maintained through institutional inertia, unable to prevent private practice persistence. The analytical observer (analytical/analytical/civilizational) sees Mountain: an immutable law that imposed practice cannot displace internalized identity. The gap between Snare (subject communities) and Rope (state) reflects the fundamental contest: is this a coordination problem (state's view) or a suppression mechanism (community's view)? The gap between all five practical perspectives and the mountain (analytical) reveals how the endogenous_climb reading naturalizes a contingent pattern into necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are computed from beneficiary/victim declarations and exit options. Subject communities: victim status + trapped exit → d ≈ 0.95 → high f(d) ≈ 1.42 → experienced extractiveness chi approaches epsilon × 1.42. State: beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → experienced extractiveness chi becomes negative (the state experiences the constraint as a benefit mechanism, not extraction). Regional elites: symmetric position (both benefit and bear costs) + constrained exit → d ≈ 0.50 → f(d) ≈ 0.65 → moderate chi. Underground communities: ambiguous (resisting the constraint, so structurally victim-like, but maintaining agency through organization) + constrained exit → d ≈ 0.45 (lower than powerless victims due to organized agency) → f(d) ≈ 0.42 → lower experienced extraction than powerless victims. The directionality derivation automatically captures the difference between powerless suppressed agents and organized agents maintaining resistance even under legal prohibition.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the coordination function (Rope reading) and extraction mechanism (Snare reading) are real and simultaneous, not competing framings. The state genuinely solves a coordination problem (calendar standardization benefits everyone who needs coordinated action). Communities genuinely bear extraction (forced abandonment of culturally constitutive practice). Both are true. The Tangled Rope classification captures the simultaneity. The mandatrophy is not 'which is it?' but 'whose benefit counts as coordination vs extraction?' From the state's perspective, standardization IS coordination. From communities' perspective, forced cultural displacement IS extraction. The indexical classification system models both positions simultaneously, showing that the same constraint appears as Rope from one position and Snare from another. The theater ratio's rise over time (0.42 → 0.68) indicates growing tension between the imposed structure and lived experience — enforcement machinery must work harder to maintain compliance theater as actual displacement fails. This is the diagnostic signal of a Tangled Rope under stress: if genuine coordination were achieved, theater should decrease (compliance would become spontaneous, not ritual); if genuine extraction were complete, theater should decrease (communities would be unable to maintain private practice). Instead, theater increases, indicating the constraint is stuck at the boundary between coordination and extraction, with neither fully achieved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_threshold_ambiguity,
    'At what generational distance does a formally displaced practice count as genuinely internalized vs. persistently resisted?',
    'Longitudinal ethnographic data on practice persistence across generations; measurement of private vs public practice retention; surveys tracking identity fusion with new vs old practices',
    'If threshold ≤ 1 generation: many cases misclassified as failures of displacement when community is still in active resistance phase. If threshold ≥ 3 generations: genuine displacement misclassified as persistent resistance. Classification shifts between tangled_rope and piton depending on where threshold is set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_threshold_ambiguity, empirical, 'Generational threshold for distinguishing internalization from persistent resistance').

omega_variable(
    enforcement_vs_internalization_causality,
    'Does sustained enforcement pressure accelerate internalization (by raising salience and forcing adaptation) or retard it (by increasing resentment and identity fusion with the old practice)?',
    'Comparative analysis of high-enforcement vs low-enforcement implementation timelines; measurement of resentment/group cohesion as function of enforcement intensity; tracking of practice persistence in private vs public spheres',
    'If enforcement accelerates: state pressure is necessary condition for displacement; tangled_rope reading understates extraction value. If enforcement retards: state pressure perpetuates resistance; snare reading understates suppression. Affects both epsilon and the directionality logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_internalization_causality, empirical, 'Causal direction between enforcement intensity and internalization speed').

omega_variable(
    endogenous_climb_vs_elite_diffusion,
    'Does displacement primarily require grassroots internalization (bottom-up adoption driven by perceived benefits), or does adoption by urban/elite populations create sufficient social pressure that rural/traditional populations eventually follow?',
    'Timeline analysis comparing elite adoption rates to general population adoption; network analysis of practice diffusion; study of whether rural adoption accelerates after elite adoption reaches critical mass',
    'If purely endogenous required: state timeline failures are systematic (high extractiveness, snare from below). If elite diffusion sufficient: hybrid scaffolding reading gains credibility (state mandate + elite adoption creates quasi-endogenous pull). If both required: all three readings are partially valid — this reading describes one causal mechanism among several.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogenous_climb_vs_elite_diffusion, empirical, 'Whether grassroots vs elite adoption pathways are independent mechanisms').

omega_variable(
    kernel_reading_contest,
    'Which reading of the legitimacy_of_imposed_practice kernel is correct: endogenous_climb (this reading), exogenous_override, or hybrid_scaffolding?',
    'This is a conceptual/preference omega. The readings are not empirically refutable — they are alternative framings of contested authority. The resolution depends on which authority structure you defer to: does the state''s formal legitimacy (exogenous) override community autonomy claims (endogenous)? Does ideology create a binding force (hybrid)? The engine routes this through reading_relations: this reading coexists_with and influences both siblings.',
    'The choice of reading determines which actor group is coded as beneficiary vs victim, which exit options are salient, and which classification emerges from the ''analytical observer'' perspective. No measurement resolves this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, preference, 'Which authority frame (state, community, hybrid) legitimizes practice displacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(limp_endoclimb_theater_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(limp_endoclimb_theater_t1, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(limp_endoclimb_theater_t2, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(limp_endoclimb_extract_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(limp_endoclimb_extract_t1, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(limp_endoclimb_extract_t2, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 30, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(limp_endoclimb_suppress_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(limp_endoclimb_suppress_t1, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(limp_endoclimb_suppress_t2, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'legitimacy_of_imposed_practice.' All three readings (endogenous_climb, exogenous_override, hybrid_scaffolding) model the same structural phenomenon — state-mandated practice displacement — but from different authority frames and with different epsilon values reflecting the empirical success of displacement under each reading's assumptions. The network links all three readings as sibling constraints. See constraint_id header and reading_relations in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
