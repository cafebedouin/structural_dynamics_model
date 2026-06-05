% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Dignity Maintenance (Dignity Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint story represents the DIGNITY READING of the speech
 *   protection kernel — one of five live constitutional readings about how
 *   speech rights should be bounded. The dignity reading holds that speech
 *   protection is conditional on not functioning as structural subordination
 *   of target groups. Unlike the absolutist reading (which protects speech
 *   near-categorically), the dignity reading recognizes group harm as a
 *   distinct category from individual offense and treats equal dignity as a
 *   ground for restricting certain speech (hate speech, group libel,
 *   dehumanizing propaganda). This reading has become institutionally
 *   dominant in many constitutional democracies (Canada, Germany, South
 *   Africa, India) while remaining contested in others (United States, where
 *   the marketplace reading retains stronger hold). The constraint is
 *   classified as Tangled Rope because it coordinates legitimate expression
 *   protection WITH protection from structural harm — genuine coordination
 *   function — but vests enforcement discretion in courts and administrative
 *   bodies, creating asymmetric power and potential for scope creep. The
 *   beneficiaries are historically marginalized groups protected by
 *   dignitarian restrictions; the victims are the abstract burdens of
 *   boundary contestation and the institutional discretion required to
 *   distinguish protected speech from subordinating speech.
 *
 * KEY AGENTS:
 *   - Historically marginalized groups: Primary beneficiary (moderate/constrained, powerless/trapped) — benefit from recognition of group harm as distinct category; trapped in having to prove dignity violations
 *   - Constitutional courts and speech-regulating authorities: Primary actor (institutional/arbitrage) — maintain and enforce the boundary; arbitrage exit because they can reinterpret restrictions or grant exemptions
 *   - Speech absolutist advocates: Secondary actor (organized/constrained) — contest the dignity boundary; constrained because they must argue within established constitutional frameworks
 *   - Victims of group subordination (sub-group differentiation by context): Primary victim (powerless/trapped) — cannot exit exposure to subordinating speech; enforcement gaps leave them unprotected
 *   - Speech boundary contestation system itself: Abstract victim — the constraint's need to distinguish protected from subordinating speech creates ongoing litigation, legislative dispute, and institutional burden
 *   - Constitutional reformers building alternatives: Tertiary actor (organized/mobile) — see the constraint as temporary solution to be superseded by structural changes reducing subordination viability
 *   - Analytical observer: Neutral perspective (analytical/analytical) — risks naturalizing dignitarian reading as irreducible natural law rather than one contingent reading of a contested kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.52).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.48).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Dignity Maintenance (Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '2b9bf45c-3742-4ec8-bdee-200d09e031f6').
narrative_ontology:cs_kernel_codification('2b9bf45c-3742-4ec8-bdee-200d09e031f6', formalized).
narrative_ontology:cs_authority_grounding('2b9bf45c-3742-4ec8-bdee-200d09e031f6', lineage).
narrative_ontology:cs_interpretation_layer_present('2b9bf45c-3742-4ec8-bdee-200d09e031f6').
narrative_ontology:cs_reading_relation('2b9bf45c-3742-4ec8-bdee-200d09e031f6', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b9bf45c-3742-4ec8-bdee-200d09e031f6', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('2b9bf45c-3742-4ec8-bdee-200d09e031f6', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b9bf45c-3742-4ec8-bdee-200d09e031f6', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_axiom('2b9bf45c-3742-4ec8-bdee-200d09e031f6', foundational, group_subordination_distinct_from_individual_harm).
narrative_ontology:cs_axiom_status(group_subordination_distinct_from_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('2b9bf45c-3742-4ec8-bdee-200d09e031f6', group_subordination_distinct_from_individual_harm, deontological).
narrative_ontology:cs_axiom('2b9bf45c-3742-4ec8-bdee-200d09e031f6', foundational, equal_dignity_speech_restriction_ground).
narrative_ontology:cs_axiom_status(equal_dignity_speech_restriction_ground, holdable).
narrative_ontology:cs_axiom_grounding('2b9bf45c-3742-4ec8-bdee-200d09e031f6', equal_dignity_speech_restriction_ground, deontological).
narrative_ontology:cs_reference_frame('2b9bf45c-3742-4ec8-bdee-200d09e031f6', equal_dignity_constitutional_foundation).
narrative_ontology:cs_drift_state('2b9bf45c-3742-4ec8-bdee-200d09e031f6', contemporary_enforcement_maturation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2b9bf45c-3742-4ec8-bdee-200d09e031f6', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, historically_marginalized_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speech_boundary_contestation).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, enforcement_discretion_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET GROUP (SNARE) — Cannot exit or avoid exposure to speech that functions as structural subordination. The constraint provides formal protection via dignity condition, but enforcement discretion and proof burden create gaps. Trapped by geographic/institutional presence and inability to opt out of public sphere participation. Experiences the constraint as extraction when enforcement fails.
constraint_indexing:constraint_classification(speech_protection_kernel__dignity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SPEECH BOUNDARY ARBITER (TANGLED ROPE) — Courts, administrative bodies, and institutional gatekeepers must distinguish protected speech from subordinating speech. This role coordinates legitimate expression protection WITH protecting groups from structural harm. But the enforcement burden creates asymmetric burden and institutional discretion — genuine coordination function paired with extractive gatekeeping power over the boundary itself.
constraint_indexing:constraint_classification(speech_protection_kernel__dignity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BENEFICIARY GROUPS (ROPE) — Historically marginalized groups benefit from the dignity condition: group harm is now a recognizable category, subordination is a violation, and equal dignity is enforceable. The constraint coordinates the collective claim to non-subordination with legitimate speech. Arbitrage exit because the group can engage in boundary contestation and legislative advocacy when dignity conditions are threatened.
constraint_indexing:constraint_classification(speech_protection_kernel__dignity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SPEECH ADVOCATE CONSTITUENCIES (TANGLED ROPE) — Organizations advocating speech protection face a coordination problem: they benefit from the dignitarian framing (which narrows but clarifies the permissible restriction set) and from legal clarity. But they also perceive extraction via the enforcement discretion burden on government — the constraint requires proving dignity harm, which invokes state power to police speech boundaries. Moderate power with constrained exit because the advocacy constituency cannot abandon speech protection norms entirely.
constraint_indexing:constraint_classification(speech_protection_kernel__dignity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORMERS (SCAFFOLD) — Constitutional amendment campaigns and institutional redesign efforts (ranked-choice voting, deliberative democracy reforms, participatory constitution-making) see the dignitarian speech constraint as a temporary solution to be superseded by structural changes that make subordination via speech less viable. Low effective extraction because organized reformers have agency to build new institutional architecture with sunset logic on the old boundary contests.
constraint_indexing:constraint_classification(speech_protection_kernel__dignity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL SPEECH ABSOLUTISM (PITON) — The absolutist doctrine persists in jurisprudence and rhetoric despite being substantially abandoned in practice. Most constitutional systems have dignity-based carve-outs (hate speech, group libel, incitement, harassment) but frame them as exceptions rather than integral to the speech protection norm. The absolutist framing is largely theater — maintained through doctrinal inertia and status-quo institutional power rather than functional justification. Theater ratio high because the doctrine's conceptual clarity masks practical pluralism.
constraint_indexing:constraint_classification(speech_protection_kernel__dignity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, equal human dignity is an irreducible normative foundation — no legitimate authority can subordinate a group structurally. The dignity reading sees the constraint as tracking this irreducible commitment. However, the presence of beneficiaries and the actual enforcement discretion contradict the mountain classification. The engine will identify this as a false summit revealing how dignity-as-natural-law reasoning naturalizes what is contingent institutional design.
constraint_indexing:constraint_classification(speech_protection_kernel__dignity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_protection_kernel__dignity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_protection_kernel__dignity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, TR),
    TR >= 0.70.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over time (0.35→0.52). The dignity reading coordinates legitimate protection for marginalized groups (genuine coordination function) with restrictions on speech that function as subordination. However, enforcement discretion creates extraction: courts and government bodies gain power to define boundaries, investigate speech, and suppress expression. The extractiveness rises over time (suppression_requirement 0.38→0.48) as enforcement becomes more sophisticated and scope creep occurs. Suppression (0.48): Moderate. Enforcement of the dignity boundary requires state capacity to investigate speech, evaluate context, and distinguish subordinating from protected expression. But suppression is not total — contested boundaries remain open for advocacy and reframing. Suppression increases over time as enforcement infrastructure matures. Theater ratio (0.35): Moderate-low. Unlike absolutist doctrine (high theater) or pure restriction (minimal theater), the dignity reading foregrounds functional analysis of what speech does — subordinates or expresses — rather than formal speech categories. Theater is present (rhetorical frames around dignity, legitimacy claims) but relatively grounded in structural analysis. Theater ratio increases slightly as institutionalization adds ritual to the boundary-policing process.
 *
 * PERSPECTIVAL GAP:
 *   The dignity reading creates perspectival distance between: (1) Target groups who experience the constraint as protective (Rope perspective) but also as extraction via enforcement burden (Snare undercurrent); (2) Speech advocates who see the boundary as extractive via government power but also valuable as clarity (Tangled Rope); (3) Absolutist doctrine which persists as Piton theater while practice implements dignitarian boundaries (Scaffold transition underway toward reformed frameworks); (4) The analytical observer who risks naturalizing dignity as Mountain when it is actually a contestable reading (false summit). The maximal gap is between the Rope beneficiary perspective (constraint coordinates and protects) and the Snare target perspective (constraint extracts via proof burden and enforcement discretion).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups (marginalized populations) derive directionality from being the victims of subordinating speech — high d value (0.75-0.85) reflecting full target status. But they are also the named beneficiaries of the constraint, creating upward pressure on d. The engine resolves this via exit_options: trapped agents have d→1.0 reflecting maximum experienced extraction despite being beneficiaries (they cannot exit the constraint even though it protects them). Speech boundary arbiters (courts, administrative bodies) derive moderate d (0.50-0.60) from being neither pure beneficiaries nor pure targets — they manage the constraint but can exit via reinterpretation or institutional shift. The institutional beneficiary (established groups not subject to subordination) derives low d (0.15-0.25) from arbitrage exit and beneficiary status. The false summit perspective (analytical observer) derives d from analytical observership (0.72-0.75) reflecting the analytical position's typical extraction level (the analyst sees structure others miss, creating tension).
 *
 * MANDATROPHY ANALYSIS:
 *   The dignitarian reading resolves potential mandatrophy by explicitly making the coordination function (protecting both expression AND equal dignity) and the extraction mechanism (enforcement discretion over speech boundaries) simultaneous and visible. The constraint is neither pure coordination nor pure extraction — it is genuinely hybrid. The mandatrophy arises because dignitarian jurisdictions claim to protect speech while actually restricting it, and absolutist jurisdictions claim maximum speech freedom while actually allowing subordinating speech. The dignity reading resolves this by making the restriction explicit: speech is protected when it does not subordinate; group dignity is a legitimate restriction ground. This makes the coordination function and extraction mechanism both transparent rather than hidden.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    group_harm_vs_individual_harm_boundary,
    'Where is the structurally defensible boundary between speech that harms individuals and speech that functions as structural group subordination?',
    'Comparative institutional analysis across constitutional systems; examination of enforcement outcomes over 10+ year periods; evaluation of whether group-based restrictions successfully prevent subordination without expanding into censorship of political critique',
    'If boundary is crisp and enforceable: dignity reading is stable Tangled Rope. If boundary collapses into individual offense or political disagreement: reading becomes Snare (enforcement becomes arbitrary extraction). If boundary shifts over time: reading requires modeling as time-varying constraint with Piton trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(group_harm_vs_individual_harm_boundary, conceptual, 'Structural boundary between individual harm and group subordination').

omega_variable(
    enforcement_discretion_ratchet,
    'Does the dignitarian restriction set create institutional ratchet dynamics where government enforcement discretion expands into political expression suppression over time?',
    'Historical analysis of restriction scope creep in jurisdictions with dignity-based speech carve-outs; examination of enforcement patterns targeting political dissent vs group subordination; comparison of narrow initial restrictions with expanded enforcement 20+ years later',
    'If ratchet is severe: the Snare perspective dominates and the constraint''s net effect is extraction disguised as protection. If ratchet is controlled: Tangled Rope classification holds. If ratchet is absent: dignity reading functions as intended coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_discretion_ratchet, empirical, 'Whether dignitarian restrictions create enforcement scope creep').

omega_variable(
    marketplace_vs_dignity_validity,
    'Under what conditions does the ''more speech as counter'' mechanism (marketplace reading) fail to prevent group subordination compared to restriction (dignity reading)?',
    'Comparative study of subordination persistence in high-speech-protection vs dignitarian systems; analysis of counter-speech effectiveness when power asymmetries favor subordinators; examination of whether marginalized groups can practically generate effective counter-speech',
    'If counter-speech is empirically effective: marketplace reading and dignity reading produce similar outcomes and the constraint is negotiable between frameworks. If counter-speech fails systematically: dignity reading''s restriction logic becomes defensible as necessary. If mixed: constraint stabilizes as Tangled Rope with real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_vs_dignity_validity, empirical, 'Efficacy of counter-speech vs restriction for preventing subordination').

omega_variable(
    reading_foreclosure_absolutism,
    'Does the dignity reading''s recognition of group harm as a legitimate restriction ground logically foreclose the absolutist reading, or do they occupy incompatible frameworks that coexist across different institutional positions?',
    'Philosophical analysis of whether both axioms (dignity constraint and unrestricted speech) can coexist in a single coherent normative framework; examination of whether jurisdictions hold both readings simultaneously across different domains (e.g., political speech protected absolutely, group-subordinating speech restricted)',
    'If foreclosed: absolute reading is analytically invalid and must be abandoned (strong structural claim). If coexists: different parties maintain incompatible commitments and the contest is inherently political. If domain-separated: readings apply to different expression categories and are compatible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_absolutism, conceptual, 'Whether dignity reading logically forecloses absolutist reading').

omega_variable(
    kernel_contestation_persistence,
    'Is the speech protection kernel genuinely contestable across multiple live readings, or has one reading effectively won the contest in the institutional domain?',
    'Survey of constitutional courts, legislatures, and academic discourse across 50+ jurisdictions; categorization of which readings are actively maintained by institutional authorities; identification of whether any reading has achieved dominant status or if pluralism persists',
    'If pluralism persists: kernel remains contested and all five sibling readings remain live positions. If one reading dominates: the kernel has effectively been resolved and minority readings are marginalized. If readings are jurisdiction-specific: the kernel is distributively contested across space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contestation_persistence, empirical, 'Persistence of kernel contestation across institutional domains').

omega_variable(
    false_summit_dignity_as_natural_law,
    'Is equal dignity a discovered natural law (Mountain) or a constructed reading of a contested kernel that benefits specific groups and harms others?',
    'Investigation of whether equal dignity commitments predate or postdate modern human rights movements; examination of whether the dignity framing serves to legitimize institutional restrictions that benefit some groups over others; analysis of whose interests are served by naturalizing dignity as foundational',
    'If natural law: mountain classification holds and the constraint is unchangeable by political process. If constructed: false summit is confirmed and the constraint is revealed as Tangled Rope with hidden asymmetries. If both: constraint requires modeling as contested natural law (CS structure with drift state).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_dignity_as_natural_law, conceptual, 'Whether dignity is natural law or constructed kernel reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_dignity_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(speech_dignity_tr_t10, speech_protection_kernel__dignity_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(speech_dignity_tr_t20, speech_protection_kernel__dignity_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(speech_dignity_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(speech_dignity_be_t10, speech_protection_kernel__dignity_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(speech_dignity_be_t20, speech_protection_kernel__dignity_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(speech_dignity_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(speech_dignity_su_t10, speech_protection_kernel__dignity_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(speech_dignity_su_t20, speech_protection_kernel__dignity_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).

% DUAL FORMULATION NOTE:
% The speech protection kernel decomposes into five structurally distinct constraint stories, one per reading. The dignity reading is this file. Each sibling reading (absolutist, marketplace, democratic-participation, harm-threshold) is a separate story with its own epsilon and perspectives. All five readings operate simultaneously across jurisdictions — some countries institutionalize the dignity reading, others the marketplace reading, others the democratic-participation reading. The kernel itself (the foundational claim about speech bounding) is contestable; the readings are the live institutional commitments across the constitutional world. Network edges link all five siblings to each other via mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
