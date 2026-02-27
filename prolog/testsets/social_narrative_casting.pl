% ============================================================================
% CONSTRAINT STORY: social_narrative_casting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_narrative_casting, []).

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
 *   constraint_id: social_narrative_casting
 *   human_readable: Social Narrative Casting (Criticism-as-Projection)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   Social narrative casting is the act of assigning a target person a fixed
 *   role within the critic's internal or shared narrative: the villain, the
 *   victim, the obstacle, the savior. The 'criticism' is the critic's attempt
 *   to hire the target into this role by invoking social pressure, shame,
 *   role-lock, and counter-narrative suppression. This constraint exhibits
 *   the full range of DR types depending on the observer's structural
 *   position. From the cast subject's perspective, it is pure extraction
 *   (Snare) — they are trapped within the critic's relational space and
 *   cannot escape the role assignment. From a therapist's perspective, it is
 *   coordination (Rope) — the critic is attempting to regulate discomfort
 *   through shared narrative, and the problem is a failure to communicate.
 *   From an institutional perspective, it is performative inertia (Piton) —
 *   inherited role assignments persist as ritual despite loss of original
 *   function. From a support coalition perspective, it is temporary and
 *   solvable (Scaffold) — awareness and alternative narratives can break the
 *   casting. The constraint generates perspectival gaps because the critic,
 *   subject, and observers have fundamentally different structural
 *   relationships to the narrative mechanism. The theater ratio (0.64)
 *   reflects that much of the casting process is performative: critics enact
 *   the role assignment through ritual (gossip, reference, framing) even when
 *   the original discomfort or threat no longer exists. The subject may
 *   comply with the role not because the role is accurate but because
 *   compliance avoids conflict — a classic example of Goodhart drift, where
 *   the performative markers (subject accepting the role, critic confirming
 *   the narrative) replace the original function (regulating genuine threat
 *   or discomfort). The constraint shows extraction growth from 0.32 to 0.52
 *   over the measurement interval, indicating that repeated role-locking
 *   deepens the subject's internalization and reduces their counter-narrative
 *   capacity.
 *
 * KEY AGENTS:
 *   - Critic (Director): Institutional/arbitrage — benefits from narrative maintenance and subject role compliance; initiates and sustains the casting mechanism
 *   - Cast Subject: Powerless/trapped — target of narrative casting; bears full cost of role-lock and counter-narrative suppression
 *   - Peer Observer (Aware): Moderate/constrained — recognizes the casting dynamic; experiences tension between social loyalty and recognition of extraction
 *   - Therapist/Mediator: Institutional/arbitrage — professional observer with capacity to reframe and provide alternative coordination mechanisms
 *   - Institutional Culture: Institutional/arbitrage — inherited role frames persist through organizational ritual and shared gossip
 *   - Support Coalition: Organized/constrained — provides alternative narratives and exit pathways for cast subjects
 *   - Analytical Observer: Analytical/analytical — identifies the constraint as both coordination (sense-making) and extraction (appropriation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_narrative_casting, 0.52).
domain_priors:suppression_score(social_narrative_casting, 0.68).
domain_priors:theater_ratio(social_narrative_casting, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_narrative_casting, extractiveness, 0.52).
narrative_ontology:constraint_metric(social_narrative_casting, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(social_narrative_casting, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_narrative_casting, tangled_rope).
narrative_ontology:human_readable(social_narrative_casting, "Social Narrative Casting (Criticism-as-Projection)").
narrative_ontology:topic_domain(social_narrative_casting, "social/psychological").

domain_priors:requires_active_enforcement(social_narrative_casting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_narrative_casting, critic_narrative_maintenance).
narrative_ontology:constraint_victim(social_narrative_casting, cast_subject_autonomy).
narrative_ontology:constraint_victim(social_narrative_casting, interpersonal_trust_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CAST SUBJECT (SNARE) — The target of narrative casting has no exit option within the critic's social/relational space. Trapped by intimacy, kinship, or institutional dependency, the subject cannot simply leave the relationship. The critic's casting mechanism uses social pressure, shame attribution, and role-lock to suppress the subject's counter-narrative. The subject experiences maximum extraction: their autonomy and self-narration are appropriated to serve the critic's internal drama.
constraint_indexing:constraint_classification(social_narrative_casting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE AWARE PEER OBSERVER (TANGLED ROPE) — A friend, family member, or colleague who recognizes the casting dynamic. This observer has constrained exit options: they can distance themselves from the critic but at social/relational cost. They see both coordination (the critic is attempting to regulate their own cognitive dissonance through shared narrative) and extraction (the subject is being used without consent). The constraint appears as a hybrid: partly a coordination failure that needs collective recognition, partly an extractive suppression mechanism.
constraint_indexing:constraint_classification(social_narrative_casting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE THERAPIST OR MEDIATOR (ROPE) — A professional observer with the capacity to arbitrage between the critic and subject. The therapist sees the casting mechanism as a coordination problem: the critic is attempting to communicate discomfort or threat through narrative role-assignment, and the subject is not aware they are being cast. The therapist's role is to make visible the implicit contract and provide alternative coordination mechanisms. The extraction from the subject is minimal from this perspective because the therapeutic frame offers exit (seek different relationships) and reframes the constraint as solvable.
constraint_indexing:constraint_classification(social_narrative_casting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INSTITUTIONAL CULTURE FRAME (PITON) — Organizations, families, and communities have inherited narratives about member roles: 'the problem employee,' 'the difficult relative,' 'the troublemaker.' These institutional casting frames persist through inertia — reinforced by ritual (staff meetings where the frame is reconfirmed), shared gossip, and performative compliance. The actual function of the casting (coordination of uncertainty, attribution of blame) has atrophied; what remains is the ritual performance of the role assignment. The frame persists because members have not yet built alternative coordination mechanisms.
constraint_indexing:constraint_classification(social_narrative_casting, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ADVOCACY OR SUPPORT COALITION (SCAFFOLD) — Organized groups (support networks, consciousness-raising circles, advocacy organizations) that help cast subjects recognize and exit narrative casting. These coalitions see the constraint as a temporary coordination failure with a sunset clause: as awareness increases, as alternative narratives become available, and as subjects gain agency through collective support, the casting mechanism loses force. The coalition's existence provides an exit path the powerless agent did not previously have.
constraint_indexing:constraint_classification(social_narrative_casting, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, narrative casting is both a coordination mechanism (humans use story-assignment to make sense of unpredictability and threat) and an extraction mechanism (the casting appropriates the subject's autonomy without consent). The constraint is structural: humans cannot avoid narrative cognition. But the suppression of the subject's counter-narrative is a contingent choice. The analytical view identifies this as tangled rope: genuine coordination function (sense-making under uncertainty) wrapped in asymmetric extraction (role-lock and counter-narrative suppression).
constraint_indexing:constraint_classification(social_narrative_casting, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_narrative_casting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_narrative_casting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_narrative_casting, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_narrative_casting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_narrative_casting, TR),
    TR >= 0.70.

:- end_tests(social_narrative_casting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The critic captures the subject's narrative autonomy and uses the subject's behavior and identity as material for the critic's internal story. This is genuine extraction, but it is not maximal because subjects often retain some narrative agency in other relational contexts or can eventually exit the relationship. The value reflects the asymmetry between the critic's narrative control and the subject's limited counter-narrative capacity. Suppression (0.68): High. The casting mechanism works by suppressing the subject's counter-narrative through shame, invalidation, role-lock, and social pressure. The subject faces significant barriers to asserting alternative self-descriptions within the critic's social space. However, suppression is not total — subjects can seek support outside the relationship, therapists can help name the mechanism, and coalitions can provide alternative narratives. Theater ratio (0.64): Moderate-high. Much of the casting performance is theater: the critic enacts the role assignment through ritual (gossip, framing, reference) that persists even when the original discomfort has been resolved. The subject may comply with the role not because the role is accurate but because compliance avoids conflict. The theater ratio rises over time as the performative enactment becomes decoupled from any regulatory function.
 *
 * PERSPECTIVAL GAP:
 *   The cast subject and the critic have opposite structural relationships to the casting constraint. The subject experiences it as extraction and role-lock (Snare). The critic experiences it as coordination and sense-making (Rope from institutional perspective). This gap persists because the critic typically lacks awareness that they are casting — from their perspective, they are simply observing and naming the subject's 'real' characteristics. The aware peer observer sees both: they recognize the extraction (subject is being appropriated) and the coordination function (critic is regulating discomfort), producing a Tangled Rope classification. The therapist sees the problem as solvable through explicit communication and alternative coordination mechanisms, classifying it as Rope with low experienced extraction because the therapeutic frame provides exit. The institutional culture frame sees inherited role assignments persisting through ritual (Piton), while the support coalition sees them as temporary problems with solutions (Scaffold). The analytical observer sees the constraint as fundamentally both coordination and extraction — narrative sense-making is necessary and structural, but the suppression of counter-narratives is contingent and extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality chain depends on recognizing who benefits from the narrative casting and who bears the costs. The critic benefits (maintains narrative coherence, regulates internal discomfort, receives social confirmation of the narrative). The subject bears costs (autonomy appropriation, role-lock, counter-narrative suppression). The beneficiary/victim declaration is clear from the structural data. The exit options determine how constrained the subject is — if trapped by kinship or institutional dependency, their d approaches 1.0 (full target). If they have alternative relational contexts or support networks, d may be lower. The derivation chain produces a perspectival gap: the critic's arbitrage exit + beneficiary status gives them d ~0.05-0.15 (derives to f(d)≈-0.12 to -0.01, negative effective extraction, they experience this as low-cost coordination). The subject's trapped exit + victim status gives them d ~0.95 (derives to f(d)≈1.42, maximum experienced extraction, they see pure cost). The aware peer's constrained exit + both-beneficiary-and-victim status (benefits from shared narrative but pays cost of witnessing extraction) gives d ~0.50-0.65, producing the Tangled Rope classification. The support coalition's constrained exit but upgraded to mobile through collective action produces d ~0.55-0.65 but with declining trajectory (Scaffold), because the coalition exit option improves over time.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint is NOT a case of coordination mislabeled as extraction, nor extraction mislabeled as coordination. It is genuinely BOTH — hence Tangled Rope at the analytical level. The coordination function (critic's narrative sense-making, regulation of discomfort, shared story-building) is real and serves a social function. The extraction mechanism (subject's appropriated autonomy, counter-narrative suppression, role-lock) is also real and asymmetric. The mandatrophy is resolved by recognizing that narrative casting is a hybrid constraint: it performs a coordination function (sense-making, threat regulation) AND it achieves this through extraction (role-lock, suppression). The critic is not lying about coordination — they genuinely need to make sense of the subject's behavior or regulate their own uncertainty. But they are doing so through a mechanism (role-assignment without consent, counter-narrative suppression) that is extractive. The therapy framing (Rope perspective) offers an exit: explicit communication of the underlying discomfort, alternative coordination mechanisms (direct conversation, collaborative meaning-making, validation of multiple narratives) that achieve the same sense-making with lower suppression. The support coalition framing (Scaffold perspective) offers another exit: the subject gains alternative narratives and community validation that breaks the critic's monopoly on interpretation. The piton framing (Piton perspective) identifies the mechanism as possibly atrophied ritual — role assignments persist even when the original discomfort no longer exists, suggesting the constraint's primary function has degraded. The mandatrophy does not resolve by choosing one type, but by mapping the perspectival landscape: Who benefits? Who bears costs? What coordination is actually happening? What extraction mechanism supports it? What exit paths exist or could be built? The answers to these questions produce the full six-type spectrum and reveal the constraint's true structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    projection_versus_observation,
    'When a critic assigns a role to a subject, how much of the role description reflects the subject''s actual behavior versus the critic''s internal narrative needs?',
    'Third-party observation; longitudinal comparison of critic''s description vs subject''s behavior in different relational contexts; analysis of role consistency across different relationships the subject maintains',
    'High projection component: casting is pure extraction (Snare emphasis). Low projection component: casting is failed coordination attempt (Rope/Tangled Rope emphasis). The omega determines whether the constraint is motivated by threat/discomfort regulation (coordination) or by control (extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(projection_versus_observation, empirical, 'Ratio of projection to observation in narrative casting').

omega_variable(
    suppression_mechanism_type,
    'Is the subject''s counter-narrative suppressed through explicit pressure (shaming, punishment, exclusion) or through implicit pressure (invalidation, reframing, dismissal)?',
    'Documentation of suppression attempts; analysis of whether counter-narrative attempts trigger overt conflict or covert delegitimization; longitudinal tracking of subject''s willingness to narrate',
    'Explicit suppression: constraints are clearly coercive (Snare/Tangled Rope with high measured suppression). Implicit suppression: constraint appears as mutual narrative failure (Rope/Piton with high theater). The mechanism type determines whether subjects recognize extraction or internalize it as their own narrative failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Explicit vs implicit suppression mechanisms').

omega_variable(
    subject_narrative_agency_preservation,
    'Do subjects in casting relationships maintain alternative self-narratives in other relational contexts, or does the casting internalize into a unified identity?',
    'Comparative analysis of subject''s self-narration across different relationships and contexts; assessment of whether subject recognizes inconsistency or has integrated contradictions into a unified self-concept; longitudinal tracking of subject autonomy in contexts without the critic',
    'High agency preservation: subject can exit through context-switching (exit_options: mobile). Low agency preservation: subject has internalized casting into identity (exit_options: trapped). Determines whether the constraint is a relational extraction or an intrapsychic capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subject_narrative_agency_preservation, empirical, 'Whether subjects preserve alternative self-narratives across contexts').

omega_variable(
    critic_awareness_of_mechanism,
    'Is the critic conscious of their narrative casting, or is it an unconscious projection mechanism?',
    'Meta-conversation attempts with critic; observation of whether explicit naming of the casting pattern produces behavior change; assessment of critic''s response to evidence contradicting the casting narrative',
    'Conscious casting: possibly strategic, more clearly extraction (Snare). Unconscious projection: possibly genuine discomfort regulation, more clearly mixed (Tangled Rope). Affects whether constraint is amenable to therapeutic intervention or requires structural separation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critic_awareness_of_mechanism, conceptual, 'Whether narrative casting is conscious strategy or unconscious projection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_narrative_casting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(snc_tr_t0, social_narrative_casting, theater_ratio, 0, 0.38).
narrative_ontology:measurement(snc_tr_t5, social_narrative_casting, theater_ratio, 5, 0.51).
narrative_ontology:measurement(snc_tr_t10, social_narrative_casting, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(snc_be_t0, social_narrative_casting, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(snc_be_t5, social_narrative_casting, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(snc_be_t10, social_narrative_casting, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_narrative_casting, information_standard).
narrative_ontology:affects_constraint(social_narrative_casting, shame_attribution_mechanism).
narrative_ontology:affects_constraint(social_narrative_casting, interpersonal_narrative_monopoly).
narrative_ontology:affects_constraint(social_narrative_casting, identity_internalization_dynamics).

% DUAL FORMULATION NOTE:
% Social narrative casting decomposes into at least three related constraints: (1) the casting mechanism itself (this story) — how critics assign roles through criticism, (2) shame attribution (downstream) — how the subject internalizes the role through shame and self-blame, and (3) narrative monopoly (upstream) — the structural conditions that allow one agent's interpretation to dominate. Each has distinct ε values and structural data. This story (ε=0.52, Tangled Rope) links to both upstream (narrative monopoly) and downstream (shame internalization) constraints. The perspectival landscape shown here is valid only when all three stories are read together as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_narrative_casting, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
