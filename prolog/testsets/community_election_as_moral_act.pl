% ============================================================================
% CONSTRAINT STORY: community_election_as_moral_act
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_community_election_as_moral_act, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: community_election_as_moral_act
 *   human_readable: Community Election as Moral Act
 *   domain: moral_philosophy/social_psychology/virtue_ethics
 *
 * SUMMARY:
 *   The moral framework that treats community selection as a moral act rests
 *   on an empirically valid causal claim: relationships do transform
 *   character, and community transitions do correlate with moral framework
 *   shifts. However, this valid causal mechanism becomes extractive when
 *   access to community mobility is radically unequal. Those with exit
 *   options (mobile professionals, cosmopolitan elite) experience the
 *   framework as accurate description of their reality and use it for
 *   intentional moral development. Those structurally trapped in inherited
 *   arrangements (economically immobile, geographically bound, family
 *   caregivers) experience the same framework as moralization of their
 *   constraint: they are blamed for moral outcomes they cannot control
 *   because they cannot exit the communities that shape them. The constraint
 *   exhibits high extractiveness (0.68) not because the underlying psychology
 *   is false, but because the moral framework universalizes a capacity
 *   (community choice) that is actually a privilege. Theater ratio (0.58)
 *   reflects the gap between philosophical discourse on 'electing your
 *   community' and the sociological reality of structural immobility. The
 *   constraint has intensified over the 30-year interval as geographic
 *   mobility has become more concentrated among the educated professional
 *   class while economic immobility has increased for others.
 *
 * KEY AGENTS:
 *   - Those Structurally Trapped: Primary victims (powerless/trapped, moderate/constrained) — economically immobile, geographically bound, family caregivers who bear moral blame for character outcomes shaped by communities they cannot exit
 *   - Those with Exit Options: Primary beneficiaries (powerful/arbitrage, moderate/mobile) — mobile professionals and cosmopolitan elite who experience accurate moral framework and use community selection for intentional character development
 *   - Mutual Aid Networks: Organized agents (organized/constrained) — building alternative community structures to create exit options for the trapped; see constraint as temporary with generational sunset
 *   - Virtue Ethics Academy: Institutional actor (institutional/arbitrage) — maintains philosophical framework through institutional inertia; discourse is performative because addressed to audiences who already have mobility
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both genuine coordination function and asymmetric extraction; constraint is hybrid not pure type
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(community_election_as_moral_act, 0.68).
domain_priors:suppression_score(community_election_as_moral_act, 0.72).
domain_priors:theater_ratio(community_election_as_moral_act, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(community_election_as_moral_act, extractiveness, 0.68).
narrative_ontology:constraint_metric(community_election_as_moral_act, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(community_election_as_moral_act, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(community_election_as_moral_act, snare).
narrative_ontology:human_readable(community_election_as_moral_act, "Community Election as Moral Act").
narrative_ontology:topic_domain(community_election_as_moral_act, "moral_philosophy/social_psychology/virtue_ethics").

domain_priors:requires_active_enforcement(community_election_as_moral_act).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(community_election_as_moral_act, those_with_exit_options).
narrative_ontology:constraint_beneficiary(community_election_as_moral_act, mobile_professionals).
narrative_ontology:constraint_beneficiary(community_election_as_moral_act, cosmopolitan_class).
narrative_ontology:constraint_victim(community_election_as_moral_act, those_structurally_trapped_in_inherited_arrangements).
narrative_ontology:constraint_victim(community_election_as_moral_act, economically_immobile).
narrative_ontology:constraint_victim(community_election_as_moral_act, geographically_bound).
narrative_ontology:constraint_victim(community_election_as_moral_act, family_caregivers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GEOGRAPHICALLY BOUND (SNARE) — Trapped by economic immobility, family caregiving obligations, or lack of resources to relocate. The framing that 'you choose your community and therefore your character' moralizes their structural constraint as a personal failing. They bear maximum extraction: blamed for moral outcomes they cannot control because they cannot exit the communities they were born into.
constraint_indexing:constraint_classification(community_election_as_moral_act, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: FAMILY CAREGIVER (SNARE) — Constrained by caregiving responsibilities that prevent relocation. Can theoretically exit but at catastrophic cost (abandoning dependent family members). The moral framing extracts by treating their constraint as a choice: 'you could leave if you really wanted to change.' High suppression through guilt and social obligation.
constraint_indexing:constraint_classification(community_election_as_moral_act, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CAREER MIGRANT (TANGLED ROPE) — Mobile through professional credentials but experiences genuine coordination function: the ability to select communities does enable intentional character development through relationship choice. Also experiences extraction: the moral framework obscures how their mobility is a privilege, not a universal capacity, and creates pressure to optimize community selection as moral performance.
constraint_indexing:constraint_classification(community_election_as_moral_act, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: COSMOPOLITAN ELITE (ROPE) — Experiences the constraint as pure coordination. Can select communities globally based on values alignment, professional networks, or lifestyle preferences. The moral framework accurately describes their reality: they do choose their communities and those choices do shape their character. Net beneficiary of the framing.
constraint_indexing:constraint_classification(community_election_as_moral_act, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MUTUAL AID NETWORK (SCAFFOLD) — Organized groups building alternative community structures (intentional communities, cooperative housing, distributed care networks) that reduce the extraction by creating exit options for the trapped. Sees the constraint as temporary: as mutual aid infrastructure matures, more people gain genuine community choice. Estimated sunset: 15-25 years for alternative structures to reach scale.
constraint_indexing:constraint_classification(community_election_as_moral_act, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: VIRTUE ETHICS ACADEMY (PITON) — Academic moral philosophy maintains the 'community election' framework through institutional inertia despite empirical evidence of structural immobility. The philosophical discourse is largely performative: seminars on 'choosing your community' addressed to audiences who already have exit options. Theater ratio reflects gap between philosophical ideal and sociological reality.
constraint_indexing:constraint_classification(community_election_as_moral_act, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (relationships do transform character; community selection does enable moral development for those with mobility) AND the asymmetric extraction (the framework moralizes structural immobility as personal failure). The constraint is not pure extraction because the causal claim is empirically valid; it is not pure coordination because access to the mechanism is radically unequal.
constraint_indexing:constraint_classification(community_election_as_moral_act, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(community_election_as_moral_act_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(community_election_as_moral_act, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(community_election_as_moral_act, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(community_election_as_moral_act, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(community_election_as_moral_act, TR),
    TR >= 0.70.

:- end_tests(community_election_as_moral_act_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The moral framework moralizes structural privilege as personal virtue. Those with community mobility genuinely benefit from intentional relationship selection, but the framework extracts from the immobile by treating their constraint as moral failure. The extraction is not total (0.68 not 0.85) because the underlying causal mechanism is real — relationships do shape character — so there is genuine coordination value for those with access. Suppression (0.72): High. Multiple mechanisms suppress alternatives: economic barriers to relocation, family caregiving obligations that prevent exit, social norms that frame leaving as abandonment, and the moral framework itself which treats staying in toxic communities as character weakness rather than structural constraint. Theater ratio (0.58): Moderate-high. Academic discourse on community election is substantially performative: philosophical seminars addressed to mobile professional audiences, self-help literature that assumes exit capacity, virtue ethics frameworks that ignore structural sociology. The theater has increased as mobility has concentrated among the educated class while the discourse has remained universal.
 *
 * PERSPECTIVAL GAP:
 *   The cosmopolitan elite sees pure coordination (Rope) — the moral framework accurately describes their reality and enables intentional character development through community choice. The geographically bound see pure extraction (Snare) — the same framework moralizes their structural immobility as personal moral failure. The career migrant sees hybrid coordination-extraction (Tangled Rope) — they benefit from mobility but recognize the framework obscures privilege. The mutual aid network sees temporary problem with sunset (Scaffold) — alternative community structures are creating exit options that will reduce extraction over generational timescales. The virtue ethics academy sees degraded ritual (Piton) — maintains philosophical framework through inertia despite empirical evidence of structural immobility. The analytical observer sees the constraint as fundamentally hybrid (Tangled Rope) — not pure extraction because the causal mechanism is real, not pure coordination because access is radically unequal. The perspectival gap reveals that the constraint's type depends entirely on the observer's structural position relative to community mobility.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those with genuine exit options: mobile professionals who can relocate for career or values, cosmopolitan elite with global community access, organized mutual aid networks building alternative structures. They experience low directionality (d ≈ 0.10-0.25) because the constraint runs toward them — they capture the coordination value of intentional community selection. Victims are those structurally trapped: economically immobile who cannot afford relocation, geographically bound by family or housing, caregivers who cannot abandon dependents. They experience high directionality (d ≈ 0.85-0.95) because the constraint extracts from them — they bear moral blame for outcomes they cannot control. The analytical observer recognizes the hybrid structure: genuine coordination for the mobile, pure extraction for the trapped, yielding moderate directionality (d ≈ 0.55) that reflects the mixed function.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates why high extractiveness does not automatically imply pure extraction (Snare from all perspectives). The analytical classification is Tangled Rope because the constraint genuinely coordinates intentional moral development for those with community mobility — the causal claim that relationships transform character is empirically valid. The extraction arises not from the mechanism being false but from access being unequal. A pure Snare would involve a false or purely theatrical mechanism (like astrology-based character assessment). Here the mechanism is real but the moral framework universalizes a privilege, creating asymmetric extraction. The mandatrophy is resolved by recognizing that coordination and extraction can coexist in the same constraint when access to the coordination mechanism is structurally unequal. The Tangled Rope classification captures this hybrid structure: genuine coordination function (relationships do shape character) combined with asymmetric extraction (only the mobile can choose their relationships).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    character_plasticity_threshold,
    'At what age or life stage does character become sufficiently fixed that community change no longer produces meaningful moral transformation?',
    'Longitudinal psychological studies tracking personality trait stability and moral framework shifts across community transitions at different life stages',
    'If plasticity persists into late adulthood: the constraint''s coordination function remains active across lifespan, increasing its value for mobile agents. If plasticity declines sharply after early adulthood: the extraction intensifies because the framework demands impossible change from older trapped agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(character_plasticity_threshold, empirical, 'Age threshold for character plasticity in response to community change').

omega_variable(
    virtual_community_sufficiency,
    'Do online communities produce character transformation comparable to geographic communities, thereby reducing the extraction on the geographically trapped?',
    'Comparative studies of moral framework shifts and character trait changes in participants of online vs geographic communities; measurement of relationship depth and transformative capacity across modalities',
    'If virtual communities are transformatively sufficient: the constraint''s extraction drops significantly because geographic immobility no longer equals community immobility. If virtual communities are transformatively insufficient: the extraction persists and may intensify as virtual participation creates illusion of choice without substance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(virtual_community_sufficiency, empirical, 'Whether virtual communities enable character transformation comparable to geographic communities').

omega_variable(
    inherited_community_quality_distribution,
    'Are inherited communities (those one is born into) systematically lower quality than elected communities (those one chooses), or is the quality distribution similar?',
    'Sociological analysis of community characteristics (social capital, norm enforcement, mutual support, toxicity markers) comparing inherited vs elected communities across socioeconomic strata',
    'If inherited communities are systematically worse: the extraction is partially justified because trapped agents genuinely suffer from lower-quality moral environments. If quality distribution is similar: the extraction is pure moralization of privilege because the mobile are not actually accessing better communities, just different ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherited_community_quality_distribution, empirical, 'Quality distribution of inherited vs elected communities').

omega_variable(
    moral_responsibility_threshold,
    'What degree of exit capacity is required before an agent bears moral responsibility for their community''s influence on their character?',
    'Philosophical analysis combined with empirical data on exit costs and moral framework shifts; identification of threshold where constraint becomes choice',
    'If threshold is low (any non-zero exit capacity): most agents bear responsibility, reducing perceived extraction. If threshold is high (only arbitrage-level exit counts): most agents are victims of circumstance, increasing perceived extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_responsibility_threshold, preference, 'Exit capacity threshold for moral responsibility attribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(community_election_as_moral_act, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_elect_tr_t0, community_election_as_moral_act, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comm_elect_tr_t10, community_election_as_moral_act, theater_ratio, 10, 0.45).
narrative_ontology:measurement(comm_elect_tr_t20, community_election_as_moral_act, theater_ratio, 20, 0.52).
narrative_ontology:measurement(comm_elect_tr_t30, community_election_as_moral_act, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(comm_elect_be_t0, community_election_as_moral_act, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comm_elect_be_t10, community_election_as_moral_act, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(comm_elect_be_t20, community_election_as_moral_act, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(comm_elect_be_t30, community_election_as_moral_act, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(community_election_as_moral_act, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of proximity_affinity_conflation (rope) and character_revelation_asymmetry (tangled_rope). The upstream constraints establish the causal mechanisms (proximity shapes affinity; character is revealed through relationship stress) that this constraint moralizes. The community election framework takes empirically valid psychological mechanisms and universalizes them into a moral framework that ignores structural access inequality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
