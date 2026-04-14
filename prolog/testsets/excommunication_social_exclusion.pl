% ============================================================================
% CONSTRAINT STORY: excommunication_social_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_excommunication_social_exclusion, []).

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
 *   constraint_id: excommunication_social_exclusion
 *   human_readable: Excommunication and Social Exclusion Mechanism
 *   domain: social/institutional/religious
 *
 * SUMMARY:
 *   Excommunication and social exclusion represent a constraint where
 *   institutional authority and community enforcement combine to produce
 *   identity dissolution through formal exclusion. The mechanism operates at
 *   the intersection of institutional power (formal excommunication decree),
 *   social coercion (community ostracism), and identity fusion (the target's
 *   self-concept constituted through community membership and belief
 *   practice). This constraint exhibits all six DR types from different
 *   perspectives, revealing how the same structural phenomenon appears as
 *   immutable law (mountain), coordination mechanism (rope), mixed extraction
 *   (tangled rope), pure extraction (snare), temporary problem (scaffold), or
 *   degraded ritual (piton) depending on the observer's structural position.
 *   The constraint's extractiveness (0.68) reflects that excommunication
 *   achieves asymmetric psychological extraction from the target across their
 *   remaining lifespan — the identity lock persists even after institutional
 *   enforcement ceases. Suppression (0.75) reflects high barriers to exit:
 *   social ostracism, economic isolation, family severance, and internalized
 *   unworthiness. Theater ratio (0.65) reflects that the formal ritual
 *   (pronouncements, ceremonies, official decrees) is performative — its
 *   coercive power depends entirely on whether the surrounding community
 *   believes and enforces the exclusion status.
 *
 * KEY AGENTS:
 *   - Excommunicated Individual: Primary victim (powerless/identity_locked) — bears full psychological and social extraction; identity fused with institution makes exit psychologically lethal
 *   - Continuing Community Members: Secondary actors (moderate/constrained) — constrained by conformity, belief alignment, and social pressure to enforce boundaries; benefit from institutional coherence alongside bearing enforcement costs
 *   - Instituting Authority: Primary beneficiary (institutional/arbitrage) — controls boundary definition, membership allocation, and exclusion criteria; experiences constraint as coordination mechanism
 *   - Social Network Integrity: Abstract victim (powerless/trapped) — ecosystem harm from forced severing of kin/relational ties; cannot organize or exit
 *   - Secular/Alternative Community Frameworks: Organized agents (organized/mobile) — building exit pathways through employment-based identity, secular community, legal protections; reducing excommunication's coercive power over generations
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable group boundary properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(excommunication_social_exclusion, 0.68).
domain_priors:suppression_score(excommunication_social_exclusion, 0.75).
domain_priors:theater_ratio(excommunication_social_exclusion, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(excommunication_social_exclusion, extractiveness, 0.68).
narrative_ontology:constraint_metric(excommunication_social_exclusion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(excommunication_social_exclusion, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(excommunication_social_exclusion, snare).
narrative_ontology:human_readable(excommunication_social_exclusion, "Excommunication and Social Exclusion Mechanism").
narrative_ontology:topic_domain(excommunication_social_exclusion, "social/institutional/religious").

domain_priors:requires_active_enforcement(excommunication_social_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(excommunication_social_exclusion, instituting_authority).
narrative_ontology:constraint_victim(excommunication_social_exclusion, excommunicated_individual).
narrative_ontology:constraint_victim(excommunication_social_exclusion, social_network_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCOMMUNICATED MEMBER (SNARE) — Identity-locked to religious/community institution despite structural mobility. The binding is cognitive fusion: self-concept constituted through community membership, faith practice, and social role. Material barriers (social ostracism, economic isolation) reinforce the identity lock. Cannot exit without psychological death — becoming a different person. Maximum experienced extraction from the perspective of an agent whose identity is dissolved through institutional act.
constraint_indexing:constraint_classification(excommunication_social_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CONTINUING COMMUNITY MEMBERS (TANGLED ROPE) — Constrained by social conformity, family ties, and belief system alignment. Perceive genuine coordination function: boundary maintenance, norm enforcement, collective identity protection. But also experience extraction — coerced participation in ostracism, social control, epistemic constraint. Mixed experience: benefit from community cohesion alongside bearing enforcement costs.
constraint_indexing:constraint_classification(excommunication_social_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTING AUTHORITY (ROPE) — Institutional actor with arbitrage exit (can redefine doctrine, rescind decisions, shift power structure). Experiences the constraint as coordination: enforcing boundaries, maintaining institutional coherence, allocating membership status. Net beneficiary — extraction flows toward institutional authority. Low effective extraction from this perspective because they control the mechanism.
constraint_indexing:constraint_classification(excommunication_social_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SECULAR AND ALTERNATIVE COMMUNITY FRAMEWORKS (SCAFFOLD) — Organized agents (secular institutions, alternative communities, legal protections) building exit pathways and alternative social identities. Modern secular communities, employment-based identity structures, and legal protections reduce excommunication's bite. Sunset mechanism: as institutional religion's social monopoly erodes, excommunication loses coercive power. Temporary constraint being resolved by social pluralism.
constraint_indexing:constraint_classification(excommunication_social_exclusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EXCOMMUNICATION RITUAL FORM (PITON) — The formal ritual of excommunication persists through institutional inertia despite reduced functional power in pluralistic societies. Theater ratio high: the ceremony, pronouncements, and formal exclusion are performative — their coercive effect depends entirely on whether the surrounding society enforces the social isolation. In societies with alternative identity structures, the ritual becomes theatrical performance rather than binding mechanism.
constraint_indexing:constraint_classification(excommunication_social_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a universal/civilizational perspective, social exclusion is claimed as immutable: all human groups exclude deviants; excommunication expresses a natural law of group boundary maintenance. However, this perspective naturalizes what is structurally contingent: excommunication's coercive power depends on social monopoly (religion is sole source of identity/community), legal enforcement absence, and the target's identity fusion. These are institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(excommunication_social_exclusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(excommunication_social_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(excommunication_social_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(excommunication_social_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(excommunication_social_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(excommunication_social_exclusion, TR),
    TR >= 0.70.

:- end_tests(excommunication_social_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The mechanism achieves sustained psychological extraction from the target through identity dissolution and social ostracism. The value reflects that excommunication's coercive power is severe — it forces the target to either accept complete identity erasure within their social context or psychologically rupture through exit. The trajectory in measurements shows increasing extractiveness over the interval (0.45 → 0.68) as social isolation intensifies and alternative identity structures become unavailable. Suppression (0.75): High. Barriers to exit are multifaceted and reinforcing: legal systems often enforce institutional authority (historical/theocratic contexts), family members participate in ostracism, economic systems tied to institutional membership, and most critically, the target's identity cannot exit without cognitive reframing that feels like self-annihilation. In modern pluralistic societies, suppression is lower because alternative identity structures exist, but in institutional monopoly contexts (historical religion, tight ideological communities, cults), suppression remains near maximum. Theater ratio (0.65): Moderate-high. The formal ritual — pronouncements, ceremonies, official decrees — is substantially performative. The coercive mechanism is not the ritual itself but the surrounding community's willingness to enforce social ostracism. In pluralistic societies with weak institutional authority, theater increases (the ritual becomes ceremony without teeth); in institutional monopoly contexts, theater decreases (the ritual is backed by real enforcement power).
 *
 * PERSPECTIVAL GAP:
 *   Excommunication demonstrates how the same institutional act produces six distinct classifications. The target perceives Snare (identity locked, trapped). The community perceives Tangled Rope (mixed coordination and extraction). The authority perceives Rope (coordination mechanism). Alternative frameworks perceive Scaffold (temporary, sunset). The ritual perceives Piton (its own degradation). The analyst perceives false Mountain (naturalized contingency). No single classification is correct — the presheaf over observation positions IS the structure. The perspectival gap reveals that excommunication's coercive power is not intrinsic to the mechanism but depends on social monopoly. In pluralistic contexts where the target can access alternative identity structures, the classification shifts from Snare toward Scaffold/Piton. In monopoly contexts, it remains Snare indefinitely.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from structural position: power level, exit options, and relationship to the extraction flow. The excommunicated individual with identity_locked exit (structurally mobile but cognitively trapped) experiences high d because their cognitive frame prevents exit exercise → high f(d) → high chi. The continuing community members with constrained exit experience moderate d — they face conformity costs but have not internalized unworthiness. The instituting authority with arbitrage exit experiences low d because they control the mechanism — d approaches 0.0 (full beneficiary position). The organized secular frameworks with mobile exit experience low-moderate d — they have structural exit and are building alternatives, reducing extraction. The directionality overrides are not needed here because the derivation chain from beneficiary/victim + exit options produces accurate d values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing how institutional exclusion mechanisms produce all six types simultaneously. The mandatrophy is not 'which type is correct?' but 'at what social scale of monopoly?' In societies with institutional religious monopoly (medieval Christendom, theocratic states, isolated communities), excommunication is Snare across nearly all perspectives — alternative identity structures do not exist, making exit genuinely impossible. In pluralistic modern societies (secular multi-community contexts), excommunication degrades across perspectives: the target can access alternative identities (Scaffold), continuing members face loosening conformity pressure (Rope shifts toward Tangled Rope), and the ritual itself becomes theatrical (Piton). The analytical observer's false summit (Mountain) — that excommunication is immutable group law — is revealed as such by the constraint's obvious change across historical contexts. This is not a natural law but a contingent institutional arrangement whose coercive power collapses when alternative identity structures become available. The mandatrophy is diagnostic: it shows that institutional exclusion mechanisms are Snare under monopoly conditions and scaffold/piton under pluralistic conditions. The transition between these states is historically observable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_dissolving,
    'Can the identity-locked excommunicated agent dissolve the identity lock through exposure to alternative frameworks, or does the lock persist post-exit?',
    'Longitudinal tracking of ex-members: psychological distress trajectories, identity reconstruction timelines, whether post-exit suppression persists without active institutional enforcement',
    'If lock dissolves: excommunication is mainly structural oppression (high suppression but exit-dependent). If lock persists: excommunication achieves internalized permanent extraction even after institutional mechanism ceases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_dissolving, empirical, 'Whether identity lock persists after excommunication mechanism removal').

omega_variable(
    institutional_vs_social_coercion,
    'Is the coercive power of excommunication exercised by the instituting authority through formal decree, or by the surrounding community through social ostracism?',
    'Comparative analysis of institutional enforcement (ex-communication decrees actively enforced by clergy) vs. community enforcement (social ostracism driven by peer belief in excommunication status). Test via scenarios where institution and community disagree.',
    'If institutional: snare derives from formal authority. If community-driven: snare derives from collective social dynamics; more extractive if community agrees, less if community rejects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_social_coercion, empirical, 'Whether coercion is institutional or community-driven').

omega_variable(
    alternative_identity_accessibility,
    'What fraction of excommunicated individuals can access alternative identity structures (secular community, professional identity, geographic mobility)?',
    'Demographic analysis of excommunicated populations: access to education, employment mobility, secular community infrastructure by time period and region',
    'If accessible (modern secular societies): excommunication drops to Tangled Rope or Scaffold (exit options improve from identity_locked to constrained/mobile). If inaccessible (historical/theocratic contexts): excommunication remains Snare with high suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_identity_accessibility, empirical, 'Accessibility of alternative identity structures for excommunicated agents').

omega_variable(
    suppression_internalization_ratio,
    'What proportion of suppression acting on excommunicated agents is structural (external ostracism) vs. internalized (psychological internalization of unworthiness)?',
    'Clinical/psychological assessment: do excommunicated agents maintain avoidance behaviors post-exit (structural suppression persisting) or override them (suppression externalized)? Trauma symptom persistence?',
    'If mostly structural: suppression ends with community contact cessation. If internalized: suppression is carried by the agent and persists indefinitely, making recidivism and secondary victimization likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ratio, empirical, 'Structural vs. internalized suppression ratio').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(excommunication_social_exclusion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(excomm_tr_t0, excommunication_social_exclusion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(excomm_tr_t3, excommunication_social_exclusion, theater_ratio, 3, 0.5).
narrative_ontology:measurement(excomm_tr_t6, excommunication_social_exclusion, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(excomm_be_t0, excommunication_social_exclusion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(excomm_be_t3, excommunication_social_exclusion, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(excomm_be_t6, excommunication_social_exclusion, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(excommunication_social_exclusion, identity_coordination).
narrative_ontology:affects_constraint(excommunication_social_exclusion, cult_member_binding).
narrative_ontology:affects_constraint(excommunication_social_exclusion, employment_at_will_termination).
narrative_ontology:affects_constraint(excommunication_social_exclusion, asylum_seeker_legal_exclusion).

% DUAL FORMULATION NOTE:
% Excommunication operates through institutional authority + community enforcement. The constraint family includes cult member binding (identity_coordination within closed groups), employment termination (weaker identity lock, stronger structural exit), and asylum exclusion (state authority + community enforcement but lower identity fusion). Each story has different ε reflecting how identity lock and alternative identity accessibility vary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(excommunication_social_exclusion, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
