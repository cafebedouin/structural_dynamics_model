% ============================================================================
% CONSTRAINT STORY: selective_retention_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_selective_retention_necessity, []).

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
 *   constraint_id: selective_retention_necessity
 *   human_readable: Selective Retention Necessity in Thinned Social Environments
 *   domain: social_philosophy/trust_theory/relational_ethics
 *
 * SUMMARY:
 *   In environments where categorical trust structures have eroded — extended
 *   family networks are geographically dispersed, stable institutional
 *   credentials no longer guarantee reliability, rooted community membership
 *   no longer confers automatic trust — the burden of trust assessment shifts
 *   entirely to the individual level. Agents must allocate scarce attention
 *   and binding commitment to specific people, built incrementally through
 *   accumulated observation of reliability across repeated interactions. This
 *   constraint exhibits all six DR types depending on the observer's
 *   structural position. For newcomers without relational history, selective
 *   retention is a snare: they bear the full cost of the reputation-building
 *   lag while others benefit from pre-existing trust relationships. For
 *   moderately-embedded agents, it is tangled rope: genuine coordination
 *   benefit (identifying reliable partners) coupled with extraction risk
 *   (exclusion from densely-embedded networks). For reliably-established
 *   individuals, it is rope: a pure coordination mechanism for filtering and
 *   scaling trust. For institutions that intermediate selective retention
 *   (platforms, professional licensing, credential systems), it is tangled
 *   rope: genuine gatekeeper function coupled with rent extraction. For
 *   legacy categorical trust systems (alumni networks, professional
 *   associations, family-based credentials), it persists as piton:
 *   performative theater in contexts where the actual verification work has
 *   migrated to selective retention. For the analytical observer at
 *   civilizational scale, it risks appearing as mountain: an immutable
 *   cognitive law. But the structural data reveals it as a false summit —
 *   selective retention necessity is contingent on prior categorical
 *   collapse, not inevitable.
 *
 * KEY AGENTS:
 *   - Trust-Seeker Without History: Powerless/trapped (biographical) — newcomers bearing full cost of reputation lag; no alternative access pathway to trust-dependent resources
 *   - Peripheral Relationship Agent: Moderate/constrained (biographical) — partially embedded individuals experiencing mixed coordination and extraction; constrained exit options
 *   - Reliable Individual: Institutional/arbitrage (generational) — agents with proven track record leveraging selective retention relationships for resource access and influence
 *   - Community Institution or Platform: Organized/constrained (generational) — churches, online reputation systems, professional licensing boards serving as trust gatekeepers while extracting rents
 *   - Legacy Categorical Trust System: Institutional/arbitrage (civilizational) — extended family, institutional credentials, stable employment acting as inert theater maintaining membership performance
 *   - Analytical Observer: Analytical/analytical (civilizational) — risks naturalizing contingent institutional arrangements as cognitive laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(selective_retention_necessity, 0.52).
domain_priors:suppression_score(selective_retention_necessity, 0.48).
domain_priors:theater_ratio(selective_retention_necessity, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(selective_retention_necessity, extractiveness, 0.52).
narrative_ontology:constraint_metric(selective_retention_necessity, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(selective_retention_necessity, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(selective_retention_necessity, tangled_rope).
narrative_ontology:human_readable(selective_retention_necessity, "Selective Retention Necessity in Thinned Social Environments").
narrative_ontology:topic_domain(selective_retention_necessity, "social_philosophy/trust_theory/relational_ethics").

domain_priors:requires_active_enforcement(selective_retention_necessity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(selective_retention_necessity, reliable_individuals).
narrative_ontology:constraint_beneficiary(selective_retention_necessity, attention_gatekeepers).
narrative_ontology:constraint_victim(selective_retention_necessity, trust_seekers).
narrative_ontology:constraint_victim(selective_retention_necessity, resource_constrained_agents).
narrative_ontology:constraint_victim(selective_retention_necessity, anomalous_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRUST-SEEKER WITHOUT HISTORY (SNARE) — A newcomer to a thinned environment has no accumulated observation history. Cannot access trust-dependent resources (credit, employment referrals, community support) without building selective retention from zero. Trapped in biographical time because reputation-building takes years. No alternative pathway to trust access except time investment that may never complete. Maximum extraction: bears full cost of the lag while others benefit from pre-existing selective retention relationships.
constraint_indexing:constraint_classification(selective_retention_necessity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PERIPHERAL RELATIONSHIP (TANGLED ROPE) — An agent with partial history (knows some people in the network but not densely embedded) experiences both coordination and extraction. The constraint enables them to form deeper selective retention bonds with proven individuals (coordination benefit), but also constrains their access to resources controlled by densely-embedded elites (extraction cost). Exit is constrained: leaving the geographic region resets selective retention to zero; staying means accepting peripheral status.
constraint_indexing:constraint_classification(selective_retention_necessity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIABLE INDIVIDUAL / INSTITUTIONAL VIEW (ROPE) — An agent with proven track record and accumulated selective retention relationships experiences the constraint as pure coordination. They can arbitrage between different social circles, leveraging their reliability reputation for access to resources and influence. The constraint solves their coordination problem: reliably identifying other reliable people and building trust at scale. No extraction experienced; only the coordination benefit of selective retention as a filtering mechanism.
constraint_indexing:constraint_classification(selective_retention_necessity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMUNITY INSTITUTION / PLATFORM (TANGLED ROPE) — Institutions that facilitate selective retention (from churches to online reputation systems to professional licensing boards) benefit from managing the trust bottleneck — they become gatekeepers of credential validation and relationship brokering. They also provide genuine coordination function by reducing verification costs. But the gatekeeper power enables extraction: setting barriers to entry, capturing rents from credential gatekeeping, controlling access to trust networks. Active enforcement required to maintain their role as arbiter of who counts as 'reliable.'
constraint_indexing:constraint_classification(selective_retention_necessity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CATEGORICAL TRUST SYSTEM (PITON) — Extended family, institutional credentials, stable employment contracts, and rooted community membership once provided categorical trust (all family members trusted by default, all card-carrying members accepted). These structures persist theatrically in some contexts (alumni networks, professional associations, denominational institutions) but have lost their primary function as trust mechanisms. The theater remains: formality of membership, rituals of belonging, performance of reliability based on category. But the actual filtering work has migrated to selective retention. The old system is inert — maintained for signaling but not functionally generating trust.
constraint_indexing:constraint_classification(selective_retention_necessity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, selective retention is an inescapable cognitive limit: human attention is finite, and trust verification requires accumulated observation. Under categorical collapse (when extended family networks, stable institutions, and rooted communities can no longer provide categorical guarantees), selective retention becomes the only remaining mechanism for allocating trust. This appears as a natural law of social coordination under thinning conditions. However, this perspective risks naturalizing what is actually a contingent institutional arrangement — the 'necessity' of selective retention only emerges when categorical structures have eroded. The constraint is not inevitable; it is the outcome of prior structural choices.
constraint_indexing:constraint_classification(selective_retention_necessity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(selective_retention_necessity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(selective_retention_necessity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(selective_retention_necessity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(selective_retention_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(selective_retention_necessity, TR),
    TR >= 0.70.

:- end_tests(selective_retention_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint creates asymmetric costs: newcomers and resource-constrained agents pay high reputation-building costs while reliably-established individuals and institutions capture benefits. However, the extraction is not absolute (0.72+) because selective retention also solves a genuine coordination problem — under categorical collapse, there is no better mechanism for rapid trust assessment. The value reflects the hybrid nature: necessary coordination solution that simultaneously enables extraction. Suppression (0.48): Moderate. The constraint is not imposed by overt coercion but by structural necessity — in thinned environments, categorical trust no longer functions, and selective retention is the adaptive response. But suppression is real: agents cannot exit the requirement to build reputation one relationship at a time; alternatives (categorical trust structures) have eroded; the lag period creates real barriers. Theater ratio (0.35): Low. Selective retention is functionally efficient — accumulated observation of reliability is the most accurate trust mechanism available under categorical collapse. The theater content is minimal: no performative ritual is required for the mechanism itself to work, though institutional intermediaries (platforms, licensing bodies) add performative layers on top.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_collapse_irreversibility,
    'Is the erosion of categorical trust structures (extended family, stable institutions, rooted communities) permanent or cyclical?',
    'Historical analysis of trust system evolution across multiple societies; identification of conditions under which categorical structures re-stabilize or re-form',
    'If irreversible: selective retention necessity is a permanent feature of modernity; mountain classification approaches validity. If cyclical: selective retention is a contingent adaptive response to a temporary structural condition; tangled rope classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_collapse_irreversibility, empirical, 'Whether categorical trust collapse is permanent or reversible').

omega_variable(
    attention_sufficiency_for_scaling,
    'Can selective retention mechanisms scale to the trust demands of large-scale coordination (nation-states, global supply chains, mass markets), or does it collapse under scale?',
    'Empirical measurement of trust-dependent transaction completion rates at different network scales; analysis of failure modes when selective retention bottleneck is exceeded',
    'If scalable: selective retention plus delegated verification produces stable large-scale trust; rope classification from institutional perspective is accurate. If hits wall at specific scale: extraction accelerates at scale as agents compete for attention; snare classification from powerless perspective intensifies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attention_sufficiency_for_scaling, empirical, 'Whether selective retention can support large-scale coordination').

omega_variable(
    reliably_observable_reliability,
    'What counts as ''accumulated observation of reliability''? How do agents distinguish genuine reliability from reputation management and strategic performance?',
    'Comparative analysis of agents who have verified reliability through direct observation vs those who have adopted performance tactics; identification of divergence between observed behavior and actual reliability under novel conditions',
    'If direct observation is effective: selective retention is a functional filtering mechanism (rope). If observation is gamed: selective retention becomes a performance extraction mechanism where reliability is theater (piton or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliably_observable_reliability, conceptual, 'Whether accumulated observation reliably identifies genuine reliability').

omega_variable(
    new_entrant_economic_viability,
    'Can a new entrant to a thinned social environment afford the time and resource investment required to build selective retention from zero, or does the lag period create economic exclusion?',
    'Longitudinal economic tracking of newcomers in thinned vs categorical-trust-rich environments; analysis of access to employment, credit, housing during the reputation-building lag',
    'If affordable: selective retention is a temporary inconvenience; tangled rope classification from moderate perspective. If creates sustained exclusion: selective retention becomes an extractive barrier to entry; snare classification from powerless perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(new_entrant_economic_viability, empirical, 'Economic feasibility of reputation-building lag for newcomers').

omega_variable(
    identity_locked_false_necessity,
    'Do agents in thinned environments internalize selective retention as ''just how trust works'' (identity-locked constraint) rather than recognizing it as a contingent adaptation to categorical collapse?',
    'Cognitive framing analysis: comparison of agents'' meta-narratives about trust necessity; identification of whether they perceive selective retention as inherent vs constructed; longitudinal tracking of belief shifts when categorical structures partially re-stabilize',
    'If widely identity-locked: the constraint persists even if categorical structures partially re-emerge; extraction mechanism becomes self-sustaining through internalization. If agents recognize contingency: selective retention can be dismantled if trust-enabling institutions are reconstructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_false_necessity, conceptual, 'Whether agents perceive selective retention as natural necessity or contingent adaptation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(selective_retention_necessity, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(srn_tr_t0, selective_retention_necessity, theater_ratio, 0, 0.25).
narrative_ontology:measurement(srn_tr_t2, selective_retention_necessity, theater_ratio, 2, 0.3).
narrative_ontology:measurement(srn_tr_t5, selective_retention_necessity, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(srn_be_t0, selective_retention_necessity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(srn_be_t2, selective_retention_necessity, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(srn_be_t5, selective_retention_necessity, base_extractiveness, 5, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(srn_su_t0, selective_retention_necessity, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(srn_su_t2, selective_retention_necessity, suppression_requirement, 2, 0.45).
narrative_ontology:measurement(srn_su_t5, selective_retention_necessity, suppression_requirement, 5, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(selective_retention_necessity, attachment_coordination).
narrative_ontology:affects_constraint(selective_retention_necessity, categorical_trust_erosion).
narrative_ontology:affects_constraint(selective_retention_necessity, attention_allocation_scarcity).

% DUAL FORMULATION NOTE:
% Selective retention necessity decomposes into three structurally distinct constraints: (1) The structural reality of categorical collapse (upstream) — why categorical trust no longer functions in thinned environments; (2) Selective retention as adaptive mechanism (this story) — how agents allocate trust under scarcity conditions; (3) Gatekeeping extraction (downstream) — how institutions profit from managing the selective retention bottleneck. Each has distinct ε. This story treats selective retention as the coordinating mechanism while acknowledging its extractive consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(selective_retention_necessity, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
