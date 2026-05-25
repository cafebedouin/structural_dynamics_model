% ============================================================================
% CONSTRAINT STORY: ulysses_chp09
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp09, []).

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
 *   constraint_id: ulysses_chp09
 *   human_readable: The Hamlet Algebra (National Library)
 *   domain: social/religious/philosophical
 *
 * SUMMARY:
 *   In the National Library episode of Joyce's Ulysses (1904), Stephen
 *   Dedalus performs an elaborate theory of Hamlet as biographical allegory,
 *   explicitly framing his intellectual navigation between Scylla and
 *   Charybdis — between Aristotelian dogmatic realism (which treats
 *   authorship as external fact) and Platonic mysticism (which treats art as
 *   disembodied form). The performance occurs in a specific institutional
 *   context: Stephen is a young artist seeking intellectual legitimacy within
 *   Dublin's literary establishment, performing for skeptical listeners
 *   including the librarian, literary figures, and casual auditors. The
 *   constraint is not 'the Hamlet theory itself' but rather 'the structural
 *   requirement that Stephen performs interpretive difficulty as the primary
 *   mechanism of gaining intellectual authority within this institutional
 *   context.' The theory's actual philosophical content is secondary to its
 *   function as a display of interpretive virtuosity. As the performance
 *   continues, the theater ratio increases: early in the recitation, Stephen
 *   offers genuine literary analysis; by the end, the performance has become
 *   almost purely a display of the theorist's conceptual agility, detached
 *   from whether anyone understands or benefits from the content. The
 *   extractiveness similarly increases: the initial theory offers modest
 *   coordination value (bringing attention to neglected biographical patterns
 *   in Hamlet); the mature performance extracts primarily from the audience's
 *   attention and the literary community's endorsement, with decreasing
 *   epistemic return.
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Primary beneficiary (institutional/arbitrage) — gains intellectual authority and social legitimacy through successful performance; can exit to other contexts if this audience rejects him, but has strong incentive to succeed here
 *   - Library Audience: Primary victim (powerless/trapped) — forced to consume the performance without access to underlying philosophical stakes; cannot exit without social cost
 *   - Irish Literary Community: Secondary victim (moderate/constrained) — depends on Stephen's intellectual innovation but also depends on access to literary discourse; constrained by need to accept or reject his authority
 *   - National Library Institution: Structural host (organized/constrained) — provides the venue and enables the performance; sees it as transitional infrastructure for Irish intellectual development
 *   - Modernist Literary Authority: Beneficiary (institutional/arbitrage) — gains prestige from the coordination mechanism (interpretive difficulty as gate-keeper) that Stephen establishes
 *   - Aristotelian/Platonic Philosophy: Victim of degradation (institutional/arbitrage) — cited and invoked but not genuinely engaged; maintains presence through inertia (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp09, 0.52).
domain_priors:suppression_score(ulysses_chp09, 0.48).
domain_priors:theater_ratio(ulysses_chp09, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp09, extractiveness, 0.52).
narrative_ontology:constraint_metric(ulysses_chp09, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ulysses_chp09, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp09, tangled_rope).
narrative_ontology:human_readable(ulysses_chp09, "The Hamlet Algebra (National Library)").
narrative_ontology:topic_domain(ulysses_chp09, "social/religious/philosophical").

domain_priors:requires_active_enforcement(ulysses_chp09).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp09, stephen_dedalus).
narrative_ontology:constraint_beneficiary(ulysses_chp09, literary_modernism).
narrative_ontology:constraint_victim(ulysses_chp09, philosophical_clarity).
narrative_ontology:constraint_victim(ulysses_chp09, audience_epistemic_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LIBRARY AUDIENCE (SNARE) — Trapped listeners cannot exit the performance without social cost; bear the extraction of attention and interpretive labor. Forced to navigate Stephen's Scylla/Charybdis choice without access to the underlying philosophical stakes. Maximum suppression: no alternative venue for literary legitimation, no exit without reputation loss.
constraint_indexing:constraint_classification(ulysses_chp09, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: IRISH LITERARY COMMUNITY (TANGLED ROPE) — Constrained by dependence on Stephen's intellectual innovation for literary advancement, but also constrained by the obscurity he imposes. Benefits from the modernist authority his Hamlet theory generates; bears the cost of having to decode it. Active enforcement required: the community must legitimize this performance as serious criticism despite its performative opacity.
constraint_indexing:constraint_classification(ulysses_chp09, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MODERNIST LITERARY AUTHORITY (ROPE) — Benefits from the coordination that Stephen establishes: by performing interpretive difficulty as intellectual rigor, modernism creates a gate-keeping mechanism that concentrates authority among those who can perform obscurity. Low extraction experienced from this perspective — pure coordination benefit through hierarchical crystallization of aesthetic legitimacy.
constraint_indexing:constraint_classification(ulysses_chp09, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL LIBRARY INSTITUTION (SCAFFOLD) — Provides temporary infrastructure for Stephen's performance; sees the constraint as transitional. The library's role is to host such displays until broader educational reform and philosophical clarity make this particular form of obscurity unnecessary. Has sunset logic: as Irish intellectual institutions mature, the need for this performative theater declines. Theater ratio moderate because the library's actual function (hosting discourse) is genuine, even if the specific performance within it is highly theatrical.
constraint_indexing:constraint_classification(ulysses_chp09, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ARISTOTELIAN/PLATONIC PHILOSOPHY APPARATUS (PITON) — The invocation of Scylla (Aristotelian realism) and Charybdis (Platonic idealism) is substantially degraded theatrical use. Stephen's Hamlet algebra does not genuinely engage these philosophical traditions — it borrows their prestige while avoiding their actual argumentative demands. The constraint maintains this apparatus through inertia: citing Aristotle and Plato seems intellectually rigorous, but the philosophical content has atrophied. Theater ratio 0.68 reflects that the philosophical scaffolding is 40% genuine engagement, 60% performative invocation.
constraint_indexing:constraint_classification(ulysses_chp09, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, all artistic performance navigates between constraining poles: mimesis vs expression, tradition vs innovation, clarity vs mystery. The Scylla/Charybdis structure is not contingent to this specific library moment — it is an irreducible feature of how human creativity functions. However, structural analysis shows this is false naturalization: the constraint is specific to Stephen's institutional position (excluded outsider seeking legitimacy) and the particular moment (modernism establishing itself against Victorian clarity). The analytical observer risks misclassifying a contingent performative strategy as a law of artistic form.
constraint_indexing:constraint_classification(ulysses_chp09, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp09_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp09, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp09, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp09, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp09, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp09_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Stephen extracts substantial intellectual authority and social capital from the performance, converting interpretive virtuosity into status. The extraction is not total because the performance does offer genuine literary insight (biographical patterns in Hamlet are real and underexplored), and some audience members do gain epistemic benefit. The increase over the interval (0.28 → 0.52) reflects the transition from offering real analysis to performing pure virtuosity. Suppression (0.48): Moderate. Significant barriers exist: the audience cannot easily exit without reputational cost; alternative philosophical frameworks are suppressed (only Aristotle/Plato are permitted as reference points); the specific technical language creates a gate preventing easy comprehension. But suppression is not total — the National Library is a public institution with some openness, and motivated listeners can follow the argument. Theater ratio (0.68): High-moderate. The performance is substantially performative: the philosophical apparatus is borrowed for prestige rather than developed substantively; the interpretive moves are designed to display the theorist's virtuosity rather than to illuminate the text. However, the early portions of the theory contain genuine literary analysis, so theater is not absolute. The increase over the interval (0.42 → 0.68) reflects degradation as the performance continues: early sections offer real literary patterns; later sections become increasingly abstract and performative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence. Stephen experiences the constraint as coordination (Rope) — he is solving the problem of establishing literary legitimacy through intellectual performance, and the Scylla/Charybdis framework provides genuine structure for his analysis. The modernist literary authority also sees coordination (Rope) — Stephen's performance establishes the mechanism by which interpretive difficulty becomes a gate-keeping authority, benefiting the modernist project. But the library audience experiences the constraint as extraction (Snare) — their attention is consumed, their comprehension is suppressed, and they have no exit. The philosophical tradition experiences the constraint as degradation (Piton) — Aristotle and Plato are invoked for prestige but not substantively engaged. The analytical observer risks seeing a natural law (Mountain) — the Scylla/Charybdis navigation as inherent to artistic creation — but structural analysis reveals this as false naturalization of a specific institutional strategy.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each perspective flows from structural position. Stephen (institutional/arbitrage) is a beneficiary with exit options — he experiences low d (derived from beneficiary + arbitrage → d ≈ 0.05-0.15, f(d) ≈ -0.12 to -0.01) producing negative effective extraction, which matches his experience: the constraint benefits him. The library audience (powerless/trapped) bears costs with no exit — high d (0.95, f(d) ≈ 1.42) producing high chi, which matches the snare experience: they are extracted from. The literary community (moderate/constrained) has some but limited exit options — medium d (0.50-0.65) producing moderate chi and the tangled rope experience: they benefit from modernist authority but are constrained by dependence on Stephen's intellectual performance. The philosophical tradition (institutional/arbitrage but degraded) experiences the constraint as institutional inertia: cited for prestige but not engaged substantively. The analytical observer risks d = 0.72 (analytical perspective canonical fallback, f(d) ≈ 1.15) which would suggest moderate extraction, but the structural data shows the constraint is contingent to this specific moment and institutional context, not a universal law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the coordination vs extraction ambiguity by demonstrating that Stephen's performance contains BOTH. The coordination function is real: bringing attention to biographical patterns in Hamlet, establishing a structure for interpreting artistic creation through biographical detail, creating a framework (Scylla/Charybdis) that organizes competing philosophical approaches. The extraction function is equally real: the difficulty of the performance suppresses audience agency; the philosophical citations are decorative rather than substantive; the primary benefit flows to Stephen's social status, not to increased literary understanding. The theater ratio (0.68) indicates that the performative component exceeds the functional component, but not absolutely — the constraint is genuinely tangled, not pure extraction masquerading as coordination. The measurements over time show degradation: the theater ratio increases and the base extractiveness increases, suggesting that as Stephen warms to his topic, the coordination function decays and the extraction function becomes more prominent. This trajectory is exactly what Mandatrophy detection requires: a constraint that begins with meaningful coordination but drifts toward pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    philosophical_authenticity_threshold,
    'At what point does invocation of philosophical tradition become substantive engagement vs pure theatrical reference?',
    'Comparative analysis of Stephen''s Hamlet argument against primary Aristotelian and Platonic texts; measurement of propositional content vs decorative citation',
    'If threshold low (citation counts): constraint appears as rope/coordination. If threshold high (rigorous engagement required): constraint appears as snare/extraction and piton/degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(philosophical_authenticity_threshold, conceptual, 'Threshold for philosophical citation to count as substantive engagement').

omega_variable(
    modernist_legitimacy_mechanism,
    'Does interpretive difficulty itself constitute a form of intellectual authority, or does it merely mask lack of clarity?',
    'Historical analysis of whether modernist obscurity correlated with philosophical breakthrough vs with status anxiety; examination of how much modernist theory later required simplification to survive',
    'If difficulty = authority: constraint is rope/coordination creating genuine epistemic hierarchy. If difficulty = mask: constraint is snare/extraction using gate-keeping to extract intellectual deference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_legitimacy_mechanism, empirical, 'Whether modernist interpretive difficulty correlates with genuine authority').

omega_variable(
    audience_epistemic_agency_recovery,
    'Can audience members reconstruct the philosophical stakes of the Hamlet algebra from the performance itself, or only through external commentary?',
    'Empirical study: test comprehension of the theory immediately after hearing it vs after reading secondary sources; measure how much audience interpretation depends on external scaffolding',
    'If self-contained: audience captures genuine coordination benefit. If dependent on external sources: constraint operates as suppression mechanism (high suppression, snare from audience perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audience_epistemic_agency_recovery, empirical, 'Whether the performance itself provides epistemic agency or requires external decoding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp09, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hamletlib_tr_t0, ulysses_chp09, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hamletlib_tr_t30, ulysses_chp09, theater_ratio, 30, 0.58).
narrative_ontology:measurement(hamletlib_tr_t60, ulysses_chp09, theater_ratio, 60, 0.68).

% Extraction over time
narrative_ontology:measurement(hamletlib_be_t0, ulysses_chp09, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hamletlib_be_t30, ulysses_chp09, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(hamletlib_be_t60, ulysses_chp09, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp09, information_standard).
narrative_ontology:affects_constraint(ulysses_chp09, scylla_charybdis_navigation).
narrative_ontology:affects_constraint(ulysses_chp09, modernist_authority_formation).

% DUAL FORMULATION NOTE:
% The Hamlet Algebra is downstream of two distinct structural constraints: (1) the philosophical problem of navigating between Aristotelian realism and Platonic idealism (which could be a Mountain if the navigation itself is inherent to philosophical thought, or a Rope if it's a specific coordination problem); (2) the social problem of establishing intellectual authority within Dublin's literary community (which is contingent and extractive). This story treats the composite constraint. Alternative decomposition would separate the philosophical claim from the social performance, giving two distinct constraint stories with different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp09, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
