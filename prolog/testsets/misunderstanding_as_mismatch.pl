% ============================================================================
% CONSTRAINT STORY: misunderstanding_as_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_misunderstanding_as_mismatch, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: misunderstanding_as_mismatch
 *   human_readable: Social Pressure for Worldview Assimilation
 *   domain: social/psychological
 *
 * SUMMARY:
 *   Social pressure for worldview assimilation operates as a hybrid
 *   coordination-extraction constraint in which groups enforce alignment
 *   around dominant belief systems through a combination of legitimate
 *   coordination needs (shared meaning-making, group identity, epistemic
 *   cohesion) and coercive mechanisms (ostracism, selective exclusion, social
 *   punishment for dissent). The constraint exhibits all six classification
 *   types from different structural positions: appearing as an immutable law
 *   of group formation (mountain) to analytical observers, as a degraded
 *   ritual maintained through performative conformity (piton) to
 *   institutional actors, as a temporary problem with a generational sunset
 *   (scaffold) to pluralism advocates, as mixed coordination and extraction
 *   (tangled_rope) to skeptical members, as pure extraction (snare) to
 *   epistemic minorities, and as functional coordination (rope) to worldview
 *   authorities. The increasing theater ratio (0.55 → 0.68) reflects that
 *   assimilation pressure has become progressively more performative as
 *   explicit enforcement mechanisms have become socially costly — the
 *   constraint now operates primarily through implicit norm signaling and
 *   public-private belief divergence rather than through direct coercion. The
 *   extractiveness increase (0.35 → 0.52) tracks the proliferation of
 *   contexts in which worldview alignment is demanded (professional
 *   environments, social media, educational institutions) rather than an
 *   increase in penalty severity per context.
 *
 * KEY AGENTS:
 *   - Epistemic Minorities: Primary victims (powerless/trapped) — individual dissenters bearing maximum extraction cost; no exit options; subject to normalization of hostility
 *   - Dominant Worldview Holders: Primary beneficiaries (institutional/arbitrage) — authorities and enforcer of group beliefs; capture compliance benefit and social authority; arbitrage options allow repositioning
 *   - Skeptical Members: Secondary victims (moderate/constrained) — residual dissenters embedded in group; receive coordination benefits alongside extraction pressure; navigate through selective conformity
 *   - Intellectual Pluralism Movement: Organized agents (organized/constrained) — advocates for epistemic diversity building norms around disagreement tolerance; constrained but with exit pathway and generational sunset
 *   - Social Conformity Mechanism: Institutional machinery (institutional/arbitrage) — the apparatus of gossip, selective inclusion, performative agreement that maintains pressure; largely theatrical
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent laws of human sociality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(misunderstanding_as_mismatch, 0.52).
domain_priors:suppression_score(misunderstanding_as_mismatch, 0.65).
domain_priors:theater_ratio(misunderstanding_as_mismatch, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, extractiveness, 0.52).
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(misunderstanding_as_mismatch, tangled_rope).
narrative_ontology:human_readable(misunderstanding_as_mismatch, "Social Pressure for Worldview Assimilation").
narrative_ontology:topic_domain(misunderstanding_as_mismatch, "social/psychological").

domain_priors:requires_active_enforcement(misunderstanding_as_mismatch).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(misunderstanding_as_mismatch, dominant_worldview_holders).
narrative_ontology:constraint_victim(misunderstanding_as_mismatch, epistemic_minorities).
narrative_ontology:constraint_victim(misunderstanding_as_mismatch, identity_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC MINORITY (SNARE) — Individual holding minoritized beliefs faces maximum social extraction. Exit options (leaving the group, expressing dissent, maintaining private doubts) all carry severe penalties: social ostracism, professional consequence, psychological isolation, loss of belonging. No escape route; constraint enforced through normalization of hostility toward deviance. Minimum agency; maximum coercion.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SKEPTICAL MINORITY MEMBER (TANGLED ROPE) — Agent with residual doubts but embedded in group receives mixed extraction and coordination benefit. The group provides belonging, shared identity, and social safety (coordination function). But expressing doubt triggers social correction, subtle ostracism, exclusion from inner discussions (extraction function). Constrained exit: can navigate through selective conformity but cannot fully escape without cost. Benefits from group coordination while bearing extraction pressure.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WORLDVIEW AUTHORITY (ROPE) — Institutional holder or enforcer of dominant worldview experiences the constraint as a coordination mechanism: teaching shared beliefs, maintaining group cohesion, ensuring epistemic alignment. The authority has arbitrage options (can shift domains, reposition beliefs, exit to different groups). Extraction flows toward this agent through compliance and conformity. Net beneficiary; sees constraint as functional coordination.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTELLECTUAL PLURALISM MOVEMENT (SCAFFOLD) — Organized advocates for epistemic diversity and open disagreement see worldview assimilation pressure as a temporary constraint with a sunset. Norm-building around intellectual humility, steel-manning opposing views, institutional safeguards for dissent (protected speech, academic freedom, explicit diversity language) represent exit pathways that are building. Constrained exit: movement has agency but faces institutional resistance. Sunset timeline: generational shift in schools and workplaces toward normalized disagreement (15-30 years).
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SOCIAL CONFORMITY MECHANISM (PITON) — The machinery of social pressure (gossip, informal shunning, selective inclusion) persists largely through institutional inertia and theatrical maintenance (ceremonial displays of correct thinking, performative agreement). Theater ratio (0.68) reflects that much assimilation pressure is performative: public displays of conformity serve more as group identity markers than as genuine worldview engineering. The mechanism would collapse if agents stopped performing belief consensus. Maintained through ritual, not force.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal, civilizational perspective, some degree of worldview convergence may appear inherent to group formation: groups require shared meaning-making to function, and deviation from group meaning disrupts coordination. The pressure toward assimilation might be seen as a natural law of social cohesion — inevitable, unchangeable, emerging from the structure of human group formation itself. However, this perspective risks naturalizing what is actually a contingent institutional arrangement (the degree and harshness of assimilation pressure varies dramatically across cultures, historical periods, and institutional contexts).
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(misunderstanding_as_mismatch_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(misunderstanding_as_mismatch, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(misunderstanding_as_mismatch, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(misunderstanding_as_mismatch, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(misunderstanding_as_mismatch, TR),
    TR >= 0.70.

:- end_tests(misunderstanding_as_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint captures real benefits for worldview authorities (compliance, reduced cognitive friction, group coherence) at costs to minorities (psychological isolation, self-censorship, constrained self-expression). The value reflects the breadth of contexts in which assimilation pressure operates (professional, educational, social, familial) and the accumulation of penalties across contexts. Suppression (0.65): High. Exit options are genuinely constrained — leaving a group often means losing social belonging, professional networks, identity anchor, and access to a shared epistemic framework. The harshness of informal penalties (social ostracism, gossip, selective exclusion) creates significant barriers to maintaining private dissent or overt disagreement. Suppression is not maximal (0.65 not 0.85+) because alternative communities exist and explicit legal protections for speech exist, even if accessing them is costly. Theater ratio (0.68): Moderate-high. The constraint operates increasingly through performative displays of belief agreement (public statements of correct thinking, curated social media identity) rather than through direct enforcement or authentic belief convergence. The rise of public/private belief divergence suggests the machinery is becoming more theatrical — sustained by maintaining visible consensus ritual rather than by genuine worldview engineering.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates dramatic perspectival divergence across structural positions. The epistemic minority sees a snare (pure extraction, no exit). The skeptical member sees tangled rope (mixed benefit and cost, constrained exit). The authority sees rope (legitimate coordination, net benefit). The pluralism movement sees scaffold (temporary problem with a generational exit path). The institutional apparatus sees piton (performative ritual, maintained through theater). The analytical observer risks mountain (naturalizing contingent enforcement as inherent law). Each classification is structurally grounded in that agent's exit options, power level, and relationship to the extraction flow. The perspectival gap reveals that 'worldview assimilation' is not a single constraint type but a presheaf over observation contexts — the same social pressure appears structurally different depending on the observer's position in the enforcement hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective derive from structural position relative to the extraction flow. Epistemic minorities with trapped exit experience d ≈ 0.95 (full targets): extraction flows directly from them. Skeptical members with constrained exit experience d ≈ 0.55-0.65 (mixed): they bear extraction but receive coordination benefits and some agency. Worldview authorities with arbitrage exit experience d ≈ 0.05-0.15 (full beneficiaries): extraction flows toward them. The scaffold perspective (organized/constrained) experiences d ≈ 0.45 (symmetric): the movement bears pressure but has agency and sees an exit path. The piton perspective (institutional/arbitrage) experiences d ≈ 0.10 (beneficiary): institutional enforcer captures conformity benefit. The analytical observer (analytical/analytical) experiences d ≈ 0.72 (observer): external position but risks bias toward naturalizing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by disambiguating between the snare (what minorities experience) and the rope (what authorities experience) through explicit beneficiary/victim declaration and exit_options differentiation. Without this decomposition, observers might collapse the constraint into a single type — either naturalizing it as an immutable feature of human group formation (false mountain) or dismissing it as minor coordination overhead (false rope). The tangled_rope classification for embedded members prevents the constraint from collapsing into a snare-only view: some agents genuinely benefit from group epistemic alignment (identity, belonging, shared meaning) while bearing extraction costs. The scaffold perspective prevents naturalizing assimilation as inevitable: organized dissent around pluralism norms and epistemic humility is building exit pathways. The piton classification reveals that increased theater ratio (performative conformity) indicates degradation rather than functional constraint maintenance — when assimilation pressure must sustain itself through ritual rather than through structural enforcement, the constraint is in transition. The mandatrophy is resolved by showing that all six types are structurally legitimate readings: there is no 'true' type hiding behind observational ambiguity. The constraint IS a tangled rope (mixed coordination and extraction at base), but appears differently depending on whether you measure from the minority position (snare), the authority position (rope), the embedded position (tangled_rope), the movement position (scaffold), the institutional machinery position (piton), or the analytical distance (mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_coordination_boundary,
    'At what point does social pressure for alignment transition from functional group coordination to coercive worldview assimilation?',
    'Comparative analysis across groups: measurement of dissent tolerance, penalty severity for non-conformity, and diversity of expressed beliefs within group structures; ethnographic documentation of explicit vs implicit enforcement mechanisms',
    'If boundary is permeable and context-dependent: assimilation pressure is primarily contingent institutional design (suggests scaffold/tangled_rope). If boundary is sharp and universal: some core pressure is inherent to group formation (suggests mountain or rope with high coordination floors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_coordination_boundary, empirical, 'Boundary between functional coordination and coercive assimilation').

omega_variable(
    epistemic_minority_cost_measurement,
    'What are the actual psychological, social, and economic costs borne by individuals who maintain minoritized beliefs within groups?',
    'Longitudinal tracking of mental health, social connection, career outcomes for dissenters vs conformists within same groups; measurement of explicit vs implicit penalties; analysis of selective disclosure patterns (evidence of cognitive load from belief management)',
    'If costs are severe and systematic: snare classification confirmed for minorities. If costs are mild and mediated by individual choice: rope or tangled_rope more accurate; extraction overestimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_minority_cost_measurement, empirical, 'Actual costs borne by belief dissenters within groups').

omega_variable(
    alternative_belonging_accessibility,
    'How accessible are alternative groups or communities that validate minoritized beliefs? Does the trapped exit classification hold when alternatives exist?',
    'Network analysis of belief-community availability; documentation of switching costs (geographic, economic, social capital) for members seeking to migrate to alternative groups; comparison of exit difficulty across different epistemic domains',
    'If alternatives are genuinely inaccessible: trapped classification confirmed. If alternatives are available but costly: exit_options should be ''constrained'' not ''trapped'', changing classification from snare to tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_belonging_accessibility, empirical, 'Accessibility of alternative communities for belief minorities').

omega_variable(
    worldview_malleability_assumption,
    'Is the constraint modeling genuine pressure to shift internal worldviews or pressure to perform conformity while maintaining private beliefs?',
    'Cognitive science analysis of belief change vs public compliance; measurement of private/public belief divergence in conformist populations; analysis of whether social pressure produces actual belief shifts or behavioral compliance only',
    'If constraint requires actual belief change: assimilation is deep, structural, and more extractive. If constraint tolerates private dissent with public conformity: extraction is primarily behavioral, theater is higher, and constraint is more like a performance mandate than a belief mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worldview_malleability_assumption, conceptual, 'Whether constraint targets internal beliefs or external performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(misunderstanding_as_mismatch, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mismatch_tr_t0, misunderstanding_as_mismatch, theater_ratio, 0, 0.55).
narrative_ontology:measurement(mismatch_tr_t5, misunderstanding_as_mismatch, theater_ratio, 5, 0.62).
narrative_ontology:measurement(mismatch_tr_t10, misunderstanding_as_mismatch, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(mismatch_be_t0, misunderstanding_as_mismatch, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mismatch_be_t5, misunderstanding_as_mismatch, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mismatch_be_t10, misunderstanding_as_mismatch, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(misunderstanding_as_mismatch, information_standard).
narrative_ontology:affects_constraint(misunderstanding_as_mismatch, moral_status_consensus_enforcement).
narrative_ontology:affects_constraint(misunderstanding_as_mismatch, epistemic_authority_gatekeeping).
narrative_ontology:affects_constraint(misunderstanding_as_mismatch, identity_driven_belief_clustering).

% DUAL FORMULATION NOTE:
% This constraint represents the coordination side of epistemic enforcement. Upstream constraints (epistemic_authority_gatekeeping, moral_status_consensus_enforcement) establish what beliefs are enforced; this constraint models the social pressure mechanism that performs enforcement. Downstream constraints (identity_driven_belief_clustering) describe how assimilation pressure shapes belief network topology. All three are structurally distinct but form a constraint family: assimilation pressure would be inert without upstream authority structures and produces measurable belief clustering effects downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
