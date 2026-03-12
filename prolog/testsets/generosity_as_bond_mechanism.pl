% ============================================================================
% CONSTRAINT STORY: generosity_as_bond_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generosity_as_bond_mechanism, []).

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
 *   constraint_id: generosity_as_bond_mechanism
 *   human_readable: Generosity as Bond Formation Mechanism
 *   domain: moral_philosophy/social_psychology/economic_anthropology
 *
 * SUMMARY:
 *   The generosity-as-bond-mechanism constraint captures the empirical
 *   finding that attachment formation often follows acts of giving rather
 *   than preceding them as motivation. Longitudinal studies of caregiver
 *   attachment (particularly adoptive parents and alloparents) show that
 *   emotional bonds strengthen through caregiving actions, not just through
 *   pre-existing affection. Neurochemical studies reveal oxytocin release
 *   during giving acts, suggesting a biological coordination mechanism. This
 *   constraint is downstream of temporal_asymmetry_of_obligation (the finding
 *   that obligations created by gifts are temporally asymmetric — the giver
 *   feels obligation to continue giving before the recipient feels obligation
 *   to reciprocate). The generosity mechanism solves a fundamental
 *   coordination problem: how do social species form trust bonds with
 *   non-kin? The answer appears to be through voluntary acts of giving that
 *   trigger neurochemical attachment responses. This is a low-extraction
 *   coordination mechanism — the giver benefits from the bond formed, the
 *   recipient benefits from both the gift and the bond, and the social
 *   network benefits from increased trust density.
 *
 * KEY AGENTS:
 *   - Giver Agent: Primary beneficiary (moderate/mobile) — forms attachment through giving; experiences neurochemical reward and social bond as genuine benefits
 *   - Recipient Agent: Primary beneficiary (moderate/mobile) — receives material benefit and relational bond; can decline or reciprocate
 *   - Social Network: Beneficiary (organized/mobile) — communities and kinship networks benefit from trust formation and social cohesion
 *   - Cultural Institution: Beneficiary (institutional/arbitrage) — religious and cultural institutions that codify generosity norms benefit from the social cohesion the mechanism produces
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the mechanism as a coordination adaptation solving trust formation in social species
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generosity_as_bond_mechanism, 0.18).
domain_priors:suppression_score(generosity_as_bond_mechanism, 0.12).
domain_priors:theater_ratio(generosity_as_bond_mechanism, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generosity_as_bond_mechanism, extractiveness, 0.18).
narrative_ontology:constraint_metric(generosity_as_bond_mechanism, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(generosity_as_bond_mechanism, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generosity_as_bond_mechanism, rope).
narrative_ontology:human_readable(generosity_as_bond_mechanism, "Generosity as Bond Formation Mechanism").
narrative_ontology:topic_domain(generosity_as_bond_mechanism, "moral_philosophy/social_psychology/economic_anthropology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generosity_as_bond_mechanism, giver_agent).
narrative_ontology:constraint_beneficiary(generosity_as_bond_mechanism, recipient_agent).
narrative_ontology:constraint_beneficiary(generosity_as_bond_mechanism, social_network).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE GIVER (ROPE) — Experiences acts of giving as attachment-forming. The constraint coordinates bond formation through voluntary action. Mobile exit (can choose not to give) with low extraction — the neurochemical reward and social bond are genuine benefits, not costs disguised as benefits.
constraint_indexing:constraint_classification(generosity_as_bond_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: THE RECIPIENT (ROPE) — Receives both material benefit and relational bond. The constraint coordinates mutual attachment formation. Mobile exit (can decline gifts or reciprocate) with low extraction — the bond formed is a coordination benefit, not an obligation trap.
constraint_indexing:constraint_classification(generosity_as_bond_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE SOCIAL NETWORK (ROPE) — Observes generosity norms as coordination mechanisms that build social capital and trust networks. The constraint solves the collective action problem of relationship formation. Organized agents (communities, kinship networks) see low extraction — the mechanism produces genuine social cohesion.
constraint_indexing:constraint_classification(generosity_as_bond_mechanism, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE CULTURAL INSTITUTION (ROPE) — Cultural and religious institutions that codify generosity norms (gift-giving rituals, charitable obligations, hospitality codes) experience the constraint as coordination infrastructure. Arbitrage exit (can adopt or modify norms) with minimal extraction — the institutions benefit from the social cohesion the mechanism produces.
constraint_indexing:constraint_classification(generosity_as_bond_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the constraint represents a low-extraction coordination mechanism that solves the fundamental problem of trust formation in social species. The neurochemical architecture (oxytocin release during giving) and the temporal structure (attachment follows action rather than preceding it) are coordination adaptations, not extraction mechanisms.
constraint_indexing:constraint_classification(generosity_as_bond_mechanism, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generosity_as_bond_mechanism_tests).
:- end_tests(generosity_as_bond_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint involves minimal extraction — the giver's costs (time, resources given) are offset by neurochemical reward and bond formation. The recipient receives net benefit. The slight extraction (0.18 rather than 0.05) reflects that giving does involve real resource transfer and opportunity cost, but the coordination benefit dominates. Suppression (0.12): Very low. Agents have high mobility — giving is voluntary, receiving can be declined, and cultural norms around generosity vary widely. The low suppression reflects that this is a coordination mechanism, not a coercive obligation. Theater ratio (0.25): Low. Most giving behavior is functional — it genuinely forms bonds and coordinates relationships. Some theater exists (performative charity, virtue signaling) but is a minority of the phenomenon. The slight increase over the interval (0.20 → 0.25) reflects increasing social media performativity around giving, but the core mechanism remains functional.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all agents see rope. The uniformity is diagnostic: when a constraint classifies as rope from all perspectives, it indicates a genuine low-extraction coordination mechanism. The analytical observer's rope classification is NOT a false summit (unlike the verification bottleneck example) because the structural data supports it — low extractiveness, low suppression, voluntary participation, mutual benefit. The constraint does not naturalize a contingent institutional arrangement; it describes a biological coordination adaptation. The omega variables address empirical uncertainties (oxytocin causality, cultural variance, attachment asymmetry) that could shift the classification if resolved differently, but current evidence supports rope across all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as rope because all agents are beneficiaries of the coordination mechanism. The giver benefits from attachment formation and neurochemical reward. The recipient benefits from the gift and the bond. The social network benefits from trust density. Cultural institutions benefit from social cohesion. No agent is a victim — the constraint solves a collective action problem (trust formation) with minimal extraction. The directionality values are low for all agents (all are beneficiaries with mobile or arbitrage exit options), producing low or negative effective extraction across all perspectives. This is a uniform-type constraint (rope-only) because the structural data shows genuine coordination with minimal extraction from all observation positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the opposite pole from the verification bottleneck: where that constraint showed all six types from different perspectives, this constraint shows rope from all perspectives. Both are legitimate structural patterns. The mandatrophy resolution here is that uniform classification is not a failure of the framework — it is a diagnostic signal. When all perspectives agree, the constraint is either a genuine low-extraction coordination mechanism (rope-only) or a genuine immutable limit (mountain-only). The structural data distinguishes these: mountains have extractiveness ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85, and emerge naturally. This constraint has slightly higher extractiveness (0.18) and does not emerge naturally in the physical sense (it is a biological adaptation, not a law of nature), so it classifies as rope-only rather than mountain-only. The framework correctly identifies this as coordination, not naturalized extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oxytocin_causality_direction,
    'Does oxytocin release during acts of giving cause attachment formation, or does pre-existing attachment cause both oxytocin release and giving behavior?',
    'Experimental manipulation: oxytocin administration in novel dyads before vs after giving acts; longitudinal neurochemical tracking in caregiver attachment formation; comparison of attachment strength in natural giving vs oxytocin-blocked giving',
    'If oxytocin is causal: the mechanism is a genuine coordination adaptation (rope confirmed). If oxytocin is epiphenomenal: the constraint may be downstream of other attachment mechanisms, and the ''generosity causes bonds'' framing is reversed causality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oxytocin_causality_direction, empirical, 'Causal direction of oxytocin in attachment formation').

omega_variable(
    cultural_variance_in_mechanism,
    'Is the generosity-attachment link universal across cultures, or does it depend on specific cultural framing of gift-giving?',
    'Cross-cultural comparison of attachment formation rates in gift-giving vs non-gift-giving relationship contexts; ethnographic analysis of cultures with different gift norms (e.g., cultures where gifts create obligation vs cultures where gifts signal existing bonds)',
    'If universal: the mechanism is a biological coordination adaptation (rope at universal scope). If culturally variable: the mechanism may be a learned norm with higher extraction in some cultural contexts (tangled_rope in obligation-heavy cultures).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_variance_in_mechanism, empirical, 'Cultural universality of generosity-attachment mechanism').

omega_variable(
    asymmetric_attachment_formation,
    'Does the giver form stronger attachment than the recipient, and if so, does this asymmetry create extraction potential?',
    'Longitudinal measurement of attachment strength in giver vs recipient; comparison of relationship dissolution rates initiated by giver vs recipient; neurochemical response magnitude comparison',
    'If symmetric: pure coordination (rope confirmed). If asymmetric with giver more attached: potential for extraction if recipient exploits giver''s attachment (would shift some perspectives toward tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_attachment_formation, empirical, 'Symmetry of attachment formation between giver and recipient').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generosity_as_bond_mechanism, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gen_bond_theater_t0, generosity_as_bond_mechanism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gen_bond_theater_t50, generosity_as_bond_mechanism, theater_ratio, 50, 0.22).
narrative_ontology:measurement(gen_bond_theater_t100, generosity_as_bond_mechanism, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(gen_bond_extract_t0, generosity_as_bond_mechanism, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(gen_bond_extract_t50, generosity_as_bond_mechanism, base_extractiveness, 50, 0.17).
narrative_ontology:measurement(gen_bond_extract_t100, generosity_as_bond_mechanism, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generosity_as_bond_mechanism, attachment_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of temporal_asymmetry_of_obligation. The upstream constraint (temporal asymmetry) has higher extractiveness (tangled_rope) because it captures the obligation dynamics that can emerge from gift-giving. This constraint (generosity as bond mechanism) isolates the pure coordination function — the neurochemical and social process by which giving forms attachment. The two constraints are structurally distinct: temporal_asymmetry focuses on obligation creation (extraction potential), while generosity_as_bond_mechanism focuses on attachment formation (coordination function). They are linked because the same acts of giving trigger both processes, but they have different epsilon values and different classification profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
