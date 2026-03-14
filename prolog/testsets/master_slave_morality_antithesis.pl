% ============================================================================
% CONSTRAINT STORY: master_slave_morality_antithesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_master_slave_morality_antithesis, []).

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
 *   constraint_id: master_slave_morality_antithesis
 *   human_readable: Master-Slave Morality Antithesis
 *   domain: philosophy/ethics/power_dynamics
 *
 * SUMMARY:
 *   Nietzsche's genealogical critique identifies slave morality as a value
 *   inversion that emerged when the powerless lacked ability to dominate
 *   directly: instead of celebrating strength, creation, and will, slave
 *   morality elevates resentment, humility, pity, and renunciation. The
 *   constraint is the systematic suppression of autonomous value-creation
 *   (master morality's domain) through institutional enforcement of values
 *   that naturalize powerlessness as virtue. However, slave morality
 *   simultaneously creates genuine coordination benefits for the oppressed —
 *   shared resentment bonds communities, mutual humility prevents internal
 *   domination, and collective weakness becomes collective strength through
 *   moral unity. The constraint is neither pure coordination (Rope) nor pure
 *   extraction (Snare) but an irreducible hybrid. The theater_ratio shows
 *   increasing performative content: as slave morality institutions mature,
 *   their moral pronouncements become increasingly detached from the actual
 *   powerlessness they claim to serve, instead functioning as legitimation
 *   for institutional power itself. An initially functional moral system
 *   (coordination among the subjugated) gradually degraded into theatrical
 *   maintenance of hierarchies it purports to oppose.
 *
 * KEY AGENTS:
 *   - Subjugated Creators: Primary victims (powerless/trapped) — their capacity for autonomous value-creation is structurally suppressed; cannot articulate alternative moralities without institutional penalty
 *   - Slave Moral Institutions: Primary beneficiaries (institutional/arbitrage) — religious institutions, academic ethics, state apparatuses; benefit from moral authority and institutional legitimacy derived from slave morality
 *   - The Powerless Collective: Secondary beneficiary (powerless/mobile) — genuine coordination benefits from shared moral framework, in-group solidarity, collective identity against domination
 *   - Intermediate Actors: Mixed beneficiary-victim (moderate/constrained) — career advancement within institutions requires internalizing slave morality, constraining autonomous value-creation
 *   - Master Moral Agents: Forced to hide under slave morality (powerful/mobile) — must either suppress own values or be branded immoral; creates identity_locked constraint for agents whose values diverge from institutional morality
 *   - Genealogical Observer: Analytical view (analytical/analytical) — sees both coordination and extraction simultaneously; detects the constraint's irreducibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(master_slave_morality_antithesis, 0.58).
domain_priors:suppression_score(master_slave_morality_antithesis, 0.72).
domain_priors:theater_ratio(master_slave_morality_antithesis, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(master_slave_morality_antithesis, extractiveness, 0.58).
narrative_ontology:constraint_metric(master_slave_morality_antithesis, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(master_slave_morality_antithesis, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(master_slave_morality_antithesis, tangled_rope).
narrative_ontology:human_readable(master_slave_morality_antithesis, "Master-Slave Morality Antithesis").
narrative_ontology:topic_domain(master_slave_morality_antithesis, "philosophy/ethics/power_dynamics").

domain_priors:requires_active_enforcement(master_slave_morality_antithesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(master_slave_morality_antithesis, slave_moral_institutions).
narrative_ontology:constraint_beneficiary(master_slave_morality_antithesis, resentment_based_value_systems).
narrative_ontology:constraint_victim(master_slave_morality_antithesis, autonomous_value_creation).
narrative_ontology:constraint_victim(master_slave_morality_antithesis, power_affirmation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJUGATED CREATOR (SNARE) — Structurally weak agents cannot articulate their own value system without institutional penalty. Speech and value-creation are constrained by the dominant moral framework. Exit requires abandoning the only morality available to the oppressed — trapped within slave morality's inversion logic.
constraint_indexing:constraint_classification(master_slave_morality_antithesis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE SLAVE MORAL INSTITUTION (ROPE) — Institutional actors embedded in slave morality experience genuine coordination benefits: resentment, pity, compassion, and humility create strong in-group bonds and collective identity. The constraint solves a real collective-action problem for the powerless. Net positive from this perspective.
constraint_indexing:constraint_classification(master_slave_morality_antithesis, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE INTERMEDIATE ACTOR (TANGLED ROPE) — Actors with moderate power face genuine value-creation constraints while also benefiting from slave morality's collective solidarity. Career advancement within morally-bound institutions is possible but requires internalizing resentment-based judgment. Both coordination and extraction coexist.
constraint_indexing:constraint_classification(master_slave_morality_antithesis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE MASTER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Powerful agents often naturalize their own value system as universal law — claiming that master morality (strength, creation, affirmation) is 'how things naturally are' for those with will. This classification is a false summit; the engine's oracle gap detector will flag it as naturalization of contingent power advantage.
constraint_indexing:constraint_classification(master_slave_morality_antithesis, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: THE MORAL AUTHORITY INSTITUTION (PITON) — Religious institutions, academic moral philosophy, and state ethics apparatuses have internalized slave morality so thoroughly that its theatrical performance (ritualized condemnation of strength, elevation of humility) becomes mistaken for structural necessity. The institution has 'become' the morality; the original coordination function has atrophied.
constraint_indexing:constraint_classification(master_slave_morality_antithesis, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: THE GENEALOGICAL OBSERVER (TANGLED ROPE) — The analytical perspective recognizes that slave morality serves both coordination (genuine collective solidarity among the powerless) and extraction (suppression of autonomous value-creation). Both functions operate simultaneously. The constraint is irreducible — cannot be decomposed into pure Rope without losing the extraction detection, nor pure Snare without losing the coordination detection.
constraint_indexing:constraint_classification(master_slave_morality_antithesis, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(master_slave_morality_antithesis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(master_slave_morality_antithesis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(master_slave_morality_antithesis, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(master_slave_morality_antithesis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(master_slave_morality_antithesis, TR),
    TR >= 0.70.

:- end_tests(master_slave_morality_antithesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts autonomous value-creation from potential masters and redirects it away from individual creation toward collective and institutional purposes. The extraction is real and sustained, but not total — partial value-creation occurs within the boundaries set by slave morality (art, philosophy, science within acceptable moral frames). The increasing extractiveness trajectory (0.35→0.58 over interval) reflects institutionalization: as slave morality matures, institutions appropriate and monopolize the moral authority originally created by dispersed powerless actors. Suppression (0.72): High. Structural barriers include institutional penalty for non-conformist value expression, social isolation for moral deviance, career exclusion, and epistemic closure that makes alternative moralities literally unthinkable within the dominant frame. Additionally, internalization creates identity_locked suppression: agents come to see their own suppression as virtue. Theater ratio (0.65): Moderate-high and increasing. Institutional slave morality increasingly performs moral pronouncements disconnected from actual powerlessness — ritualized condemnation of strength, elevation of humility, etc. become theatrical maintenance of institutional authority rather than genuine solutions to subjugation. The theater has grown as institutions have captured the moral framework originally created by the powerless.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits profound perspectival divergence across power levels. The subjugated creator sees Snare (pure extraction of their own value-creation, with coordination happening despite the constraint, not because of it). The institutional beneficiary sees Rope (pure coordination — slave morality genuinely solves collective action problems). The analytical observer sees Tangled Rope (both coordination and extraction are structural, neither reducible). The powerful agent in a slave-morality-dominated society experiences identity_locked suppression of their own values, creating a constraint that looks like Snare from their perspective (forced value inversion). The false summit (Master perspective seeing slave morality as Mountain) reveals the oracle gap: the powerful naturalize their own dominance as universal law, missing the genealogical fact that value systems are constructed, not discovered. The master moralist (powerful agent defending slave morality institutions) sees Rope or even positive contribution, missing the extraction flowing toward institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Slave moral institutions benefit from the constraint (low d → negative f(d) → extraction flows toward them). The powerless experience extraction suppressing their autonomous values, but also receive coordination benefits from shared moral framework. This mixed directionality is the core Tangled Rope signature: beneficiaries exist (institutions), victims exist (creators and masters), and the constraint serves both coordination and extraction simultaneously. No agent perceives pure extraction or pure coordination — all see mixed effects. The intermediate actor (moderate power) faces constrained exit because leaving slave morality incurs identity loss and social penalty, but staying incurs value suppression. The master moral agent faces similar constraint but in reverse: their identity and values conflict with dominant morality, creating identity_locked suppression. Directionality overrides are unnecessary; the structural data (beneficiaries, victims, exit options) produce accurate d values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as Tangled Rope rather than being forced into Snare or Rope because both dimensions are empirically real and irreducible. Slave morality does coordinate the powerless (genuine Rope function). It also does extract autonomous value-creation from masters and redirect it (genuine Snare function). The misclassification to Snare-only would be mandatrophy: treating the real coordination benefits (in-group solidarity, collective resistance, moral framework for the powerless) as fictional. The misclassification to Rope-only would be inverse mandatrophy: treating the real suppression of alternative value systems as non-extractive. Tangled Rope classification correctly identifies that the constraint's existence depends on both coordination (why the powerless embrace it) and extraction (why it persists even as institutions monopolize the authority). The increasing theater_ratio over time suggests potential degradation toward Piton: as moral institutions hollow out and perform moral pronouncements disconnected from actual powerlessness, the coordination function decays while extraction continues. Future historical analysis would test whether this trajectory actually occurs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    morality_origin_mechanism,
    'Is slave morality a genuine discovery of moral truth, an adaptive invention by the powerless, or a self-deception mechanism that disguises power dynamics?',
    'Genealogical analysis comparing origin narratives; empirical study of moral intuitions across power positions; cross-cultural comparison of morality systems in stratified vs egalitarian societies',
    'If genuine discovery: morality is transcendent (Mountain). If adaptive invention: constraint is Rope (coordination with some inertia). If self-deception: constraint is Snare (pure extraction rationalized).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(morality_origin_mechanism, conceptual, 'Whether slave morality is discovered truth, adaptive invention, or rationalized power suppression').

omega_variable(
    internalization_vs_coercion,
    'What proportion of slave morality''s suppression is structural (external enforcement) vs internalized (agents adopting it as their own framework)?',
    'Longitudinal study of moral commitment before/after exit from suppressive institutions; comparison of stated vs revealed moral preferences; analysis of moral reasoning in isolation vs community contexts',
    'If primarily coerced: suppression metric should be structural barriers (trapped exit). If primarily internalized: suppression reflects identity_locked exit (agent cannot see outside the morality). Changes classification emphasis across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_coercion, empirical, 'Structural coercion vs internalized adoption of slave morality').

omega_variable(
    value_creation_ceiling,
    'Do slave morality institutions systematically suppress autonomous value-creation, or do they simply redirect it toward collective rather than individual goods?',
    'Comparative analysis of innovation rates, artistic output, and philosophical contribution under master vs slave moral dominance; analysis of value-creation that serves community vs ego; historical case studies of creative flourishing in egalitarian vs hierarchical moral contexts',
    'If suppressed: extractiveness correctly measured at 0.58. If redirected: extractiveness should be lower (0.30-0.40) — the constraint is coordination-heavy. If amplified: extractiveness could be negative — slave morality generates collective value exceeding individual loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_creation_ceiling, empirical, 'Whether slave morality suppresses or redirects autonomous value-creation').

omega_variable(
    exit_path_feasibility,
    'Can individuals or groups exit slave morality systems by adopting alternative value systems, or does the antithesis make exit permanently identity-locked?',
    'Study of individuals/groups that have abandoned morality systems; analysis of identity transformation required; documentation of psychological and social cost of exit; comparison with other identity-locked constraints (cults, professional capture)',
    'If genuinely possible: exit_options upgrade from trapped to constrained or identity_locked. If permanently locked: identity_locked is correct classification. Determines whether escape velocities are possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_path_feasibility, empirical, 'Feasibility of exiting slave morality through identity transformation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(master_slave_morality_antithesis, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mast_tr_t0, master_slave_morality_antithesis, theater_ratio, 0, 0.4).
narrative_ontology:measurement(mast_tr_t3, master_slave_morality_antithesis, theater_ratio, 3, 0.52).
narrative_ontology:measurement(mast_tr_t6, master_slave_morality_antithesis, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(mast_be_t0, master_slave_morality_antithesis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mast_be_t3, master_slave_morality_antithesis, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(mast_be_t6, master_slave_morality_antithesis, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(master_slave_morality_antithesis, identity_coordination).
narrative_ontology:affects_constraint(master_slave_morality_antithesis, institutional_legitimacy_cascade).
narrative_ontology:affects_constraint(master_slave_morality_antithesis, resentment_accumulation_dynamics).

% DUAL FORMULATION NOTE:
% Master-Slave morality antithesis could decompose into two constraints with different ε values: (1) slave_morality_as_collective_coordination (ε≈0.15, Rope) capturing the genuine in-group solidarity function, and (2) value_creation_suppression_mechanism (ε≈0.72, Snare) capturing the suppression of autonomous creative capacity. They are presented as a single Tangled Rope story because the constraint's structural integrity depends on the entanglement — neither function works without the other. The institutional capture of moral authority (affecting both coordination and extraction) only appears when both stories are analyzed together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
