% ============================================================================
% CONSTRAINT STORY: contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_contraction_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: contraction_reading
 *   human_readable: Honor Redefinition as Constraint on Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   In the contraction reading of honor-violence legitimacy, dueling becomes
 *   structurally unthinkable not through legal prohibition alone but through
 *   redefinition of honor itself to exclude violence as a legitimate response
 *   mechanism. The constraint operates at the conceptual level: the category
 *   of actions that 'count' as honorable contracts to exclude combat. This
 *   reading instantiates a specific kernel interpretation — that the honor
 *   concept narrows its scope — distinct from sibling readings that would
 *   model honor's abandonment entirely (drop reading) or its expansion to
 *   include multiple legitimacy channels (composite reading). The contraction
 *   is enforced through institutional mechanisms (state law, educational
 *   reframing, social capital redistribution) that make violence-based honor
 *   claims literally unthinkable within the dominant legitimacy framework.
 *   The constraint exhibits tangled rope structure: it solves a genuine
 *   coordination problem (how to adjudicate status without endemic feuding)
 *   while simultaneously extracting authority over status-granting from
 *   distributed nobility to centralized state. The battlefield becomes
 *   unthinkable as a site of honor not because violence itself is abandoned
 *   but because the conceptual space that connected violence to honor
 *   collapses. Dueling rituals persist through institutional inertia (piton
 *   perspective) even as their functional legitimacy drains away. The warrior
 *   caste bears maximum suppression through identity lock: they cannot
 *   exercise the profession that constituted their selfhood because the
 *   redefinition of honor has rendered that profession dishonorable.
 *
 * KEY AGENTS:
 *   - Warrior Caste: Primary victim (powerless/identity_locked) — identity constituted through violence-based honor; redefinition renders profession unthinkable without identity dissolution
 *   - State Consolidators: Primary beneficiary (institutional/arbitrage) — monopolize honor-granting authority; eliminate distributed violence; reduce competition for legitimate status enforcement
 *   - Transitional Nobility: Secondary agent (moderate/constrained) — experience mixed extraction and coordination; lose military independence but gain security and new status pathways
 *   - Enlightenment Advocates: Organized coalition (organized/constrained) — intellectually legitimate the redefinition as progressive; scaffold structure suggests generational sunset once new norms consolidate
 *   - Merchant-Derived Nobility: Beneficiary (powerful/mobile) — ascend through new honor regime; wealth now counts as legitimate status marker
 *   - Residual Duel Rituals: Institutional persistence (institutional/arbitrage) — formal dueling continues as performative ritual divorced from functional violence; maintains piton structure through inertia
 *   - Analytical Observer: Civilizational (analytical/analytical) — risks naturalizing contingent reading as inevitable law of state development; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(contraction_reading, 0.38).
domain_priors:suppression_score(contraction_reading, 0.62).
domain_priors:theater_ratio(contraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(contraction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(contraction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(contraction_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(contraction_reading, tangled_rope).
narrative_ontology:human_readable(contraction_reading, "Honor Redefinition as Constraint on Violence (Contraction Reading)").
narrative_ontology:topic_domain(contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(contraction_reading, distributed).
narrative_ontology:cs_authority_grounding(contraction_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(contraction_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(contraction_reading, institutional_peace_advocates).
narrative_ontology:constraint_beneficiary(contraction_reading, state_monopoly_claimants).
narrative_ontology:constraint_victim(contraction_reading, warrior_caste_identity).
narrative_ontology:constraint_victim(contraction_reading, pre_legal_honor_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WARRIOR CASTE (SNARE) — Identity constituted through combat honor; redefinition of honor to exclude violence renders their professional identity structurally impossible. Trapped not by external force but by cognitive frame: to accept the new honor definition is to cease being what they are. Maximum suppression from the target's standpoint — exit requires identity death. The constraint is experienced as involuntary dissolution of selfhood.
constraint_indexing:constraint_classification(contraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRANSITIONAL NOBILITY (TANGLED ROPE) — Dual structure: genuine coordination problem (how to adjudicate honor without violence; how to maintain social rank through non-violent means) coupled with asymmetric extraction (state consolidates honor-granting authority; nobility loses autonomous enforcement capacity). Benefits from new status order but at cost of military independence. Significant constraints but not total — can renegotiate identity within legitimacy boundaries.
constraint_indexing:constraint_classification(contraction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE CONSOLIDATORS (ROPE) — Pure beneficiary. Redefinition of honor as non-violent is a coordination solution to the problem of distributed violence: instead of each noble household enforcing rank through combat, the state apparatus enforces rank through law and symbolic capital. The constraint coordinates the problem of status without endemic feuding. State experiences the redefinition as solving a collective action problem, not imposing extraction.
constraint_indexing:constraint_classification(contraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ENLIGHTENMENT ADVOCATES (SCAFFOLD) — Organized agents (philosophers, religious reformers, legal scholars) who articulate the redefinition as progressive — that 'true' honor lies in civic virtue, intellectual achievement, and social responsibility rather than martial prowess. See the transition as temporary suppression of an old form to establish new norms, with built-in sunset: once the new honor regime consolidates (generational timescale), the old violence-based honor becomes unthinkable and the constraint (coercion into new definitions) can relax. Theater_ratio reflects that the scaffold actively performs the intellectual work of legitimating the new frame.
constraint_indexing:constraint_classification(contraction_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEGRADED DUEL RITUALS (PITON) — In later generations, formal dueling persists not as real violence enforcement but as theatrical ritual — the duel becomes a stylized performance that satisfies honor claims without lethal force (formal dueling codes prohibit most combative advantage, emphasize formality over outcome). Theater_ratio rises: the duel ritual becomes performative, maintained by institutional inertia among military academies and aristocratic circles long after the functional role (violence as legitimate status enforcement) has been superseded. The ritual persists because alternatives haven't fully displaced it, not because it works.
constraint_indexing:constraint_classification(contraction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: MERCHANT-DERIVED NOBILITY (TANGLED ROPE) — Powerful agents without warrior ancestry whose rank depends on the new honor regime (legal/contractual rather than martial). Beneficiaries from the contraction: their wealth and intellect now count as honor. But also constrained by the apparatus enforcing the redefinition — they cannot operate outside the state's legitimacy framework. Mobile exit exists but at significant cost to social standing. This perspective sees tangled rope structure: genuine coordination benefit (wealth enables social mobility) with persistent state extraction (honor requires state confirmation).
constraint_indexing:constraint_classification(contraction_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL/NATURAL LAW (MOUNTAIN — FSM CANDIDATE) — Views the contraction as an inevitable phase in civilizational development: that honor-based violence is inherently unstable when states achieve monopoly on force; that conceptual redefinition to non-violent honor is a natural equilibrium selection problem. Naturalizes the specific reading (contraction: honor excludes violence) as a law of social physics rather than contingent institutional choice. Engine's false summit detector will identify this as naturalization of what is actually a reading-dependent committer choice.
constraint_indexing:constraint_classification(contraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(contraction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(contraction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(contraction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(contraction_reading, TR),
    TR >= 0.70.

:- end_tests(contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high, rising over the interval. Initial extractiveness (0.22) reflects the coordination problem being solved — the state genuinely reduces violence and provides alternative status mechanisms. But extractiveness rises to 0.42 as the constraint mechanism becomes explicit: the state actively suppresses violence-based honor claims and enforces the new definition. The trajectory shows initial coordination benefit giving way to sustained extraction as the authority consolidation persists. Suppression (0.62): High and persistent. The suppression mechanism is multilayered: legal prohibition (duelists prosecuted), social sanctions (duelists lose honor rather than gain it), educational reframing (new definitions of virtue taught as natural and inevitable), and identity-lock dynamics (younger generations cannot imagine violence as honorable without experiencing cognitive alienation from their own framing). Theater ratio (0.58): Rising from 0.32 to 0.58, indicating increasing performative content over the interval. Initially, the redefinition has genuine coordination content — intellectuals, reformers, and state actors solve real problems (reducing feudal violence). But as the constraint consolidates, performative elements increase: formal dueling rituals persist without lethal function; educational institutions perform the teaching of new honor norms; state ceremonies emphasize the legitimacy of the new regime. The rising theater_ratio signals that functional justification for the constraint is being replaced by institutional inertia and legitimacy performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The warrior caste experiences snare (identity dissolved through involuntary frame collapse). The state apparatus experiences rope (solving a coordination problem). The scaffold coalition experiences temporary constraint with sunset (generational learning process). Residual dueling communities experience piton (ritual without function). Transitional nobility experience tangled rope (mixed costs and benefits). Merchant nobility experience benefits without perceiving extraction. The analytical observer risks mountain classification (naturalizing contingent reading). These gaps reflect that the same structural event — 'honor redefined to exclude violence' — is experienced as either immutable natural law (mountain, false summit), rational progress (rope), temporary discipline (scaffold), degraded ritual (piton), or extraction apparatus (snare), depending on structural position and time horizon. The perspectival map is the full structure; no single type captures the constraint's reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives. Warrior caste victims with identity_locked exit: d ≈ 0.89 (nearly pure target), generating high experienced extraction through identity fusion making exit unthinkable. State beneficiaries with arbitrage exit: d ≈ 0.05 (nearly pure beneficiary), generating negative effective extraction (they are subsidized by the constraint's consolidation). Transitional nobility with constrained exit: d ≈ 0.58 (mixed), experiencing moderate extraction through cost of renegotiating status despite some coordination benefits. Enlightenment advocates with constrained exit: d ≈ 0.45 (moderate), experiencing coordination benefit (their intellectual framework is being institutionalized) but also extraction risk (they remain constrained by the state's authority over legitimacy). Merchant nobility with mobile exit: d ≈ 0.35 (moderate beneficiary), experiencing low net extraction because their exit options are genuinely open (they can migrate, relocate, or adopt alternative status markers). The high variance in d across perspectives drives the perspectival gap: the same constraint produces opposite experienced extractiveness values depending on agent position.
 *
 * MANDATROPHY ANALYSIS:
 *   The contraction reading resolves mandatrophy by showing that the constraint simultaneously solves the coordination problem (state replaces endemic feudal violence) and extracts authority (state monopolizes status-granting). Both functions are real. The tension is not 'which type is correct?' but 'which function dominates the observer's experience?' For the state apparatus and merchant nobility, coordination dominates (rope/tangled rope). For the warrior caste, extraction dominates (snare). For the scaffold coalition, temporary constraint dominates (scaffold). For the analytical observer, the risk is naturalizing the state's perspective as inevitable law. The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid — it coordinates the violence problem while extracting authority — and that different perspectives weight these functions differently based on their structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint the inevitable contraction of honor (violence naturally squeezed out by state consolidation), or one contingent reading of a contested kernel whose sibling readings (drop or composite) would produce structurally different constraints?',
    'Historical comparison: examine alternate trajectories where honor definition did NOT contract (e.g., parallel equilibria where state tolerates dueling; where honor remains violence-inclusive). Identify the decision points where the contraction reading was chosen vs alternatives.',
    'If contraction is inevitable: classification as mountain (natural law of state consolidation) is justified. If contingent: false summit detection fires; the constraint is tangled rope (institutional choice enforced on warrior class), not a law of nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether contraction reading is inevitable or contingent institutional choice').

omega_variable(
    identity_lock_vs_structural_trap,
    'Is the warrior caste''s binding to violence-based honor a cognitive identity lock (they could choose new frames but cannot see this from inside their identity), or a structural economic trap (their wealth and status genuinely depend on exclusive military function)?',
    'Post-transition analysis: do warrior caste members who successfully reframe their identity (adopt merchant identity, ecclesiastical authority, or civic leadership) regain agency and mobility? Or do they face persistent structural barriers even after cognitive frame shift?',
    'If identity lock: classification as snare at identity_locked exit is correct. If structural trap: should be snare at trapped exit with higher suppression. If mixed: suppression decomposition needed in omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Whether warrior binding is cognitive identity lock or material structural dependence').

omega_variable(
    suppression_mechanism_internalization,
    'Does the suppression of dueling decline after the redefinition establishes (generational settling time), or does it persist because the new honor regime requires continuous enforcement?',
    'Measurement of dueling frequency, legal prosecution, and social sanctions over 50+ year horizons. If suppression declines: indicates scaffold sunset working. If suppression persists or rises: indicates structural extraction, not temporary coordination problem.',
    'If suppression declines: scaffold classification confirmed. If suppression persists: reclassify as snare or persistent tangled rope with no sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of violence-based honor declines over generational timescale').

omega_variable(
    sibling_reading_structural_delta,
    'How would the ''drop reading'' (honor concept itself abandoned, replaced by civic virtue) or ''composite reading'' (honor remains violence-inclusive but also includes civic channels) produce different ε values and classifications?',
    'Counterfactual analysis: construct constraint stories for drop_reading and composite_reading. Compare their extractiveness, suppression, beneficiary/victim structures. Identify which reading best explains observed historical outcomes (suppression patterns, dueling frequency, institutional consolidation timelines).',
    'If drop_reading explains outcomes better: contraction_reading may be false (honor didn''t contract; it was abandoned). If composite_reading explains outcomes better: contraction_reading is partial (honor expanded, not contracted). If contraction_reading best explains: validates this reading''s ε as primary structural description.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural comparison with drop and composite readings of honor_violence_legitimacy kernel').

omega_variable(
    state_monopoly_vs_honor_logic,
    'Is the contraction reading a consequence of state monopoly on legitimate force, or is it a rereading of honor logic driven by intellectual frameworks (Enlightenment thought, religious reform) independent of state consolidation?',
    'Historical sequence analysis: examine whether honor redefinition preceded or followed state force consolidation in specific cases. Identify causal mechanisms: do states that fail to consolidate force monopoly also fail to enforce honor contraction? Do intellectual movements preceded by weaker states still produce honor redefinition?',
    'If driven by state monopoly: beneficiary is state apparatus; constraint enforces its consolidation. If driven by intellectual reframing: beneficiary is reform coalition; constraint enforces ideological shift. Different beneficiary groups suggest different perspectives and potentially different ε values for alternative framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_vs_honor_logic, empirical, 'Whether contraction is driven by state consolidation or intellectual reframing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(contraction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(contraction_theater_t0, contraction_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(contraction_theater_t1, contraction_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(contraction_theater_t2, contraction_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(contraction_extract_t0, contraction_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(contraction_extract_t1, contraction_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(contraction_extract_t2, contraction_reading, base_extractiveness, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(contraction_reading, drop_reading).
narrative_ontology:affects_constraint(contraction_reading, composite_reading).

% DUAL FORMULATION NOTE:
% The honor_violence_legitimacy kernel decomposes into three structurally distinct constraint stories: contraction_reading (ε=0.38, honor narrows to exclude violence), drop_reading (ε=estimated 0.55, honor abandoned entirely), and composite_reading (ε=estimated 0.32, honor expands to multiple legitimacy channels). Each reading produces different beneficiary/victim structures and different measured extractiveness values. The sibling constraints are linked through the shared kernel and through their differential explanatory power for observed historical outcomes. This constraint (contraction_reading) is the canonical reading for European aristocratic transitions but may be empirically dominated by sibling readings in other historical contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(contraction_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
