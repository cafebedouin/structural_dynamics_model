% ============================================================================
% CONSTRAINT STORY: starwars_evolutionary_mutation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_starwars_evolutionary_mutation, []).

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
 *   constraint_id: starwars_evolutionary_mutation
 *   human_readable: Jedi as Systemic Evolutionary Outliers
 *   domain: social/political/biological
 *
 * SUMMARY:
 *   The Jedi Order represents a systemic evolutionary outlier—a mutation in
 *   the galactic political landscape that reveals which institutional
 *   'necessities' are genuine constraints and which are bureaucratic
 *   artifacts. Presented as the inevitable response to Force sensitivity (a
 *   natural phenomenon), the Jedi Order is structurally a tangled rope: it
 *   provides genuine coordination benefits (training, community,
 *   meaning-making for Force-sensitive individuals) while simultaneously
 *   extracting compliance through isolation, celibacy, hierarchical
 *   obedience, and suppression of alternative Force traditions. The
 *   constraint exhibits all six classification types depending on observer
 *   position: snare for unrecognized Force users with no exit; tangled rope
 *   for Jedi initiates with constrained mobility; rope for the
 *   Republic/Empire power structure that outsources force-user management;
 *   scaffold for reformist movements building alternative interpretations;
 *   piton for the Jedi Order's self-maintaining institutional narrative; and
 *   false mountain for the analytical observer who might naturalize the
 *   arrangement as inevitable. The theater ratio (0.68) reflects that Council
 *   deliberation, prophecy reading, and meditation rituals increasingly serve
 *   symbolic functions while actual force-user management occurs through
 *   hidden execution, political coercion, and suppression of alternatives.
 *   The constraint's base extractiveness (0.38) has risen from 0.18 over the
 *   monitored interval, indicating accumulation of rent-seeking mechanisms
 *   (e.g., Jedi tax exemptions, political influence, control of
 *   Force-sensitive identification) layered onto the original coordination
 *   function.
 *
 * KEY AGENTS:
 *   - Force-Sensitive Individuals (Unrecognized): Powerless/trapped — primary victims facing systematic suppression and forced institutional absorption
 *   - Jedi Initiates: Moderate/constrained — experience mixed coordination benefits and extractive constraints (training vs isolation, community vs obedience)
 *   - Sith Lineage/Alternative Force Users: Powerless/trapped — institutionalized as the pathologized alternative; represent constrained evolutionary path
 *   - Jedi Reform Movements: Organized/mobile — seek to expand understanding of Force ethics and dissolve binary suppression models (scaffold perspective)
 *   - Republic/Empire Central Authority: Institutional/arbitrage — benefits from Jedi as distributed force-user management without direct policing costs
 *   - Jedi Council: Institutional/arbitrage — maintains order's institutional inertia; theater ratio increases as verification capacity degrades
 *   - Analytical Observer: Civilizational/analytical — risks naturalizing contingent institutional arrangement as law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(starwars_evolutionary_mutation, 0.38).
domain_priors:suppression_score(starwars_evolutionary_mutation, 0.52).
domain_priors:theater_ratio(starwars_evolutionary_mutation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(starwars_evolutionary_mutation, extractiveness, 0.38).
narrative_ontology:constraint_metric(starwars_evolutionary_mutation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(starwars_evolutionary_mutation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(starwars_evolutionary_mutation, tangled_rope).
narrative_ontology:human_readable(starwars_evolutionary_mutation, "Jedi as Systemic Evolutionary Outliers").
narrative_ontology:topic_domain(starwars_evolutionary_mutation, "social/political/biological").

domain_priors:requires_active_enforcement(starwars_evolutionary_mutation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(starwars_evolutionary_mutation, galactic_order_maintenance).
narrative_ontology:constraint_beneficiary(starwars_evolutionary_mutation, institutional_stability).
narrative_ontology:constraint_victim(starwars_evolutionary_mutation, individual_force_sensitive_autonomy).
narrative_ontology:constraint_victim(starwars_evolutionary_mutation, alternative_force_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNRECOGNIZED FORCE USER (SNARE) — Individuals with Force sensitivity outside Jedi institutional channels face systematic suppression: blocked from training, pathologized as unstable, imprisoned as dangerous, or absorbed into Jedi order under coercive terms. No exit exists. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SITH LINEAGE (SNARE) — Institutionalized as the sole alternative to Jedi, the Sith represent a constrained evolutionary path: dark side users are expected to either join the Sith order or be hunted by the Jedi. Both options involve extraction of loyalty and submission. The Sith structure itself becomes a trap—teaching that individual will dominates, but constraining choice within a hierarchical master-apprentice model. d≈0.93, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: JEDI INITIATE (TANGLED ROPE) — Force-sensitive children brought into the Order at young age experience coordination benefits (training, community, purpose) alongside extraction (loss of biological family, obedience to Council doctrine, celibacy constraints, non-attachment restrictions). Constrained exit: leaving the Order during training is possible but carries stigma and loss of social identity. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REPUBLIC/EMPIRE POWER STRUCTURE (ROPE) — Galactic authorities experience the Jedi as coordination mechanism: the Order solves the collective action problem of force-user management without requiring central policing infrastructure. The institutional arrangement provides deniability and distributed legitimacy. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Net beneficiary; negative extraction indicates subsidy.
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: JEDI REFORM MOVEMENT (SCAFFOLD) — Individuals seeking to expand Jedi understanding (attachment ethics, balance philosophy, alternative training) operate within the Order but push against its constraints. This perspective sees the Order's rigidity as temporary—newer interpretations of the Force (e.g., Gray Jedi, balanced philosophy) represent sunset mechanisms displacing the old binary extraction model. d≈0.48, f(d)≈0.62, σ=1.0 → χ≈0.19. Moderate chi; agents have agency and see an exit path (institutional reform).
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: JEDI ORDER'S SELF-CONCEPTION (PITON) — The Order maintains a narrative of necessity: 'We must control Force users because unchecked power destabilizes civilization.' This framing persists despite substantial evidence of degradation: the Order's actual verification capacity (Council prophecy reading, accurate threat assessment) is substantially performative. theater_ratio=0.68 reflects that Council deliberation, lightsaber katas, and meditation rituals serve symbolic functions while actual Force-user management occurs through political leverage and hidden execution. Piton classification emerges from high theater despite institutional position.
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a biological/physical standpoint, Force sensitivity appears to be a naturally occurring genetic/energetic variation. One might argue that some institutional constraint is inevitable: communities will organize around individuals with rare, powerful capacities. However, the specific Jedi form (isolation, non-attachment, Council governance) is NOT a law of nature. The base properties (ε=0.38, suppression=0.52, theater=0.68) contradict mountain classification—the engine flags this as a false summit, revealing that 'some coordination is inevitable' naturalizes the specific Jedi institutional arrangement.
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(starwars_evolutionary_mutation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(starwars_evolutionary_mutation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(starwars_evolutionary_mutation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(starwars_evolutionary_mutation, TR),
    TR >= 0.70.

:- end_tests(starwars_evolutionary_mutation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Jedi Order provides genuine benefits (training Force control, community, purpose for Force-sensitive individuals) alongside extractive mechanisms (isolation from non-Force families, celibacy constraints, hierarchical obedience, suppression of alternative traditions). The intermediate value reflects the hybrid nature: early in the Order's history (0.18), the coordination function dominated; over time (0.38 by interval end), extraction accumulates as the Order consolidates political power. Suppression (0.52): Moderate-high. The Order suppresses alternative Force traditions, non-Jedi force-user development, and free association among Force-sensitive individuals. However, suppression is not total—Sith exist as a constrained alternative, reform movements operate within the Order, and some unrecognized Force users escape detection. Theater ratio (0.68): High. The Jedi Council's prophecy reading, threat assessment, and deliberation are substantially performative—they confer legitimacy and justify coercive actions but lack strong predictive accuracy. Meditation rituals, lightsaber training, and council chambers serve symbolic validation functions. Actual force-user suppression occurs through political leverage and hidden execution. The theater ratio has increased over the interval as the Order's institutional power has grown while its actual verification capacity has not kept pace with the complexity of Force manifestations across galactic populations.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural arrangement (Jedi institutional control of Force-user development) produces incompatible classifications from different observation positions. The powerless Force user sees a Snare—no exit, total suppression, coercive institutional absorption. The Jedi initiate sees a Tangled Rope—genuine benefits alongside genuine costs, with some constrained mobility. The institutional authority sees a Rope—the Order solves a coordination problem at low cost. The reform movement sees a Scaffold—old rigidities are being displaced by new ethics. The Order itself sees a Piton—its own narrative of necessity persists through institutional inertia despite degraded function. The analytical observer risks seeing a Mountain—'some coordination is necessary for force-user societies'—but the structural data reveals this as a false summit. The perspectival gap is not measurement error; it is evidence that the constraint's classification depends critically on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Unrecognized Force user: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; no exit. Jedi initiate: Mixed (victim of constraints, beneficiary of training) + constrained → d≈0.65, f(d)≈0.95. Significant but not maximal extraction. Sith/Alternative users: Victim + trapped (forced into suppressed alternative) → d≈0.93, f(d)≈1.38. Near-maximum extraction. Republic/Empire authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; negative effective extraction. Jedi Reform: Organized + mobile → d≈0.48, f(d)≈0.62. Low effective extraction; agents have agency. Jedi Order self-conception: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. But piton classification comes from theater gate (≥0.70), not from chi calculation. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit detector flags the mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT DECOMPOSITION OPPORTUNITY: This constraint may decompose into three separate stories with different ε values and structural relationships: (1) Force-User Coordination (ε≈0.15, Rope)—the genuine problem of organizing Force-sensitive individuals into functioning communities; (2) Jedi Institutional Extraction (ε≈0.38, Tangled Rope)—the specific institutional mechanisms of control, celibacy, and suppression layered onto coordination; (3) False Necessity Naturalization (ε≈0.52, Snare)—the cognitive/political constraint that prevents recognition of alternative governance models. Currently modeled as a single constraint exhibiting all types, but ε-invariance principle suggests decomposition if base measurement changes interpretation fundamentally. The existing unitary model captures the mandatrophy: all six types are legitimate from different perspectives—the constraint is NOT a false summit or a false snare, but a genuine hybrid where the classification depends entirely on structural position and measurement basis. The 'necessity' of Jedi order is neither a law of nature nor pure oppression; it is a contingent institutional arrangement optimized for central authority convenience rather than Force-user autonomy or alternative tradition preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    force_sensitivity_distribution,
    'Is Force sensitivity a rare heritable trait or a dormant capacity in all sentients that training can unlock?',
    'Genetic analysis of Force-sensitive populations; empirical testing of untrained individuals for latent capacity; cross-cultural variation in Force manifestation rates',
    'If rare heritable: Jedi monopoly on training is a coordination solution (Rope tendencies increase). If universal dormant: Jedi restriction represents pure exclusion (Snare classification strengthens from multiple perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(force_sensitivity_distribution, empirical, 'Whether Force sensitivity is rare or dormant-universal').

omega_variable(
    force_user_stability_without_jedi,
    'Can Force-sensitive individuals achieve psychological and institutional stability outside the Jedi Order''s non-attachment framework?',
    'Longitudinal comparison of Jedi-trained vs independent Force users; assessment of emotional regulation, relationship function, and institutional reliability; analysis of Sith organizational stability vs instability',
    'If yes: Jedi''s restrictive ethics are contingent choice, not biological necessity. Institutional extraction becomes unmistakable (Snare/Tangled Rope prevails). If no: the Jedi''s suppressive framework is justified as coordination (Rope/Mountain case strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(force_user_stability_without_jedi, empirical, 'Whether Force users can stabilize outside Jedi non-attachment framework').

omega_variable(
    council_prophecy_verification,
    'Do Jedi Council prophecy and threat assessments predict actual events at rates better than chance or institutional reputation?',
    'Historical analysis of Council prophecies and threat assessments; comparison of prediction accuracy against baseline galactic event rates; identification of confirmation bias and false negatives (threats missed)',
    'If accurate: theater_ratio downward revision, piton classification weakens (real function exists). If at chance rates: theater_ratio confirmed, piton classification strengthens (pure institutional inertia). Forces mandatrophy resolution regarding whether the Order''s coercive necessity is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(council_prophecy_verification, empirical, 'Whether Jedi Council predictions exceed chance accuracy').

omega_variable(
    alternative_force_governance_models,
    'Have historical Force-governing models outside the Jedi/Sith binary (e.g., Gray Jedi communities, Nightsisters collective) produced stable outcomes comparable to Jedi or Sith order?',
    'Comparative institutional analysis of alternative Force communities; assessment of longevity, internal stability, external threat rates, member autonomy preservation',
    'If comparable stability: entire constraint may decompose into two separate stories—one modeling Jedi as Rope (coordination), another as Snare (extraction). Network relationship established. If inferior stability: Jedi suppression is justified (Mountain case strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_force_governance_models, empirical, 'Whether alternative Force governance models achieve comparable stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(starwars_evolutionary_mutation, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jedi_tr_t0, starwars_evolutionary_mutation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jedi_tr_t500, starwars_evolutionary_mutation, theater_ratio, 500, 0.55).
narrative_ontology:measurement(jedi_tr_t1000, starwars_evolutionary_mutation, theater_ratio, 1000, 0.68).

% Extraction over time
narrative_ontology:measurement(jedi_be_t0, starwars_evolutionary_mutation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(jedi_be_t500, starwars_evolutionary_mutation, base_extractiveness, 500, 0.28).
narrative_ontology:measurement(jedi_be_t1000, starwars_evolutionary_mutation, base_extractiveness, 1000, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(starwars_evolutionary_mutation, enforcement_mechanism).
narrative_ontology:affects_constraint(starwars_evolutionary_mutation, sith_institutional_path_dependency).
narrative_ontology:affects_constraint(starwars_evolutionary_mutation, force_user_identity_suppression).

% DUAL FORMULATION NOTE:
% The Jedi constraint family includes: (1) Force-User Coordination (ε≈0.15, Rope)—modeling genuine coordination need; (2) Jedi Institutional Extraction (ε≈0.38, Tangled Rope, current story)—modeling institutional mechanisms; (3) Sith Alternative Path (ε≈0.42, Tangled Rope)—modeling constrained evolutionary option. Each exhibits different ε values reflecting distinct structural claims. The current story models the hybrid; upstream Rope story would establish base coordination need; parallel Sith story would establish how the suppression works.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(starwars_evolutionary_mutation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
