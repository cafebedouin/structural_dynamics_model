% ============================================================================
% CONSTRAINT STORY: magna_carta_liberties
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_liberties, []).

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
 *   constraint_id: magna_carta_liberties
 *   human_readable: The Great Charter of Liberties (Magna Carta, 1215)
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Magna Carta of 1215 is a forced peace treaty between King John and a
 *   coalition of rebellious barons, presented as establishing universal
 *   principles of justice ('to no one will we sell, deny, or delay right or
 *   justice') but systematically structured to benefit the baronial coalition
 *   while extracting from or excluding other populations. The charter
 *   coordinates baronial collective action against arbitrary royal
 *   prerogative through defined protections: guaranteed trial by peers,
 *   restricted reliefs and wardship terms, consent required for major
 *   taxation. Simultaneously, it consolidates extraction from unfree peasants
 *   (explicitly excluded from 'free men' protections), Jewish creditors
 *   (subject to specific debt forgiveness and wardship provisions), and the
 *   royal prerogative itself (permanently constrained from a baronial
 *   perspective but temporarily and incompletely from the crown's
 *   perspective). The charter's enforceability degrades rapidly: King John
 *   repudiates it within months; it requires reissue in 1217 and 1225 under
 *   pressure; by 1265 the Mise of Lewes must reinvoke it in the barons' wars.
 *   The theater ratio increases over time as the charter becomes increasingly
 *   performative—reissued as a claims document about justice rather than an
 *   enforceable constraint—while extraction mechanisms persist against
 *   excluded populations. This makes the Magna Carta a diagnostic case of
 *   tangled rope: genuine coordination mechanism + structural extraction +
 *   degradation over time.
 *
 * KEY AGENTS:
 *   - Rebellious Barons: Primary beneficiaries (organized/constrained) — Extract wealth protection and political voice while coordinating against royal arbitrary power
 *   - King John and Royal Administration: Institutional actor (institutional/arbitrage) — Initially loses prerogative through force, then incrementally recovers authority through reinterpretation and non-enforcement
 *   - Unfree Peasantry: Primary victims (powerless/trapped) — Systematically excluded from charter protections; remain subject to manorial extraction without recourse
 *   - Lesser Knights and Freeholders: Secondary beneficiaries (moderate/constrained) — Benefit from some protections but remain hierarchically constrained
 *   - Jewish Creditors and Financial Communities: Secondary victims (moderate/trapped) — Explicitly targeted by debt forgiveness provisions; subject to crown wardship extraction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — Sees the charter as performing universalism while embedding exclusion; coordination + extraction hybrid
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_liberties, 0.38).
domain_priors:suppression_score(magna_carta_liberties, 0.72).
domain_priors:theater_ratio(magna_carta_liberties, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_liberties, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_liberties, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(magna_carta_liberties, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_liberties, tangled_rope).
narrative_ontology:human_readable(magna_carta_liberties, "The Great Charter of Liberties (Magna Carta, 1215)").
narrative_ontology:topic_domain(magna_carta_liberties, "political/legal").

domain_priors:requires_active_enforcement(magna_carta_liberties).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_liberties, rebellious_barons).
narrative_ontology:constraint_beneficiary(magna_carta_liberties, free_men_charter_holders).
narrative_ontology:constraint_victim(magna_carta_liberties, royal_prerogative).
narrative_ontology:constraint_victim(magna_carta_liberties, unfree_peasants).
narrative_ontology:constraint_victim(magna_carta_liberties, jewish_creditors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNFREE PEASANTRY (SNARE) — The vast majority of England's population, bound to manorial holdings, remains outside the charter's protections. They bear extraction through labor obligations, feudal rents, and wardship exactions without recourse. The charter explicitly excludes them from 'free men' protections. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.54.
constraint_indexing:constraint_classification(magna_carta_liberties, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LESSER KNIGHTS AND FREEHOLDERS (TANGLED ROPE) — Middling landholders benefit from some charter protections (no arbitrary reliefs, defined wardship terms) but remain constrained by feudal hierarchy and royal authority. They gain coordination benefits (predictable tenure, due process for major disputes) while bearing costs of enforcement and royal resistance. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(magna_carta_liberties, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REBELLIOUS BARONS (ROPE) — The charter's primary architects and beneficiaries. Extract wealth protection and political consultation rights while presenting the charter as universal justice. Experience the constraint as coordinating their collective action against royal arbitrary power. Benefit from defined inheritance, relief terms, and guaranteed counsel before taxation. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.11. Moderate effective extraction because they also benefit from the coordination function they created.
constraint_indexing:constraint_classification(magna_carta_liberties, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ROYAL ADMINISTRATION (PITON) — King John initially accepts the charter as a forced peace agreement, then repudiates it. The charter becomes a performative document — repeated reissue (1217, 1225) and reinterpretation with minimal enforcement, maintained through institutional ritual and baronial pressure rather than functional restraint on royal authority. Theater_ratio=0.65 reflects that the charter persists largely as a claims document about justice rather than as an enforceable constraint. d≈0.10, f(d)≈-0.02, σ=1.0 → χ≈-0.01. The crown sees the charter as degraded restraint maintained through performance.
constraint_indexing:constraint_classification(magna_carta_liberties, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: JEWISH CREDITORS (SNARE) — The charter includes explicit extraction from Jewish moneylenders through debt forgiveness and wardship provisions that redirect revenue from Jewish estates to the crown and barons. This is a secondary extraction mechanism: Jewish creditors are trapped (restricted residential options, legal disabilities, economic dependence on credit markets) and explicitly named victims of the charter's financial redistribution. d≈0.90, f(d)≈1.35, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(magna_carta_liberties, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the charter is a hybrid: it coordinates baronial collective action against arbitrary royal prerogative (coordination function) while extracting from unfree peasants, Jews, and future royal authority (asymmetric extraction). The charter claims to establish universal principles of justice ('to no one will we sell, deny, or delay right or justice') while systematically excluding 95% of the population. This is the defining characteristic of tangled rope: genuine coordination mechanism + structural exclusion = hybrid. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.25.
constraint_indexing:constraint_classification(magna_carta_liberties, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_liberties_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_liberties, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_liberties, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_liberties, TR),
    TR >= 0.70.

:- end_tests(magna_carta_liberties_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The charter extracts wealth and prerogative from the crown (benefiting barons) and financial rights from Jews (through wardship and debt provisions) while consolidating feudal extraction against peasants. The net extraction is moderate because much of the baronial gain is legitimate first-mover power consolidation (solving the coordination problem of opposing royal arbitrariness), not pure rent-seeking. Suppression (0.72): High. Multiple mechanisms prevent alternatives or exit: unfree peasants have no legal recourse or mobility; Jews face legal disabilities and economic dependence; barons must maintain collective action to enforce the charter against royal resistance; the charter itself explicitly forbids alternative frameworks ('to no one will we... deny... right or justice'). Theater ratio (0.65): Moderate-high. The charter is repeatedly reissued with modifications, suggesting performative maintenance rather than stable enforcement. The gap between charter claims (universal justice) and reality (systematic exclusion) indicates that the charter functions partly as a claims document establishing baronial legitimacy rather than as a functional constraint on power.
 *
 * PERSPECTIVAL GAP:
 *   The baronial perspective sees coordination (Rope/Tangled Rope) — solving the collective action problem of opposing arbitrary royal power and establishing predictable tenure. The royal perspective sees degraded constraint (Piton) — the charter persists as a performance obligation while royal authority is gradually recovered. The peasant perspective sees exclusion (Snare) — no protection, no voice, extraction continues with explicit charter sanction. The Jewish perspective sees targeted extraction (Snare) — wealth confiscation through wardship and debt cancellation, justified by the charter's language. The analytical observer sees the charter as tangled rope: genuine coordination mechanism + structural extraction + exclusion + degradation. The perspectival gap reveals that the 'universal' language of the charter masks a coalition-specific instrument.
 *
 * DIRECTIONALITY LOGIC:
 *   Rebellious barons: Beneficiary + constrained → d≈0.35, f(d)≈0.30. Moderate effective extraction because they also benefit from the coordination function. Unfree peasants: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — no exit, explicitly excluded. Lesser freeholders: Mixed (beneficiary of some protections + constrained) → d≈0.60, f(d)≈0.85. Moderate extraction. Jewish creditors: Victim + trapped → d≈0.90, f(d)≈1.35. High extraction through targeted debt provisions. King John: Institutional + arbitrage → d≈0.10, f(d)≈-0.02. Initially forced into victim role but rapidly recovers prerogative through reinterpretation. Analytical observer: analytical → d≈0.50, f(d)≈0.65. Symmetric position — sees both coordination and extraction clearly.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the Magna Carta is the tension between the charter's explicit claim to universal justice and its systematic exclusion of the majority population. The charter asserts 'To no one will we sell, deny, or delay right or justice' — a universal principle — while explicitly limiting 'justice' to 'free men,' a category that excludes perhaps 95% of England's population (unfree peasants, Jews, women, non-landholders). This is not a misreading or later corruption: the charter itself defines the exclusion. The resolution is that the Magna Carta is genuinely a tangled rope from the analytical perspective: it is simultaneously (1) a real coordination mechanism solving the baronial collective action problem, (2) an asymmetric extraction mechanism benefiting barons while consolidating peasant subjection, and (3) a claims document establishing the ideology of 'rule of law' that will eventually expand (1381 Peasants' Revolt invokes the charter for peasant protections). The mandatrophy is not 'is it coordination or extraction?' but 'for whom?' The charter works as coordination for barons and as extraction for peasants and Jews. Declaring both beneficiaries and victims resolves the mandatrophy: it is hybrid because it serves both functions simultaneously for different populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_enforcement_mechanism,
    'Was the Magna Carta ever functionally enforced as a binding constraint on royal authority, or did it remain a performative document?',
    'Comparative analysis of charter terms vs. documented royal behavior (1215-1265); tracking of reissues and modifications; correlation between charter violations and baronial response',
    'If functionally enforced: tangled rope classification holds, charter represents genuine hybrid. If purely performative: piton classification dominates, charter is inertial theater. The 1265 Mise of Lewes and barons'' wars suggest partial enforcement, but enforcement degrades by 1300.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_enforcement_mechanism, empirical, 'Whether Magna Carta functioned as binding constraint or performative document').

omega_variable(
    peasant_exclusion_intentionality,
    'Is the charter''s exclusion of unfree peasants a deliberate extraction mechanism or merely a reflection of contemporary feudal hierarchy?',
    'Textual analysis of charter''s language (''free men''); comparison with earlier royal documents; examination of whether alternative formulations were debated; tracking of peasant revolts'' charter invocation (1381 Peasants'' Revolt explicitly claims charter protections)',
    'If deliberate extraction: snare classification for peasants is justified; charter is weaponized hierarchy. If unreflective: charter is tangled rope that could expand (and eventually does expand in 1381 reinterpretation)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_exclusion_intentionality, conceptual, 'Whether peasant exclusion was intentional extraction or unreflective of feudal norms').

omega_variable(
    jewish_debt_provision_motive,
    'Do the charter''s provisions on Jewish debt forgiveness target Jewish creditors specifically as extraction victims, or are they collateral consequences of broader feudal reform?',
    'Comparison of Jewish provisions with earlier royal debt policy; tracking of crown revenue from Jewish wardship 1215-1230; examination of whether similar provisions applied to Christian creditors',
    'If targeted extraction: charter is explicitly a multi-victim snare for Jews. If incidental: charter is tangled rope with embedded anti-Semitic effect but without intentional targeting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_debt_provision_motive, conceptual, 'Whether Jewish creditors were deliberately targeted by charter extraction provisions').

omega_variable(
    charter_reissue_authenticity,
    'Do the 1217 and 1225 reissues represent genuine baronial enforcement or erosion of the charter through royal manipulation?',
    'Textual comparison of three versions; tracking of which provisions were dropped, modified, or enforced; analyzing who requested reissues and under what pressure; examining 1265 Mise of Lewes for reference back to original terms',
    'If genuine enforcement: charter shows increasing constraint power (tangled rope strengthening). If erosion: charter shows piton degradation (theater increasing as function declines)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_reissue_authenticity, empirical, 'Whether charter reissues strengthened or weakened constraints on royal authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_liberties, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magc_tr_t0, magna_carta_liberties, theater_ratio, 0, 0.35).
narrative_ontology:measurement(magc_tr_t5, magna_carta_liberties, theater_ratio, 5, 0.5).
narrative_ontology:measurement(magc_tr_t10, magna_carta_liberties, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(magc_be_t0, magna_carta_liberties, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(magc_be_t5, magna_carta_liberties, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(magc_be_t10, magna_carta_liberties, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_liberties, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_liberties, feudal_tenure_hierarchy).
narrative_ontology:affects_constraint(magna_carta_liberties, baronial_collective_action_problem).

% DUAL FORMULATION NOTE:
% The Magna Carta operates between two structural levels: (1) coordination among barons to solve their collective action problem (organizing against arbitrary royal power), and (2) consolidation of feudal extraction against peasants and Jews. These are not two observations of the same constraint but two aspects of a hybrid constraint. The charter's claim to universalism masks this hybridity, which is why it persists — the universalist ideology conceals the specific beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_liberties, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
