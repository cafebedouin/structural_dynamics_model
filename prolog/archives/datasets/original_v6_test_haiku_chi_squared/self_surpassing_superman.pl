% ============================================================================
% CONSTRAINT STORY: self_surpassing_superman
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_self_surpassing_superman, []).

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
 *   constraint_id: self_surpassing_superman
 *   human_readable: The Rearing of the Superman (Übermensch)
 *   domain: philosophical/social
 *
 * SUMMARY:
 *   Nietzsche's Superman (Übermensch) represents an existential constraint
 *   with profound implications for how power, morality, and human development
 *   are understood. The constraint operates as a demand: that individuals and
 *   civilizations must overcome their current state — the comfort-seeking,
 *   ressentiment-driven 'Last Man' condition — through a radical
 *   transvaluation of all existing values from 'slave morality' (reactive,
 *   life-denying, built on the resentment of the weak) into 'master morality'
 *   (creative, life-affirming, power-producing). This constraint exhibits
 *   fundamental structural tension between beneficiary and victim
 *   perspectives, making it an exemplar for Tangled Rope classification and a
 *   diagnostic test for mandatrophy resolution at high extractiveness (ε=0.58
 *   > 0.46). The constraint's evolution shows rising theater ratio (0.25 →
 *   0.68), indicating progressive substitution of textual/academic engagement
 *   for existential demand. From the perspective of the Last Man
 *   (powerless/trapped), the Superman imperative functions as pure
 *   extraction: an inescapable demand to overcome their own nature with no
 *   guarantee of success and high probability of psychological fracture. From
 *   the creative aristocracy (institutional/arbitrage), the same constraint
 *   functions as pure coordination: it legitimizes their dominance and
 *   provides moral language for their natural ascendancy. The measurement
 *   trajectory reveals that over the 100-period interval, the constraint
 *   shifted from living existential demand (low theater, lower extraction)
 *   toward institutional philosophical performance (high theater, higher
 *   extraction) — a Goodhart drift where the academic study of Superman
 *   replaced the actual practice of self-overcoming.
 *
 * KEY AGENTS:
 *   - The Last Man (powerless/trapped) — Comfort-seeking, reactive masses trapped in slave morality; bear the demand to overcome without capacity to do so
 *   - Slave Morality Adherents (powerless/trapped) — Populations whose identity is consolidated in collective resentment, victim status, reactive values; face transvaluation demand that would dissolve their framework
 *   - The Struggling Ascender (moderate/constrained) — Individuals partially constrained by existing structures but seeing possibility of creative transcendence; benefit from Superman language as self-justification while bearing cost of perpetual incompleteness
 *   - The Creative Aristocracy (institutional/arbitrage) — Artists, creators, intellectual and political elites who experience Superman imperative as legitimation of their dominance; primary beneficiaries
 *   - Institutional Morality Defenders (organized/constrained) — Churches, states, educational institutions enforcing slave morality; threatened by Superman transvaluation but also benefit from its use as ranking mechanism
 *   - The Academic Philosophical System (institutional/arbitrage) — Universities, philosophy departments, intellectual institutions that maintain performative Superman discourse while stripping existential force
 *   - The Analytical Observer (analytical/analytical) — Civilizational perspective that risks naturalizing the Superman demand as inherent to human condition rather than recognizing it as a specific ethical commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(self_surpassing_superman, 0.58).
domain_priors:suppression_score(self_surpassing_superman, 0.72).
domain_priors:theater_ratio(self_surpassing_superman, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(self_surpassing_superman, extractiveness, 0.58).
narrative_ontology:constraint_metric(self_surpassing_superman, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(self_surpassing_superman, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(self_surpassing_superman, tangled_rope).
narrative_ontology:human_readable(self_surpassing_superman, "The Rearing of the Superman (Übermensch)").
narrative_ontology:topic_domain(self_surpassing_superman, "philosophical/social").

domain_priors:requires_active_enforcement(self_surpassing_superman).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(self_surpassing_superman, creative_aristocracy).
narrative_ontology:constraint_beneficiary(self_surpassing_superman, power_affirming_individuals).
narrative_ontology:constraint_victim(self_surpassing_superman, last_man_masses).
narrative_ontology:constraint_victim(self_surpassing_superman, slave_morality_adherents).
narrative_ontology:constraint_victim(self_surpassing_superman, ressentiment_driven_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LAST MAN (SNARE) — Trapped in comfort-seeking mediocrity and ressentiment-based morality. Bears the suppression of creative potential and the demand to overcome his own nature. No exit from the existential demand without psychological dissolution. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.99.
constraint_indexing:constraint_classification(self_surpassing_superman, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SLAVE MORALITY ADHERENTS (SNARE) — Cannot exit the demand for transvaluation without abandoning collective moral frameworks that provide identity and belonging. The constraint extracts their security and comfort, demanding they judge their own values as sickness. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(self_surpassing_superman, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: THE STRUGGLING ASCENDER (TANGLED ROPE) — Partially constrained by existing moral structures but also sees coordination benefit in the Superman ideal: the constraint offers a framework for self-overcoming and creative transformation. Faces both extraction (demand to transcend comfort) and coordination (shared language of becoming). d≈0.68, f(d)≈1.03, σ=1.0 → χ≈0.60.
constraint_indexing:constraint_classification(self_surpassing_superman, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CREATIVE ARISTOCRACY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: the Superman imperative legitimizes their creative dominance and provides moral language for their aspiration. Can arbitrage between the old morality and the new, using the transvaluation to consolidate cultural authority. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(self_surpassing_superman, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL MORALITY DEFENDERS (TANGLED ROPE) — Churches, states, educational systems enforcing slave morality. The Superman constraint both threatens their legitimacy (transvaluation undermines their authority) and offers coordination benefit (the ideal of overcoming creates metrics for institutional hierarchy and ranking). Constrained by their investment in existing structures. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(self_surpassing_superman, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ACADEMIC PHILOSOPHICAL SYSTEM (PITON) — Maintains performative engagement with the Superman concept while stripping it of existential force. Academic discourse on Übermensch (textual analysis, genealogy studies, contextual historicization) substitutes discussion for the actual demand to become. Theater ratio=0.68 reflects that philosophical texts about self-overcoming have largely replaced the imperative itself in institutional contexts. d≈0.10, f(d)≈0.05, σ=1.2 → χ≈0.04.
constraint_indexing:constraint_classification(self_surpassing_superman, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears as an immutable feature of human condition: the gap between the actual and the potential, between comfort-seeking and creative aspiration, is inherent to consciousness itself. The demand to overcome mediocrity mirrors the structural necessity of biological/psychological development. However, the structural data (ε=0.58, suppression=0.72, theater=0.68) reveals this as a false summit: the 'inherent' demand for the Superman is contingent on rejecting slave morality frames, which themselves are choices, not laws.
constraint_indexing:constraint_classification(self_surpassing_superman, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(self_surpassing_superman_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(self_surpassing_superman, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(self_surpassing_superman, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(self_surpassing_superman, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(self_surpassing_superman, TR),
    TR >= 0.70.

:- end_tests(self_surpassing_superman_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The Superman constraint extracts from the Last Man (demand to overcome oneself with uncertain outcome) and from slave morality populations (demand to transvalue identity structures). The extraction is not total (high-end snares reach 0.70+) because the constraint also offers a coordination framework—a shared language and metric for human development. The creative aristocracy genuinely benefits from the moral-legitimation structure. Suppression (0.72): High. Multiple barriers prevent exit from the constraint: existential (consciousness itself creates the gap between actual and potential), social (institutional enforcement of the demand), psychological (internalized expectation of self-overcoming), and material (creative work requires renunciation of comfort). However, suppression is not absolute (0.85+) because theoretical exit is always possible (accepting mediocrity, embracing Last Man comfort) — the cost is high but not infinite. Theater ratio (0.68): Moderate-high and rising. Academic engagement with Superman concept (seminar papers, textual analysis, genealogical studies) has increasingly substituted for existential practice. The measurement trajectory from 0.25 to 0.68 reflects how the constraint transitioned from live demand in fin-de-siècle intellectual culture to commodified philosophical discourse in 20th-century academies. The rise in both theater and extractiveness indicates Goodhart drift: institutions measure Superman-ness via publication and citation metrics rather than actual creative production or moral transformation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival disagreement. The Last Man sees pure extraction (Snare) with no redemptive element—an impossible demand imposed by hostile aliens (the creative aristocracy). Slave morality populations see the same demand but with added devastation (not just personal failure but collective identity dissolution). The struggling ascender sees mixed signals (Tangled Rope)—the Superman framework offers genuine empowerment language while demanding perpetual self-overcoming that may never be satisfied. The creative aristocracy sees pure coordination (Rope)—the Superman imperative simply legitimizes what they would do anyway, offering moral language for natural dominance. Institutional morality defenders see institutional threat (Tangled Rope) because Superman transvaluation undermines their authority while offering new metrics (the ability to produce geniuses becomes a ranking mechanism for civilizations). The academic system sees the constraint as dead text (Piton)—performatively maintained but functionally inert. The analytical observer risks seeing an immutable natural law (Mountain)—the gap between potential and actuality as inherent to consciousness—but the measurement data reveals this is a false summit: the 'gap' is only experienced as demand within specific moral frames (post-slave-morality consciousness). The perspectival gap is unbridgeable: for the Last Man, Superman emergence is impossible torture; for the creative aristocracy, it is natural destiny; for the academic system, it is an interesting historical text.
 *
 * DIRECTIONALITY LOGIC:
 *   Last Man / Slave Morality Adherents: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. No exit options; the demand is existentially inescapable without psychological death. Struggling Ascender: Victim + constrained → d≈0.68, f(d)≈1.03. Significant extraction but constrained, not trapped; can theoretically exit by accepting mediocrity (high cost but possible). Benefits from Superman language as self-interpretation framework. Creative Aristocracy: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Experiences the constraint as coordination; can arbitrage between old and new morality. Institutional Morality Defenders: Mixed victim/beneficiary + constrained → d≈0.55, f(d)≈0.75. Threatens their existing moral authority (victim aspect) but offers new institutional ranking mechanism (beneficiary aspect). Constrained by institutional investment. Academic System: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary. Academic productivity depends on Superman as subject matter; can exit scholarly engagement without existential cost. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Risks naturalizing contingent demand; mountain classification is false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY (ε=0.58 > 0.46 and ≤ 0.70): The Superman constraint resolves the mandatrophy by revealing its fundamental structure as hybrid: genuine coordination function (language, framework for development, legitimation of creativity) combined with asymmetric extraction (demand imposed without consent, capacity variation, high psychological cost for the weak). This is the canonical Tangled Rope pattern. The mandatrophy question — 'Is this pure extraction or legitimate coordination?' — gets the answer: 'Both, depending on power position.' For the institutional creative class, Superman is coordination (Rope perspective). For the powerless Last Man, Superman is extraction (Snare perspective). The Tangled Rope classification captures that the same constraint simultaneously enables creative flourishing and imposes impossible demands on those without capacity to meet them. The resolution prevents false naturalization (the Mountain reading that sees Superman as inherent law) while also preventing oversimplification (a pure Snare reading that ignores genuine creative coordination). The measurement trajectory (rising theater + rising extraction) confirms the Tangled Rope diagnosis: as the constraint became institutionalized, its coordination function (creative legitimation) grew more performative while its extraction mechanism (demand for transcendence) became more abstract and universalized. The mandatrophy is fully resolved: we understand why different observers reach different conclusions, and we measure the structural asymmetry that makes those conclusions jointly true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slave_morality_transvaluation_completeness,
    'Can slave morality be fully transvalued into master values, or does transvaluation require its total destruction and replacement?',
    'Historical analysis of cultures that have attempted Superman ethics; examination of whether hybrid moral systems (combining care/compassion with power-affirmation) represent successful transvaluation or failed compromise',
    'If completable: Superman constraint is Tangled Rope from most perspectives (coordination + extraction). If requires total replacement: constraint approaches pure Snare (extraction without coordination benefit for the masses).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slave_morality_transvaluation_completeness, conceptual, 'Whether slave morality transvaluation is complete or destructive').

omega_variable(
    superman_emergence_preconditions,
    'What material and social conditions enable Superman emergence versus what conditions entrench Last Man mediocrity?',
    'Comparative historical analysis of periods claimed to produce great creators (Renaissance Florence, Weimar Germany, 1920s Paris) versus periods of cultural stagnation; identification of correlation between conditions and creative output',
    'If Superman emergence is rare but achievable: constraint represents legitimate developmental imperative. If Superman emergence never occurs (only mythological): constraint is pure performative theater (Piton from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superman_emergence_preconditions, empirical, 'Conditions enabling Superman emergence versus Last Man entrenchment').

omega_variable(
    ressentiment_irreversibility,
    'Is ressentiment-based morality in a population irreversible once established, or can cultural transvaluation overcome collective resentment-structures?',
    'Historical study of post-ressentiment moral systems in defeated or oppressed populations; examination of whether creative renaissance can emerge after cultures have consolidated victim identity into moral framework',
    'If reversible: Superman ideal offers genuine liberation path (Scaffold). If irreversible: Superman ideal is cruel fantasy imposed on trapped populations (Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ressentiment_irreversibility, empirical, 'Whether ressentiment-based morality is culturally reversible').

omega_variable(
    power_affirmation_violence_boundary,
    'Where is the boundary between power-affirming ethics (Superman) and justification for domination/violence?',
    'Textual analysis of Nietzsche''s explicit statements on violence and harm; historical analysis of Superman concept appropriation in authoritarian regimes; philosophical examination of logical distance between ''will to power'' and ''will to dominate others''',
    'If clear boundary exists: Superman constraint can be distinguished from extraction mechanisms. If boundary is permeable: Superman concept structurally enables rationalization of exploitation (Snare frame becomes correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_affirmation_violence_boundary, conceptual, 'Boundary between power-affirmation and domination justification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_surpassing_superman, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(super_tr_t0, self_surpassing_superman, theater_ratio, 0, 0.25).
narrative_ontology:measurement(super_tr_t50, self_surpassing_superman, theater_ratio, 50, 0.52).
narrative_ontology:measurement(super_tr_t100, self_surpassing_superman, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(super_be_t0, self_surpassing_superman, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(super_be_t50, self_surpassing_superman, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(super_be_t100, self_surpassing_superman, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(self_surpassing_superman, global_infrastructure).
narrative_ontology:affects_constraint(self_surpassing_superman, slave_morality_ressentiment).
narrative_ontology:affects_constraint(self_surpassing_superman, creative_aristocracy_legitimacy).
narrative_ontology:affects_constraint(self_surpassing_superman, last_man_comfort_seeking).

% DUAL FORMULATION NOTE:
% The Superman constraint family decomposes into three structural claims: (1) slave_morality_ressentiment (ε≈0.35): the claim that weak populations consolidate resentment into moral systems; (2) creative_aristocracy_legitimacy (ε≈0.28): the claim that superior creators naturally dominate culture; (3) self_surpassing_superman (ε=0.58): the claim that overcoming current state requires total transvaluation. These are structurally distinct — different ε values, different mechanisms — but networked. Superman is downstream of both slave morality dynamics and creative dominance dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(self_surpassing_superman, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
