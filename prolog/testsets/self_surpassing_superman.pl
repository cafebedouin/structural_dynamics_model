% ============================================================================
% CONSTRAINT STORY: self_surpassing_superman
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: The Rearing of the Superman (Übermensch) — Self-Surpassing Imperative
 *   domain: philosophical/social
 *
 * SUMMARY:
 *   The Übermensch doctrine creates a structural constraint that operates
 *   across two incompatible populations: the Last Man (slave morality
 *   adherent) who experiences it as existential extraction, and the aspiring
 *   overman who experiences it as productive demand. The constraint
 *   originated as a philosophical imperative to overcome Christian morality's
 *   revaluation of weakness as virtue. In the 120+ years since Nietzsche, the
 *   doctrine has been progressively absorbed into institutional frameworks
 *   (academia, artistic establishments, self-help industries) where its
 *   radical content has been substantially neutralized. The constraint now
 *   functions as a piton — performatively invoked (self-surpassing sounds
 *   profound) but largely inert as an actual demand for transvaluation. Yet
 *   for the aspiring overman, the demand retains existential force: it
 *   structures creative practice, generates meaning in a post-religious
 *   world, and justifies the isolation and anxiety that accompany genuine
 *   intellectual work. For the Last Man, the constraint functions as a snare:
 *   the herd morality that once provided comfort is now explicitly condemned
 *   as mediocrity, but the alternative (genuine self-surpassing) requires
 *   abandoning the only moral framework available. The extraction accelerates
 *   over the measurement interval as institutional appropriation increases
 *   (theater ratio rises from 0.35 to 0.64) while the actual demand for
 *   self-surpassing becomes more diffuse and performative (extractiveness
 *   rises from 0.55 to 0.68 because mediocrity itself becomes shameful
 *   without genuine alternative paths to dignity).
 *
 * KEY AGENTS:
 *   - Last Man Population: Primary victim (powerless/trapped) — bears extraction through constant exposure to condemnation of their mediocrity; cannot exit without existential rupture; slave morality offers no dignity within the new framework
 *   - Aspiring Overman: Secondary victim and partial beneficiary (moderate/constrained) — experiences constraint as both creative enabling (coordination) and existential isolation (extraction); constrained exit because abandoning the quest means regression to herd
 *   - Master Value System: Primary beneficiary (institutional/arbitrage) — abstract system that is validated and propagated by the constraint; experiences pure coordination (organizing human excellence); no victims from this perspective
 *   - Cultural Elite (Artists/Philosophers): Organized beneficiary (organized/constrained) — benefits from the constraint's elevation of excellence and justification of elite status; extracted from by permanent demand for surpassing; constrained exit because loss of elite identity
 *   - Institutional Nietzsche Apparatus: Parasitic institutional actor (institutional/arbitrage) — universities, publishers, cultural authorities that extract legitimacy from Nietzschean language while performing the constraint rather than embodying it
 *   - Post-Religious Society: Civilizational context (analytical/analytical) — the Death of God created the vacuum that the self-surpassing imperative fills; without that narrative, the constraint has no structural force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(self_surpassing_superman, 0.68).
domain_priors:suppression_score(self_surpassing_superman, 0.72).
domain_priors:theater_ratio(self_surpassing_superman, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(self_surpassing_superman, extractiveness, 0.68).
narrative_ontology:constraint_metric(self_surpassing_superman, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(self_surpassing_superman, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(self_surpassing_superman, snare).
narrative_ontology:human_readable(self_surpassing_superman, "The Rearing of the Superman (Übermensch) — Self-Surpassing Imperative").
narrative_ontology:topic_domain(self_surpassing_superman, "philosophical/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(self_surpassing_superman, master_value_practitioners).
narrative_ontology:constraint_victim(self_surpassing_superman, slave_morality_adherents).
narrative_ontology:constraint_victim(self_surpassing_superman, last_man_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LAST MAN (SNARE) — Trapped in comfortable mediocrity enforced by herd morality. Cannot exit without psychological rupture and social exile. The constraint demands constant self-negation as the price of existence. No alternative framework available to this agent except the transcendence that requires abandoning their fundamental values. Maximum extraction: the entire interior life must be reorganized or permanently suppressed.
constraint_indexing:constraint_classification(self_surpassing_superman, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE ASPIRING OVERMAN (TANGLED ROPE) — Experiences the constraint as both coordination and extraction. The demand for self-surpassing creates genuine creative productivity and meaning-making (coordination benefit), but also requires isolation from herd support systems, constant anxiety, and existential risk. The aspiring overman benefits from the constraint's productivity-generating function while suffering its isolating effects. Constrained exit: abandoning the quest means retreating to herd mediocrity, but continuing means permanent liminality.
constraint_indexing:constraint_classification(self_surpassing_superman, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE MASTER VALUE SYSTEM (ROPE) — Abstract beneficiary. The constraint's demand for self-surpassing creates a coordination function: it organizes creative work, establishes hierarchies of excellence, and generates artistic/intellectual productivity. Master values experience the constraint as pure coordination — no extraction, only the mobilization of potential. Arbitrage exit available: master values can be applied selectively to favor particular practitioners or domains.
constraint_indexing:constraint_classification(self_surpassing_superman, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CULTURAL ELITE (TANGLED ROPE) — Organized practitioners who benefit from the constraint's demand for excellence (coordination: it justifies their elite status and patronage). But also extracted from by the same demand: the pressure to constantly surpass oneself, the social burden of representing excellence, the obligation to create meaning in a post-religious void. Constrained exit: abandoning the constraint means loss of cultural authority and legitimacy, but continuing means permanent anxiety and performance demand.
constraint_indexing:constraint_classification(self_surpassing_superman, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: THE NIETZSCHE INSTITUTIONAL APPARATUS (PITON) — The constraint has become largely performative: universities teach 'self-surpassing' as an intellectual virtue, self-help literature commodifies the overman ideal, motivational discourse invokes master values without structural commitment. The actual philosophical demand (transvaluation of all values, rejection of herd morality) has been absorbed into the existing system as performance. Theater ratio is high because the institutional apparatus extracts legitimacy from Nietzschean language while neutralizing its radical content. The apparatus maintains the constraint through cultural inertia — Nietzsche is canonical, so self-surpassing sounds profound — even though the actual work of transvaluation is optional.
constraint_indexing:constraint_classification(self_surpassing_superman, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a timeless perspective, the constraint might appear as a natural law of human development: 'humans must overcome themselves or stagnate.' This naturalizes what is actually a contingent philosophical/cultural demand rooted in Nietzsche's rejection of Christian morality. The engine's false summit detector identifies this as a naturalization fallacy — the constraint is not immutable; it is a specific cultural-historical formation that depends on the prior 'Death of God' narrative. Without that narrative framework, the demand for self-surpassing has no force.
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
 *   Extractiveness (0.68): High. The constraint extracts value from the Last Man (he must endure condemnation of his only available moral framework) and from the aspiring overman (she must surrender social belonging and psychological comfort for the chance to create). The extraction increases over time as institutional appropriation makes mediocrity shameful without providing genuine alternative paths to dignity. The measurement interval shows extractiveness rising from 0.55 to 0.68 as the constraint becomes more culturally pervasive and less structurally supportive. Suppression (0.72): High. Significant barriers to actual self-surpassing include: (1) the unavailability of concrete criteria (what counts as surpassing?), (2) social isolation and exile from herd support, (3) existential anxiety and risk of self-destruction, (4) the institutional neutralization that makes it impossible to practice genuine transvaluation within established structures. The Last Man has no exit; the aspiring overman has only constrained exit. Theater ratio (0.64): Moderate-high. The constraint has been substantially absorbed into institutional performance — universities teach it, corporations invoke it in motivational language, self-help books commodify it. The radical philosophical content (transvaluation of all values, rejection of herd morality entirely) is almost entirely absent from institutional practice. Yet the performative invocation (calling oneself 'committed to excellence,' 'pushing boundaries,' 'surpassing limits') has become ubiquitous. The measurement shows theater rising from 0.35 to 0.64 as the decades pass, indicating progressive institutional routinization.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between snare (Last Man), tangled rope (aspiring overman and cultural elite), rope/coordination (master values), and piton (institutional apparatus) reflects the constraint's structural inhomogeneity. The same demand for self-surpassing classifies differently depending on the agent's power, exit options, and beneficiary/victim status. This gap is not resolvable by clarification — it reflects genuine structural differences in how the constraint operates. The Last Man truly is trapped in a snare; the aspiring overman truly does experience tangled rope; the institutional apparatus truly has routinized it into piton. The analyst risks collapsing this gap by treating the constraint as a unified natural law rather than as a complex social structure with multiple victim classes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the self-surpassing demand. The Last Man has high d (d ≈ 0.95): trapped exit + victim status → full target, experiencing maximum extraction f(d) ≈ 1.42. The aspiring overman has moderate d (d ≈ 0.50): constrained exit + mixed beneficiary-victim status → symmetric position, experiencing moderate extraction f(d) ≈ 0.65. The master value system has low d (d ≈ 0.05): arbitrage exit + pure beneficiary status → almost no extraction, f(d) ≈ -0.12. The cultural elite have moderate-high d (d ≈ 0.60): constrained exit + both beneficiary and victim status → f(d) ≈ 0.80. The institutional apparatus has very low d (d ≈ 0.02): arbitrage exit + beneficiary status (legitimacy extracted) → f(d) ≈ -0.15. The analytical observer has high analytical d (d ≈ 0.73): analytical exit and analytical power → f(d) ≈ 1.15. These derived d values show why the Last Man experiences the constraint as a snare (high f(d) amplifies χ) while the institutional apparatus experiences it as a rope or piton (low f(d) dampens χ despite high base extractiveness).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint must be classified as snare rather than falsely naturalized as mountain because the empirical reality shows: (1) High suppression (0.72) — not the zero-suppression of natural law, but contingent social barriers; (2) Increasing theater ratio (0.35→0.64) — natural laws do not become increasingly performative; (3) Multiple extractive mechanisms — the Death of God narrative is historically contingent, not universal; (4) The false summit: the analytical observer's 'natural law' reading naturalizes what is actually institutional appropriation and social extraction. The snare classification is confirmed by the measurement trajectory: as institutional apparatus absorbs the constraint (theater increases), the actual existential force on the Last Man increases (extractiveness rises from 0.55 to 0.68), indicating that performance substitutes for genuine alternative pathways. The tangled rope classifications for aspiring overman and cultural elite are genuine — they do experience coordination benefits (meaning-making, creative enabling) alongside extraction (isolation, permanent anxiety). The piton classification for the institutional apparatus is confirmed: the constraint persists through cultural inertia (Nietzsche is canonical) even as its radical content is systematically neutralized. The constraint is NOT a natural law; it is a high-extraction snare that has been progressively pacified into a piton for institutional actors while remaining existentially destructive for the Last Man.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    who_defines_surpassing,
    'Who determines what constitutes ''surpassing'' oneself, and according to which criteria? Is surpassing defined by the individual agent, by cultural standards, or by objective excellence metrics?',
    'Genealogical analysis of how ''surpassing'' has been operationalized across different Nietzschean movements (artistic, athletic, military, academic); examination of whether the criteria are self-set or externally imposed',
    'If self-defined: constraint is more rope-like (coordination around agent''s own values). If externally imposed: constraint is more snare-like (external demand that agent must accept). If determined by ''objective'' master values: reveals the hidden normativity in supposedly value-neutral excellence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_defines_surpassing, conceptual, 'Definition authority for what constitutes self-surpassing').

omega_variable(
    death_of_god_dependency,
    'Does the constraint''s force depend on the prior ''Death of God'' narrative (Christian morality has failed)? If God were alive or alternative moral systems available, would the demand for self-surpassing retain its existential power?',
    'Historical comparison with pre-Nietzschean self-improvement traditions (Stoicism, Confucianism, monastic asceticism); analysis of whether cultures with living religious frameworks experience the self-surpassing imperative differently',
    'If dependent: the constraint is culturally contingent, not universal; classification shifts from mountain toward snare (contingent extraction masquerading as natural law). If independent: self-surpassing is a trans-cultural human demand; mountain classification gains support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(death_of_god_dependency, conceptual, 'Whether constraint force depends on Death of God narrative').

omega_variable(
    extraction_vs_agency,
    'Does the constraint extract value from the Last Man (they must surrender their mediocre comfort) or enable agency for the aspiring overman (they gain power to self-create)? Are these the same person at different times, or structurally different agents?',
    'Psychological and sociological data on which populations experience the self-surpassing demand as productive agency vs as destructive self-negation; longitudinal tracking of whether individuals who internalize the constraint report well-being gains or existential distress',
    'If extraction dominates: snare classification confirmed (majority population harmed). If agency dominates: rope or scaffold classification (enabling structure). If mixed by agent type: tangled rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_agency, empirical, 'Whether constraint enables or extracts value').

omega_variable(
    institutional_neutralization,
    'Can the institutional apparatus (universities, publishing, cultural institutions) ever commit genuinely to the constraint''s radical content (transvaluation of all values, rejection of herd morality), or does professionalization necessarily neutralize it into performative virtue?',
    'Historical analysis of institutional Nietzscheanism: do philosophy departments actually practice transvaluation or teach it as historical content? Do elite institutions genuinely reject herd morality or merely aestheticize it? Examination of whether any institutional apparatus has successfully embodied radical self-surpassing without routinization.',
    'If neutralization is inevitable: piton classification is structural (performance inertia is permanent). If genuine institutional commitment possible: scaffold classification (temporary institution with sunset as radical commitment deepens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_neutralization, conceptual, 'Whether institutional apparatus can genuinely embody radical content').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_surpassing_superman, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(self_tr_t0, self_surpassing_superman, theater_ratio, 0, 0.35).
narrative_ontology:measurement(self_tr_t50, self_surpassing_superman, theater_ratio, 50, 0.52).
narrative_ontology:measurement(self_tr_t100, self_surpassing_superman, theater_ratio, 100, 0.64).

% Extraction over time
narrative_ontology:measurement(self_be_t0, self_surpassing_superman, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(self_be_t50, self_surpassing_superman, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(self_be_t100, self_surpassing_superman, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(self_surpassing_superman, information_standard).
narrative_ontology:affects_constraint(self_surpassing_superman, death_of_god_narrative).
narrative_ontology:affects_constraint(self_surpassing_superman, master_slave_morality_antithesis).
narrative_ontology:affects_constraint(self_surpassing_superman, creative_destruction_ethic).

% DUAL FORMULATION NOTE:
% The self-surpassing constraint is downstream of the Death of God narrative (parent constraint) — without the prior collapse of Christian moral authority, the demand for self-surpassing has no existential force. It is also structurally coupled to the master-slave morality antithesis (the transvaluation of values requires both critique of slave morality and assertion of master values). Both parent constraints should be analyzed separately with their own ε values to show how the self-surpassing imperative emerges from their combination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(self_surpassing_superman, analytical, 0.73).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
