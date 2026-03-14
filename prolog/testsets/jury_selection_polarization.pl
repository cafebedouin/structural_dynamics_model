% ============================================================================
% CONSTRAINT STORY: jury_selection_polarization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jury_selection_polarization, []).

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
 *   constraint_id: jury_selection_polarization
 *   human_readable: Jury Selection Polarization in Political Trials
 *   domain: legal/political/institutional
 *
 * SUMMARY:
 *   Jury selection polarization in politically charged trials creates a
 *   structural tension between the jury system's coordination function
 *   (trial-by-peer, evidence presentation, community legitimacy) and the
 *   capture of that coordination by partisan epistemology. Political
 *   polarization has systematized the biasing mechanisms of jury selection:
 *   prosecution uses peremptory challenges and questioning to identify
 *   politically aligned jurors; venue selection concentrates defendants from
 *   opposing factions into politically hostile communities; juror identity
 *   fusion with political tribes transforms evidence evaluation into identity
 *   confirmation. The constraint shows increasing extractiveness over the
 *   measurement interval (0.35 → 0.58), reflecting both the rising salience
 *   of political trials and the optimization of partisan jury capture
 *   strategies. Theater ratio has risen (0.52 → 0.68), indicating that jury
 *   selection rituals (voir dire questions, jury instructions on
 *   impartiality) have become increasingly performative — they maintain the
 *   appearance of impartiality while systematically failing to detect or
 *   remove polarized jurors.
 *
 * KEY AGENTS:
 *   - Politically Disfavored Defendants: Primary victims (powerless/trapped) — systematically higher conviction risk due to juror polarization; no exit from venue or jury composition
 *   - Trial Legitimacy: Primary victim (institutional/trapped) — verdicts lose credibility when perceived as politically predetermined; abstract collective good that cannot organize
 *   - Jury Impartiality Norm: Primary victim (institutional/trapped) — foundational norm degraded to theatrical performance; cannot exit or adapt to polarized electorate
 *   - Prosecutorial State: Primary beneficiary (institutional/arbitrage) — captures conviction leverage through jury polarization; controls venue and juror challenge strategy; high arbitrage capacity
 *   - Partisan Political Actors: Secondary beneficiary (powerful/mobile) — use politicized trials for faction messaging and identity reinforcement; benefit from narrative control
 *   - Opposing Faction's Population: Secondary victim (moderate/constrained) — coerced jury participation; pressure to vote partisan lines; constrained by duty obligation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees tangled rope: genuine coordination infrastructure captured by partisan extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jury_selection_polarization, 0.58).
domain_priors:suppression_score(jury_selection_polarization, 0.62).
domain_priors:theater_ratio(jury_selection_polarization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jury_selection_polarization, extractiveness, 0.58).
narrative_ontology:constraint_metric(jury_selection_polarization, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jury_selection_polarization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jury_selection_polarization, tangled_rope).
narrative_ontology:human_readable(jury_selection_polarization, "Jury Selection Polarization in Political Trials").
narrative_ontology:topic_domain(jury_selection_polarization, "legal/political/institutional").

domain_priors:requires_active_enforcement(jury_selection_polarization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jury_selection_polarization, prosecutorial_state).
narrative_ontology:constraint_beneficiary(jury_selection_polarization, partisan_political_actors).
narrative_ontology:constraint_victim(jury_selection_polarization, defendants_from_opposing_political_faction).
narrative_ontology:constraint_victim(jury_selection_polarization, trial_legitimacy).
narrative_ontology:constraint_victim(jury_selection_polarization, jury_impartiality_norm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLITICALLY DISFAVORED DEFENDANT (SNARE) — Trapped by venue selection and juror polarization. Faces systematically higher conviction probability based on juror political alignment rather than evidence. Cannot exit the trial mechanism without abandoning legal defense. Bears full extraction cost: unfair verdict risk, reputational damage, and imprisonment despite reasonable doubt. Maximum experienced suppression — state controls venue, prosecution controls narrative framing, jury composition reflects partisan geography.
constraint_indexing:constraint_classification(jury_selection_polarization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSING FACTION'S POPULATION (TANGLED ROPE) — Constrained by jury duty obligation and geographic residence. Faces pressure to vote conviction/acquittal along partisan lines regardless of evidence. Extraction exists (coerced participation in biased verdict), but coordination function persists (jury system does provide trial-by-peer structure and requires some evidence presentation). Some exit costs (contempt risk, relocation burden) but not absolute trapping. Mixed experience: civic obligation meets political capture.
constraint_indexing:constraint_classification(jury_selection_polarization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROSECUTORIAL STATE (ROPE) — Benefits from juror polarization without formal responsibility. Experiences jury selection as coordination mechanism: identifying jurors aligned with state position and removing opposing jurors through challenges is efficient resource allocation. Net beneficiary through conviction leverage. High arbitrage capacity: state controls timing, venue, charges, and prosecution strategy. Extraction runs toward this actor.
constraint_indexing:constraint_classification(jury_selection_polarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARTISAN POLITICAL ACTORS (TANGLED ROPE) — Powerful actors using politicized trials as coordination mechanism for faction messaging and identity reinforcement. Genuine coordination function (rallying base around trials as political narratives) coexists with extraction from defendants and jury integrity. Mobile exit options (political activities, media focus, legislative action), but sustained investment in trial narratives suggests significant benefit flow. Moderate to high experienced extraction relative to their power level.
constraint_indexing:constraint_classification(jury_selection_polarization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JURY IMPARTIALITY NORM (PITON) — The foundational norm of jury impartiality persists as performative theater: voir dire questions about bias are ritualistic rather than effective at detecting or removing polarized jurors. Jury instructions to set aside partisan preferences are theater — they cannot rewire tribal identity or partisan epistemology. The norm is maintained through institutional inertia despite systematic degradation. Theater ratio reflects that jury selection rituals (questions, challenges, instructions) perform impartiality without achieving it.
constraint_indexing:constraint_classification(jury_selection_polarization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (CIVILIZATIONAL VIEW) — From a system-level perspective, jury selection polarization is a hybrid constraint: the jury system genuinely coordinates trial participation and evidence presentation (coordination function), but that coordination mechanism has been captured by partisan epistemology such that verdict distribution tracks political affiliation rather than evidence quality. The constraint requires active enforcement (jury selection rituals, instructions) to function at all, and enforcement is systematically biased. This is the canonical tangled rope: genuine coordination infrastructure overlaid with asymmetric extraction.
constraint_indexing:constraint_classification(jury_selection_polarization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jury_selection_polarization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jury_selection_polarization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jury_selection_polarization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jury_selection_polarization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jury_selection_polarization, TR),
    TR >= 0.70.

:- end_tests(jury_selection_polarization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The prosecution captures significant conviction leverage through systematic jury composition, but extraction is not maximal because: (1) evidence quality still influences outcomes (juries are not entirely partisan), (2) some cross-partisan conviction occurs, and (3) defense strategies can partially mitigate venue effects. The upward trajectory (0.35 → 0.58) reflects increasing sophistication of partisan jury capture over the past decade as political trial frequency has increased and prosecution strategies have optimized. Suppression (0.62): High. Multiple suppression mechanisms operate: venue concentration, juror identification bias (tribal epistemology makes partisan jurors invisible to themselves), jury duty obligation traps unwilling participants, peremptory challenge asymmetry, and public polarization that invades jury deliberation. Jurors cannot easily exit polarized identity frames; defendants cannot easily change venue or jury composition. Theater ratio (0.68): High and rising. Jury selection rituals (voir dire questioning, jury instructions on setting aside bias, peremptory challenges) perform impartiality without achieving it. The performance has increased as the system has become more visibly polarized — more extensive jury instructions and more careful questioning signal to observers that impartiality is being pursued, even as verdict distributions diverge sharply from evidence quality.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap exists between the prosecutorial state's Rope classification and the disfavored defendant's Snare classification. This gap reveals the asymmetric extraction captured by the tangled rope: the same jury selection mechanism that coordinates efficient trial participation also extracts systematically biased verdicts. The piton perspective on jury impartiality reveals that the system's legitimacy depends on theatrical maintenance of a norm that has ceased to function. The scaffold perspective would require venue reform (cross-state jury pools, professional jurors, or algorithmic jury composition) that depolarizes juries, but current structural data shows no such alternative pathway developing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values track the agent's structural relationship to the verdict-outcome distribution and their capacity to shape it. The prosecutorial state has d ≈ 0.10 (beneficiary with arbitrage exit) — they control venue, jury challenges, and charging decisions; they benefit from polarized convictions; they can pivot to different cases or prosecutorial strategies. Their derived d feeds into f(d) yielding negative or near-zero effective extraction chi — the constraint runs *toward* them. The disfavored defendant has d ≈ 0.95 (victim with trapped exit) — they bear the conviction risk, cannot change venue or jury composition meaningfully, and cannot exit the trial mechanism. Their derived d feeds into f(d) yielding maximum effective extraction chi. The partisan political actors have d ≈ 0.40 (beneficiary with mobile exit) — they benefit from trial narratives but can shift focus to other political activities; moderate experience of extraction benefit. The jury impartiality norm occupies an unusual position: it is a rule/principle, not an agent, but it is treated as a victim (d ≈ 0.90) because it is systematically violated. The opposing faction's population has d ≈ 0.72 (victim with constrained exit) — they bear jury duty costs and partisan pressure; significant extraction but with some agency through defense participation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing the jury system's genuine coordination function from its capture by partisan extraction. The jury system coordinates trial participation, evidence presentation, and peer verdict — these are real coordination services that the constraint provides. But that coordination infrastructure has been systematically captured: venue rules concentrate defendants with hostile jurors; peremptory challenges remove jurors from opposing factions; tribal epistemology ensures partisan voting regardless of evidence; jury instructions to 'set aside bias' are theater because bias is now identity, not conscious prejudice. The tangled rope classification captures this hybrid: real coordination overlaid with real extraction. The mandatrophy risk would be misclassifying this as pure Rope (claiming coordination is sufficient) or pure Snare (claiming coordination function is lost entirely). The data supports Tangled Rope: both functions persist, both are measurable, and both require the active enforcement mechanisms (jury selection, instructions, challenges) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    polarization_causation_direction,
    'Does political polarization cause juror bias, or does the jury system''s structural need for local venue and peer composition inevitably channel existing community polarization into verdict disparities?',
    'Comparative analysis of verdict distributions across high-polarization and low-polarization communities for identical case types; isolation of venue effects from juror ideology effects',
    'If polarization causes bias: problem is remediable through jury education or jury composition reforms. If system structure channels polarization: problem is structural and requires venue/composition rules changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polarization_causation_direction, empirical, 'Whether polarization causes bias or system structure channels it').

omega_variable(
    juror_awareness_suppression_gap,
    'Are jurors conscious that they are voting along partisan lines, or has tribal epistemology become sufficiently naturalized that partisan filtering of evidence appears neutral to the juror?',
    'Post-trial juror interviews; comparison of self-reported reasoning vs verdict alignment with juror political affiliation; analysis of juror comments on evidence quality',
    'If conscious: suppression is high but potentially addressable through transparency. If naturalized: suppression is near-total — jurors cannot perceive their own bias, making intervention much harder.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(juror_awareness_suppression_gap, empirical, 'Whether juror bias is conscious or naturalized').

omega_variable(
    venue_reform_feasibility,
    'Can venue changes (federal venue rules, cross-state jury pools, or professional jurors) meaningfully reduce verdict polarization, or does partisan epistemology persist across geographic scales?',
    'Comparative verdict analysis pre/post-venue rule changes; analysis of federal vs state trial outcomes for identical case types; international comparison to professional jury systems',
    'If feasible: scaffold perspective is accurate — structural reforms can build alternative pathways with sunset to polarization dominance. If infeasible: constraint is more fundamental than institutional reform can address.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(venue_reform_feasibility, empirical, 'Whether venue reform can reduce verdict polarization').

omega_variable(
    extraction_mechanism_intentionality,
    'Is the prosecutorial state deliberately gaming jury polarization, or is polarization an unintended side effect of venue rules and jury composition that state prosecutors are exploiting post-facto?',
    'Analysis of prosecution strategy memos, venue selection patterns, juror challenge usage, and correlation with defendant political affiliation; comparison to non-political trials in same venues',
    'If deliberate: extraction is intentional and systemic. If side effect: constraint is Piton-like degradation of system that was never designed for polarized electorate. Mandatrophy implications differ significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_intentionality, conceptual, 'Whether prosecution deliberately exploits polarization or benefits incidentally').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jury_selection_polarization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jury_tr_t0, jury_selection_polarization, theater_ratio, 0, 0.52).
narrative_ontology:measurement(jury_tr_t5, jury_selection_polarization, theater_ratio, 5, 0.62).
narrative_ontology:measurement(jury_tr_t10, jury_selection_polarization, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(jury_be_t0, jury_selection_polarization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jury_be_t5, jury_selection_polarization, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(jury_be_t10, jury_selection_polarization, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jury_selection_polarization, enforcement_mechanism).
narrative_ontology:affects_constraint(jury_selection_polarization, political_trial_venue_selection).
narrative_ontology:affects_constraint(jury_selection_polarization, prosecutorial_discretion_asymmetry).

% DUAL FORMULATION NOTE:
% Jury selection polarization is downstream of systemic political polarization but represents a distinct structural constraint in the judicial system. Upstream constraints include venue rules and peremptory challenge doctrine; downstream constraints include verdict credibility and trial legitimacy. Each has its own extractiveness reflecting its specific structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
