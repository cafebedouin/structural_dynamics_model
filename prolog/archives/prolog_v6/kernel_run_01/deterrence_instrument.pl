% ============================================================================
% CONSTRAINT STORY: deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deterrence_instrument, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deterrence_instrument
 *   human_readable: Execution as Deterrence Instrument: Legitimacy Contingent on Empirical Deterrence
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: the
 *   state_killing_authority. The deterrence_instrument reading legitimizes
 *   execution through the empirical claim that it prevents future murders
 *   through rational deterrence. This reading differs structurally from its
 *   siblings (retributive_desert, which legitimizes execution through
 *   proportional desert independent of effects; categorical_impermissibility,
 *   which rejects state killing as categorically illegitimate). The
 *   deterrence reading is contingent: execution is legitimate IF AND ONLY IF
 *   deterrence evidence exists. The current empirical status: the National
 *   Research Council (2012) concluded that deterrence claims lack credible
 *   support. Yet executions continue in US jurisdictions, revealing a
 *   structural gap between the reading's legitimacy claim and the empirical
 *   grounds for that claim. The condemned person bears the irreversible cost
 *   of execution; future potential murder victims are the supposed
 *   beneficiaries; the state's legitimacy apparatus benefits from the
 *   credible appearance of protective authority. The constraint exhibits high
 *   extractiveness (0.68) and rising theater ratio (0.35 → 0.58 over 40
 *   years), indicating that the infrastructure supporting execution persists
 *   despite empirical refutation of its stated rationale.
 *
 * KEY AGENTS:
 *   - Current Condemned: Primary victim (powerless/trapped) — bears irreversible cost of execution; receives zero deterrence benefit (dead persons do not benefit from future crime prevention)
 *   - Future Potential Murder Victims: Supposed primary beneficiary (powerless, identity unknown, unorganizable) — exist only as abstract category in deterrence hypothesis; cannot voice preferences or consent to use of condemned as deterrent
 *   - Crime Victims and Their Families: Secondary beneficiary/victim (moderate/constrained) — benefit from system claiming to prevent future victimization; harmed by system's demand for emotional participation and potential retriggering
 *   - State Legitimacy Apparatus: Primary beneficiary (institutional/arbitrage) — derives protective-authority credibility from execution; can adjust policy without fundamental delegitimation
 *   - Wrongfully Convicted Persons: Structural victim (powerless/trapped) — empirically innocent bearers of the constraint's cost; receive zero deterrence benefit and no legitimate justification
 *   - Criminological Research Community: Secondary observer (analytical/analytical) — produces evidence that deterrence effect is absent; evidence is institutionally ignored
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deterrence_instrument, 0.68).
domain_priors:suppression_score(deterrence_instrument, 0.72).
domain_priors:theater_ratio(deterrence_instrument, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deterrence_instrument, extractiveness, 0.68).
narrative_ontology:constraint_metric(deterrence_instrument, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(deterrence_instrument, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deterrence_instrument, snare).
narrative_ontology:human_readable(deterrence_instrument, "Execution as Deterrence Instrument: Legitimacy Contingent on Empirical Deterrence").
narrative_ontology:topic_domain(deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deterrence_instrument, '756a8840-45f7-4220-b7bc-9d057d2db359').
narrative_ontology:cs_created_at('756a8840-45f7-4220-b7bc-9d057d2db359', '').
narrative_ontology:cs_kernel_codification('756a8840-45f7-4220-b7bc-9d057d2db359', formalized).
narrative_ontology:cs_authority_grounding('756a8840-45f7-4220-b7bc-9d057d2db359', extraction).
narrative_ontology:cs_interpretation_layer_present('756a8840-45f7-4220-b7bc-9d057d2db359').
narrative_ontology:cs_kernel_id(deterrence_instrument, state_killing_authority).
narrative_ontology:cs_reading_relation('756a8840-45f7-4220-b7bc-9d057d2db359', retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('756a8840-45f7-4220-b7bc-9d057d2db359', categorical_impermissibility, forecloses).
narrative_ontology:cs_axiom('756a8840-45f7-4220-b7bc-9d057d2db359', foundational, deterrence_empirically_contingent_legitimacy).
narrative_ontology:cs_axiom_status(deterrence_empirically_contingent_legitimacy, holdable).
narrative_ontology:cs_axiom('756a8840-45f7-4220-b7bc-9d057d2db359', foundational, instrumental_rationality_justifies_death).
narrative_ontology:cs_axiom_status(instrumental_rationality_justifies_death, holdable).
narrative_ontology:cs_reference_frame('756a8840-45f7-4220-b7bc-9d057d2db359', empirical_deterrence_justified_execution).
narrative_ontology:cs_drift_state('756a8840-45f7-4220-b7bc-9d057d2db359', post_nrc_meta_analysis_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deterrence_instrument, future_potential_murder_victims).
narrative_ontology:constraint_beneficiary(deterrence_instrument, state_legitimacy_claim).
narrative_ontology:constraint_victim(deterrence_instrument, current_condemned).
narrative_ontology:constraint_victim(deterrence_instrument, due_process_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED PERSON (SNARE) — Completely trapped; execution is irreversible. Bears the entire physical and existential cost of the deterrence hypothesis. No alternatives, no agency, no escape. Maximum extraction without coordination benefit to the condemned. The deterrence logic instrumentalizes the condemned as a means to protect others, with zero reciprocal benefit.
constraint_indexing:constraint_classification(deterrence_instrument, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CRIME VICTIMS AND FAMILIES (TANGLED ROPE) — Constrained by trauma, ongoing legal proceedings, limited exit from the justice system's emotional demands. Experience both extraction (forced participation in trials, delayed closure, potential retriggering) and coordination benefit (the constraint attempts to prevent future victimization through deterrence). Their structural position is genuinely mixed: they benefit from a system claiming to protect them, but the system also demands emotional labor and subjects them to secondary victimization.
constraint_indexing:constraint_classification(deterrence_instrument, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE LEGITIMACY APPARATUS (ROPE) — The state benefits from the deterrence claim as a legitimacy mechanism. Execution provides tangible evidence of state capacity to protect citizens and enforce law. The state has arbitrage options: it can adjust penalty severity, adopt alternative deterrents, or modify the capital punishment system. From the state's perspective, the constraint solves a coordination problem (demonstrating protective authority) with tolerable coercive overhead. The state experiences this as coordination, not extraction.
constraint_indexing:constraint_classification(deterrence_instrument, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WRONGFULLY CONVICTED PERSONS (SNARE) — Structurally similar to the justly condemned but with zero legitimate extraction justification. No deterrence benefit accrues to an executed innocent person. This perspective reveals the constraint's brittleness: its legitimacy depends entirely on accurate conviction. Error rates (DNA exonerations, prosecutorial misconduct, inadequate defense) show that the trap closes on the innocent as well as the guilty. For wrongfully convicted persons, the constraint is pure extraction with no coordination function.
constraint_indexing:constraint_classification(deterrence_instrument, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: DETERRENCE EMPIRICAL APPARATUS (PITON) — The constraint's legitimacy claim rests on the empirical proposition that executions deter future murders. Decades of criminological research (Ehrlich, Bowers, Hashem, NRC meta-analyses) show zero statistically credible deterrence effect. The constraint persists despite this empirical refutation — executions continue in the US despite the National Research Council's 2012 conclusion that deterrence claims lack credible support. Theater ratio (0.58) reflects this: significant institutional infrastructure (trials, appeals, execution chambers) performs a legitimacy function (demonstrating state control) that has decoupled from the deterrence rationale. The piton classification captures that the constraint's primary function has atrophied while the machinery persists.
constraint_indexing:constraint_classification(deterrence_instrument, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational timescale, this perspective naturalizes state execution as an immutable response to murder: 'The taking of life for the taking of life is the natural law of retaliation.' This framing treats the deterrence instrumental claim as derivative from a deeper natural law of proportional justice. However, the structural data reveals this as a false summit: the deterrence claim is not a law of nature but a specific institutional justification grafted onto a retaliatory tradition. The mountain classification will trigger the false-summit detector — the presence of beneficiaries (future victims, state legitimacy) and the empirical refutation of the deterrence hypothesis expose the natural-law framing as contingent institutionalization rather than immutable principle.
constraint_indexing:constraint_classification(deterrence_instrument, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INSTITUTIONAL READING (TANGLED ROPE) — The deterrence instrument reading as instantiated by criminal justice institutions and defended by legitimacy theorists. From this perspective, the constraint coordinates two functions: (1) it provides a rationale for state capital punishment that citizens might accept (instrumental rather than merely retaliatory), and (2) it attempts to prevent future murders. However, the reading's legitimacy is entirely contingent on empirical deterrence evidence, which is currently absent. The constraint exhibits the tangled-rope signature: genuine coordination function (providing a legitimacy framework the state can defend) combined with asymmetric extraction (the condemned bears the cost regardless of whether deterrence occurs). The reading's core claim is that execution is legitimate IF AND ONLY IF deterrence evidence exists — this is what makes the constraint analyzable as a contingent rather than categorical judgment.
constraint_indexing:constraint_classification(deterrence_instrument, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deterrence_instrument_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deterrence_instrument, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deterrence_instrument, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deterrence_instrument, TR),
    TR >= 0.70.

:- end_tests(deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The condemned person bears an irreversible cost (death) under a hypothesis that has no empirical support. The extraction increases over the 40-year interval (0.52 → 0.75) because: (1) empirical evidence against deterrence accumulated (Ehrlich refutation in 1980s, NRC meta-analysis in 2012) without changing practice, revealing that the stated rationale no longer credibly grounds the policy; (2) wrongful conviction evidence accumulated (DNA exonerations), showing that the constraint's cost is borne by the innocent; (3) the state's interest in execution persists despite evidence, indicating the real function is not deterrence but legitimacy theater and retaliatory satisfaction. Suppression (0.72): High. The condemned person has no exit option (execution is final), minimal appeal capacity (clemency is discretionary), and faces institutional pressure to accept guilt (plea deals). Victims and potential victims are structurally unable to organize or withdraw consent. Theater ratio (0.58): Moderate-high and rising. The constraint's infrastructure (trials, appeals, execution chambers, victim impact statements) performs an institutional legitimacy function that has decoupled from deterrence. The rise in theater over time reflects the growing gap between the stated rationale (deterrence, now empirically refuted) and the continuing practice (serving retaliatory and legitimacy functions). Claimed type (Snare): Justified by high extraction + high suppression + absence of coordination benefit to the condemned. The constraint lacks a genuine coordination function from the condemned's perspective; any coordination benefit accrues to future victims (an abstract category) and the state (which benefits from legitimacy appearance).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is profound and reveals the reading's structural tension. The condemned sees pure extraction (Snare): death without reciprocal benefit. The state sees coordination (Rope): a mechanism to credibly demonstrate protective authority. Crime victims see mixed coordination-extraction (Tangled Rope): the system claims to protect them but also demands emotional labor and potential retriggering. Wrongfully convicted persons see pure extraction without even the stated deterrence benefit (Snare with zero rationalization). The empirical apparatus sees institutional theater decoupled from stated function (Piton). The natural-law observer risks naturalizing a contingent institutional arrangement (false summit Mountain). The analytical observer sees a reading whose core legitimacy claim (empirical deterrence) has been refuted, yet the constraint persists — this reveals that deterrence was never the primary legitimacy ground but a rationalization layered over deeper retaliatory and institutional interests. The perspectival gap between the natural-law view and the analytical view is the engine's false-summit detector: if the constraint truly were a natural law of retaliation, empirical refutation would not matter; the fact that deterrence evidence was demanded and sought reveals the reading is institutionally contingent, not naturally necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position. For the condemned: beneficiary status = false (no deterrence benefit, no legitimacy benefit), victim status = true, exit options = trapped → d ≈ 0.98 (maximum target). For the state: beneficiary status = true (legitimacy, retaliatory satisfaction), victim status = false, exit options = arbitrage → d ≈ 0.10 (near-full beneficiary). For future victims: beneficiary status = true (abstract, contingent on deterrence evidence), victim status = false, exit options = analytical (cannot act) → d ≈ 0.35 (mixed, weighted toward benefit). For wrongfully convicted persons: beneficiary status = false, victim status = true, exit options = trapped → d ≈ 0.98 (maximum target, same as justly condemned). The engine's directionality derivation captures why the condemned and the state experience fundamentally incompatible classifications: the condemned experiences maximum extraction (d → 1.0 → high χ), the state experiences coordination (d → 0.0 → negative χ). This perspectival gap is structural, not observational.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy at extractiveness (0.68 > 0.70 threshold requires mandatrophy_resolved = true) by explicitly grounding the deterrence_instrument reading's legitimacy in an empirical claim (deterrence effect) that can be true or false. The reading is not universally applicable — it depends on whether the empirical hypothesis holds. The mandatrophy is resolved by identifying the contingency: execution is legitimate IF deterrence evidence exists. Current empirical status: deterrence evidence does not exist (NRC finding). Therefore, under this reading's own logic, execution is not currently legitimate as a deterrence instrument. The constraint persists anyway, revealing that: (1) the deterrence reading is not actually the primary legitimacy ground (retribution or institutional interest persists underneath), or (2) the state has adopted the categorical_impermissibility reading's opposite (categorically affirms the right to execute regardless of effects), or (3) the reading has been superseded by practice, becoming a rationalization rather than a principle. The resolution of the mandatrophy does not eliminate the constraint — it clarifies the constraint's actual structure by exposing the gap between the stated legitimacy claim and the empirical grounds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_effect_empirical_gap,
    'Does execution deter future murders at rates statistically distinguishable from zero, controlling for concurrent policies and demographic factors?',
    'Meta-analysis of deterrence studies using consistent methodology (NRC standard: fixed-effects models, publication bias correction, specification robustness). Prospective studies comparing murder rates pre/post moratorium with instrumental variable design.',
    'If deterrence effect > 0.05 (5% reduction in murder rate per execution): deterrence_instrument reading preserves legitimacy; constraint classification remains Snare/Tangled Rope. If deterrence effect ≤ 0.05 or indistinguishable from zero: reading''s core premise fails; constraint''s legitimacy claim becomes purely retaliatory (shifts to categorical_impermissibility or retributive_desert readings); constraint reclassifies to pure extraction (Snare for all perspectives). This is the axiomatic resolution point.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_effect_empirical_gap, empirical, 'Empirical deterrence effect of capital punishment on murder rates').

omega_variable(
    false_summit_natural_law_retaliation,
    'Is the deterrence claim a derivative justification grafted onto a deeper retaliatory natural law, or is it the primary legitimacy grounding?',
    'Historical analysis: examine how state legitimacy for execution was justified pre-deterrence (classical retributive tradition) vs. post-deterrence (modern instrumental justifications). Identify whether deterrence was added to shore up retaliatory authority or whether it genuinely replaced retaliatory reasoning as the primary ground.',
    'If deterrence is derivative: the mountain natural-law framing masks a tangled rope or snare (false summit). The retaliatory tradition persists regardless of deterrence evidence, and removing deterrence evidence doesn''t resolve the constraint''s legitimacy — it just unmasks the retaliatory ground. If deterrence is primary: removing the deterrence evidence fundamentally undermines the legitimacy claim, shifting to categorical impermissibility. This determines whether the constraint has a hidden retaliatory kernel immune to empirical refutation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_retaliation, conceptual, 'Whether deterrence claim is primary or derivative legitimacy grounding').

omega_variable(
    death_penalty_innocence_rate,
    'What proportion of death-sentenced inmates in a given jurisdiction are actually innocent (exonerations, reversals, or evidence of actual innocence post-conviction)?',
    'Empirical: DNA exonerations, post-conviction DNA testing, reversals on grounds of prosecutorial misconduct or inadequate counsel. Probabilistic: estimated innocence rate (Gross et al. 4% estimate for 1973-2004 US cohort) applied to current death row.',
    'If innocence rate ≥ 2%: the constraint''s error catastrophe becomes quantifiable. For every 50 executions, ~1 is empirically innocent. The wrongfully_convicted perspective (Snare with zero deterrence benefit) becomes a systematic feature, not an exception. This converts the constraint from contingent-on-deterrence to structurally incoherent: even deterrence evidence would not legitimate execution of the innocent. If innocence rate < 2%: error catastrophe is manageable, though still nonzero. Wrongly convicted perspective remains theoretically possible but statistically marginal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(death_penalty_innocence_rate, empirical, 'Proportion of death-sentenced inmates later exonerated or found innocent').

omega_variable(
    comparative_institutional_deterrence_paths,
    'Do death-penalty abolition jurisdictions (or those with long moratoria) show different murder rate trajectories than high-execution jurisdictions?',
    'Comparative analysis: murder rates in death-penalty states vs. abolition states (EU, Canada, most of US by 2024) over identical time periods, controlling for socioeconomic factors. Natural experiment: US states that abolished or resumed capital punishment.',
    'If abolition jurisdictions show equivalent or declining murder rates: no deterrence effect is evident, and the constraint''s legitimacy collapses. If death-penalty states show lower rates: potential deterrence effect exists (though causation is difficult to isolate). This evidence directly feeds the primary omega (deterrence_effect_empirical_gap).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_institutional_deterrence_paths, empirical, 'Murder rate trajectories in death-penalty vs. abolition jurisdictions').

omega_variable(
    reading_kernel_contest,
    'Which of the three readings of the state_killing_authority kernel dominates legitimacy discourse in a given jurisdiction at a given time?',
    'Textual analysis of appellate decisions, legislative justifications, and public discourse. Examine which legitimacy argument (deterrence, retribution, categorical impermissibility) appears in death sentencing opinions and clemency petitions.',
    'If deterrence_instrument reading dominates: the constraint''s classification depends on deterrence evidence (high sensitivity to empirical omega). If retributive_desert reading dominates: classification is less sensitive to empirical evidence (retribution requires no effect proof). If categorical_impermissibility reading dominates: constraint shifts to pure cultural/legal rejection regardless of empirical data. Identifies which reading''s axioms currently ground institutional practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, empirical, 'Which legitimacy reading dominates current institutional practice').

omega_variable(
    wrongful_conviction_cascade_mechanism,
    'Does the deterrence instrumentalization create incentive structures that increase wrongful conviction risk (pressure on prosecutors to secure convictions, underfunding of defense, acceptance of unreliable evidence)?',
    'Structural analysis: compare exoneration rates pre/post capital punishment adoption in same jurisdiction. Examine prosecutorial incentives and defense funding in capital vs. non-capital jurisdictions. Identify whether the state''s interest in demonstrating execution capacity creates systemic bias toward conviction.',
    'If cascade exists and is substantial: the constraint creates a feedback loop where its legitimacy claim (deterrence) induces institutional pressures that increase the very harm (murder) it claims to deter (through wrongful convictions of innocents, who subsequently commit murders to escape wrongful sentences). This converts the constraint from a coordination mechanism to a self-defeating extraction system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_cascade_mechanism, empirical, 'Whether deterrence instrumentalization increases wrongful conviction risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deterrence_instrument, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deterrence_theater_t0_1970s, deterrence_instrument, theater_ratio, 0, 0.35).
narrative_ontology:measurement(deterrence_theater_t20_1990s, deterrence_instrument, theater_ratio, 20, 0.48).
narrative_ontology:measurement(deterrence_theater_t40_2010s, deterrence_instrument, theater_ratio, 40, 0.58).
narrative_ontology:measurement(deterrence_theater_t10_1980s, deterrence_instrument, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(deterrence_extract_t0_1970s, deterrence_instrument, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(deterrence_extract_t20_1990s, deterrence_instrument, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(deterrence_extract_t40_2010s, deterrence_instrument, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(deterrence_extract_t10_1980s, deterrence_instrument, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(deterrence_instrument, retributive_desert).
narrative_ontology:affects_constraint(deterrence_instrument, categorical_impermissibility).
narrative_ontology:affects_constraint(deterrence_instrument, wrongful_conviction_cascade).

% DUAL FORMULATION NOTE:
% The state_killing_authority kernel decomposes into three constraint stories, each with different ε, different beneficiary/victim structures, and different classifications. The deterrence_instrument reading (this constraint) has ε=0.68, Snare from condemned perspective, Rope from state perspective. The retributive_desert reading (sibling) has different ε (conditional on desert philosophy, not empirical deterrence) and different classification (potentially Mountain if retribution is natural law, or Rope if treated as coordination). The categorical_impermissibility reading (sibling) rejects all three readings' premises and classifies execution differently (as extraction for state legitimacy, regardless of stated rationale). These are not the same constraint viewed differently — their ε values differ, their beneficiary/victim structures differ, and their classification ranges differ. They are three interpretive readings of the same kernel with structurally distinct constraint profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deterrence_instrument, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
