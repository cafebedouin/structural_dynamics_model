% ============================================================================
% CONSTRAINT STORY: abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abolition_reading, []).

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
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abolition_reading
 *   human_readable: State Execution as Categorically Impermissible (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   State execution as a mechanism of criminal justice represents a contested
 *   kernel with fundamentally divergent readings. This constraint story
 *   instantiates the ABOLITION READING: the categorical claim that state
 *   execution is impermissible regardless of crime severity or procedural
 *   safeguards. Under this reading, capital punishment is a snare — a
 *   mechanism of pure extraction (finality imposed on condemned persons with
 *   no legitimate beneficiaries, no coordination function, and no
 *   alternative-suppressing justification). The abolition reading rejects
 *   both the retributive claim (that proportional punishment including death
 *   is a natural law of justice) and the deterrence claim (that execution
 *   deters serious crime beyond alternative severe penalties). From this
 *   perspective, the death penalty persists through institutional inertia,
 *   historical path-dependence, and legitimacy claims that have atrophied
 *   under empirical scrutiny. The constraint exhibits high and rising theater
 *   ratio: procedural safeguards (appeals, clemency boards, DNA exoneration
 *   reviews) perform oversight without preventing executions. The sibling
 *   readings (retributive and deterrence) would classify the same
 *   institutional practice as a rope (coordination) or mountain (natural
 *   law), but this story does not instantiate those readings. The analytical
 *   observer at the abolition reading finds a snare with no defensible
 *   justification; any wrongful execution proves the system's categorical
 *   illegitimacy.
 *
 * KEY AGENTS:
 *   - Condemned Persons: Primary victim (powerless/trapped) — face irreversible extraction; no exit mechanism or correction procedure
 *   - Wrongfully Condemned Class: Statistical victim (moderate/constrained) — estimated 4-5% of death row inmates executed for crimes they did not commit; error is structural, not incidental
 *   - Judicial System: Institutional actor (institutional/constrained) — maintains death penalty through inertia despite documented failures; theater of procedural oversight masks extractive core
 *   - Abolitionist Coalition: Organized agents (organized/mobile) — legislators, legal advocates, civil society actors who have successfully abolished capital punishment in multiple jurisdictions; experience constraint as surmountable
 *   - Retentionist State Authority: Institutional actor (institutional/arbitrage) — maintains and defends capital punishment; benefits from legitimacy claims resting on retributive and deterrence justifications; this is the alternative reading's beneficiary
 *   - Analytical Observer (Abolition): Civilizational perspective (analytical/analytical) — rejects retributive and deterrence framings as empirically falsified or philosophically unjustified; sees state execution as pure extraction with institutional inertia as sole explanation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abolition_reading, 0.88).
domain_priors:suppression_score(abolition_reading, 0.92).
domain_priors:theater_ratio(abolition_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abolition_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(abolition_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(abolition_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abolition_reading, snare).
narrative_ontology:human_readable(abolition_reading, "State Execution as Categorically Impermissible (Abolition Reading)").
narrative_ontology:topic_domain(abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(abolition_reading, formalized).
narrative_ontology:cs_authority_grounding(abolition_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(abolition_reading).
narrative_ontology:cs_kernel_id(abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(abolition_reading, executed_persons).
narrative_ontology:constraint_victim(abolition_reading, wrongfully_condemned).
narrative_ontology:constraint_victim(abolition_reading, judicial_system_integrity).
narrative_ontology:constraint_victim(abolition_reading, democratic_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONDEMNED PERSON (SNARE) — Faces absolute irreversibility; no exit mechanism exists. Death penalty imposes maximum suppression through finality itself. Any error — factual, procedural, or systemic — cannot be corrected. The condemned person is the constraint's primary target. Extraction is maximal and irreversible.
constraint_indexing:constraint_classification(abolition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE WRONGFULLY CONDEMNED CLASS (SNARE) — Statistical inevitability of wrongful executions under this constraint means an entire subpopulation (estimated 4-5% of death row inmates) faces execution for crimes they did not commit. This class has no individual escape mechanism; their error is structural to the system. Generational time horizon reflects that wrongful executions become systemic facts, not isolated incidents.
constraint_indexing:constraint_classification(abolition_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE JUDICIAL SYSTEM (PITON) — The death penalty persists as institutional practice despite documented failures in achieving retributive or deterrent functions. Review procedures (appeals, clemency boards, DNA exonerations) create theater of oversight without preventing executions. The constraint continues through inertia and legitimacy claims that have atrophied. Theater ratio high because procedural safeguards are performative — they cannot prevent the categorical harm.
constraint_indexing:constraint_classification(abolition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ABOLITIONIST COALITION (TANGLED ROPE) — Organized actors (legal advocates, legislatures, civil society) experience the death penalty as a constraint they can challenge and has successfully challenged in multiple jurisdictions. This perspective sees genuine coordination failure (the state's monopoly on justice does require some penalty mechanism) alongside extraction (the use of execution when alternatives exist). Exit is possible through legislative action, and some states have exited. The constraint exhibits hybrid properties from this perspective.
constraint_indexing:constraint_classification(abolition_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RETENTIONIST STATE AUTHORITY (ROPE) — From the institutional perspective that retains capital punishment, the death penalty appears as a coordination mechanism: it allocates the ultimate penalty for ultimate crimes, signals state commitment to justice, and (claimed) deters serious crime. This perspective experiences the constraint as legitimate coordination, not extraction. However, the empirical falsification of deterrence claims and the documented rate of wrongful convictions systematically undermines this reading.
constraint_indexing:constraint_classification(abolition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL LAW VIEW / RETRIBUTIVE READING (MOUNTAIN) — The sibling retributive reading views proportional punishment (including death for murder) as a categorical natural law — a requirement of justice itself, not a contingent policy choice. From this perspective, abolition is a constraint on legitimate state authority, not the death penalty itself. However, this is the alternative reading, not the one this story instantiates. The abolition reading rejects the natural law framing and treats state execution as a constructed institutional practice with no inherent legitimacy.
constraint_indexing:constraint_classification(abolition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ABOLITION FRAME (SNARE) — From the civilizational and global analytical perspective, the abolition reading classifies state execution as a snare with no legitimate beneficiaries. The constraint extracts finality (irreversibility) from those subject to it, suppresses alternatives (life imprisonment, restorative justice), and persists through institutional inertia and legitimacy claims that rest on empirically falsified deterrence theory. This perspective rejects the retributive natural-law frame as a false legitimacy claim layered atop extractive institutional practice.
constraint_indexing:constraint_classification(abolition_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abolition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abolition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abolition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(abolition_reading, TR),
    TR >= 0.70.

:- end_tests(abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): Very high. The abolition reading holds that state execution imposes maximum extraction on condemned persons with no legitimate coordination function or reciprocal benefit. The irreversibility of death penalty execution means any procedural error is catastrophic and uncorrectable. The wrongful execution rate (estimated 4-5%) is not a margin of error to be tolerated within an otherwise justified system; it is proof of systemic illegitimacy under the abolition reading. The measurement trajectory from 0.72 to 0.88 reflects increasing recognition of wrongful conviction rates and empirical falsification of deterrence claims over the 75-year interval, causing the abolition reading to consolidate and the extractiveness value to rise as alternative justifications collapse. Suppression (0.92): Extreme. State monopoly on execution creates absolute suppression for condemned persons — no escape, no appeal, no reversibility. Life imprisonment is substitutable in all functional respects (incapacitation, general deterrence, victim closure) from the abolition perspective, yet execution is maintained. The suppression is both structural (state monopoly, legal barriers to clemency) and legitimacy-based (claims that retribution or deterrence justify execution despite evidence to the contrary). Theater ratio (0.65): Moderate-high and rising. Appeals, DNA review, clemency boards, and procedural safeguards create performative oversight that conveys process legitimacy while failing to prevent executions. The theater ratio rises over the interval because procedural complexity increases while actual reversal or commutation rates remain low, indicating that procedure has become decoupled from outcome. The abolition reading interprets theater increase as indication that procedural theater substitutes for genuine limitation of execution authority.
 *
 * PERSPECTIVAL GAP:
 *   The abolition reading produces maximal perspectival gap between the retentionist institutional perspective and all other perspectives. The retentionist state authority (institutional/immediate) sees rope or mountain — legitimate coordination or natural law. The condemned person sees pure snare — irreversible extraction with no justification. The abolitionist coalition sees surmountable constraint (scaffold or tangled rope) — a temporary institutional arrangement that can and has been abolished through democratic action. The judicial system sees piton — a degraded ritual persisting through inertia. The wrongfully condemned class sees snare with statistical inevitability — the system extracts finality from innocent persons at predictable rates. The analytical observer (abolition reading) sees snare with no defensible legitimacy claim — the retributive and deterrence framings are empirically falsified or philosophically contingent. This perspectival geography is specific to the abolition reading; the retributive reading would invert it (retentionist state sees rope/mountain; abolitionists see constraint on legitimate authority).
 *
 * DIRECTIONALITY LOGIC:
 *   The abolition reading locates no beneficiaries with extraction flowing toward them. The retributive reading (sibling, not instantiated here) would identify state authority and law-abiding citizens as beneficiaries of proportional punishment and general deterrence. The abolition reading rejects both claimed benefits: (1) retributive justice is treated as a contingent moral framing, not a natural law or institutional necessity; (2) deterrence is empirically falsified — capital punishment does not deter murder at rates superior to life imprisonment. Therefore, d approaches 1.0 for the condemned person (full target, no reciprocal benefit). From the analytical perspective, even the retentionist state authority experiences d = 0.95 (primarily target of the abolition claim) because the abolition reading treats the state's claimed benefits as false legitimacy claims. The abolitionist coalition experiences d = 0.55 (mobile organized actors, mixed costs and benefits) — they face resource barriers to legislative change but have successfully exited in multiple jurisdictions, so exit is not impossible.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolition reading resolves mandatrophy by rejecting the legitimacy of the claimed coordination functions (retribution, deterrence). Under the retributive reading (sibling), capital punishment is a rope (pure coordination without significant extraction) or a mountain (natural law of proportional justice). Under the abolition reading, the same institutional practice is a snare (extraction without legitimate coordination). The classification difference is not an empirical gap but a fundamental disagreement about whether retributive justice is a categorical moral principle. The mandatrophy resolved value (true) indicates that wrongful execution data, empirically falsified deterrence claims, and the existence of adequate alternative penalties (life imprisonment) have made the mandatrophy question acute: if wrongful executions occur at predictable rates (4-5%), the system cannot be redeemed by procedural improvements or accuracy claims. The abolition reading treats wrongful execution as proof of systemic illegitimacy, not as a failure to be engineered away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is state execution a natural law of proportional justice (retributive reading) or a constructed institutional constraint with no legitimate justification (abolition reading)?',
    'The contest is not empirically resolvable; it is a foundational disagreement about whether retributive justice is a categorical moral principle or a contingent moral framing. Resolution depends on philosophical frameworks (deontological vs consequentialist), not data.',
    'If retributive reading holds: state execution is a rope or mountain (legitimate coordination/law), and abolition is the constraint. If abolition reading holds: state execution is a snare (pure extraction), and retributivism is a false legitimacy claim. This constraint story instantiates the abolition reading only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Fundamental disagreement about whether retributive justice is natural law or constructed framing').

omega_variable(
    wrongful_execution_rate_threshold,
    'At what wrongful execution rate does the retributive reading''s empirical foundation collapse?',
    'Systematic review of death penalty cases; DNA exoneration data; statistical modeling of systemic error rates in capital cases; comparison to non-capital felony conviction rates.',
    'Current estimates: 4-5% wrongful execution rate. If true rate > 10%: retributive framing becomes indefensible on its own terms (proportional punishment of the innocent is not proportional). If rate < 1%: retributive reading has stronger empirical ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_rate_threshold, empirical, 'What proportion of executions are of factually innocent persons').

omega_variable(
    deterrence_claim_empirical_status,
    'Does capital punishment deter serious crime at rates superior to alternative severe punishments (life without parole)?',
    'Meta-analysis of deterrence studies; comparison of murder rates in abolitionist vs retentionist jurisdictions controlling for socioeconomic factors; temporal analysis of deterrence claims before and after abolition in individual states.',
    'If deterrence claim is empirically false (consensus of criminological research): a primary justification for state execution collapses, leaving only retributive framing. The abolition reading''s snare classification depends on rejecting deterrence as a legitimate justification; if deterrence were real, the constraint would shift toward tangled_rope (coordination mixed with extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_claim_empirical_status, empirical, 'Whether capital punishment deters murder beyond the effect of life imprisonment').

omega_variable(
    alternative_penalty_sufficiency,
    'Does life imprisonment without parole adequately serve the legitimate state interests that retributionists cite (incapacitation, general deterrence, victim closure)?',
    'Comparative criminology; victim impact data; jurisdictional studies comparing states that have abolished capital punishment but retained LWOP vs states that retained capital punishment; long-term recidivism rates for LWOP inmates.',
    'If LWOP is structurally sufficient: abolition reading''s argument is strengthened (death penalty is extraction surplus to legitimate penological needs). If LWOP is inadequate for specific crimes: retributive reading gains ground (some crimes may require execution to satisfy justice requirements).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_penalty_sufficiency, empirical, 'Whether life imprisonment without parole adequately serves penological and restorative functions').

omega_variable(
    sibling_reading_instantiation,
    'What empirical or philosophical findings would shift this constraint story to instantiate the retributive reading instead of the abolition reading?',
    'This is a meta-omega documenting the reading contest itself. Empirical: reversal of wrongful execution rate estimates, confirmation of deterrence effect, or systemic failure of LWOP as alternative. Philosophical: widespread acceptance of natural-law retributivism as foundational to criminal justice theory.',
    'If shifted: same structural constraint would be reclassified from snare to rope or mountain depending on whether retributive justice is treated as natural law or institutional coordination. The constraint_id and narrative would remain; the reading_id would change to retributive_reading; claimed_type would change; perspectives would invert.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_instantiation, conceptual, 'Conditions under which the retributive reading becomes the operant instantiation of the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abolition_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abol_tr_t0, abolition_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(abol_tr_t25, abolition_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(abol_tr_t50, abolition_reading, theater_ratio, 50, 0.65).
narrative_ontology:measurement(abol_tr_t75, abolition_reading, theater_ratio, 75, 0.65).

% Extraction over time
narrative_ontology:measurement(abol_be_t0, abolition_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(abol_be_t25, abolition_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(abol_be_t50, abolition_reading, base_extractiveness, 50, 0.88).
narrative_ontology:measurement(abol_be_t75, abolition_reading, base_extractiveness, 75, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(abolition_reading, retributive_reading).
narrative_ontology:affects_constraint(abolition_reading, deterrence_reading).
narrative_ontology:cs_reading_relation(abolition_reading, retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation(abolition_reading, deterrence_reading, coexists_with).
% Temporal layer: t0 reference frame and t1 drift state. t2 (trajectory) is computed by cs_drift_engine.pl.
% Reference frame: the classical punitive authority that administered CP as ordinary punishment.
% Drift: its normative legitimacy has substantially eroded in international HR law; authority structure
% has not acknowledged this erosion as dispositive → unacknowledged substantial authority erosion.
% Engine computes: authority_erosion + substantial + false → husk.
narrative_ontology:cs_reference_frame(abolition_reading, classical_punitive_authority).
narrative_ontology:cs_drift_state(abolition_reading, contemporary_human_rights_era,
    gap(authority_erosion, substantial, false)).

% DUAL FORMULATION NOTE:
% The constraint 'state_execution_authority' is a contested kernel with three structurally distinct readings: abolition_reading (this file), retributive_reading (sibling), and deterrence_reading (sibling). Each reading instantiates a different constraint with different ε values, beneficiary/victim declarations, and classifications. The readings are not observable-dependent variants of one constraint; they are fundamentally different claims about what state execution is and whether it is justified. Each story has its own constraint_id and is compiled separately. Network links indicate conceptual kinship, not shared metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
