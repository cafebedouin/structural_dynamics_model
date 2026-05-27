% ============================================================================
% CONSTRAINT STORY: originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_originalist_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: originalist_reading
 *   human_readable: Originalist Constitutional Constraint: Historical Public Meaning Binding
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   Originalism is ONE READING of the contested kernel: how constitutional
 *   meaning is fixed and transmitted across time. This reading asserts that
 *   the Constitution's meaning is fixed at the moment of ratification (1788
 *   for the original Constitution; amendment dates for subsequent
 *   amendments), and that judges are bound by the historical public meaning
 *   at those moments. Contemporary circumstances, moral progress, or evolving
 *   social understanding are irrelevant to meaning (though relevant to
 *   application). This is a structural constraint because it binds judges to
 *   a specific interpretive methodology and suppresses alternative
 *   interpretive approaches (living constitutionalism, purposivism,
 *   positivism). The constraint exhibits snare characteristics: it forecloses
 *   rights claims that lack historical support, suppresses alternative
 *   interpretation methods, and extracts institutional power from progressive
 *   legal movements. The beneficiary is the originalist judiciary and
 *   counter-majoritarian constraint advocates who prefer judges to have
 *   narrow, text-bound authority. The victims are rights claimants whose
 *   claims cannot be grounded in 18th-century evidence and the broader
 *   institutional capacity for constitutional meaning to evolve with moral
 *   understanding. The constraint's extractiveness has risen over 40 years
 *   (from 0.35 to 0.58) as originalism has moved from a fringe methodological
 *   position to institutional dominance; the theater ratio has also risen
 *   (from 0.22 to 0.38) as originalist opinions increasingly feature
 *   performative displays of historical scholarship disconnected from the
 *   actual constraint-binding mechanism.
 *
 * KEY AGENTS:
 *   - Rights Claimant Without Historical Pedigree: Primary victim (powerless/trapped) — person or group whose claimed right lacks explicit 18th-century support and is foreclosed by originalist methodology
 *   - Progressive Legal Movement: Secondary victim (moderate/constrained) — coalition seeking to expand constitutional protections; faces high barriers to exit through litigation or amendment
 *   - Originalist Judiciary: Primary beneficiary (institutional/arbitrage) — judges and court system that use originalism as a coordination mechanism and legitimacy claim
 *   - Counter-Majoritarian Constraint Advocates: Primary beneficiary (institutional/arbitrage) — scholars, think tanks, political movements that benefit from narrow judicial power and prefer originalism
 *   - Constitutional Amendment Coalition: Organized agents (organized/constrained) — state legislatures, social movements pursuing formal amendment as exit pathway
 *   - Historical-Evidence Interpretive Community: Institutional actor (institutional/arbitrage) — scholars and archivists maintaining the performative infrastructure of originalist interpretation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent methodological choice as immutable constitutional logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(originalist_reading, 0.58).
domain_priors:suppression_score(originalist_reading, 0.72).
domain_priors:theater_ratio(originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(originalist_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(originalist_reading, snare).
narrative_ontology:human_readable(originalist_reading, "Originalist Constitutional Constraint: Historical Public Meaning Binding").
narrative_ontology:topic_domain(originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(originalist_reading, '21325e7e-6c37-4288-8d2e-376b81da39c9').
narrative_ontology:cs_created_at('21325e7e-6c37-4288-8d2e-376b81da39c9', '').
narrative_ontology:cs_kernel_codification('21325e7e-6c37-4288-8d2e-376b81da39c9', formalized).
narrative_ontology:cs_authority_grounding('21325e7e-6c37-4288-8d2e-376b81da39c9', lineage).
narrative_ontology:cs_interpretation_layer_present('21325e7e-6c37-4288-8d2e-376b81da39c9').
narrative_ontology:cs_kernel_id(originalist_reading, us_constitution_meaning).
narrative_ontology:cs_reading_relation('21325e7e-6c37-4288-8d2e-376b81da39c9', living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('21325e7e-6c37-4288-8d2e-376b81da39c9', positivist_reading, coexists_with).
narrative_ontology:cs_axiom('21325e7e-6c37-4288-8d2e-376b81da39c9', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('21325e7e-6c37-4288-8d2e-376b81da39c9', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('21325e7e-6c37-4288-8d2e-376b81da39c9', foundational, historical_public_meaning_is_discoverable).
narrative_ontology:cs_axiom_status(historical_public_meaning_is_discoverable, holdable).
narrative_ontology:cs_axiom_grounding('21325e7e-6c37-4288-8d2e-376b81da39c9', historical_public_meaning_is_discoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('21325e7e-6c37-4288-8d2e-376b81da39c9', framers_intent_binding_authority).
narrative_ontology:cs_drift_state('21325e7e-6c37-4288-8d2e-376b81da39c9', contemporary_judicial_practice, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(originalist_reading, originalist_judiciary).
narrative_ontology:constraint_victim(originalist_reading, rights_claimants_lacking_historical_support).
narrative_ontology:constraint_victim(originalist_reading, evolving_moral_understanding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIGHTS CLAIMANT (SNARE) — An individual or group whose claimed right (e.g., privacy protection, equal protection for historically excluded group) lacks explicit 18th-century textual or evidentiary support. Trapped by the constraint: cannot exit the originalist framework through litigation; the binding mechanism is institutional (courts apply originalist method). Experiences maximum extraction: claim is foreclosed not by contemporary counter-evidence but by historical silence. No suppression-avoidance option; the constraint is enforced by the judiciary.
constraint_indexing:constraint_classification(originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRESSIVE LEGAL MOVEMENT (SNARE) — Constrained by high litigation barriers: must either (a) construct post-hoc historical evidence reinterpreting 18th-century intent to support contemporary values, (b) pursue constitutional amendment (massive coordination cost), or (c) lobby for judicial adoption of non-originalist methodology. Each exit path is costly. The constraint extracts institutional power from this coalition: their preferred interpretation method is suppressed in originalist-dominated courts.
constraint_indexing:constraint_classification(originalist_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINALIST JUDICIARY (ROPE) — The institutional beneficiary. Originalism functions as a coordination mechanism for judges: it provides a clear interpretive method (historical public meaning at ratification) that constrains discretion and reduces jurisdictional variation. The originalist judge experiences the constraint as coordination: 'We have a neutral method; we follow the historical record.' Arbitrage position: can justify departures through originalist reinterpretation of history; can exit to non-originalism at low cost (switch methodology). Net beneficiary through institutional legitimacy and doctrinal clarity.
constraint_indexing:constraint_classification(originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL AMENDMENT PATHWAY (SCAFFOLD) — Organized agents (state legislatures, social movements) see the originalist constraint as temporary: constitutional amendment is a sunset mechanism. When a right lacks 18th-century support (e.g., marriage equality, privacy), amendment codifies it in the text. This transforms the constraint from snare to rope (if the amendment is ratified). Amendment is hard (high suppression for reformers), but it is a structured exit. Theater is moderate: amendment procedure is genuinely functional, though culturally theatrical. Extraction declining over time as more amendments accumulate.
constraint_indexing:constraint_classification(originalist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL-EVIDENCE INTERPRETIVE COMMUNITY (PITON) — Scholars and archivists who claim originalism requires mastering primary sources. The community maintains the constraint through institutional inertia: the legitimacy of originalism increasingly rests on the performance of historical scholarship rather than on the actual binding force of the method. Theater ratio is rising as originalist opinions contain performative displays of historical erudition (e.g., extensive footnotes, invented historical narratives) disconnected from the actual decision. The functional constraint — judges bound by historical meaning — has atrophied; it is maintained by theatrical citation to 'the founding record.'
constraint_indexing:constraint_classification(originalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the constraint appears as an immutable feature of written constitutions: any text has a fixed meaning at the moment of ratification, and that meaning constrains later interpreters. This is presented as a logical/epistemic truth: you cannot ground legitimacy in a living document's contemporary sense because 'living' meaning is indeterminate; you must anchor to something fixed (the ratification moment). However, the structural data contradicts this classification — the engine will identify this as a false summit: the 'immutable logic' of textual interpretation naturalizes a contingent methodological choice (originalism) rather than an inherent property of constitutional texts.
constraint_indexing:constraint_classification(originalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(originalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(originalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(originalist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(originalist_reading, TR),
    TR >= 0.70.

:- end_tests(originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, rising over time. The originalist constraint extracts institutional power from alternative interpretive methods and forecloses rights claims lacking historical support. But it is not a pure snare of maximum extraction (ε ≥ 0.70) because (a) the constraint is theoretically justified (not purely coercive), (b) some flexibility exists through reinterpretation of historical meaning, and (c) amendment provides a structured (though difficult) exit pathway. The rise from 0.35 to 0.58 reflects originalism's institutional consolidation: as the methodology has moved from fringe to mainstream in federal courts, its extractive force has increased — it now forecloses more rights claims and suppresses more alternative methodologies. Suppression (0.72): High. The constraint is enforced by federal courts with clear institutional mechanisms (precedent, standing doctrine, jurisdictional rules). The suppression of alternative interpretive methods is substantial: living constitutionalism is marginalized in appellate practice; purposivism is confined to statutory interpretation. Exit barriers are high: rights claimants must either construct historical evidence (difficult, requires specialized expertise), pursue amendment (requires supermajority consensus), or accept foreclosure. Theater ratio (0.38): Moderate and rising. Originalist opinions generally function as genuine constraint: judges cite historical sources and attempt to follow historical meaning (lower theater). But the ratio is rising because originalist historical narratives increasingly show performative characteristics: selective citation, invented historical consensus, footnotes that perform erudition rather than resolve interpretive ambiguity. The constraint is degrading toward piton territory as institutional legitimacy increasingly rests on the performance of scholarship rather than on the actual binding force of historical methodology.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a maximum perspectival gap across the six types. The powerless rights claimant experiences a snare with no exit. The moderate progressive movement experiences a snare with constrained exits (litigation, amendment, persuasion). The originalist judiciary experiences a rope — they see their method as a neutral coordination mechanism that constrains discretion and provides clear rules. The amendment coalition experiences a scaffold — they see the constraint as temporary, with a structured exit path (formal amendment). The historical-evidence community experiences a piton — they maintain the constraint through institutional inertia and performative scholarship. The civilizational analytical observer risks experiencing a mountain — treating the constraint as an inherent feature of constitutional law — but structural analysis reveals it as a false summit: a contingent methodological choice naturalized as immutable logic. The gap between snare (victim perspective) and rope (beneficiary perspective) reveals the constraint's core asymmetry: what beneficiaries experience as neutral coordination, victims experience as foreclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural relationship to the constraint. Rights claimants lack exit (trapped) and suffer from the constraint (victims), producing high d → high χ. The originalist judiciary benefits from the constraint (beneficiaries) and has substantial exit options (arbitrage: they can reinterpret history, switch methodologies, or justify outcomes through historical reasoning), producing low d → negative χ. Amendment advocates are organized and have some agency (constrained exit), producing moderate d → moderate χ. The historical-evidence community captures institutional legitimacy from maintaining the constraint (beneficiaries) with arbitrage options, producing low d. The analytical observer is in an ambiguous position (identity_locked in their own analytical frame): they recognize the constraint's contingency but are bound by the institutional expectation that constitutional interpretation requires some binding methodology. The engine computes d automatically from these beneficiary/victim declarations and exit options; the narrative justifies why each agent has that structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy in a single type — it instantiates the mandatrophy across all six types. The originalist reading exhibits: snare (from victim perspective), rope (from beneficiary perspective), scaffold (from amendment perspective), piton (from institutional-maintenance perspective), mountain (from false natural law perspective), and the theoretical integration never collapses into a single type. This is appropriate: the constraint is a reading of a contested kernel. Different parties hold different readings (originalist, living constitutionalist, positivist), and each reading entails a different constraint structure. The mandatrophy is not a failure of the framework — it is a feature of how contested kernels operate. The origalist reading IS a snare for rights claimants; it IS a rope for originalist judges; both are true from within their respective readings. The analytical observer's job is to track the perspectival gap, not to collapse it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_record_reconstruction,
    'What constitutes reliable ''historical public meaning'' at ratification when the historical record is fragmentary, contested, and extensively mediated by subsequent interpretation?',
    'Historiographical analysis comparing originalist historical claims to contemporary historical scholarship; identification of cases where originalist historical narrative contradicts or omits primary source evidence',
    'If historical record is genuinely recoverable: originalism constrains judicial discretion (snare classification holds). If record is irreducibly ambiguous: originalism masks discretionary choice (reclassify toward piton or snare with higher theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_record_reconstruction, empirical, 'Recoverability and reliability of historical public meaning at ratification').

omega_variable(
    vagueness_application_gap,
    'When original public meaning is genuinely vague (e.g., ''cruel and unusual'' punishment), does applying that vagueness to contemporary facts constitute binding the judge or delegating discretion to each era?',
    'Doctrinal analysis of originalist judicial opinions addressing vague constitutional terms; tracking instances where ''applying the original meaning'' to new fact patterns produces divergent judicial outcomes despite identical interpretive methodology',
    'If vagueness creates wide application latitude: orignal binding is illusory (snare is actually piton with high theater; judges have discretion masked by method). If originalist method constrains application: snare classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vagueness_application_gap, conceptual, 'Whether vague original meaning constrains application or permits discretionary extension').

omega_variable(
    amendment_sufficiency_exit,
    'Is constitutional amendment a genuine exit pathway for rights claimants, or does it function as a theatrical escape hatch that is normatively (not legally) foreclosed?',
    'Historical analysis of amendment proposal rates vs ratification rates; survey of whether progressive movements view amendment as real option or as symbolic gesture; tracking of how originalist rhetoric responds to amendment pressure',
    'If amendment is real exit: scaffold classification (temporary constraint with sunset) is correct. If amendment is normatively foreclosed (e.g., supermajority requirement makes amendment impossible for minority rights): snare has no practical exit (maximum extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_sufficiency_exit, empirical, 'Whether amendment pathway provides genuine or theatrical exit for rights claimants').

omega_variable(
    reading_kernel_contest,
    'Which reading of the US Constitution — originalist, living constitutionalist, or positivist — correctly describes how constitutional meaning is actually fixed and transmitted?',
    'This is the irreducible structural ambiguity at the kernel level. The kernel (the Constitution as stabilized text) admits multiple readings. Originalism asserts meaning is fixed at ratification; living constitutionalism asserts meaning evolves with changing understanding; positivism asserts meaning is whatever courts say it is.',
    'If originalism is correct: this constraint is legitimate binding (mountain or rope, not snare). If living constitutionalism is correct: this constraint is a false natural law (snare naturalized as mountain). If positivism is correct: the constraint is entirely performative (piton).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Irreducible contest over how constitutional meaning is fixed and transmitted across readings').

omega_variable(
    identity_lock_judicial,
    'Are originalist judges committed to the methodology because it genuinely constrains their discretion, or because their professional identity and career trajectory are fused with originalism as an institutional movement?',
    'Biographical analysis of judicial conversions (judges adopting/abandoning originalism); tracking of originalist outcomes that contradict the judge''s stated political preferences vs non-originalist outcomes that align with preference',
    'If identity-locked: originalist judges would experience the constraint as rope (neutral, binding method) from inside their frame, but external analysis would reveal it as snare or piton (extraction masked by methodological theater). The perspectival gap reveals the binding is cognitive, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_judicial, empirical, 'Whether originalist judicial commitment is to methodology or to institutional identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orig_tr_t0, originalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(orig_tr_t20, originalist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(orig_tr_t40, originalist_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(orig_be_t0, originalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orig_be_t20, originalist_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(orig_be_t40, originalist_reading, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% The US Constitution meaning is contested across three readings: originalist, living constitutionalist, and positivist. Each reading generates a distinct constraint story with different ε values, beneficiary/victim structures, and classification. The three constraints form a kernel family linked by affecting_constraints edges. The originalist reading (this story) has ε=0.58; the living constitutionalist and positivist readings will have their own ε values reflecting their own structural relationships to the constitutional text. All three stories decompose the single natural-language concept 'how constitutional meaning is fixed' into structurally precise claims with different observables and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
