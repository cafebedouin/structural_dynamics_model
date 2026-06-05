% ============================================================================
% CONSTRAINT STORY: establishment_clause__history_tradition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_establishment_clause__history_tradition_reading, []).

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
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: establishment_clause__history_tradition_reading
 *   human_readable: Establishment Clause (History-Tradition Reading)
 *   domain: legal/constitutional/religious_freedom
 *
 * SUMMARY:
 *   The history-tradition reading of the Establishment Clause holds that
 *   government action cannot establish religion if longstanding practices
 *   from the founding generation and unbroken tradition permitted it. This
 *   reading produces a distinctive power asymmetry: practices grandfathered
 *   by historical pedigree are protected from Establishment Clause challenge,
 *   while novel government religious endorsements are strictly scrutinized.
 *   The reading appears to government agents and defenders of traditional
 *   religious practices as pure coordination — a clear rule distinguishing
 *   what is permitted. But from the perspective of religious minorities
 *   asserting novel Establishment Clause claims, the reading is an asymmetric
 *   extraction mechanism that locks in power from the founding era. From the
 *   powerless victims of longstanding endorsements (e.g., religious
 *   minorities disadvantaged by centuries-old legislative prayer), the
 *   reading is a snare — they cannot exit from grandfathered violations. The
 *   constraint exemplifies how a single constitutional interpretation can
 *   yield different types depending on the observer's structural position
 *   relative to the grandfathered baseline.
 *
 * KEY AGENTS:
 *   - Government Maintaining Traditional Practices: Institutional beneficiary (institutional/arbitrage) — grandfathering longstanding practices provides clear authority to continue them without Establishment Clause scrutiny
 *   - Religious Minorities Asserting Novel Claims: Moderate victim (moderate/constrained) — face higher scrutiny for new Establishment Clause violations but can theoretically organize litigation; extraction is asymmetric but not total
 *   - Religious Minorities Under Historical Endorsements: Powerless victim (powerless/trapped) — disadvantaged by grandfathered practices (e.g., legislative prayer, religious monuments established before the 20th century); no legal avenue to challenge them
 *   - The Analytical Observer: Civilizational position (analytical/analytical) — sees the reading as both coordinating (clear boundaries) and extracting (locks in historical power asymmetry)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(establishment_clause__history_tradition_reading, 0.52).
domain_priors:suppression_score(establishment_clause__history_tradition_reading, 0.68).
domain_priors:theater_ratio(establishment_clause__history_tradition_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(establishment_clause__history_tradition_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(establishment_clause__history_tradition_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(establishment_clause__history_tradition_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(establishment_clause__history_tradition_reading, tangled_rope).
narrative_ontology:human_readable(establishment_clause__history_tradition_reading, "Establishment Clause (History-Tradition Reading)").
narrative_ontology:topic_domain(establishment_clause__history_tradition_reading, "legal/constitutional/religious_freedom").

domain_priors:requires_active_enforcement(establishment_clause__history_tradition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(establishment_clause__history_tradition_reading, '81188edd-9aee-4113-a54c-02926261c5bf').
narrative_ontology:cs_kernel_codification('81188edd-9aee-4113-a54c-02926261c5bf', fixed_text).
narrative_ontology:cs_authority_grounding('81188edd-9aee-4113-a54c-02926261c5bf', lineage).
narrative_ontology:cs_interpretation_layer_present('81188edd-9aee-4113-a54c-02926261c5bf').
narrative_ontology:cs_reading_relation('81188edd-9aee-4113-a54c-02926261c5bf', establishment_clause__coercion_test_reading, coexists_with).
narrative_ontology:cs_reading_relation('81188edd-9aee-4113-a54c-02926261c5bf', establishment_clause__endorsement_test_reading, influences).
narrative_ontology:cs_reading_relation('81188edd-9aee-4113-a54c-02926261c5bf', establishment_clause__lemon_test_reading, coexists_with).
narrative_ontology:cs_axiom('81188edd-9aee-4113-a54c-02926261c5bf', foundational, longstanding_practice_immunized_from_establishment_clause).
narrative_ontology:cs_axiom_status(longstanding_practice_immunized_from_establishment_clause, holdable).
narrative_ontology:cs_axiom_grounding('81188edd-9aee-4113-a54c-02926261c5bf', longstanding_practice_immunized_from_establishment_clause, conventional).
narrative_ontology:cs_axiom('81188edd-9aee-4113-a54c-02926261c5bf', foundational, founding_generation_consent_legitimates_government_religious_practice).
narrative_ontology:cs_axiom_status(founding_generation_consent_legitimates_government_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('81188edd-9aee-4113-a54c-02926261c5bf', founding_generation_consent_legitimates_government_religious_practice, deontological).
narrative_ontology:cs_reference_frame('81188edd-9aee-4113-a54c-02926261c5bf', founding_generation_practice_baseline).
narrative_ontology:cs_drift_state('81188edd-9aee-4113-a54c-02926261c5bf', contemporary_religious_pluralism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('81188edd-9aee-4113-a54c-02926261c5bf', '').
narrative_ontology:cs_kernel_id(establishment_clause__history_tradition_reading, establishment_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(establishment_clause__history_tradition_reading, longstanding_religious_practices).
narrative_ontology:constraint_beneficiary(establishment_clause__history_tradition_reading, traditional_government_religious_endorsements).
narrative_ontology:constraint_victim(establishment_clause__history_tradition_reading, novel_establishment_claims).
narrative_ontology:constraint_victim(establishment_clause__history_tradition_reading, religious_minorities_asserting_new_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GOVERNMENT MAINTAINING TRADITION (ROPE) — The government benefits from the history-tradition reading because longstanding religious endorsements (legislative prayer, religious monuments, military chaplaincies) are grandfathered in and protected from challenge. The reading provides coordination: government agents understand what is permitted (old practices) and what is forbidden (new impositions). This is experienced as pure coordination with no extraction — the beneficiary of the reading agrees it clarifies boundaries.
constraint_indexing:constraint_classification(establishment_clause__history_tradition_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS MINORITIES / NOVEL CLAIMANTS (TANGLED ROPE) — Groups asserting new Establishment Clause violations (e.g., religious monuments on public property erected in the last 50 years, new government religious endorsements) face a constraint with genuine asymmetry. The history-tradition reading extracts from them by grandfathering old violations while blocking new ones. But the constraint also provides coordination: the rule is predictable and applies equally to all novel claims. Constrained exit because challenging the historical baseline requires constitutional revision, not just litigation.
constraint_indexing:constraint_classification(establishment_clause__history_tradition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MINORITIES UNDER HISTORICAL ENDORSEMENTS (SNARE) — Groups disadvantaged by longstanding religious practices (e.g., legislative prayer benefiting Christian traditions, religious monuments from the founding era) have no exit from the constraint. They cannot sue to remove established practices because the history-tradition reading grandfathers them in. They are trapped: powerless to change the status quo, bearing the ongoing cost of government religious endorsement, with suppression (no legal avenue) maintained by the reading's durability.
constraint_indexing:constraint_classification(establishment_clause__history_tradition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The history-tradition reading functions as both coordination and extraction. Coordination: it provides clear boundaries (old practices permitted, new ones scrutinized). Extraction: it locks in power asymmetries from the founding era (the founding generation was Christian-majority; practices favoring Christianity are grandfathered). The reading coordinates by freezing a particular historical snapshot as immutable. The snapshot happens to benefit established traditions at the expense of religious minorities with novel claims. This is the analytical position from which mandatrophy resolves: the reading is NOT pure coordination (it has asymmetric extraction) but also NOT pure extraction (it provides genuine boundary-clarity coordination). The misclassification risk is calling it pure coordination (Rope) when it is actually Tangled Rope.
constraint_indexing:constraint_classification(establishment_clause__history_tradition_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(establishment_clause__history_tradition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(establishment_clause__history_tradition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(establishment_clause__history_tradition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(establishment_clause__history_tradition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(establishment_clause__history_tradition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The history-tradition reading benefits practices that have longstanding pedigree and extracts from novel claims. The asymmetry is real — new government religious endorsements face strict scrutiny while old ones are grandfathered. But the extraction is not maximal because the rule is predictable and applies uniformly to all novel claims (coordination function). The rising measurement trajectory (0.35 → 0.58) reflects that as more practices acquire historical pedigree (religious monuments, legislative prayer traditions become older), the grandfathering benefit compounds, increasing the extraction from novel challengers. Suppression (0.68): Moderate-high. Religious minorities harmed by grandfathered practices face substantial barriers: they cannot sue to remove established practices; the reading denies them the legal avenue of the Establishment Clause. However, suppression is not maximal (0.68, not 0.90) because new practices CAN be challenged, and theoretically new religious minorities could file test cases. Theater ratio (0.58): Moderate. The history-tradition reading requires judicial determination of what counts as 'founding-era tradition' and whether a practice has maintained 'unbroken continuity.' This involves historical and textual argument that is partly substantive (does the history actually support the claim?) and partly performative (the court's authority to declare what counts as tradition). The rising trajectory reflects increasing courtroom ritual around historical justification as more practices claim grandfathering status.
 *
 * PERSPECTIVAL GAP:
 *   The government and defenders of traditional religious practices perceive the history-tradition reading as Rope: a coordination mechanism that clarifies what is permissible (old) and what is not (new). Religious minorities asserting novel Establishment Clause violations perceive Tangled Rope: the rule provides some coordination (predictability) but extracts by raising the bar for novel claims. Religious minorities trapped under grandfathered practices perceive Snare: the reading provides no exit and no remedy. The analytical observer perceives Tangled Rope as the accurate classification: the reading coordinates while extracting, and misclassifying it as pure Rope ignores the real asymmetry that harms minorities under historical endorsements.
 *
 * DIRECTIONALITY LOGIC:
 *   The history-tradition reading creates directionality asymmetry through the grandfathering mechanism. Government agents and traditional practice defenders have arbitrage options: they can continue old practices without legal risk, giving them low d (beneficiary position, low effective extraction). Religious minorities with novel claims have constrained exit: they must convince courts that practices are novel enough to scrutinize, a high bar. Minorities under grandfathered practices have trapped exit: they have no legal remedy. The reading's d values range from ~0.15 (government beneficiary) to ~0.85 (powerless victim under grandfathered practice), producing a wide perspectival gap. The moderate victim (religious minority asserting novel claim) occupies middle ground at ~0.55, experiencing both some extraction and some coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The history-tradition reading resolves mandatrophy by acknowledging that it is genuinely Tangled Rope, not pure Rope. The misclassification risk is treating it as pure coordination (Rope) because it clarifies boundaries. But the boundaries are asymmetric: old practices are protected while new ones are scrutinized. This asymmetry is not a bug in the reading but its core feature — it intentionally preserves traditional practices. The mandatrophy is resolved by accepting that constitutional readings can coordinate while extracting, and that the extraction serves the reading's core function (protecting tradition). The misclassification that the reading prevents is calling it pure extraction (Snare), which would ignore the genuine coordination function it provides to government agents and defenders of tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_era_baseline_ambiguity,
    'What counts as the ''founding generation and unbroken tradition'' — only practices predating 1789, or practices established by 1950, or all longstanding practices with some minimum duration?',
    'Historical and jurisprudential analysis: examine how courts have actually applied the history-tradition test to determine the working threshold. Cases like Town of Greece v. Galloway show courts accepting 19th-century legislative prayer as falling within the tradition; other courts may accept earlier cutoffs.',
    'If threshold is strict (pre-1789 only): many 19th and 20th century practices are vulnerable. If threshold is lenient (50+ years continuous): nearly all existing practices are grandfathered. The reading''s suppression value depends on where the threshold is drawn.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_era_baseline_ambiguity, empirical, 'The historical cutoff for ''founding-era tradition''').

omega_variable(
    tradition_continuity_requirement,
    'Does a practice need to be continuous from the founding era, or is resurrection of an abandoned practice (reintroduction after decades of absence) enough to claim ''tradition''?',
    'Case law analysis: whether courts allow practices that were dropped and then reestablished. If continuity is required, practices interrupted by periods of suppression lose tradition status. If resurrection counts, practices can be revived.',
    'If strict continuity required: suppression of discontinued practices is durable. If resurrection counts: suppression is weaker, and new practices can claim grandfathering if they can be traced to historical precedent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tradition_continuity_requirement, empirical, 'Whether tradition requires unbroken continuity or allows practice resurrection').

omega_variable(
    tradition_test_versus_coercion_test_foreclosure,
    'Does the history-tradition reading logically foreclose the coercion-test reading, or can both readings coexist with different institutional domains?',
    'Jurisprudential analysis: examine whether history-tradition and coercion-test produce contradictory outcomes for the same government action. If they classify the same practice differently, which reading''s core premise must be abandoned?',
    'If they foreclose each other: only one reading can be constitutionally valid. If they coexist: different courts or jurisdictions can adopt different tests. The kernel contest hinges on whether these readings are logically incompatible or merely empirically different.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tradition_test_versus_coercion_test_foreclosure, conceptual, 'Whether history-tradition reading forecloses coercion-test reading').

omega_variable(
    originalism_versus_living_constitution_presupposition,
    'Does the history-tradition reading presuppose an originalist constitutional hermeneutics, or can it be adopted from a living-constitution framework?',
    'Doctrinal analysis: trace whether courts adopting the history-tradition test ground it in originalist premises (founders'' original understanding immutable) or in common-law traditionalism (practices acquire legitimacy through continuity). The same test can rest on different meta-constitutional assumptions.',
    'If originalist presupposition required: the reading is tied to strict originalism and forecloses living-constitution readings of the Establishment Clause. If traditionalism works independent of originalism: the reading is compatible with multiple constitutional hermeneutics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_versus_living_constitution_presupposition, conceptual, 'Whether history-tradition reading requires originalist presuppositions').

omega_variable(
    asymmetry_between_grandfathering_and_novel_scrutiny,
    'Is the asymmetry between grandfathered old practices and scrutinized new practices a feature of the history-tradition reading (intentionally protects tradition) or a bug (inconsistently applies the Constitution''s text)?',
    'Constitutional theory: whether the Establishment Clause is better read as protecting stability of longstanding arrangements (feature) or as a timeless rule applying equally regardless of historical timing (bug). This determines whether extractiveness (0.52) is justified coordination overhead or unjustified asymmetric extraction.',
    'If feature: suppression is legitimate stability mechanism; extractiveness is coordination cost. If bug: suppression is unconstitutional grandfather clause; extractiveness is illicit extraction. The classification itself depends on this normative judgment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetry_between_grandfathering_and_novel_scrutiny, preference, 'Whether grandfathering old practices is constitutionally justified').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(establishment_clause__history_tradition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esta_tr_t0, establishment_clause__history_tradition_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(esta_tr_t50, establishment_clause__history_tradition_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(esta_tr_t100, establishment_clause__history_tradition_reading, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(esta_be_t0, establishment_clause__history_tradition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(esta_be_t50, establishment_clause__history_tradition_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(esta_be_t100, establishment_clause__history_tradition_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(esta_su_t0, establishment_clause__history_tradition_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(esta_su_t50, establishment_clause__history_tradition_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(esta_su_t100, establishment_clause__history_tradition_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(establishment_clause__history_tradition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(establishment_clause__history_tradition_reading, establishment_clause__coercion_test_reading).
narrative_ontology:affects_constraint(establishment_clause__history_tradition_reading, establishment_clause__endorsement_test_reading).
narrative_ontology:affects_constraint(establishment_clause__history_tradition_reading, establishment_clause__lemon_test_reading).

% DUAL FORMULATION NOTE:
% The Establishment Clause kernel has been decomposed into four structurally distinct constraint stories, one for each competing reading. Each reading produces a different classification when applied to the same government actions. The history-tradition reading is the most protective of longstanding practices and the most extractive toward novel claimants. Each story has its own epsilon value, beneficiary/victim structure, and perspectives. They are linked by network.affects_constraints to show how different constitutional interpretations create different constraint structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(establishment_clause__history_tradition_reading, institutional, 0.18).
constraint_indexing:directionality_override(establishment_clause__history_tradition_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
