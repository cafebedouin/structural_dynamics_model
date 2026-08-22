% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Study as Preparation for Messianic Temple Restoration
 *   domain: religious/textual/legal
 *
 * SUMMARY:
 *   The Kodashim (Sacrificial law) remain binding in Jewish halakhic
 *   tradition despite the Temple's destruction and the impossibility of their
 *   performance for nearly two millennia. This story instantiates the
 *   'study-as-preparation' reading: sacrificial law is binding precisely
 *   because study preserves the technical knowledge required to resume
 *   performance when the Temple is restored in the messianic age. The study
 *   is not mere history or cultural memory — it is an active halakhic
 *   obligation grounded in the principle that the law's binding force does
 *   not depend on present performability, only on the law's own authority.
 *   This reading competes with two siblings: 'study-as-archive' (Kodashim
 *   documents a defunct system; study is historical preservation, not cosmic
 *   obligation) and 'study-as-performance' (studying sacrificial law itself
 *   enacts the cosmic function of sacrifice; physical Temple absence is
 *   irrelevant). All three readings draw on shared textual sources but
 *   interpret the law's function, the study's obligation, and the endpoint
 *   differently. The preparation reading uniquely defines itself by deferral:
 *   the constraint is binding now but performable only in the future, making
 *   it structurally a scaffold — temporary support for a transition whose
 *   completion is postponed indefinitely.
 *
 * KEY AGENTS:
 *   - rabbinic_scholars_current_generation: carry the study obligation despite non-performance; bear deferral cost; moderate power; constrained exit (identity-locked by community and law)
 *   - messianic_future_community: non-agent placeholder for the eschatological beneficiary; receives preserved knowledge; analytically positioned; civilizational time horizon
 *   - halakhic_authority_structure: institutional agenda-setter; maintains binding obligation; grounds authority in law's persisting force regardless of Temple absence
 *   - competing_interpretations: excluded sibling readings (archive, performance); represent alternative communities and textual framings
 *   - broader_jewish_community: observers; receive cultural transmission and identity affirmation from tradition's survival; not directly obligated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.28).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.15).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, scaffold).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study as Preparation for Messianic Temple Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/textual/legal").

narrative_ontology:has_sunset_clause(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, 'a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa').
narrative_ontology:cs_kernel_codification('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', fixed_text).
narrative_ontology:cs_authority_grounding('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', lineage).
narrative_ontology:cs_interpretation_layer_present('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa').
narrative_ontology:cs_reading_relation('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', kodashim_obligation__study_as_archive, influences).
narrative_ontology:cs_reading_relation('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', foundational, law_binding_persists_across_performance_gap).
narrative_ontology:cs_axiom_status(law_binding_persists_across_performance_gap, holdable).
narrative_ontology:cs_axiom_grounding('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', law_binding_persists_across_performance_gap, deontological).
narrative_ontology:cs_axiom('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', foundational, study_instrumental_preparation_for_restoration).
narrative_ontology:cs_axiom_status(study_instrumental_preparation_for_restoration, holdable).
narrative_ontology:cs_axiom_grounding('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', study_instrumental_preparation_for_restoration, deontological).
narrative_ontology:cs_reference_frame('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', law_binding_regardless_of_performance_capacity).
narrative_ontology:cs_drift_state('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', contemporary_indefinite_messianic_deferral, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a7e1d659-4fa2-4c5d-8b51-fd5dc8dea0aa', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_community).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, rabbinic_scholars_current_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the obligation to study sacrificial law (Kodashim) in exhaustive technical detail despite centuries of inability to perform it. They invest years mastering architectural, procedural, and interpretive knowledge whose practical application is permanently deferred. Their exit options are limited by religious community expectation and the binding force of halakhic obligation. They cannot simply opt out of Kodashim study without rejecting the authority structure that grounds their identity as observant Jews.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_scholars_current_generation, payer,
    moderate, biographical, constrained, global).

% A non-agent entity representing the eschatological future state when the Temple is restored and sacrificial law becomes performable again. Benefits from the accumulated, preserved technical knowledge maintained by current-generation study. Does not exist in the present and cannot reciprocate or modify the constraint during the current dispensation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future_community, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, messianic_future_community).

% Maintains and enforces the binding nature of Kodashim study obligation. Grounds its authority in the rabbinical reading that the law remains in force regardless of Temple absence, and that studying it is a form of performance that preserves cosmic order and readies the system for eschatological restoration. Administers the interpretation that this study is not mere memory but active legal obligation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, halakhic_authority_structure, agenda_setter,
    institutional, civilizational, analytical, global).

% Alternative readings of Kodashim (archive reading, performance reading) represent different communities and intellectual traditions that would reframe the obligation, its scope, or its function. They are excluded from the binding institutional consensus that defines the study-as-preparation reading, though their textual arguments remain live intellectual options in broader Judaic discourse.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, competing_interpretations, excluded,
    moderate, generational, constrained, global).

% Receives textual transmission and cultural identity transmission from the Kodashim study tradition without bearing the direct obligation. They witness and recognize the tradition's preservation function and may support it, but are not bound to study Kodashim themselves. Their interest is in whether the tradition survives and what it signals about Jewish law's persistence.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, broader_jewish_community, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves technical knowledge of sacrificial law across centuries of non-performance, maintaining both legal precedent and the cognitive infrastructure needed to restore Temple practice if and when the messianic age arrives. Solves the coordination problem: how does a community preserve working knowledge of a complex system that cannot be practiced but must remain ready?
% TRANSFER_FUNCTION: Transfers burden from messianic future beneficiary backward to current-generation scholars. Current scholars invest time and intellectual effort; the messianic future receives the preserved knowledge asset. The transfer is mediated by halakhic obligation, not by transaction or explicit agreement.
% ABSENT_VOICES: Non-Orthodox streams and secular scholars are excluded from the binding institutional consensus; their alternative framings (Kodashim as archive, as performance, as historical artifact) would reconfigure the obligation's meaning and enforceability. Voices within Orthodox communities that would question whether perpetual study of the unperformable serves cosmic order are marginalized by consensus authority.
% DISAPPEARANCE_RATIONALE: If the Kodashim study obligation disappeared, the Jewish legal tradition would lose the technical knowledge base required for Temple restoration and would signal that the messianic framework itself is no longer operative. Orthodox Jewish communities would interpret this disappearance as a doctrinal shift away from messianic expectation or away from the law's binding force in exile. Secular or alternative communities would see it as inevitable rationalization, but the broader tradition would experience it as a constitutional rupture.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), sacrificial law became unperformable but the halakhic system remained binding. The founding problem: how should an entire legal corpus remain authoritative when its central practices became impossible? The rabbinic response encoded in this reading: the study of the law is itself a form of performance that maintains cosmic order and preserves readiness for eventual restoration.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox halakhic authorities (Maimonides, Talmudic consensus) attest that study substitutes for performance and maintains binding obligation. Historical scholarship from outside the benefiting parties (secular Jewish Studies) attests that the founding problem was indeed solved by rabbinic innovation and that the study-as-preparation reading is one of multiple competing solutions. Competing religious readings (archive reading, performance reading) attest that the founding problem admits multiple coherent solutions.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) because the constraint imposes genuine cost (intellectual labor on scholars) while delivering deferred benefit (preserved knowledge for a future that may never arrive). Suppression is minimal (0.15) because the constraint is rarely enforced through coercion — it persists through institutional consensus and identity-fusion. Scholars remain committed to study not primarily through external pressure but through integration of the obligation into their halakhic identity and community role. Theater ratio is low-to-moderate (0.22) because the constraint's function is genuinely instrumental (knowledge preservation) but the increasing gap between preparation rhetoric and indefinite deferral introduces performative elements — study increasingly enacts readiness rather than producing readiness for a near restoration. Accessibility collapse is high (0.72) because once the binding obligation is accepted, alternatives effectively disappear: scholars embedded in Orthodox institutions have no legitimate exit, and those outside Orthodox authority structures (non-Orthodox movements, secular scholars) are excluded from the consensus rather than offered choice. Resistance is moderate (0.38) because the constraint encounters both acceptance (from those committed to messianic framework) and pushback (from modernizers, secularizers, alternative readings) without generating open rebellion within most Orthodox communities. The measurement series spans 2000 years to capture the long deferral arc: extractiveness rises gradually as the messianic horizon recedes in lived experience; theater ratio rises as the performance gap widens; by modernity, study functions increasingly as identity maintenance and archive despite being framed as preparation.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority structure and current-generation scholars experience this constraint from opposite pole positions. From the authority perspective, the constraint is rope-like: genuine coordination (knowledge preservation) with minimal extraction, sustained by institutional consensus and the law's own binding force. From the scholar perspective, the constraint is scaffold-like: they bear the cost of perpetual study whose payoff is deferred indefinitely, and their exit is blocked by the same institutional framework that makes the obligation binding. The engine should compute different types from these two seats — authority as beneficiary (low d, low effective extraction) and scholars as payer (high d, higher effective extraction). The broader Jewish community observes the constraint without bearing it directly, enjoying the cultural identity transmission it provides (slight beneficiary position) while remaining outside the binding obligation. This multi-seat divergence is exactly the measurement the DR engine performs: the claim (scaffold) is authored from the structural perspective; the metrics (moderate extractiveness, low suppression, rising theater) describe how it actually operates. Where computed seats diverge from the claim, that divergence is the data — a claimed scaffold that computes as rope from the authority seat and as snare from the scholar seat reveals the constraint's true shape as seen from each position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the beneficiary/victim split and exit options. Messianic future is the nominal beneficiary (receives preserved knowledge) but is non-agent and analytically positioned (d does not apply). Current-generation scholars are the victims in this reading (d near 1.0): they bear the obligation, cannot exit without rejecting their identity within the community, and receive no present benefit. The halakhic authority structure is the agenda-setter and secondary beneficiary (d low): it maintains the obligation, derives authority from the law's binding force, and collects the cultural and institutional benefits of perpetuating the tradition. Competing readings are excluded (not seated in the obligation matrix). The broader community sits as observer (d near 0.5): they receive transmission benefit (slight positive) but also bear diffuse cultural cost (maintaining the tradition requires institutional resources).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preservation of knowledge for unperformable law) was solved by rabbinic innovation establishing that study substitutes for performance. The mandatrophy question: has this solution's function outlived its original justification? If the messianic restoration is indefinitely deferred (Ω_high on the timeline omega), then the constraint's function shifts: study is no longer preparation but archive or performance. The preparation reading entails a genuine sunset clause (restoration of Temple, resumption of performance), but the historical trajectory shows indefinite deferral. This creates a mandatrophy condition: the constraint remains bound to its founding problem (readiness for restoration) but the founding problem's timeline has become indefinitely extended. The institutional response is theater: study continues to be framed as preparation while increasingly functioning as identity maintenance and archive. The measurement series captures this: theater ratio rises over time as the performance gap widens. A contemporary challenge: younger Orthodox communities increasingly question whether perpetual study of the unperformable serves the claimed function, suggesting institutional consensus may be fragile — another mandatrophy signal. The constraint could resolve mandatrophy by shifting reading (to archive or performance) or by explicitly embracing indefinite deferral as the normal state. Currently it remains in mandate-atrophy limbo: bound to a function (preparation for restoration) whose timeline has become open-ended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_uncertainty,
    'Is the messianic restoration (the condition for the constraint''s sunset) a historical expectation with meaningful timeline, or has it become indefinitely deferred?',
    'Historical tracking of explicit rabbinic and community expectations about messianic timing; measurement of how the deferral changes study''s perceived function over centuries.',
    'If the restoration is indefinitely deferred (Ω_high), the constraint becomes permanent despite being authored as scaffold; study shifts from preparation toward archive or performance reading. If restoration remains a live expectation (Ω_low), the constraint retains its preparatory function and sunset clause remains credible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_timeline_uncertainty, empirical, 'Whether the messianic restoration is a historical expectation or indefinitely deferred.').

omega_variable(
    study_as_preparation_vs_performance,
    'Does studying sacrificial law count as a form of performance that enacts cosmic function in the present, or is it purely instrumental preparation for future performance?',
    'Textual analysis of halakhic sources distinguishing the preparation reading from the performance reading; identification of which communities hold which interpretation and how they justify the functional difference.',
    'If study IS performance (performance reading), extractiveness drops further and the constraint is rope-like (pure coordination without deferral burden). If study is ONLY preparation (this reading), extractiveness is moderate and burden is borne by current generation for future benefit — a classic scaffold structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_preparation_vs_performance, conceptual, 'Whether study itself constitutes performance or is merely preparatory.').

omega_variable(
    institutional_consensus_fragility,
    'Is the binding force of Kodashim study obligation dependent on consensus authority that could erode, or is it rooted in halakhic sources that remain binding regardless of interpretive fashion?',
    'Tracking whether younger Orthodox communities and institutions continue to mandate Kodashim study with the same intensity and whether non-Orthodox movements formally abandon the obligation.',
    'If consensus is fragile and eroding (Ω_high), the constraint is contingent on ongoing institutional maintenance and could shift toward archive reading. If the obligation is anchored in immutable textual authority (Ω_low), it persists regardless of institutional pressure and the scaffold structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_consensus_fragility, empirical, 'Whether the binding obligation depends on institutional consensus or textual authority.').

omega_variable(
    reading_kernel_containment,
    'Can the preparation reading coexist with the archive and performance readings within a single Orthodox framework, or does the choice of reading entail rejection of the others?',
    'Documentary analysis of how different communities and authorities frame the three readings; whether a single institution holds multiple framings in tension or whether each reading entails a distinct community boundary.',
    'If the readings coexist (type: coexists_with), the constraint is one position in an ongoing debate. If preparation forecloses archive or performance (type: forecloses), the constraint''s authority depends on maintaining institutional barriers against competing framings. If preparation influences but does not foreclose (type: influences), the constraint is hegemonic within Orthodox institutions but acknowledges live alternatives in broader Jewish discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_containment, conceptual, 'Whether the three readings are logically separable or entail mutual exclusion within a single institutional framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(koda_tr_t400, kodashim_obligation__study_as_preparation, theater_ratio, 400, 0.12).
narrative_ontology:measurement(koda_tr_t800, kodashim_obligation__study_as_preparation, theater_ratio, 800, 0.15).
narrative_ontology:measurement(koda_tr_t1200, kodashim_obligation__study_as_preparation, theater_ratio, 1200, 0.19).
narrative_ontology:measurement(koda_tr_t1600, kodashim_obligation__study_as_preparation, theater_ratio, 1600, 0.22).
narrative_ontology:measurement(koda_tr_t1900, kodashim_obligation__study_as_preparation, theater_ratio, 1900, 0.24).
narrative_ontology:measurement(koda_tr_t2000, kodashim_obligation__study_as_preparation, theater_ratio, 2000, 0.22).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(koda_be_t400, kodashim_obligation__study_as_preparation, base_extractiveness, 400, 0.22).
narrative_ontology:measurement(koda_be_t800, kodashim_obligation__study_as_preparation, base_extractiveness, 800, 0.25).
narrative_ontology:measurement(koda_be_t1200, kodashim_obligation__study_as_preparation, base_extractiveness, 1200, 0.28).
narrative_ontology:measurement(koda_be_t1600, kodashim_obligation__study_as_preparation, base_extractiveness, 1600, 0.28).
narrative_ontology:measurement(koda_be_t1900, kodashim_obligation__study_as_preparation, base_extractiveness, 1900, 0.27).
narrative_ontology:measurement(koda_be_t2000, kodashim_obligation__study_as_preparation, base_extractiveness, 2000, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_preparation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, resource_allocation).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_preparation, 0.12).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is part of the kodashim_obligation kernel family, which decomposes into three structurally distinct constraints based on competing readings of the law's function after Temple destruction. Each reading instantiates a different ε value and beneficiary/victim structure. The preparation reading (this file) treats study as instrumental preparation (moderate ε, current generation as victim, messianic future as non-agent beneficiary, scaffold structure). The archive reading treats study as historical preservation (low ε, broader community as beneficiary, rope structure). The performance reading treats study as present cosmic function (low ε, current generation as beneficiary, rope structure). The three readings coexist in Jewish legal discourse; no single reading forecloses the others at the institutional level, though different communities emphasize different framings. Links: preparation → archive (archive reading absorbs preparation if messianic deferral becomes indefinite), preparation → performance (performance reading forecloses preparation if present efficacy is embraced), archive ⇄ performance (horizontal coexistence, different framings of the same community function).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
