% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation in Messianic Suspension
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   After the destruction of the Second Temple in 70 CE, the halakhic system
 *   faced a crisis: commandments related to Temple sacrifice became
 *   materially impossible. The messianic suspension reading resolves this by
 *   categorizing the obligation as neither fulfilled, violated, nor
 *   abrogated, but suspended — deferred to a future restoration event when
 *   the Temple will be rebuilt and the obligation adjudicated by messianic
 *   authority. Study of sacrifice law is authorized as the legitimate form of
 *   engagement during suspension, maintaining the obligation in cognitive
 *   form without fulfilling it. This reading coexists with alternatives that
 *   reframe study as either occupying the obligation's place
 *   (study-as-occupation) or archiving knowledge for future restoration
 *   (study-as-archiving). The constraint here is the suspension framework
 *   itself: the deferral structure that keeps the obligation alive without
 *   requiring current performance.
 *
 * KEY AGENTS:
 *   - Torah_study_practitioners: engage in daily study as the authorized response to suspension
 *   - Halakhic_authority_structure: maintains the framework that classifies suspension and arbitrates new teachings
 *   - Jewish_diaspora_communities: live under conditions of material impossibility and depend on the suspension doctrine for coherence
 *   - Messianic_anticipation_framework: the abstract commitment structure that grounds the entire deferral (observer/non-agent)
 *   - Alternative_obligation_readings: excluded from authority within this framework but live in parallel communities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.08).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.12).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation in Messianic Suspension").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '0d352009-2c96-4421-8f9c-c818ef5f4b40').
narrative_ontology:cs_kernel_codification('0d352009-2c96-4421-8f9c-c818ef5f4b40', fixed_text).
narrative_ontology:cs_authority_grounding('0d352009-2c96-4421-8f9c-c818ef5f4b40', lineage).
narrative_ontology:cs_interpretation_layer_present('0d352009-2c96-4421-8f9c-c818ef5f4b40').
narrative_ontology:cs_reading_relation('0d352009-2c96-4421-8f9c-c818ef5f4b40', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_reading_relation('0d352009-2c96-4421-8f9c-c818ef5f4b40', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_axiom('0d352009-2c96-4421-8f9c-c818ef5f4b40', foundational, obligation_state_deferral_possible).
narrative_ontology:cs_axiom_status(obligation_state_deferral_possible, holdable).
narrative_ontology:cs_axiom_grounding('0d352009-2c96-4421-8f9c-c818ef5f4b40', obligation_state_deferral_possible, deontological).
narrative_ontology:cs_axiom('0d352009-2c96-4421-8f9c-c818ef5f4b40', secondary, future_authority_adjudication_binding).
narrative_ontology:cs_axiom_status(future_authority_adjudication_binding, holdable).
narrative_ontology:cs_axiom_grounding('0d352009-2c96-4421-8f9c-c818ef5f4b40', future_authority_adjudication_binding, theological).
narrative_ontology:cs_reference_frame('0d352009-2c96-4421-8f9c-c818ef5f4b40', temple_destroyed_obligation_suspended).
narrative_ontology:cs_drift_state('0d352009-2c96-4421-8f9c-c818ef5f4b40', contemporary_2000_years_post_destruction, gap(stable, substantial, true)).
narrative_ontology:cs_created_at('0d352009-2c96-4421-8f9c-c818ef5f4b40', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, torah_study_practitioners).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, halakhic_authority_structure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, jewish_diaspora_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in daily study of sacrifice law and Temple procedure as the authorized form of engagement with the commandment during suspension. The study itself is neither atonement nor preparation but maintains the obligation in cognitive form. Practitioners benefit from a coherent status that honors both the commandment's binding force and the Temple's destruction — they are neither violating an active law nor pretending violation carries no weight.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, torah_study_practitioners, beneficiary,
    moderate, civilizational, mobile, global).

% Maintains the framework that classifies the obligation as suspended rather than abrogated or fulfilled. Preserves the standing distinction between study-as-substitute and study-as-archiving, and adjudicates whether new teachings about the obligation are permissible or constitute encroachment on the future restoration's authority. Does not collect rents from the arrangement but administers the halakhic status quo.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_authority_structure, agenda_setter,
    institutional, civilizational, analytical, global).

% Live under a legal framework (diaspora law, secular states) that makes Temple sacrifice impossible. The suspension doctrine resolves the contradiction between binding obligations and material impossibility by deferring adjudication. Communities benefit from a status that preserves obligation without creating permanent guilt or requiring Talmudic reinterpretation every generation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, jewish_diaspora_communities, beneficiary,
    organized, civilizational, constrained, global).

% The abstract commitment structure that grounds authority: a future restoration event that will adjudicate what happens to suspended obligations. Not an agent that collects or pays, but the temporal reference point that makes suspension coherent.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_anticipation_framework, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__messianic_suspension, messianic_anticipation_framework).

% Other readings of the obligation (study-as-occupation, study-as-archiving) that would reframe the obligation's status. They are structurally excluded from authority within this reading's framework — not suppressed, but not given standing to adjudicate the suspension.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, alternative_obligation_readings, excluded,
    powerful, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the logical contradiction between a standing commandment and material impossibility (destroyed Temple) by establishing a third category — suspension — that honors both the commandment's binding force and the reality of absence. Practitioners coordinate around a shared understanding that neither violates nor fulfills the obligation.
% TRANSFER_FUNCTION: Moves time: the obligation is deferred from present adjudication to a future restoration event. No transfer of goods or labor occurs; the transfer is of jurisdiction from the present interpreting community to the future messianic authority.
% ABSENT_VOICES: Other reading communities (study-as-occupation, study-as-archiving) that interpret the same kernel differently are not invited into this framework's adjudication. Alternative scholarly approaches that derive different conclusions from identical texts are structurally absent from halakhic authority here.
% DISAPPEARANCE_RATIONALE: If the suspension framework vanished, practitioners would still study the same texts and live in the same material absence of Temple. What would disappear is the status distinction — the obligation would need to be reclassified as abrogated, fulfilled-by-substitute, or eternally violated. The diaspora condition persists; the doctrinal frame organizes response to it.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the sacrificial commandments remained textually binding but materially impossible. How can a commandment bind when its performance is impossible? How can the obligation's binding force be honored without creating perpetual violation-status?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic literature (Talmud, commentaries) from the Second Temple period onward attests this problem. Philosophers outside the halakhic community (Maimonides, medieval commentators, modern scholars) corroborate that the problem is structural and enduring — the impossibility has not been resolved by historical change, only deferred by the suspension doctrine.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because the suspension arrangement collects nothing from practitioners and imposes no asymmetric cost. Study is beneficial to practitioners (they maintain a coherent status) and carries minimal burden beyond the study time itself. Suppression is low (0.12) because the framework is maintained by intellectual assent rather than external coercion — practitioners choose to study within it; alternatives are not violently suppressed but are simply not given standing in this reading's authority. Theater is moderate-rising (0.35→0.45 over the interval) because the suspension doctrine's performative function increases as centuries pass: the longer the deferral, the more the framework's work becomes maintaining the deferral itself rather than addressing the founding problem. By year 2000, sustained study of an obligation that will not be performed until restoration is primarily a theatrical maintenance of status rather than functional engagement with an active commandment. Accessibility collapse is very high (0.92) because once the suspension reading is internalized, alternatives are nearly unthinkable within its framework — practitioners cannot imagine the obligation as live and current, nor as abrogated, without accepting the suspension's conclusion.
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic authority perspective, the suspension is a coherent logical solution and a feature of the system's adaptability — the framework honored the binding force of the commandment while accommodating material impossibility. From practitioners' perspectives who accept the framework, it is a coherent status that preserves obligation-status without guilt. From alternative readings (study-as-occupation), this framework forecloses the possibility that study itself constitutes performance and relocates the obligation to a future event. From external secular observers, the entire framework is deferred adjudication that depends on an event (restoration) outside the causal order.
 *
 * DIRECTIONALITY LOGIC:
 *   Torah_study_practitioners benefit from the coherent status without paying a cost beyond study time — they are near the beneficiary end (d near 0.2). Halakhic_authority_structure is the agenda-setter but does not extract rents; it maintains the framework and its authority depends on practitioners' assent — d near 0.4 (moderate toward coordinator). Jewish_diaspora_communities benefit from a status that preserves obligation-bound identity without requiring violation — they are beneficiaries but also constrained by material impossibility (d near 0.35). No agent is trapped as a victim; the lowest-mobility agents have legitimate exit via alternative readings, though within this reading's framework that exit is framed as rejecting authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to honor an impossible commandment) remains live across the 2000-year interval. The suspension solution has not rendered the problem obsolete — it has institutionalized deferral. The measured theater_ratio rise (0.35→0.45) captures this: as centuries pass without restoration, the framework's primary function shifts from answering the founding problem to maintaining the deferral status. A piton trajectory would show near-total theater (>0.80); the measured trajectory shows functional engagement persisting even as deferral-maintenance becomes more prominent. The mandate has not atrophied; the deferral has institutionalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_event_coherence,
    'What exactly happens when/if the Temple is restored? Does the suspended obligation immediately become current and require performance? Or does the messianic authority adjudicate it differently than pre-destruction obligation?',
    'Authoritative pronouncement by messianic authority at restoration (inaccessible now); textual analysis of halakhic sources about what obligations bind in messianic times; philosophical analysis of whether time-suspension creates ontological change in the obligation.',
    'If restoration simply reactivates the obligation, suspension is coherent. If restoration permits reinterpretation or abrogation, suspension is a temporary deferral. If restoration transforms the obligation entirely, suspension is a holding pattern for a fundamentally different future state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_event_coherence, conceptual, 'Whether messianic restoration resolves the suspended obligation or transforms it.').

omega_variable(
    study_as_substitute_vs_archive,
    'Does study of sacrifice law constitute a form of obligation-engagement (substitute performance during absence) or merely knowledge preservation (archiving for future restoration)?',
    'Textual comparison of halakhic rulings across centuries; examination of whether study is prescribed as *necessary* (obligation-occupying) or *permitted* (optional engagement); analysis of how practitioners understand their own motivations.',
    'If study is substitute performance, the suspension reading coexists with study-as-occupation as different interpretations of what the same study accomplishes. If study is archiving, they diverge sharply on the obligation''s status. This impacts whether the constraint here is about suspension per se or about study''s role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_substitute_vs_archive, empirical, 'Whether study is part of the obligation''s engagement or separate from it.').

omega_variable(
    deferral_vs_abrogation_indistinguishability,
    'If messianic restoration never occurs, is indefinite suspension indistinguishable from implicit abrogation? At what point does a deferral become effectively permanent?',
    'Philosophical analysis of temporal logic and obligation; examination of whether 2000 years of non-restoration produces a shift in halakhic stance; observation of whether newer communities treat the obligation differently than classical sources.',
    'If deferral and abrogation become indistinguishable, the suspension reading''s coherence depends on the realistic possibility of restoration. If communities eventually treat suspension as de facto abrogation, the constraint''s classification shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_vs_abrogation_indistinguishability, conceptual, 'Whether indefinite deferral and abrogation are structurally equivalent.').

omega_variable(
    reading_authority_conflict,
    'Which reading of the obligation holds legitimate authority: messianic_suspension, study-as-occupation, or study-as-archiving? Can all three coexist, or does adoption of one foreclose the others?',
    'Halakhic authority pronouncements on whether practitioners may adopt alternative readings; community practice showing which reading dominates; institutional enforcement (or lack thereof) against alternative interpretations.',
    'If readings coexist, this constraint describes only one framework among legitimate options. If one reading holds authority and others are suppressed, the constraint''s suppression metric may be understated (excluded alternatives suggest low suppression; enforced uniformity suggests higher suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_authority_conflict, empirical, 'Whether this reading holds exclusive authority or coexists with alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.35).
narrative_ontology:measurement(temp_tr_t200, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 200, 0.38).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.42).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.45).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.45).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2000, 0.45).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(temp_be_t200, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 200, 0.09).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.07).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2000, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(temp_su_t200, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 200, 0.1).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 500, 0.11).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1000, 0.12).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1500, 0.12).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__messianic_suspension, 0.06).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).

% DUAL FORMULATION NOTE:
% The temple_sacrifice_obligation kernel decomposes into three readings with distinct ε and structural properties. This story instantiates messianic_suspension (very low extractiveness, framework-coherence, deferral logic). The study-as-archiving reading treats study as knowledge preservation with different claim/metric profile. The study-as-occupation reading treats study as obligation-fulfillment with higher interaction with the founding problem. Each reading is a separate constraint with its own ε, beneficiary/victim structure, and classification. They are linked by kernel_id to enable comparison of how different readings of the same textual commitment produce different structural patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
