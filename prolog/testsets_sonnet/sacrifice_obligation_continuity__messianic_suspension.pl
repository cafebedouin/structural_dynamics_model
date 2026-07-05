% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Suspended Sacrifice Obligation Pending Messianic Restoration
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   Following the destruction of the Temple, the obligation to perform animal
 *   sacrifice could no longer be physically enacted. This reading of the
 *   kernel holds that the obligation is neither fulfilled nor violated but
 *   suspended pending messianic restoration of the Temple and priesthood — a
 *   legal and theological category distinct from claiming the law is dead
 *   (archival_preservation), distinct from claiming study itself constitutes
 *   fulfillment (study_as_performance), and distinct from insisting only
 *   future physical performance can satisfy the command while study is mere
 *   preparation (performance_only). Under messianic_suspension, study
 *   functions as a readiness-maintenance protocol: it keeps legal,
 *   liturgical, and communal knowledge intact so that performance can resume
 *   immediately upon restoration, but study does not itself discharge the
 *   commandment. There is no active victim set because no one is currently
 *   required to perform sacrifice and no one is currently in violation — the
 *   extraction, where it exists, is the diffuse, moderate cost of perpetual
 *   readiness-maintenance (calendar observance, study cycles, communal
 *   liturgy) borne without the psychological weight of guilt that the
 *   performance_only reading would impose.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.38).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.42).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.38).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Suspended Sacrifice Obligation Pending Messianic Restoration").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, 'f8462739-f777-4660-a17b-b04ee7c68a2b').
narrative_ontology:cs_kernel_codification('f8462739-f777-4660-a17b-b04ee7c68a2b', fixed_text).
narrative_ontology:cs_authority_grounding('f8462739-f777-4660-a17b-b04ee7c68a2b', lineage).
narrative_ontology:cs_interpretation_layer_present('f8462739-f777-4660-a17b-b04ee7c68a2b').
narrative_ontology:cs_reading_relation('f8462739-f777-4660-a17b-b04ee7c68a2b', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('f8462739-f777-4660-a17b-b04ee7c68a2b', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('f8462739-f777-4660-a17b-b04ee7c68a2b', sacrifice_obligation_continuity__performance_only, influences).
narrative_ontology:cs_axiom('f8462739-f777-4660-a17b-b04ee7c68a2b', foundational, obligation_persists_without_present_dischargeability).
narrative_ontology:cs_axiom_status(obligation_persists_without_present_dischargeability, holdable).
narrative_ontology:cs_axiom_grounding('f8462739-f777-4660-a17b-b04ee7c68a2b', obligation_persists_without_present_dischargeability, deontological).
narrative_ontology:cs_axiom('f8462739-f777-4660-a17b-b04ee7c68a2b', foundational, study_maintains_readiness_without_constituting_fulfillment).
narrative_ontology:cs_axiom_status(study_maintains_readiness_without_constituting_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('f8462739-f777-4660-a17b-b04ee7c68a2b', study_maintains_readiness_without_constituting_fulfillment, conventional).
narrative_ontology:cs_reference_frame('f8462739-f777-4660-a17b-b04ee7c68a2b', temple_era_sacrificial_normativity).
narrative_ontology:cs_drift_state('f8462739-f777-4660-a17b-b04ee7c68a2b', post_destruction_rabbinic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f8462739-f777-4660-a17b-b04ee7c68a2b', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_class).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, communal_religious_continuity).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, future_messianic_polity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, observant_laity).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, observant_laity).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, divine_covenant_permanence).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, temple_restoration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal category of suspension: decides which laws remain theoretically binding, how study substitutes for performance, and what readiness requires. Its institutional authority and vocation are constituted by being the custodians of a law that cannot currently be enacted but must be perpetually studied and transmitted.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_class, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_class, beneficiary).

% Carries the readiness burden through communal liturgy, study cycles, and calendar observances (e.g. recitation of sacrificial portions) that commemorate an obligation they cannot fulfill and are not permitted to abandon. They bear no guilt for non-performance but must sustain the practice of anticipation indefinitely, at real cost of time and communal resources.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, observant_laity, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, observant_laity, beneficiary).

% A not-yet-existing restored polity for whose sake the present readiness is maintained. It is not an actor that can be interviewed or held accountable; it functions as the deferred beneficiary that legitimates present-day study and suspension without ever being tested against present conditions.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, future_messianic_polity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__messianic_suspension, future_messianic_polity).

% Argue from outside the tradition (or from reformist wings within it) that the suspension category is a legal fiction sustaining an institution's relevance rather than a genuine metaphysical waiting. They are structurally excluded from the halakhic conversation that adjudicates the category's validity — their objections circulate in academic and denominational literature but do not bind the agenda_setter's determinations.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, reform_and_secular_critics, excluded,
    moderate, biographical, mobile, global).

% Study the suspension doctrine as a case of institutionalized deferral — comparing it to other traditions' handling of obsolete-but-sacred obligations. They can describe the structure but hold no authority to resolve whether the suspension is genuine or performative.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_class).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves legal, liturgical, and communal continuity across an interruption (loss of the Temple) that would otherwise force a binary choice between declaring the law dead or declaring the community perpetually in violation. Suspension lets the community coordinate around a shared future-oriented identity without present enactment or present guilt.
% TRANSFER_FUNCTION: Moves scholarly and communal attention, calendar time, liturgical labor, and institutional legitimacy toward the rabbinic class that administers the category, in exchange for a stable, guilt-free identity narrative for observant laity. No material sacrifice occurs; what is transferred is interpretive authority and the ongoing cost of readiness-maintenance.
% ABSENT_VOICES: Secular and reform critics who view the suspension as an indefinitely extendable legal fiction are not parties to the halakhic determination; archaeologists and historians who might independently assess 'imminence' of restoration have no standing in the doctrinal process either.
% DISAPPEARANCE_RATIONALE: If the suspension category vanished, communities would have to either declare the obligation permanently void (collapsing into the archival_preservation reading) or declare it currently binding and violated (an untenable crisis of communal legitimacy). The entire apparatus of substitutionary study, liturgical commemoration, and scholarly authority organized around 'pending restoration' would need to reorganize around one of the other kernel readings.
% FOUNDING_PROBLEM: The Temple's destruction removed the physical site and priestly apparatus required for sacrificial performance, creating an acute crisis: how can a law commanded as eternal continue to bind a community with no means to perform it, without either abandoning the law or declaring the community in permanent transgression?
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion and comparative legal scholars (outside the rabbinic beneficiary class) corroborate that the destruction-of-Temple crisis was real and that suspension-style doctrines are a recurring structural response across traditions facing obsolete cultic obligations; they diverge from the rabbinic class in treating the 'pending restoration' as an open-ended legitimating fiction rather than a live metaphysical expectation, since no external corroboration of imminent restoration is possible in principle.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38) reflecting the real but non-coercive cost of sustained study and liturgical commemoration across generations — a readiness tax, not a guilt tax. Suppression (0.42) reflects that the category itself is not optional within observant communities: declaring the obligation simply void (archival reading) or already fulfilled through study alone (study_as_performance reading) are both foreclosed moves within traditional halakhic discourse, which constrains communal discourse toward the suspension framing. Theater ratio is moderate (0.3) and rising over the interval, reflecting the increasing symbolic elaboration of commemorative practices (extended liturgical passages, ritual reenactments in text) as centuries pass without restoration, without any change in the underlying legal claim. Accessibility collapse is moderately high (0.6): once the suspension category is accepted, deviating from either the archival or study-as-performance readings becomes doctrinally difficult, though not as totalizing as a mountain claim. Resistance is lower (0.35) because most affected communities affirmatively hold the suspension frame rather than resisting it under coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic scholarly class sits closest to the beneficiary end: it administers the suspension category, adjudicates what readiness requires, and derives ongoing institutional authority and vocational purpose from being custodians of a law held in abeyance. Observant laity sit near symmetric — they gain a stable, guilt-free identity narrative and communal continuity (a genuine coordination benefit) but bear the diffuse cost of perpetual readiness practice. The future_messianic_polity is declared as a non-agent beneficiary: it cannot be interviewed, audited, or held to account, and functions structurally as a deferred legitimating referent rather than a real party bearing costs or receiving benefits in the present.
 *
 * MANDATROPHY ANALYSIS:
 *   The suspension reading is precisely the structure that prevents this constraint from collapsing into either pure obsolescence-denial (declaring an unperformable law still literally binding, which would manufacture perpetual violators) or pure abandonment (declaring the law dead, which the archival_preservation reading does). By naming the obligation 'suspended,' the tradition avoids mandatrophy in both directions: it does not extract guilt from a community that structurally cannot comply (as performance_only risks), and it does not quietly retire the obligation's normative force while continuing to extract study-labor under false pretenses (which would be the failure mode if the suspension were declared but the founding problem were treated as permanently, rather than contingently, unresolved).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_versus_indefinite_deferral,
    'Is ''pending messianic restoration'' a genuine metaphysical waiting period with real (if unknown) terminal conditions, or is it functionally indistinguishable from indefinite deferral that has operated as a legitimating fiction for nearly two millennia?',
    'No empirical resolution mechanism exists in principle, since messianic restoration is not independently verifiable ex ante; the closest available evidence is comparative-historical analysis of how the doctrine''s content and enforcement have shifted in response to political conditions (e.g., periods of heightened messianic expectation correlating with doctrinal emphasis shifts) rather than in response to any external criterion of imminence.',
    'If functionally indistinguishable from indefinite deferral, the suspension reading''s low extraction score becomes harder to sustain, since a category with no possible falsification condition and real ongoing costs starts to resemble the performance_only reading''s or a tangled_rope''s burden-without-exit structure rather than a genuine scaffold with an eventual sunset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_versus_indefinite_deferral, conceptual, 'Whether messianic suspension is a bounded scaffold or an unfalsifiable, effectively permanent category.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why do particular communities and eras favor the messianic_suspension reading over the study_as_performance or archival_preservation readings — is reading selection driven by doctrinal argument, or by which reading best preserves the authority of the currently dominant interpretive institution?',
    'Historical-sociological tracing of which denominational and institutional bodies champion which reading, cross-referenced with those bodies'' independent interests in preserving scholarly authority versus lay autonomy versus cultural-memory framing.',
    'If reading-selection tracks institutional interest more than doctrinal argument, the messianic_suspension reading''s coordination function (avoiding both guilt and abandonment) is partly cover for the rabbinic class''s interest in remaining indispensable custodians of an ever-pending law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the choice among sibling kernel readings tracks doctrine or institutional self-interest.').

omega_variable(
    future_polity_beneficiary_status,
    'Can a non-existent future polity meaningfully function as a ''beneficiary'' in the directionality calculus, or does declaring it as such simply launder present-day institutional benefit through a beneficiary that can never object, audit, or confirm receipt?',
    'Conceptual analysis of analogous cases (e.g., trusts for unborn beneficiaries, environmental obligations to future generations) to determine whether temporally deferred, non-existent beneficiaries can be coherently modeled as bearing directionality weight distinct from present administering parties.',
    'If the future polity cannot coherently bear beneficiary status, its declared benefit collapses into additional benefit accruing to the rabbinic scholarly class and observant laity in the present, concentrating rather than diffusing the beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_polity_beneficiary_status, conceptual, 'Whether a non-existent future beneficiary is a coherent structural category or a laundering device.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t0, projected).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 400, 0.14).
narrative_ontology:measurement_basis(sacr_tr_t400, projected).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 800, 0.18).
narrative_ontology:measurement_basis(sacr_tr_t800, projected).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1200, 0.22).
narrative_ontology:measurement_basis(sacr_tr_t1200, projected).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1600, 0.26).
narrative_ontology:measurement_basis(sacr_tr_t1600, projected).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1950, 0.3).
narrative_ontology:measurement_basis(sacr_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(sacr_be_t0, projected).
narrative_ontology:measurement(sacr_be_t400, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 400, 0.25).
narrative_ontology:measurement_basis(sacr_be_t400, projected).
narrative_ontology:measurement(sacr_be_t800, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 800, 0.3).
narrative_ontology:measurement_basis(sacr_be_t800, projected).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1200, 0.33).
narrative_ontology:measurement_basis(sacr_be_t1200, projected).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1600, 0.36).
narrative_ontology:measurement_basis(sacr_be_t1600, projected).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement_basis(sacr_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(sacr_su_t0, projected).
narrative_ontology:measurement(sacr_su_t400, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 400, 0.37).
narrative_ontology:measurement_basis(sacr_su_t400, projected).
narrative_ontology:measurement(sacr_su_t800, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 800, 0.38).
narrative_ontology:measurement_basis(sacr_su_t800, projected).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1200, 0.4).
narrative_ontology:measurement_basis(sacr_su_t1200, projected).
narrative_ontology:measurement(sacr_su_t1600, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1600, 0.41).
narrative_ontology:measurement_basis(sacr_su_t1600, projected).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement_basis(sacr_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.1).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the sacrifice_obligation_continuity kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. messianic_suspension occupies the structural middle: it neither dissolves the obligation's normative force (unlike archival_preservation) nor claims present textual fulfillment (unlike study_as_performance) nor insists guilt attaches to non-performance (unlike performance_only). All four share the same underlying kernel text and history but differ in beneficiary/victim structure, extractiveness, and classification. Network edges link all four so contamination propagation analysis can trace how a shift in one reading's institutional dominance would pressure the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
