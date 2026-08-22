% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Messianic Suspension of Sacrifice Obligation with Study Maintenance Protocol
 *   domain: religious/textual/legal
 *
 * SUMMARY:
 *   Following the destruction of the Jerusalem Temple in 70 CE, the Jewish
 *   interpretive tradition faced a foundational problem: the Torah commands
 *   sacrifice as an obligation, but the physical and institutional means to
 *   perform sacrifice were destroyed and remained impossible for nearly two
 *   millennia. The messianic-suspension reading resolves this by declaring
 *   the obligation suspended (not violated, not abolished) pending messianic
 *   restoration. Study of the sacrifice laws becomes the mechanism by which
 *   the community maintains readiness—textual expertise replaces ritual
 *   performance as the operative requirement. This reading is one of four
 *   competing interpretations of the same underlying kernel (the binding
 *   status of sacrifice law): study-as-performance argues study IS
 *   fulfillment; performance-only insists literal sacrifice must be restored;
 *   archival-preservation treats the law as historically obsolete;
 *   messianic-suspension frames it as conditionally binding but currently
 *   suspended. The constraint story here instantiates ONLY the
 *   messianic-suspension reading, with all structural data reflecting that
 *   framing.
 *
 * KEY AGENTS:
 *   - observant_community: Maintains the suspension framework and study infrastructure; benefits from the interpretive coherence it provides (neither violation nor impossible demand)
 *   - textual_scholars: Occupy professional and intellectual positions within the reading; their expertise is valued precisely because study is obligatory readiness work
 *   - alternative_reading_adherents: Excluded from this reading's authority; would contest either that suspension is coherent or that study qualifies as readiness maintenance
 *   - continuity_observer: Analytical seat tracking whether the suspended-but-binding reading remains intelligible across generations without collapsing into pure theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.48).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.22).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.48).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Messianic Suspension of Sacrifice Obligation with Study Maintenance Protocol").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious/textual/legal").

narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, 'e8d44ffa-36db-43fd-afba-3ccbc2da01a3').
narrative_ontology:cs_kernel_codification('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', fixed_text).
narrative_ontology:cs_authority_grounding('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', lineage).
narrative_ontology:cs_interpretation_layer_present('e8d44ffa-36db-43fd-afba-3ccbc2da01a3').
narrative_ontology:cs_reading_relation('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', sacrifice_obligation_continuity__study_as_performance, influences).
narrative_ontology:cs_reading_relation('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_axiom('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', foundational, obligation_suspended_not_violated).
narrative_ontology:cs_axiom_status(obligation_suspended_not_violated, holdable).
narrative_ontology:cs_axiom_grounding('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', obligation_suspended_not_violated, conventional).
narrative_ontology:cs_axiom('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', foundational, study_maintains_readiness_for_restoration).
narrative_ontology:cs_axiom_status(study_maintains_readiness_for_restoration, holdable).
narrative_ontology:cs_axiom_grounding('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', study_maintains_readiness_for_restoration, deontological).
narrative_ontology:cs_reference_frame('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', suspended_obligation_with_readiness_maintenance).
narrative_ontology:cs_drift_state('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', contemporary_indefinite_deferral, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e8d44ffa-36db-43fd-afba-3ccbc2da01a3', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, observant_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, textual_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the interpretive framework that suspends the active obligation to perform sacrifice while preserving study as a readiness mechanism. They set and transmit the reading, determine what counts as proper study, and frame the suspension as temporary rather than permanent cancellation. They bear the burden of maintaining textual expertise and ritual readiness infrastructure without performing the sacrificial act itself.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, observant_community, agenda_setter,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, observant_community, beneficiary).

% Occupy professional and intellectual positions maintained by the framework's requirement of sustained study of sacrifice law. Their expertise is valued precisely because the obligation is suspended but readiness must be maintained. The constraint creates demand for their interpretive labor and textual authority.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, textual_scholars, beneficiary,
    moderate, biographical, mobile, regional).

% Communities and scholars who hold competing readings of the sacrifice obligation (study-as-performance, performance-only, or archival-preservation frameworks). They are excluded from the messianic-suspension reading's authority structure; their objection would be that the suspension reading either too narrowly constrains study (if they hold study-as-performance) or falsely maintains an obligation already obsolete (if they hold archival-preservation).
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, alternative_reading_adherents, excluded,
    organized, civilizational, constrained, regional).

% The analytical seat that measures whether the suspension framework holds coherence across interpretive generations, whether the readiness infrastructure remains functional, and whether the conditional obligation (suspended pending restoration) remains intelligible as a live rather than performative commitment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, continuity_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__messianic_suspension, observant_community).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes the continuity of a religious law framework across a historical interval where its primary performance mode is impossible or suspended. Creates a coherent position between permanent abolition and active obligation: the law is neither dead nor currently binding, but preserved in readiness for a future restoration that the reading declares inevitable. Coordinates textual, institutional, and interpretive resources around the maintenance of this suspended state.
% TRANSFER_FUNCTION: Moves the interpretive and institutional burden of maintaining sacrifice-law expertise from the performance domain (ritual actors performing sacrifices) to the study domain (scholars and communities sustaining textual knowledge and readiness infrastructure). The obligation is transferred from physical to intellectual maintenance, without guilt or violation for non-performance.
% ABSENT_VOICES: Adherents of competing readings—study-as-performance scholars who argue study IS the fulfillment; performance-only communities who insist on literal restoration or permanent cancellation; archival-preservation advocates who deny any normative force to the suspension. These readers would contest whether suspension is coherent, whether study truly maintains the obligation in its suspended form, or whether the entire framework is theatrical cover for functional abolition.
% DISAPPEARANCE_RATIONALE: If the messianic-suspension reading vanished, the constraint on maintaining readiness would dissolve—textual scholarship would be devalued as merely historical rather than obligatory preparation; institutional resources for expertise would redirect; the interpretive coherence that allows the community to hold the obligation as simultaneously suspended (not violated) and binding (not abolished) would collapse, forcing a choice between archival study or performance-restoration or explicit abolition.
% FOUNDING_PROBLEM: Following the destruction of the sacrificial temple in 70 CE, the primary obligation to perform sacrifice became impossible to fulfill while the interpretive texts and legal framework remained authoritative. The founding problem: how can a religious community maintain fidelity to a binding law that cannot be performed, without declaring the law dead, violated, or no longer binding?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic and scholarly consensus across medieval and modern Jewish tradition attests the founding problem remains live: the obligation to sacrifice is presented as suspended, not abolished, pending messianic restoration. This is corroborated by independent historical analysis of how temple-dependent legal systems adapted after temple destruction. Competing readings (study-as-performance, archival-preservation) contest whether the suspension is live or performative, but all parties acknowledge the founding crisis: a binding law rendered unperformable.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the framework creates a sustained burden without guilt: the community must maintain readiness infrastructure and textual expertise without performing the primary obligation. This is extractive relative to archival-preservation (which would eliminate the burden entirely) but less extractive than performance-only (which would demand impossible action) or study-as-performance (which would redefine study as fulfillment, eliminating readiness burden). Suppression is low (0.22) because the framework is internally coherent and accepted by the observant community—the suspension is not imposed externally but adopted as a solution to an impossible problem. Theater ratio is high (0.61) because the constraint's function has drifted from performance-readiness toward interpretive maintenance: the burden is now primarily textual and institutional rather than oriented toward actual restoration. The measurement series shows extractiveness rising slightly then stabilizing (temple destruction created the need; centuries of Talmudic development refined the framework; by the medieval period, the reading had settled into stable theater-ratio of maintenance), indicating the constraint matured into a steady-state interpretive practice. The shared time grid runs from 70 CE (temple destruction, founding event) to 2000 CE (contemporary reading position), with five measurement points ensuring every metric is authored at every examined moment.
 *
 * PERSPECTIVAL GAP:
 *   The observant community that holds the messianic-suspension reading experiences the constraint as coherent: it preserves the obligation while avoiding violation, maintains readiness without guilt, and frames study as meaningful work toward a future restoration. Alternative readers experience a structural gap: study-as-performance adherents see the suspension as unnecessary (study already fulfills); archival-preservation scholars see it as incoherent (the obligation is dead, not suspended). The engine should compute this as a low-suppression constraint (the reading is accepted by its own community) but with high accessibility_collapse for outsiders (once you accept the suspension reading, alternatives become less accessible because they either deny the binding nature of the law or demand impossible performance). The directionality is subtle: the observant community is both agenda_setter (they maintain the reading) and beneficiary (they benefit from the coherence it provides), while alternative readers are excluded, not victimized—the constraint does not extract from them, but it does foreclose their reading within this framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The observant community holds near-zero directionality (d ~0.2) as beneficiary: they set the agenda, they accepted the suspension voluntarily, and they bear the burden of maintaining readiness as their chosen solution to an impossible problem. This is not extraction but rather a coordinated response to a crisis. The extractiveness score (0.48) reflects not directionality-driven targeting but rather the inherent burden of maintaining readiness without performance: it is extractive relative to the lighter burden of archival study, but less extractive than the impossible demand of actual performance. Alternative readers are excluded rather than targeted: they are not victims of the suspension reading, but they would object to it. Textual scholars benefit (d ~0.1) because their expertise is now obligatory work, creating professional opportunity. The constraint has no clear victim set in the present moment (no one is currently forced to perform an impossible sacrifice), which distinguishes it from snare and tangled-rope configurations—the victims would only emerge if the suspension were lifted (forcing present-day performance) or breached (revealing violation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to maintain fidelity to an unperformable obligation—remains live, not dead. The messianic-suspension reading solves it by reframing the obligation as suspended rather than violated. However, theater_ratio rising from 0.45 to 0.61 indicates the constraint's function has drifted significantly: early centuries focused on readiness maintenance and interpretive debate about restoration; by the medieval and modern periods, the readiness function has become increasingly performative—study is valued for its own sake, restoration seems ever-deferred, and the constraint increasingly functions to maintain interpretive tradition rather than active readiness. This is a classic mandatrophy trajectory: the original function (maintain readiness for restoration) has been displaced by a secondary function (maintain textual tradition). The reading does not explicitly resolve this drift—it would require either openly admitting the restoration is indefinitely deferred (moving toward archival-preservation) or redefining study-as-performance. The constraint thus carries a latent mandatrophy: it claims to maintain readiness for a messianic future that grows ever more distant, while performing increasingly as an elaborate system for scholarly engagement with historical law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_functional_abolition,
    'Is the messianic suspension a genuine conditional obligation (suspended, not violated, awaiting restoration) or has it functionally become permanent abolition dressed in the language of deferral?',
    'Comparative historical analysis: if present-day interpretive communities actively maintain readiness infrastructure and treat restoration as live (not merely rhetorical), the suspension reading holds; if readiness is performative theater and restoration is treated as indefinitely deferred, the constraint has drifted toward archival-preservation.',
    'If the suspension is genuine, the constraint is a scaffold (temporary, pending restoration) with moderate extractiveness (readiness burden); if it has become functional abolition, the reading collapses into archival-preservation, extractiveness drops, and theater_ratio becomes the dominant signal of the divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_functional_abolition, empirical, 'Whether suspension remains a live conditional or has drifted into indefinite deferral.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Which of the four sibling readings best captures the actual normative force of the constraint in the community? Is the community genuinely adopting messianic-suspension, or has it migrated toward study-as-performance without formally renaming the reading?',
    'Ethnographic and textual analysis: examine how contemporary scholars and communities speak about the obligation (as suspended pending restoration, as fulfilled through study, as historically obsolete, as preserved in memory). Track which reading is cited in normative discourse and which operates implicitly.',
    'If the community has effectively adopted study-as-performance while maintaining messianic-suspension language, the constraint''s actual classification is different from its declared reading. If it has moved toward archival-preservation, the extractiveness and theater metrics would reflect that shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether the declared messianic-suspension reading matches the community''s actual operative framing.').

omega_variable(
    restoration_timeline_indexing,
    'Does the messianic-suspension reading remain temporally neutral (restoration possible at any moment, indefinitely, with equal readiness burden), or has it become time-indexed to increasingly distant expectations?',
    'Diachronic analysis: track medieval, early modern, and contemporary rabbinic and scholarly discourse on the expected timeline of restoration. If timeline expectations lengthen or become explicitly indefinite, the constraint''s extractiveness should increase (readiness burden grows as restoration recedes).',
    'If the timeline becomes indefinitely extended, the scaffold''s sunset clause becomes inoperative (the condition—messianic restoration—moves beyond practical temporal horizon), and the constraint drifts toward piton (maintained by institutional inertia rather than active readiness function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(restoration_timeline_indexing, empirical, 'How the temporal horizon of messianic restoration affects the constraint''s operative extractiveness.').

omega_variable(
    alternative_reading_coexistence_stability,
    'Can four distinct readings of the sacrifice obligation kernel coexist indefinitely within the same interpretive tradition without one eventually dominating or foreclosing the others?',
    'Long-term institutional analysis: observe whether the four readings (messianic-suspension, study-as-performance, performance-only, archival-preservation) remain live or whether one reading consolidates institutional authority, forcing others toward marginal status.',
    'If messianic-suspension loses interpretive authority to study-as-performance or archival-preservation, this constraint''s classification would become subordinate or obsolete. If one reading forecloses another (rare), the kernel structure would require reconsideration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_coexistence_stability, conceptual, 'Whether the four sibling readings remain in stable coexistence or trend toward consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 70, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 70, 0.45).
narrative_ontology:measurement_basis(sacr_tr_t70, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 500, 0.52).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1000, 0.58).
narrative_ontology:measurement_basis(sacr_tr_t1000, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1500, 0.62).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2000, 0.61).
narrative_ontology:measurement_basis(sacr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 70, 0.35).
narrative_ontology:measurement_basis(sacr_be_t70, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 500, 0.42).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1000, 0.48).
narrative_ontology:measurement_basis(sacr_be_t1000, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1500, 0.5).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement_basis(sacr_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 70, 0.28).
narrative_ontology:measurement_basis(sacr_su_t70, observed).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 500, 0.24).
narrative_ontology:measurement_basis(sacr_su_t500, observed).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1000, 0.22).
narrative_ontology:measurement_basis(sacr_su_t1000, observed).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1500, 0.21).
narrative_ontology:measurement_basis(sacr_su_t1500, observed).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 2000, 0.22).
narrative_ontology:measurement_basis(sacr_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is part of the sacrifice_obligation_continuity kernel family comprising four competing readings of the binding status of sacrifice law post-70 CE. All four stories share the same kernel but instantiate different ε values and beneficiary structures. The messianic-suspension reading maintains a suspended (not violated, not abolished) obligation through study-based readiness maintenance. Study-as-performance redefines study as fulfillment. Performance-only insists on literal restoration. Archival-preservation treats the law as historically obsolete. These are not perspectives on a single constraint but four structurally distinct constraints generated from one kernel. Links via network.affects_constraints map the family dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
