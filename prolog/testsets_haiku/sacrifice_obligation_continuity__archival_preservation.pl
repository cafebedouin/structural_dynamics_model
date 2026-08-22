% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrifice Law as Historical-Textual Artifact (Archival Preservation Reading)
 *   domain: religious/legal/textual
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel: sacrifice law
 *   in post-Temple Judaism. The archival-preservation reading claims that
 *   sacrifice obligation terminated when the Temple was destroyed and literal
 *   performance became impossible. Study and textual transmission preserve
 *   the knowledge and cultural memory without asserting ongoing normative
 *   force. This reading produces a constraint with zero extractiveness — no
 *   parties benefit or suffer because no obligation is claimed. The reading
 *   is one among four: messianic-suspension (obligation is deferred, not
 *   terminated); performance-only (study prepares for future restoration);
 *   study-as-performance (textual engagement IS fulfillment). This story
 *   treats the archival-preservation reading as the referent and measures its
 *   structural properties from that reading's own framework.
 *
 * KEY AGENTS:
 *   - textual_scholars: preserve and transmit sacrifice law knowledge through study and commentary
 *   - religious_communities: engage with the tradition through study rather than performance
 *   - historical_witnesses: attest that the Temple's destruction is the foundational fact that prompted the reading shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.0).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, mountain).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Law as Historical-Textual Artifact (Archival Preservation Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious/legal/textual").

domain_priors:emerges_naturally(sacrifice_obligation_continuity__archival_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, 'f63715ed-1c53-429b-bb19-39eefc0d33dd').
narrative_ontology:cs_kernel_codification('f63715ed-1c53-429b-bb19-39eefc0d33dd', fixed_text).
narrative_ontology:cs_authority_grounding('f63715ed-1c53-429b-bb19-39eefc0d33dd', lineage).
narrative_ontology:cs_interpretation_layer_present('f63715ed-1c53-429b-bb19-39eefc0d33dd').
narrative_ontology:cs_reading_relation('f63715ed-1c53-429b-bb19-39eefc0d33dd', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('f63715ed-1c53-429b-bb19-39eefc0d33dd', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('f63715ed-1c53-429b-bb19-39eefc0d33dd', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('f63715ed-1c53-429b-bb19-39eefc0d33dd', foundational, temple_destruction_terminates_literal_obligation).
narrative_ontology:cs_axiom_status(temple_destruction_terminates_literal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f63715ed-1c53-429b-bb19-39eefc0d33dd', temple_destruction_terminates_literal_obligation, deontological).
narrative_ontology:cs_axiom('f63715ed-1c53-429b-bb19-39eefc0d33dd', foundational, textual_preservation_is_voluntary_practice).
narrative_ontology:cs_axiom_status(textual_preservation_is_voluntary_practice, holdable).
narrative_ontology:cs_axiom_grounding('f63715ed-1c53-429b-bb19-39eefc0d33dd', textual_preservation_is_voluntary_practice, conventional).
narrative_ontology:cs_reference_frame('f63715ed-1c53-429b-bb19-39eefc0d33dd', post_temple_discontinuation).
narrative_ontology:cs_drift_state('f63715ed-1c53-429b-bb19-39eefc0d33dd', contemporary_scholarly_consensus, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f63715ed-1c53-429b-bb19-39eefc0d33dd', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves textual and ritual knowledge across generations through study, commentary, and archival practice; maintains cultural memory of sacrifice law's historical function without claiming ongoing normative force.
% TRANSFER_FUNCTION: No extraction. Knowledge and cultural memory move through educational institutions and textual communities; no party collects rent or benefit from the constraint's operation.
% ABSENT_VOICES: This reading silences (or renders unnecessary) the voices of practitioners who believe sacrifice law remains binding in some form — whether through messianic suspension, study-as-performance, or future physical restoration. Those voices are excluded from the archival-preservation framing by the reading's core claim that obligation has terminated.
% DISAPPEARANCE_RATIONALE: If archival preservation ceased — study and textual transmission stopped — the world would rearrange only among the small community of scholars and religious practitioners engaged with the tradition. The general population experiences no constraint from sacrifice law; its archival preservation is a self-contained cultural practice. The world outside the textual community would be indifferent to its disappearance.
% FOUNDING_PROBLEM: After the destruction of the Jerusalem Temple, sacrifice law became impossible to perform in its literal scriptural form. The founding problem was: how to preserve the commandment's textual and conceptual heritage without claiming ongoing obligation to perform what cannot be performed.
% FOUNDING_PROBLEM_CORROBORATION: Jewish textual scholars and historians (outside the communities that maintain this reading) attest that the founding problem is the historical fact of the Temple's destruction. However, they disagree about its consequence: this reading's claim that obligation has terminated is contested by the other readings listed in the kernel context. The problem is live; the solution is contested.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_continuity__archival_preservation),
    narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.0 because the reading claims no normative obligation and no asymmetric distribution of benefits. No party collects rent from the constraint; no target bears extraction costs. Suppression is 0.0: there is no coercive machinery maintaining the constraint because there is no binding claim. Theater-ratio is 0.0: there is no performative or symbolic maintenance required because the constraint is not claimed to be operative. Accessibility-collapse is high (0.95): once the archival-preservation reading is understood, the alternatives (that obligation persists in some form) are recognized as live options chosen by other communities; the collapse reflects that the reading presents itself as factually inevitable (the Temple is gone, performance is impossible) but this factual inevitability does not eliminate the alternative readings' claims about meaning and obligation. Resistance is minimal (0.08): the reading meets little organized resistance because it is adopted by the scholarly consensus and does not assert a claim others must obey; those who believe obligation persists (the sibling readings' communities) maintain their own interpretations without direct conflict with archival preservation.
 *
 * PERSPECTIVAL GAP:
 *   All adopters of the archival-preservation reading sit in the same structural position: they accept that obligation has terminated and that study preserves memory without binding force. There is no perspectival gap within this reading because no seats have opposed interests. The gap exists between this reading and the sibling readings (messianic-suspension, performance-only, study-as-performance), but those are different constraints, not different seats in this constraint. Communities that adopt messianic-suspension or study-as-performance experience this constraint differently — they do not experience it at all, because they inhabit a different reading of the kernel. The engine computes per-seat types only when multiple seats exist within a single constraint; here, the constraint has at most one coherent seat (the reading's adopters) and therefore a single computed type.
 *
 * DIRECTIONALITY LOGIC:
 *   The absence of beneficiaries and victims is structural: the reading asserts that no normative force operates and therefore no asymmetric distribution of benefits or costs occurs. There are no stakeholders to position along the beneficiary-target axis because the constraint does not impose an obligation that some benefit from and others pay for. Directionality is undefined (no seats exist). The archival-preservation reading is self-contained: it describes a fact about the world (the Temple is destroyed, literal sacrifice is impossible) and draws a normative conclusion (obligation terminates) that applies equally to all members of the tradition who adopt this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to preserve textual and cultural memory of sacrifice law. The constraint persists because the textual tradition remains alive (scholars study, commentaries circulate, the law is taught). The mandate is intact and the mechanism matches the mandate: archival preservation accomplishes exactly what it was built to accomplish, with no layer of rent-extraction or coercive enforcement masking a dead function. There is no mandatrophy because the constraint was not founded to extract or compel, only to preserve. If the constraint did persist without functional mandate — if scholars studied sacrifice law only because institutional inertia required it, not because the community valued the knowledge — then mandatrophy would be present. The archival-preservation reading does not describe that situation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_archival_boundary,
    'Does termination of obligation entail the constraint is no longer a constraint at all, or is archival preservation itself a constraint (on how the knowledge is transmitted and preserved)?',
    'Structural analysis: if the claim is that no normative obligation exists, then the constraint is a fact about what is NOT binding (a purely negative claim). If the claim is that textual transmission is obligatory, then a normative constraint remains but shifts from performance to study.',
    'If archival preservation itself is obligatory (study is commanded), the constraint re-enters the normative domain and cannot claim zero extractiveness. If the claim is purely negative (no obligation to perform, optional to study), the constraint exits the normative space entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_archival_boundary, conceptual, 'Whether archival preservation is itself a binding obligation or merely a voluntary cultural practice.').

omega_variable(
    reading_vs_kernel_identity,
    'Is this reading describing a single coherent constraint (the post-Temple archival arrangement), or is it one reading of a kernel (the sacrifice law itself) under multiple contradictory interpretations?',
    'If this is a kernel reading: the sibling readings coexist with this one across different communities and traditions. If this is a standalone constraint: the other named readings are competing accounts, not coexisting frameworks. The epsilon-invariance test: if measuring the constraint (archival preservation as zero-obligation) produces a different type than measuring the kernel (sacrifice law as contested obligation), two constraints exist.',
    'If this is a reading of a kernel, the engine computes constraint identity from the kernel_id and reading_id; per-seat classification follows the reading chosen. If standalone, the constraint sits at the intersection of all readings and may not be coherently classified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_kernel_identity, conceptual, 'The constraint identity and kernel-reading status of this story relative to the kernel and its siblings.').

omega_variable(
    messianic_deferral_vs_termination,
    'Does the archival-preservation reading claim that obligation is permanently terminated, or that it is deferred pending messianic restoration?',
    'Textual and theological analysis: does the reading assert obligation will never resume (termination), or that it is in abeyance awaiting a future condition (deferral)? The reading''s own authoritative sources provide the answer.',
    'Termination => zero ongoing obligation => extractiveness remains 0. Deferral => obligation is temporarily suspended => extractiveness depends on whether study is preparation for future performance (obligation persists in readiness) or mere preservation (obligation is suspended). The reading''s characterization of its own reference point determines the epsilon value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_deferral_vs_termination, empirical, 'Whether this reading claims permanent termination or temporary deferral of sacrifice obligation.').

omega_variable(
    cultural_practice_vs_constraint,
    'Is archival preservation of textual tradition a constraint in the Deferential Realism sense (an arrangement that structures agent behavior, defines beneficiaries and victims), or is it a cultural practice that exists outside the constraint framework?',
    'If no agent is beneficiary or victim, no normative force operates, and the constraint vacates the space entirely. The test: does anyone benefit from or bear costs from the preservation arrangement? If yes, a constraint exists (and extractiveness would be non-zero if benefits are asymmetric). If no, it is a cultural practice without a constraint structure.',
    'If it is purely a cultural practice (no benefits/costs to specific agents), the constraint dissolves and the story should be reframed as descriptive cultural history, not as a Deferential Realism constraint. If preservation generates asymmetric benefits (scholars are beneficiaries, lay people bear costs of supporting institutions), the constraint re-enters with non-zero extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_practice_vs_constraint, conceptual, 'Whether archival preservation is a constraint structure or a cultural practice outside the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 70, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t70, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1200, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t1200, observed).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1800, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t1800, observed).
narrative_ontology:measurement(sacr_tr_t2026, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 2026, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 70, 0.0).
narrative_ontology:measurement_basis(sacr_be_t70, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 500, 0.0).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1200, 0.0).
narrative_ontology:measurement_basis(sacr_be_t1200, observed).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1800, 0.0).
narrative_ontology:measurement_basis(sacr_be_t1800, observed).
narrative_ontology:measurement(sacr_be_t2026, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 2026, 0.0).
narrative_ontology:measurement_basis(sacr_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__archival_preservation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, information_standard).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_continuity kernel decomposes into four constraint stories, each representing a different reading: archival_preservation (this story, zero extractiveness, cultural memory), messianic_suspension (obligation deferred, constraint persists), performance_only (study as preparation, obligation persists through future performance), and study_as_performance (textual engagement fulfills obligation, constraint persists through study). The four stories share a kernel (the standing question of sacrifice law's post-Temple status) but instantiate different epsilon values and constraint types. The network edges link all four as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
