% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Sacrifice Obligation Continuity — Archival Preservation Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'sacrifice_obligation_continuity.' The archival_preservation reading
 *   holds that sacrifice law is no longer binding; study preserves cultural
 *   memory and textual tradition without normative force. This reading
 *   emerged historically after the Temple's destruction (70 CE) as Jewish law
 *   developed multiple interpretive strategies to account for the
 *   impossibility of performance. The archival_preservation reading claims
 *   the obligation has exited the constraint space entirely — it is no longer
 *   a binding commandment but a documented historical practice. The
 *   constraint described here is the standing arrangement under this
 *   reading's lights: obligation is gone; study is cultural work, not
 *   normative obligation. Extractiveness is zero because the reading asserts
 *   no normative force, no binding claim on practitioners, no coercion.
 *   Accessibility collapse is high (0.95) because, given the reading that
 *   obligation has exited, alternatives (non-study, alternative
 *   interpretations, secular scholarship) are nearly completely available —
 *   there is no constraint binding the reader to engage with the tradition at
 *   all. Resistance is minimal (0.05) because the reading's adoption is a
 *   matter of scholarly and community consensus-building, not coercive
 *   enforcement.
 *
 * KEY AGENTS:
 *   - Textual scholars: pursue scholarly study as historical and interpretive inquiry; benefit from the reading that obligation has exited
 *   - Cultural historians: study sacrifice law as artifact and memory; benefit from ability to engage without normative pressure
 *   - Contemporary Jewish communities: navigate between multiple readings; must decide institutionally which reading governs their practice
 *   - Messianic believers: hold the reading that obligation is suspended, not terminated; excluded from archival_preservation's framing
 *   - Performance mandatists: hold the reading that obligation requires or will require physical performance; excluded when obligation is treated as exited
 *   - Study-as-performance proponents: hold the reading that textual study fulfills the obligation; excluded when study is treated as mere preservation
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
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, mountain).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Obligation Continuity — Archival Preservation Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious_law/ritual_studies/textual_tradition").

domain_priors:emerges_naturally(sacrifice_obligation_continuity__archival_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, 'ec0953fe-c969-4447-85d0-815d91326b38').
narrative_ontology:cs_kernel_codification('ec0953fe-c969-4447-85d0-815d91326b38', fixed_text).
narrative_ontology:cs_authority_grounding('ec0953fe-c969-4447-85d0-815d91326b38', lineage).
narrative_ontology:cs_interpretation_layer_present('ec0953fe-c969-4447-85d0-815d91326b38').
narrative_ontology:cs_reading_relation('ec0953fe-c969-4447-85d0-815d91326b38', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('ec0953fe-c969-4447-85d0-815d91326b38', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('ec0953fe-c969-4447-85d0-815d91326b38', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('ec0953fe-c969-4447-85d0-815d91326b38', foundational, obligation_has_exited_constraint_space).
narrative_ontology:cs_axiom_status(obligation_has_exited_constraint_space, holdable).
narrative_ontology:cs_axiom_grounding('ec0953fe-c969-4447-85d0-815d91326b38', obligation_has_exited_constraint_space, deontological).
narrative_ontology:cs_axiom('ec0953fe-c969-4447-85d0-815d91326b38', secondary, study_is_preservation_not_performance).
narrative_ontology:cs_axiom_status(study_is_preservation_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('ec0953fe-c969-4447-85d0-815d91326b38', study_is_preservation_not_performance, conventional).
narrative_ontology:cs_reference_frame('ec0953fe-c969-4447-85d0-815d91326b38', post_temple_destruction_obligation_status).
narrative_ontology:cs_drift_state('ec0953fe-c969-4447-85d0-815d91326b38', contemporary_scholarly_consensus, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec0953fe-c969-4447-85d0-815d91326b38', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, textual_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, cultural_historians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pursue scholarly study of sacrifice law and rabbinic interpretation as a legitimate academic practice. They benefit from the reading that obligation has exited the constraint space: this permits study as historical and textual inquiry without normative force or demand for restoration. Their scholarship documents the tradition, preserves interpretive lineages, and makes the material accessible to students and the broader intellectual community.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, textual_scholars, beneficiary,
    moderate, generational, mobile, global).

% Study sacrifice law as cultural artifact and historical practice. They benefit from the reading that treats study as cultural memory work, not normative obligation. This permits engagement with the material without the demand that the practice be restored or that the scholar take a position on whether the obligation remains binding.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, cultural_historians, beneficiary,
    moderate, generational, mobile, global).

% Navigate the status of sacrifice law within living Jewish law and practice. They hear multiple readings of the kernel and must decide, institutionally and communally, which reading governs their own interpretive frameworks and practices. This reading — archival preservation — is one option they consider, alongside messianic_suspension, performance_only, and study_as_performance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, contemporary_jewish_communities, observer,
    organized, generational, analytical, global).

% Hold the reading that sacrifice obligation is suspended pending messianic restoration, not terminated. They would object to the archival_preservation reading on grounds that it forecloses the possibility of future physical restoration and misrepresents the legal status of the obligation (suspended, not abolished). They are structurally excluded from the conversation when scholarship adopts archival_preservation as the binding reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, messianic_believers, excluded,
    moderate, civilizational, identity_locked, global).

% Hold the reading that sacrifice obligation requires or will require physical performance, not merely study. They would argue that archival_preservation misses the obligation's binding force and that study is preparation, not fulfillment. They are excluded when scholarship treats obligation as having entirely exited the constraint space.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, performance_mandatists, excluded,
    moderate, civilizational, identity_locked, global).

% Hold the reading that textual study itself constitutes performance of the sacrifice obligation, preserving the obligation within the study practice. They would dispute archival_preservation's claim that obligation has exited; they argue the obligation persists through scholarly engagement. They are excluded when scholarship treats study as mere preservation, not as binding normative practice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, study_as_performance_proponents, excluded,
    moderate, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__archival_preservation, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__archival_preservation, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the textual, interpretive, and historical record of sacrifice law and rabbinic response to the loss of the Temple. Study creates a unified, curated archive accessible to scholars, students, and cultural historians without requiring participants to adjudicate the binding force of the obligation or commit to restoration.
% TRANSFER_FUNCTION: Moves interpretive authority from the obligation itself (binding practice) to the archive and scholarly tradition. What was a normative commandment becomes a documented historical practice; the force of the tradition shifts from duty to memory.
% ABSENT_VOICES: Messianic believers and performance-obligation holders are structurally excluded: they would argue that obligation has not exited constraint space and that archival preservation misrepresents the legal and normative status. They are not consulted when scholarly consensus adopts this reading.
% DISAPPEARANCE_RATIONALE: If archival preservation study vanished, sacrifice law would still have exited normative force in Jewish practice; the archive is a *representation* of that exit, not its *cause*. The underlying fact — that Temple sacrifice is no longer performed and obligation has been suspended or terminated — persists regardless of whether scholars study it. The archive is cultural memory work, not the constitutive force maintaining the constraint.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), sacrifice became impossible to perform. Jewish law and theology had to account for the loss: Was the obligation still binding? Was it suspended? Did study replace performance? The tradition developed multiple interpretive responses, each staking a different claim about the legal and normative status of sacrifice law.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic texts (Mishnah, Talmud, responsa) from multiple centuries attest the founding problem: how to maintain the obligation's legal status after the Temple's destruction. Contemporary Jewish communities continue to engage the problem — different movements and scholars adopt different readings. The problem is live because the multiple readings remain held by different parties, and no single reading has achieved universal consensus among Jewish communities.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is zero because the archival_preservation reading asserts that obligation has exited the constraint space entirely. There is no normative claim, no binding force, no extraction from practitioners. Suppression is zero for the same reason: if obligation has exited, there is no enforcement machinery, no exclusion of alternatives, no coercive suppression. Theater is zero because there is no performance claim — study is presented straightforwardly as cultural preservation work, not as theatrical compliance with a hidden obligation. Accessibility collapse is very high (0.95) because, given that obligation has exited, practitioners have nearly complete freedom to engage or not engage with the tradition, to adopt alternative readings, or to pursue non-religious scholarship. The high accessibility reflects the reading's central claim: obligation is gone, so the constraint no longer binds. Resistance is low (0.05) because the reading is adopted through scholarly and community discourse, not through enforcement conflict. The measurement series is empty because the constraint's core metrics are static under this reading: once obligation exits, extractiveness and suppression remain at zero; theater remains at zero. A temporal series would only be meaningful if the reading tracked the process of obligation's historical exit (0–70 CE), but the constraint is authored as the standing arrangement after that exit.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap in the archival_preservation reading because extractiveness is zero. All parties experience the same constraint: obligation has exited; study is cultural preservation. The gap that exists is between archival_preservation and its sibling readings. From the messianic_suspension reading, archival_preservation misrepresents the legal status (suspension, not exit). From the performance_only reading, archival_preservation treats study as end-state rather than preparation. From the study_as_performance reading, archival_preservation denies the obligation's persistence through textual engagement. These gaps are between readings, not between seats within a single reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not computed under this reading because extractiveness is zero and there is no binding coercive force. The beneficiaries (textual scholars, cultural historians) are identified because they benefit from the reading that obligation has exited — their scholarly practice becomes legitimate intellectual work rather than normative duty. But beneficiary status in a zero-extractiveness reading does not entail that the reading is extractive. The declaration serves to flag the false-summit question (see omegas): if scholars are using the 'archival preservation' framing to de-obligate the tradition and create professional standing, the reading may be constructed rather than natural. The excluded parties (messianic believers, performance mandatists, study-as-performance proponents) are identified because they hold sibling readings that contradict archival_preservation and would be foreclosed or marginalized if this reading became canonical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (how to maintain the obligation after Temple destruction) remains live because multiple readings persist. No single reading has achieved consensus that would resolve the problem. The archival_preservation reading resolves mandatrophy by treating obligation as having exited the constraint space — the founding problem is solved by declaring it solved: obligation is no longer binding. But other communities hold readings where the problem persists (suspension reading: obligation persists in suspended form; performance reading: obligation awaits future restoration; study-as-performance reading: obligation persists through study). The constraint avoids declaring mandatrophy_resolved because the reading itself does not resolve the kernel's contest — it represents one option within an ongoing plurality of readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_sacrifice_obligation,
    'This constraint is one reading of the kernel ''sacrifice_obligation_continuity.'' What is the kernel itself — the standing commitment that different readings interpret differently?',
    'The kernel is the standing textual and legal claim in Jewish tradition that sacrifice law is a binding commandment. The archival_preservation reading interprets this standing claim as: the obligation has exited constraint space entirely; study is preservation, not performance. Sibling readings interpret it as: suspension pending messianic restoration (messianic_suspension); obligation to perform or prepare for future performance (performance_only); obligation fulfilled through study (study_as_performance). Each reading holds the same kernel and proposes a different structural relationship to it.',
    'If the kernel is correctly identified as a standing legal claim, the archival_preservation reading is viable: it asserts the obligation is no longer binding. If the kernel is identified differently — as an eternal principle, as a suspended duty, or as constitutively dependent on study — the reading would need to be reframed or rejected. The clarity of the kernel identity determines which readings are coherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_sacrifice_obligation, conceptual, 'Identity of the contested kernel that the readings interpret.').

omega_variable(
    false_summit_beneficiary_ambiguity,
    'Does declaring beneficiaries on a Mountain constraint that claims zero extractiveness create a false-summit signal? Textual scholars benefit from the reading that obligation has exited — is this a natural fact, or are beneficiaries using the ''archival preservation'' framing to collect professional standing?',
    'Distinguish between two scenarios: (1) Obligation has genuinely exited constraint space (natural fact); scholars benefit because they can study freely without normative pressure (incidental). (2) Scholars use the ''archival preservation'' framing to de-obligate the tradition and create professional intellectual space (constructed benefit). The distinction tracks whether the beneficiaries are describing the constraint (natural reading) or defending it (constructed reading).',
    'If scenario 1 is correct, the archival_preservation reading is genuinely a mountain (natural fact): obligation has been dissolved by historical circumstance, not by beneficiary pressure. If scenario 2 is correct, the reading is a false summit: beneficiaries (scholars) have constructed the ''archival preservation'' framing to extract professional standing and de-obligate the tradition. The mountainhood turns on whether the obligation''s exit is a historical fact or a beneficiary-defended position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_ambiguity, conceptual, 'Whether the claimed mountainhood reflects natural dissolution or beneficiary-constructed framings.').

omega_variable(
    sibling_reading_coexistence,
    'Can archival_preservation coexist with study_as_performance in a single Jewish legal framework, or does archival_preservation logically foreclose study_as_performance?',
    'Examine whether a single framework can hold both: (1) ''Obligation has exited; study is preservation without normative force'' AND (2) ''Study itself fulfills the obligation; the obligation persists through textual engagement.'' These appear contradictory — one asserts obligation exits, the other asserts it persists. But the contradiction may be frame-dependent: different Jewish communities and traditions may adopt different readings. If the same community cannot hold both, the readings foreclose each other in that framework. If different communities hold different readings stably, they coexist.',
    'If the readings foreclose each other, archival_preservation is a stronger claim — it rules out study_as_performance. If they coexist, archival_preservation is a weaker claim — it is one option among several live readings, not a dominant frame. The coexistence question determines how sharply the reading boundary is drawn.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether archival_preservation and study_as_performance are logically incompatible or can coexist across different communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, information_standard).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested kernel 'sacrifice_obligation_continuity.' All four readings share the same historical referent (the status of sacrifice law after Temple destruction) but propose different structural relationships to it. They are linked via network.affects_constraints to form a constraint family. The archival_preservation reading claims obligation has exited; study_as_performance claims obligation persists through textual engagement; messianic_suspension claims obligation is suspended pending restoration; performance_only claims obligation awaits or will require future performance. Each reading has distinct extractiveness, beneficiaries, and structural properties. They are separate constraint stories, not alternative measurements of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
