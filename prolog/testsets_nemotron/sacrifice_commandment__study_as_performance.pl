% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfillment of the Commandment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint story instantiates the 'study_as_performance' reading of
 *   the contested kernel 'sacrifice_commandment'. The reading holds that
 *   intellectual engagement with sacrificial law — learning the tractates of
 *   Kodashim, analyzing the procedures of the korbanot — IS the fulfillment
 *   of the commandment itself, not a substitute or preparation. The
 *   scholar-worshipper who studies Zevachim or Menachot performs the avodah
 *   intellectually; the divine obligation is discharged through the mind's
 *   alignment with the Torah's structure. No Temple, no altar, no animal is
 *   required. The constraint is the halakhic principle that makes this
 *   equivalence binding. From this reading's lights, the standing arrangement
 *   is a pure coordination: the system coordinates the scholar's intellect
 *   with the divine will through the medium of Torah study. There is no
 *   extraction — the scholar benefits (zechut, deveikut, fulfillment), no
 *   party pays, no enforcement is needed beyond the scholar's own commitment.
 *   The reading asserts this is a mountain: a structural feature of the
 *   halakhic universe, not a human institution.
 *
 * KEY AGENTS:
 *   - scholar_worshipper: Primary beneficiary (analytical/biographical) — fulfills divine obligation through study
 *   - rabbinic_authority: Secondary agenda_setter (institutional/generational) — transmits and authorizes the equivalence
 *   - divine_lawgiver: Ultimate agenda_setter (analytical/universal) — source of the commandment and its fulfillment conditions
 *   - analytical_observer: Observer (analytical/universal) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.02).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.02).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, mountain).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Fulfillment of the Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic").

domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, '2003a7ea-ffe8-4036-8947-0ace8ff3c9c5').
narrative_ontology:cs_kernel_codification('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', fixed_text).
narrative_ontology:cs_authority_grounding('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', lineage).
narrative_ontology:cs_interpretation_layer_present('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5').
narrative_ontology:cs_reading_relation('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', foundational, study_is_avodah).
narrative_ontology:cs_axiom_status(study_is_avodah, holdable).
narrative_ontology:cs_axiom_grounding('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', study_is_avodah, deontological).
narrative_ontology:cs_axiom('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', foundational, torah_lo_bashamayim_hi).
narrative_ontology:cs_axiom_status(torah_lo_bashamayim_hi, holdable).
narrative_ontology:cs_axiom_grounding('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', torah_lo_bashamayim_hi, deontological).
narrative_ontology:cs_reference_frame('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', torah_study_as_divine_service).
narrative_ontology:cs_drift_state('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', contemporary_post_temple, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2003a7ea-ffe8-4036-8947-0ace8ff3c9c5', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshipper).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, torah_lo_bashamayim_hi).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, talmud_torah_keneged_kulam).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, zechut_avot).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Studies sacrificial law (Kodashim tractates, korbanot procedures) as the direct fulfillment of the divine commandment. Experiences the study as intrinsically valuable worship — intellectual engagement with Torah structure aligns the mind with divine will. No material cost net of reward: the study provides intellectual satisfaction, spiritual fulfillment (deveikut), and communal honor. Can adopt or abandon this reading at will; alternative readings (performance_only, archive_maintenance) remain fully accessible. Exit is costless — one simply studies differently or stops.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshipper, beneficiary,
    analytical, biographical, arbitrage, universal).

% Transmits and authorizes the halakhic principle 'talmud torah keneged kulam' and its application to sacrificial law. Adjudicates the boundary of what counts as valid study-for-fulfillment. Does not extract from the scholar — the authority's role is interpretive transmission, not rent collection. Benefits indirectly from the system's coherence and the scholar's deference, but this is not extraction from the constraint. Can revise the interpretation (analytical exit) but institutional inertia makes revision costly.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, rabbinic_authority, agenda_setter,
    institutional, generational, analytical, universal).

% The ultimate source of the commandment and the definition of its fulfillment. In this reading, the divine will is that study IS the performance — the Torah's structure makes intellectual engagement equivalent to physical execution. No extraction, no enforcement needed beyond the scholar's own alignment. The constraint emerges from the divine intellect's self-revelation in Torah.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, divine_lawgiver, agenda_setter,
    analytical, civilizational, analytical, universal).

% Sees the full kernel structure: three readings of sacrifice_commandment, each instantiating a different constraint with different ε, beneficiaries, and classifications. This reading claims mountain with zero extraction; the siblings claim piton/snare and scaffold/tangled_rope. The observer notes the ε-invariance principle at work: the label 'sacrifice commandment' covers three structurally distinct constraints.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_commandment__study_as_performance, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the scholar's intellect with the divine will through the medium of Torah study, making intellectual engagement the direct fulfillment of the sacrificial commandment without requiring physical Temple service.
% TRANSFER_FUNCTION: No transfer occurs. The arrangement moves nothing from anyone to anyone. The scholar's study is the fulfillment itself; the divine obligation is discharged by the act of study. No value flows between parties.
% ABSENT_VOICES: The performance_only reading's adherents (who hold the commandment is suspended) and the archive_maintenance reading's adherents (who hold study is preservation not worship) are not excluded from this reading's framework — they hold different readings of the same kernel. Within this reading's framework, no voice is absent; the constraint's operation does not depend on silencing alternatives.
% DISAPPEARANCE_RATIONALE: If the equivalence 'study = performance' vanished overnight, the scholar could adopt performance_only (commandment suspended) or archive_maintenance (study as preparation) without material loss. The divine obligation's fulfillability would shift, but the scholar's situation — studying Torah — remains. The world does not rearrange because the constraint is a reading, not an enforcement mechanism.
% FOUNDING_PROBLEM: How to fulfill the sacrificial commandments after the Temple's destruction (70 CE) when physical performance is impossible. The study_as_performance reading answers: the commandment was never essentially physical — its essence is the intellectual alignment with divine will that study achieves.
% FOUNDING_PROBLEM_CORROBORATION: The scholar-worshipper and rabbinic_authority attest the problem is live and solved by this reading. The performance_only reading's adherents (Brisker school, some Lithuanian yeshivot) attest the problem is live but UNSOLVED — study cannot replace performance. The archive_maintenance reading's adherents (some modern academic Talmud scholars) attest the problem is REFRAMED — study is preservation, not fulfillment. No neutral corroboration exists outside the reading communities.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_commandment__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε = 0.02: Near-zero base extractiveness. The reading's own lights see no transfer of value from a victim to a beneficiary. The scholar-worshipper incurs no net cost — study is intrinsically rewarding (intellectual satisfaction, spiritual fulfillment, communal honor). The 0.02 residual reflects the marginal opportunity cost of study time, which this reading treats as de minimis. Suppression = 0.0: No coercion, no alternatives suppressed. The performance_only reading remains a live option; nothing forces the scholar-worshipper to adopt this reading. Theater_ratio = 0.03: Minimal performative overhead — the study is the thing itself. Accessibility_collapse = 0.15: Alternatives (performance_only, archive_maintenance) remain fully accessible; the constraint does not collapse them. Resistance = 0.05: Only trivial resistance (some may find the equation implausible, but no structural resistance). Emerges_naturally = true: The reading claims this equivalence is a necessary entailment of halakhic ontology (Torah lo bashamayim hi — the Torah is not in heaven; its interpretation is the ongoing revelation). Beneficiaries = [scholar_worshipper]: The only party who benefits is the one who performs the study. No victims. Vindicated propositions document the doctrinal support.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat classifications from the structural data. The scholar_worshipper seat (beneficiary, analytical/biographical, mobile/arbitrage exit) should compute as mountain — zero effective extraction, full coordination. The rabbinic_authority seat (agenda_setter, institutional/generational, analytical exit) also computes mountain — it administers a constraint that extracts nothing. The analytical_observer sees the same. If the performance_only reading were instantiated as a separate constraint, its scholar seat would experience suppression (commandment suspended, unfulfillable) and the kernel would appear as a snare or piton. The divergence across readings is the point of the kernel decomposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: scholar_worshipper is declared beneficiary with mobile/arbitrage exit (can adopt any reading, can stop studying) → d ≈ 0.0 (full beneficiary). Rabbinic_authority is agenda_setter with analytical exit → d ≈ 0.2 (mild target, administers but does not extract). No victims declared. The engine will derive d ≈ 0.0 for the beneficiary seat and d ≈ 0.2 for the agenda_setter seat. Effective extraction χ ≈ ε for both seats (damping for beneficiary, slight amplification for agenda_setter), remaining near zero. This matches the reading's claim of zero extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to fulfill the sacrificial commandment without a Temple) is CONTESTED — the performance_only reading says it cannot be fulfilled; archive_maintenance says the problem is preservation not fulfillment. This reading says the problem IS solved by study. No mandatrophy: the arrangement's function (fulfillment through study) is live and uncontested within this reading. The mandate has not outlived its function because the function is the study itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''sacrifice_commandment'', or is it a standalone constraint?',
    'Cross-file manifest verification: the kernel_id sacrifice_commandment is declared in the SCOPE decomposition manifest with reading_id study_as_performance. The sibling readings performance_only and archive_maintenance are separate constraint stories. If the manifest structure holds, this reading instantiates a distinct constraint with its own ε.',
    'If the kernel structure is rejected, this story reverts to an ordinary constraint without committer-frame fields. The ε remains 0.02 and classification mountain, but the cs_structure block would be omitted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment of this story to the kernel/reading frame').

omega_variable(
    natural_law_vs_constructed_fulfillment,
    'Is the equation of study with sacrificial performance a genuine natural-law entailment of the halakhic system, or a constructed interpretive move that benefits the scholar class?',
    'Historical-genealogical analysis of when ''talmud torah keneged kulam'' was first invoked to substitute for Temple service; comparison with the archive_maintenance reading''s claim that study is preservation not worship. If the equation emerged post-70 CE as a response to Temple destruction rather than from pre-existing halakhic principle, it is constructed.',
    'If constructed, the mountain claim is a false summit: identifiable beneficiaries (scholar_worshipper, rabbinic authority) exist, the constraint does not emerge naturally, and FSM would reclassify toward tangled_rope or piton. If genuine natural-law entailment, the mountain classification holds with zero extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_fulfillment, conceptual, 'Whether the study-as-performance equivalence is a natural-law mountain or a constructed constraint benefiting the scholar class').

omega_variable(
    zero_extraction_claim,
    'Is the extractiveness truly zero, or does the arrangement extract labor, status, or resources from the scholar-worshipper toward institutional beneficiaries not declared here?',
    'Institutional analysis of yeshiva/kollel economies: who funds the scholar''s study, who accrues prestige and authority from the scholar''s output, whether the scholar''s time is truly voluntary worship or coerced by communal expectation. If the scholar bears material cost without commensurate return, ε > 0.',
    'If extraction is non-zero, the mountain classification fails (mountain requires near-zero ε). The constraint would reclassify based on who bears the cost and who benefits — likely tangled_rope if coordination exists, snare if purely extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zero_extraction_claim, empirical, 'Whether the scholar-worshipper is genuinely a pure beneficiary or bears hidden costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__study_as_performance, base_extractiveness, 100, 0.01).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__study_as_performance, base_extractiveness, 500, 0.02).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__study_as_performance, base_extractiveness, 1000, 0.02).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__study_as_performance, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_commandment__study_as_performance, base_extractiveness, 2000, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__study_as_performance, 0.05).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% Kernel sacrifice_commandment decomposes into three constraint stories with divergent ε and classification: study_as_performance (mountain, ε≈0.02), performance_only (piton/snare, ε≈0.7), archive_maintenance (scaffold/tangled_rope, ε≈0.15). The divergence is structural, not observational — each reading instantiates a different constraint with different beneficiaries, victims, and enforcement structure. This story links to its siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
