% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study as Genuine Exercise of Sacrifice Obligation (Halakhic Reading)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint story represents the study_as_exercise_reading of the
 *   sacrifice_obligation_kernel: the halakhic position that intellectual
 *   engagement with sacrifice law (learning the tractates, analyzing the
 *   procedures, understanding the requirements) constitutes genuine
 *   fulfillment of the mitzvah under current conditions where the Temple
 *   stands destroyed. The obligation is not suspended or reduced — it is
 *   occupied through study. This reading claims zero extractiveness because
 *   no material transfer occurs and the arrangement is framed as authorized
 *   transformation rather than loss. The beneficiary is rabbinic authority,
 *   which holds the interpretive monopoly on what counts as valid study,
 *   which texts are canonical, and what constitutes 'genuine engagement.' The
 *   vindicated propositions are Torah study as avodah (service), authorized
 *   transformation of mitzvot when performance is impossible, and
 *   intellectual engagement as obligation-fulfillment.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Primary beneficiary (institutional/arbitrage) — holds interpretive monopoly on fulfillment criteria
 *   - observant_jews: Primary participants (organized/constrained) — engage in study as mitzvah performance
 *   - messianic_expectants: Secondary participants (organized/identity_locked) — hold sibling reading of divine suspension
 *   - temple_restoration_advocates: Excluded (powerful/trapped) — hold performance_only_reading
 *   - secular_scholars: Observer (analytical/analytical) — study as cultural-historical archive (symbolic_archive_reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.02).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.15).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study as Genuine Exercise of Sacrifice Obligation (Halakhic Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, 'cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8').
narrative_ontology:cs_kernel_codification('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', formalized).
narrative_ontology:cs_authority_grounding('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', lineage).
narrative_ontology:cs_interpretation_layer_present('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8').
narrative_ontology:cs_reading_relation('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', foundational, study_is_genuine_avodah).
narrative_ontology:cs_axiom_status(study_is_genuine_avodah, holdable).
narrative_ontology:cs_axiom_grounding('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', study_is_genuine_avodah, deontological).
narrative_ontology:cs_axiom('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', foundational, authorized_transformation_preserves_mitzvah_integrity).
narrative_ontology:cs_axiom_status(authorized_transformation_preserves_mitzvah_integrity, holdable).
narrative_ontology:cs_axiom_grounding('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', authorized_transformation_preserves_mitzvah_integrity, conventional).
narrative_ontology:cs_reference_frame('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', torah_obligation_intact_despite_temple_destruction).
narrative_ontology:cs_drift_state('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', contemporary_halakhic_consensus, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cbfdf7be-4300-4f59-9f2e-6b1568ed6ee8', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, observant_jews).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, messianic_expectants).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__study_as_exercise_reading, observant_jews).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, torah_study_as_avodah).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, authorized_transformation_of_mitzvot).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, intellectual_engagement_fulfills_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the interpretive monopoly on what constitutes valid study, canonical texts, and genuine engagement. Defines the boundaries of the mitzvah's current form. Collects no material rents but maintains status as the authorized adjudicators of halakhic fulfillment. Can shape the constraint's evolution through responsa and institutional authority.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, beneficiary,
    institutional, civilizational, arbitrage, global).

% Engage in study of sacrifice law as their primary mode of fulfilling the mitzvah. Invest significant time, cognitive effort, and educational resources. Experience the study as genuine religious fulfillment (beneficiary) while bearing the cost of sustained intellectual engagement (payer). Exit options are constrained: leaving the framework means abandoning the halakhic system's coherence, but alternative readings exist within the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, observant_jews, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, observant_jews, beneficiary).

% Hold the sibling messianic_suspension_reading: they study sacrifice law to maintain operational readiness for restoration, not as current fulfillment. Their identity is fused with the expectation of imminent messianic transformation. They benefit from the study framework this reading provides but contest its claim that study IS the fulfillment rather than preparation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, messianic_expectants, beneficiary,
    organized, generational, identity_locked, global).

% Hold the performance_only_reading: they advocate for actual physical sacrifice on the Temple Mount now. They are structurally excluded from the halakhic conversation because mainstream authority rejects their position as dangerous/premature. They would object to the claim that study fulfills the obligation — they see it as evasion. Their exit is trapped: they cannot perform the mitzvah as they understand it, and their reading is marginalized.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, temple_restoration_advocates, excluded,
    powerful, biographical, trapped, national).

% Study sacrifice law as cultural-historical archive (symbolic_archive_reading). They make no halakhic claim and are neither beneficiaries nor payers within the halakhic system. Their engagement is analytical and preservative. They observe the internal contest among readings from outside the commitment structure.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, secular_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__study_as_exercise_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__study_as_exercise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the vitality and operational continuity of the sacrifice obligation when physical performance is impossible, by authorizing intellectual engagement as legitimate fulfillment. Solves the coordination problem of how a community sustains a Temple-centered mitzvah without a Temple, without fragmenting into competing claims about suspension or abrogation.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy capital to rabbinic authority (gatekeeping what counts as valid study/engagement). Moves time, attention, and cognitive investment from observant Jews into the study framework. No material transfer occurs. The 'gain' is the maintenance of halakhic continuity and the authority's interpretive monopoly.
% ABSENT_VOICES: Those who would perform physical sacrifice if permitted (temple_restoration_advocates) are structurally excluded from mainstream halakhic discourse. Those who reject halakhic authority entirely (secular Jews, non-Jews) are not in the conversation. The messianic_expectants are present but hold a sibling reading — they are not absent, they are in contention.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the sacrifice obligation would revert to contested status: performance_only advocates would push for physical restoration, messianic_expectants would claim divine suspension, secular scholars would treat it as archive only. The halakhic system would lose its authorized mechanism for occupying the obligation. Observant Jews would lose their primary mode of fulfillment. The rabbinic authority would lose its interpretive monopoly over this mitzvah's current form.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the sacrifice obligation — central to Torah law — became physically impossible to perform. The community faced: (a) treat the obligation as abrogated (loss of Torah integrity), (b) demand impossible performance (crisis of feasibility), (c) await divine restoration (passive suspension), or (d) authorize an alternative mode of fulfillment that preserves the obligation's vitality. This reading instantiates option (d).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record (Temple destruction, cessation of sacrifices) and by all four readings' own existence — each reading is a response to the same founding problem. The performance_only_reading and messianic_suspension_reading corroborate the problem's reality by offering different solutions. The symbolic_archive_reading corroborates by denying the problem has a halakhic solution. No single party owns the founding problem; it is the kernel's historical condition.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because the constraint involves no material transfer, no mandatory payment, and no coercive extraction — study is voluntary intellectual engagement framed as fulfillment. Suppression is low (0.15) but non-zero because the interpretive monopoly creates soft boundaries: unauthorized study methods or texts may be dismissed as 'not genuine engagement,' creating subtle pressure toward authorized channels. Theater ratio is minimal (0.05) because the study function is the genuine article, not a performance masking extraction. Accessibility collapse is low (0.2) because alternatives (other readings, secular study, non-participation) remain fully accessible — no one is prevented from holding a different reading. Resistance is moderate (0.35) because the performance_only_reading and messianic_suspension_reading actively contest this reading's legitimacy, and the symbolic_archive_reading denies its halakhic force entirely.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, this constraint is pure coordination (rope): it solves the problem of how to maintain the sacrifice obligation's vitality when physical performance is impossible, and the authority's interpretive role is the coordination mechanism. From the observant Jew seat, the constraint is experienced as genuine obligation-fulfillment — study IS the mitzvah. From the performance_only_reading seat, this reading appears as a substitution that avoids the real obligation. From the messianic_suspension_reading seat, this reading appears as presumptuous — only divine authority can transform the obligation. The engine computes these per-seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority is the structural beneficiary (d near 0.0): it controls the interpretive framework that defines valid fulfillment, collects no material rents but holds status/legitimacy capital. Observant Jews are near-symmetric (d ~ 0.5): they voluntarily engage in study as fulfillment, gain spiritual/religious benefit, bear time/attention cost. No victim set exists — the reading explicitly frames suspension as authorized transformation, not extraction from a victim class. The messianic_suspension_reading and performance_only_reading are sibling constraints with their own beneficiary/victim structures, not victims of this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain sacrifice obligation vitality without Temple) remains live — the Temple is still destroyed, the obligation still cannot be physically performed. The arrangement has not outlived its function; it actively occupies the obligation. No mandatrophy resolution is declared because the founding problem persists. The constraint is not a degraded remnant (piton) nor a transitional measure (scaffold) — it is the current authorized form of the mitzvah.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this constraint instantiate one reading of the sacrifice_obligation_kernel rather than the kernel itself?',
    'Committee frame: this story IS the study_as_exercise_reading of the sacrifice_obligation_kernel. The sibling readings (messianic_suspension_reading, performance_only_reading, symbolic_archive_reading) are separate constraints with their own ε values, beneficiaries, and classifications.',
    'Confirms ε-invariance discipline: each reading gets its own constraint_id, its own extractiveness, its own type. The kernel is the shared referent; the readings are distinct instantiated constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the sacrifice_obligation_kernel; sibling readings are separate constraints.').

omega_variable(
    interpretive_monopoly_as_extraction,
    'Does the rabbinic authority''s interpretive monopoly on what counts as fulfillment constitute a subtle extraction mechanism despite low measured extractiveness?',
    'Trace whether the authority''s gatekeeping power over valid study methods, canonical texts, and认定 of ''genuine engagement'' creates dependency that benefits the authority structurally — even if no material transfer occurs. Compare with performance_only_reading where the authority''s gatekeeping is over physical performance.',
    'If the interpretive monopoly functions as structural extraction (controlling access to legitimacy), the constraint''s effective classification shifts toward tangled_rope despite near-zero base extractiveness. This would reveal a coordination-extraction hybrid masked by the ''study is fulfillment'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_as_extraction, conceptual, 'Whether interpretive gatekeeping constitutes extraction even without material transfer.').

omega_variable(
    suspension_vs_transformation_boundary,
    'Is the boundary between ''authorized transformation'' (this reading) and ''divine suspension'' (messianic_suspension_reading) a genuine structural difference or a terminological distinction without operational consequence?',
    'Examine whether the two readings produce different halakhic consequences for: (a) liability for non-study, (b) conditions for resumption of physical sacrifice, (c) status of one who studies without proper intent. If consequences are identical, the boundary is terminological.',
    'If structurally identical, the two readings collapse into one constraint (ε-invariance violation — same ε, same stakeholders, same type). If distinct, they remain separate stories linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_transformation_boundary, conceptual, 'Whether authorized transformation and divine suspension are structurally distinct or terminologically distinct only.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_study_exercise_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacrifice_study_exercise_tr_t50, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(sacrifice_study_exercise_tr_t100, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(sacrifice_study_exercise_tr_t150, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 150, 0.05).
narrative_ontology:measurement(sacrifice_study_exercise_tr_t200, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 200, 0.05).
narrative_ontology:measurement(sacrifice_study_exercise_tr_t250, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 250, 0.05).

% Extraction over time
narrative_ontology:measurement(sacrifice_study_exercise_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(sacrifice_study_exercise_be_t50, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 50, 0.02).
narrative_ontology:measurement(sacrifice_study_exercise_be_t100, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 100, 0.02).
narrative_ontology:measurement(sacrifice_study_exercise_be_t150, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 150, 0.02).
narrative_ontology:measurement(sacrifice_study_exercise_be_t200, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 200, 0.02).
narrative_ontology:measurement(sacrifice_study_exercise_be_t250, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 250, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_study_exercise_su_t0, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sacrifice_study_exercise_su_t50, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement(sacrifice_study_exercise_su_t100, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 100, 0.14).
narrative_ontology:measurement(sacrifice_study_exercise_su_t150, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 150, 0.15).
narrative_ontology:measurement(sacrifice_study_exercise_su_t200, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 200, 0.15).
narrative_ontology:measurement(sacrifice_study_exercise_su_t250, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 250, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: sacrifice_obligation_kernel with four readings. This reading (study_as_exercise) claims zero extractiveness, beneficiary = rabbinic authority (interpretive monopoly). The performance_only_reading claims higher extractiveness (physical performance demanded but impossible, creating guilt/obligation debt), victim = observant Jews. The messianic_suspension_reading claims near-zero extractiveness but different coordination function (readiness maintenance). The symbolic_archive_reading claims zero extractiveness, no halakhic beneficiaries, treats study as cultural preservation. All four linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
