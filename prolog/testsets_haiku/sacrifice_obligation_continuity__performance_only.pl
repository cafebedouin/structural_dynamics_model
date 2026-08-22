% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation Requires Physical Performance (Rabbinic Law Absence Reading)
 *   domain: religious/legal/textual
 *
 * SUMMARY:
 *   The Rabbinic corpus confronts an irreducible practical problem: the Torah
 *   commands sacrifice (Leviticus 1–7) as an ongoing obligation, but no
 *   legitimate sacrificial altar has existed since the destruction of the
 *   Jerusalem Temple in 70 CE. A performance_only reading asserts that the
 *   obligation persists unchanged: the commandment requires physical
 *   performance (animal sacrifice at an authorized altar), study of
 *   sacrificial law is preparation for future restoration (not fulfillment),
 *   and the current generation lives under an obligation it cannot satisfy.
 *   This reading produces a structurally unstable situation: a binding
 *   obligation without a lawful remedy, imposing theological
 *   guilt-without-satisfaction on those who study and recognize the
 *   impossibility. The reading is one of four coherent interpretations of the
 *   same foundational texts (the kernel: the Torah's sacrificial
 *   commandments); the other three readings reframe the obligation as
 *   suspended-but-not-violated (messianic_suspension),
 *   fulfilled-through-study (study_as_performance), or no-longer-binding
 *   (archival_preservation). This constraint story captures only the
 *   performance_only reading — the structure unique to one interpretation.
 *
 * KEY AGENTS:
 *   - jewish_people_current_generations: Bound by the sacrifice obligation they cannot perform; study is preparation, not satisfaction.
 *   - legal_scholars_rabbinic_authority: Interpret and enforce the obligation; benefit from the interpretive framework that makes their scholarship necessary.
 *   - temple_restoration_movement: Anticipates future performance and frames study as readiness for when the Temple is rebuilt.
 *   - alternative_readings_adherents: Other Jewish communities accept study_as_performance or messianic_suspension; they reject this reading's guilt structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.78).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.71).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation Requires Physical Performance (Rabbinic Law Absence Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious/legal/textual").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '3c0323d1-4637-4a44-ac53-99a13b1637aa').
narrative_ontology:cs_kernel_codification('3c0323d1-4637-4a44-ac53-99a13b1637aa', fixed_text).
narrative_ontology:cs_authority_grounding('3c0323d1-4637-4a44-ac53-99a13b1637aa', lineage).
narrative_ontology:cs_interpretation_layer_present('3c0323d1-4637-4a44-ac53-99a13b1637aa').
narrative_ontology:cs_reading_relation('3c0323d1-4637-4a44-ac53-99a13b1637aa', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('3c0323d1-4637-4a44-ac53-99a13b1637aa', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('3c0323d1-4637-4a44-ac53-99a13b1637aa', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('3c0323d1-4637-4a44-ac53-99a13b1637aa', foundational, physical_performance_requirement).
narrative_ontology:cs_axiom_status(physical_performance_requirement, holdable).
narrative_ontology:cs_axiom_grounding('3c0323d1-4637-4a44-ac53-99a13b1637aa', physical_performance_requirement, deontological).
narrative_ontology:cs_axiom('3c0323d1-4637-4a44-ac53-99a13b1637aa', foundational, obligation_immutability).
narrative_ontology:cs_axiom_status(obligation_immutability, holdable).
narrative_ontology:cs_axiom_grounding('3c0323d1-4637-4a44-ac53-99a13b1637aa', obligation_immutability, deontological).
narrative_ontology:cs_axiom('3c0323d1-4637-4a44-ac53-99a13b1637aa', secondary, study_as_preparation).
narrative_ontology:cs_axiom_status(study_as_preparation, holdable).
narrative_ontology:cs_axiom_grounding('3c0323d1-4637-4a44-ac53-99a13b1637aa', study_as_preparation, instrumental).
narrative_ontology:cs_reference_frame('3c0323d1-4637-4a44-ac53-99a13b1637aa', perpetual_sacrifice_obligation).
narrative_ontology:cs_drift_state('3c0323d1-4637-4a44-ac53-99a13b1637aa', post_temple_destruction_rabbinic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3c0323d1-4637-4a44-ac53-99a13b1637aa', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, legal_scholars).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_israelites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, temple_restoration_anticipators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and teach Rabbinic law regarding sacrifice. Under this reading, they maintain the obligation's binding force while acknowledging its impossibility, thus validating their interpretive authority as necessary mediators between divine law and lived practice. Their scholarship is essential to preserving the obligation and preparing for future restoration. They benefit from the reading's framework, which makes their work indispensable; their exit options are constrained by institutional and professional identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, legal_scholars, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__performance_only, legal_scholars, beneficiary).

% Live under the sacrifice obligation without the legal means to fulfill it. They recognize the commandment as binding but face perpetual normative failure and guilt-without-remedy. They may study the sacrificial law as preparation or acknowledgment, but the study does not satisfy the obligation under this reading. Their exit options are severely constrained: rejecting the obligation means rejecting a core element of their Jewish identity and community standing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_israelites, payer,
    moderate, biographical, identity_locked, regional).

% Maintain expectation that the Temple will be restored and sacrifice resumed. Under this reading, their preparatory study and preservation of sacrificial knowledge is meaningful and eventual fulfillment is possible. They frame the current generation's obligation as readiness for restoration. Their hope is structurally dependent on the obligation remaining unfulfilled in the present — if the obligation were satisfied through study (study_as_performance reading), restoration would be unnecessary.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, temple_restoration_anticipators, beneficiary,
    organized, civilizational, identity_locked, regional).

% Accept study_as_performance or messianic_suspension readings, which relieve the guilt-without-remedy structure of performance_only. They are excluded from the decision-making authority of the Rabbinic establishment that enforces this reading. If they had a seat, they would argue for reinterpreting fulfillment or suspending the obligation rather than sustaining guilt. Their absence from the interpretive authority structure is what allows the performance_only reading to persist despite offering no remedy.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, alternative_reading_adherents, excluded,
    organized, biographical, constrained, regional).

% Maintains the institutional authority to interpret Torah and enforce Rabbinic law. Under this reading, they sustain the obligation's binding force as a matter of halakhic principle (divine law is immutable), even though its fulfillment is structurally impossible. They resist reinterpretation that would relieve the guilt burden (study_as_performance) or suspend the obligation (messianic_suspension). Their institutional legitimacy rests on maintaining the integrity of the halakhic system, which they interpret as requiring this reading despite its cost.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_establishment, agenda_setter,
    institutional, generational, trapped, regional).

% Analyze the constraint from outside the tradition. They document the tension between obligation and impossibility, map the four competing readings, and measure the psychological and institutional costs of sustaining guilt-without-remedy across generations.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, non_jewish_observers, observer,
    analytical, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, legal_scholars).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the unbroken obligation to sacrifice as a core commandment and preserves the legal tradition that transmits this obligation across generations. Study and interpretation sustain the textual and halakhic knowledge necessary for future Temple restoration. The obligation coordinates collective Jewish identity around fidelity to divine law, even when that law is currently impossible to fulfill.
% TRANSFER_FUNCTION: Transfers the burden of unfulfilled obligation (guilt, normative failure, spiritual anxiety) from the legal scholars and institutional authority (who interpret and maintain the reading) to the current generation of Jews (who live under the obligation they cannot satisfy). The scholars benefit from the interpretive authority and spiritual significance the obligation confers; the congregant-victims carry the guilt and the identity-fusion that prevents exit.
% ABSENT_VOICES: Adherents of the study_as_performance and messianic_suspension readings would object that this reading falsely perpetuates guilt-without-remedy and that their alternative readings better honor both the obligation and the Temple's destruction. They are excluded from the interpretive authority structure of the Rabbinic establishment, which enforces the performance_only reading as the correct understanding of halakhic obligation.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the study_as_performance reading became dominant, the guilt-without-remedy structure would dissolve: study would become fulfillment, and the current generation would no longer live under an impossible obligation. This would substantially alter the psychological and spiritual landscape of Jewish practice. If the messianic_suspension reading became dominant, the obligation would be suspended (not violated), relieving guilt while maintaining readiness. The constraint as authored is not merely a belief — it shapes lived practice, self-understanding, and institutional authority; its disappearance would require institutional and interpretive change.
% FOUNDING_PROBLEM: The destruction of the Temple in 70 CE eliminated the physical location and institutional framework for performing sacrifice, an obligation the Torah presents as perpetual. The founding problem was: how to maintain fidelity to an unchangeable divine commandment in the absence of the means to perform it.
% FOUNDING_PROBLEM_CORROBORATION: The Rabbinic establishment (defenders of this reading) attests the founding problem is live and unresolved: the Temple is still destroyed, the obligation persists, fulfillment is impossible. Scholars of alternative readings attest that the founding problem has been solved by reinterpreting fulfillment (study_as_performance) or accepting suspension (messianic_suspension): the problem was 'how to be faithful to the obligation,' not 'how to perform sacrifice,' and other readings provide coherent answers. Historians and comparative religious scholars note that the tension between obligation and impossibility has been sustained as a structurally productive problem for 2000 years, creating institutional roles for scholars and spiritual significance for the community — the problem's non-resolution is functional for the authority structure that maintains this reading.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the obligation structure creates liability-without-remedy: the current generation faces normative failure and guilt for a commandment execution is structurally impossible without Temple restoration. Suppression is moderately high (0.71) because the reading depends on maintaining the obligation's binding force despite the recognized impossibility — alternative interpretations (study_as_performance, messianic_suspension) relieve the guilt burden, so holding this reading requires suppressing those alternatives as inadequate or theologically false. Theater ratio is elevated (0.62) because much of the enacted study activity functions as performative guilt-management and readiness-signaling rather than genuine preparation for imminent Temple restoration — the most likely scenario is no Temple in the foreseeable future, so study continues as ritual acknowledgment. Accessibility_collapse is very high (0.88) because the reading's core premise (the obligation is unchanged, study does not satisfy it) is grounded in the immutability of divine law; once accepted, alternatives collapse — one cannot coherently hold that the obligation is both eternal and somehow nullified by circumstances. Resistance is moderate (0.42) because many Jewish communities and movements explicitly reject this reading in favor of the alternatives, creating persistent counter-pressure, but the Rabbinic establishment has institutional authority to enforce it despite dissent. The measurement series shows subtle rise in extractiveness and theater over 2000 years, tracking the increasing burden of unremedied obligation as generation succeeds generation without Temple restoration.
 *
 * PERSPECTIVAL GAP:
 *   The difference between scholar and congregant seats drives the per-seat type divergence. From the scholar's position, this constraint may compute as rope (genuine coordination: maintaining the textual tradition, preparing for messianic restoration, preserving continuity). From the congregant's position, it computes as tangled_rope or snare (coordination cover for extraction: the obligation persists without remedy, generating unresolvable guilt and validating the scholar class that interprets it). The engine computes each per-seat type from power + exit + beneficiary/victim data; the claimed_type (tangled_rope) reflects the system-level assessment that both functions are structurally present but asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal scholars derive directionality near the beneficiary end (d ≈ 0.2–0.3): the obligation validates their interpretive authority, creates demand for their scholarship, and allows them to maintain a role as mediators between divine law and lived practice. They have institutional power and can shape how the obligation is understood and taught. Current-generation Jews bear the victim end (d ≈ 0.75–0.85): they face normative failure, guilt-without-remedy, and identity-fusion that makes exit (rejecting the obligation) psychologically and socially costly. Their exit options are identity_locked — leaving this reading means rejecting a core component of their religious identity. The asymmetry is structural: the reading's persistence depends on scholars maintaining the obligation's binding force while the congregant-victims absorb the guilt. Study functions as coordination (maintaining the textual tradition) and extraction (scholars benefit from the validated obligation; congregants pay in guilt and frustration).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the destruction of the Temple in 70 CE — is live (the Temple remains destroyed; no sacrifice is possible). The mandate was to perform sacrifice as an ongoing obligation. The obligation persists unchanged (under this reading), but the performance mechanism is gone. This is mandatrophy: the mandate persists while the functional problem it addresses has been institutionally managed via the obligation reframing (study as preparation, ritual acknowledgment of the obligation, expectation of future restoration). The obligation no longer solves the Temple-destruction crisis because the crisis is now 2000 years old and managed through other institutional mechanisms (synagogue worship, Rabbinic law, messianic hope). The performance_only reading preserves the obligation at the cost of perpetual guilt-without-remedy; other readings (study_as_performance, messianic_suspension) resolve the mandatrophy by reinterpreting fulfillment or suspension. This reading resists resolution, which is what makes it extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_termination_ambiguity,
    'Is the performance obligation genuinely suspended (unfulfilled but not violated) or is it effectively terminated for the current generation while only nominally alive?',
    'Compare this reading''s normative consequences with the messianic_suspension reading: does the current generation bear guilt for non-performance, or do they bear only the obligation to preserve readiness? If guilt is central to this reading, termination language clarifies the structural difference.',
    'If suspended, the obligation creates liability without remedy (extractive); if terminated, the reading admits the obligation is no longer binding and reduces extractiveness. The guilt-without-remedy structure is what makes the distinction empirically tractable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_termination_ambiguity, conceptual, 'Whether performance impossibility ends the obligation or leaves it suspended and unfulfillable.').

omega_variable(
    study_placeholder_functionality,
    'Does the authorized study fulfill any part of the sacrifice obligation, or is study purely preparatory and the obligation remains entirely unsatisfied?',
    'Examine Talmudic sources directly: does the canon assign halakhic credit to study of sacrificial law, or is study explicitly excluded from obligation-satisfaction? The study_as_performance reading resolves this oppositely.',
    'If study carries halakhic efficacy, extractiveness drops substantially (study becomes partial fulfillment, reducing the guilt burden on current agents); if study is purely preparatory, extractiveness remains high (guilt persists, unsatisfied).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_placeholder_functionality, empirical, 'Whether study of sacrifice law carries halakhic efficacy or is preparation only.').

omega_variable(
    victim_set_determination,
    'Who bears the obligation — only current rabbinical interpreters, or does liability extend to the entire Jewish people collectively, or to all Israel of every generation?',
    'Textual analysis of the legal formula (Mishnah 5:8, etc.): ''we study as if performing'' — does the formula distribute liability across the whole community, or locate it in the scholarly class responsible for interpretation?',
    'If liability is universal (all Israel), extractiveness is tempered by distributed burden; if localized in scholars, extractiveness concentrates on a smaller, more organized group with higher power, raising effective extraction. The ''current generation'' victim framing assumes broad distribution; concentrated reading would shift who pays.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_determination, conceptual, 'Whether obligation-failure liability is distributed across the Jewish community or concentrated in the scholarly class.').

omega_variable(
    textual_covenant_vs_structural_impossibility,
    'Is the reading grounded in the immutability of divine law (the obligation persists unchanged because it is covenantal) or in the practical impossibility of performance (the obligation persists because we cannot change it, even though we cannot fulfill it)?',
    'Jurisprudential analysis: does the reading derive from deontological claims about law''s eternal binding force, or from pragmatic concession that performance is structurally impossible and the law cannot be nullified? Different grounds lead to different remedial pathways.',
    'If covenantal/deontological, the constraint is harder to alter (would require theological revision); if pragmatic/structural, legislative or technological remedies (rebuilding the Temple) could resolve it. The gateway to falsification and remediation differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_covenant_vs_structural_impossibility, conceptual, 'Whether the obligation''s persistence is grounded in covenant or structural impossibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.58).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__performance_only, theater_ratio, 500, 0.59).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__performance_only, theater_ratio, 1000, 0.61).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__performance_only, theater_ratio, 1500, 0.62).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__performance_only, theater_ratio, 2000, 0.62).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 500, 0.74).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1000, 0.76).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1500, 0.77).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2000, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 500, 0.69).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1500, 0.71).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2000, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__performance_only, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_continuity kernel decomposes into four constraint stories, each representing a different reading of the same foundational texts (Torah commandments and Rabbinic interpretation). The performance_only reading (this story) asserts the obligation is unchanged and requires physical performance; study is preparation, not fulfillment. The study_as_performance reading (sibling) asserts study itself fulfills the obligation. The messianic_suspension reading asserts the obligation is suspended but not violated, pending restoration. The archival_preservation reading asserts the obligation is no longer binding. Each reading has different extractiveness, different victim sets, and different terminal classifications. They are not the same constraint viewed from different angles — their ε values differ substantially (0.78 for performance_only, ~0.35 for study_as_performance, ~0.45 for messianic_suspension, ~0.15 for archival_preservation). The network links the four stories as a family; the engine treats them as separate constraints with independent classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
