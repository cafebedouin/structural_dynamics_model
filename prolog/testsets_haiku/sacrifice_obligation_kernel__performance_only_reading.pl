% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation (Performance-Only Reading): Unfulfilled Mitzvah
 *   domain: religious_law/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   The performance-only reading instantiates one interpretation of the
 *   Jewish sacrifice obligation kernel: the command to offer sacrifices in
 *   the Temple remains halakhically binding, but its performance is
 *   structurally impossible (Temple destroyed, no priesthood, no purity
 *   system). Study of sacrifice law is preparatory and spiritually valuable,
 *   but it does not discharge the mitzvah — the obligation remains
 *   unfulfilled. This reading is distinguished from three sibling readings:
 *   (1) the messianic_suspension reading holds that the obligation is
 *   divinely suspended pending restoration; (2) the study_as_exercise reading
 *   holds that intellectual engagement with the law constitutes genuine
 *   exercise of the mitzvah; (3) the symbolic_archive reading treats the
 *   sacrifice law as a cultural-historical preserve without halakhic claim.
 *   The performance-only reading stands out for its structural clarity: the
 *   command is in force, the preconditions are absent, the gap is unbridged.
 *   This gap has persisted for 1,900 years, creating what appears as
 *   extraction (perpetual unfulfilled obligation) with no identifiable
 *   beneficiary agent — the extraction is structural, not agential.
 *
 * KEY AGENTS:
 *   - jewish_people: the obligated community, identity-locked to the command
 *   - halakhic_authorities: institutional interpreters who administer competing readings and sustain the legal structure
 *   - study_practitioners: engage intensively in sacrifice law without discharging the obligation
 *   - rabbinic_tradition: preserves the kernel and its interpretive framework across centuries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.89).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.72).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, snare).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation (Performance-Only Reading): Unfulfilled Mitzvah").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority/commitment_systems").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, 'cad1c861-f049-4ba0-89b0-4b1d37128cfc').
narrative_ontology:cs_kernel_codification('cad1c861-f049-4ba0-89b0-4b1d37128cfc', fixed_text).
narrative_ontology:cs_authority_grounding('cad1c861-f049-4ba0-89b0-4b1d37128cfc', lineage).
narrative_ontology:cs_interpretation_layer_present('cad1c861-f049-4ba0-89b0-4b1d37128cfc').
narrative_ontology:cs_reading_relation('cad1c861-f049-4ba0-89b0-4b1d37128cfc', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('cad1c861-f049-4ba0-89b0-4b1d37128cfc', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('cad1c861-f049-4ba0-89b0-4b1d37128cfc', sacrifice_obligation_kernel__symbolic_archive_reading, influences).
narrative_ontology:cs_axiom('cad1c861-f049-4ba0-89b0-4b1d37128cfc', foundational, mitzvah_binding_until_explicit_abrogation).
narrative_ontology:cs_axiom_status(mitzvah_binding_until_explicit_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('cad1c861-f049-4ba0-89b0-4b1d37128cfc', mitzvah_binding_until_explicit_abrogation, deontological).
narrative_ontology:cs_axiom('cad1c861-f049-4ba0-89b0-4b1d37128cfc', foundational, precondition_absence_does_not_void_command).
narrative_ontology:cs_axiom_status(precondition_absence_does_not_void_command, holdable).
narrative_ontology:cs_axiom_grounding('cad1c861-f049-4ba0-89b0-4b1d37128cfc', precondition_absence_does_not_void_command, deontological).
narrative_ontology:cs_reference_frame('cad1c861-f049-4ba0-89b0-4b1d37128cfc', torah_command_perpetually_binding).
narrative_ontology:cs_drift_state('cad1c861-f049-4ba0-89b0-4b1d37128cfc', contemporary_post_1900_years, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('cad1c861-f049-4ba0-89b0-4b1d37128cfc', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, jewish_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, study_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, mitzvah_performance_requirement).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, commandment_binding_until_abrogation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commanded to perform animal sacrifice but lacks the capacity to do so: the Temple is destroyed, no altar exists, no authorized priesthood (kohanim) has the ritual purity required. They must carry the unfulfilled obligation as a perpetual debt. Exit means renouncing Jewish identity; remaining in the identity means remaining bound to an unexecutable command.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, jewish_people, payer,
    powerless, generational, identity_locked, global).

% Interpret and transmit the halakhic structure. They face a sustained interpretive pressure: the command remains operative in the legal code, but the performative capacity is absent and has been for nearly two millennia. They administer competing doctrines about the command's status (suspension, transformation, or preservation through study).
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Engage in intensive study of sacrifice law (Mishnah Kodashim, Talmud). Under the performance-only reading, study is preparatory activity but does not discharge the obligation itself. They bear the burden of intellectual engagement without the satisfaction of command fulfillment. Some experience the study as spiritual practice; others experience the perpetual incompleteness as a form of loss.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, study_practitioners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, study_practitioners, observer).

% Preserves the legal texts and interpretive framework. Transmits the command as active law. Does not benefit materially from the unfulfilled obligation but maintains its institutional authority by keeping the command in force and the interpretive conversation alive.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, rabbinic_tradition, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The sacrifice law coordinates the Jewish people around a shared obligation and a shared practice of study. Even under the performance-only reading, the command creates a unified reference point for halakhic identity and sustains an interpretive community dedicated to understanding the law's operation.
% TRANSFER_FUNCTION: The constraint moves psychological and spiritual burden from the halakhic authorities (who could declare the obligation void or transformed) to the Jewish people as a whole, who carry the unfulfilled debt. No material extraction occurs; the transfer is the perpetuation of obligation under conditions of structural impossibility.
% ABSENT_VOICES: Competing readings of the same kernel (messianic_suspension, study_as_exercise, symbolic_archive) are alternative interpretations held by different communities and scholars. Under the performance-only reading, these alternative readings are not seated in the same halakhic framework — they represent different authority structures or different approaches to the same kernel. Their absence from this reading's endorsement does not silence them in practice; multiple communities simultaneously hold different readings.
% DISAPPEARANCE_RATIONALE: If the performance-only reading were abandoned and one of the sibling readings adopted instead (e.g., study-as-exercise), the Jewish people would experience relief from the unbridgeable gap between command and capacity. The world would not rearrange in a material sense, but the halakhic and spiritual landscape would shift fundamentally: the obligation would either be reinterpreted as fulfilled through study, suspended pending messianic restoration, or preserved as cultural archive rather than as active law. The disappearance of THIS reading does not eliminate the kernel (the command remains in the texts) but changes its binding force.
% FOUNDING_PROBLEM: God commanded the Jewish people to offer sacrifices in the Temple as atonement, purification, and thanksgiving. The sacrifice system provided a performative means of addressing sin, maintaining covenant relationship, and sustaining the community's ritual life.
% FOUNDING_PROBLEM_CORROBORATION: Consensus across Jewish communities: the Temple was destroyed in 70 CE by Roman forces, and it has not been rebuilt. The physical, institutional, and purity preconditions for sacrifice are absent and have been for 1,900 years. No Jewish scholar contests this historical fact. The debate is over the LEGAL INTERPRETATION of what a dead founding problem means for the command's ongoing force — does a dead problem suspend the command, transform it, archive it, or leave it eternally binding?
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.0 at t=0 (the command was performable) to 0.89 at t=1900 (nearly two millennia of unfulfilled obligation). The trajectory models the accumulation of unperformed mitzvah. Theater rises from 0.0 to 0.68, reflecting increasing substitution of study and preservation-activity for actual performance. Suppression remains high (0.72) because the Jewish people remain obligated despite structural impossibility — the obligation is maintained in force by halakhic authority despite its unexecutability. The accessibility_collapse (0.91) reflects the near-completeness of the Temple's destruction and the legal closure around alternative sacrifice sites (only the specific Temple in Jerusalem was ever valid). Resistance (0.58) is moderate because the Jewish people have not attempted serious reconstitution of the Temple sacrifice system; the resistance that exists comes from alternative readings (study_as_exercise, symbolic_archive) that implicitly challenge the performance-only reading's binding force. The claim is snare because the obligation persists despite being unexecutable, and the halakhic community sustains this persistence through interpretive authority. No single agent benefits; the extraction is structural.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish people's perspective (identity_locked, powerless), the constraint is an unbridgeable command — they are obligated to do what they cannot do. From the halakhic authorities' perspective (institutional, administrative), the constraint is a legal structure they sustain and interpret; they face the pressure to either reinterpret the command (adopt a sibling reading) or maintain the performance-only reading's unflinching clarity. The engine computes a per-seat directionality: the Jewish people sit at d near 1.0 (full target), while the authorities sit at d near 0.5 (they administer the law but do not directly benefit from the obligation's unfulfilled status). This divergence is structural, not reducible to power differentials.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people are victims under all readings of the kernel — they are obligated. Under the performance-only reading specifically, they are victims of a command whose preconditions are absent. They cannot exit without renouncing Jewish identity (identity_locked). Halakhic authorities are not beneficiaries in the sense of collecting rents; they are agenda-setters insofar as they interpret and transmit the law. The directionality reflects this: the Jewish people bear the burden of unfulfilled obligation; the authorities sustain the interpretive framework that keeps the obligation in force. A directionality override is not needed because the structural data (victims, no named beneficiaries, requires_active_enforcement=true) produces the correct directionality from the engine's derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sacrifice for atonement and covenant maintenance) is DEAD: the Temple does not exist, no Jewish community has successfully reconstituted it, and the legal prerequisites are absent. The disappearance_verdict is CONTESTED because different readings of the kernel produce different verdicts. Under the performance-only reading, the disappearance of the Temple changed the preconditions but not the command's binding force — the command remains in the legal code and the tradition teaches it as law. This is the classic signature of mandatrophy: the founding problem has vanished, but the constraint persists. The constraint is called a snare precisely because it bridges this gap — the obligation is maintained in force despite its unexecutability, and the halakhic community sustains this maintenance through institutional authority. If the performance-only reading were replaced by study_as_exercise (sibling reading), the mandatrophy would be partially resolved: study would become the valid exercise of the mitzvah. If replaced by messianic_suspension, the obligation would become conditional and temporarily deferred. If replaced by symbolic_archive, the constraint would be reclassified from binding law to cultural preservation. The performance-only reading is distinguished by its refusal to resolve the mandatrophy — it keeps the command in force despite its unexecutability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_impossibility_not_agent_extraction,
    'Is the unfulfilled obligation a SNARE (extraction by an agent defending an unexecutable command to maintain authority) or a structural IMPOSSIBILITY (a command whose preconditions are absent, leaving a gap that persists but has no active extractor)?',
    'Examine the intent and function of the halakhic authorities in keeping the performance-only reading alive. If authorities deliberately preserve the reading to maintain their interpretive authority and the community''s ongoing obligation to consult them, it is extractive. If the reading is preserved as a matter of legal principle (the command cannot be abrogated without messianic change or explicit divine instruction), the extraction is structural rather than agential.',
    'If agential extraction is found, the classification holds as snare. If the extractiveness is purely structural (nobody benefits from the gap persisting, but the gap persists because the command''s preconditions are absent), reclassification to a distinct constraint type may be warranted — a structural impossibility is a different category from agential extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_impossibility_not_agent_extraction, conceptual, 'Whether the constraint''s extractiveness arises from agent action or from structural impossibility.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily STRUCTURAL (lack of Temple, lack of purity preconditions, legal barriers to reconstituting sacrifice) or INTERNALIZED (the Jewish people have accepted the obligation as binding law and do not seriously attempt to reconstitute the Temple or challenge the command)?',
    'Historical analysis of attempted reconstructions, challenges to the command''s validity, and post-exit narratives of apostasy or secular Jewish identity. If substantive movements have attempted to reconstitute sacrifice or explicitly reject the command''s binding force, suppression is more structural. If the Jewish people have internalized the obligation as eternally binding, suppression is internalized.',
    'If internalized, the effective suppression is higher than the structural measure suggests — the target carries the suppression with them into secular or alternative identity frameworks. This deepens the snare classification. If structural, the suppression may be partially reversible if the physical preconditions change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural barrier or internalized acceptance.').

omega_variable(
    kernel_reading_contest_status,
    'This constraint is ONE READING of a contested kernel. Which reading best represents the halakhic mainstream? Is the performance-only reading actually the dominant interpretation in contemporary Jewish law, or is one of the sibling readings (messianic_suspension, study_as_exercise, symbolic_archive) more widely endorsed?',
    'Survey of contemporary rabbinic literature, halakhic codes, and institutional positions from major Jewish denominations and yeshivas. Establish which reading is taught as normative law and which are presented as minority or archaic positions.',
    'If the performance-only reading is mainstream, the constraint accurately represents the binding obligation and its unfulfilled status. If one of the sibling readings is mainstream, this constraint misrepresents the actual halakhic position and should be read as documenting a minority reading of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, empirical, 'The actual distribution of readings across the Jewish legal tradition.').

omega_variable(
    messianic_restoration_counterfactual,
    'Under the performance-only reading, the obligation awaits messianic restoration. Is this a substantive eschatological belief that shapes the community''s present action, or a metaphorical frame that functions as indefinite deferral?',
    'Examine whether Jewish communities act as if messianic restoration is imminent, probable, or indefinitely remote. If imminent or probable, the obligation''s binding force is conditional on an expected near-term event. If indefinitely remote, the obligation functions as perpetually binding without expectation of discharge.',
    'If messianic restoration is psychologically imminent, the obligation''s unfulfilled status may be tolerable as temporary. If indefinitely remote, the obligation becomes unbridgeable and the extractiveness is magnified by hopelessness. This feeds into the theater_ratio interpretation: theatrical performance of readiness (study, preservation of law) stands in for actual performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_restoration_counterfactual, conceptual, 'Whether messianic restoration is a lived expectation or a metaphorical deferral mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 70, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t70, projected).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement_basis(sacr_tr_t200, projected).
narrative_ontology:measurement(sacr_tr_t600, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 600, 0.45).
narrative_ontology:measurement_basis(sacr_tr_t600, projected).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1200, 0.61).
narrative_ontology:measurement_basis(sacr_tr_t1200, projected).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1900, 0.68).
narrative_ontology:measurement_basis(sacr_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 70, 0.15).
narrative_ontology:measurement_basis(sacr_be_t70, projected).
narrative_ontology:measurement(sacr_be_t200, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement_basis(sacr_be_t200, projected).
narrative_ontology:measurement(sacr_be_t600, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 600, 0.68).
narrative_ontology:measurement_basis(sacr_be_t600, projected).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1200, 0.82).
narrative_ontology:measurement_basis(sacr_be_t1200, projected).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1900, 0.89).
narrative_ontology:measurement_basis(sacr_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 70, 0.35).
narrative_ontology:measurement_basis(sacr_su_t70, projected).
narrative_ontology:measurement(sacr_su_t200, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement_basis(sacr_su_t200, projected).
narrative_ontology:measurement(sacr_su_t600, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 600, 0.62).
narrative_ontology:measurement_basis(sacr_su_t600, projected).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1200, 0.69).
narrative_ontology:measurement_basis(sacr_su_t1200, projected).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1900, 0.72).
narrative_ontology:measurement_basis(sacr_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__performance_only_reading, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_kernel, part of a constraint family of four. The performance-only reading is characterized by high extractiveness (0.89) arising from the unbridgeable gap between command and capacity; the other readings resolve or reframe the gap differently. All four readings are linked via network.affects_constraints so that contamination propagation and kernel-contest analysis can track how changes in one reading's authority affect the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
