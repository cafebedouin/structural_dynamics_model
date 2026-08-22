% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation Suspended Until Messianic Restoration (Kernel Reading: Messianic Suspension)
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   In Jewish halakhic tradition, the obligation to offer sacrifice is a
 *   binding divine commandment (mitzvah). After the destruction of the Second
 *   Temple in 70 CE, physical sacrifice became impossible. The messianic
 *   suspension reading holds that the obligation is divinely suspended (not
 *   transformed, not abolished, not substituted) pending the restoration of
 *   the Temple and messianic conditions. During the suspension, study of
 *   sacrifice law maintains operational readiness—the knowledge and
 *   practice-capacity required to perform the mitzvah when restoration comes.
 *   The present community incurs the cost of study without the benefit of
 *   performance; the future community (post-restoration) inherits the
 *   capacity to perform. This reading competes with three siblings: (1)
 *   study-as-exercise: study itself fulfills the obligation through
 *   intellectual engagement; (2) performance-only: study is merely
 *   preparatory and does not fulfill the mitzvah; (3) symbolic-archive: study
 *   preserves cultural identity and historical continuity but makes no
 *   halakhic claim to fulfill an obligation. The messianic suspension reading
 *   is distinct in that it treats the obligation as temporally
 *   partitioned—suspended in the present, executable in the future—and study
 *   as instrumental (readiness maintenance) rather than substitutive
 *   (fulfillment) or merely symbolic (identity).
 *
 * KEY AGENTS:
 *   - Contemporary Jewish community: incurs the study obligation and the intellectual/spiritual cost of maintaining it despite inability to perform the underlying mitzvah. Identity is deeply bound to halakhic authority; exit is identity_locked.
 *   - Rabbinic authority collective: interprets the suspension doctrine, enforces the study obligation, determines adequacy of readiness maintenance. Grounds authority in lineage transmission of Torah and Talmud. Extracts no material benefit from the arrangement.
 *   - Future generations post-restoration: inherit operational knowledge of sacrifice law and the capacity to perform when restoration comes. Their benefit depends on present compliance with the study obligation.
 *   - Alternative reading holders: advocate for study-as-exercise, performance-only, or symbolic-archive readings. Structured in disagreement with the suspension reading's burden and interpretation.
 *   - Analytic observer: maps the constraint from outside the faith framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation Suspended Until Messianic Restoration (Kernel Reading: Messianic Suspension)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

narrative_ontology:has_sunset_clause(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, 'e4c4d813-86cc-425b-8357-c668db6a5a0f').
narrative_ontology:cs_kernel_codification('e4c4d813-86cc-425b-8357-c668db6a5a0f', fixed_text).
narrative_ontology:cs_authority_grounding('e4c4d813-86cc-425b-8357-c668db6a5a0f', lineage).
narrative_ontology:cs_interpretation_layer_present('e4c4d813-86cc-425b-8357-c668db6a5a0f').
narrative_ontology:cs_reading_relation('e4c4d813-86cc-425b-8357-c668db6a5a0f', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4c4d813-86cc-425b-8357-c668db6a5a0f', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4c4d813-86cc-425b-8357-c668db6a5a0f', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('e4c4d813-86cc-425b-8357-c668db6a5a0f', foundational, divine_suspension_doctrine_incumbent).
narrative_ontology:cs_axiom_status(divine_suspension_doctrine_incumbent, holdable).
narrative_ontology:cs_axiom_grounding('e4c4d813-86cc-425b-8357-c668db6a5a0f', divine_suspension_doctrine_incumbent, deontological).
narrative_ontology:cs_axiom('e4c4d813-86cc-425b-8357-c668db6a5a0f', foundational, study_as_readiness_instrumental).
narrative_ontology:cs_axiom_status(study_as_readiness_instrumental, holdable).
narrative_ontology:cs_axiom_grounding('e4c4d813-86cc-425b-8357-c668db6a5a0f', study_as_readiness_instrumental, instrumental).
narrative_ontology:cs_reference_frame('e4c4d813-86cc-425b-8357-c668db6a5a0f', temple_sacrifice_obligation_codified_in_torah).
narrative_ontology:cs_drift_state('e4c4d813-86cc-425b-8357-c668db6a5a0f', post_temple_destruction_indefinite_non_restoration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e4c4d813-86cc-425b-8357-c668db6a5a0f', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_post_restoration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_jewish_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divine_suspension_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, study_as_readiness_maintenance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the obligation to study sacrifice law despite the inability to perform the underlying mitzvah. The cost is the intellectual and spiritual labor of maintaining competence in a practice that cannot be actualized in the present. Exit would require abandoning the halakhic framework that defines the reading itself.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_jewish_community, payer,
    organized, generational, identity_locked, global).

% Interprets and transmits the suspension doctrine; enforces the obligation to study as maintenance of readiness; determines what counts as adequate preparation for restoration. Authority grounds itself in lineage interpretation of the Torah and Talmud; does not extract material benefit from the arrangement.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority_collective, agenda_setter,
    institutional, civilizational, analytical, global).

% Will inherit operational knowledge of sacrifice law preserved through study in the suspension period. The present generation's compliance with study obligation ensures that when restoration comes, the capacity to perform will not have been lost to historical discontinuity. They cannot exit this arrangement; it defines their future possibility.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_post_restoration, beneficiary,
    powerless, civilizational, trapped, universal).

% Advocate for competing readings (study-as-exercise, performance-only, symbolic-archive) and would argue that the suspension reading over-burdens the present without justified benefit. They are in structured disagreement with the agenda-setter about whether the suspension doctrine is the correct halakhic interpretation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, alternative_reading_holders, excluded,
    organized, generational, constrained, global).

% Observes the constraint structure from outside the faith framework. Maps the halakhic obligation, the suspension doctrine, the study requirement, and the beneficiary structure to understand how postponement and readiness maintenance operate as a constraint on contemporary action.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, analytic_observer_secular, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves intellectual and spiritual capital — the knowledge and practice-readiness required to perform sacrifice — across a historical period when performance is impossible. Maintains collective competence in a law that cannot be executed, so that if/when restoration comes, the capacity to perform is not lost to generational discontinuity.
% TRANSFER_FUNCTION: The arrangement moves intellectual and spiritual labor from the present community (who study despite inability to perform) to future generations (who inherit operational knowledge). The present bears the cost of study; the future collects the benefit of readiness. No material transfer occurs; the transfer is of capacitive inheritance.
% ABSENT_VOICES: Competing reading holders — those who advocate that study-as-exercise fulfills the obligation, that symbolic-archive adequacy removes the burden, or that performance-only framing makes study merely preparatory — would object to the suspension reading's burden. Secular legal scholars and those skeptical of divine suspension doctrine would argue the arrangement is circular (study maintains readiness for something that may never occur) and self-perpetuating.
% DISAPPEARANCE_RATIONALE: If the suspension reading and its study obligation disappeared: one faction (study-as-exercise holders) would argue the obligation is already fulfilled; another faction (performance-only holders) would argue the obligation is violated and must await literal restoration; a third faction (symbolic-archive holders) would argue identity and cultural continuity are preserved without halakhic burden. The world does not physically rearrange, but the halakhic and spiritual status of the Jewish community would be contested and reorganized around competing frameworks.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, physical sacrifice became impossible. Yet the Torah commanded the performance of sacrifice as a binding obligation. The founding problem: how can an obligation remain in force when its performance condition is not met? The suspension reading answers: the obligation is divinely suspended (not abrogated) pending restoration; study maintains readiness across the suspension period.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities across medieval and modern halakhic tradition attest the founding problem persists (the Temple has not been restored; the obligation has not been rescinded). External corroboration comes from archaeological and historical study confirming no Second Temple reconstruction has occurred; the halakhic tradition's own internal debates (Talmudic passages, medieval Rishonim, modern Acharonim) document the problem as live and contested. Non-Jewish historians and religious scholars corroborate that the Temple destruction and the problem it posed to Jewish law is historically real.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the obligation is acknowledged as suspended, not violated; study is recognized as instrumental (maintaining capacity) rather than as extraction for its own sake. There are no identified victims during the suspension period—the contemporary community accepts the study burden as justified by future benefit. Suppression is minimal (0.08) because the arrangement is internally coherent within the reading's own authority framework; no party is coerced against their recognition of the reading's legitimacy (though alternative reading holders dispute it). Theater ratio is moderate-low (0.22) and rising over time: in the early suspension period, study functions genuinely as readiness maintenance (low theater); as centuries of non-restoration accumulate, the ceremonial and identity-maintenance aspects of study grow relative to its instrumental readiness function (rising theater). Accessibility collapse is moderate (0.35) because within the framework of the reading and the halakhic authority, alternatives appear less accessible, but outside the framework (for secular Jews, for adherents of alternative readings), alternatives remain available. Resistance is low (0.12) because the reading's own framework legitimizes the obligation; resistance comes from competing readings and those skeptical of the framework itself, not from those who accept the reading. The measurement series track a 1956-year interval from Temple destruction (year 70) to present (2026). Extractiveness shows slight decline from early period through medieval period (uncertainty about initial post-destruction response, gradual institutionalization of the reading reduces its extractive feel), then stabilization. Theater rises as performance deferral lengthens and symbolic/identity-maintenance aspects become increasingly salient relative to literal readiness. This pattern is consistent with a scaffold constraint whose primary function (maintaining readiness for restoration) gradually acquires secondary functions (identity maintenance, communal cohesion) as the deferral lengthens.
 *
 * PERSPECTIVAL GAP:
 *   Rabbinic authority (d~0.2): grounds legitimacy in lineage interpretation; maintains the reading without material extraction; sees the arrangement as genuine knowledge preservation coordination. Contemporary community (d~0.65): incurs study obligation; bears intellectual/spiritual cost; benefits indirectly through identity coherence and community standing; identity-locked so exit is existentially costly. Future generations (d~0.0): pure beneficiaries of inherited knowledge; trapped by temporal position; no decision-making agency in the arrangement. Alternative reading holders (d~0.75): excluded from canonical interpretation; bear opportunity cost of the suspension reading's dominance; their alternative readings cannot revise the obligation structure. Secular Jews with tenuous identity connection (d~0.85): bear nominal study obligation for minimal identity benefit; highest target position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: future_generations_post_restoration. They inherit operational knowledge and readiness capacity from the present generation's study compliance. Their benefit is existential and conditional on restoration, but within the reading's framework it is real. Under the suspension reading, they are the primary beneficiary because the entire rationale for the study obligation is their future capacity. Victims/payers: contemporary_jewish_community. They bear the cost of study—intellectual labor, spiritual engagement with an obligation they cannot fulfill, potential psychological tension between the command and its non-executability. They are not exploited in the classical sense (no wealth transfer, no suppression apparatus), but they do incur cost without offsetting current benefit. However, they also benefit from identity coherence, community standing, and participation in a tradition that transcends individual generation. The payer/beneficiary boundary is permeable here; they are simultaneously bearers of cost and collectors of relational good. This distinguishes the reading from pure extraction. Authority: agenda_setter. Rabbinic institutions interpret and transmit the suspension doctrine. They do not extract material benefit; their role is supervisory and transmissive. They do extract interpretive authority—the power to define what counts as adequate readiness—but this is not extractiveness in the classical sense; it is the ordinary function of authority. Under directionality derivation: authority should compute d~0.2 (powerful but not extractive in the sense of collecting from the arrangement; moderate beneficiary of maintaining authority, but this is second-order). Directionality overrides are not required; the structural derivation from beneficiary/payer + power + exit should produce the right values.
 *
 * MANDATROPHY ANALYSIS:
 *   The suspension reading is a scaffold constraint with an explicit sunset clause (the has_sunset_clause: true reflects that restoration ends the suspension). The founding problem (how to maintain an obligation during a period of non-executability) is live under the reading's own framework. The classification as scaffold correctly captures that the arrangement is temporary, transitional, justified by the condition it will end when restoration occurs. The question of mandatrophy arises if one contests whether restoration is likely or possible. Under a secularist frame that rejects messianic restoration as a live possibility, the founding problem becomes 'dead' (the condition restoration solves will never materialize) while the arrangement persists (the study obligation continues as community practice). This mismatch—dead founding problem + persistent arrangement—would flag mandatrophy. But within the reading's own halakhic framework, the founding problem remains live (restoration is doctrinally anticipated) and the arrangement is justified. The engine should flag mandatrophy only if the empirical observation of non-restoration over 1956 years combined with projections of indefinite non-restoration becomes the operating assumption. The current measurements show theater_ratio rising (from 0.08 to 0.22), which is consistent with a scaffold whose primary function (literal readiness) is increasingly overlaid with secondary functions (identity, community). If theater continues rising toward 0.5+ while extractiveness stays low, this would indicate the original rationale (literal restoration readiness) has been substantially displaced by identity-maintenance functions—a drift toward piton without the classical piton's extraction (because no party benefits materially). This is a distinct failure mode: a scaffold whose function atrophies not into extraction but into pure performance. The current measurements do not yet establish this; the trajectory is tracked for future assessment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_doctrine_empirical_grounding,
    'Is the suspension doctrine a claim about divine intention/halakhic status, or a post-hoc interpretive framework constructed to maintain rabbinic authority and community identity across an impossible condition?',
    'Genealogical analysis of Talmudic and medieval textual sources to establish whether suspension language appears as ancient doctrine or later interpretive elaboration; comparison with alternative readings'' textual grounding; analysis of whether the suspension reading provides pragmatic goods (psychological coherence, institutional stability) that might explain its adoption independent of its truth.',
    'If suspension is post-hoc construction, the reading reclassifies as identity-coordination cover story (higher extractiveness from the study obligation component, which would then serve institutional maintenance rather than future-generation benefit). If ancient doctrine, the reading''s scaffolding character is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_doctrine_empirical_grounding, empirical, 'Whether divine suspension is ancient doctrine or interpretive construction.').

omega_variable(
    future_generation_beneficiary_counterfactual,
    'If messianic restoration never occurs, do future generations genuinely benefit from inherited knowledge of sacrifice law, or does the benefit evaporate at the perpetual deferral of the promised condition?',
    'Sustained historical observation (centuries to millennia) of whether the knowledge inheritance actually preserves operational capacity across generations without execution, or whether knowledge degrades, meaning shifts, and the inherited framework becomes increasingly ceremonial or symbolic. Comparison of pre-suspension-reading and post-suspension-reading communities'' capacity to articulate sacrifice law.',
    'If benefit persists across indefinite suspension, the future-generation beneficiary role is real and extractiveness remains low. If benefit evaporates or degrades under perpetual deferral, the arrangement becomes a circular obligation (study for a benefit that depends on a condition that may never arrive) and reclassifies as identity-locking piton or snare on the present community.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_beneficiary_counterfactual, empirical, 'Whether deferred restoration preserves or erodes the beneficiary structure.').

omega_variable(
    sibling_reading_foreclosure_ambiguity,
    'Does this reading (messianic suspension) genuinely foreclose the study-as-exercise reading, or do they coexist as competing valid interpretations within Jewish tradition?',
    'Documentary evidence from rabbinic halakhic discourse: do authoritative figures explicitly rule that study-as-exercise CANNOT be true while suspension is true (foreclosure), or do they acknowledge both as live halakhic positions held by different schools (coexistence)?',
    'If foreclosure: the reading_relations entry should shift from coexists_with to forecloses. If coexistence: the current coexists_with entry is correct and reflects the pluralism of halakhic interpretation. This affects the engine''s assessment of whether the kernel permits multiple readings or enforces canonical interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_ambiguity, conceptual, 'Whether this reading logically excludes the study-as-exercise reading.').

omega_variable(
    identity_lock_mechanism_in_study_obligation,
    'Is the exit_options=''identity_locked'' for the contemporary community a structural fact (exiting the study obligation requires abandoning Jewish identity and halakhic authority) or a contingent social fact (one could leave the community without abandoning personhood)?',
    'Ethnographic and historical study of actual exit pathways: do individuals who cease studying sacrifice law retain Jewish identity and community standing, or does non-compliance trigger identity severance? Analysis of whether the identity-lock is structural-theological (the reading claims identity IS defined by halakhic obligation) or institutional-social (the community enforces identity severance).',
    'If structural, the suppression and theater values should be lower; if institutional, they should be higher. A structural identity-lock produces the ''identity_locked'' exit classification legitimately; an institutional enforcement would better classify as ''constrained'' with social coercion, raising suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_study_obligation, conceptual, 'Whether the study obligation uses identity-lock or social enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 70, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t70, projected).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 500, 0.12).
narrative_ontology:measurement_basis(sacr_tr_t500, projected).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t1000, projected).
narrative_ontology:measurement(sacr_tr_t1700, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1700, 0.18).
narrative_ontology:measurement_basis(sacr_tr_t1700, projected).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1950, 0.21).
narrative_ontology:measurement_basis(sacr_tr_t1950, observed).
narrative_ontology:measurement(sacr_tr_t2026, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(sacr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 70, 0.18).
narrative_ontology:measurement_basis(sacr_be_t70, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 500, 0.16).
narrative_ontology:measurement_basis(sacr_be_t500, projected).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.14).
narrative_ontology:measurement_basis(sacr_be_t1000, projected).
narrative_ontology:measurement(sacr_be_t1700, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1700, 0.12).
narrative_ontology:measurement_basis(sacr_be_t1700, projected).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement_basis(sacr_be_t1950, observed).
narrative_ontology:measurement(sacr_be_t2026, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2026, 0.15).
narrative_ontology:measurement_basis(sacr_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__messianic_suspension_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel constraint family consists of four readings, each instantiating a different constraint from the same kernel (Torah obligation to sacrifice). All four share the same referent (the obligation structure) but assess it under different readings' lights, producing different ε values and classifications. The messianic suspension reading (this file) treats the obligation as suspended and study as instrumental readiness (low ε, scaffold type). The study-as-exercise reading treats study as fulfillment and the obligation as occupied (lower ε, rope type). The performance-only reading treats the obligation as violated and study as merely preparatory (higher ε, snare type). The symbolic-archive reading treats the obligation as cultural history without halakhic force (very low ε, rope type). These are not perspectives on a single constraint; they are structurally distinct constraints derived from competing readings of the kernel. Each reading's ε is invariant within that reading; the sibling readings do not affect the measurement, only the classification within that reading's own frame. The network links establish that changes in one reading's authority grounding or acceptability within Jewish tradition will influence others' structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
