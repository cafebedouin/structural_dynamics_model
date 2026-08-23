% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation Suspended Pending Messianic Restoration
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The biblical obligation to offer sacrifices in the Jerusalem Temple
 *   became impossible after the Temple's destruction in 70 CE. The
 *   messianic_suspension reading holds that the obligation is not abolished
 *   but suspended — neither fulfilled nor violated — pending messianic
 *   restoration of the Temple. Study of sacrifice law (kodshim) maintains the
 *   knowledge in waiting but does not constitute fulfillment, preparation, or
 *   occupation of the obligation. The authority structure (rabbinic lineage)
 *   defers all adjudication to the future restoration event. No current party
 *   bears the obligation; no current party extracts from it. The constraint
 *   presents as a Mountain: a fixed structural feature of the covenantal
 *   order in exile.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Agenda setter (institutional/generational/analytical) — maintains the suspension doctrine through halakhic transmission
 *   - jewish_people: Beneficiary (organized/generational/identity_locked/global) — relieved of an impossible obligation while covenantal continuity is preserved
 *   - torah_scholars: Beneficiary (moderate/biographical/constrained/global) — study sacrifice law as knowledge maintenance, not compliance
 *   - messiah: Excluded (analytical/civilizational/trapped/universal) — the future restorer who alone can reactivate the obligation
 *   - secular_observers: Observer (analytical/immediate/analytical/global) — analyze the doctrinal structure from outside the commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.02).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, mountain).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation Suspended Pending Messianic Restoration").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic/commitment_system").

domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '726f869a-c3c9-4ebd-84a3-175d1730489c').
narrative_ontology:cs_kernel_codification('726f869a-c3c9-4ebd-84a3-175d1730489c', fixed_text).
narrative_ontology:cs_authority_grounding('726f869a-c3c9-4ebd-84a3-175d1730489c', lineage).
narrative_ontology:cs_interpretation_layer_present('726f869a-c3c9-4ebd-84a3-175d1730489c').
narrative_ontology:cs_reading_relation('726f869a-c3c9-4ebd-84a3-175d1730489c', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('726f869a-c3c9-4ebd-84a3-175d1730489c', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('726f869a-c3c9-4ebd-84a3-175d1730489c', foundational, obligation_suspended_until_messianic_restoration).
narrative_ontology:cs_axiom_status(obligation_suspended_until_messianic_restoration, holdable).
narrative_ontology:cs_axiom_grounding('726f869a-c3c9-4ebd-84a3-175d1730489c', obligation_suspended_until_messianic_restoration, deontological).
narrative_ontology:cs_axiom('726f869a-c3c9-4ebd-84a3-175d1730489c', foundational, study_maintains_knowledge_not_fulfillment).
narrative_ontology:cs_axiom_status(study_maintains_knowledge_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('726f869a-c3c9-4ebd-84a3-175d1730489c', study_maintains_knowledge_not_fulfillment, deontological).
narrative_ontology:cs_reference_frame('726f869a-c3c9-4ebd-84a3-175d1730489c', messianic_suspension_framework).
narrative_ontology:cs_drift_state('726f869a-c3c9-4ebd-84a3-175d1730489c', contemporary_exile, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('726f869a-c3c9-4ebd-84a3-175d1730489c', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, jewish_people).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, torah_scholars).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, divine_decree_suspends_obligation).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_as_future_adjudication).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, study_as_knowledge_maintenance_not_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and adjudicates the suspension doctrine through halakhic lineage. Maintains that the obligation cannot be fulfilled,準備d, or substituted until messianic restoration. Collects no material extraction; legitimacy derives from faithful transmission of the suspension claim. Exit would mean abandoning the rabbinic office and its claim to represent the covenantal tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% The collective covenantal subject. The suspension relieves them of an obligation that is objectively impossible to fulfill (no Temple, no priesthood, no altar). They bear no cost, perform no compliance, and gain covenantal continuity without rupture. Exit from the covenantal framework is identity-locked — leaving the people means leaving the constraint's scope entirely.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, jewish_people, beneficiary,
    organized, generational, identity_locked, global).

% Engage in intensive study of kodshim (sacrificial law) as maintenance of knowledge-in-waiting. This study is not compliance, not preparation, not occupation — it preserves the technical knowledge for the restoration moment. They gain intellectual purpose and communal status; the cost is opportunity cost of study time, but exit (changing specialization) is constrained by professional identity and communal expectation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, torah_scholars, beneficiary,
    moderate, biographical, constrained, global).

% The future restorer whose arrival reactivates the obligation. Not a participant in the current constraint — the constraint's structure is defined by his absence. He cannot exit the role because the role is defined by the constraint's reactivation condition; if he came, the constraint would dissolve into active obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messiah, excluded,
    analytical, civilizational, trapped, universal).

% Analyze the doctrinal structure from outside the commitment. They see a religious community maintaining continuity through a suspended obligation. They neither collect nor pay; they observe the constraint's operation as a social fact.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, secular_observers, observer,
    analytical, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains covenantal continuity across the catastrophe of Temple destruction by declaring the obligation suspended rather than abolished — preserving the obligation's force while acknowledging its current impossibility. Coordinates the community around a shared future orientation (messianic restoration) rather than a failed present performance.
% TRANSFER_FUNCTION: Moves nothing in the present epoch. All transfer (sacrificial offerings, priestly portions, altar service) is deferred to the messianic future. The constraint is a temporal bridge: it holds the obligation's place without demanding its execution.
% ABSENT_VOICES: Proponents of study_as_occupation (who argue study fulfills the obligation now), early Christian movements (who claimed the obligation was fulfilled/superseded in Christ), reform/haskalah voices (who argued the obligation was obsolete), and contemporary Temple activists (who argue the obligation can be partially reactivated now on the Temple Mount). These voices are structurally excluded from the halakhic conversation that maintains the suspension doctrine.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine vanished overnight, the obligation would revert to active but unfulfillable — creating a halakhic crisis where every Jew is in violation of a core covenantal command with no means of compliance. The community would either fracture (some attempting sacrifices, some abandoning the obligation, some declaring it abolished) or require an immediate new doctrinal resolution. The world rearranges because the suspension is the only structure holding the obligation in abeyance.
% FOUNDING_PROBLEM: How to maintain the covenantal obligation of Temple sacrifice when the Temple's destruction (70 CE) made fulfillment objectively impossible, without abolishing the obligation or declaring the covenant broken.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of rabbinic response to the destruction (Mishnah, Tosefta, Talmud) — multiple independent traditions record the suspension doctrine as the consensus resolution. Josephus (non-rabbinic, non-beneficiary) documents the destruction and the cessation of sacrifice. Christian polemicists (adversarial witnesses) confirm the rabbinic claim that sacrifice is suspended, not abolished. The problem remains live because the Temple has not been rebuilt and the messianic restoration has not occurred.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because no transfer occurs — the obligation is structurally inert in the present. Suppression is minimal (0.05) because no enforcement is needed; the suspension is accepted as the covenantal reality. Theater ratio is low (0.08) — study of kodshim is genuine intellectual engagement, not performance of compliance. Accessibility collapse is very high (0.92) — once the suspension doctrine is accepted, no alternative (rebuilding the Temple now, offering sacrifices elsewhere) is halakhically viable. Resistance is near-zero (0.03) — the constraint meets no active resistance because it demands nothing of anyone now. The metrics are stable across the interval because the structural situation (Temple destroyed, Messiah not come) is unchanged.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the constraint is a Mountain — the suspension is the covenantal reality, not a human choice. From a secular observer seat, it may appear as a constructed deferral that preserves authority. The engine computes per-seat types from the structural data; the authored claim (mountain) reflects the reading's own self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic authority sits at d ≈ 0.1 (near beneficiary) — it administers a constraint that extracts nothing and requires no enforcement, preserving its interpretive role without cost. The Jewish people sit at d ≈ 0.0 (full beneficiary) — they are relieved of an impossible obligation. Scholars sit at d ≈ 0.2 (mild beneficiary) — they gain intellectual purpose without burdensome compliance. The messiah is structurally excluded (d undefined) — the constraint's reactivation condition. No payer seat exists in the current epoch, which is why extractiveness is near-zero.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain covenantal obligation when fulfillment is impossible) remains live — the Temple is still destroyed, exile continues. The suspension arrangement has not atrophied; it remains the active halakhic resolution. No mandatrophy resolution is declared because the problem the constraint was built to solve persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is the messianic_suspension reading of the temple_sacrifice_obligation kernel. How would the sibling readings (study_as_occupation, study_as_archiving) structurally alter the beneficiary/victim sets and extractiveness profile?',
    'Author separate constraint stories for each sibling reading per ε-invariance principle; compare their base_properties.extractiveness, beneficiaries, victims, and cs_structure.axioms. The kernel family is linked via network.affects_constraints.',
    'If study_as_occupation instantiates a constraint with non-zero extractiveness (study as costly performance) and victims (those pressured to study), the kernel contains both mountain and extractive readings. If study_as_archiving shows zero extractiveness but different authority_grounding, the family maps a pure coordination vs. authority-maintenance split.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Commitment-system kernel with three contested readings; this story instantiates only the messianic_suspension reading.').

omega_variable(
    authority_benefit_from_suspension_narrative,
    'Does the rabbinic authority structure extract narrative/legitimacy benefit from maintaining the suspension narrative, even with zero material extraction?',
    'Trace institutional resource flows (funding, allegiance, interpretive monopoly) to the suspension doctrine; compare with counterfactual where obligation is declared fulfillable now.',
    'If authority captures legitimacy rents from the suspension claim, effective extractiveness for the authority seat may be non-zero despite base_properties.extractiveness ≈ 0. Would reclassify authority seat from mountain toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_benefit_from_suspension_narrative, empirical, 'Whether zero material extraction masks narrative/legitimacy extraction by the agenda-setting authority.').

omega_variable(
    suspension_as_natural_law_vs_constructed_deferral,
    'Is the suspension a genuine natural-law constraint (Temple destruction makes fulfillment objectively impossible) or a constructed halakhic deferral that could be revoked by sufficient authority?',
    'Analyze whether any halakhic authority in the lineage has ever claimed power to reactivate the obligation pre-messianically; examine the logical structure of the suspension claim (divine decree vs. rabbinic enactment).',
    'If constructed, the mountain claim is a false summit (FSM candidate) — beneficiaries would be the authority structure that avoids the crisis of an unfulfillable obligation. If natural-law, the mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_as_natural_law_vs_constructed_deferral, conceptual, 'Natural-law vs. constructed-deferral ambiguity for a Mountain constraint with authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t70, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 70, 0.05).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.06).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.07).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.07).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t1800, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t2024, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t70, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 70, 0.02).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.02).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.02).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t1800, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1800, 0.02).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t2024, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2024, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t70, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 70, 0.03).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 500, 0.04).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t1000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1000, 0.04).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t1500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t1800, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t2024, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__messianic_suspension, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint, study_as_occupation, and study_as_archiving form a constraint family decomposing the temple_sacrifice_obligation kernel. Each reading instantiates a distinct constraint with different extractiveness profiles, beneficiary/victim sets, and axiom structures. This reading (messianic_suspension) has near-zero extractiveness and no victims; study_as_occupation may show non-zero extractiveness (study as costly performance) with pressured students as victims; study_as_archiving likely mirrors this reading's low extractiveness but with different authority_grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
