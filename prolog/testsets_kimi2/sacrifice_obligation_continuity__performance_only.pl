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
 *   human_readable: Sacrifice Obligation: Performance-Only Reading
 *   domain: religious/ritual/textual
 *
 * SUMMARY:
 *   This constraint instantiates the performance_only reading of the
 *   sacrifice_obligation_continuity kernel in Rabbinic Judaism. After the
 *   destruction of the Second Temple, the biblical commandment to offer
 *   sacrifices became physically impossible. This reading maintains that the
 *   obligation remains fully binding, that study of sacrificial law is merely
 *   preparatory for a future restoration, and that the current generation
 *   therefore lives in a state of unresolved guilt and suspended compliance.
 *   The structural asymmetry is sharp: the rabbinic scholarly class
 *   administers the unfulfillable obligation and derives institutional
 *   authority from its custodianship, while the observant community bears the
 *   normative burden without remedy. The claim is tangled_rope because the
 *   arrangement genuinely coordinates intergenerational preservation of a
 *   complex textual tradition, but the same structure extracts compliance and
 *   guilt from those who cannot escape the obligation.
 *
 * KEY AGENTS:
 *   - Rabbinic scholars: agenda-setter and beneficiary (institutional/analytical) â administer the interpretation and sustain the obligation's normative force.
 *   - Observant community: payer (organized/identity_locked) â bear the unfulfillable obligation and the guilt of non-performance.
 *   - Study-fulfillment proponents: excluded (organized/constrained) â hold the sibling reading that study discharges the obligation; structurally silenced within this framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.82).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.78).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious/ritual/textual").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8').
narrative_ontology:cs_kernel_codification('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', fixed_text).
narrative_ontology:cs_authority_grounding('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', lineage).
narrative_ontology:cs_interpretation_layer_present('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8').
narrative_ontology:cs_reading_relation('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', sacrifice_obligation_continuity__messianic_suspension, influences).
narrative_ontology:cs_reading_relation('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_axiom('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', foundational, ritual_act_requires_physical_performance).
narrative_ontology:cs_axiom_status(ritual_act_requires_physical_performance, holdable).
narrative_ontology:cs_axiom_grounding('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', ritual_act_requires_physical_performance, theological).
narrative_ontology:cs_axiom('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', foundational, textual_engagement_cannot_substitute_ritual).
narrative_ontology:cs_axiom_status(textual_engagement_cannot_substitute_ritual, holdable).
narrative_ontology:cs_axiom_grounding('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', textual_engagement_cannot_substitute_ritual, theological).
narrative_ontology:cs_reference_frame('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', temple_era_normative_structure).
narrative_ontology:cs_drift_state('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', post_destruction_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('4eb0a8e7-bf54-4be6-b7f2-eb2d6ec5d1f8', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, rabbinic_scholars).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, observant_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They administer the halakhic ruling that biblical sacrifice requires physical performance and that study of sacrificial law is preparation for a future Temple, not present fulfillment. They design curricula, issue responsa, and maintain the textual apparatus that sustains the obligation's normative force. Their institutional authority and communal role are validated by being the sole custodians of an unfulfillable commandment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_scholars, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__performance_only, rabbinic_scholars, beneficiary).

% They remain halakhically obligated to bring sacrifices they cannot perform due to the Temple's absence. They are directed to study sacrificial law as a placeholder, but this study does not discharge the obligation or the guilt of its non-fulfillment. Their compliance is enforced through communal education, liturgical petition, and identity-bound norms; exit would require abandoning religious identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, observant_community, payer,
    organized, biographical, identity_locked, global).

% They hold that Torah study of sacrifice law fulfills the commandment in the current era. This position is recognized in competing readings of the same kernel but is structurally excluded from legitimacy within the performance_only framework, which treats such claims as mistaken relaxations of an absolute physical requirement.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, study_fulfillment_proponents, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, rabbinic_scholars).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed textual and procedural knowledge of sacrificial law across generations so that the practice can be restored immediately upon rebuilding the Temple, maintaining intergenerational continuity of an interrupted ritual tradition.
% TRANSFER_FUNCTION: Moves unresolved obligation and guilt from the current generation to an indefinite future by declaring study a mere placeholder; transfers interpretive authority and institutional centrality to the rabbinic scholarly class as the sole managers of the unfulfillable commandment.
% ABSENT_VOICES: Proponents of the study-as-performance reading, messianic-suspension jurists, and secular historians who would argue for ritual suspension, archival non-normativity, or doctrinal revision are excluded from the halakhic conversation within this framework; their absence is what permits the unfulfillable obligation to persist as a live normative demand.
% DISAPPEARANCE_RATIONALE: If the performance-only requirement disappeared, the observant community's unresolved guilt would be discharged, the rabbinic class would lose its distinctive custodianship of the unfulfillable commandment, and competing readings that permit study as fulfillment or messianic suspension would become normatively dominant within the tradition.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the physical site and priestly infrastructure for biblical sacrifice, threatening the continuity of a central commandment and the normative framework built around it.
% FOUNDING_PROBLEM_CORROBORATION: The observant community (payer seat) corroborates that the founding problem is live in their ongoing experience of unfulfilled obligation. Academic historians and liberal religious movements outside the rabbinic beneficiary structure attest that the Temple's destruction is a settled historical condition and that the persistence of the obligation is a constructed institutional choice rather than an unresolved crisis.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.82) is high because the constraint imposes a divine obligation that cannot be physically discharged, generating permanent guilt and compulsive study without satisfaction. Suppression (0.78) is high because the framework actively excludes alternative readings through halakhic boundary maintenance and identity-locking. Theater_ratio (0.50 at interval end) is moderate-high: the study is genuinely educational, but an increasing share of the activity performs the maintenance of rabbinic authority and the simulation of future readiness rather than present discharge. Accessibility_collapse (0.80) is high because once one accepts the theological premise and the rabbinic interpretive framework, the alternative readings become nearly inaccessible. Resistance (0.40) is moderate: competing readings and modern historical scholarship offer external resistance, but within the bounded community the constraint faces little internal opposition. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic seat, the constraint is a tragic but necessary coordination mechanism preserving a sacred tradition across catastrophe; from the observant community's seat, it is an extractive structure that maintains guilt without offering relief. The engine computes this divergence from the structural data: same constraint, same metrics, different directionalities produce different per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars sit near the beneficiary end (low d): they define the constraint, control its interpretation, and their institutional role is subsidized by the necessity of their custodianship. The observant community sits near the target end (high d): they are the ones from whom the unfulfilled obligation extracts guilt and compliance. Study-fulfillment proponents are excluded from the directionality computation (excluded role) but their structural suppression is what keeps the constraint's accessibility_collapse high. The engine will compute low effective extraction for scholars and high effective extraction for the observant community.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preserving sacrificial law after the Temple's destruction â is contested. The arrangement has persisted for two millennia, far longer than any transitional crisis would warrant. The temporal measurements show rising theater_ratio and stable high extractiveness, suggesting that the coordination function (preservation) has atrophied into a performance of readiness while the extraction function (guilt, authority) has remained constant. This prevents mislabeling the constraint as pure coordination (Rope) because the victims are structurally integral, and prevents mislabeling it as pure extraction (Snare) because the preservation function is genuine. Tangled_rope captures the hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the performance_only reading of kernel sacrifice_obligation_continuity; does the study of sacrificial law constitute fulfillment of the commandment, or merely preparation for an undefined future restoration?',
    'Halakhic consensus emerging from a recognized legislative body or messianic restoration providing the physical conditions for sacrifice; if study is ruled sufficient, the victim set empties and extractiveness collapses.',
    'If the sibling study_as_performance reading is adopted, the constraint reclassifies toward rope; if this reading persists without resolution, the high extractiveness and victim set remain structurally locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity between kernel readings: fulfillment versus preparation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of the unfulfillable obligation sustained primarily by internalized guilt and identity fusion, or by institutional and communal enforcement?',
    'Post-exit trajectory analysis: if agents who leave the observant community continue to experience guilt and obligation, suppression is partially internalized; if the burden lifts immediately upon exit, suppression is structural.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s hold on the payer seat is stronger than visible enforcement suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in religious obligation').

omega_variable(
    mandate_obsolescence,
    'Has the coordination function of preserving sacrificial knowledge been achieved, leaving only the extractive function of authority maintenance and guilt generation?',
    'Independent assessment of textual transmission completeness versus the institutional resources still devoted to the obligation''s active administration.',
    'If the coordination function is complete, the constraint approaches piton or snare; if genuine new preservation work remains, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence, empirical, 'Whether the founding coordination problem is solved while the arrangement persists').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_only_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perf_only_tr_t400, sacrifice_obligation_continuity__performance_only, theater_ratio, 400, 0.26).
narrative_ontology:measurement(perf_only_tr_t800, sacrifice_obligation_continuity__performance_only, theater_ratio, 800, 0.32).
narrative_ontology:measurement(perf_only_tr_t1200, sacrifice_obligation_continuity__performance_only, theater_ratio, 1200, 0.4).
narrative_ontology:measurement(perf_only_tr_t1600, sacrifice_obligation_continuity__performance_only, theater_ratio, 1600, 0.46).
narrative_ontology:measurement(perf_only_tr_t2000, sacrifice_obligation_continuity__performance_only, theater_ratio, 2000, 0.5).

% Extraction over time
narrative_ontology:measurement(perf_only_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(perf_only_be_t400, sacrifice_obligation_continuity__performance_only, base_extractiveness, 400, 0.66).
narrative_ontology:measurement(perf_only_be_t800, sacrifice_obligation_continuity__performance_only, base_extractiveness, 800, 0.71).
narrative_ontology:measurement(perf_only_be_t1200, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1200, 0.76).
narrative_ontology:measurement(perf_only_be_t1600, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1600, 0.8).
narrative_ontology:measurement(perf_only_be_t2000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2000, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(perf_only_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perf_only_su_t400, sacrifice_obligation_continuity__performance_only, suppression_requirement, 400, 0.56).
narrative_ontology:measurement(perf_only_su_t800, sacrifice_obligation_continuity__performance_only, suppression_requirement, 800, 0.63).
narrative_ontology:measurement(perf_only_su_t1200, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1200, 0.69).
narrative_ontology:measurement(perf_only_su_t1600, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1600, 0.74).
narrative_ontology:measurement(perf_only_su_t2000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2000, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__performance_only, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_continuity kernel. The kernel decomposes into four structurally distinct constraints because the same textual tradition generates different epsilon values, victim sets, and classification types depending on whether study counts as fulfillment, the obligation is suspended, or the law is treated as archival memory. Each reading has its own constraint_id and they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
