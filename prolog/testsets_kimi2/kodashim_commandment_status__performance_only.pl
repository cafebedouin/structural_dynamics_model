% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Temple-Contingent Sacrifice Commandment â Performance-Only Reading
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint instantiates the performance_only reading of the
 *   kodashim_commandment_status kernel. The reading holds that biblical
 *   sacrifice laws are contingent on Temple existence and are fully
 *   suspendedâreduced to a huskâwithout an operative altar. Despite this
 *   suspension, the rabbinic scholarly establishment maintains an extensive
 *   institutional apparatus devoted to studying these inoperative laws,
 *   extracting cognitive, material, and temporal resources from students and
 *   communal welfare pools. The constraint is authored as a tangled_rope
 *   because a genuine coordination function (textual preservation, communal
 *   continuity) coexists with asymmetric extraction (resource diversion to
 *   obsolete practice).
 *
 * KEY AGENTS:
 *   - torah_scholarly_establishment: Primary agenda-setter (institutional/constrained) â administers curricula and captures prestige and employment.
 *   - yeshiva_students: Primary target (powerless/identity_locked) â bear the cognitive and opportunity costs of obsolete study.
 *   - impoverished_communities: Secondary target (powerless/trapped) â pay through foregone charitable redirection.
 *   - communal_donors: Secondary target (moderate/constrained) â fund the apparatus under social obligation.
 *   - halakhic_reformers: Excluded voice (moderate/constrained) â advocates for redirection, marginalized from design.
 *   - academic_observers: Analytical observer (analytical/analytical) â external structural analysis.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.72).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.58).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.72).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Temple-Contingent Sacrifice Commandment â Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '9ed838fc-e0b0-43a0-ac5c-9d361744ef63').
narrative_ontology:cs_kernel_codification('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', fixed_text).
narrative_ontology:cs_authority_grounding('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', lineage).
narrative_ontology:cs_interpretation_layer_present('9ed838fc-e0b0-43a0-ac5c-9d361744ef63').
narrative_ontology:cs_reading_relation('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', kodashim_commandment_status__messianic_deferral, influences).
narrative_ontology:cs_axiom('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', foundational, commandment_suspended_without_temple).
narrative_ontology:cs_axiom_status(commandment_suspended_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', commandment_suspended_without_temple, conventional).
narrative_ontology:cs_axiom('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', foundational, study_does_not_fulfill_sacrifice).
narrative_ontology:cs_axiom_status(study_does_not_fulfill_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', study_does_not_fulfill_sacrifice, conventional).
narrative_ontology:cs_reference_frame('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', temple_contingent_performance_mandate).
narrative_ontology:cs_drift_state('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', contemporary_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9ed838fc-e0b0-43a0-ac5c-9d361744ef63', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, torah_scholarly_establishment).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, yeshiva_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, impoverished_communities).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, communal_donors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers yeshiva curricula, rabbinic ordination requirements, and communal prestige systems. Insists on the non-negotiable study of all Talmudic orders including Kodashim. Derives institutional authority, employment, and communal standing from expertise in the full textual corpus.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, torah_scholarly_establishment, agenda_setter,
    institutional, generational, constrained, global).

% Invest years of cognitive labor mastering intricate sacrificial regulations that the reading regards as functionally inoperative. Exit is identity-locked: leaving the curriculum or demanding practical training carries severe social and institutional penalties. They bear the direct opportunity cost of the constraint.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_students, payer,
    powerless, biographical, identity_locked, national).

% Rely on communal charitable resources that are diverted to sustain the scholarly apparatus around suspended commandments. They are not present in curriculum-design conversations and pay through foregone material support.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, impoverished_communities, payer,
    powerless, immediate, trapped, local).

% Direct philanthropic resources toward yeshiva maintenance under religious and social obligation. They cannot easily redirect funds toward live commandments or welfare without communal sanction, bearing the cost of the diverted resources.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, communal_donors, payer,
    moderate, biographical, constrained, regional).

% Advocate reallocating scholarly resources toward applied halakha and social welfare. Structurally excluded from mainstream yeshiva discourse and curriculum committees.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_reformers, excluded,
    moderate, biographical, constrained, national).

% Analyze the structural gap between the commandment's suspended status and continued resource allocation. Hold an analytical seat outside the halakhic system's internal resource economy.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, torah_scholarly_establishment).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves textual expertise and interpretive continuity in sacrificial law against potential future Temple restoration; maintains communal cohesion around the full Torah corpus.
% TRANSFER_FUNCTION: Moves cognitive labor, educational time, and communal funding from live commandments, applied ethics, and material welfare to the study of inoperative sacrificial regulations.
% ABSENT_VOICES: Impoverished community members needing redirected charitable resources, advocates for applied halakha in medicine and social justice, and students who would choose live-commandment training are structurally absent from curriculum design.
% DISAPPEARANCE_RATIONALE: If the scholarly apparatus devoted to sacrifice laws vanished, yeshiva curricula would reallocate toward applicable halakha, philanthropic resources would shift to welfare and education, and the interpretive community would reorganize around operative commandments.
% FOUNDING_PROBLEM: Preservation of Torah knowledge and priestly expertise during the Temple's destruction and exile, ensuring immediate readiness for sacrificial service upon restoration.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Jewish law attest the founding problem is functionally dead under current conditions; reform-oriented halakhic voices outside the mainstream yeshiva structure corroborate this from a non-beneficiary seat. The beneficiary scholarly establishment alone claims ongoing readiness is live, with no independent corroboration from a seat that does not share in the scholarly prestige economy.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.72) because substantial communal resourcesâstudent time, donor funds, institutional prestigeâare directed toward laws the reading considers inoperative. Suppression (0.58) reflects both structural barriers (curriculum gatekeeping) and internalized identity fusion that treats all Torah study as equally sacred, suppressing cost-benefit scrutiny. Theater ratio is elevated (0.62) because a growing share of sacrificial-law study is performative maintenance of expertise that has no practical outlet, serving institutional reproduction more than functional preservation. Accessibility collapse (0.48) is moderate: alternatives (applied halakha study, welfare redirection) are culturally thinkable but institutionally blocked. Resistance (0.32) is low because dissent is marginalized and the scholarly establishment controls the prestige economy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (scholarly establishment) experiences the constraint as preservation of a sacred textual heritageâa necessary coordination against future restoration. The payer seats (students, poor, donors) experience the same structure as resource extraction into an intellectual husk. The engine computes this divergence from the structural data: same constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The scholarly establishment is the declared beneficiary (low d, subsidized by the constraint). Students, impoverished communities, and donors are declared victims (high d, extracted from). The establishment's exit is constrained by institutional identity dependence; the students' exit is identity-locked; the poor are trapped by material dependency; donors are constrained by social obligation. High power Ã low exit for the establishment dampens its effective extraction, while low power Ã locked exit for students amplifies theirs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it carries both a live coordination function (genuine textual preservation) and identifiable asymmetric extraction (resource diversion). It is not a rope because victims exist and extraction is substantial. It is not a snare because the coordination story is not mere coverâthe preservation function is structurally real. It is not a piton because the scholarly establishment is a concentrated beneficiary that actively maintains the constraint. The tangled_rope classification captures the hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_obsolescence,
    'Does the performance_only reading correctly identify the sacrificial commandment as a husk, or does the continued study represent a live coordination function that this reading misidentifies as extraction?',
    'Cross-reading comparison of resource allocation patterns and correlation with communal welfare metrics; engine computation of axiom contradiction across the three kernel readings.',
    'If the commandment is genuinely suspended, the continued study is extractive overhead and the constraint trends toward snare; if study maintains a live function, the constraint is better classified as tangled_rope with moderate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_obsolescence, conceptual, 'Whether the performance_only reading''s obsolescence claim is structurally accurate.').

omega_variable(
    suppression_internalization_study,
    'Is the maintenance of sacrificial-law study sustained by structural institutional barriers or by internalized identity fusion that makes redirection cognitively unavailable to students and donors?',
    'Post-exit trajectory analysis of students who leave the yeshiva system; resource reallocation patterns in communities that attempt curricular reform.',
    'If suppression is primarily internalized, effective extraction exceeds structural measures and resistance to reform is higher than surface indicators suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_study, empirical, 'Structural versus internalized suppression in obsolete practice maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.35).
narrative_ontology:measurement(koda_tr_t10, kodashim_commandment_status__performance_only, theater_ratio, 10, 0.42).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__performance_only, theater_ratio, 20, 0.49).
narrative_ontology:measurement(koda_tr_t30, kodashim_commandment_status__performance_only, theater_ratio, 30, 0.55).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__performance_only, theater_ratio, 40, 0.59).
narrative_ontology:measurement(koda_tr_t50, kodashim_commandment_status__performance_only, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(koda_be_t10, kodashim_commandment_status__performance_only, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__performance_only, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(koda_be_t30, kodashim_commandment_status__performance_only, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__performance_only, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(koda_be_t50, kodashim_commandment_status__performance_only, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__performance_only, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(koda_su_t10, kodashim_commandment_status__performance_only, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__performance_only, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(koda_su_t30, kodashim_commandment_status__performance_only, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(koda_su_t40, kodashim_commandment_status__performance_only, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(koda_su_t50, kodashim_commandment_status__performance_only, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__performance_only, 0.08).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This story is one member of the kodashim_commandment_status constraint family. The kernel (biblical sacrificial law) decomposes into three structurally distinct constraints because the Îµ values differ significantly across readings: study_as_performance treats study as non-extractive fulfillment (lower Îµ), messianic_deferral treats study as provisional readiness (moderate Îµ), and performance_only treats study as resource diversion into a husk (higher Îµ). They are linked by affects_constraints but each carries its own stable referent and metrics per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
