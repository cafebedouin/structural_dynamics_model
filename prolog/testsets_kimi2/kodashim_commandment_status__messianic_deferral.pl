% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status â Messianic Deferral Reading
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The commandment to offer sacrifices (kodashim) in the Jerusalem Temple is
 *   a fixed textual kernel in the Torah. Following the Temple's destruction
 *   in 70 CE, rabbinic Judaism developed competing readings of this kernel's
 *   status. This constraint instantiates the messianic_deferral reading: the
 *   commandment is not obsolete but temporarily suspended, and intensive
 *   study of sacrificial law maintains communal readiness for restoration.
 *   The reading generates moderate extractiveness by directing present
 *   communal resources toward a messianic future while subordinating
 *   immediate welfare needs. The constraint operates as a tangled_rope: it
 *   genuinely coordinates intergenerational knowledge preservation while
 *   asymmetrically extracting from the present lay community.
 *
 * KEY AGENTS:
 *   - rabbinic_authority (institutional/identity_locked): Sets the Halakhic curriculum and rules of deferral; derives legitimacy from Talmudic lineage
 *   - yeshiva_networks (organized/constrained): Receive funding and scholars to maintain sacrificial law study; institutional survival depends on the deferral narrative
 *   - lay_community (moderate/identity_locked): Funds yeshivas and bears opportunity cost of subordinated present needs; exit blocked by theological identity fusion
 *   - liberal_denominations (moderate/mobile): Excluded voices who reject the deferral framework and treat sacrifice laws as historically concluded
 *   - academic_observers (analytical/analytical): Observe the sociological function without Halakhic commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.45).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.55).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status â Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'cb4ac593-ce68-4782-bf3a-a684e9a5215a').
narrative_ontology:cs_kernel_codification('cb4ac593-ce68-4782-bf3a-a684e9a5215a', fixed_text).
narrative_ontology:cs_authority_grounding('cb4ac593-ce68-4782-bf3a-a684e9a5215a', lineage).
narrative_ontology:cs_interpretation_layer_present('cb4ac593-ce68-4782-bf3a-a684e9a5215a').
narrative_ontology:cs_reading_relation('cb4ac593-ce68-4782-bf3a-a684e9a5215a', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('cb4ac593-ce68-4782-bf3a-a684e9a5215a', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_axiom('cb4ac593-ce68-4782-bf3a-a684e9a5215a', foundational, messianic_restoration_mandates_readiness).
narrative_ontology:cs_axiom_status(messianic_restoration_mandates_readiness, holdable).
narrative_ontology:cs_axiom_grounding('cb4ac593-ce68-4782-bf3a-a684e9a5215a', messianic_restoration_mandates_readiness, theological).
narrative_ontology:cs_axiom('cb4ac593-ce68-4782-bf3a-a684e9a5215a', foundational, study_preserves_obligation_without_discharge).
narrative_ontology:cs_axiom_status(study_preserves_obligation_without_discharge, holdable).
narrative_ontology:cs_axiom_grounding('cb4ac593-ce68-4782-bf3a-a684e9a5215a', study_preserves_obligation_without_discharge, conventional).
narrative_ontology:cs_reference_frame('cb4ac593-ce68-4782-bf3a-a684e9a5215a', temple_cult_operational).
narrative_ontology:cs_drift_state('cb4ac593-ce68-4782-bf3a-a684e9a5215a', contemporary_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cb4ac593-ce68-4782-bf3a-a684e9a5215a', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, yeshiva_networks).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, lay_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines the Halakhic curriculum and rules that study of kodashim maintains the commandment in a state of readiness. Administers the deferral framework, authorizes which texts are studied, and rules on the legitimacy of alternative framings. Derives institutional legitimacy from continuity with the Talmudic lineage.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinic_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Receive communal funding, student enrollment, and state subsidies to maintain study of Temple tractates. Their institutional existence and resource flows depend on the framing that sacrificial law study is a valid and necessary religious occupation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, yeshiva_networks, beneficiary,
    organized, generational, constrained, national).

% Funds yeshiva institutions through communal donations and, in some polities, through tax-like allocation. Subordinates present social welfare needs to the support of Torah study framed as messianic preparation. Social and theological identity fusion makes open rejection of the framework personally and communally costly.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, lay_community, payer,
    moderate, biographical, identity_locked, global).

% Reject the deferral framework and treat sacrificial commandments as historically concluded rather than suspended. They are structurally absent from Halakhic ruling bodies and would argue for present-focused ethical obligation over indefinite messianic preparation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, liberal_denominations, excluded,
    moderate, biographical, mobile, global).

% Study the sociological and historical function of the deferral mechanism without participating in its Halakhic legitimacy claims or bearing its costs.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, yeshiva_networks).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves intergenerational continuity of sacrificial law across the destruction of the Second Temple, ensuring that if the Temple is rebuilt, the technical knowledge and institutional memory required to resume the commandments survives.
% TRANSFER_FUNCTION: Moves communal resourcesâfunding, scholarly labor, educational attention, and social prestigeâfrom present-generation welfare and other Torah studies toward the maintenance of sacrificial law expertise, justified by future messianic contingency.
% ABSENT_VOICES: Liberal Jewish denominations and secular critics who treat the sacrificial commandments as historically obsolete; marginalized community members whose immediate material needs compete with yeshiva funding; voices arguing that present ethical obligation should take precedence over indefinitely deferred messianic preparation.
% DISAPPEARANCE_RATIONALE: If the deferral framework vanished, yeshiva curricula would drop Temple tractates, scholarly labor would shift to other legal or practical domains, communal funding would redirect toward present welfare, and the intergenerational chain of sacrificial expertise would likely break within two generations.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE created a crisis of Halakhic continuity: how do Jewish communities remain faithful to Torah commandments that require a centralized sacrificial altar no longer extant?
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and archaeologists corroborate the historical founding event (Temple destruction). The specific response of study-as-readiness is attested primarily by the rabbinic and scholarly beneficiaries themselves; no significant corroboration exists from outside the benefiting parties for the deferral mechanism, though the historical rupture itself is universally attested.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint redirects substantial communal resources from present welfare to messianic preparation, while preserving a genuine coordination function (intergenerational textual continuity). Suppression (0.55) is moderate: alternatives exist in adjacent communities but are suppressed within Orthodox Halakhic discourse through social and theological coercion. Theater_ratio (0.30) acknowledges that a meaningful fraction of study performs readiness for a restoration deferred for two millennia, yet the preservation function is not entirely performative. Accessibility_collapse (0.50) reflects that alternatives collapse within the Orthodox framework though they flourish outside it. Resistance (0.40) is present from secular and liberal Jewish voices but contained within the Halakhic community.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority and yeshiva networks experience the constraint as a sacred duty of preservation and a legitimate response to historical catastrophe; the lay community experiences it as a persistent resource drain and a subordination of present needs to an indefinitely deferred future. The engine computes this divergence from the structural data: agenda_setter and beneficiary seats face directionality near the subsidy end, while the payer seat with identity-locked exit sits near the full-target end.
 *
 * DIRECTIONALITY LOGIC:
 *   The lay_community is declared victim (payer): they fund the system and bear opportunity cost without receiving the primary benefit of restored sacrifices, and their identity_locked exit amplifies effective extraction. The yeshiva_networks are declared beneficiary: they receive funding and institutional purpose, with constrained exit modulating directionality toward the beneficiary end. The rabbinic_authority is the agenda_setter: they administer the constraint and derive authority from it, structurally fused with the arrangement through identity_locked exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe destruction of the Temple and the rupture in cultic practiceâis historically real. However, the specific solution of study-as-readiness risks mandatrophy if the messianic horizon recedes indefinitely. Temporal measurements show slowly rising theater_ratio over two millennia, signaling increasing performative maintenance relative to functional restoration probability. The constraint avoids piton classification because the coordination function remains genuine and identifiable beneficiaries (yeshiva networks) actively maintain it. It is not a snare because the coordination is not merely cover: the knowledge would indeed be lost without this institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_natural_or_constructed,
    'Is the commandment''s suspension an intrinsic feature of the textual kernel (the law was always contingent on Temple existence), or a constructed rabbinic response to historical catastrophe?',
    'Textual source criticism of Pentateuchal codes and Talmudic sugyot to determine if contingency is original or retroactive.',
    'If constructed, the deferral mechanism is a rabbinic innovation with extractive potential; if natural, the constraint is closer to a textual mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_natural_or_constructed, conceptual, 'Whether suspension is intrinsic to the kernel or a rabbinic construction').

omega_variable(
    messianic_imminence_uncertainty,
    'Does the expectation of imminent messianic restoration inflate the present opportunity cost, and would distant or absent messianism collapse the justification for study?',
    'Sociological measurement of messianic belief intensity correlated with yeshiva resource allocation across communities.',
    'If messianism is weak, the constraint functions more as a piton (inertial maintenance); if strong, as a tangled_rope with genuine future-oriented coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_imminence_uncertainty, empirical, 'Messianic belief intensity as a structural support for the deferral').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (economic dependency on yeshiva institutions, social excommunication) or internalized (theological conviction that exit means apostasy)?',
    'Post-exit trajectory study of individuals who leave the Orthodox community: do they continue to feel bound by the deferral framework?',
    'If internalized, effective suppression is higher than structural measure suggests, increasing the constraint''s extractive asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__messianic_deferral, theater_ratio, 500, 0.1).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__messianic_deferral, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__messianic_deferral, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(koda_tr_t1800, kodashim_commandment_status__messianic_deferral, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__messianic_deferral, theater_ratio, 2000, 0.3).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 500, 0.25).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1000, 0.35).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1500, 0.4).
narrative_ontology:measurement(koda_be_t1800, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 2000, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(koda_su_t500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 500, 0.45).
narrative_ontology:measurement(koda_su_t1000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(koda_su_t1500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement(koda_su_t1800, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1800, 0.5).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 2000, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kodashim_commandment_status kernel. It differs from study_as_performance in treating study as instrumental readiness rather than present fulfillment, and from performance_only in imposing an active duty of study rather than mere suspension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
