% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Study of Sacrifice Laws as Commandment Fulfillment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The destruction of the Second Temple removed the physical site for
 *   biblical sacrificial commandments. This constraint instantiates the
 *   halakhic reading that intensive study of the sacrificial laws (tractates
 *   Kodashim) is not merely preparation or commemoration but fulfills the
 *   commandment itself. The kernel remains occupied: the obligation is
 *   structurally active through intellectual engagement rather than suspended
 *   or reduced to a husk. As a rope, the arrangement coordinates continued
 *   covenantal practice across diasporic communities without extracting from
 *   any party; the near-zero metrics reflect the absence of material transfer
 *   or coercion.
 *
 * KEY AGENTS:
 *   - classical_rabbinic_authority (institutional/analytical): Agenda-setter â transmits and authorizes the study-as-fulfillment principle through halakhic codification and curriculum.
 *   - torah_study_community (organized/identity_locked): Beneficiary â receives commandment continuity and spiritual fulfillment via study; exit means leaving the normative community.
 *   - textually_excluded_practitioners (powerless/trapped): Excluded â lack access to the primary fulfillment modality; structurally silent in the tradition's discourse.
 *   - critical_historical_scholars (analytical/analytical): Observer â documents the doctrinal shift from altar to academy without normative commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.02).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.05).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.02).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Study of Sacrifice Laws as Commandment Fulfillment").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, 'd8f0d62e-cd35-4bc6-8c0c-bacb377a9af3').
narrative_ontology:cs_kernel_codification('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3', fixed_text).
narrative_ontology:cs_authority_grounding('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3', lineage).
narrative_ontology:cs_interpretation_layer_present('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3').
narrative_ontology:cs_reading_relation('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3', foundational, study_of_laws_equals_performance).
narrative_ontology:cs_axiom_status(study_of_laws_equals_performance, holdable).
narrative_ontology:cs_axiom_grounding('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3', study_of_laws_equals_performance, deontological).
narrative_ontology:cs_reference_frame('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3', temple_era_full_performance).
narrative_ontology:cs_drift_state('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3', post_temple_exilic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d8f0d62e-cd35-4bc6-8c0c-bacb377a9af3', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, torah_study_community).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, torah_study_ritual_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the halakhic framework through textual interpretation and transmission; authorizes the principle that Talmudic study of sacrificial law substitutes for altar performance; sets curriculum and religious norms across diasporic communities.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, classical_rabbinic_authority, agenda_setter,
    institutional, generational, analytical, global).

% Engages in daily study of tractates Kodashim and related texts as a form of divine service; receives spiritual continuity and commandment fulfillment without access to the Temple; exit from this practice means exiting the normative community.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, torah_study_community, beneficiary,
    organized, generational, identity_locked, global).

% Lacks literacy or institutional access to advanced Talmudic study; is formally within the covenant but cannot access the primary fulfillment modality this reading privileges; would advocate for alternative fulfillment pathways if included in the discourse.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, textually_excluded_practitioners, excluded,
    powerless, biographical, trapped, local).

% Analyze the historical development of the study-as-performance doctrine from outside the halakhic commitment system; document the shift from altar to academy without adjudicating its normative validity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, critical_historical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains active covenantal relationship to sacrificial commandments after the loss of the Temple by substituting intellectual study of the laws for physical performance, preventing the commandments from becoming obsolete or suspended.
% TRANSFER_FUNCTION: Moves the locus of religious fulfillment from the physical altar to the academy or study hall; transfers no material goods between parties.
% ABSENT_VOICES: Individuals and communities lacking textual literacy, time, or institutional access to advanced Talmudic study are structurally excluded from the primary fulfillment modality; they would advocate for alternative pathways if present.
% DISAPPEARANCE_RATIONALE: If the study-as-performance principle vanished, the community would lose its primary mechanism for active engagement with sacrificial commandments; practice would shift either toward messianic deferral or performance-only suspension, and the curricular emphasis on Kodashim would decline from divine service to antiquarian interest.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the physical and priestly infrastructure required for biblical sacrifices, creating a crisis of continuity for commandments that presuppose an altar.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sages and medieval codifiers (Maimonides, Mishneh Torah) attest the problem and the study solution from within the tradition; historians of Second Temple Judaism and critical scholars of rabbinic literature corroborate the historical rupture from outside the beneficiary set.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.02 because the constraint moves no material resource from one party to another; suppression is 0.05 because alternative readings (performance-only, messianic deferral) are not actively suppressed but coexist in the broader tradition; theater_ratio is 0.05 because study is functionally central to the community's religious life rather than performative maintenance. Accessibility_collapse is moderate (0.35) because once the study frame is adopted, literal performance alternatives recede from practical imagination, though they remain conceptually available. Resistance is low (0.10) because the reading is broadly accepted within the traditional communities that maintain it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic authority) experiences the constraint as a preserved continuity of revelation; the beneficiary seat (study community) experiences it as lived religious obligation; the excluded seat experiences it as a barrier to participation. The engine will compute low effective extraction for the first two and higher directionality for the excluded, though the absence of victim declarations keeps the structural classification benign.
 *
 * DIRECTIONALITY LOGIC:
 *   The torah_study_community is the declared beneficiary: the constraint subsidizes their access to divine service by lowering the barrier from physical altar to intellectual engagement. Classical_rabbinic_authority subsidizes its own institutional role as interpreter. The textually_excluded_practitioners are not declared victims (the prompt specifies victim set empty) and therefore do not trigger asymmetric extraction; their structural position is captured by the excluded role and the absent_voices interview.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â loss of the Temple â is still live. The arrangement is not a piton because it carries genuine coordination function (preserving covenantal practice) and is not maintained by inertia alone. It is not a snare because there is no identifiable victim. It is not a scaffold because it carries no sunset clause and is intended as steady-state. The rope classification is robust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_fulfillment_ontology,
    'Does Talmudic study of sacrifice laws constitute a substantively equivalent fulfillment of the commandment, or is it a compensatory placeholder that preserves the commandment only nominally?',
    'Phenomenological study of practitioners'' self-understanding; comparative analysis across Jewish communities with varying emphasis on Kodashim study.',
    'If compensatory only, the coordination function weakens and the constraint may drift toward piton or scaffold classification; if substantively equivalent, the rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_fulfillment_ontology, conceptual, 'Ontological status of study as fulfillment.').

omega_variable(
    exclusion_asymmetric_cost,
    'Does the study-as-performance reading impose asymmetric costs on textually excluded practitioners despite the absence of declared victims?',
    'Sociological surveys of religious fulfillment anxiety or alienation among non-literate or non-studying members of observant communities.',
    'If exclusion produces measurable harm, the victim set may need expansion and the classification could shift toward tangled_rope; if no harm is found, the zero-victim structure holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_asymmetric_cost, empirical, 'Whether textual exclusion creates hidden costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_perf_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(kodashim_study_perf_tr_t10, kodashim_commandment_status__study_as_performance, theater_ratio, 10, 0.05).
narrative_ontology:measurement(kodashim_study_perf_tr_t20, kodashim_commandment_status__study_as_performance, theater_ratio, 20, 0.05).
narrative_ontology:measurement(kodashim_study_perf_tr_t30, kodashim_commandment_status__study_as_performance, theater_ratio, 30, 0.05).
narrative_ontology:measurement(kodashim_study_perf_tr_t40, kodashim_commandment_status__study_as_performance, theater_ratio, 40, 0.05).
narrative_ontology:measurement(kodashim_study_perf_tr_t50, kodashim_commandment_status__study_as_performance, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(kodashim_study_perf_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(kodashim_study_perf_be_t10, kodashim_commandment_status__study_as_performance, base_extractiveness, 10, 0.02).
narrative_ontology:measurement(kodashim_study_perf_be_t20, kodashim_commandment_status__study_as_performance, base_extractiveness, 20, 0.02).
narrative_ontology:measurement(kodashim_study_perf_be_t30, kodashim_commandment_status__study_as_performance, base_extractiveness, 30, 0.02).
narrative_ontology:measurement(kodashim_study_perf_be_t40, kodashim_commandment_status__study_as_performance, base_extractiveness, 40, 0.02).
narrative_ontology:measurement(kodashim_study_perf_be_t50, kodashim_commandment_status__study_as_performance, base_extractiveness, 50, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_study_perf_su_t0, kodashim_commandment_status__study_as_performance, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(kodashim_study_perf_su_t10, kodashim_commandment_status__study_as_performance, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(kodashim_study_perf_su_t20, kodashim_commandment_status__study_as_performance, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(kodashim_study_perf_su_t30, kodashim_commandment_status__study_as_performance, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(kodashim_study_perf_su_t40, kodashim_commandment_status__study_as_performance, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(kodashim_study_perf_su_t50, kodashim_commandment_status__study_as_performance, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kodashim_commandment_status kernel, decomposed per the Îµ-invariance principle because the structurally distinct claims (study-as-fulfillment, temporal suspension, contingent suspension) have different Îµ values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
