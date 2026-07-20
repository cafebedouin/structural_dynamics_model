% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Prophetic Reinterpretation of Plural Marriage
 *   domain: religious/political
 *
 * SUMMARY:
 *   The 1890 Manifesto issued by Wilford Woodruff suspended plural marriage
 *   practice in the LDS Church while retaining the underlying doctrine as
 *   divinely instituted. This constraint story treats the Manifesto as an
 *   endogenous prophetic reinterpretationâlegitimate revelation that
 *   temporally suspends practice to preserve the church's salvific mission
 *   under federal existential threat. The reading coordinates the mainstream
 *   membership around compliance but extracts from fundamentalist dissenters
 *   who maintain the original practice and are excommunicated. It is one
 *   reading of the plural_marriage_mandate kernel, competing with
 *   exogenous_override_reading (federal coercion) and
 *   institutional_pragmatism_reading (survival legitimation).
 *
 * KEY AGENTS:
 *   - church_institution: Agenda-setter and beneficiary (institutional/generational) â issues the Manifesto, gains survival and statehood
 *   - fundamentalist_dissenters: Payer/victim (powerless/identity_locked) â excommunicated for maintaining plural marriage
 *   - us_federal_government: Observer (institutional/analytical) â applied external coercion creating the survival pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.52).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.68).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto as Prophetic Reinterpretation of Plural Marriage").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious/political").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, 'd339e41f-9eaa-49fc-b68c-0bea8412f650').
narrative_ontology:cs_kernel_codification('d339e41f-9eaa-49fc-b68c-0bea8412f650', fixed_text).
narrative_ontology:cs_authority_grounding('d339e41f-9eaa-49fc-b68c-0bea8412f650', lineage).
narrative_ontology:cs_interpretation_layer_present('d339e41f-9eaa-49fc-b68c-0bea8412f650').
narrative_ontology:cs_reading_relation('d339e41f-9eaa-49fc-b68c-0bea8412f650', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('d339e41f-9eaa-49fc-b68c-0bea8412f650', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('d339e41f-9eaa-49fc-b68c-0bea8412f650', foundational, prophetic_revelation_legitimate).
narrative_ontology:cs_axiom_status(prophetic_revelation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d339e41f-9eaa-49fc-b68c-0bea8412f650', prophetic_revelation_legitimate, theological).
narrative_ontology:cs_axiom('d339e41f-9eaa-49fc-b68c-0bea8412f650', foundational, doctrine_practice_separability).
narrative_ontology:cs_axiom_status(doctrine_practice_separability, holdable).
narrative_ontology:cs_axiom_grounding('d339e41f-9eaa-49fc-b68c-0bea8412f650', doctrine_practice_separability, theological).
narrative_ontology:cs_reference_frame('d339e41f-9eaa-49fc-b68c-0bea8412f650', prophetic_restoration_framework).
narrative_ontology:cs_drift_state('d339e41f-9eaa-49fc-b68c-0bea8412f650', post_1890_compliance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d339e41f-9eaa-49fc-b68c-0bea8412f650', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, living_prophecy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and administers the 1890 Manifesto as prophetic revelation, suspending plural marriage practice while retaining doctrinal authority over it. Gains organizational survival, restoration of seized property, statehood for Utah, resumed missionary work, and continued temple access and ordinances. The leadership is bound to the new directive by institutional and theological commitment; reversing it would fracture the church.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, agenda_setter,
    institutional, generational, constrained, national).

% Continue to regard plural marriage as a divine requirement and are excommunicated from the church for maintaining the practice after the Manifesto. Bear the costs of exclusion from salvific ordinances, community, family networks, and institutional identity. Many are geographically concentrated and theologically fused to the original doctrine, making exit from the constraint equivalent to exit from their core identity.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters, payer,
    powerless, biographical, identity_locked, regional).

% Applied coercive pressure through anti-polygamy legislation and enforcement, creating the external conditions that made church survival contingent on abandoning plural marriage practice. Observes the church's internal doctrinal maneuver from outside the revelatory framework, measuring compliance through legal and political outcomes.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, us_federal_government, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Utah-based church community around a single prophetic directive to suspend plural marriage practice, preserving institutional unity, temple access, missionary operations, and territorial political viability under existential federal pressure.
% TRANSFER_FUNCTION: Transfers compliance obligation from church leadership to membership (abandon plural marriage practice); transfers institutional membership and salvific access away from fundamentalist dissenters who maintain the original practice.
% ABSENT_VOICES: Plural wives and women in the church had no seat in the councils producing the Manifesto; fundamentalist dissenters were not present in the decision-making body; federal authorities were external to the revelatory framework and absent from its internal legitimation.
% DISAPPEARANCE_RATIONALE: Without the Manifesto, the church would likely have faced continued federal property seizure, disincorporation under the Edmunds-Tucker Act, and territorial political collapse; fundamentalist dissenters would have remained inside the institutional fold rather than being excommunicated, and the doctrinal practice would have persisted until federal destruction or schism.
% FOUNDING_PROBLEM: The church faced existential destruction under federal anti-polygamy enforcement: property confiscation, disincorporation, imprisonment of leaders, and blocked statehood, threatening its survival and ability to perform ordinances.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative records (Edmunds Act 1882, Edmunds-Tucker Act 1887) and non-Mormon historical scholarship corroborate the existential threat; the church's own historical department acknowledges the federal pressure while attributing the solution to revelation, corroborating the problem from inside the beneficiary set only. External corroboration comes from congressional records and federal court proceedings.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the coordination benefit (church survival, continued ordinances) is substantial and real, but the cost borne by fundamentalist dissentersâexcommunication and loss of salvific communityâis non-trivial. Suppression is higher (0.68) because the constraint persists through active enforcement (excommunication of post-Manifesto practitioners, especially after the 1904 Second Manifesto). Theater is moderate-low (0.30): the prophetic framing is institutionally sincere but the doctrine-practice separation generates ongoing performative maintenance. Accessibility collapse is moderate (0.60): within the faith framework, alternatives collapse once prophetic authority is accepted; outside it, alternatives remain visible. Resistance (0.55) reflects fundamentalist non-compliance and schism formation. The metrics describe an actively enforced, moderately extractive coordination mechanism; the claimed type of rope is structurally asserted by the prophetic-authority framing and is left for the engine to test against the metrics.
 *
 * PERSPECTIVAL GAP:
 *   The church_institution seat experiences the Manifesto as salvific coordination preserving the kingdom of God; the fundamentalist_dissenters seat experiences it as betrayal of divine law and violent excision from the community of the saved. The us_federal_government seat sees it as successful coercion achieving compliance. The engine computes these divergences from the same structural dataâbeneficiary status, identity-locked exit, and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   church_institution is declared in beneficiaries and holds agenda_setter role with constrained exit (institutional commitment), placing its directionality near the beneficiary end (low d). fundamentalist_dissenters are declared in victims and hold payer role with identity_locked exit, placing directionality near the target end (high d). The federal government is an analytical observer with no stake in the constraint's operation. Effective extraction is amplified for the fundamentalists and damped for the institution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâexistential federal pressureâis dead by the early 20th century, yet the constraint persists. However, it is not a piton because the church_institution continues to benefit meaningfully from the Manifesto (maintained legitimacy, expanded missionary work, mainstream integration). The R5 genealogy flags that the arrangement outlived its original survival pressure, but the ongoing coordination function prevents automatic mandatrophy reclassification. The metrics (theater_ratio 0.30) do not support a primarily performative constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_origin_ambiguity,
    'Is the 1890 Manifesto a genuine prophetic revelation (this reading), or does its authority derive from federal coercion (exogenous_override_reading) or institutional survival strategy (institutional_pragmatism_reading)?',
    'Historical-textual analysis of Woodruff''s private writings, federal correspondence, and comparative theological accounts of revelation under political pressure.',
    'If the exogenous or pragmatism readings are correct, the constraint''s type shifts from rope to tangled_rope or snare, the directionality of church_institution inverts from beneficiary to payer or agenda_setter capturing extraction, and the legitimacy of the suppression against fundamentalist_dissenters collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_origin_ambiguity, conceptual, 'Kernel contest: location of authority for the 1890 Manifesto').

omega_variable(
    fundamentalist_victim_permanence,
    'Does the endogenous reinterpretation reading generate ongoing victimization of fundamentalist_dissenters through the doctrine-practice separation, or does the rope coordination function absorb the cost over time?',
    'Longitudinal analysis of excommunication rates, schism persistence (FLDS, Centennial Park), and generational reintegration of suspended doctrines.',
    'If ongoing victimization is structural, the rope classification is undermined by sustained extraction against a trapped population; if the cost is transitional, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentalist_victim_permanence, empirical, 'Whether the victim set is transitional or permanent under this reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pm_endog_tr_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pm_endog_tr_t5, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(pm_endog_tr_t10, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(pm_endog_tr_t15, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(pm_endog_tr_t20, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(pm_endog_tr_t25, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement(pm_endog_tr_t30, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(pm_endog_be_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pm_endog_be_t5, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(pm_endog_be_t10, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(pm_endog_be_t15, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(pm_endog_be_t20, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(pm_endog_be_t25, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(pm_endog_be_t30, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pm_endog_su_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(pm_endog_su_t5, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(pm_endog_su_t10, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(pm_endog_su_t15, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(pm_endog_su_t20, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(pm_endog_su_t25, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(pm_endog_su_t30, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel decomposes into three structurally distinct constraint readings. This reading (endogenous_reinterpretation) treats the 1890 Manifesto as legitimate prophetic coordination; the exogenous_override reading treats it as federal coercion; the institutional_pragmatism reading treats it as survival-driven legitimation strategy. Each reading has a distinct epsilon, beneficiary/victim structure, and type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
