% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Sovereign Authority over End-of-Life Decisions (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy reading' of end-of-life
 *   decision-making, asserting that competent individuals possess sovereign
 *   authority over their own death. It is a contested ethical and legal
 *   claim. When this authority is denied, it leads to significant extraction
 *   (prolonged suffering) for those seeking to exercise it. The constraint
 *   functions as a Tangled Rope: it coordinates individual will with medical
 *   practice, but the denial mechanisms (legal prohibitions, institutional
 *   policies, individual conscientious objections) create asymmetric
 *   extraction. The metrics reflect the ongoing struggle to establish and
 *   protect this autonomy, with gradual but slow progress in reducing
 *   extractiveness and suppression as more jurisdictions legalize forms of
 *   assisted dying.
 *
 * KEY AGENTS:
 *   - competent_individuals_seeking_eold: Primary beneficiary (if granted), Payer (if denied) — bears prolonged suffering
 *   - competent_individuals_denied_eold: Primary victim — bears prolonged suffering and loss of autonomy
 *   - facilitating_healthcare_professionals: Beneficiary — aligns with patient autonomy
 *   - sanctity_of_life_advocates: Excluded — their core premise contradicts this reading
 *   - legislators_and_policymakers: Agenda setter — determines legal frameworks
 *   - conscientious_objectors_hcp: Payer — bears cost of moral conflict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.65).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.75).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Sovereign Authority over End-of-Life Decisions (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '6916a868-25d1-4a16-a23b-d3532695d1ec').
narrative_ontology:cs_kernel_codification('6916a868-25d1-4a16-a23b-d3532695d1ec', formalized).
narrative_ontology:cs_authority_grounding('6916a868-25d1-4a16-a23b-d3532695d1ec', practice).
narrative_ontology:cs_interpretation_layer_present('6916a868-25d1-4a16-a23b-d3532695d1ec').
narrative_ontology:cs_reading_relation('6916a868-25d1-4a16-a23b-d3532695d1ec', end_of_life_decision_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('6916a868-25d1-4a16-a23b-d3532695d1ec', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('6916a868-25d1-4a16-a23b-d3532695d1ec', foundational, individual_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(individual_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('6916a868-25d1-4a16-a23b-d3532695d1ec', individual_autonomy_is_paramount, deontological).
narrative_ontology:cs_axiom('6916a868-25d1-4a16-a23b-d3532695d1ec', secondary, suffering_is_not_to_be_prolonged).
narrative_ontology:cs_axiom_status(suffering_is_not_to_be_prolonged, holdable).
narrative_ontology:cs_axiom_grounding('6916a868-25d1-4a16-a23b-d3532695d1ec', suffering_is_not_to_be_prolonged, instrumental).
narrative_ontology:cs_reference_frame('6916a868-25d1-4a16-a23b-d3532695d1ec', individual_self_determination).
narrative_ontology:cs_drift_state('6916a868-25d1-4a16-a23b-d3532695d1ec', contemporary_legal_challenges, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6916a868-25d1-4a16-a23b-d3532695d1ec', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_individuals_seeking_eold).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, facilitating_healthcare_professionals).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, competent_individuals_denied_eold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, conscientious_objectors_hcp).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who, with full mental capacity, wish to exercise control over the timing and manner of their death to avoid prolonged suffering or loss of dignity. They benefit when their autonomy is respected and facilitated.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_individuals_seeking_eold, beneficiary,
    moderate, immediate, constrained, local).

% Individuals whose requests to exercise end-of-life authority are denied due to legal, institutional, or individual conscientious objections, leading to prolonged suffering and loss of autonomy. They bear the direct cost of the constraint's denial.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_individuals_denied_eold, payer,
    powerless, immediate, trapped, local).

% Medical practitioners who ethically align with patient autonomy and are willing to facilitate end-of-life decisions within legal and professional guidelines. They benefit from clarity and legal protection in upholding patient wishes.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, facilitating_healthcare_professionals, beneficiary,
    organized, biographical, constrained, national).

% Groups and individuals who believe human life has intrinsic value independent of individual will, and that intentional life-ending is morally wrong. Their core premise is excluded from the 'autonomy reading's' foundational claims.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, sanctity_of_life_advocates, excluded,
    organized, generational, analytical, global).

% Government bodies and officials responsible for creating and amending laws and policies regarding end-of-life care, including the legality and regulation of assisted dying. They mediate between competing ethical frameworks.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legislators_and_policymakers, agenda_setter,
    institutional, generational, mobile, national).

% Healthcare professionals who, for moral or religious reasons, object to participating in end-of-life procedures that intentionally hasten death. They bear a cost if forced to participate or if their professional options are limited by their objection.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, conscientious_objectors_hcp, payer,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the competent individual's will regarding their death with the medical system's capacity to facilitate it, ensuring respect for patient autonomy and dignity at the end of life.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority over one's death from external authorities (state, medical system, family) to the competent individual. When this authority is denied, it transfers prolonged suffering and loss of control to the individual.
% ABSENT_VOICES: Sanctity-of-life advocates and some religious institutions are structurally excluded from the core premise of absolute individual sovereignty over death, as their foundational beliefs directly contradict it. They would argue for limits based on the intrinsic value of life.
% DISAPPEARANCE_RATIONALE: If this authority vanished overnight, competent individuals would lose a fundamental right to self-determination at the end of life. This would lead to increased suffering for those facing terminal illness, a shift in medical ethics towards paternalism or external control, and a reorganization of legal frameworks to explicitly deny this autonomy.
% FOUNDING_PROBLEM: The historical and ongoing problem of individuals facing prolonged suffering, loss of dignity, or unwanted medical interventions at the end of life, without the ability to choose the timing or manner of their death.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups, bioethicists, and numerous individuals facing terminal illness attest to the ongoing nature of this problem. Public opinion surveys in many countries also indicate strong support for individual choice in end-of-life decisions, corroborating the problem's persistence from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because denying a competent individual control over their death, especially in the face of suffering, imposes a severe cost. Suppression (0.75) is also high due to persistent legal prohibitions and strong ethical opposition in many contexts. The theater ratio is low (0.15) as the debate is direct and functional, not performative. Accessibility collapse (0.40) is moderate; while palliative care offers alternatives to suffering, it does not address the core desire for autonomous control over the timing of death. Resistance (0.70) is high, reflecting the ongoing legal and social battles to either establish or prevent this authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of competent individuals seeking to exercise this authority, the constraint is a fundamental right that, when denied, becomes a source of profound suffering and injustice. From the perspective of sanctity-of-life advocates, the very idea of this authority is a moral transgression. Legislators navigate these deeply conflicting views, leading to a contested and evolving landscape.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent individuals seeking to exercise this authority are beneficiaries when it is granted, experiencing low directionality. However, when denied, they become targets, experiencing high directionality due to their trapped exit options and the severe costs of prolonged suffering. Facilitating healthcare professionals are beneficiaries as their actions align with patient autonomy. Sanctity-of-life advocates are excluded, as their position is fundamentally opposed to the constraint's premise. Legislators are agenda-setters, mediating the conflicting demands.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (individual autonomy) is a live and actively contested ethical principle. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring the extraction from denial) or a Snare (ignoring the genuine coordination function for those whose autonomy is respected). The ongoing contestation and the presence of both beneficiaries and victims are central to its nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_autonomy_limits,
    'Is individual sovereign authority over death truly absolute, or are there inherent limits (e.g., mental capacity, coercion risk, societal impact) that this reading downplays?',
    'Empirical studies on the prevalence of coercion or impaired judgment in cases of assisted dying, and philosophical analysis of the boundaries of autonomy in a social context.',
    'If inherent limits are found to be significant and unmitigated, the effective extractiveness for vulnerable populations could be higher than currently measured, potentially shifting the classification towards a Snare if safeguards are insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_autonomy_limits, conceptual, 'The scope and limits of individual autonomy in end-of-life decisions.').

omega_variable(
    slippery_slope_risk,
    'Does the legalization and facilitation of end-of-life decision authority lead to a ''slippery slope'' where vulnerable individuals are pressured or where the value of life is devalued, or is this a speculative fear?',
    'Longitudinal empirical studies from jurisdictions where assisted dying is legal, tracking rates of uptake, demographics of those choosing it, and reported reasons, compared to rates of coercion or abuse.',
    'If a ''slippery slope'' is empirically validated, the effective extractiveness for vulnerable groups would be significantly higher, and the constraint''s coordination function would be undermined by unintended negative consequences. If not, the autonomy reading''s justification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slippery_slope_risk, empirical, 'The potential for unintended negative consequences from legalizing assisted dying.').

omega_variable(
    hcp_conscientious_objection_balance,
    'How should the individual''s right to end-of-life autonomy be balanced with the conscientious objection of healthcare professionals?',
    'Development of legal and ethical frameworks that provide clear guidelines for referral, transfer of care, and institutional responsibilities, ensuring both patient access and professional integrity.',
    'If conscientious objection is allowed to systematically impede access, the effective extractiveness for individuals seeking end-of-life options increases. If it is systematically overridden without adequate support for HCPs, it creates extraction for them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hcp_conscientious_objection_balance, preference, 'Balancing patient autonomy with healthcare professional''s moral integrity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t2000, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(end__tr_t2005, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(end__tr_t2010, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(end__tr_t2015, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(end__tr_t2020, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(end__tr_t2030, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2030, 0.15).

% Extraction over time
narrative_ontology:measurement(end__be_t2000, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(end__be_t2005, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(end__be_t2010, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(end__be_t2015, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(end__be_t2020, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(end__be_t2030, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2030, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t2000, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(end__su_t2005, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(end__su_t2010, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(end__su_t2015, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement(end__su_t2020, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(end__su_t2030, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2030, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
