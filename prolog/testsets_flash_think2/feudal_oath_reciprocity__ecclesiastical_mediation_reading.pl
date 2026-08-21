% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Reciprocity: Ecclesiastical Mediation Reading
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'ecclesiastical mediation' reading
 *   of the feudal oath reciprocity kernel. It describes how Christian charity
 *   and sacramental oath obligations, as interpreted and enforced by the
 *   Church, served to limit secular extraction within the medieval feudal
 *   system. The constraint is claimed as a Tangled Rope, reflecting its dual
 *   function of coordinating feudal relations while also enabling the Church
 *   to extract interpretive authority and moral capital, and constraining
 *   secular lords. The metrics reflect a moderate level of extraction and
 *   suppression, as the Church's authority was significant but not absolute.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.45).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.6).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Reciprocity: Ecclesiastical Mediation Reading").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'dcafdcba-e220-4877-bc5f-5566c06a8122').
narrative_ontology:cs_kernel_codification('dcafdcba-e220-4877-bc5f-5566c06a8122', fixed_text).
narrative_ontology:cs_authority_grounding('dcafdcba-e220-4877-bc5f-5566c06a8122', lineage).
narrative_ontology:cs_interpretation_layer_present('dcafdcba-e220-4877-bc5f-5566c06a8122').
narrative_ontology:cs_reading_relation('dcafdcba-e220-4877-bc5f-5566c06a8122', feudal_oath_reciprocity__lord_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('dcafdcba-e220-4877-bc5f-5566c06a8122', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('dcafdcba-e220-4877-bc5f-5566c06a8122', foundational, charity_limits_secular_power).
narrative_ontology:cs_axiom_status(charity_limits_secular_power, holdable).
narrative_ontology:cs_axiom_grounding('dcafdcba-e220-4877-bc5f-5566c06a8122', charity_limits_secular_power, deontological).
narrative_ontology:cs_axiom('dcafdcba-e220-4877-bc5f-5566c06a8122', foundational, sacramental_oath_binding).
narrative_ontology:cs_axiom_status(sacramental_oath_binding, holdable).
narrative_ontology:cs_axiom_grounding('dcafdcba-e220-4877-bc5f-5566c06a8122', sacramental_oath_binding, theological).
narrative_ontology:cs_reference_frame('dcafdcba-e220-4877-bc5f-5566c06a8122', divine_law_as_supreme_arbiter).
narrative_ontology:cs_drift_state('dcafdcba-e220-4877-bc5f-5566c06a8122', late_medieval_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dcafdcba-e220-4877-bc5f-5566c06a8122', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets divine law and canon law, mediates disputes between lords and vassals, and enforces moral obligations through spiritual sanctions. Benefits from increased moral and interpretive authority within the feudal system.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, church_hierarchy, agenda_setter,
    institutional, generational, analytical, global).

% Bound by feudal oaths, which are reinforced by sacramental obligations and Christian charity as interpreted by the Church. Their ability to extract resources or demand service from vassals is constrained by these theological limits, leading to a 'cost' in foregone maximal extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, payer,
    powerful, biographical, constrained, regional).

% Benefit from the Church's mediation and the theological limits placed on their lords' demands. They can appeal to ecclesiastical courts or moral principles to resist excessive extraction or abuse, providing a degree of protection within the feudal structure.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals, beneficiary,
    moderate, biographical, constrained, local).

% Bear the ultimate burden of feudal extraction. While not directly party to the oath, they indirectly benefit from any limits on lordly abuses that the Church's mediation might enforce, preventing total destitution, but remain largely without direct recourse.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasants, payer,
    powerless, immediate, trapped, local).

% Develop, interpret, and apply the complex body of canon law that underpins the Church's authority in mediating feudal relations. They provide the intellectual and legal framework for the constraint's operation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, canon_lawyers, observer,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a moral and legal framework for reciprocal obligations within the feudal system, preventing unchecked secular power and ensuring a degree of justice and stability by integrating spiritual authority.
% TRANSFER_FUNCTION: Transfers interpretive authority and moral leverage to the Church, which in turn limits the material extraction from vassals by secular lords, channeling some 'social surplus' into spiritual capital and institutional legitimacy for the Church.
% ABSENT_VOICES: Secular legal theorists advocating for purely temporal, non-theological bases for feudal law; early proto-capitalists or merchants who might advocate for purely contractual, non-oath-bound economic relations, or those who reject ecclesiastical authority entirely.
% DISAPPEARANCE_RATIONALE: If the ecclesiastical mediation and its underlying theological obligations vanished overnight, feudal relations would likely devolve into more brutal, unchecked power dynamics, leading to greater instability, increased secular extraction, and a breakdown of the moral economy that partially constrained medieval society.
% FOUNDING_PROBLEM: The inherent tension in feudalism between the lord's power and the vassal's need for protection, often leading to arbitrary and excessive extraction, threatening social order, Christian morality, and the stability of the realm.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicles, papal bulls, records of ecclesiastical courts, and theological treatises attest to the problem and the Church's role in attempting to mitigate it. Secular legal historians might offer a more cynical view of the Church's motives, but the historical record of appeals to the Church for justice is clear from outside the benefiting parties.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).
:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.45) because while the Church's intervention limited the most egregious secular demands, it did not eliminate them, and the Church itself gained authority. Suppression is moderate (0.60) as the Church wielded significant spiritual power (excommunication, interdict) to enforce its interpretations, but secular lords often resisted. Theater ratio is low (0.20) because the theological underpinnings of the oaths were genuinely believed and actively enforced, not merely performative. The measurements show a relatively stable, slightly fluctuating pattern, reflecting the ongoing tension and negotiation between secular and ecclesiastical powers over time.
 *
 * PERSPECTIVAL GAP:
 *   From the Church's perspective, this constraint is a necessary moral framework for a just society, a form of coordination. From the perspective of secular lords, it is an external imposition that limits their rightful authority and extraction. Vassals, while benefiting, might still view it as a system that ultimately binds them. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church hierarchy is a beneficiary and agenda-setter, gaining moral authority and influence (low d). Vassals are beneficiaries, receiving protection from excessive lordly demands (low d). Secular lords are targets, as their power to extract is constrained (high d). Peasants are also targets, bearing the ultimate burden, though indirectly benefiting from any limits on lordly abuses (high d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''ecclesiastical_mediation_reading'' of the ''feudal_oath_reciprocity'' kernel?',
    'Comparative analysis with historical sources focusing on Church documents, canon law, and records of ecclesiastical courts, contrasting with secular legal texts and chronicles.',
    'If the reading is found to be misaligned with historical evidence, the classification and structural properties would need to be re-evaluated to better reflect the actual historical dynamics, potentially shifting extractiveness or suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Confirms the fidelity of this story to the specified kernel reading.').

omega_variable(
    secular_resistance_efficacy,
    'How effective were secular lords in resisting or circumventing ecclesiastical mediation and its limits on extraction?',
    'Detailed case studies of specific feudal disputes, examining the outcomes of appeals to both secular and ecclesiastical courts, and the long-term trends in lordly power versus Church influence.',
    'If secular resistance was highly effective, the measured extractiveness for secular lords might be lower (less constrained), and the suppression higher (more active Church enforcement needed), potentially shifting the constraint closer to a Snare for vassals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_resistance_efficacy, empirical, 'Assesses the practical limits of ecclesiastical authority against secular power.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily spiritual (fear of damnation, excommunication) or institutional (Church courts, interdicts, political pressure)?',
    'Analysis of the specific mechanisms of enforcement used by the Church in feudal disputes, and the documented responses of secular lords and vassals to different types of sanctions.',
    'If suppression was primarily spiritual, its long-term efficacy might be more variable and dependent on individual belief, potentially leading to lower effective suppression over time. If institutional, it implies a more robust and persistent enforcement apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ecclesiastical authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(feud_tr_t5, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(feud_tr_t10, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(feud_tr_t15, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(feud_tr_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(feud_tr_t25, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement(feud_tr_t30, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(feud_be_t5, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(feud_be_t10, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(feud_be_t15, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(feud_be_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(feud_be_t25, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(feud_be_t30, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(feud_su_t5, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(feud_su_t10, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(feud_su_t15, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(feud_su_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(feud_su_t25, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(feud_su_t30, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feudal_oath_reciprocity' kernel, each representing a distinct structural interpretation of the feudal oath's function and effects. This reading focuses on the Church's role in mediating and limiting secular extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
