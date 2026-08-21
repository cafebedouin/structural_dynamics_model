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
 *   human_readable: Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the feudal oath as interpreted and mediated by
 *   the Christian Church during the High Middle Ages. The Church's emphasis
 *   on Christian charity and sacramental obligations imposed moral and
 *   spiritual limits on the secular power of lords, preventing maximal
 *   extraction from their vassals. This reading positions the Church as a key
 *   institutional actor in shaping feudal reciprocity, creating a 'tangled
 *   rope' where genuine coordination (social order, reciprocal duties) is
 *   intertwined with the Church's own institutional extraction of
 *   interpretive authority and spiritual compliance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.45).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.6).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '9eb6fe72-7ae2-48e4-b778-546ce8a72b78').
narrative_ontology:cs_kernel_codification('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', formalized).
narrative_ontology:cs_authority_grounding('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', lineage).
narrative_ontology:cs_interpretation_layer_present('9eb6fe72-7ae2-48e4-b778-546ce8a72b78').
narrative_ontology:cs_reading_relation('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', foundational, oath_bound_by_christian_charity).
narrative_ontology:cs_axiom_status(oath_bound_by_christian_charity, holdable).
narrative_ontology:cs_axiom_grounding('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', oath_bound_by_christian_charity, deontological).
narrative_ontology:cs_axiom('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', foundational, sacramental_obligation_limits_secular_power).
narrative_ontology:cs_axiom_status(sacramental_obligation_limits_secular_power, holdable).
narrative_ontology:cs_axiom_grounding('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', sacramental_obligation_limits_secular_power, theological).
narrative_ontology:cs_reference_frame('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', divinely_ordained_reciprocal_duty).
narrative_ontology:cs_drift_state('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', late_medieval_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9eb6fe72-7ae2-48e4-b778-546ce8a72b78', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lords).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the moral and sacramental obligations of the feudal oath, using the threat of excommunication or interdict to limit secular lords' demands. Benefits from increased moral authority and influence over secular affairs.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, church_hierarchy, agenda_setter,
    institutional, generational, mobile, continental).

% Bound by the oath to provide protection and justice, but also constrained by ecclesiastical interpretations of charity and sacramental duty, which limit their ability to extract maximal resources from their vassals. They face spiritual penalties for over-extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lords, payer,
    powerful, biographical, constrained, regional).

% Owe fealty and service to their lord, but benefit from the Church's mediation, which provides a check on arbitrary demands and offers a moral framework for reciprocal obligations. Their spiritual well-being is tied to the oath's proper observance.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals, beneficiary,
    moderate, biographical, constrained, local).

% Analyze the interplay between customary law, royal decrees, and ecclesiastical pronouncements in shaping feudal obligations. They observe the practical effects of the Church's interpretive authority.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_legal_scholars, observer,
    analytical, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a moral and spiritual framework for reciprocal obligations between lords and vassals, providing a basis for social order and limiting the potential for unchecked secular power through divine sanction.
% TRANSFER_FUNCTION: Transfers moral authority and interpretive power over feudal relations to the Church, which in turn limits the material extraction (labor, goods, taxes) from vassals by lords, redirecting some of that potential extraction into spiritual compliance.
% ABSENT_VOICES: Radical secularists or proto-nationalist monarchs who would reject the Church's temporal authority and seek to establish purely secular legal systems for feudal relations are largely absent or suppressed, as the Church's influence is pervasive.
% DISAPPEARANCE_RATIONALE: If the ecclesiastical mediation vanished, the feudal oath would revert to a more purely secular contract, likely leading to increased extraction by lords, greater conflict, and a breakdown of the moral economy that underpinned medieval society. The balance of power would shift dramatically.
% FOUNDING_PROBLEM: To establish a stable social and political order in a fragmented post-Roman world, where secular power was often arbitrary and violence endemic, by binding rulers and ruled through sacred oaths and reciprocal duties.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicles and theological treatises from the period attest to the ongoing need for moral constraints on secular power and the Church's role in maintaining social cohesion. While the specific form of feudalism has passed, the underlying problem of binding power to moral limits remains relevant in historical analysis.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.45) is moderate, reflecting the Church's success in limiting secular lords' demands, but also its own 'extraction' of interpretive authority and spiritual adherence. Suppression (0.6) is significant, as the Church actively enforced its interpretations through spiritual sanctions. Theater ratio (0.25) is relatively low, as the spiritual threats were genuinely feared and often effective. The claimed type is 'tangled_rope' because it genuinely coordinated social order while simultaneously enabling the Church to extract interpretive authority and influence.
 *
 * PERSPECTIVAL GAP:
 *   From the Church's perspective, this was a 'rope' of divine order, ensuring justice and charity. From the lords' perspective, it was a 'snare' limiting their temporal power. From the vassals' perspective, it was a 'rope' offering protection against arbitrary rule. The engine will compute these per-seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church hierarchy is a beneficiary (gains moral authority, limits rivals) and an agenda-setter (interprets the oath). Vassals are beneficiaries (protected from arbitrary extraction). Lords are payers (constrained in their extraction). This creates the asymmetric flow characteristic of a tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'snare' (lord extraction reading) or a pure 'rope' (vassal coordination reading). By identifying the Church's active, self-interested role in mediating the oath, it highlights the hybrid nature of the constraint, where a genuine coordination function (social stability) is coupled with institutional extraction (Church authority).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    church_authority_vs_secular_power,
    'To what extent did the Church''s spiritual authority genuinely limit secular extraction, versus merely providing a rhetorical cover for existing power dynamics?',
    'Comparative historical analysis of regions with stronger vs. weaker ecclesiastical influence on feudal law, examining differences in vassal obligations and lordly demands.',
    'If the Church''s influence was primarily rhetorical, the constraint''s effective extractiveness for lords would be lower, and its suppression of vassals higher, pushing it closer to a ''snare'' for vassals and a ''rope'' for lords. If genuine, the ''tangled_rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_authority_vs_secular_power, empirical, 'Assessing the true impact of ecclesiastical mediation on feudal power dynamics.').

omega_variable(
    sacramental_obligation_sincerity,
    'How genuinely did lords and vassals perceive and internalize the sacramental obligations of the oath, beyond mere political expediency?',
    'Analysis of personal letters, wills, and confessional records for evidence of genuine spiritual concern regarding oath-breaking, rather than just fear of temporal punishment.',
    'If internalization was low, the ''suppression'' metric would be more purely structural (external threat), and the ''theater_ratio'' might be higher, as the spiritual aspect becomes more performative. If high, the constraint''s moral force was more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_obligation_sincerity, empirical, 'The degree of internalized spiritual commitment to the feudal oath.').

omega_variable(
    reading_framing_choice,
    'Is this ''ecclesiastical_mediation_reading'' the most appropriate framing, or would a ''vassal_coordination_reading'' (emphasizing mutual, bounded obligations) or ''lord_extraction_reading'' (emphasizing lordly dominance) be more structurally accurate?',
    'Analysis of the primary sources (charters, legal codes, theological texts) to determine which interpretive tradition held the most consistent and effective sway over the longest period, and whose interests were most consistently served by that interpretation.',
    'If a different reading were adopted, the claimed_type, extractiveness, and beneficiary/victim structure would shift significantly, leading to a different classification (e.g., ''rope'' for vassal coordination, ''snare'' for lord extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Under-determination of the primary interpretive frame for the feudal oath.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 1000, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1100, 0.25).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1200, 0.3).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1300, 0.25).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1100, 0.45).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1200, 0.48).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1300, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1100, 0.6).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1200, 0.65).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1300, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feudal_oath_reciprocity' kernel, each representing a distinct structural interpretation of the same underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
