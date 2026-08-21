% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling's Decline by Institutional Displacement
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   This constraint story describes the decline of dueling as a primary
 *   dispute-resolution mechanism due to the rise and increasing efficacy of
 *   institutional alternatives such as courts, the banking system, and libel
 *   law. This reading posits that dueling was not primarily suppressed by
 *   direct prohibition or cultural shift, but rather outcompeted by superior
 *   coordination mechanisms. The constraint is a 'rope' because it represents
 *   a coordination on more effective dispute resolution, with minimal
 *   extraction and voluntary adoption of alternatives. Dueling itself
 *   persists as a fringe option, but its social function is largely
 *   displaced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.25).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling's Decline by Institutional Displacement").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '0c429d68-5268-438d-8d79-941c1d317f66').
narrative_ontology:cs_kernel_codification('0c429d68-5268-438d-8d79-941c1d317f66', implicit).
narrative_ontology:cs_authority_grounding('0c429d68-5268-438d-8d79-941c1d317f66', practice).
narrative_ontology:cs_reading_relation('0c429d68-5268-438d-8d79-941c1d317f66', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c429d68-5268-438d-8d79-941c1d317f66', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('0c429d68-5268-438d-8d79-941c1d317f66', foundational, institutional_efficacy_drives_social_change).
narrative_ontology:cs_axiom_status(institutional_efficacy_drives_social_change, holdable).
narrative_ontology:cs_axiom_grounding('0c429d68-5268-438d-8d79-941c1d317f66', institutional_efficacy_drives_social_change, empirically_contingent).
narrative_ontology:cs_axiom('0c429d68-5268-438d-8d79-941c1d317f66', foundational, dispute_resolution_is_a_functional_market).
narrative_ontology:cs_axiom_status(dispute_resolution_is_a_functional_market, holdable).
narrative_ontology:cs_axiom_grounding('0c429d68-5268-438d-8d79-941c1d317f66', dispute_resolution_is_a_functional_market, empirically_contingent).
narrative_ontology:cs_reference_frame('0c429d68-5268-438d-8d79-941c1d317f66', functional_institutional_competition).
narrative_ontology:cs_drift_state('0c429d68-5268-438d-8d79-941c1d317f66', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0c429d68-5268-438d-8d79-941c1d317f66', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, courts).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, banking_system).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_practitioners).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, disputants_seeking_non_violent_resolution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who previously might have resorted to dueling now have more reliable, less risky, and often more effective institutional avenues for resolving disputes concerning honor, reputation, or financial claims. They benefit from the availability of these alternatives.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, disputants_seeking_non_violent_resolution, beneficiary,
    moderate, biographical, mobile, local).

% As the primary institutional alternative, courts offered formal, legally binding dispute resolution. Their increasing efficiency and scope made dueling less attractive by providing a superior mechanism for justice and redress, particularly for libel and financial disputes.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% The growth of a robust banking and credit system provided formal mechanisms for resolving financial disputes and enforcing contracts, which previously might have escalated to dueling in honor cultures. It offered a more stable and predictable framework for commercial interactions.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, banking_system, beneficiary,
    institutional, generational, analytical, national).

% Lawyers and legal institutions specializing in libel and defamation law offered a civil route to defend reputation, providing a non-violent and legally sanctioned alternative to dueling for matters of honor. They benefited from the expansion of this legal domain.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Individuals who still adhered to traditional honor codes found dueling increasingly disfavored and legally risky. While not directly 'victims' of extraction, they bore the social cost of choosing a declining, less legitimate dispute resolution method, facing legal penalties and social ostracization.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_adherents, payer,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a set of increasingly effective and legitimate institutional alternatives (courts, banking, libel law) that coordinated dispute resolution away from dueling, offering more predictable and less violent outcomes.
% TRANSFER_FUNCTION: Transferred the social function of dispute resolution from informal, violent honor-based mechanisms to formal, non-violent legal and economic institutions, shifting social capital and legitimacy to these new structures.
% ABSENT_VOICES: Traditionalists who valued dueling as a necessary component of honor culture were increasingly marginalized; their arguments for its social utility were outcompeted by the perceived efficacy and legitimacy of institutional alternatives.
% DISAPPEARANCE_RATIONALE: If the institutional alternatives (courts, banking, libel law) had not emerged or had disappeared, dueling would likely have persisted as a more central, albeit still contested, mechanism for dispute resolution, particularly in matters of honor. The social fabric of dispute resolution would be fundamentally different.
% FOUNDING_PROBLEM: The problem of resolving disputes, particularly those involving honor and financial claims, in a manner that was perceived as legitimate and effective by society.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists corroborate that the problem of dispute resolution is perennial, and that the institutional mechanisms that displaced dueling continue to evolve to address it. This is attested by academic scholarship and legal reforms, not just by the benefiting institutions themselves.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the shift to institutional alternatives was largely voluntary, driven by their superior utility rather than coercive extraction. Suppression is also low (0.25) because while dueling was increasingly illegal, its decline was more about the attractiveness of alternatives than active enforcement. Theater ratio is negligible (0.05) as the institutional alternatives were genuinely functional. The slight increase in extractiveness and suppression towards the end of the interval reflects the final marginalization of dueling, where choosing it incurred higher social and legal costs, but this was a consequence of its displacement, not its primary cause.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the emerging institutions, this was a natural evolution towards more rational and effective governance. From the perspective of honor culture adherents, it was a loss of a traditional, albeit violent, means of maintaining social order and personal dignity. However, this reading emphasizes the functional superiority of the new institutions as the primary driver of change, rather than a direct clash of values or coercive suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The courts, banking system, and libel law practitioners are beneficiaries as their roles expanded and became more central to dispute resolution. Disputants seeking non-violent resolution are also beneficiaries, gaining access to safer and more effective methods. Honor culture adherents are 'payers' in the sense that they bear the social cost of adhering to a declining practice, but they are not 'victims' of extraction by the new institutions, as they could voluntarily adopt the alternatives.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_institutional_displacement,
    'Was institutional displacement the primary cause of dueling''s decline, or was it merely one factor among others (e.g., cultural shifts, direct legal prohibition)?',
    'Comparative historical analysis across different societies with varying legal and cultural contexts, isolating the impact of institutional development from other factors. Counterfactual historical modeling.',
    'If institutional displacement was not primary, this reading''s classification as a ''rope'' (voluntary coordination) might be too benign, and the ''contraction_reading'' or ''overdetermined_composite_reading'' might be more accurate, potentially revealing higher suppression or extraction from other sources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_institutional_displacement, empirical, 'Assessing the relative causal weight of institutional displacement versus other factors in dueling''s decline.').

omega_variable(
    voluntary_adoption_vs_coercion,
    'To what extent was the adoption of institutional alternatives truly voluntary, versus being subtly coerced by increasing legal penalties and social ostracization for dueling?',
    'Detailed micro-historical studies of individual choices and social pressures in specific communities, examining the perceived costs and benefits of dueling versus legal recourse over time.',
    'If coercion played a more significant role than currently assessed, the ''suppression'' metric might be higher, and the constraint could lean towards a ''tangled_rope'' or even ''snare'' if the alternatives were not genuinely superior but merely enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_coercion, empirical, 'Distinguishing between voluntary choice and subtle coercion in the shift away from dueling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(duel_tr_t1780, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1780, 0.07).
narrative_ontology:measurement(duel_tr_t1810, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1810, 0.06).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(duel_tr_t1870, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1870, 0.04).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1750, 0.2).
narrative_ontology:measurement(duel_be_t1780, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1780, 0.18).
narrative_ontology:measurement(duel_be_t1810, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1810, 0.16).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1840, 0.14).
narrative_ontology:measurement(duel_be_t1870, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1870, 0.13).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1750, 0.3).
narrative_ontology:measurement(duel_su_t1780, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1780, 0.28).
narrative_ontology:measurement(duel_su_t1810, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1810, 0.26).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1840, 0.24).
narrative_ontology:measurement(duel_su_t1870, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1870, 0.23).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1900, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dueling_disappearance_mechanism' kernel, focusing on institutional displacement. It is linked to sibling readings that emphasize cultural contraction and overdetermined causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
