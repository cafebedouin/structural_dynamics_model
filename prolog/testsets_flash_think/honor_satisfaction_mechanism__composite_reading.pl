% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism (Composite Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint story, 'Honor Satisfaction Mechanism (Composite
 *   Reading)', analyzes the decline of dueling as a legitimate means of honor
 *   satisfaction through the lens of multiple, interacting mechanisms: state
 *   monopoly on violence, evolving bourgeois norms, the financial
 *   disincentives of insurance, and a fundamental category-shift in its
 *   social perception. It is one reading of the
 *   'honor_satisfaction_mechanism' kernel, emphasizing the multi-faceted
 *   erosion of the constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.78).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.85).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Composite Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, 'beea2751-4457-417c-b249-c3f7e513a767').
narrative_ontology:cs_kernel_codification('beea2751-4457-417c-b249-c3f7e513a767', distributed).
narrative_ontology:cs_authority_grounding('beea2751-4457-417c-b249-c3f7e513a767', distributed).
narrative_ontology:cs_reading_relation('beea2751-4457-417c-b249-c3f7e513a767', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('beea2751-4457-417c-b249-c3f7e513a767', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('beea2751-4457-417c-b249-c3f7e513a767', foundational, honor_requires_personal_satisfaction).
narrative_ontology:cs_axiom_status(honor_requires_personal_satisfaction, overridden).
narrative_ontology:cs_axiom_grounding('beea2751-4457-417c-b249-c3f7e513a767', honor_requires_personal_satisfaction, deontological).
narrative_ontology:cs_axiom('beea2751-4457-417c-b249-c3f7e513a767', foundational, state_monopoly_on_violence_is_supreme).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('beea2751-4457-417c-b249-c3f7e513a767', state_monopoly_on_violence_is_supreme, conventional).
narrative_ontology:cs_reference_frame('beea2751-4457-417c-b249-c3f7e513a767', aristocratic_honor_code_system).
narrative_ontology:cs_drift_state('beea2751-4457-417c-b249-c3f7e513a767', post_enlightenment_industrial_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('beea2751-4457-417c-b249-c3f7e513a767', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_authority).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_society).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_companies).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, aristocratic_duellists).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, families_of_duellists).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, honor_code_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserted a monopoly on legitimate violence, criminalizing dueling and enforcing its prohibition through legal means. Benefited from increased internal stability and control over social order.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Promoted new norms emphasizing rationality, commerce, and domesticity over aristocratic honor, gradually eroding the social legitimacy of dueling. Benefited from a more stable and predictable social environment.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_society, beneficiary,
    organized, generational, mobile, national).

% Introduced life insurance policies that often contained clauses voiding coverage in cases of death by duel, creating a significant financial disincentive for participants and their families. Benefited from new market opportunities in risk management.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_companies, beneficiary,
    organized, biographical, arbitrage, national).

% Initially adhered to the honor code, but increasingly faced legal penalties (fines, imprisonment, exile), social ostracization, and financial ruin for engaging in dueling. Their traditional means of honor satisfaction became a high-cost, high-risk endeavor.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, aristocratic_duellists, payer,
    powerful, biographical, constrained, regional).

% Suffered the direct loss of life, financial hardship due to insurance policy voiding, and social stigma associated with dueling. Had very limited options to avoid these costs once a family member engaged in a duel.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, families_of_duellists, payer,
    powerless, biographical, trapped, local).

% Individuals who, despite the rising costs and declining legitimacy, still felt compelled by a personal or group identity tied to the honor code. Their commitment to honor made exit from the dueling logic difficult, even as the practice became untenable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, honor_code_adherents, payer,
    moderate, biographical, identity_locked, local).

% Analyze the historical evolution and decline of dueling, documenting the interplay of legal, social, and economic factors that led to its recategorization and eventual disappearance as a legitimate practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_scholars_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, state_authority).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, it coordinated the resolution of honor disputes among the aristocracy, providing a structured, albeit violent, means of maintaining social status and reputation within that class.
% TRANSFER_FUNCTION: Transferred social legitimacy and status to those who successfully navigated the honor system, while transferring legal penalties, social ostracization, and financial ruin to those who engaged in dueling as its legitimacy eroded.
% ABSENT_VOICES: The voices of the common populace, who were largely excluded from the honor system and bore the societal costs of aristocratic violence, were absent from the formal mechanisms of honor satisfaction. Later, those advocating for a complete abolition of dueling, rather than its mere recategorization, were marginalized.
% DISAPPEARANCE_RATIONALE: If the honor satisfaction mechanism, including dueling, were to suddenly regain its former legitimacy and prevalence, it would require a fundamental reordering of state power (monopoly on violence), social values (bourgeois norms), and legal/financial frameworks (insurance law). The current legal and social structures are deeply antithetical to dueling.
% FOUNDING_PROBLEM: To provide a formal, albeit violent, mechanism for aristocratic men to defend their honor and resolve disputes, thereby maintaining social order and status within their class.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists widely corroborate that the social function of dueling as a legitimate honor satisfaction mechanism is dead, superseded by state legal systems and evolving social norms. Contemporary proponents of dueling are fringe actors, not representative of mainstream society or legal authority.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.78) is high because dueling became increasingly costly for participants due to legal penalties, social stigma, and financial ruin. Suppression (0.85) is severe, reflecting the state's active criminalization and the pervasive social disapproval. The theater ratio (0.55) indicates that while the practice largely atrophied, residual acts of dueling or adherence to honor codes became more performative or defiant, rather than serving their original social function. Accessibility collapse (0.82) is high as alternatives to dueling (legal recourse, social negotiation) became dominant, and the option of dueling itself became extremely difficult and risky. Resistance (0.35) is moderate-low, as active resistance to the prohibition of dueling largely faded with its social legitimacy. The claimed type is 'tangled_rope' to reflect its historical dual function of coordinating honor while extracting costs, even as the coordination function eroded over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state authority and bourgeois society, the decline of dueling was a positive development, leading to greater social order and rationality. For aristocratic duellists and honor code adherents, it represented a loss of a traditional means of maintaining status and a profound shift in their social identity, forcing them to bear increasing costs for a diminishing social return.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority, bourgeois society, and insurance companies are beneficiaries, as they gained from the decline of dueling and the establishment of new social and legal orders. Aristocratic duellists, their families, and honor code adherents are victims/payers, bearing the direct costs of legal penalties, social ostracization, financial loss, and the erosion of their identity-bound practices. The directionality for duellists shifted from a complex mix of benefit/cost to predominantly cost as the constraint eroded.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of the honor satisfaction mechanism (to resolve aristocratic honor disputes) became dead as the problem it addressed was superseded by new legal and social structures. The constraint persisted, not to fulfill its original mandate, but through the active enforcement of state authority and the imposition of new social and financial costs, effectively transforming its function from coordination to extraction and suppression. The high theater ratio reflects the performative maintenance of honor codes even as their functional basis disappeared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''composite_reading'' of the ''honor_satisfaction_mechanism'' kernel?',
    'Comparative analysis with sibling readings (''decline_reading'', ''contraction_reading'') to determine if the multi-faceted erosion described here is the most fitting interpretation of the historical evidence.',
    'If a sibling reading is more accurate, the classification and metric trajectories would shift to emphasize either a simple decline in frequency (decline_reading) or a fundamental cognitive impossibility (contraction_reading), rather than the interplay of multiple mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the primary mechanism of dueling''s decline.').

omega_variable(
    relative_weight_of_mechanisms,
    'What was the relative weight and causal priority of state monopoly, bourgeois norms, insurance, and category-shift in the erosion of dueling?',
    'Further historical and sociological research, potentially using counterfactual analysis or comparative case studies across different national contexts.',
    'A clearer understanding of causal priority would refine the temporal measurements and the specific mechanisms of suppression and extraction, potentially altering the perceived ''force'' behind the constraint''s erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_weight_of_mechanisms, empirical, 'Uncertainty regarding the causal hierarchy of factors contributing to dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(hono_tr_t1725, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1725, 0.28).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1750, 0.35).
narrative_ontology:measurement(hono_tr_t1775, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1775, 0.42).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1800, 0.48).
narrative_ontology:measurement(hono_tr_t1825, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1825, 0.52).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1850, 0.55).
narrative_ontology:measurement(hono_tr_t1875, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1875, 0.55).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.55).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1700, 0.45).
narrative_ontology:measurement(hono_be_t1725, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1725, 0.55).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement(hono_be_t1775, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1775, 0.68).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1800, 0.73).
narrative_ontology:measurement(hono_be_t1825, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1825, 0.76).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1850, 0.77).
narrative_ontology:measurement(hono_be_t1875, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1875, 0.78).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(hono_su_t1725, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1725, 0.65).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(hono_su_t1775, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1775, 0.75).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1800, 0.8).
narrative_ontology:measurement(hono_su_t1825, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1825, 0.83).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1850, 0.85).
narrative_ontology:measurement(hono_su_t1875, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1875, 0.85).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, bourgeois_social_norms).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, insurance_contract_law).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_satisfaction_mechanism' kernel. This 'composite_reading' emphasizes the multi-faceted erosion of dueling through legal, social, and economic pressures, distinct from readings focusing on simple decline or cognitive impossibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
