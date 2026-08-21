% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail Risk Dominance in Energy Policy
 *   domain: risk_assessment/energy_policy/public_safety
 *
 * SUMMARY:
 *   This constraint describes a dominant framework in risk assessment,
 *   particularly within energy policy and public safety governance, where
 *   low-probability high-consequence events, irreversibility, and
 *   intergenerational burden are given overriding weight. This approach often
 *   sidelines traditional probabilistic risk assessment and cost-benefit
 *   analysis, leading to the effective suppression of certain technologies
 *   (e.g., nuclear energy) and projects with long-term impacts. This story is
 *   one reading of the 'acceptable_risk_for_energy' kernel, focusing on the
 *   'catastrophic_tail_dominant' perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.78).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.85).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, snare).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail Risk Dominance in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, '8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6').
narrative_ontology:cs_kernel_codification('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', implicit).
narrative_ontology:cs_authority_grounding('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', practice).
narrative_ontology:cs_interpretation_layer_present('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6').
narrative_ontology:cs_reading_relation('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', acceptable_risk_for_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', foundational, catastrophic_consequences_unacceptable).
narrative_ontology:cs_axiom_status(catastrophic_consequences_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', catastrophic_consequences_unacceptable, deontological).
narrative_ontology:cs_axiom('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', foundational, intergenerational_burden_must_be_avoided).
narrative_ontology:cs_axiom_status(intergenerational_burden_must_be_avoided, holdable).
narrative_ontology:cs_axiom_grounding('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', intergenerational_burden_must_be_avoided, deontological).
narrative_ontology:cs_reference_frame('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', post_chernobyl_precautionary_principle).
narrative_ontology:cs_drift_state('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', contemporary_climate_crisis_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8fb88d63-b115-4cb4-8e28-0eb54ca5f1a6', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, renewable_energy_sector).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_sector).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, proponents_of_probabilistic_risk_assessment).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, industrial_developers_of_long_term_projects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote the prioritization of low-probability, high-consequence risks, emphasizing irreversibility and intergenerational burden in energy policy debates and regulatory processes. They benefit from the constraint on competing energy sources.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocates, agenda_setter,
    organized, generational, mobile, global).

% Are the conceptual beneficiaries of policies that prioritize long-term safety and avoid irreversible harms, particularly regarding nuclear waste or persistent pollutants. Their interests are represented by advocates.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).

% Indirectly benefits as competing energy technologies (e.g., nuclear) face higher regulatory and public acceptance hurdles due to the emphasis on catastrophic tail risks, making renewables comparatively more attractive.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, renewable_energy_sector, beneficiary,
    powerful, biographical, arbitrage, global).

% Bears significant costs due to the framework's emphasis on catastrophic tail risks (e.g., reactor accidents, waste disposal), leading to stringent regulations, public opposition, and project delays or cancellations. Exit means abandoning a core energy technology.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_sector, payer,
    institutional, generational, constrained, national).

% Their methodologies, which weigh risks by probability and consequence, are often sidelined or dismissed in favor of a more absolute 'unacceptable risk' framing for catastrophic events. They bear the cost of their expertise being devalued.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, proponents_of_probabilistic_risk_assessment, payer,
    moderate, biographical, constrained, global).

% Face increased regulatory burdens, public scrutiny, and financial risks for projects with potential long-term environmental or safety impacts, even if probabilities are low. This includes sectors beyond energy, such as large-scale infrastructure or chemical industries.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, industrial_developers_of_long_term_projects, payer,
    powerful, generational, constrained, national).

% Are tasked with implementing and enforcing policies derived from this risk calculus, often under public and political pressure to adopt the most precautionary stance, even if it means imposing high costs or limiting options.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, public_safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Often find their cost-benefit analyses and expected-value models marginalized in policy debates where catastrophic tail risks are given overriding, non-probabilistic weight. They are excluded from the primary decision-making framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, economists_and_risk_analysts, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocates).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate public and regulatory action around a precautionary principle for high-impact, low-probability risks, ensuring long-term safety and intergenerational equity in energy and industrial policy.
% TRANSFER_FUNCTION: Transfers the burden of proof and the cost of extreme risk mitigation onto technologies and projects deemed to have catastrophic tail risks, effectively transferring resources (or preventing their allocation) from these sectors to those deemed safer or less impactful long-term.
% ABSENT_VOICES: Economists and traditional risk analysts who prioritize expected value and cost-benefit analysis, as well as proponents of technologies (e.g., advanced nuclear) who argue their tail risks are manageable or overstated, are often excluded from the core framing of acceptable risk.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, risk assessments would immediately shift to more probabilistic or comparative models, potentially re-enabling projects currently stalled by tail-risk concerns, and significantly altering energy policy, investment, and public discourse around technology.
% FOUNDING_PROBLEM: The perceived inadequacy of traditional risk assessment to account for truly catastrophic, irreversible, or intergenerational harms, particularly after events like Chernobyl or Fukushima, or concerns about climate change and nuclear waste.
% FOUNDING_PROBLEM_CORROBORATION: Environmental organizations, public health bodies, and some intergovernmental panels corroborate the ongoing concern about catastrophic and irreversible risks. However, industry and some scientific bodies contest the degree to which the problem remains unsolved by modern engineering.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this framework imposes significant, often disproportionate, costs on technologies and projects by prioritizing worst-case scenarios over their probabilistic benefits or comparative risks. Suppression is very high as it actively marginalizes and devalues alternative risk assessment methodologies. Theater ratio is moderate, reflecting genuine concern for safety but also a performative aspect in blocking certain technologies. Accessibility collapse is high as it creates an almost insurmountable barrier for certain energy options. Resistance is moderate from those whose methodologies or projects are suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of environmental advocates, this framework is a necessary safeguard for planetary and intergenerational well-being. From the perspective of the nuclear industry or economists, it is an overly conservative, unscientific, and economically damaging constraint that prevents rational decision-making and hinders progress on other critical issues like climate change.
 *
 * DIRECTIONALITY LOGIC:
 *   Environmental advocates and future generations are the primary beneficiaries, as the framework aligns with their goals of long-term safety and precaution. The renewable energy sector benefits indirectly by reducing competition. The nuclear energy sector, proponents of probabilistic risk assessment, and industrial developers of long-term projects are the primary targets, bearing the costs of this stringent risk calculus. Public safety regulators act as agenda-setters, implementing the framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scientific_vs_policy_choice,
    'Is the dominance of catastrophic tail risk in energy policy a robust scientific risk assessment or a policy choice driven by specific values?',
    'Analysis of the scientific consensus on risk quantification for extreme events, and examination of the explicit normative justifications used in policy documents and regulatory decisions.',
    'If primarily a policy choice, the constraint''s classification as a Snare is reinforced, highlighting the value-driven extraction. If it is a scientifically robust assessment, it might lean closer to a Mountain (though still with beneficiaries), indicating a more inherent limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_vs_policy_choice, conceptual, 'Distinguishing scientific necessity from value-driven policy in risk assessment.').

omega_variable(
    irreversibility_definition_consistency,
    'How is ''irreversibility'' defined within this framework, and is its application consistent across different technologies and risks?',
    'Comparative analysis of regulatory decisions and public discourse regarding various technologies (e.g., nuclear waste, carbon emissions, persistent chemicals) to identify implicit or explicit definitions and their consistent application.',
    'Inconsistent application or an overly broad definition of ''irreversibility'' would suggest a performative aspect, increasing the theater_ratio and reinforcing the Snare classification. A consistent, narrow definition would lend more credibility to the framework''s stated rationale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_definition_consistency, empirical, 'Consistency and scope of the ''irreversibility'' criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(acce_tr_t2020, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2025, 0.3).
narrative_ontology:measurement(acce_tr_t2030, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2030, 0.3).

% Extraction over time
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(acce_be_t2020, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement(acce_be_t2030, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2030, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2015, 0.83).
narrative_ontology:measurement(acce_su_t2020, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2025, 0.85).
narrative_ontology:measurement(acce_su_t2030, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2030, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_disposal_standards).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, carbon_emission_targets).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'acceptable_risk_for_energy' kernel, focusing on the catastrophic tail risk perspective. It is structurally distinct from the 'expected_value_dominant' and 'comparative_risk_dominant' readings, which represent alternative approaches to risk assessment in energy policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
