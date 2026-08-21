% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__deployer_liability, []).

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
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: AI Liability: Deployer Primary Responsibility
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'deployer_liability' reading of
 *   the broader 'liability_attribution' kernel in AI governance. It describes
 *   a legal and regulatory framework where the primary responsibility for
 *   harms caused by AI systems is attributed to the entities that deploy
 *   them, based on their control over the deployment context and decision
 *   authority. This framework aims to provide clarity for affected parties
 *   and shield upstream developers and foundation model providers from direct
 *   liability for downstream harms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.7).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.8).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.7).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "AI Liability: Deployer Primary Responsibility").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '4da0e7ea-768f-4422-bf4a-4eb47501a55f').
narrative_ontology:cs_kernel_codification('4da0e7ea-768f-4422-bf4a-4eb47501a55f', formalized).
narrative_ontology:cs_authority_grounding('4da0e7ea-768f-4422-bf4a-4eb47501a55f', lineage).
narrative_ontology:cs_interpretation_layer_present('4da0e7ea-768f-4422-bf4a-4eb47501a55f').
narrative_ontology:cs_reading_relation('4da0e7ea-768f-4422-bf4a-4eb47501a55f', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('4da0e7ea-768f-4422-bf4a-4eb47501a55f', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('4da0e7ea-768f-4422-bf4a-4eb47501a55f', foundational, control_implies_responsibility).
narrative_ontology:cs_axiom_status(control_implies_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('4da0e7ea-768f-4422-bf4a-4eb47501a55f', control_implies_responsibility, deontological).
narrative_ontology:cs_axiom('4da0e7ea-768f-4422-bf4a-4eb47501a55f', secondary, innovation_shielding_is_priority).
narrative_ontology:cs_axiom_status(innovation_shielding_is_priority, holdable).
narrative_ontology:cs_axiom_grounding('4da0e7ea-768f-4422-bf4a-4eb47501a55f', innovation_shielding_is_priority, instrumental).
narrative_ontology:cs_reference_frame('4da0e7ea-768f-4422-bf4a-4eb47501a55f', traditional_tort_law_principles).
narrative_ontology:cs_drift_state('4da0e7ea-768f-4422-bf4a-4eb47501a55f', contemporary_ai_governance_debate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4da0e7ea-768f-4422-bf4a-4eb47501a55f', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, developers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, affected_parties).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations or individuals who integrate and use AI systems in specific contexts. They bear primary legal and financial responsibility for harms caused by the AI, requiring extensive due diligence and risk management. Their ability to exit AI deployment is limited by competitive pressures and strategic necessity.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, deployers, payer,
    moderate, biographical, constrained, global).

% Entities that develop and provide foundational AI models. Under this regime, they are largely shielded from downstream liability, externalizing significant risk to deployers. They benefit from reduced legal exposure and increased market adoption of their models.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Creators of specific AI applications built on foundational models. They benefit from the externalization of deployment-related risks, allowing them to focus on innovation without primary liability for contextual harms. Their mobility allows them to shift focus or jurisdiction if liability regimes change.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, developers, beneficiary,
    powerful, biographical, mobile, global).

% Government agencies and legal institutions responsible for establishing and enforcing AI liability frameworks. They define 'control' and 'decision authority' for deployers and adjudicate disputes, aiming to create clear lines of responsibility.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Individuals or groups harmed by AI systems. They benefit from a clear point of contact (the deployer) for seeking recourse, rather than navigating complex supply chains. However, their ability to obtain full compensation may still be constrained by the deployer's resources or the difficulty of proving causation.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, affected_parties, beneficiary,
    powerless, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies the primary locus of responsibility for AI-related harms, providing a predictable framework for legal recourse and encouraging risk management at the point of deployment.
% TRANSFER_FUNCTION: Transfers the primary burden of legal and financial liability for AI harms from upstream developers and foundation model providers to downstream deployers.
% ABSENT_VOICES: Deployers, particularly smaller entities, may feel their voices are absent in the design of these liability rules, arguing that their 'control' is often limited by the inherent opacity and complexity of the AI systems they deploy. They would advocate for more distributed or upstream liability.
% DISAPPEARANCE_RATIONALE: If this liability attribution vanished, the legal landscape for AI would become highly uncertain. Developers and providers would face increased, unpredictable risk, potentially stifling innovation or leading to withdrawal from certain markets. Affected parties would struggle to find clear avenues for redress, and the entire AI value chain would need to re-evaluate risk allocation.
% FOUNDING_PROBLEM: The rapid deployment of complex AI systems created a 'liability gap' where it was unclear who was responsible for harms, hindering both innovation (due to developer uncertainty) and victim recourse.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, regulatory proposals (e.g., EU AI Act), and industry bodies (e.g., IEEE) corroborate the existence of a liability gap and the need for clear attribution. While developers and providers benefit from this specific solution, the problem itself is widely acknowledged by independent experts.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__deployer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__deployer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__deployer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.7) because deployers bear a significant, often unquantifiable, risk burden that is externalized from upstream actors. `Suppression` is high (0.8) due to the coercive nature of legal frameworks and the limited alternatives for deployers to avoid this liability if they wish to use AI. `Theater_ratio` is low (0.2) as the system is largely functional in its intent to assign responsibility, though the definition of 'control' may have performative elements. `Accessibility_collapse` is high (0.7) for deployers, as options to deploy AI without assuming this liability are severely limited. `Resistance` is moderate (0.6) as deployers, especially smaller ones, actively lobby for more balanced liability models.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of deployers, this constraint is highly extractive, forcing them to internalize risks they may not fully control or understand due to AI opacity. From the perspective of developers and foundation model providers, it is a necessary coordination mechanism that fosters innovation by clarifying risk. Regulatory bodies view it as a balanced approach to ensure accountability and victim redress.
 *
 * DIRECTIONALITY LOGIC:
 *   Deployers are the primary targets (`payer`) as they absorb the liability. Foundation model providers and developers are the primary beneficiaries, as this framework shields them from significant downstream risk. Affected parties are also beneficiaries, as they gain a clearer path to recourse. Regulatory bodies act as agenda-setters, defining and enforcing the rules.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not currently subject to mandatrophy, as the problem of AI liability is live and evolving. The framework is actively being shaped to address a contemporary challenge, rather than persisting due to inertia. The contest is over the *form* of the solution, not its necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_definition_ambiguity,
    'How precisely can ''deployment context control and decision authority'' be defined and measured, especially for opaque or autonomous AI systems?',
    'Development of industry-specific standards, regulatory guidance, and case law that clarify the scope and limits of deployer control in practice.',
    'If ''control'' remains ambiguous, deployers may bear liability for factors beyond their actual influence, increasing effective extraction. If clarified, it could make the constraint more equitable, reducing extraction for deployers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_definition_ambiguity, conceptual, 'Ambiguity in defining ''control'' for liability attribution.').

omega_variable(
    innovation_vs_safety_tradeoff,
    'Does placing primary liability on deployers disproportionately stifle AI adoption and innovation, particularly for SMEs, compared to alternative liability models?',
    'Empirical studies comparing AI adoption rates, risk management practices, and innovation output under different liability regimes (e.g., in jurisdictions with varying approaches).',
    'If innovation is significantly stifled, the coordination function of the constraint (enabling responsible AI use) is undermined, potentially shifting its classification towards a Snare for deployers. If not, the current balance is more justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_vs_safety_tradeoff, empirical, 'Impact of deployer liability on AI innovation and adoption.').

omega_variable(
    risk_shifting_vs_optimal_allocation,
    'Is this attribution primarily a risk-shifting mechanism benefiting powerful upstream actors, or does it genuinely represent the most efficient and just allocation of liability?',
    'Comprehensive economic and ethical analysis of the AI value chain, considering information asymmetry, risk-bearing capacity, and incentives for safety across all actors.',
    'If primarily risk-shifting, the ''tangled_rope'' classification''s extractive component is amplified. If optimal, the coordination function is stronger, potentially leaning towards a ''rope'' for the system as a whole, even if extractive for deployers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_shifting_vs_optimal_allocation, preference, 'Whether liability attribution is risk-shifting or optimal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t2020, liability_attribution__deployer_liability, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(liab_tr_t2022, liability_attribution__deployer_liability, theater_ratio, 2022, 0.22).
narrative_ontology:measurement(liab_tr_t2024, liability_attribution__deployer_liability, theater_ratio, 2024, 0.2).
narrative_ontology:measurement(liab_tr_t2026, liability_attribution__deployer_liability, theater_ratio, 2026, 0.18).
narrative_ontology:measurement(liab_tr_t2028, liability_attribution__deployer_liability, theater_ratio, 2028, 0.2).
narrative_ontology:measurement(liab_tr_t2030, liability_attribution__deployer_liability, theater_ratio, 2030, 0.2).

% Extraction over time
narrative_ontology:measurement(liab_be_t2020, liability_attribution__deployer_liability, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(liab_be_t2022, liability_attribution__deployer_liability, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(liab_be_t2024, liability_attribution__deployer_liability, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement(liab_be_t2026, liability_attribution__deployer_liability, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement(liab_be_t2028, liability_attribution__deployer_liability, base_extractiveness, 2028, 0.7).
narrative_ontology:measurement(liab_be_t2030, liability_attribution__deployer_liability, base_extractiveness, 2030, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t2020, liability_attribution__deployer_liability, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(liab_su_t2022, liability_attribution__deployer_liability, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement(liab_su_t2024, liability_attribution__deployer_liability, suppression_requirement, 2024, 0.75).
narrative_ontology:measurement(liab_su_t2026, liability_attribution__deployer_liability, suppression_requirement, 2026, 0.78).
narrative_ontology:measurement(liab_su_t2028, liability_attribution__deployer_liability, suppression_requirement, 2028, 0.8).
narrative_ontology:measurement(liab_su_t2030, liability_attribution__deployer_liability, suppression_requirement, 2030, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
