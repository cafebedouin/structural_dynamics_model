% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__developer_liability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer Primary Liability for Capability Creation
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint describes the legal and regulatory principle that
 *   attributes primary liability for harms caused by technology to the
 *   developers or creators of the underlying capability. It is one reading of
 *   the broader 'liability_attribution' kernel, which is contested among
 *   different stakeholders. This reading places the burden of risk and
 *   responsibility upstream, often regardless of downstream deployment
 *   contexts. The metrics reflect a growing trend of extractiveness and
 *   suppression as regulatory pressure on tech harms increases.
 *
 * KEY AGENTS:
 *   - software_developers: Primary payer (moderate power/constrained exit)
 *   - ai_model_creators: Primary payer (moderate power/constrained exit)
 *   - deployers: Primary beneficiary (powerful/mobile exit)
 *   - regulatory_bodies: Agenda setter (institutional/analytical exit)
 *   - affected_public: Beneficiary (powerless/trapped exit)
 *   - insurance_providers: Beneficiary (organized/arbitrage exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.65).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.7).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.65).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, snare).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Primary Liability for Capability Creation").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '23ebcac3-b64a-43ed-bfda-a3a57b494136').
narrative_ontology:cs_kernel_codification('23ebcac3-b64a-43ed-bfda-a3a57b494136', formalized).
narrative_ontology:cs_authority_grounding('23ebcac3-b64a-43ed-bfda-a3a57b494136', lineage).
narrative_ontology:cs_interpretation_layer_present('23ebcac3-b64a-43ed-bfda-a3a57b494136').
narrative_ontology:cs_reading_relation('23ebcac3-b64a-43ed-bfda-a3a57b494136', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('23ebcac3-b64a-43ed-bfda-a3a57b494136', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('23ebcac3-b64a-43ed-bfda-a3a57b494136', foundational, creator_bears_ultimate_responsibility).
narrative_ontology:cs_axiom_status(creator_bears_ultimate_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('23ebcac3-b64a-43ed-bfda-a3a57b494136', creator_bears_ultimate_responsibility, deontological).
narrative_ontology:cs_axiom('23ebcac3-b64a-43ed-bfda-a3a57b494136', secondary, upstream_incentives_prevent_harm).
narrative_ontology:cs_axiom_status(upstream_incentives_prevent_harm, holdable).
narrative_ontology:cs_axiom_grounding('23ebcac3-b64a-43ed-bfda-a3a57b494136', upstream_incentives_prevent_harm, instrumental).
narrative_ontology:cs_reference_frame('23ebcac3-b64a-43ed-bfda-a3a57b494136', traditional_product_liability_framework).
narrative_ontology:cs_drift_state('23ebcac3-b64a-43ed-bfda-a3a57b494136', contemporary_ai_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('23ebcac3-b64a-43ed-bfda-a3a57b494136', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, regulatory_bodies).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, software_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, ai_model_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, affected_public).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, insurance_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary legal and financial risk for harms caused by the capabilities they create, even when those capabilities are misused or deployed in unforeseen contexts. This includes costs for legal defense, settlements, and increased insurance premiums. Exit means abandoning the field or moving to jurisdictions with different liability regimes.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, software_developers, payer,
    moderate, biographical, constrained, global).

% Similar to software developers, they face significant liability for the behavior of AI models, particularly for emergent properties or unintended consequences. They must invest heavily in safety, explainability, and risk mitigation, often without full control over deployment contexts. Exit is difficult due to career path dependence and high R&D investment.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, ai_model_creators, payer,
    moderate, biographical, constrained, global).

% Benefit from externalizing much of the liability risk to developers. They can deploy capabilities with less direct legal exposure, focusing on operational aspects rather than fundamental safety or ethical design, as long as they adhere to deployment guidelines. Their exit options include choosing different developers or platforms.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deployers, beneficiary,
    powerful, biographical, mobile, global).

% Establish and enforce the legal frameworks that attribute primary liability to developers. They benefit from a clearer target for enforcement actions and a simplified regulatory burden compared to complex, distributed liability models. Their role is to define and uphold this specific attribution principle.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from having a clear party to hold accountable for harms caused by technology. However, the actual ability to seek redress is often constrained by legal costs and the complexity of proving causation, especially for diffuse or systemic harms. Their options are limited to seeking legal action or advocating for policy change.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, affected_public, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from the increased demand for liability insurance among developers and creators. They price risk based on the attributed liability, creating a new market for specialized coverage. They can adjust premiums or terms to manage their own exposure, effectively arbitraging the risk.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, insurance_providers, beneficiary,
    organized, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simplifies the legal and regulatory landscape by assigning a single, primary point of accountability (the creator) for technological harms, theoretically incentivizing upstream safety and ethical design.
% TRANSFER_FUNCTION: Transfers legal and financial risk, as well as the burden of managing potential harms, from deployers and society at large to the creators of technological capabilities.
% ABSENT_VOICES: Advocates for distributed or shared liability models, who would argue that control over deployment context and ongoing operation are equally critical to harm prevention, are often marginalized in discussions focused solely on creation. They would point to the limitations of developer control post-deployment.
% DISAPPEARANCE_RATIONALE: If developer primary liability vanished, deployers would immediately face increased risk and pressure to implement their own robust safety and ethical frameworks. Regulatory bodies would need to redesign enforcement mechanisms, and the entire tech liability insurance market would shift dramatically, reorganizing around new attribution principles.
% FOUNDING_PROBLEM: The challenge of attributing responsibility for harms caused by complex, rapidly evolving technologies, especially when the 'user' or 'deployer' is a large corporation and the 'creator' is a smaller entity or individual.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and some segments of the affected public attest that the problem of accountability for tech harms remains live. However, deployers and many legal scholars (outside the direct beneficiaries) argue that the current attribution model is overly simplistic and fails to account for the distributed nature of control and responsibility in modern tech ecosystems.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__developer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__developer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__developer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because developers bear significant costs (legal, insurance, R&D for safety) that are not fully offset by benefits, and deployers externalize risk. Suppression is also high (0.70) as developers are compelled by legal frameworks and market expectations to accept this liability, with limited options to shift it. Theater ratio is low (0.20) as the enforcement of this liability is quite real, though some 'safety' rhetoric may mask the underlying risk transfer. The increasing trend in extractiveness and suppression reflects the growing complexity and impact of technology, leading to more stringent liability demands on creators.
 *
 * PERSPECTIVAL GAP:
 *   Developers experience this as a snare, being the primary targets of liability with constrained exit options. Deployers, however, experience it as a rope or even a subsidy, as it externalizes significant risk to others. Regulatory bodies view it as a necessary enforcement mechanism. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and AI model creators are clear targets (high d) as they bear the primary costs. Deployers are beneficiaries (low d) as they offload risk. Regulatory bodies are agenda setters, benefiting from a clear enforcement target. The affected public is a beneficiary in principle (clear accountability) but often trapped in practice (difficulty of redress). Insurance providers are beneficiaries through market arbitrage.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; the problem of tech liability is live and growing. However, the *attribution principle* itself is contested. This specific reading (developer primary liability) prevents mislabeling the coordination function of 'accountability' as pure extraction by acknowledging the societal benefit of clear responsibility, but it computes as a snare for developers due to the asymmetric burden and constrained exit options. The alternative readings (deployer or shared liability) would shift the burden, potentially altering the classification for different seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developer_control_vs_liability,
    'To what extent do developers retain control over the deployment and use of their capabilities after creation, such that primary liability is a just attribution?',
    'Empirical studies of the ''control gap'' between creation and deployment, and legal analysis of contractual terms and open-source licensing models.',
    'If developer control is demonstrably low post-deployment, this reading''s justification weakens, potentially shifting liability towards deployers or a shared model. This would reduce extractiveness for developers and increase it for deployers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developer_control_vs_liability, empirical, 'Ambiguity regarding the alignment of control and liability.').

omega_variable(
    natural_law_vs_constructed_liability,
    'Is primary developer liability an inevitable consequence of creating a capability, or a constructed legal/regulatory choice that could be otherwise?',
    'Comparative legal analysis across jurisdictions with different liability regimes (e.g., strict liability vs. negligence, product liability vs. service liability).',
    'If it''s a constructed choice, the ''snare'' classification for developers is reinforced, highlighting the policy decision to concentrate risk. If it''s seen as an inevitable consequence, the ''mountain'' aspect of ''creator responsibility'' might be emphasized, though the extraction would still be present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_liability, conceptual, 'Whether liability attribution is a natural consequence or a policy choice.').

omega_variable(
    reading_comparison_impact,
    'How would the classification of this constraint change if the ''deployer_liability'' or ''shared_liability'' readings of the kernel were adopted?',
    'Constructing full constraint stories for ''deployer_liability'' and ''shared_liability'' and comparing their computed classifications for each stakeholder seat.',
    'Adopting ''deployer_liability'' would likely shift the ''snare'' classification from developers to deployers, making deployers victims and developers beneficiaries. ''Shared_liability'' would likely result in a ''tangled_rope'' for both, with more balanced extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_comparison_impact, conceptual, 'Impact of alternative kernel readings on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t2000, liability_attribution__developer_liability, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(liab_tr_t2008, liability_attribution__developer_liability, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(liab_tr_t2016, liability_attribution__developer_liability, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(liab_tr_t2024, liability_attribution__developer_liability, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(liab_be_t2000, liability_attribution__developer_liability, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(liab_be_t2008, liability_attribution__developer_liability, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(liab_be_t2016, liability_attribution__developer_liability, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement(liab_be_t2024, liability_attribution__developer_liability, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t2000, liability_attribution__developer_liability, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(liab_su_t2008, liability_attribution__developer_liability, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(liab_su_t2016, liability_attribution__developer_liability, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement(liab_su_t2024, liability_attribution__developer_liability, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, ai_safety_regulation).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, software_product_liability).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'liability_attribution' kernel. Its structural properties and classification are distinct from sibling readings like 'deployer_liability' and 'shared_liability', which attribute liability differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
