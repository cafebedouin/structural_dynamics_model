% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer-Primary Liability for AI System Harms
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint story captures the 'developer-primary liability' reading
 *   of the contested liability_attribution kernel. Under this reading, legal
 *   and regulatory frameworks assign primary responsibility for AI system
 *   harms to the entities that create and release the underlying models,
 *   regardless of deployment context. The constraint operates through tort
 *   law evolution (strict liability for ultrahazardous activities), statutory
 *   regimes (EU AI Act provider obligations), and contractual cascades
 *   (cloud/platform terms of service). Developers bear compliance costs,
 *   insurance premiums, and litigation risk that scale with model capability;
 *   deployers capture economic upside while externalizing tail risk. The
 *   opacity burden — documenting model limitations, training data provenance,
 *   and capability boundaries — falls on developers as a condition of
 *   release.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.62).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer-Primary Liability for AI System Harms").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '3b5992a0-5363-47ba-822b-b81c4641d08b').
narrative_ontology:cs_kernel_codification('3b5992a0-5363-47ba-822b-b81c4641d08b', formalized).
narrative_ontology:cs_authority_grounding('3b5992a0-5363-47ba-822b-b81c4641d08b', lineage).
narrative_ontology:cs_interpretation_layer_present('3b5992a0-5363-47ba-822b-b81c4641d08b').
narrative_ontology:cs_reading_relation('3b5992a0-5363-47ba-822b-b81c4641d08b', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('3b5992a0-5363-47ba-822b-b81c4641d08b', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('3b5992a0-5363-47ba-822b-b81c4641d08b', foundational, creator_bears_primary_responsibility).
narrative_ontology:cs_axiom_status(creator_bears_primary_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('3b5992a0-5363-47ba-822b-b81c4641d08b', creator_bears_primary_responsibility, deontological).
narrative_ontology:cs_axiom('3b5992a0-5363-47ba-822b-b81c4641d08b', foundational, capability_creation_entails_liability).
narrative_ontology:cs_axiom_status(capability_creation_entails_liability, holdable).
narrative_ontology:cs_axiom_grounding('3b5992a0-5363-47ba-822b-b81c4641d08b', capability_creation_entails_liability, conventional).
narrative_ontology:cs_reference_frame('3b5992a0-5363-47ba-822b-b81c4641d08b', creator_liability_framework).
narrative_ontology:cs_drift_state('3b5992a0-5363-47ba-822b-b81c4641d08b', contemporary_ai_governance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3b5992a0-5363-47ba-822b-b81c4641d08b', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, enterprise_adopters).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, platform_operators).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, model_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_maintainers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, research_labs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, open_source_maintainers).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, creator_responsibility_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, proximate_causation_in_ai_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and release foundation models and AI systems. Bear primary legal liability for downstream harms regardless of deployment context they cannot control. Face uncertain tort exposure, regulatory fines, and compliance costs that scale with model capability rather than deployment decisions. Exit requires abandoning the field or accepting liability caps that jurisdictions may not honor.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, model_developers, payer,
    organized, biographical, constrained, global).

% Release model weights and code openly. Gain community recognition and ecosystem adoption (beneficiary) but absorb unbounded liability surface when downstream actors deploy without safeguards (payer). Professional identity is fused to open release norms; stopping releases feels like abandoning the field's core ethic. Cannot practically monitor or control thousands of downstream deployments.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_maintainers, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, open_source_maintainers, beneficiary).

% Produce frontier capabilities with large compute budgets. Face escalating liability insurance premiums, mandatory safety testing regimes, and potential strict liability statutes. Their institutional mission (advancing capability) directly creates the liability surface. Can lobby and shape standards but cannot exit the liability regime without ceasing frontier research.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, research_labs, payer,
    institutional, generational, constrained, global).

% Integrate models into products and services. Make deployment-time decisions about guardrails, use cases, and user populations. Under developer-primary liability, they externalize the tail risk of catastrophic harm to model creators while capturing the economic upside. Can switch model providers if liability terms become unfavorable; their exit is low-friction compared to developers.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deployers, beneficiary,
    powerful, biographical, mobile, global).

% Large corporations adopting AI for internal workflows and customer-facing products. Benefit from liability regimes that place burden on upstream model providers rather than on integration decisions. Have legal teams to negotiate indemnification clauses and jurisdiction-shop for favorable terms. Their scale gives them arbitration-grade exit across model ecosystems.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, enterprise_adopters, beneficiary,
    institutional, biographical, arbitrage, global).

% Host model marketplaces and deployment infrastructure (e.g., cloud providers, API platforms). Set terms of service that cascade liability upstream to developers while taking platform fees. Shape regulatory dialogue through lobbying and standard-setting bodies. Can shift liability allocation via contract and technical architecture (e.g., mandatory safety filters).
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, platform_operators, agenda_setter).

% Design and enforce liability frameworks through statutes (EU AI Act, US executive orders, tort law evolution). Face pressure to assign liability to the most identifiable, deep-pocketed actor (developers) rather than the diffuse deployment chain. Their decisions create the constraint's enforcement machinery and define the opacity burden.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulators_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Experience AI harms (bias, accidents, manipulation, job displacement) but have no seat in liability regime design. Would argue for broad, accessible compensation but are structurally excluded from legislative and judicial processes that allocate liability. Their exit is impossible — they live in the deployment environment regardless of who pays.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, affected_public, excluded,
    powerless, immediate, trapped, global).

% Price and underwrite AI liability risk. Their actuarial models feed into developer premiums and deployer coverage terms. They observe the constraint's operation from a risk-pricing perspective and can withdraw capacity if liability regimes become uninsurable, effectively hardening the constraint.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, insurers_reinsurers, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates responsibility for AI system harms when multiple actors (developers, deployers, users) contribute to the causal chain. Solves the 'who pays when the model fails' problem by designating a primary liable party.
% TRANSFER_FUNCTION: Moves the financial and legal burden of AI harms from deployers (who choose use context, user population, guardrails) to developers (who created the capability but lack deployment control). Transfers risk downstream-to-upstream against the gradient of operational control.
% ABSENT_VOICES: Affected publics who bear harm but cannot access compensation mechanisms; small deployers and startups who lack negotiating power for indemnification; Global South jurisdictions whose liability frameworks are being shaped by EU/US precedent without their input.
% DISAPPEARANCE_RATIONALE: If developer-primary liability vanished overnight, deployers would face immediate liability exposure for harms they currently externalize. Insurance markets would reprice deployer policies upward. Developers would shift to stricter licensing, capability gating, or closed releases. The entire AI deployment economics would reorganize around deployer-borne risk.
% FOUNDING_PROBLEM: Early AI governance debates identified a 'responsibility gap': when an autonomous system causes harm, neither the user nor the developer clearly bears responsibility because the system's behavior emerges from training, not explicit programming. Developer-primary liability was proposed to close this gap by anchoring accountability at the capability's origin.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by early AI safety literature (e.g., Amodei et al. 2016 'Concrete Problems in AI Safety') and policy white papers from 2017-2020. However, legal scholars (e.g., Selbst 2020, EU Parliament studies 2021) contest whether the responsibility gap still exists given modern deployment control tools, and deployer-side advocates argue the gap was always a mischaracterization that served to externalize risk.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is substantial because developers absorb liability that is causally shaped by deployment decisions they cannot control. Suppression (0.62) is moderate-high: the constraint persists through active enforcement (courts, regulators, platform contracts) and suppresses alternative liability allocations (deployer-primary, shared). Theater ratio (0.28) reflects that safety testing and documentation requirements serve genuine coordination (transparency, risk assessment) but increasingly perform liability-defensibility rather than reduce harm. Accessibility collapse (0.45) is moderate — alternative liability frameworks exist conceptually (shared, deployer-primary) but are politically and legally difficult to instantiate. Resistance (0.55) is significant: developer coalitions, open-source communities, and some jurisdictions push back against unbounded upstream liability.
 *
 * PERSPECTIVAL GAP:
 *   From the developer seat, this constraint is a snare-like extraction: they pay for harms they cannot prevent. From the deployer seat, it is a rope-like coordination: liability is cleanly allocated, enabling deployment without negotiating per-model indemnification. From the regulator seat, it is a scaffold: a transitional rule that may evolve toward shared liability as deployment control tools mature. The engine computes these per-seat classifications from the structural data; this commentary explains the structural asymmetry driving the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers (model_developers, open_source_maintainers, research_labs) are structural payers: they bear costs disproportionate to their control over harm pathways. Deployers (deployers, enterprise_adopters, platform_operators) are structural beneficiaries: they externalize tail risk while controlling deployment context. Regulators_courts are agenda_setters who write and enforce the rules. Affected_public are excluded — they experience harm but have no voice in liability allocation. Insurers_reinsurers observe and price the risk. The directionality derivation from beneficiary/victim declarations plus exit options (developers: constrained/identity_locked; deployers: mobile/arbitrage) produces the expected d-gradient: developers near full target (d→1.0), deployers near beneficiary (d→0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (responsibility gap for autonomous systems) is contested — modern deployment tooling (guardrails, monitoring, fine-tuning) gives deployers significant control over harm pathways, undermining the original justification for developer-primary liability. Yet the constraint persists and intensifies (rising extractiveness, suppression). This suggests mandatrophy: the constraint's mandate has atrophied but the arrangement persists because deployers benefit from externalized risk and regulators prefer the administrative simplicity of a single liable party. The constraint is not a pure snare (there is genuine coordination in having a clear liability anchor) but the coordination function is weakening relative to the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is developer-primary liability a stable legal regime or one reading of a contested kernel that could shift to deployer-primary or shared liability?',
    'Track legislative developments (EU AI Act implementation, US federal AI legislation), tort law evolution (first major AI harm strict liability rulings), and insurance market signals (whether deployer policies start pricing tail risk). A regime shift would be signaled by multiple jurisdictions adopting shared liability statutes or landmark rulings allocating liability to deployment decisions.',
    'If the kernel shifts to shared or deployer-primary liability, this constraint''s extractiveness drops sharply (developers no longer sole payers) and its type may shift from tangled_rope toward rope or scaffold. The current high extractiveness is reading-contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this constraint represents a stable legal equilibrium or a contingent reading of a contested liability kernel.').

omega_variable(
    opacity_burden_allocation,
    'Is the developer''s opacity burden (documenting limitations, provenance, capabilities) a genuine coordination cost or an extractive transfer of due diligence from deployers?',
    'Compare documentation requirements under developer-primary vs. shared liability regimes. If deployers in shared-liability jurisdictions produce equivalent documentation voluntarily, the burden is coordination; if they produce less, the developer-primary regime transfers due diligence upstream.',
    'If the opacity burden is extractive transfer, measured extractiveness understates true extraction (documentation cost is hidden compliance burden). If coordination, the burden is part of the genuine function and extractiveness is accurately measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opacity_burden_allocation, empirical, 'Whether documentation and transparency obligations are genuine coordination overhead or extractive cost-shifting.').

omega_variable(
    open_source_liability_surface,
    'Does unbounded liability for open-weight releases create a structural pressure toward closed development that the constraint''s coordination function does not justify?',
    'Measure open-weight release rates and capability gaps between open and closed models over the interval. If open releases decline sharply while capability gaps widen, the liability regime is suppressing a coordination channel (open research) without proportional safety gain.',
    'If open-source suppression is disproportionate, the constraint''s theater ratio is understated (safety theater masks innovation suppression) and its type drifts toward snare. The identity_locked exit of open_source_maintainers amplifies this effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_liability_surface, empirical, 'Whether liability exposure for open releases suppresses beneficial coordination (open research) beyond what safety justifies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_dev_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liab_dev_tr_t2, liability_attribution__developer_liability, theater_ratio, 2, 0.18).
narrative_ontology:measurement(liab_dev_tr_t4, liability_attribution__developer_liability, theater_ratio, 4, 0.22).
narrative_ontology:measurement(liab_dev_tr_t6, liability_attribution__developer_liability, theater_ratio, 6, 0.25).
narrative_ontology:measurement(liab_dev_tr_t8, liability_attribution__developer_liability, theater_ratio, 8, 0.27).
narrative_ontology:measurement(liab_dev_tr_t10, liability_attribution__developer_liability, theater_ratio, 10, 0.28).

% Extraction over time
narrative_ontology:measurement(liab_dev_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(liab_dev_be_t2, liability_attribution__developer_liability, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(liab_dev_be_t4, liability_attribution__developer_liability, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(liab_dev_be_t6, liability_attribution__developer_liability, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(liab_dev_be_t8, liability_attribution__developer_liability, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(liab_dev_be_t10, liability_attribution__developer_liability, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(liab_dev_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(liab_dev_su_t2, liability_attribution__developer_liability, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(liab_dev_su_t4, liability_attribution__developer_liability, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(liab_dev_su_t6, liability_attribution__developer_liability, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(liab_dev_su_t8, liability_attribution__developer_liability, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(liab_dev_su_t10, liability_attribution__developer_liability, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__developer_liability, 0.12).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, ai_act_provider_obligations).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, foundation_model_insurance_market).

% DUAL FORMULATION NOTE:
% This constraint is one member of the liability_attribution constraint family (kernel: liability_attribution). The three readings — developer_liability, deployer_liability, shared_liability — are structurally distinct constraints with different ε, beneficiary/victim structures, and types. They are linked via affects_constraints. The developer_liability reading has ε=0.68 (substantial extraction from developers); the deployer_liability reading would have ε≈0.45 (lower extraction, deployers internalize deployment risk); the shared_liability reading would have ε≈0.35 (distributed extraction, closer to rope). The ε-invariance principle requires separate stories because the referent (the standing liability arrangement) is evaluated differently under each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__developer_liability, organized, 0.85).
constraint_indexing:directionality_override(liability_attribution__developer_liability, moderate, 0.9).
constraint_indexing:directionality_override(liability_attribution__developer_liability, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
