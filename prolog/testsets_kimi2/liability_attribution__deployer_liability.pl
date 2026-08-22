% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Deployer-Primary AI Liability Allocation
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint instantiates the deployer_liability reading of the
 *   liability_attribution kernel, a legal-regulatory design that assigns
 *   primary liability for AI harms to the party with deployment context
 *   control. It operates within technology governance and tort reform
 *   debates, positioning deployers as the accountable locus while shielding
 *   upstream developers and foundation model providers. The constraint is
 *   claimed as tangled_rope because it addresses a genuine coordination
 *   problemâuncertainty over who pays for AI harmâwhile asymmetrically
 *   extracting compliance and liability costs from deployers who did not
 *   create the underlying model.
 *
 * KEY AGENTS:
 *   - ai_deployers (payer): moderate power, constrained exit, national scope â bear primary liability and due diligence burdens for opaque upstream systems.
 *   - ai_developers (beneficiary): powerful, mobile exit, global scope â externalize downstream risk while retaining creative control.
 *   - foundation_model_providers (beneficiary): institutional, arbitrage exit, global scope â shielded from primary liability for deployment harms.
 *   - regulatory_authorities (agenda_setter): institutional, analytical exit, national scope â administer and enforce the liability allocation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.75).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.7).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.75).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer-Primary AI Liability Allocation").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '342148d1-cae9-4730-bf0a-9cd85812099e').
narrative_ontology:cs_kernel_codification('342148d1-cae9-4730-bf0a-9cd85812099e', formalized).
narrative_ontology:cs_authority_grounding('342148d1-cae9-4730-bf0a-9cd85812099e', lineage).
narrative_ontology:cs_interpretation_layer_present('342148d1-cae9-4730-bf0a-9cd85812099e').
narrative_ontology:cs_reading_relation('342148d1-cae9-4730-bf0a-9cd85812099e', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('342148d1-cae9-4730-bf0a-9cd85812099e', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('342148d1-cae9-4730-bf0a-9cd85812099e', foundational, deployer_context_control_generates_primary_duty).
narrative_ontology:cs_axiom_status(deployer_context_control_generates_primary_duty, holdable).
narrative_ontology:cs_axiom_grounding('342148d1-cae9-4730-bf0a-9cd85812099e', deployer_context_control_generates_primary_duty, conventional).
narrative_ontology:cs_axiom('342148d1-cae9-4730-bf0a-9cd85812099e', secondary, model_creation_without_deployment_control_limits_liability).
narrative_ontology:cs_axiom_status(model_creation_without_deployment_control_limits_liability, holdable).
narrative_ontology:cs_axiom_grounding('342148d1-cae9-4730-bf0a-9cd85812099e', model_creation_without_deployment_control_limits_liability, conventional).
narrative_ontology:cs_reference_frame('342148d1-cae9-4730-bf0a-9cd85812099e', deployer_control_liability_framework).
narrative_ontology:cs_drift_state('342148d1-cae9-4730-bf0a-9cd85812099e', contemporary_ai_regulation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('342148d1-cae9-4730-bf0a-9cd85812099e', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, ai_developers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, ai_deployers).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, deployment_context_control_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, decision_authority_liability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear primary legal liability for harms arising from AI systems they deploy, including opaque foundation models created by others. Must absorb due diligence costs, compliance overhead, and legal penalties, with limited ability to contractually shift liability back to upstream creators.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_deployers, payer,
    moderate, biographical, constrained, national).

% Create AI models and capabilities but are shielded from downstream deployment liability under this reading. Deployment risk is externalized to the deployer party, reducing exposure to civil and regulatory penalties for misuse or malfunction in contexts they do not control.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_developers, beneficiary,
    powerful, generational, mobile, global).

% Provide general-purpose foundation models to downstream deployers. Under this liability reading, they are insulated from primary liability for specific deployment harms, preserving revenue and scaling capacity while deployers assume the compliance burden.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Design and enforce the liability framework, designating deployers with deployment context control as the primary responsible party. They adjudicate penalties, define due diligence standards, and mediate disputes between upstream and downstream actors.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies who must answer for AI harms in specific deployment contexts by assigning primary liability to the party with operational control, reducing legal uncertainty for regulators, courts, and injured parties seeking redress.
% TRANSFER_FUNCTION: Moves liability risk, compliance cost, and due diligence burden from AI developers and foundation model providers to deployers, who must absorb insurance premiums, legal defense costs, and penalties for downstream harms.
% ABSENT_VOICES: Deployer trade associations contest the allocation but are overruled in the rulemaking process; end-users harmed by AI appear as plaintiffs but do not shape the liability allocation itself; legal scholars advocating for developer-primary or strict product-liability regimes are present in discourse but lose under this specific reading.
% DISAPPEARANCE_RATIONALE: If deployers no longer bore primary liability, risk would flow back to developers and foundation model providers, deployment insurance markets would reprice, due diligence spending would shift from deployment to design phase, and regulatory enforcement targets would migrate upstream.
% FOUNDING_PROBLEM: Uncertainty about who is legally responsible when general-purpose AI systems cause harm in specific contexts, particularly when the model creator is far removed from the deployment decision and the deployer controls the use environment.
% FOUNDING_PROBLEM_CORROBORATION: Administrative agency economists and independent law review analyses attest that the attribution problem is genuine and unresolved; deployer associations corroborate the problem's existence but contest this specific allocation as the solution.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.75, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness rises from 0.35 to 0.75 over the interval as the liability framework hardens from proposal to enforceable regulation and case law, concentrating liability on deployers. Suppression requirement tracks enforcement infrastructure maturation, rising to 0.70 as courts and regulators build capacity to penalize non-compliant deployers. Theater ratio remains moderate (0.30) because the accountability function is partially real, though due diligence requirements risk becoming performative when deployers lack access to upstream model documentation. Accessibility collapse (0.60) reflects that alternative liability allocations (developer-primary, shared) remain legally thinkable but are institutionally disadvantaged by this reading's ascendancy. Resistance (0.55) captures organized deployer opposition and industry lobbying against the allocation.
 *
 * PERSPECTIVAL GAP:
 *   From the deployer seat, the constraint reads as extraction: they are made liable for systems they did not design and cannot fully audit, paying costs that accrue to upstream parties. From the developer and foundation model provider seats, the constraint reads as justified coordination that clarifies risk boundaries and preserves innovation incentives. From the regulatory seat, it appears as an efficient governance mechanism that targets the party closest to the harm event. The engine computes this divergence from the structural data: deployers are victims with constrained exit (high d), while upstream actors are beneficiaries with mobile or arbitrage exit (low d).
 *
 * DIRECTIONALITY LOGIC:
 *   Deployers are structurally targeted: they are declared victims, bear the compliance costs, and have constrained exit options because ceasing deployment means exiting the market. Their directionality sits near the full-target end. Developers and foundation model providers are structural beneficiaries: they collect risk avoidance, have global mobility and jurisdictional arbitrage options, and their directionality sits near the beneficiary end. Regulatory authorities are near-symmetric; they neither collect rents nor bear liability, functioning as the enforcement instrument.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents the false binary of labeling this either a pure rope (genuine coordination only) or a pure snare (extraction only). The founding problemâuncertainty over AI liability attributionâis live, corroborated by outside legal and economic analysis, so the coordination function is not a cover story. However, the specific allocation to deployers is contested, asymmetrically benefits upstream creators, and requires active legal enforcement to hold, satisfying the tangled_rope gate. A snare classification would ignore the genuine accountability coordination; a rope classification would ignore the externalization of risk to a structurally weaker seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence,
    'Does the deployer''s control over deployment context sufficiently distinguish their causal contribution from the developer''s such that primary liability should attach only to the deployer, or is this reading an artificial severance of a continuous causal chain?',
    'Comparative legal analysis across jurisdictions adopting deployer-primary, developer-primary, and shared liability regimes, measuring outcomes for harm reduction, innovation incentives, and compliance costs.',
    'If the causal chain is continuous and inseparable, the deployer_liability reading functions as an extractive severance benefiting upstream creators; if control is genuinely separable, the reading is structurally justified as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural ambiguity between deployer liability as justified coordination versus artificial risk severance').

omega_variable(
    opacity_due_diligence_burden,
    'Can deployers meaningfully discharge a due diligence duty when foundation model providers withhold training data, architecture details, and evaluation protocols?',
    'Empirical assessment of deployer due diligence capacity under current opacity; natural experiment from transparency mandates or safe-harbor disclosures.',
    'If deployers cannot access necessary information, the due diligence burden becomes performative, theater_ratio rises, and the constraint slides toward snare; if transparency is achievable, the burden remains a genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_due_diligence_burden, empirical, 'Whether due diligence burden is functional or performative under upstream opacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__deployer_liability, theater_ratio, 4, 0.18).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__deployer_liability, theater_ratio, 8, 0.22).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.25).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__deployer_liability, theater_ratio, 16, 0.28).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__deployer_liability, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(liab_be_t4, liability_attribution__deployer_liability, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(liab_be_t8, liability_attribution__deployer_liability, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(liab_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(liab_be_t16, liability_attribution__deployer_liability, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(liab_be_t20, liability_attribution__deployer_liability, base_extractiveness, 20, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(liab_su_t4, liability_attribution__deployer_liability, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(liab_su_t8, liability_attribution__deployer_liability, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(liab_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(liab_su_t16, liability_attribution__deployer_liability, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(liab_su_t20, liability_attribution__deployer_liability, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This constraint is the deployer_liability reading of the liability_attribution kernel. It is structurally distinct from the developer_liability reading (which extracts from creators) and the shared_liability reading (which moderates extraction across the chain). The epsilon values differ because this reading externalizes deployment risk to the deployer seat, whereas the developer reading concentrates extraction on creators and the shared reading distributes it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
