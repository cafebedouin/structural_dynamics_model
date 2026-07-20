% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer Primary Liability Attribution
 *   domain: technology_governance/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the developer_liability reading of the
 *   contested liability_attribution kernel: the legal rule that creators of
 *   underlying technological capabilities bear primary liability for
 *   downstream harms, regardless of deployment context. Deployers, who
 *   control the operational environment and user interactions, are
 *   structurally positioned as beneficiaries of risk externalization, while
 *   developersâoften with less visibility into downstream useâare
 *   positioned as liability targets. The claim/metric independence is
 *   deliberate: the constraint is claimed as tangled_rope because a genuine
 *   coordination function (safety incentives) coexists with asymmetric
 *   extraction (risk offloading), while the metrics describe substantial
 *   extractiveness and active enforcement.
 *
 * KEY AGENTS:
 *   - technology_developers: Primary target (moderate/constrained) â bears extraction via upstream liability
 *   - system_deployers: Primary beneficiary (powerful/mobile) â captures risk externalization and reduced legal exposure
 *   - regulatory_authorities: Agenda setter (institutional/analytical) â administratively convenient enforcement hook
 *   - shared_liability_advocates: Excluded voice (moderate/constrained) â argues for distributed attribution across the value chain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.71).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.69).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.71).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.69).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Primary Liability Attribution").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, 'a947a97e-deb4-4636-b92e-a77f8447c72f').
narrative_ontology:cs_kernel_codification('a947a97e-deb4-4636-b92e-a77f8447c72f', formalized).
narrative_ontology:cs_authority_grounding('a947a97e-deb4-4636-b92e-a77f8447c72f', lineage).
narrative_ontology:cs_interpretation_layer_present('a947a97e-deb4-4636-b92e-a77f8447c72f').
narrative_ontology:cs_reading_relation('a947a97e-deb4-4636-b92e-a77f8447c72f', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('a947a97e-deb4-4636-b92e-a77f8447c72f', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('a947a97e-deb4-4636-b92e-a77f8447c72f', foundational, creator_originary_liability).
narrative_ontology:cs_axiom_status(creator_originary_liability, holdable).
narrative_ontology:cs_axiom_grounding('a947a97e-deb4-4636-b92e-a77f8447c72f', creator_originary_liability, deontological).
narrative_ontology:cs_axiom('a947a97e-deb4-4636-b92e-a77f8447c72f', secondary, deployer_context_non_attenuating).
narrative_ontology:cs_axiom_status(deployer_context_non_attenuating, holdable).
narrative_ontology:cs_axiom_grounding('a947a97e-deb4-4636-b92e-a77f8447c72f', deployer_context_non_attenuating, conventional).
narrative_ontology:cs_reference_frame('a947a97e-deb4-4636-b92e-a77f8447c72f', developer_accountability_framework).
narrative_ontology:cs_drift_state('a947a97e-deb4-4636-b92e-a77f8447c72f', contemporary_ai_regulation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a947a97e-deb4-4636-b92e-a77f8447c72f', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, system_deployers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, technology_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and build foundational models, algorithms, and software capabilities. Under a developer-primary liability rule, they bear legal responsibility for harms arising from system use regardless of how deployers configure or operate the technology. They must invest heavily in defensive documentation, capability limitation, and compliance infrastructure. Exit requires abandoning capability development entirely, as liability attaches to the artifact itself.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, technology_developers, payer,
    moderate, biographical, constrained, global).

% Integrate, configure, and operate technology in specific real-world contexts. Under developer-primary liability, they inherit reduced or secondary legal exposure despite controlling the operational environment and user interactions. They capture the value of externalized risk, arbitraging across jurisdictions with favorable liability rules, while the upstream developer absorbs the expected litigation cost.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, system_deployers, beneficiary,
    powerful, biographical, mobile, global).

% Draft, interpret, and enforce liability frameworks for emerging technologies. They find developer-primary liability administratively tractable because it identifies a clear, early-point defendant and avoids complex discovery into deployment context. They face asymmetric lobbying pressure: deployers often prefer this framing, while developers lack comparable political organization.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Legal scholars, civil society advocates, and some jurists arguing that liability should be distributed across the value chain according to actual causal contribution and operational control. They contend deployment context is essential to harm prevention. They are structurally excluded from drafting tables when statutes or early precedent fix primary liability on developers by default.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, shared_liability_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, system_deployers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a direct, upstream incentive for developers to invest in safety, transparency, and controllability by internalizing the expected cost of harm caused by the capabilities they originate.
% TRANSFER_FUNCTION: Transfers legal and financial liability risk from deployers, who control the operational context, to developers, who create the underlying capability, moving expected litigation cost and compliance burden upstream in the value chain.
% ABSENT_VOICES: Shared-liability advocates and deployer-liability proponents argue that operational context and real-time control are essential to harm prevention; they are structurally excluded when primary liability is fixed on developers by statutory default or judicial precedent. Small developers without legal departments are underrepresented in regulatory drafting.
% DISAPPEARANCE_RATIONALE: If developer-primary liability vanished overnight, deployers would face reassessed risk exposure, insurance markets would reprice coverage along the value chain, developer product architectures would shift away from defensive limitation toward capability expansion, and regulatory enforcement would lose its primary administrative hook.
% FOUNDING_PROBLEM: The difficulty of attributing harm caused by complex socio-technical systems to a responsible party, combined with information asymmetry between creators and operators regarding system limits and the opacity of deployment contexts.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and tort scholars attest that causal attribution in socio-technical systems is genuinely difficult. However, civil society advocates and deployer-liability scholars attest the problem is being solved in the wrong directionâdeveloper liability is over-weighted relative to deployer controlâand that the attribution-difficulty narrative is partly deployed by deployers to externalize risk. Independent legal theorists outside the benefiting deployer class corroborate that attribution is hard but dispute that developer-primary is the natural resolution.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.71, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.71 at interval end) is high because the liability rule decouples risk-bearing from operational control: developers pay for harms they cannot prevent. Suppression (0.69) reflects the active legal machinery required to maintain this allocationâcourts, statutory interpretation, and preemption of alternative liability schemas. Theater ratio (0.42) indicates that a growing share of developer activity is defensive documentation and performative safety ritual rather than substantive capability improvement. The measurement series share a single time grid so temporal sampling is aligned across all tracked metrics.
 *
 * PERSPECTIVAL GAP:
 *   The developer seat experiences the constraint as unjust risk allocationâbeing held responsible for contexts they do not control. The deployer seat experiences it as efficient incentive alignment that happens to align with their risk-minimization interest. The regulatory seat experiences it as an administratively tractable enforcement target. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology_developers are structural targets: they bear costs without controlling the harm-generating context (high d). System_deployers are structural beneficiaries: they offload risk while retaining operational authority (low d). Regulatory authorities sit near symmetricâthey enforce the transfer but do not personally collect the extracted value. Shared_liability_advocates are excluded from the directional computation because they are structurally absent from the rule-setting process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâattribution difficulty in complex systemsâis genuine, but the developer-primary solution risks mandatrophy if it persists beyond its functional justification. If deployers develop sufficient operational monitoring and control capacity, fixing primary liability on developers becomes a zombie coordination story: it still claims to solve attribution, but the actual function is risk externalization to a less politically powerful party. The temporal measurements show rising extractiveness and theater, suggesting the coordination story is aging into extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developer_control_limit,
    'Can developers meaningfully control or monitor downstream deployment contexts sufficiently to prevent the harms for which they are held liable?',
    'Empirical study of developer visibility into deployer operations, combined with natural experiments from jurisdictions with shared or deployer-primary liability comparing safety outcomes.',
    'If developers lack meaningful control, the liability rule is primarily extractive risk-shifting rather than genuine safety coordination, supporting reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developer_control_limit, empirical, 'Whether developer liability tracks controllable design choices or offloads uncontrollable deployment risk.').

omega_variable(
    kernel_reading_ambiguity,
    'Is the liability_attribution kernel genuinely underdetermined, or does the developer_liability reading dominate because deployers capture the regulatory process?',
    'Comparative regulatory history tracing lobbyist influence and legislative drafting origins across jurisdictions with divergent liability rules.',
    'If deployer capture explains the reading''s dominance, the constraint is better understood as a snare disguised by legal formalism; if genuine theoretical disagreement explains it, tangled_rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this reading reflects open normative contestation or regulatory capture by deployer interests.').

omega_variable(
    deployer_benefit_driver,
    'Does the developer-primary liability reading persist because it optimally solves the attribution problem, or because deployers are structurally better positioned to lobby for risk externalization?',
    'Political economy analysis of liability reform campaigns, comparing deployer and developer lobbying expenditure against legislative outcomes.',
    'A deployer-driven persistence mechanism would confirm the extraction function dominates the coordination function, raising effective extraction and supporting snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployer_benefit_driver, empirical, 'Political economy of why the developer liability reading endures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.2).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__developer_liability, theater_ratio, 5, 0.26).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__developer_liability, theater_ratio, 10, 0.32).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__developer_liability, theater_ratio, 15, 0.36).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__developer_liability, theater_ratio, 20, 0.39).
narrative_ontology:measurement(liab_tr_t25, liability_attribution__developer_liability, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(liab_be_t5, liability_attribution__developer_liability, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(liab_be_t10, liability_attribution__developer_liability, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(liab_be_t15, liability_attribution__developer_liability, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(liab_be_t20, liability_attribution__developer_liability, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(liab_be_t25, liability_attribution__developer_liability, base_extractiveness, 25, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(liab_su_t5, liability_attribution__developer_liability, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(liab_su_t10, liability_attribution__developer_liability, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(liab_su_t15, liability_attribution__developer_liability, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(liab_su_t20, liability_attribution__developer_liability, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(liab_su_t25, liability_attribution__developer_liability, suppression_requirement, 25, 0.69).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% The liability_attribution kernel decomposes into three structurally distinct constraints (developer_liability, deployer_liability, shared_liability) because each reading assigns a different Îµ, a different beneficiary/victim structure, and a different directionality profile to the same natural-language concept. This story (developer_liability) is substantially extractive and contested; the sibling stories may carry different metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
