% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Joint Liability Attribution Distributed by Causal Contribution and Control
 *   domain: legal/technology/governance
 *
 * SUMMARY:
 *   Under the shared-liability reading of the liability-attribution kernel,
 *   both software developers and deployment operators are jointly liable for
 *   harms arising from deployed systems, with liability distributed according
 *   to causal contribution (who created the capability) and control (who made
 *   deployment decisions). This reading distributes responsibility across the
 *   value chain and creates coordination costs through required causal
 *   analysis, contractual allocation, and insurance-market emergence. It is
 *   one of three readings in the contested kernel: the deployer_liability
 *   reading assigns primary responsibility to operators (as controllers of
 *   deployment context); the developer_liability reading assigns it to
 *   developers (as creators of capability). This story instantiates ONLY the
 *   shared-liability reading as a clean ε-invariant constraint, according to
 *   Kernel Rule 1. The shared-liability frame emerges as the doctrine that
 *   holds both parties accountable according to their structural influence on
 *   the harm, rejecting single-party immunity claims.
 *
 * KEY AGENTS:
 *   - Software developers: moderate power, identity-locked exit (professional identity fused to coding); targeted by liability exposure for code defects
 *   - Deployment operators: institutional power, constrained exit (cannot easily exit from deploying software); targeted by liability exposure for negligent operation and failure to patch
 *   - Injured parties: powerless but with multiple litigation pathways; benefits from expanded defendant set (beneficiaries, though bearing increased discovery complexity)
 *   - Insurance markets: institutional beneficiary; new product categories and premium volume from allocation instruments and indemnification demand
 *   - Legal system and courts: analytical beneficiary; gains clarity from causal-contribution principle at cost of complex fact-finding in each case
 *   - Technology companies (high-opacity developer-operators): institutional power, trapped exit; bears coordination costs from formalizing internal accountability and liability allocation
 *   - Excluded regulators: sector regulators whose public-safety mandates are absent from the private liability allocation process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.68).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.72).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Joint Liability Attribution Distributed by Causal Contribution and Control").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "legal/technology/governance").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '288c09da-b8f2-4540-9a68-9c90fca90f9c').
narrative_ontology:cs_kernel_codification('288c09da-b8f2-4540-9a68-9c90fca90f9c', distributed).
narrative_ontology:cs_authority_grounding('288c09da-b8f2-4540-9a68-9c90fca90f9c', extraction).
narrative_ontology:cs_interpretation_layer_present('288c09da-b8f2-4540-9a68-9c90fca90f9c').
narrative_ontology:cs_reading_relation('288c09da-b8f2-4540-9a68-9c90fca90f9c', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('288c09da-b8f2-4540-9a68-9c90fca90f9c', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_axiom('288c09da-b8f2-4540-9a68-9c90fca90f9c', foundational, causation_and_control_are_verifiable).
narrative_ontology:cs_axiom_status(causation_and_control_are_verifiable, holdable).
narrative_ontology:cs_axiom_grounding('288c09da-b8f2-4540-9a68-9c90fca90f9c', causation_and_control_are_verifiable, empirically_contingent).
narrative_ontology:cs_axiom('288c09da-b8f2-4540-9a68-9c90fca90f9c', secondary, liability_exposure_drives_incentive_alignment).
narrative_ontology:cs_axiom_status(liability_exposure_drives_incentive_alignment, holdable).
narrative_ontology:cs_axiom_grounding('288c09da-b8f2-4540-9a68-9c90fca90f9c', liability_exposure_drives_incentive_alignment, instrumental).
narrative_ontology:cs_reference_frame('288c09da-b8f2-4540-9a68-9c90fca90f9c', causation_and_control_based_allocation).
narrative_ontology:cs_drift_state('288c09da-b8f2-4540-9a68-9c90fca90f9c', contemporary_insurance_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('288c09da-b8f2-4540-9a68-9c90fca90f9c', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, injured_parties_litigation_pathways).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_markets).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, legal_system_clarity_seekers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, software_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, deployment_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, legal_system_and_courts).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, technology_companies_internal_developer_operators).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, contracting_parties_allocation_negotiators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and release code that becomes embedded in deployed systems operated by others. Under shared liability, they are held jointly responsible for harms arising from their code even when the deployment context, operational decisions, and configuration choices were entirely outside their control or visibility. They must invest in defensive coding practices, maintain professional liability insurance, and participate in contractual allocation discussions with deployers who control the operational context. Their professional identity is inseparable from their code; exiting the market means abandoning a career, not merely switching projects. They bear the cost of proving their causal contribution relative to deployer negligence, a burden complicated by the asymmetric information (they cannot see how their code is deployed).
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, software_developers, payer,
    moderate, biographical, identity_locked, global).

% Deploy and operate software systems in production, making decisions about access controls, network configuration, patching schedules, and isolation from other systems. Under shared liability, they share responsibility with developers even when the code is defective or the developers failed to patch known vulnerabilities, because operators control whether the code runs, who accesses it, and how it is monitored. They must negotiate liability allocation with developers (extracting indemnification or co-insurance arrangements), maintain insurance covering deployed external code, and prove in disputes that harms resulted from developer negligence rather than operational failures. They cannot easily exit the market for affected software categories without restructuring entire systems; they are trapped by installed bases of deployed code.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, deployment_operators, payer,
    institutional, generational, constrained, global).

% Parties harmed by malfunction, security breach, or misuse of deployed software systems. Shared liability expands their potential defendant set: they can sue developers for code defects, operators for negligent deployment or failure to patch, or both. This multiplicity increases the probability of finding a solvent defendant who can satisfy a judgment. However, discovering the causal chain and proving each party's contribution requires more extensive evidence-gathering; they must invest in expert analysis to attribute causation rather than simply proving injury, and they face arguments from multiple defendants about allocation of responsibility.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, injured_parties_litigation_pathways, beneficiary,
    powerless, immediate, mobile, national).

% Professional liability insurance for software development and technology operations. Shared liability creates demand for new product categories: coverage for developers against operator misuse (protecting developer exposure when operators deploy negligently), coverage for operators against developer defects (protecting operator exposure when code is fundamentally broken), joint and several coverage allowing contractual allocation, and hold-harmless instruments enabling parties to assign liability to counterparties. They benefit from premium volume growth, underwriting complexity (which creates value capture), and the information opacity (defect rates, deployment contexts) that makes precise risk pricing impossible—opacity allows wider premium spreads.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_markets, beneficiary,
    institutional, biographical, arbitrage, global).

% Courts, regulatory bodies, and legal scholars tasked with resolving disputes about liability and establishing coherent doctrine. Shared liability provides a framework coherent with causal reasoning: rather than assigning all responsibility to one party regardless of actual influence, the framework asks who contributed to the harm and who controlled the circumstances in which the capability was deployed. This analytical coherence comes at the cost of higher case volume and complexity—each dispute requires detailed fact-finding about causation and control, creating precedent-rich litigation. The legal system benefits from the principled clarity of causation-based allocation even as it bears the cost of expanded dispute complexity.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, legal_system_and_courts, beneficiary,
    institutional, generational, analytical, national).

% Large technology companies where developers and operators work for the same institution (internal developer-operator relationships). Shared liability forces them to formalize internal accountability structures, conduct detailed causal analysis within the organization, and establish liability allocation across departments. They set the standard for enforcement through internal governance structures and contractual templates that internal teams must negotiate. They are trapped—unable to exit the constraint's scope—and must absorb the coordination costs of proving causation internally, maintaining clear records of control and responsibility, and potentially pursuing subrogation claims between internal departments.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, technology_companies_internal_developer_operators, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, technology_companies_internal_developer_operators, agenda_setter).

% Parties (developers and operators) entering contractual relationships who must now explicitly negotiate allocation of liability between code providers and system operators. Shared liability distributes the opacity burden—the work of causal analysis and control documentation—onto the parties themselves. They must invest in contract negotiation, forensic capabilities to trace what actually caused harms, documentation of who controlled deployment decisions, and ongoing relationship maintenance to manage evolving risk allocations. They cannot exit without finding counterparties already bearing liability allocation costs, or withdrawing from the software ecosystem. The constraint extracts the cost of coordinating around opacity from the parties who must manage the relationship.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, contracting_parties_allocation_negotiators, payer,
    moderate, biographical, constrained, global).

% Sector regulators (financial services, healthcare, critical infrastructure) tasked with holding operators accountable for deployed systems. Shared liability distributes responsibility across the value chain, which can obscure their oversight and dilute the direct accountability pressure they can exert on operators. A regulator may want to hold the operator solely liable (clear accountability for deployer), but the shared-liability frame distributes fault to developers, diluting the pressure the regulator can bring on the operator. Regulators are not at the table during developer-operator contracting and must infer liability allocation from litigation discovery or post-facto incident investigation. Their public-safety mandates are structurally excluded from the private liability negotiation.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, excluded_sector_regulators, excluded,
    institutional, generational, analytical, national).

% Doctrinal legal scholars, empirical researchers, and analytical observers studying how liability attribution operates in practice in technology systems. Can observe from outside the enforcement structure to measure whether the shared-liability framing is structurally coherent or whether power asymmetries systematically collapse it; whether causal attribution is verifiable or merely theoretical; whether the coordination function (distributing responsibility coherently) is stronger than the extraction function (imposing costs for opacity management).
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, analytical_legal_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, insurance_markets).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates liability across the value chain (developers and operators) according to causal contribution and control, attempting to align incentive exposure with actual influence over harm causation. Creates structured frameworks for contractual allocation, enabling private-market solutions to causal-attribution problems. Establishes markets for professional liability insurance, indemnification instruments, and causal-forensics services. Allows injured parties to pursue multiple defendants rather than seeking single points of accountability.
% TRANSFER_FUNCTION: Transfers the cost of causal-attribution work from courts (post-harm fact-finding) to the parties themselves (ex-ante contracting and documentation). Shifts liability exposure from concentrated defendant sets (either developer or operator alone) to distributed responsibility along the code-to-operation causal chain. Moves premium volume and underwriting complexity to insurance markets. Distributes the burden of opacity management—the work of documenting causation, control, and defect rates—onto developers and operators who must disclose information to defend their positions.
% ABSENT_VOICES: End users who operate or interact with deployed systems but have no seat at the liability allocation table, despite their usage patterns defining what 'normal operation' and 'misuse' mean for causal analysis. Sector regulators (financial, healthcare, critical infrastructure) whose public-safety mandates and accountability frameworks are excluded from private developer-operator contracting but who must enforce safety standards in deployed systems. Cybersecurity researchers and threat-modeling communities whose causal analysis of security failures is not systematized into official liability allocation. Developers and operators in smaller markets or jurisdictions with less sophisticated dispute-resolution infrastructure, who bear the same liability exposure but lack access to insurance and legal contracting expertise.
% DISAPPEARANCE_RATIONALE: If the shared-liability framework vanished, the entire developer-operator-insurer ecosystem would collapse back to single-party liability doctrine (either developer or deployer). Developers would either withdraw from high-risk sectors, demand complete pre-deployment indemnification (changing pricing and availability), or accept full liability and raise insurance costs across all software. Operators would either accept full liability and restructure operational practices to reduce risk, or demand developers provide guarantees equivalent to full indemnification. Insurance products would simplify to single-party coverage rather than complex allocation instruments. The legal landscape would abandon causal-chain analysis in favor of simpler single-defendant doctrines. The coordination function (distributing liability coherently) would vanish; what would emerge instead is institutional concentration of liability on the party with larger legal budget.
% FOUNDING_PROBLEM: Early software liability doctrine could not handle distributed causation: code defects matter only when deployed, but deployment negligence matters only when code is defective. Neither developers nor operators can unilaterally control whether harms occur; both influence the outcome. Assigning all responsibility to the developer lets operators deploy with negligence; assigning all to the operator lets developers ship defective code. Injured parties faced either unsolvent defendants (the small developer cannot pay) or defendants with no actual control over the deployment context (the developer cannot patch if the operator never applies patches). The founding problem is establishing a liability allocation method that distributes exposure in proportion to actual causal influence and control.
% FOUNDING_PROBLEM_CORROBORATION: Technology incident analysis (post-mortems of security breaches, software failures, system outages) consistently attributes causation to combinations of developer defect and operator failure to patch, configure safely, or monitor. Litigation discovery in software-related injury cases (data breach claims, fraud claims involving algorithmic failures, product liability for embedded systems) routinely surfaces that neither party alone caused the harm; causation is genuinely distributed. Insurance actuarial data shows that defect rates (developer signal) and deployment-failure rates (operator signal) are statistically independent risk factors, supporting the premise that both parties' practices matter. These attestations come from outside the liable parties: accident investigators, insurance underwriters, regulatory bodies analyzing incident reports, and independent security researchers analyzing publicly disclosed breaches. Court opinions in technology liability cases increasingly acknowledge that single-party liability fails to capture the actual causal structure.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 at t=0 to 0.68 at t=35, plateauing in the final interval. The rise reflects accumulating costs as parties develop practices for causal analysis, contracting overhead, and insurance-market premiums. Theater-ratio rise (0.18→0.41) shows that while causal analysis remains functional, an increasing share of activity is defensive compliance—forensic preparation for potential disputes, formalized record-keeping, liability management within organizations—rather than actual harm reduction. Suppression rises (0.55→0.72) as the requirement to prove causal contribution and control creates asymmetric information burdens: developers must disclose code complexity and defect rates; operators must disclose deployment context and control decisions. The threat of liability exposure suppresses parties' willingness to share information and constrains their freedom to experiment with novel deployment patterns. The plateau at t=30+ reflects maturation of the regime: allocation practices stabilize, insurance products standardize, and parties develop defensive routines that maintain suppression at steady state. The metrics are authored to reflect this real trajectory, independent of the claimed type; the engine determines whether the classification fits.
 *
 * PERSPECTIVAL GAP:
 *   Developers perceive the constraint as unfair (they are liable for harms caused by operator negligence they could not control); operators perceive it as forcing them to subsidize developer risk through insurance costs and internal governance. Injured parties perceive it as beneficial (multiple defendant paths increase recovery chances) but bear increased discovery costs. The legal system perceives it as intellectually coherent (causation and control are verifiable principles) but as creating case-volume explosion and fact-finding burden. These gaps emerge from the structural asymmetries: developers lack operational context; operators lack visibility into code; injured parties lack causal information from both. The shared-liability frame distributes the opacity burden across all parties, leaving each partially exposed. From the deployer-liability reading (sibling), the obligation on developers to share causal information is an unjust extraction; from the developer-liability reading (other sibling), the obligation on operators to prove control is an evasion of creator responsibility. This story's reading is that both should be liable according to their actual influence, which requires the burden distribution sketched here.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers are payers: they bear liability exposure for code defects even when deployment decisions were entirely outside their control. Their exit is identity-locked because professional coding practice is inseparable from their identity; they cannot withdraw without career loss. Directionality is high (toward full target, ~0.85). Deployment operators are payers: they bear liability for operational negligence and bear costs of contracting for allocation of developer-created risks. Their exit is constrained (they can restructure deployment practices but cannot fully avoid deploying external software). Directionality is high (~0.75). Both are targets, making this a Tangled Rope: there is a genuine coordination function (distributing liability coherently across the causal chain) AND asymmetric extraction (both parties are constrained and pay costs but neither is a pure beneficiary of the constraint itself—the beneficiaries are injured-parties' litigation pathways, insurance markets, and the legal system). Injured parties are beneficiaries but also bear costs (complexity of multi-defendant litigation). This asymmetric benefit/cost structure across the payer set confirms Tangled Rope classification. The directive overrides are not needed; structural derivation produces correct directionality from the base beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The shared-liability reading's founding problem (allocating liability coherently across causal contribution and control) is live and substantive—not every actual incident maps cleanly to single-party causation, so the framework's core function remains operative. However, the theater-ratio rise (0.18→0.41) indicates that formalized causal-analysis practices are increasingly decoupled from actual harm reduction. Parties invest in forensic record-keeping and defensive positioning more to manage liability exposure than to improve code safety or operational resilience. The constraint is not mandatrophic; it has not outlived its function. But the measurements show drift toward ritualized compliance: legal conformity overshoots functional learning. The divergence between claimed_type (tangled_rope) and the authored metrics does not indicate mandatrophy; it indicates that the rope is working but under increasing regulatory burden. The Tangled Rope classification survives intact: coordination function (causal attribution) remains; extraction (litigation costs, insurance premiums, internal governance overhead) is substantial but justified by the coordination requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_verifiability,
    'Can causation and control actually be verified and apportioned in practice, or do power asymmetries and information opacity collapse the shared-liability framework toward single-party liability in dispute outcomes?',
    'Systematic analysis of actual liability disputes (insurance claims, litigation discovery, regulatory findings) over a 10+ year period: measure the proportion of cases where liability is actually apportioned between developer and operator vs. concentrated on one party; measure whether apportionment correlates with technical causation or with bargaining power.',
    'If verification fails systematically (asymmetric power wins over causal analysis), the shared-liability reading collapses toward de-facto deployer-liability (operators have more resources and control over litigation narrative). If verification succeeds, the reading survives as doctrine though implementation costs remain high. Classification shifts from Tangled Rope (if coherent allocation occurs) to Snare (if allocation failure concentrates liability on weaker parties).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_verifiability, empirical, 'Whether causal attribution can be enforced against power asymmetries.').

omega_variable(
    information_disclosure_suppression,
    'Does the requirement to disclose causation-relevant information (developers must reveal code complexity and defect rates; operators must expose deployment context) suppress beneficial information-sharing practices, or does it drive beneficial transparency?',
    'Study of information-sharing patterns pre-shared-liability and post-adoption in comparable jurisdictions: measure whether developers'' willingness to share code defect data, threat models, and remediation timelines increases (transparency) or decreases (defensive suppression); measure whether operators'' disclosure of deployment configurations and access controls increases or decreases.',
    'If suppression dominates, the constraint''s extraction costs (coordinating around opacity) exceed its coordination benefits—Snare. If transparency dominates, the suppression metric should be revised downward and the Tangled Rope classification held (extraction is coordination cost, not pure extraction). The measurement captures the current state (suppression_requirement rises with maturation); this omega asks whether the current pattern is temporally stable or will reverse with norm adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_disclosure_suppression, empirical, 'Whether causation-disclosure requirements suppress or promote beneficial transparency.').

omega_variable(
    sibling_reading_logical_coherence,
    'Can the shared-liability reading coexist with deployer-liability and developer-liability readings within a single legal framework, or does accepting shared liability logically foreclose the single-party readings?',
    'Analysis of jurisdictions that have adopted mixed doctrines (some statutory sectors assign deployer liability, others shared liability, others developer liability for specific contexts like medical devices vs. consumer software). Document whether courts successfully apply context-specific allocation rules or whether they collapse toward a default reading that preempts others.',
    'If the readings are genuinely coexistent, this is correctly classified as coexists_with sibling relations. If courts systematically apply one reading across all contexts (making others effectively foreclosed), the relations should be revised to forecloses. If one reading is operationally impossible given the others'' legal precedents, the cs_structure must be corrected.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_logical_coherence, conceptual, 'Whether shared liability and single-party liability readings can coexist doctrinalally.').

omega_variable(
    developer_identity_lock_mechanism,
    'Is the measured identity_locked exit option for developers accurate, or do alternative professional identities (e.g., internal developer within large tech company, university researcher not liable for industrial deployment) provide escape routes that should be reflected as constrained rather than identity-locked?',
    'Career-trajectory analysis of software developers entering and exiting liability-exposed sectors: measure the proportion who transition to protected roles vs. who accept liability exposure as a cost of professional identity; measure whether awareness of liability exposure affects career choices at entry.',
    'If escape routes are available to many developers (constrained exit), the directionality for developers should be lowered slightly (~0.75 rather than ~0.85), which would dampen effective extraction slightly. If identity lock is genuine, the measurement stands. This affects both the per-developer directionality and the institutional dynamics of developer labor supply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developer_identity_lock_mechanism, empirical, 'Whether developer exit from liability exposure is identity-locked or merely constrained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__shared_liability, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(liab_tr_t5, observed).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(liab_tr_t10, observed).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__shared_liability, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(liab_tr_t15, observed).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(liab_tr_t20, observed).
narrative_ontology:measurement(liab_tr_t25, liability_attribution__shared_liability, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(liab_tr_t25, observed).
narrative_ontology:measurement(liab_tr_t30, liability_attribution__shared_liability, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(liab_tr_t30, observed).
narrative_ontology:measurement(liab_tr_t35, liability_attribution__shared_liability, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(liab_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t5, liability_attribution__shared_liability, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(liab_be_t5, observed).
narrative_ontology:measurement(liab_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(liab_be_t10, observed).
narrative_ontology:measurement(liab_be_t15, liability_attribution__shared_liability, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(liab_be_t15, observed).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(liab_be_t20, observed).
narrative_ontology:measurement(liab_be_t25, liability_attribution__shared_liability, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(liab_be_t25, observed).
narrative_ontology:measurement(liab_be_t30, liability_attribution__shared_liability, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(liab_be_t30, observed).
narrative_ontology:measurement(liab_be_t35, liability_attribution__shared_liability, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(liab_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t5, liability_attribution__shared_liability, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(liab_su_t5, observed).
narrative_ontology:measurement(liab_su_t10, liability_attribution__shared_liability, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(liab_su_t10, observed).
narrative_ontology:measurement(liab_su_t15, liability_attribution__shared_liability, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(liab_su_t15, observed).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(liab_su_t20, observed).
narrative_ontology:measurement(liab_su_t25, liability_attribution__shared_liability, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(liab_su_t25, observed).
narrative_ontology:measurement(liab_su_t30, liability_attribution__shared_liability, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(liab_su_t30, observed).
narrative_ontology:measurement(liab_su_t35, liability_attribution__shared_liability, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(liab_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__shared_liability, 0.18).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, software_liability_insurance_markets).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, contract_allocation_indemnification).

% DUAL FORMULATION NOTE:
% This constraint is part of the liability-attribution kernel family (3 readings: deployer_liability, developer_liability, shared_liability). The shared-liability reading distributes causation-based responsibility across developers and operators, creating distinct structural pressures from single-party liability readings. All three readings share the same referent (the question of who bears liability for deployed-software harms) but produce different victim/beneficiary structures and different coordination cost profiles. The shared-liability reading is distinguished by requiring both developers and operators to participate in causal analysis and contractual allocation, which creates new markets and enforcement burdens not present in single-party readings. Network linkage enables the corpus to measure how the three readings interact: does adoption of shared liability in one jurisdiction drive adoption in others (influences), or do they remain isolated (coexists_with)? Does evidence that shared liability fails empirically foreclose the deployer and developer readings, or simply demonstrate alternative efficiency?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__shared_liability, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
