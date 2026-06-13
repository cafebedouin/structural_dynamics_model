% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Developer Primary Liability Attribution for Deployed Capability Harms
 *   domain: legal/technological/regulatory
 *
 * SUMMARY:
 *   A contested liability allocation regime in which developers are assigned
 *   primary legal responsibility for harms caused by deployed software they
 *   created, regardless of deployment context, integration choices, or
 *   operator control over configuration and use. This reading of the
 *   liability_attribution kernel asserts that creation—producing the
 *   underlying technical capability—is the grounding for responsibility.
 *   Rival readings (deployer_liability, shared_liability) ground
 *   responsibility in deployment control or causal contribution across the
 *   value chain. The developer-liability reading has become the default in
 *   many jurisdictions via tort doctrine and product liability law, but faces
 *   increasing contestation from open-source governance communities, security
 *   researchers, and institutional developers who argue deployers'
 *   context-specific choices are the primary causal factors.
 *
 * KEY AGENTS:
 *   - software_developers: moderate power, identity-locked exit, generational time horizon — bear the legal liability despite limited deployment control
 *   - deployers_and_operators: institutional power, arbitrage exit, generational time horizon — set deployment policy, control system architecture, collect operational benefits
 *   - open_source_contributors: powerless, identity-locked exit, biographical time horizon — face personal liability for volunteer work with global deployment they cannot monitor
 *   - institutional_users: powerful, arbitrage exit, generational time horizon — deploy capabilities in critical systems while externalizing risk to developers
 *   - regulatory_bodies: institutional power, analytical exit — observe and can reshape the constraint through regulatory interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.72).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Primary Liability Attribution for Deployed Capability Harms").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "legal/technological/regulatory").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, 'ba715415-329c-4286-beac-13a799253b82').
narrative_ontology:cs_kernel_codification('ba715415-329c-4286-beac-13a799253b82', distributed).
narrative_ontology:cs_authority_grounding('ba715415-329c-4286-beac-13a799253b82', extraction).
narrative_ontology:cs_reading_relation('ba715415-329c-4286-beac-13a799253b82', liability_attribution__liability_attribution_deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('ba715415-329c-4286-beac-13a799253b82', liability_attribution__liability_attribution_shared_liability, influences).
narrative_ontology:cs_axiom('ba715415-329c-4286-beac-13a799253b82', foundational, creation_grounds_responsibility).
narrative_ontology:cs_axiom_status(creation_grounds_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('ba715415-329c-4286-beac-13a799253b82', creation_grounds_responsibility, deontological).
narrative_ontology:cs_axiom('ba715415-329c-4286-beac-13a799253b82', secondary, developer_perpetual_liability).
narrative_ontology:cs_axiom_status(developer_perpetual_liability, overridden).
narrative_ontology:cs_axiom_grounding('ba715415-329c-4286-beac-13a799253b82', developer_perpetual_liability, empirically_contingent).
narrative_ontology:cs_reference_frame('ba715415-329c-4286-beac-13a799253b82', creator_bears_primary_risk).
narrative_ontology:cs_drift_state('ba715415-329c-4286-beac-13a799253b82', contemporary_open_source_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ba715415-329c-4286-beac-13a799253b82', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deployers_and_operators).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, liability_insurance_industries).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, institutional_users).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, software_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_contributors).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, small_tool_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, plaintiffs_counsel).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, creator_bears_perpetual_responsibility).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, capability_ownership_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create the underlying technical capability—a library, framework, or tool. Under developer-primary liability, they bear legal and financial responsibility when the capability is deployed in contexts that cause harm, even contexts they did not design for, did not anticipate, and cannot practically monitor. A developer of a cryptographic library may be sued when a medical device using it fails; a web framework creator faces liability when deployed in systems where security misconfigurations cause data breaches. Their exit options include stopping work, disclaiming liability via open-source licenses (which courts increasingly refuse to honor), or paying for comprehensive liability insurance. Professional identity is deeply fused with the work; many developers have built careers around specific tools.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, software_developers, payer,
    moderate, generational, identity_locked, global).

% Deploy the developer's capability into their own systems, choosing use cases, integrating with other components, and controlling deployment security and configuration. Under developer-primary liability, they externalize risk—they make deployment decisions and reap operational benefits while developers bear financial and reputational consequences. They set liability policy through tort litigation, regulatory interpretation, and legislative lobbying. They have deep resources and can hire legal counsel to navigate liability landscapes; they can also switch developers and shift liability, creating arbitrage.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deployers_and_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, deployers_and_operators, agenda_setter).

% Volunteer work on public tools with no organizational backing. Under developer-primary liability, they face personal liability for harms in deployed contexts globally, despite zero compensation and no control over deployment. Many are geographically distributed across jurisdictions with different liability standards, making centralized defense impossible. They often cannot afford insurance. Their identity is fused with their open-source work; exiting means abandoning years of invested reputation and community standing.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_contributors, payer,
    powerless, biographical, identity_locked, global).

% Use developer-created capabilities in critical systems—hospitals use medical device software built on open libraries, financial institutions use cryptographic and data-processing frameworks, governments use frameworks for regulatory systems. Developer-primary liability allows them to deploy with minimal review of the developer's practices (knowing liability flows to the developer) and to shift deployment risk backward to the creator. They benefit from the capability without bearing proportional risk for their integration choices.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, institutional_users, beneficiary,
    powerful, generational, arbitrage, global).

% Sell professional liability and errors-and-omissions insurance to developers and development firms. Developer-primary liability inflates the perceived risk developers must insure against, allowing insurers to price higher premiums and expand coverage scope. Insurers profit from the constraint's operation regardless of whether claims materialize.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, liability_insurance_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Represent parties harmed by deployed software. Developer-primary liability provides a clear defendant with identifiable liability, making cases easier to construct and potentially more lucrative (developers and their insurers settle rather than litigate complex causation). Counsel benefits from the simplicity of targeting the creator.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, plaintiffs_counsel, beneficiary,
    organized, biographical, arbitrage, national).

% Alternative developers and platforms whose tools could be deployed in place of the dominant ones. Under developer-primary liability, they are excluded from the conversation about appropriate liability allocation because liability doctrine is set by litigation and legislative interpretation, not by market competition. A rival developer could argue for deployer liability but has no seat at the table where liability standards are adjudicated.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deployers_excluded_rivals, excluded,
    powerful, generational, trapped, global).

% Government agencies that set product liability standards, interpret consumer protection law, and investigate incidents. They observe the constraint and its effects, and can reshape it through regulatory interpretation (creating safe-harbor defenses for developers, mandating deployer conduct standards, etc.). They have the formal authority to rebalance but face lobbying pressure from beneficiary constituencies.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, deployers_and_operators).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear chain of responsibility for software failures: identifying who is responsible for harms and ensuring some party bears liability. This coordinates the risk-attribution problem—someone must be answerable for failures, and the developer-primary reading names the developer as that answerable party. This solves a genuine coordination problem: without clear attribution, injured parties have no defendant and developers have no incentive structure.
% TRANSFER_FUNCTION: Transfers financial and legal liability from deployers and operators to developers. When deployed software causes harm, the developer bears the legal obligation to defend against suit, to pay damages (or their insurer does), and to bear reputational loss. Deployers retain control over deployment decisions, system architecture, and security practices while developers bear the consequence of those choices.
% ABSENT_VOICES: Developers themselves are largely absent from the process that allocates liability to them. Liability allocation is set through litigation (where developers are defendants, not co-authors of the rule), regulatory interpretation (where developers have weak lobbying power compared to institutional deployers and insurers), and tort doctrine developed by judges and legal scholars, not by developer communities or open-source governance structures. Developers speak only reactively, defending themselves in court.
% DISAPPEARANCE_RATIONALE: If developer-primary liability disappeared overnight, deployers would face liability for their own deployment decisions and system configuration. They would need to conduct deeper security reviews before deploying third-party code, implement more robust testing, and maintain liability insurance for their own operational choices. The entire risk-allocation landscape of software deployment would reorganize—developers would face weaker incentives to warn of limitations, but deployers would face stronger incentives to validate and control. The cost of software deployment would rise for users and institutions, while development friction would decrease for creators.
% FOUNDING_PROBLEM: Early software markets lacked clear responsibility attribution for deployed harms. When software caused injury or loss, injured parties could not identify who was legally responsible, and developers had no standardized way to establish the boundaries of their responsibility. Courts needed a clear assignment of liability to adjudicate cases; the developer-primary reading emerged from early product liability doctrine that treated the creator as the responsible party.
% FOUNDING_PROBLEM_CORROBORATION: Courts adjudicating software liability cases attest that clear attribution is necessary and cite developer creation as the grounding for responsibility (foundational principle). Open-source governance communities, developer advocacy organizations (Software Freedom Conservancy, Apache Foundation), and independent security researchers attest that deployers' integration choices and configuration errors are the primary causal factors in most harms, and that imposing responsibility on developers who cannot control deployment context is dysfunctional. Institutional deployers and insurers attest that developer liability provides predictable risk allocation and incentivizes developer caution, but do not dispute that responsibility could be allocated differently.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
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
 *   Extractiveness is high (0.68 at interval end, rising from 0.51 at t0) because the constraint transfers financial and legal liability from deployers (who control deployment decisions and reap benefits) to developers (who lack deployment context and control). The measurement series shows steady rise from t0–t30, plateauing near the end, indicating the constraint is accumulating extractive force as liability jurisprudence consolidates (more case law, higher settlement costs, expanded insurance requirements) but then stabilizing as developer communities adapt (better licensing practices, liability caps in open-source governance, institutional backing for major projects). Suppression is also high (0.72 at interval end, rising from 0.48 at t0) because developers' ability to exit the constraint is severely limited: professional identity is fused with their creation (identity_locked exit), legal doctrine actively enforces the liability assignment, and institutional pressure (insurers, deployers, regulators) actively suppresses alternative liability framings. Theater ratio is moderate (0.42 at interval end) because the constraint operates with both genuine coordination function (establishing clear responsibility for attribution) and extractive overhead (shifting responsibility backward to parties with less control). As the interval progresses, theater rises as deployers increasingly justify developer liability on coordination grounds while suppressing deployer-control based alternatives (theater_ratio t0→t30), but plateaus as the constraint stabilizes and open-source communities develop workarounds (capping liability via community governance, licensing disclaimers, institutional liability insurance).
 *
 * PERSPECTIVAL GAP:
 *   From the deployer/institutional seat, the constraint is a rational allocation of responsibility to the party who created the capability—clear incentive alignment, predictable risk, efficient insurance markets. From the developer seat (especially powerless and open-source contributors), the constraint is arbitrary—responsibility flows to the person with least control over deployment context, security configuration, and operational use. A developer of a cryptographic library cannot audit every medical device or financial system that uses it; the deployer (hospital, bank) controls integration, configuration, and security practices but bears no liability. The engine should compute dramatically different types for these seats: deployers and institutional users perceive coordination with moderate extraction (rope or tangled_rope from the beneficiary view); developers perceive pure extraction with negligible coordination (snare or pure extraction from the target view). This divergence is the measurement the framework exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers are full targets (d near 1.0): they bear the liability transfer, lack exit options (identity_locked), and have no control over deployment context. Deployers are full beneficiaries (d near 0.0): they externalize risk, control deployment, and collect operational benefits. Open-source contributors are even more extreme targets (d = 1.0): powerless, identity-locked, biographical time horizon, zero compensation. Institutional users sit near beneficiary (d low): they deploy with minimal review burden, knowing liability flows to developers, and can switch developers if liability becomes onerous. No directionality override is needed; the derivation chain (beneficiary/victim + exit → d) produces the correct values structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (responsibility attribution for software harms) is CONTESTED in status. Deployers and courts attest it remains live—they cite ongoing litigation and the need for clear responsibility. Developers and security researchers attest it is functionally dead for allocation purposes—the constraint persists not because attribution is unclear, but because deployers have captured liability policy and are using it to externalize risk. The disappearance verdict is world_rearranges: if developer-primary liability disappeared, deployers would face liability for their deployment choices and would dramatically increase their vetting and control practices. This is mandatrophy: the original problem (unclear attribution) was solved years ago by established doctrine, but the constraint persists because beneficiaries (deployers, insurers, institutional users) benefit from continued extraction, not from solving the founding problem. The constraint has evolved from a coordination mechanism (solving attribution ambiguity) into an extraction mechanism (shifting deployer risk to developers). The theater_ratio rise (t0→t30) captures this decay: as doctrine crystallizes, the constraint requires less functional coordination work and more active enforcement to suppress alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_reading_contestation,
    'Is the liability_attribution kernel a genuine shared commitment all parties hold (with differing readings), or do the reading factions contest the kernel itself—i.e., do developers and deployer-liability advocates reject the premise that a SINGLE party should bear primary responsibility?',
    'Examine legislative proposals, developer advocacy documents, and court briefs: do advocates for alternative readings propose joint liability, distributed liability, or responsibility frameworks different from ''single primary bearer''? If they propose alternatives to the primary-bearer framing itself, the kernel is contested, not just the readings.',
    'If the kernel is contested, this constraint and its siblings are not all readings of the same commitment—they are readings of alternative foundational premises. The network relationships would change (siblings do not coexist, they foreclose). If the kernel is shared and only readings differ, the sibling relationships hold as authored (coexists_with and influences relations are correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_contestation, conceptual, 'Whether the kernel (single primary bearer) is a shared commitment or contested premise.').

omega_variable(
    control_vs_creation_causation,
    'When deployed software causes harm, is the primary causal factor the developer''s creation (design decisions, implementation choices, inherent capability) or the deployer''s control (integration choices, configuration, operational context)? Which is MORE causal?',
    'Empirical analysis of actual harm incidents: reverse-engineer each incident to identify which party''s choices (developer or deployer) would have prevented the harm if made differently. Aggregate across a representative incident corpus.',
    'If deployer control is more causal in most incidents, developer-primary liability is misdirected—it holds the less-causal party responsible. The constraint would reclassify toward snare (pure extraction) from its current tangled_rope classification. If creation is more causal, the current allocation is justified; if mixed, shared_liability reading becomes more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(control_vs_creation_causation, empirical, 'Which party''s choices are the primary causal factor in deployed-software harms.').

omega_variable(
    identity_lock_mechanism,
    'For developers, is the identity-lock mechanism professional identity (career path dependence), relational identity (self-concept constituted through the tool''s community), ideological identity (worldview committed to open contribution), or institutional identity (the developer''s organization has become the tool)?',
    'Post-exit trajectory: developers who exit (abandon a project or stop publishing) and how their self-concept, career, and community standing shift. Do developers who exit report liberated identity (institutional identity only) or persistent fusion (professional/relational/ideological identity remains)?',
    'If identity-lock is primarily institutional (organizational), some developers could exit by shifting organizations. If primarily professional, relational, or ideological, developers carry the lock regardless of organization. Strong identity-lock means d stays near 1.0 even if explicit liability exits (licensing disclaimers); weak institutional-only lock means d drops if the organization changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'The mechanism of identity-fusion binding developers to the constraint.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.72) structural (deployers actively lobby for developer liability, regulators enforce it, legal systems punish alternatives) or internalized (developers believe they SHOULD be responsible, have internalized the responsibility doctrine, accept it as legitimate)?',
    'Post-exit suppression trajectory: after a developer exits and is released from legal liability (via licensing disclaimer, statute of repose, organizational wind-down), does the developer''s sense of responsibility and guilt persist? Do they still feel accountable for harms caused by their old code?',
    'If suppression is structural (external enforcement), the constraint weakens if enforcement relaxes. If suppression is internalized (developers have adopted the responsibility doctrine as self-concept), the constraint persists even if external enforcement disappears. Internalized suppression means effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is structural (enforced) or internalized (believed).').

omega_variable(
    open_source_vs_commercial_divergence,
    'Should open-source and commercial developers be under the same liability regime, or do their structural differences (compensation, organizational backing, exit options) justify separate rules?',
    'Comparative regulatory analysis: jurisdictions that have created separate liability regimes for open-source (safe harbors, liability caps, community governance). Do those regimes produce better incentive alignment than uniform developer-primary liability?',
    'If open-source and commercial developers have substantially different exit options and control over deployment, they might warrant different liability allocations within the developer-primary reading. The ε-value might be lower (more coordinated) for commercial developers (with organizations and insurance) and higher (more extractive) for open-source contributors (powerless, uncompensated). This could split the developer-liability constraint into two stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_source_vs_commercial_divergence, empirical, 'Whether uniform developer-primary liability is appropriate for heterogeneous developer circumstances.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__developer_liability, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(liab_tr_t5, observed).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__developer_liability, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(liab_tr_t10, observed).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__developer_liability, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(liab_tr_t15, observed).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__developer_liability, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(liab_tr_t20, observed).
narrative_ontology:measurement(liab_tr_t25, liability_attribution__developer_liability, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(liab_tr_t25, observed).
narrative_ontology:measurement(liab_tr_t30, liability_attribution__developer_liability, theater_ratio, 30, 0.43).
narrative_ontology:measurement_basis(liab_tr_t30, observed).
narrative_ontology:measurement(liab_tr_t40, liability_attribution__developer_liability, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(liab_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t5, liability_attribution__developer_liability, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(liab_be_t5, observed).
narrative_ontology:measurement(liab_be_t10, liability_attribution__developer_liability, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(liab_be_t10, observed).
narrative_ontology:measurement(liab_be_t15, liability_attribution__developer_liability, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(liab_be_t15, observed).
narrative_ontology:measurement(liab_be_t20, liability_attribution__developer_liability, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(liab_be_t20, observed).
narrative_ontology:measurement(liab_be_t25, liability_attribution__developer_liability, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(liab_be_t25, observed).
narrative_ontology:measurement(liab_be_t30, liability_attribution__developer_liability, base_extractiveness, 30, 0.69).
narrative_ontology:measurement_basis(liab_be_t30, observed).
narrative_ontology:measurement(liab_be_t40, liability_attribution__developer_liability, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(liab_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t5, liability_attribution__developer_liability, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(liab_su_t5, observed).
narrative_ontology:measurement(liab_su_t10, liability_attribution__developer_liability, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(liab_su_t10, observed).
narrative_ontology:measurement(liab_su_t15, liability_attribution__developer_liability, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(liab_su_t15, observed).
narrative_ontology:measurement(liab_su_t20, liability_attribution__developer_liability, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(liab_su_t20, observed).
narrative_ontology:measurement(liab_su_t25, liability_attribution__developer_liability, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(liab_su_t25, observed).
narrative_ontology:measurement(liab_su_t30, liability_attribution__developer_liability, suppression_requirement, 30, 0.74).
narrative_ontology:measurement_basis(liab_su_t30, observed).
narrative_ontology:measurement(liab_su_t40, liability_attribution__developer_liability, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(liab_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__developer_liability, 0.14).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution_deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution_shared_liability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested liability_attribution kernel. The developer-primary reading (this file) asserts creation is the grounding for responsibility. The deployer-primary reading asserts deployment control is the grounding. The shared-liability reading distributes responsibility along causal contribution and control. Each reading instantiates a different ε, beneficiary/victim structure, and suppression mechanism. All three are linked via network.affects_constraints because they are alternative interpretations of the same foundational legal commitment. The developer-liability reading produces higher extractiveness and suppression than shared-liability (which distributes risk more broadly) and may foreclose shared-liability within a single jurisdiction, but coexists with deployer-liability as rival readings held by different factions (developers vs. deployers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
