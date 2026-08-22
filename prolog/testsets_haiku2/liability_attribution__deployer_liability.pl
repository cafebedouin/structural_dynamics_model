% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer Primary Liability Doctrine
 *   domain: legal/regulatory/technology_governance
 *
 * SUMMARY:
 *   The liability_attribution kernel is contested across three structurally
 *   distinct readings about where legal responsibility should attach when a
 *   deployed foundation model causes harm. This JSON instantiates the
 *   DEPLOYER-PRIMARY READING: the doctrine that deployers — the organizations
 *   operating foundation models in specific contexts — bear primary liability
 *   for downstream harms because they control deployment context, make the
 *   decision to deploy, and can monitor and modify use. Under this reading,
 *   foundation model providers are substantially shielded from liability;
 *   developers of applications integrating the models occupy an intermediate
 *   position; and the burden of due diligence, inspection, and legal defense
 *   falls on deployers. The constraint is CLAIMED as tangled_rope (real
 *   coordination problem solved: someone must be accountable) but the metrics
 *   describe substantial extraction (deployers externalize little risk;
 *   providers externalize substantial deployment costs) and high suppression
 *   (the doctrine is enforced through contractual disclaimers, liability
 *   waivers, and terms-of-service restrictions that deployers have limited
 *   ability to negotiate). This reading's ε is assessed from the standing
 *   arrangement's impact on deployers under this interpretation; ε does not
 *   shift if alternative readings are adopted — the referent is fixed
 *   (deployer-bearing-liability arrangement), the reading-indexed value
 *   (0.68) describes how extractive that arrangement is.
 *
 * KEY AGENTS:
 *   - deployers_of_foundation_models: Primary targets of liability; bear deployment context risk without full visibility into model behavior
 *   - foundation_model_providers: Primary beneficiaries; exert control over model release, disclaimers, and terms, while liability flows downstream
 *   - software_developers: Secondary beneficiaries; integrate models without bearing deployment liability
 *   - liability_insurers: Beneficiaries collecting premiums on deployer exposure; partly constrained by unpredictable loss profiles
 *   - end_users_harmed_by_deployment: Excluded; cannot sue providers directly; must pursue deployers
 *   - regulators_and_courts: Agenda-setters enforcing the liability allocation; must distinguish deployer negligence from model defect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.71).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer Primary Liability Doctrine").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "legal/regulatory/technology_governance").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, 'b542bf39-5cf6-4b6b-b42f-dd9a6de713b4').
narrative_ontology:cs_kernel_codification('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', formalized).
narrative_ontology:cs_authority_grounding('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', extraction).
narrative_ontology:cs_interpretation_layer_present('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4').
narrative_ontology:cs_reading_relation('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', foundational, deployment_context_is_locus_of_control).
narrative_ontology:cs_axiom_status(deployment_context_is_locus_of_control, holdable).
narrative_ontology:cs_axiom_grounding('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', deployment_context_is_locus_of_control, instrumental).
narrative_ontology:cs_axiom('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', secondary, deployer_due_diligence_is_feasible).
narrative_ontology:cs_axiom_status(deployer_due_diligence_is_feasible, holdable).
narrative_ontology:cs_axiom_grounding('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', deployer_due_diligence_is_feasible, empirically_contingent).
narrative_ontology:cs_reference_frame('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', deployer_responsibility_for_operational_decisions).
narrative_ontology:cs_drift_state('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', contemporary_ai_governance_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b542bf39-5cf6-4b6b-b42f-dd9a6de713b4', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, software_developers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, deployers_of_foundation_models).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, liability_insurers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, software_developers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, liability_insurers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations that integrate foundation models into production systems bear primary liability for downstream harms under this reading. They must conduct due diligence on models they cannot fully inspect, maintain monitoring infrastructure, purchase liability insurance, and defend against claims even when harm arises from undetectable model behaviors. Their exit is identity-locked: they depend on foundation model capabilities for their business model and cannot credibly pivot away without massive capability loss. They are locked into the liability regime because exiting AI deployment means abandoning competitive advantage and core capability.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, deployers_of_foundation_models, payer,
    organized, biographical, identity_locked, global).

% Organizations that develop and release foundation models. They benefit by externalizing deployment risk through contractual liability waivers and terms-of-service disclaimers. They retain control over what models are released, what disclaimers attach, and what terms are imposed on deployers, while deployers bear the legal and financial burden of deployment. They have arbitrage options: they can license to multiple jurisdictions with different liability regimes, release new model versions (abandoning sunk costs of prior deployment liability), or move capital to new markets. They collect licensing revenue while deployers absorb liability costs.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Application developers who integrate foundation models into software products. They benefit by not bearing deployment liability: liability flows through them to deployers, not backward to them. They pay indirectly by being contractually bound by provider terms and disclaimers, and they bear some responsibility for model selection and configuration, but the primary liability burden is downstream. They have moderate mobility: they can switch model providers, change integration patterns, or pivot to model-free services more easily than deployers.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, software_developers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, software_developers, payer).

% Individuals and groups who suffer harm from deployed models. Under deployer-primary reading, they are excluded from the liability allocation: they cannot sue model providers directly (protected by deployer-primary doctrine and provider disclaimers) and must pursue deployers. They have no seat in the framework; their interests are represented only insofar as deployers face liability exposure that creates incentive to prevent harm. They are trapped: they use the deployed system and bear the harm risk without recourse to providers.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, end_users_harmed_by_deployment, excluded,
    powerless, immediate, trapped, global).

% Legal authorities and regulatory bodies that enforce and interpret liability doctrine. They set the agenda for how deployer-primary liability is operationalized, what counts as adequate due diligence, and how courts adjudicate disputes between deployers and providers or between deployers and harmed parties. They must distinguish deployer negligence from model defect, determine what liability waivers are enforceable, and develop precedent that stabilizes the doctrine. They administer the distinction between deployer choices and provider capability.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Insurance providers offering AI liability coverage to deployers. They benefit because deployers become the insurable party for foundation model deployment risk, creating a demand for liability coverage. They collect premiums and manage deployer risk pools. They pay indirectly by bearing claim costs and adapting coverage to unpredictable loss profiles; they are constrained by the volatility of foundation model harms (emergent capabilities, undetectable behaviors) that make pricing difficult and exposure uncertain.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, liability_insurers, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, liability_insurers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, enforceable locus of liability that avoids infinite regress (end user → developer → provider → foundation model team) by assigning primary responsibility to the party with deployment context control. This solves the allocation problem: someone must be accountable for how the system is actually used.
% TRANSFER_FUNCTION: Transfers deployment risk from foundation model providers and intermediate developers to deployers. Deployers absorb legal exposure, compliance burden, due diligence costs, and liability insurance costs. Foundation model providers and developers externalize these costs by contractual disclaimers and liability waivers, shifting them downstream to the deployment operator.
% ABSENT_VOICES: End users and downstream harm victims are excluded: they cannot sue the model provider directly and must pursue deployers. Regulatory bodies in jurisdictions skeptical of this allocation (EU AI Act model, for instance) are not seated in this reading's framework — they would argue for shared or developer-primary liability but are kept outside by choice of jurisdictional reading. Small deployers with limited resources and limited model-inspection capacity would object if heard; they are typically unrepresented in policy forums.
% DISAPPEARANCE_RATIONALE: If deployer-primary liability vanished (replaced by developer-primary or shared liability), the incentive structures reverse: foundation model providers would face direct liability for downstream harms and would implement stricter release controls, deploy monitoring infrastructure post-release, and demand deployer compliance audits. Deployers would shift toward liability disclaimers and could pass legal risk upstream. The distribution of inspection burden, insurance costs, and risk capital would reorganize entirely.
% FOUNDING_PROBLEM: Foundation models are opaque, powerful, and widely deployed. Liability must attach somewhere. Early deployments created ambiguity: when a model harmed someone, was the harm the provider's responsibility (they created the capability), the developer's responsibility (they chose which model to integrate), or the deployer's responsibility (they decided to deploy it)? Deployer-primary reading asserts that deployment context — the specific use case, integration point, and operational environment — is where liability should attach, because that is where the decision to deploy with that context was made.
% FOUNDING_PROBLEM_CORROBORATION: Foundation model providers and liability insurers attest deployer-primary is the only workable allocation (direct provider liability would freeze model releases). Regulatory bodies (EU, parts of US states) attest the founding problem is still live but deployer-primary mislabels who has control: providers control the model; deployers control how it is used, but not what it can do. Academic literature and deployment-context harm case law support contested status: some harms are clearly deployer-caused (bad prompt engineering, no monitoring); others are clearly model-caused (undetectable emergent capabilities, training-data artifacts). No consensus external corroboration exists; disagreement is structural to the kernel contest.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness climbs from 0.52 to 0.68 over the interval as deployer liability doctrine expands in scope and case law hardens around deployer-primary responsibility. Early in the interval (t=0), the doctrine was nascent and contested; by t=25, deployer liability has become standard in commercial contracts, liability insurance requirements, and regulator guidance, reducing deployer exit options and increasing the extraction they bear. Theater ratio starts lower (0.28) because the coordination function is genuine (someone must bear liability to avoid diffusion), but rises to 0.42 as the burden becomes routinized and defensive compliance (audits, monitoring, documentation) substitutes for actual risk reduction — the performance of due diligence becomes decoupled from the actual reduction of foundation model deployment risk. Suppression rises from 0.58 to 0.71 as contractual liability waivers become standardized and deployers' ability to negotiate alternative terms declines — the arrangement is actively enforced through legal and contractual mechanisms that deployers cannot evade without exiting the foundation model deployment market entirely.
 *
 * PERSPECTIVAL GAP:
 *   Deployers and foundation model providers experience radically different structural positions within this constraint, and should compute different per-seat classifications. From the deployer seat: the constraint is extractive (liability without commensurate control), suppressive (waivers leave no real exit), and increasingly theatrical (defensive documentation consuming resources without reducing actual risk). From the provider seat: the constraint solves a critical coordination problem (clear responsibility allocation) and enables their business model (liability shielding through contractual terms). From the insurance seat: the constraint creates a profitable, if volatile, risk pool (deployers demand coverage; losses are real but somewhat predictable by sector). The engine computes these seat divergences from the power + time_horizon + exit_options atoms and the declared beneficiary/victim structure; the perspectival gap is structural, not a measurement error. Absent deployer exit options (they cannot escape foundation models without major capability loss), their directionality is pushed toward full-target end; providers have arbitrage options (they can license, disclaim, or distribute widely across jurisdictions with different liability regimes), pushing their directionality toward beneficiary end.
 *
 * DIRECTIONALITY LOGIC:
 *   Deployers are victims: they bear liability, conduct due diligence, purchase insurance, and defend against claims, while having constrained exit options (identity_locked to foundation model deployment via capability dependencies; constrained by regulatory and contractual waivers). Their directionality is high (d near 1.0, full target). Foundation model providers are beneficiaries: they exert control (deciding what to release, what to disclaim, what terms to impose), collect indirect value (licensing fees, commercial advantage from deployer liability burden shifting costs upstream to deployers), and have arbitrage options (they can release under different terms in different jurisdictions, license to different types of users, or pivot to new models while prior deployments absorb sunk costs). Their directionality is low (d near 0.0, full beneficiary). Software developers occupy middle ground: they benefit from liability flow passing through them to deployers (they avoid direct deployment liability), but they are moderately constrained (they depend on model availability and provider terms). Liability insurers benefit structurally (deployer liability creates insurable demand) but are partly constrained (loss volatility and deployer risk profiles limit their margins). End users are excluded and powerless (trapped, immediate horizon); regulators are analytical observers (their directionality is administrative, not extractive).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (liability must attach somewhere; foundation models created ambiguity) remains live and contested, but the doctrine that solves it — deployer-primary liability — is increasingly divorced from whether it actually achieves the goal of accountability. Case law and industry practice reveal that deployer liability often fails to create ex-ante accountability: deployers cannot fully inspect foundation models, cannot predict emergent behaviors, and often cannot remediate harms without provider cooperation. The constraint persists not because it solves the founding problem effectively, but because it serves the institutional interests of providers and insurers. Theater rises as deployers go through compliance motions (documenting due diligence, implementing monitoring) that simulate accountability without materializing it. The mandate — clear liability allocation — has outlived its fitness to the founding problem (accountability), making this a candidate for mandatrophy reclassification. The theater_ratio trajectory (rising from 0.28 to 0.42) and the suppression trajectory (0.58 to 0.71) indicate a constraint becoming more theatrical and more enforced precisely as it drifts from solving its founding problem. The constraint persists due to institutional inertia: providers benefit from liability shielding, deployers have no better alternative (switching doctrines requires regulatory change), and insurers profit from the arrangement. Mandatrophy is not yet resolved (the founding problem is not wholly dead, and the doctrine still structures liability allocation), but the trajectory points toward future mandatrophy diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deployer_inspection_feasibility,
    'Can deployers actually conduct meaningful due diligence on foundation models given their opacity, scale, and emergent capabilities?',
    'Empirical study of deployer inspection practices: what fraction of deployed harms are detectable by deployer due diligence versus undetectable in testing? Post-deployment monitoring data showing what harms actually surface versus what was anticipated.',
    'If deployers cannot feasibly detect most harms, the liability doctrine mislabels control: deployers bear risk for outcomes they cannot foresee, making the doctrine unfairly extractive and potentially unjust. If deployers can detect most harms through practice, the doctrine is fitness-appropriate. Resolution directly affects whether this constraint should be reclassified from tangled_rope (mixed coordination/extraction) toward pure snare (extraction masquerading as accountability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deployer_inspection_feasibility, empirical, 'The feasibility of deployer due diligence on opaque models').

omega_variable(
    foundational_reading_premise,
    'Does deployment context ACTUALLY represent the locus of maximum control and responsibility, or is model design (provider) and integration choice (developer) the real locus?',
    'Case law divergence: do courts consistently hold deployers responsible even when model failure is undetectable in deployment context? Do courts ever pierce through to developers or providers when deployer negligence is absent but model behavior is demonstrably harmful? Comparative regulatory analysis: do alternative readings (developer-primary in some jurisdictions, shared-liability in others) produce better accountability outcomes by the same metrics?',
    'If case law consistently pierces deployer liability to reach providers/developers when deployer due diligence was genuine, the foundational axiom of deployer-primary reading is falsified and the reading forecloses to shared-liability or developer-primary. If courts consistently stop at deployers regardless, the axiom holds. This is a core kernel dispute — resolution determines whether this reading remains holdable or becomes overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_reading_premise, conceptual, 'Whether deployment context is the true locus of control and responsibility').

omega_variable(
    liability_doctrine_sibling_coexistence,
    'Can deployer-primary and developer-primary liability doctrines coexist in the same legal framework, or does holding both simultaneously create internal contradiction?',
    'Jurisdictional analysis: do any legal systems hold both deployers and developers as primary liable parties for the same harm class? If so, does doctrine provide clear rules for priority/contribution, or does ambiguity persist? If not, which jurisdictions chose which reading and why?',
    'If the readings coexist without contradiction in some jurisdictions (e.g., via different liability standards: deployers bear strict liability for deployment harms; developers bear negligence liability for integration choices), then the relation to siblings is coexists_with (different legal seats, different standards, both live). If they logically foreclose each other (both claiming to be the sole primary liable party), the relation is forecloses. This directly affects the cs_structure.reading_relations array.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_doctrine_sibling_coexistence, conceptual, 'Whether deployer-primary and developer-primary liability doctrines logically foreclose each other').

omega_variable(
    suppression_mechanism_structure_vs_internalization,
    'Is the measured suppression (0.71) primarily structural (contractual waivers, legal barriers, market exit costs) or internalized (deployers have accepted the allocation as legitimate and self-police even without external enforcement)?',
    'Post-waiver-removal scenario analysis: if deployers were told waivers would be void and providers would be jointly liable, would deployer behavior and risk-taking change? Survey data on deployer acceptance of the doctrine versus resignation to it. Market dynamics: do deployers actively seek alternative liability regimes, or do they accommodate the current one as settled?',
    'If suppression is mostly structural (waivers enforced, exit is costly), removing the constraint would require legal intervention. If mostly internalized (deployers believe deployer-primary is the right allocation), the constraint persists by cultural acceptance and the extraction is less visible. If both, the effective suppression is higher than the structural measure alone suggests because even waiver removal would not free deployers — they carry the internalized norm with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structure_vs_internalization, empirical, 'Whether suppression of deployer liability resistance is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liability_deployer_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.28).
narrative_ontology:measurement(liability_deployer_tr_t5, liability_attribution__deployer_liability, theater_ratio, 5, 0.32).
narrative_ontology:measurement(liability_deployer_tr_t10, liability_attribution__deployer_liability, theater_ratio, 10, 0.36).
narrative_ontology:measurement(liability_deployer_tr_t15, liability_attribution__deployer_liability, theater_ratio, 15, 0.39).
narrative_ontology:measurement(liability_deployer_tr_t20, liability_attribution__deployer_liability, theater_ratio, 20, 0.41).
narrative_ontology:measurement(liability_deployer_tr_t25, liability_attribution__deployer_liability, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(liability_deployer_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(liability_deployer_be_t5, liability_attribution__deployer_liability, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(liability_deployer_be_t10, liability_attribution__deployer_liability, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(liability_deployer_be_t15, liability_attribution__deployer_liability, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(liability_deployer_be_t20, liability_attribution__deployer_liability, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(liability_deployer_be_t25, liability_attribution__deployer_liability, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(liability_deployer_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(liability_deployer_su_t5, liability_attribution__deployer_liability, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(liability_deployer_su_t10, liability_attribution__deployer_liability, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(liability_deployer_su_t15, liability_attribution__deployer_liability, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(liability_deployer_su_t20, liability_attribution__deployer_liability, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(liability_deployer_su_t25, liability_attribution__deployer_liability, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__deployer_liability, 0.12).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the liability_attribution kernel. It models the allocation of legal responsibility for foundation model harms when deployed in production systems, specifically the deployer-primary reading. Sibling readings (developer_liability, shared_liability) model alternative allocations that would produce different ε values and different victim/beneficiary sets from the same deployed-system scenario. The three stories are linked via network.affects_constraints to model the kernel contest. Each reading independently instantiates its own constraint structure, beneficiary/victim declarations, and ε value, following ε-invariance: different readings of the same kernel produce different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__deployer_liability, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
