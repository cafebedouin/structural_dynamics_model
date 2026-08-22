% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   domain: technology governance/legal theory/regulatory design
 *
 * SUMMARY:
 *   The deployer-primary liability allocation assigns primary legal
 *   responsibility for AI-system harm to the party that controls the
 *   deployment context and makes the deployment decision, shielding
 *   capability creators from downstream claims. The rule has a genuine
 *   coordination function: a default defendant makes AI exposure insurable,
 *   gives victims an answerable counterparty, and concentrates prevention
 *   incentives where deployment choices are made. It also carries a
 *   substantial asymmetric transfer: foundation model providers externalize
 *   deployment risk through warranty disclaimers and capped exposure, while
 *   deployers, regressively by size, bear exposure they cannot fully govern
 *   because model opacity converts the provider's information advantage into
 *   the deployer's diligence burden. The colloquial question 'who is liable
 *   for AI harm' is a kernel, not a constraint: it decomposes into three
 *   structurally distinct allocations with different epsilon values,
 *   beneficiary/victim structures, and failure modes. This file instantiates
 *   only the deployer_liability reading, with epsilon 0.58 describing the
 *   deployer-primary arrangement as this reading assesses it. Under the
 *   developer_liability sibling the victim and beneficiary sets invert; under
 *   shared_liability the sharp deployer-provider asymmetry diffuses along the
 *   value chain. The three files link via network.affects_constraints. The
 *   expected structural delta is realized in the authored data: deployers sit
 *   in the victim set, providers in the beneficiary set, and opacity appears
 *   as the diligence burden that generates the auditor stakeholder.
 *
 * KEY AGENTS:
 *   - foundation_model_providers: Primary beneficiary (institutional/arbitrage) — shielded from downstream claims; deployment risk sits with customers
 *   - enterprise_deployers: Payer seat with partial incumbent benefit (powerful/constrained) — bears primary exposure; the compliance burden also raises smaller rivals' costs
 *   - small_deployers: Primary payer (moderate/trapped) — same exposure as large deployers with none of the capacity
 *   - ai_decision_subjects: Excluded seat (powerless/trapped) — first incidence of deployment failures, no seat in the design
 *   - harmed_third_parties: Dual beneficiary/payer seat (powerless/constrained) — identifiable defendant, solvency-bounded recovery
 *   - liability_insurers: Beneficiary (institutional/arbitrage) — underwrites and shapes the deployer-side exposure
 *   - ai_safety_auditors: Beneficiary (organized/mobile) — sells the diligence the rule makes the deployer's burden
 *   - open_source_model_maintainers: Beneficiary without profit (moderate/mobile) — shielded, while their downstream users inherit full exposure
 *   - legislative_bodies: Agenda-setter (institutional/constrained) — enacts the allocation and would have to unwind entrenched structures to change it
 *   - tort_law_scholars: Analytical observer (analytical/analytical) — traces the control principle's doctrinal lineage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.58).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.55).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.58).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer-Primary AI Liability Allocation").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology governance/legal theory/regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '7cb489e8-0ed7-469b-bfcc-688afdbcd624').
narrative_ontology:cs_kernel_codification('7cb489e8-0ed7-469b-bfcc-688afdbcd624', formalized).
narrative_ontology:cs_authority_grounding('7cb489e8-0ed7-469b-bfcc-688afdbcd624', lineage).
narrative_ontology:cs_interpretation_layer_present('7cb489e8-0ed7-469b-bfcc-688afdbcd624').
narrative_ontology:cs_reading_relation('7cb489e8-0ed7-469b-bfcc-688afdbcd624', liability_attribution__developer_liability, forecloses).
narrative_ontology:cs_reading_relation('7cb489e8-0ed7-469b-bfcc-688afdbcd624', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('7cb489e8-0ed7-469b-bfcc-688afdbcd624', foundational, liability_follows_deployment_control).
narrative_ontology:cs_axiom_status(liability_follows_deployment_control, holdable).
narrative_ontology:cs_axiom_grounding('7cb489e8-0ed7-469b-bfcc-688afdbcd624', liability_follows_deployment_control, deontological).
narrative_ontology:cs_axiom('7cb489e8-0ed7-469b-bfcc-688afdbcd624', secondary, provider_shield_preserves_capability_investment).
narrative_ontology:cs_axiom_status(provider_shield_preserves_capability_investment, holdable).
narrative_ontology:cs_axiom_grounding('7cb489e8-0ed7-469b-bfcc-688afdbcd624', provider_shield_preserves_capability_investment, instrumental).
narrative_ontology:cs_reference_frame('7cb489e8-0ed7-469b-bfcc-688afdbcd624', deployment_context_control_baseline).
narrative_ontology:cs_drift_state('7cb489e8-0ed7-469b-bfcc-688afdbcd624', contemporary_foundation_model_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cb489e8-0ed7-469b-bfcc-688afdbcd624', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, liability_insurers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, ai_safety_auditors).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, open_source_model_maintainers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, harmed_third_parties).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, enterprise_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, small_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, enterprise_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, harmed_third_parties).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, deployment_control_principle).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, cheapest_cost_avoider_deterrence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train and license general-purpose foundation models, selling API access and enterprise licenses. Under this allocation they are not the defendants when deployed systems cause harm; deployment risk sits with the customer. Their standard contracts disclaim warranties, cap their own exposure, and place monitoring and compliance duties on the licensee. If the allocation changed they could restructure licensing terms, move serving infrastructure between jurisdictions, or shift to provider-managed deployment tiers.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Integrate foundation models into customer-facing products and internal decisions, underwriting, hiring triage, clinical support, and make the deployment decisions the rule treats as decisive. They carry the legal exposure when systems misbehave, negotiate for indemnification that providers rarely grant, and fund diligence programs that cannot fully inspect the models they run. They also gain a cost moat: the compliance burden falls harder on smaller rivals. They can self-insure, reprice contracts, or drop AI features, but their installed operations depend on the capabilities.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, enterprise_deployers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, enterprise_deployers, beneficiary).

% Run AI tools inside small businesses such as clinics, agencies, and retailers, with no legal department, no negotiated license terms, and no ability to audit the underlying model. They accept the provider's standard terms as-is and carry the same primary exposure as large deployers. A single adverse judgment or regulatory action can exceed their capitalization, and stopping AI use is often competitively impossible once workflows depend on it.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, small_deployers, payer,
    moderate, biographical, trapped, national).

% Are the people on the receiving end of AI-informed decisions such as loan applicants, job candidates, patients, and benefit claimants. They bear the first incidence of deployment failures but have no seat in the design of the liability allocation; they enter the system only after injury, as claimants against whichever party the rule names.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_decision_subjects, excluded,
    powerless, immediate, trapped, global).

% Hold a direct claim against the deployer, who is identifiable, local, and present in the courtroom in a way a model provider is not. Their recovery, however, is bounded by the deployer's solvency and insurance limits, and they carry the burden of proving what an opaque system did.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, harmed_third_parties, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, harmed_third_parties, payer).

% Underwrite the deployer-side exposure the allocation creates, collecting premiums on a near-mandatory insurable interest and shaping deployer safety practice through policy terms and exclusions. They can reprice, exclude model classes, or withdraw from the line entirely if the allocation shifts.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, liability_insurers, beneficiary,
    institutional, biographical, arbitrage, global).

% Sell the diligence that the rule makes the deployer's burden: bias audits, red-teaming, documentation review. Demand for their services is created by the allocation rather than demonstrated by harm reduction, and their findings function in litigation as evidence of care or negligence.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_safety_auditors, beneficiary,
    organized, biographical, mobile, continental).

% Publish model weights without warranty, support, or contractual relationship with downstream users. The allocation leaves them outside the liability chain entirely while every business that downloads a checkpoint inherits the full deployer exposure with no recourse upward. Most operate without legal review of any kind and can stop maintaining or relicense at will.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, open_source_model_maintainers, beneficiary,
    moderate, generational, mobile, global).

% Enact and revise the allocation through statutes, directives, and safe-harbor provisions, guided by industry consultation in which provider interests are professionally represented and deployer interests arrive fragmented. Revisiting the allocation means unwinding insurance and contract structures that have already organized around it.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, legislative_bodies, agenda_setter,
    institutional, generational, constrained, continental).

% Trace the allocation to the control principle's doctrinal lineage in employer liability, products liability, and data-controller regimes, and publish on whether the principle survives model opacity and provider fine-tuning. They bear nothing and collect nothing under the rule.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, tort_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the defendant-allocation problem for a harm chain spanning multiple parties: a default rule naming one primarily answerable party lets victims sue without litigating the whole value chain first, lets insurers price a defined exposure, and concentrates ex ante prevention incentives on the party that chooses the deployment context, the oversight arrangements, and the go-live decision.
% TRANSFER_FUNCTION: Moves expected liability costs, diligence costs, and insurance premiums from capability providers to deployers, regressively since small deployers carry the same exposure with less capacity, and moves compensation from deployers and their insurers to harmed parties where the deployer is solvent. It also moves the epistemic burden: model opacity becomes the deployer's diligence problem rather than the provider's disclosure duty.
% ABSENT_VOICES: People who will be harmed by deployed systems (ai_decision_subjects) have no seat in the allocation debate and arrive only as claimants after injury. Small deployers are represented mainly by associations dominated by large ones; open-source maintainers and downstream users of open-weight models were largely absent from legislative consultation. Capability-provider interests, by contrast, are professionally represented throughout.
% DISAPPEARANCE_RATIONALE: If the deployer-primary rule vanished overnight, every AI harm case would reopen the who-is-answerable fight across the value chain; insurers would withdraw or reprice pending clarity; deployment contracts would be rewritten; and risk-averse sectors such as health, finance, and hiring would pause deployment while providers faced direct claims they currently never see. The AI deployment economy is organized around this allocation.
% FOUNDING_PROBLEM: Capability providers argued they cannot anticipate or govern downstream uses of general-purpose models, early deployers wanted a clear answerable counterparty, and regulators wanted prevention incentives on the party making deployment decisions. The rule was built to assign primary responsibility by control, since the deployer chooses the context, the data, the oversight, and the go-live, while keeping open-ended downstream claims off a nascent foundation-model industry.
% FOUNDING_PROBLEM_CORROBORATION: Deployer associations and enterprise counsel attest the control rationale's descriptive core, that deployers do make the deployment decisions, while contesting the allocation it grounds; tort-law scholarship traces the control principle to employer-liability and data-controller doctrine from outside the benefiting parties; insurer actuarial analyses, though from a beneficiary seat, attest on pricing evidence that deployer-side exposure is real and insurable. The shield-for-providers component of the founding problem is attested mainly by providers themselves and their trade associations; no source outside the benefiting parties independently attests that the shield remains necessary.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Claimed type tangled_rope: the allocation has both a genuine coordination function (a default defendant rule that makes AI risk insurable and assigns prevention incentives) and an asymmetric transfer (capability providers shielded while deployers carry exposure they cannot fully inspect). The metrics are authored independently of the claim. Extractiveness 0.58 reflects a substantial but not total transfer: deployers retain real context control that partially justifies their position, and harmed parties gain an identifiable defendant. Suppression 0.55 is a raw structural property, unscaled by power or scope (the engine owns that arithmetic): deployers cannot contract out of the allocation because providers refuse indemnification, cannot choose a different allocation within a jurisdiction, and face active court enforcement. Theater 0.32: a growing share of diligence activity functions as litigation armor rather than harm reduction. Accessibility collapse 0.35 is low because the alternative allocations remain live legislative proposals in multiple jurisdictions; the design space has not closed. Resistance 0.6: deployer associations, insurer pricing pushback, and jurisdictional competition actively contest the allocation. All three measurement series run on one shared time grid (t = 0, 3, 6, 9, 12, 15 over a 2020-2035 window); points at t >= 9 are marked projected. The rising base_extractiveness series models the transfer deepening as deployment broadens to small deployers and capability opacity grows; the rising theater_ratio models audits becoming shields; the rising suppression_requirement models the enforcement machinery (case law, contractual indemnity enforcement, regulatory guidance) hardening around the allocation.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and payer seats should compute different types from the same structure. From foundation_model_providers the allocation is a clean enabling rule that keeps open-ended downstream claims off capability development and prices deployment risk where deployment decisions are made. From small_deployers it operates as an uninsurable-in-practice exposure attached to a capability they cannot inspect, negotiate, or refuse. Enterprise deployers sit between and are genuinely dual-positioned: they bear the exposure but also gain a cost moat, since the diligence burden falls harder on smaller rivals — a same-level divergence where actors with equal nominal status (deployer) split by capacity and exit. The agenda-setter seat experiences neither the exposure nor the shield and reads the rule as policy design; the excluded seat experiences only the downstream decisions, never the allocation that governs who answers for them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations put foundation_model_providers, liability_insurers, ai_safety_auditors, open_source_model_maintainers, and harmed_third_parties at the low-directionality end: the liability shield, the premium stream, the audit demand, and the identifiable defendant are all receipts of this arrangement. The victim declarations put enterprise_deployers and small_deployers at the high-directionality end, with small_deployers' trapped exit pushing them nearest the full-target position. Harmed third parties are genuinely mixed: declared beneficiary because the rule names them an answerable counterparty, carrying a secondary payer position because recovery is bounded by deployer solvency; the deployer_solvency_recovery_gap omega tracks whether the beneficiary declaration overstates their position. No directionality overrides are authored: beneficiary/victim declarations plus exit options already separate the seats correctly, and the derivation distinguishes enterprise from small deployers through their different power atoms and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two mislabels. A pure-extraction reading ('the rule just shifts risk onto the weak') would erase the real coordination function: without a default defendant rule, AI exposure is uninsurable, victims litigate the whole value chain, and prevention incentives diffuse to nobody. A pure-coordination reading (the provider framing that clear rules enable innovation) would erase the asymmetric transfer: the shield for providers and the diligence burden on deployers who cannot inspect what they run. The founding problem is still partially live, since deployers do control context, but contested, because provider fine-tuning services, unilateral model updates, and opacity have moved control toward the provider in exactly the dimensions the rule assigns to the deployer. If the control premise fails empirically, this reading's own logic routes toward the shared_liability sibling rather than toward pure extraction; the founding_problem_status 'contested' plus world_rearranges verdict correctly produces no dead-mandate mismatch flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_kernel_reading_contest,
    'Is deployer-primary liability the correct resolution of the liability_attribution kernel, or would the developer_liability or shared_liability reading produce a structurally different and better-grounded constraint?',
    'Comparative analysis across jurisdictions adopting different allocations, legislative outcomes, and case-law development on which party courts actually hold answerable when deployment-context control and capability creation point in different directions.',
    'Under developer_liability, deployers exit the victim set and foundation model providers enter it, inverting this reading''s beneficiary/victim structure. Under shared_liability, the sharp deployer-pays/provider-shielded asymmetry dissolves into a diffused value-chain distribution and the concentrated gain_flow disappears.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_kernel_reading_contest, preference, 'This constraint is one reading of the liability_attribution kernel; sibling readings would restructure the beneficiary and victim sets.').

omega_variable(
    opacity_control_mismatch,
    'Does the deployer''s control over deployment context suffice to justify primary liability when the underlying capability is opaque, updated unilaterally by the provider, and not fully auditable by the deployer?',
    'Empirical study of deployer diligence efficacy under opacity, disclosure mandates, and case law on what counts as adequate diligence when the deployer cannot inspect the capability; natural experiments from jurisdictions requiring provider disclosure.',
    'If deployer control is illusory in the capability dimensions that cause harm, the control principle''s justificatory core fails and this reading drifts toward the shared_liability sibling; if deployers genuinely select and shape deployment outcomes, the allocation holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_control_mismatch, empirical, 'Whether deployment-context control remains a sound liability-grounding relation under model opacity.').

omega_variable(
    deployer_solvency_recovery_gap,
    'Is harmed parties'' recovery under this allocation systematically bounded by deployer solvency, such that the beneficiary declaration for harmed_third_parties overstates their position?',
    'Claims data comparing awarded damages against actual recovery by deployer size class, and insurance-market analysis of coverage availability and pricing for small deployers.',
    'A systematic recovery gap would move harmed_third_parties toward the payer side, raise measured extractiveness, and strengthen the case that the allocation''s coordination benefit accrues mainly to providers and insurers rather than to the injured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployer_solvency_recovery_gap, empirical, 'Whether the rule delivers compensation or an empty chair when deployers are undercapitalized.').

omega_variable(
    provider_shield_rationale_status,
    'Is the shield for foundation model providers still justified by industry nascence, or does it now operate as risk externalization from a mature industry with pricing power and insurance access of its own?',
    'Market maturity analysis: provider margins, availability and cost of provider-side liability coverage, and whether providers now sell deployment-adjacent services (fine-tuning, guardrails, managed deployment) that give them deployment-context knowledge the founding problem assumed they lacked.',
    'If the industry has matured, the shield''s coordination justification decays, the arrangement''s extractive component grows, and this reading drifts toward the shared_liability sibling or toward reclassification; if genuine nascence persists in frontier capabilities, the shield retains its founding-problem warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provider_shield_rationale_status, empirical, 'Whether the developer shield still serves its founding rationale or now functions as pure risk transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deployer_liability_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(deployer_liability_tr_t0, observed).
narrative_ontology:measurement(deployer_liability_tr_t3, liability_attribution__deployer_liability, theater_ratio, 3, 0.19).
narrative_ontology:measurement_basis(deployer_liability_tr_t3, observed).
narrative_ontology:measurement(deployer_liability_tr_t6, liability_attribution__deployer_liability, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(deployer_liability_tr_t6, observed).
narrative_ontology:measurement(deployer_liability_tr_t9, liability_attribution__deployer_liability, theater_ratio, 9, 0.28).
narrative_ontology:measurement_basis(deployer_liability_tr_t9, projected).
narrative_ontology:measurement(deployer_liability_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(deployer_liability_tr_t12, projected).
narrative_ontology:measurement(deployer_liability_tr_t15, liability_attribution__deployer_liability, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(deployer_liability_tr_t15, projected).

% Extraction over time
narrative_ontology:measurement(deployer_liability_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(deployer_liability_be_t0, observed).
narrative_ontology:measurement(deployer_liability_be_t3, liability_attribution__deployer_liability, base_extractiveness, 3, 0.47).
narrative_ontology:measurement_basis(deployer_liability_be_t3, observed).
narrative_ontology:measurement(deployer_liability_be_t6, liability_attribution__deployer_liability, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(deployer_liability_be_t6, observed).
narrative_ontology:measurement(deployer_liability_be_t9, liability_attribution__deployer_liability, base_extractiveness, 9, 0.55).
narrative_ontology:measurement_basis(deployer_liability_be_t9, projected).
narrative_ontology:measurement(deployer_liability_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(deployer_liability_be_t12, projected).
narrative_ontology:measurement(deployer_liability_be_t15, liability_attribution__deployer_liability, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(deployer_liability_be_t15, projected).

% Suppression requirement over time
narrative_ontology:measurement(deployer_liability_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(deployer_liability_su_t0, observed).
narrative_ontology:measurement(deployer_liability_su_t3, liability_attribution__deployer_liability, suppression_requirement, 3, 0.44).
narrative_ontology:measurement_basis(deployer_liability_su_t3, observed).
narrative_ontology:measurement(deployer_liability_su_t6, liability_attribution__deployer_liability, suppression_requirement, 6, 0.49).
narrative_ontology:measurement_basis(deployer_liability_su_t6, observed).
narrative_ontology:measurement(deployer_liability_su_t9, liability_attribution__deployer_liability, suppression_requirement, 9, 0.52).
narrative_ontology:measurement_basis(deployer_liability_su_t9, projected).
narrative_ontology:measurement(deployer_liability_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.54).
narrative_ontology:measurement_basis(deployer_liability_su_t12, projected).
narrative_ontology:measurement(deployer_liability_su_t15, liability_attribution__deployer_liability, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(deployer_liability_su_t15, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI liability attribution' covers three structurally distinct allocations, decomposed per the epsilon-invariance principle. This story (deployer_liability, tangled_rope, epsilon 0.58, victims = deployers) is the reading where the control principle grounds primary liability. The developer_liability sibling inverts the beneficiary/victim structure (providers become the target seat, deployers the shielded beneficiary); the shared_liability sibling diffuses extraction along the value chain and eliminates the concentrated gain_flow seat. This reading structurally influences the shared reading: once insurance markets, contractual indemnity architectures, and case law organize around deployer-primary liability, any shift to distributed allocation carries path-dependent switching costs. This reading forecloses the developer reading within any single framework, since both claim the unique primary locus. The upstream/downstream epistemic link runs through the control principle's doctrinal lineage: the stronger the empirical case that deployers control what matters, the stronger this reading and the weaker its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
