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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer-Primary AI Liability Allocation
 *   domain: technology governance/legal theory/regulatory design
 *
 * SUMMARY:
 *   A maturing body of statutes, tort doctrines, and procurement norms fixes
 *   primary liability for AI-caused harm on the developers who create the
 *   underlying capability, regardless of who chose the deployment context in
 *   which the harm materialized. The rule solved a real problem — injured
 *   parties facing a value chain in which every node blamed the next — and
 *   simultaneously handed deployers a structural gift: the costs of harms
 *   their choices precipitated land upstream. Disclosure obligations convert
 *   model opacity from a shared epistemic problem into the developer's
 *   private compliance burden. This file instantiates ONE reading of the
 *   liability_attribution kernel — the developer_liability reading — as a
 *   clean, single-epsilon constraint; the deployer_liability and
 *   shared_liability readings are separate constraints with inverted or
 *   graded victim/beneficiary structures, linked via
 *   network.affects_constraints. The epsilon referent is the standing
 *   developer-primary arrangement as it actually operates, never some
 *   endorsed alternative allocation.
 *
 * KEY AGENTS:
 *   - - national_legislatures: Agenda setter (institutional/constrained) — enact and politically defend the allocation default
 *   - - ai_deployers: Primary beneficiary (organized/mobile) — receive externalized risk, cheapest exit in the structure
 *   - - liability_insurers: Secondary beneficiary (institutional/arbitrage) — collect premiums on the risk pool the rule stabilizes
 *   - - harmed_third_parties: Incidental beneficiary (powerless/constrained) — gain a solvent defendant; no seat in rule-making
 *   - - foundation_model_developers: Primary target with dual position (institutional/constrained) — bear the transfer while collecting moat effects
 *   - - open_source_model_maintainers: Primary target (powerless/identity_locked) — uninsurable exposure fused with community identity
 *   - - independent_ai_builders: Target (moderate/constrained) — regressive fixed overhead
 *   - - downstream_fine_tuners: Excluded voice (powerless/trapped) — generate proximate harms, absent from the conversation
 *   - - legal_academia: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.62).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.55).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.62).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer-Primary AI Liability Allocation").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology governance/legal theory/regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '0f550dcc-4e36-484f-aa69-ecf94e49c9c0').
narrative_ontology:cs_kernel_codification('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', formalized).
narrative_ontology:cs_authority_grounding('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', lineage).
narrative_ontology:cs_interpretation_layer_present('0f550dcc-4e36-484f-aa69-ecf94e49c9c0').
narrative_ontology:cs_reading_relation('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', foundational, capability_creation_anchors_liability).
narrative_ontology:cs_axiom_status(capability_creation_anchors_liability, holdable).
narrative_ontology:cs_axiom_grounding('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', capability_creation_anchors_liability, deontological).
narrative_ontology:cs_axiom('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', secondary, safety_investment_priced_at_creation).
narrative_ontology:cs_axiom_status(safety_investment_priced_at_creation, holdable).
narrative_ontology:cs_axiom_grounding('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', safety_investment_priced_at_creation, instrumental).
narrative_ontology:cs_reference_frame('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', creator_answerability_framework).
narrative_ontology:cs_drift_state('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', contemporary_deployer_duty_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f550dcc-4e36-484f-aa69-ecf94e49c9c0', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, ai_deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, liability_insurers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, harmed_third_parties).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, foundation_model_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_model_maintainers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, independent_ai_builders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, foundation_model_developers).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, creator_responsibility_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, precautionary_innovation_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and amend the statutes and doctrinal positions that fix where primary responsibility for AI-caused harm rests. Once the allocation is framed publicly as consumer protection, reversal carries heavy political cost even as evidence accumulates that the burden lands far from the point of greatest causal control. Respond to incident-salience cycles rather than to base-rate harm data.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, national_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Integrate third-party models into products and services. Under this allocation they face sharply reduced primary exposure for harms originating in the capability itself, negotiate indemnities shifting residual risk upstream, and bring offerings to market faster. Exit is comparatively cheap: they can switch model vendors, run multiple suppliers, or self-host, and the liability rule follows them nowhere.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, ai_deployers, beneficiary,
    organized, biographical, mobile, global).

% Underwrite the new developer-liability market created by the allocation: collect premiums sized to model-class risk, shape actuarial standards for what counts as adequate disclosure, and lobby for allocation stability because a movable anchor destroys rateable risk pools. Capital mobility lets them withdraw from unprofitable lines entirely.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, liability_insurers, beneficiary,
    institutional, generational, arbitrage, continental).

% People injured by deployed AI systems. The allocation gives them a solvent, identifiable defendant regardless of who made the deployment choices that produced the injury, raising recovery odds compared with diffuse value-chain litigation. The trade-off they cannot vote on: deterrence aimed at the wrong node may increase the frequency of the very incidents they must then litigate. They enter the process only after harm, as claimants.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, harmed_third_parties, beneficiary,
    powerless, immediate, constrained, global).

% Train and release frontier models. Bear primary exposure for harms arising in deployment contexts they neither selected nor supervise, fund compulsory insurance and mandated disclosure programs describing internal states they themselves cannot fully inspect, and cannot contract out of statutory or tort allocation once products ship. At the same time, fixed compliance and insurance costs fall hardest on smaller rivals, concentrating market share toward incumbents — a benefit they collect without administering the rule.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, foundation_model_developers, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, foundation_model_developers, beneficiary).

% Volunteer maintainers who release capable model weights openly. They cannot purchase affordable coverage against downstream misuse at scale, cannot monitor or recall deployments, and hold personal assets against tort claims. Releasing openly is constitutive of their practice and standing in their communities; stepping back from it means abandoning the identity that organizes their working life, so the realistic option set is narrower than resignation suggests.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_model_maintainers, payer,
    powerless, biographical, identity_locked, global).

% Small firms and individual builders commercializing model-derived products. Insurance minimums and disclosure obligations operate as fixed overhead scaled independently of revenue, which they can least afford; the practical exits are pivoting out of AI altogether or selling to larger players, both of which shrink the independent sector the allocation ostensibly regulates.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, independent_ai_builders, payer,
    moderate, biographical, constrained, regional).

% Parties who modify, fine-tune, or compose models after release, generating a large share of proximate harms. They have no seat in the legislative and doctrinal conversations that set the allocation; their causal contribution is invisible to the rule's categories. Organizing them is structurally difficult — dispersed, often pseudonymous, and lacking a trade association.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, downstream_fine_tuners, excluded,
    powerless, immediate, trapped, global).

% Scholars tracing the doctrinal movement between creation-anchored and control-anchored attribution across jurisdictions, publishing comparative analyses and restatement proposals. No material stake in the allocation's direction; their influence runs through citation networks and bench appointments rather than through the rule itself.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, legal_academia, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, ai_deployers).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns one determinate bearer of primary responsibility when an AI system causes harm, ending the value-chain diffusion in which creator, deployer, integrator, and user each point elsewhere; concentrates the financial incentive for safety investment at the point where capability design decisions are made.
% TRANSFER_FUNCTION: Moves expected liability cost — litigation, settlements, compulsory insurance premiums, mandated disclosure programs — from deployers and incident contexts to developers; moves compensation toward injured third parties, funded disproportionately by developer-side premiums and settlements.
% ABSENT_VOICES: Downstream fine-tuners and application operators who generate many proximate harms have no seat in drafting rooms and would press for contribution-weighted attribution. Volunteer maintainers reach the table only through occasional advocacy letters. Injured third parties arrive solely as post-hoc claimants with no role in setting the rule that governs their future injuries.
% DISAPPEARANCE_RATIONALE: Without the allocation, courts lose the default-defendant rule and revert to multi-party finger-pointing litigation in which every node blames the adjacent one; insurers lose a rateable pool and retreat from the line; release practices, indemnity clauses, and procurement contracts written around the current anchor would all be renegotiated; deployment decisions presently priced as external to the deployer would be repriced internally overnight.
% FOUNDING_PROBLEM: Early high-profile AI harm cases stalled because responsibility diffused along the value chain: developers pointed to deployer misuse, deployers pointed to model provenance, integrators pointed to both, and injured parties recovered nothing while no actor faced a decisive financial reason to invest in prevention.
% FOUNDING_PROBLEM_CORROBORATION: Court dockets of dismissed or abandoned AI-harm suits, actuarial analyses published by insurance industry associations, and filings by victim-advocacy organizations all attest the attribution-diffusion problem from seats outside the beneficiary set. Industry groups also attest it, but they sit inside the beneficiary set, so corroboration rests on the courts-insurers-victim-advocates triad rather than on the parties the arrangement enriches.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Base extractiveness sits at 0.62 because the liability transfer is systematically decoupled from control: developers answer financially for outcomes shaped by deployer configuration choices, user misuse, and post-release modification they cannot observe, and the compliance overhead (coverage minimums, disclosure programs) scales with model capability rather than with the developer's share of causal contribution. Suppression is 0.55 as a raw, unscaled structural property: within any operating jurisdiction a developer cannot contract out of the allocation, and for open-source maintainers there is effectively no compliant path at all — though alternative attributions remain live across jurisdictions and doctrinal schools, which is why suppression is substantial rather than overwhelming. Theater is 0.31 and climbing: model cards and usage policies began as functional safety artifacts and increasingly function as liability shields — documents produced to demonstrate diligence rather than to inform downstream decisions. Accessibility_collapse is 0.45 because rival readings survive in adjacent jurisdictions and academic proposals; the rule forecloses alternatives only within its own enforcement perimeter. Resistance is 0.6: sustained lobbying, jurisdictional arbitrage, strategic release-withholding, and open-community pushback all register. The three measurement series run on one shared time grid (points 0-30) so every metric is authored at every examined time point; the rising suppression_requirement trajectory tracks the genuine build-out of enforcement machinery (mandatory disclosure regimes, audit requirements, cross-border enforcement cooperation), not mere metric substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the legislature's seat the allocation reads as protective coordination — a consumer-protection triumph over corporate finger-pointing. From the open-source maintainer's seat the identical rule computes as existential exposure administered by institutions the maintainer cannot influence and cannot afford. The foundation_model_developer seat splits internally: it pays the transfer while quietly collecting the moat effect, so its computed classification depends on which structural relationship the engine weighs. The deployer and insurer seats should compute as subsidized or lightly loaded. The engine derives this divergence from the authored power/exit/role data; nothing in the claimed type adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the low-d end: ai_deployers externalize the exact costs the rule relocates and hold the cheapest exit in the structure (mobile, multi-supplier), putting them nearest the subsidy pole; liability_insurers collect rents on the stabilized pool with arbitrage-grade exit; harmed_third_parties receive compensation transfers rather than rents, so their d is low without implying they administer anything. Targets sit near the high-d end: open_source_model_maintainers combine uninsurable exposure, zero organizational power, and identity-fused exit, placing them closest to full-target; independent_ai_builders add constrained exits; foundation_model_developers are institutional but constrained (shipped products, reputational and contractual lock-in), keeping their derived d high with slight damping from the secondary moat benefit. Spatial scope matters asymmetrically: developer seats operate globally, where verifying downstream use is hardest, so the engine's scope amplification lands squarely on the highest-d seats while nationally-scoped agenda setters feel little of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — responsibility diffusion along the value chain — remains live: courts still encounter harm chains no single node will own. Declaring the mandate resolved would be premature, and no sunset mechanism exists. The tangled_rope classification earns its keep by blocking both symmetrical misreadings: a pure-rope reading would hide the deployer-side externalization that is the arrangement's largest unilateral transfer, while a pure-snare reading would discard the real accountability service that injured third parties — who have no other solvent defendant — genuinely receive from it. The R5 mismatch consumer should find no zombie signature here: status=live paired with verdict=world_rearranges, and the temporal record shows extraction accumulating on top of a functioning coordination core rather than replacing one. The watch item is the theater trajectory: if disclosure ritualization continues its climb while incident-prevention yield stays flat, the arrangement drifts toward enforcement-only operation, at which point the coordination half of the tangled rope thins toward piton dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (developer_liability) of the liability_attribution kernel; what would the sibling readings (deployer_liability, shared_liability) change structurally, and where is the disagreement located?',
    'Track statutory defaults, appellate rulings, and model-contract indemnity clauses across jurisdictions over time: convergence on a single anchor resolves the kernel toward that reading; durable pluralism confirms three coexisting constraints requiring separate stories.',
    'Switching the anchor inverts or grades the entire victim/beneficiary structure — a deployer-anchor regime removes foundation_model_developers and open_source_model_maintainers from the victim set and inserts deployers; a shared-anchor regime dissolves the binary into contribution-weighted payer shares, collapsing the tangled_rope asymmetry this story authors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this file is the creator-anchored reading of a three-way kernel; the structural delta between readings is the location of the liability anchor.').

omega_variable(
    causal_weight_creation_vs_deployment,
    'What share of realized AI harm is causally attributable to capability-inherent properties versus deployment-context choices?',
    'Structured causal decomposition across a large incident-forensics registry, separating design-stage factors from configuration, integration, and use-stage factors.',
    'If deployment-context factors dominate, developer-primary allocation misprices deterrence — undercharging the node with the most control and overcharging the node with the least — pushing the arrangement''s effective profile toward pure extraction; if capability-inherent factors dominate, the coordination function is sound and measured extraction is largely the price of the accountability service itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_weight_creation_vs_deployment, empirical, 'Whether the liability transfer tracks causal contribution or systematically misses it.').

omega_variable(
    disclosure_burden_productivity,
    'Does assigning opacity management to developers produce usable downstream transparency or ritual disclosure artifacts?',
    'Compare downstream-actor comprehension, incident-response utility, and decision-relevance of developer disclosures across regimes with differing disclosure mandates; audit whether disclosure content changes deployment behavior.',
    'A ritual outcome drives the theater_ratio trajectory higher and supports eventual piton-style drift (compliance performance substituting for prevention); productive disclosure sustains the coordination half of the tangled rope and caps theater growth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_burden_productivity, empirical, 'Whether the developer-borne opacity burden functions or performs.').

omega_variable(
    insurance_market_absorption,
    'Does developed insurance capacity convert developer liability exposure into priced overhead, or leave uninsured classes (volunteer maintainers, small builders) carrying uncompensable tail risk?',
    'Premium and coverage-denial data plus exit surveys among open-source maintainers and independent builders, segmented by insurability.',
    'Absorption softens effective extraction for insured institutional seats while hardening it for uninsured seats — bifurcating the payer seat the engine computes and potentially splitting one apparent victim group into structurally distinct populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_market_absorption, empirical, 'Whether the insurance layer equalizes or stratifies the developer burden.').

omega_variable(
    open_source_release_abandonment_threshold,
    'At what expected liability exposure do volunteer maintainers stop releasing capable open weights, and does the ecosystem substitute anonymous or unattributable releases?',
    'Longitudinal analysis of open-weight release rates correlated with enacted-and-enforced liability milestones across jurisdictions, including shifts toward unattributable distribution channels.',
    'Mass abandonment would collapse the rule''s claimed incentive channel — safety effort priced at creation presumes identifiable creators to charge — while liability continues accruing against shrinking, less accountable targets, accelerating drift toward enforcement-only operation and weakening the coordination function this classification depends on.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_release_abandonment_threshold, empirical, 'Threshold at which the constraint destroys its own addressable population of regulated creators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(developer_liability_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(developer_liability_tr_t0, observed).
narrative_ontology:measurement(developer_liability_tr_t6, liability_attribution__developer_liability, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(developer_liability_tr_t6, observed).
narrative_ontology:measurement(developer_liability_tr_t12, liability_attribution__developer_liability, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(developer_liability_tr_t12, observed).
narrative_ontology:measurement(developer_liability_tr_t18, liability_attribution__developer_liability, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(developer_liability_tr_t18, observed).
narrative_ontology:measurement(developer_liability_tr_t24, liability_attribution__developer_liability, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(developer_liability_tr_t24, observed).
narrative_ontology:measurement(developer_liability_tr_t30, liability_attribution__developer_liability, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(developer_liability_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(developer_liability_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(developer_liability_be_t0, observed).
narrative_ontology:measurement(developer_liability_be_t6, liability_attribution__developer_liability, base_extractiveness, 6, 0.47).
narrative_ontology:measurement_basis(developer_liability_be_t6, observed).
narrative_ontology:measurement(developer_liability_be_t12, liability_attribution__developer_liability, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(developer_liability_be_t12, observed).
narrative_ontology:measurement(developer_liability_be_t18, liability_attribution__developer_liability, base_extractiveness, 18, 0.56).
narrative_ontology:measurement_basis(developer_liability_be_t18, observed).
narrative_ontology:measurement(developer_liability_be_t24, liability_attribution__developer_liability, base_extractiveness, 24, 0.59).
narrative_ontology:measurement_basis(developer_liability_be_t24, observed).
narrative_ontology:measurement(developer_liability_be_t30, liability_attribution__developer_liability, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(developer_liability_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(developer_liability_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(developer_liability_su_t0, observed).
narrative_ontology:measurement(developer_liability_su_t6, liability_attribution__developer_liability, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(developer_liability_su_t6, observed).
narrative_ontology:measurement(developer_liability_su_t12, liability_attribution__developer_liability, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(developer_liability_su_t12, observed).
narrative_ontology:measurement(developer_liability_su_t18, liability_attribution__developer_liability, suppression_requirement, 18, 0.48).
narrative_ontology:measurement_basis(developer_liability_su_t18, observed).
narrative_ontology:measurement(developer_liability_su_t24, liability_attribution__developer_liability, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(developer_liability_su_t24, observed).
narrative_ontology:measurement(developer_liability_su_t30, liability_attribution__developer_liability, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(developer_liability_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, resource_allocation).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'AI liability attribution' per the epsilon-invariance principle: the label covers three structurally distinct claims whose epsilon values diverge because the victim and beneficiary sets invert or grade with the chosen anchor. This member (developer_liability) authors epsilon over the developer-bearing arrangement; deployer_liability authors the inverted arrangement; shared_liability replaces the binary structure with contribution-weighted shares. Upstream, the inherited product-liability tradition (manufacturer-anchored strict liability with comparative fault) feeds all three members and is cited as evidence for each; edges here link the siblings so contamination propagation can trace how a doctrinal shift at one anchor pressures the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
