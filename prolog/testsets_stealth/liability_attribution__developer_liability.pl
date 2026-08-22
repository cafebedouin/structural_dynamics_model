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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer-Primary Liability Attribution for AI Capability Harms
 *   domain: technology governance/legal theory/regulatory design
 *
 * SUMMARY:
 *   The developer-liability reading of the liability_attribution kernel fixes
 *   primary legal responsibility for harms from deployed AI systems on the
 *   parties that created the underlying capability. The arrangement solves a
 *   real problem — when creation and deployment are split across different
 *   parties, victims otherwise face a chain of possible defendants and no
 *   party fully owns the risk — but it does so by placing the liability load
 *   on developers, who control the capability yet not the deployment context
 *   in which much harm materializes. Deployers integrate models into
 *   products, make the context decisions that shape outcomes, and answer
 *   mainly for ordinary negligence; the difference between their exposure and
 *   their causal contribution is the arrangement's transfer. The claim/metric
 *   gap is deliberate: the arrangement is CLAIMED as tangled_rope (genuine
 *   accountability coordination plus asymmetric burden), and the authored
 *   metrics independently describe moderately-high, slowly accumulating
 *   extraction with rising enforcement intensity — the engine measures the
 *   divergence; nothing here reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - - ai_model_developers: Primary target (institutional/constrained) — bears liability, defense, and disclosure costs for harms shaped substantially downstream of them
 *   - - ai_deployers: Primary beneficiary (powerful/mobile) — receives externalized risk; retains only residual negligence exposure
 *   - - injured_end_users: Secondary beneficiary with payer residue (organized/constrained) — gains a single solvent defendant but pays through litigation complexity and product pricing
 *   - - liability_insurance_underwriters: Secondary beneficiary (institutional/arbitrage) — the fixed attribution defines their insurable event and premium base
 *   - - courts_and_regulators: Agenda setter (institutional/analytical) — administers the rule and articulates the standard of care it implies
 *   - - open_source_contributors: Excluded voice (moderate/constrained) — outside the rule's clean application yet inside its blast radius; absent from rulemaking
 *   - - tort_law_scholars: Analytical observer (analytical/analytical) — maps incentive effects across alternative attribution schemes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.62).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.55).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.62).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer-Primary Liability Attribution for AI Capability Harms").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology governance/legal theory/regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '395184eb-cea4-4d78-a69c-5054ebee8971').
narrative_ontology:cs_kernel_codification('395184eb-cea4-4d78-a69c-5054ebee8971', formalized).
narrative_ontology:cs_authority_grounding('395184eb-cea4-4d78-a69c-5054ebee8971', lineage).
narrative_ontology:cs_interpretation_layer_present('395184eb-cea4-4d78-a69c-5054ebee8971').
narrative_ontology:cs_reading_relation('395184eb-cea4-4d78-a69c-5054ebee8971', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('395184eb-cea4-4d78-a69c-5054ebee8971', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('395184eb-cea4-4d78-a69c-5054ebee8971', foundational, capability_creation_implies_control).
narrative_ontology:cs_axiom_status(capability_creation_implies_control, holdable).
narrative_ontology:cs_axiom_grounding('395184eb-cea4-4d78-a69c-5054ebee8971', capability_creation_implies_control, empirically_contingent).
narrative_ontology:cs_axiom('395184eb-cea4-4d78-a69c-5054ebee8971', secondary, upstream_safety_incentive_priority).
narrative_ontology:cs_axiom_status(upstream_safety_incentive_priority, holdable).
narrative_ontology:cs_axiom_grounding('395184eb-cea4-4d78-a69c-5054ebee8971', upstream_safety_incentive_priority, instrumental).
narrative_ontology:cs_reference_frame('395184eb-cea4-4d78-a69c-5054ebee8971', creator_control_risk_frame).
narrative_ontology:cs_drift_state('395184eb-cea4-4d78-a69c-5054ebee8971', general_purpose_model_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('395184eb-cea4-4d78-a69c-5054ebee8971', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, ai_deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, injured_end_users).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, liability_insurance_underwriters).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, ai_model_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, injured_end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, train, and release the underlying models. Under this attribution rule they answer in court for harms that materialize after release, including harms shaped by choices made downstream of them. They fund safety evaluation, documentation, and disclosure programs whose demanded scope expands with each contested case. Relocating development does not remove exposure to suit wherever deployments cause harm, and declining to sell into major markets is not a viable path for firms whose business is those markets.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, ai_model_developers, payer,
    institutional, generational, constrained, global).

% Integrate third-party models into products and services and make the deployment-context decisions: which customers, what guardrails, what instructions, what monitoring. Because primary responsibility sits upstream, their own exposure is limited to ordinary negligence claims, and they can select vendors partly on the strength of the vendor's liability posture and indemnity offers. Switching providers or discontinuing a model line is routine commercial practice.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, ai_deployers, beneficiary,
    powerful, biographical, mobile, global).

% People harmed by deployed systems. The rule hands them a single identifiable, typically well-resourced defendant instead of a chain of intermediaries. The cost side reaches them too: connecting a harm to a capability requires litigating through technical opacity, and whatever compensation flows is ultimately priced into the products they buy.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, injured_end_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, injured_end_users, payer).

% Price and sell coverage against the allocated exposure. A stable attribution rule defines the insurable event and the premium base; the entire product line rests on knowing whose conduct is being insured. They can reprice or withdraw from the line if the allocation shifts, which gives them a commercial stake in the rule's persistence.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, liability_insurance_underwriters, beneficiary,
    institutional, biographical, arbitrage, global).

% Adjudicate which party answers for a given harm and articulate the standard of care the rule implies; precedent accumulates into the operative allocation. They hear from every side, but the docket is dominated by post-harm disputes, so the rule's shape evolves case by case rather than by design review.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Distributed individuals contributing code, weights, or fine-tunes to freely licensed models. Entity-level liability does not map onto them cleanly: they are effectively outside the rule's reach yet inside its blast radius when their contributions are implicated in harm. They have minimal presence in the legislative and standard-setting processes where the rule's scope gets drawn.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_contributors, excluded,
    moderate, biographical, constrained, global).

% Analyze the allocation's incentive effects and doctrinal coherence across jurisdictions, publishing comparisons of alternative attribution schemes. They hold no operational stake in which seat bears the load.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, tort_law_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, ai_deployers).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every harm from a deployed AI system a single legally responsible party, so injured parties know whom to sue and the party holding the deepest knowledge of the capability carries the incentive to make it safe before release.
% TRANSFER_FUNCTION: Moves liability costs — damages, defense costs, compliance and disclosure burdens — from deployers and injured parties onto model developers, with the transfer priced into model licensing and insurance premiums.
% ABSENT_VOICES: Open-source contributors and small developers are scarcely represented in the rulemaking conversation that draws the rule's scope; end users enter chiefly as plaintiffs after harm rather than as participants in designing the attribution scheme. Both would object to features of the current allocation — the former to its unworkable application to distributed contribution, the latter to discovery burdens — and both are outside the room where scope is set.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, primary exposure would snap back to deployers under general negligence, vendor indemnity clauses and insurance contracts would be rewritten within quarters, deployer vendor-selection would reprice around newly internalized risk, and plaintiffs would face a defendant-identification problem the rule currently solves for them.
% FOUNDING_PROBLEM: Harms began arising from systems whose creation and use were split across different parties, leaving victims without a clear defendant and leaving no single party in control of the whole risk. The arrangement was built to fix accountability on the party that made the capability.
% FOUNDING_PROBLEM_CORROBORATION: Judicial opinions applying the rule, plaintiff-side litigation dockets, and tort scholarship produced outside both the developer and deployer camps attest that the split creation/use attribution problem was real and remains live. Deployer trade associations dispute the allocation but not the existence of the problem; the corroborating sources for the problem's liveness stand outside the beneficiary set.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.62 because the liability load on developers is materially decoupled from their stage-wise control: they answer for deployment-context harms they cannot inspect, and the disclosure burden grows with the opacity of their own artifacts. Suppression is 0.55 — the rule is enforced through courts and regulators with no opt-out for anyone selling into major markets, but alternatives (contractual indemnification, insurance, jurisdictional structuring) remain partly workable, so alternatives are degraded rather than eliminated. Theater ratio 0.30 reflects a growing share of safety-evaluation and documentation activity performed for the liability file rather than for risk reduction. Accessibility collapse 0.45 and resistance 0.60 fit a contested legal construct: alternatives persist, and developers actively lobby for safe harbors, litigate scope, and fund scholarship favoring reallocation. The temporal series run on ONE shared grid (t=0,6,12,18,24,30) with all three tracked metrics authored at every point: extractiveness rises monotonically as general-purpose models multiply deployment contexts faster than developer control extends; suppression_requirement rises as documentation mandates and regulatory scrutiny harden the enforcement machinery; theater rises as compliance artifacts accumulate. The rising extractiveness trajectory is the accumulation signature worth watching — it is the input the abductive accumulation trigger reads.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the developer seat the arrangement is enforced extraction: costs arrive from conduct they neither chose nor supervised. From the deployer seat the same structure is cheap, functional coordination — a clear rule about who answers, purchased at the price of ordinary-negligence care. From the bench the arrangement is simply working accountability: every docket has a defendant. From the injured-user seat it is mixed: a solvent target for suit, reached through technically forbidding discovery. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: developers (declared victims, constrained exit) sit near the full-target end; deployers (declared beneficiaries, mobile exit) derive near the full-beneficiary end; insurers (beneficiaries, arbitrage exit) likewise near the beneficiary end. Two overrides correct derivations the raw declarations would misstate. First, deployers derive as near-pure beneficiaries, but they retain residual negligence exposure and reputational risk from the deployments they control, so their d is overridden upward to 0.12. Second, injured users are declared beneficiaries (they receive the defendant-identification function) and would derive near-symmetric-to-subsidized, but they also carry the indirect cost side — litigation through opacity and compensatory pricing passed into products — so their d is overridden to 0.35. The overrides are keyed to power atoms that uniquely identify these seats in this story (powerful = deployers, organized = injured users), avoiding collision with the institutional-power developers and courts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — harms from artifacts whose creator and deployer differ, leaving victims without a clear defendant — is live, attested from outside the benefiting parties by judicial opinions, plaintiff dockets, and independent tort scholarship. Because the problem is live, the arrangement is not a piton: its accountability function still performs. Because the burden is genuinely asymmetric — deployers collect the risk shield while developers fund it — the arrangement is not a pure rope either. The tangled_rope claim keeps both facts in view: decomposing the story into 'pure coordination' would launder the transfer to deployers; decomposing it into 'pure extraction' would erase the real defendant-identification and upstream-incentive functions that victims and courts rely on. The measurement series guards the failure mode specific to this arrangement: if extractiveness keeps climbing while the control-share omega resolves toward deployment dominance, the coordination half atrophies and the structure drifts toward a snare profile with developers as captive payers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation_swap,
    'This story instantiates the developer_liability reading of the liability_attribution kernel; would the deployer_liability or shared_liability readings relocate the victim and beneficiary sets so that the measured burden lands on entirely different seats?',
    'Comparative institutional analysis across jurisdictions and contract regimes that allocate primary responsibility differently, tracing where liability costs, defense burdens, and disclosure obligations actually settle.',
    'Under a deployer-primary reading the victim set moves to deployers and developers become shielded beneficiaries; under shared liability the burden disperses along the value chain and no single seat captures it. The classification of THIS story is contingent on the reading, not on the underlying technology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation_swap, conceptual, 'Reading-contingency of the victim/beneficiary structure within the liability_attribution kernel.').

omega_variable(
    creation_vs_deployment_control_share,
    'What share of realized AI harm is determined by choices available at capability creation versus choices made at deployment?',
    'Systematic causal review of harm incidents, coding each case for the controllability of the harm-relevant factors at the creation stage versus the deployment stage.',
    'If deployment-stage choices dominate, developer-primary liability draws heavily from a seat with diminishing marginal prevention leverage and the arrangement drifts toward pure cost transfer; if creation-stage choices dominate, the rule''s upstream incentive alignment is real and a larger share of the developer burden is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creation_vs_deployment_control_share, empirical, 'Empirical distribution of harm causation between creation and deployment control points.').

omega_variable(
    opacity_disclosure_effect,
    'Do developer-facing disclosure and documentation mandates reduce realized harm, or do they primarily redistribute compliance cost while the underlying information asymmetry persists?',
    'Compare harm rates and incident-discovery latency across regimes with differing disclosure requirements, controlling for model capability class and deployment volume.',
    'If disclosure is largely inert, the opacity-burden component of the developer''s load is pure overhead and effective extraction rises; if disclosure measurably reduces harm, part of the burden is functional and the coordination account strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_disclosure_effect, empirical, 'Whether the opacity burden placed on developers buys harm reduction or is deadweight compliance.').

omega_variable(
    insurance_moral_hazard_neutralization,
    'Does liability insurance decouple deployer behavior from the allocated risk, converting the rule''s incentive function into a pure premium transfer?',
    'Deployer safety-practice data stratified by insured versus uninsured exposure, plus premium experience curves across the underwriting cycle.',
    'If moral hazard dominates, the rule''s coordination value shrinks toward zero and its transfer character dominates the computed classification; if insurability preserves deployer-side care, the coordination function stands and the hybrid reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_moral_hazard_neutralization, empirical, 'Whether insurance intermediation preserves or dissolves the rule''s incentive effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.16).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__developer_liability, theater_ratio, 6, 0.19).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__developer_liability, theater_ratio, 12, 0.22).
narrative_ontology:measurement(liab_tr_t18, liability_attribution__developer_liability, theater_ratio, 18, 0.25).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__developer_liability, theater_ratio, 24, 0.28).
narrative_ontology:measurement(liab_tr_t30, liability_attribution__developer_liability, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(liab_be_t6, liability_attribution__developer_liability, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(liab_be_t12, liability_attribution__developer_liability, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(liab_be_t18, liability_attribution__developer_liability, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(liab_be_t24, liability_attribution__developer_liability, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(liab_be_t30, liability_attribution__developer_liability, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(liab_su_t6, liability_attribution__developer_liability, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(liab_su_t12, liability_attribution__developer_liability, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(liab_su_t18, liability_attribution__developer_liability, suppression_requirement, 18, 0.51).
narrative_ontology:measurement(liab_su_t24, liability_attribution__developer_liability, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(liab_su_t30, liability_attribution__developer_liability, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI liability' conflates three structurally distinct arrangements that share one kernel: developer-primary, deployer-primary, and shared/causal-contribution attribution. Per the epsilon-invariance principle these are separate constraints, not one constraint viewed from angles — swapping the primary seat swaps the victim set, the beneficiary set, and therefore epsilon. This story authors the developer_liability member. The upstream/downstream structure runs from whichever reading a jurisdiction adopts toward the others: accumulated strain under developer-primary attribution (unworkability against distributed open-source contributors, opacity-dispute growth) exerts structural pressure toward shared-liability schemes without logically eliminating them, hence the influences edge to shared_liability and the coexists_with edge to deployer_liability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__developer_liability, powerful, 0.12).
constraint_indexing:directionality_override(liability_attribution__developer_liability, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
