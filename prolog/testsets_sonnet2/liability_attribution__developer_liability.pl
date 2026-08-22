% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer-Primary Liability Attribution Reading
 *   domain: technology governance / legal theory / regulatory design
 *
 * SUMMARY:
 *   This story instantiates the developer-primary reading of the
 *   liability_attribution kernel: when an AI capability causes harm, legal
 *   and regulatory exposure attaches primarily to the party that built the
 *   underlying model, rather than to the party that deployed, configured, or
 *   fine-tuned it for a specific use. The reading has a genuine coordination
 *   rationale — a single traceable liability node reduces litigation cost and
 *   gives injured parties a stable target — but it also structurally
 *   externalizes the risk of deployment-context decisions onto developers who
 *   cannot observe or control those decisions, particularly for open weights
 *   and API-distributed capability. This is authored as ONE of three sibling
 *   readings of the same kernel (deployer_liability, shared_liability); the
 *   other readings are separate constraint stories with their own ε and
 *   stakeholder structures, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - model_developers: primary target (moderate/constrained) — bears legal exposure for capability it built but cannot observe deployed
 *   - open_source_maintainers: most exposed target (powerless/trapped) — no infrastructure to absorb or price liability risk
 *   - small_ai_labs: secondary target (moderate/constrained) — structurally disadvantaged relative to large developers who can self-insure
 *   - deploying_enterprises: primary beneficiary (powerful/arbitrage) — externalizes context-specific risk upstream, shops among developers by liability posture
 *   - downstream_integrators and end_user_platforms: secondary beneficiaries (organized/mobile) — thinner compliance burden despite proximity to harm
 *   - harmed_end_users: excluded party (powerless/trapped) — redress is pointed at the least contextually informed party
 *   - regulators_and_courts: agenda setter (institutional/analytical) — fixes the attribution rule based on an information-asymmetry theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.61).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer-Primary Liability Attribution Reading").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology governance / legal theory / regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '6186b20e-20e7-4b0b-b09d-9b3e983f2ad6').
narrative_ontology:cs_kernel_codification('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', distributed).
narrative_ontology:cs_authority_grounding('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', distributed).
narrative_ontology:cs_reading_relation('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', foundational, capability_creator_bears_primary_responsibility).
narrative_ontology:cs_axiom_status(capability_creator_bears_primary_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', capability_creator_bears_primary_responsibility, instrumental).
narrative_ontology:cs_axiom('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', secondary, informational_asymmetry_justifies_upstream_liability).
narrative_ontology:cs_axiom_status(informational_asymmetry_justifies_upstream_liability, holdable).
narrative_ontology:cs_axiom_grounding('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', informational_asymmetry_justifies_upstream_liability, empirically_contingent).
narrative_ontology:cs_reference_frame('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', single_actor_product_liability_model).
narrative_ontology:cs_drift_state('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', multi_party_deployment_chain_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6186b20e-20e7-4b0b-b09d-9b3e983f2ad6', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deploying_enterprises).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, downstream_integrators).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_user_platforms).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, model_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_maintainers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, small_ai_labs).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, creator_responsibility_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, capability_traceability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds and releases the underlying model or capability. Under this reading, bears primary tort and regulatory exposure for downstream harms because they created the capability, regardless of how a deployer configured, fine-tuned, or repurposed it. Carries the burden of proving the harm arose from deployment choices rather than the base model, which is difficult given the opacity of large models even to their own creators. Exit from this liability exposure requires either not releasing the capability at all or absorbing large compliance/insurance costs — neither is a real option for continuing to operate in the field.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, model_developers, payer,
    moderate, biographical, constrained, global).

% Releases weights, code, or architectures without commercial deployment infrastructure or revenue to fund legal defense. Under developer-primary attribution, this group is squarely inside the liability frame despite having no visibility into, or control over, how third parties deploy the released capability. Cannot practically insure against this or negotiate liability caps the way a well-resourced lab can; withdrawal from open release is the only real exit, which forecloses the coordination benefit of open publication.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_maintainers, payer,
    powerless, biographical, trapped, global).

% Competes with larger labs on capability but lacks the legal and insurance infrastructure to absorb primary liability exposure. Faces a structural disadvantage: large developers can price liability into enterprise contracts and self-insure, but smaller labs face existential risk from a single adverse judgment. Exit means exiting the market or licensing capability through a larger developer's liability shield, which concentrates the field.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, small_ai_labs, payer,
    moderate, biographical, constrained, national).

% Integrates third-party models into products and services, choosing configuration, prompting, fine-tuning, and use context. Under developer-primary attribution, this group externalizes the bulk of harm-based legal risk to the model's creator even though it controls the actual point of deployment and the context in which harm materializes. Can select among competing developer offerings partly on the basis of which developer accepts more liability exposure, giving this group negotiating leverage and effective arbitrage across the developer market.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deploying_enterprises, beneficiary,
    powerful, biographical, arbitrage, global).

% Builds intermediate products and APIs on top of base models, often reselling or repackaging capability with its own added logic. Benefits from a liability regime that keeps primary exposure upstream at the model developer, letting this group operate with a thinner compliance and insurance burden than it would otherwise carry.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, downstream_integrators, beneficiary,
    organized, biographical, mobile, national).

% Operates the consumer-facing surface where harm is typically first observed (a chatbot interface, a generated-content feed). Under developer-primary liability, this group's exposure is comparatively limited even though it is closest to the actual harm event and controls user-facing guardrails, moderation, and disclosure.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, end_user_platforms, beneficiary,
    organized, biographical, mobile, global).

% Experiences the actual harm — discriminatory output, defamation, physical or financial injury from an AI-mediated decision — but has no seat in setting the liability allocation rule. Their practical redress depends entirely on which party the attribution rule points them toward; under developer-primary attribution they must litigate against an upstream party they never interacted with and whose internal workings they cannot inspect, rather than the deployer whose specific configuration and use context actually produced the harm.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, harmed_end_users, excluded,
    powerless, immediate, trapped, national).

% Writes and interprets the statutes, agency rules, and case law that fix liability at the developer node of the value chain. Justifies this allocation on the theory that the developer is the party with unique knowledge of and control over the underlying capability's design, training data, and known failure modes, and is therefore best positioned to prevent harm ex ante and best able to internalize its cost through pricing or insurance.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, deploying_enterprises).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixing liability at a single, identifiable node (the developer who created the capability) gives injured parties, insurers, and courts a stable, traceable target for redress instead of litigating causal apportionment across an entire multi-party value chain for every incident.
% TRANSFER_FUNCTION: Moves legal, financial, and reputational risk from the parties who configure and deploy a capability in a specific context to the party that built the general-purpose capability, regardless of whether the deployment context was foreseeable or controllable by the developer.
% ABSENT_VOICES: Harmed end users are not party to the liability-allocation debate at all; their interest is only in effective, timely redress, which this reading may actually worsen by pointing them at an upstream developer with the least contextual knowledge of how the harm occurred. Open-source maintainers and small labs are also structurally underrepresented relative to the large developers and enterprise deployers who dominate the policy conversation.
% DISAPPEARANCE_RATIONALE: If developer-primary attribution were abandoned overnight, liability exposure would immediately migrate toward deployers and integrators who actually control deployment context; developer pricing, licensing terms, insurance markets, and the willingness of open-source maintainers to release capability at all would all shift substantially. Deploying enterprises currently arbitraging across developer liability postures would lose that leverage.
% FOUNDING_PROBLEM: As capable, general-purpose AI models proliferated through many downstream deployments, courts and regulators needed a workable liability doctrine that did not require reconstructing the entire deployment chain for every harm, and reached for the party with the deepest knowledge of the underlying system's design and training.
% FOUNDING_PROBLEM_CORROBORATION: Deploying enterprises and downstream integrators corroborate that developer-primary attribution reflects real informational asymmetry about model internals. Independent legal scholars and consumer-harm litigators, outside the beneficiary set, corroborate that the doctrine is increasingly used to shield the party with actual control over the harm-producing context — the deployer — while developers with no visibility into specific deployment decisions absorb liability they cannot practically manage.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.68 and rising over the interval because the doctrine increasingly functions to shield the party with actual deployment control (the deployer) while routing legal and financial exposure to a developer node that, especially for open or widely-licensed models, has no visibility into the specific context that produced harm. Suppression is authored at 0.61 — moderate-high, because developers have limited practical ability to contract around this exposure (indemnification terms are frequently unenforceable against tort claims, and open-source release forecloses contractual liability-shifting entirely) and because regulatory and case-law momentum is actively reinforcing the developer-primary rule rather than leaving it as a default that can be negotiated away. Theater ratio is moderate-low (0.30): the traceability rationale is not pure cover — a single identifiable liability node does reduce litigation cost — but a growing share of the doctrine's actual function is shielding deployers rather than efficiently allocating risk to the least-cost avoider.
 *
 * PERSPECTIVAL GAP:
 *   From the regulators-and-courts seat, this reading looks like efficient coordination: fix liability where the technical knowledge and design control genuinely sit. From the model-developer and open-source-maintainer seats, the same rule computes as extraction: they bear cost for harms produced by configuration and deployment decisions made entirely outside their control or knowledge, with no correspondingly larger claim on the value the deployment generates. The engine should compute these seats as structurally divergent — agenda_setter/beneficiary seats see coordination, payer seats see enforced cost-shifting.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and open-source maintainers are declared victims: they bear the transfer (legal/regulatory exposure) without commensurate control over the harm-producing event, and their exit options range from constrained (established labs, which can partially price risk into licensing) to trapped (individual and small open-source maintainers, who cannot price or insure against the exposure at all and can only withdraw from release). Deploying enterprises and integrators are declared beneficiaries: they capture the coordination benefit (predictable, developer-borne liability) without bearing proportionate cost, and their arbitrage/mobile exit options let them select developers partly on liability-shielding terms, reinforcing the extraction. This directly implements the kernel's expected structural delta for this reading: developers in the victim set, deployers as beneficiaries of externalized risk, opacity treated as the developer's burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — courts needing a workable, traceable liability target rather than reconstructing an entire deployment chain per incident — was real and may still be partially live for genuinely single-actor products. But as capability increasingly moves through multi-party deployment chains (open weights, API resale, fine-tuning marketplaces), the coordination justification for concentrating liability at the developer node has weakened relative to its extractive effect, which is why founding_problem_status is authored as contested rather than dead or live outright: it is not that the problem vanished, but that the rule's current beneficiary structure (enterprise deployers with arbitrage power) diverges from the rule's original efficiency rationale. Treating this as tangled_rope rather than snare preserves the fact that a genuine coordination function (a stable, traceable liability target reduces transaction cost for injured parties) still exists alongside the asymmetric extraction — collapsing it to pure extraction would mislabel the traceability benefit as pure cover, while calling it a clean rope would ignore the externalization onto powerless developers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developer_liability_vs_deployer_liability_kernel_choice,
    'Is fixing liability at the developer node (this reading) the structurally correct allocation, or does actual causal and informational control over harm-producing deployment decisions sit with the deployer (the sibling deployer_liability reading), making this reading a misallocation dressed as traceability efficiency?',
    'Case-level analysis of a representative sample of AI-harm litigation: for each case, determine whether the harm-producing decision (data selection, fine-tuning, prompting, guardrail configuration, deployment context) was made by the developer or the deployer, and whether the party actually held liable had knowledge of or control over that decision.',
    'If deployment-context decisions are shown to be the dominant causal factor in most harms, this reading systematically misallocates liability away from the least-cost avoider and toward a party that cannot observe or prevent the harm, strengthening the case that developer-primary attribution functions as extraction rather than efficient coordination. If developer-side design defects dominate, the reading''s coordination rationale is stronger than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developer_liability_vs_deployer_liability_kernel_choice, empirical, 'Whether developer-primary attribution tracks actual causal control or externalizes deployer-controlled risk.').

omega_variable(
    opacity_burden_allocation,
    'Should the burden of managing and disclosing model opacity (the difficulty of predicting or explaining a model''s behavior in novel deployment contexts) fall on the developer who built the opaque system, or is opacity itself a joint product of the developer''s design choices and the deployer''s context choices, making unilateral burden assignment to either party a category error?',
    'Technical audit methodology assessing what fraction of unpredictable-behavior incidents trace to base model properties (interpretable by the developer in principle) versus emergent properties of specific fine-tuning, prompting, or deployment configurations (interpretable only by the deployer).',
    'If opacity is substantially deployment-context-dependent, treating it as the developer''s exclusive burden (as this reading does) systematically overstates developer culpability; if opacity is substantially intrinsic to the base model, the reading''s allocation is closer to structurally sound.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opacity_burden_allocation, conceptual, 'Whether model opacity is a developer-attributable property or a joint developer-deployer product.').

omega_variable(
    open_source_chilling_effect,
    'Does developer-primary liability attribution, applied without a safe-harbor for capability released without commercial control over downstream use, structurally suppress open-weight release in a way that reduces overall system transparency and auditability?',
    'Track open-weight release rates and licensing-restriction terms in jurisdictions that have adopted developer-primary liability doctrines versus jurisdictions that have adopted deployer-primary or shared-liability regimes, controlling for capability class and time period.',
    'If open release measurably declines under this regime, the doctrine''s extractive effect extends beyond direct liability cost to a second-order suppression of the transparency and audit ecosystem the coordination rationale is nominally meant to support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_chilling_effect, empirical, 'Whether developer-primary liability chills open-weight release as a side effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.18).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__developer_liability, theater_ratio, 4, 0.2).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__developer_liability, theater_ratio, 8, 0.22).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__developer_liability, theater_ratio, 12, 0.25).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__developer_liability, theater_ratio, 16, 0.27).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__developer_liability, theater_ratio, 20, 0.29).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__developer_liability, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(liab_be_t4, liability_attribution__developer_liability, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(liab_be_t8, liability_attribution__developer_liability, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(liab_be_t12, liability_attribution__developer_liability, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(liab_be_t16, liability_attribution__developer_liability, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(liab_be_t20, liability_attribution__developer_liability, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(liab_be_t24, liability_attribution__developer_liability, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(liab_su_t4, liability_attribution__developer_liability, suppression_requirement, 4, 0.49).
narrative_ontology:measurement(liab_su_t8, liability_attribution__developer_liability, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(liab_su_t12, liability_attribution__developer_liability, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(liab_su_t16, liability_attribution__developer_liability, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(liab_su_t20, liability_attribution__developer_liability, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(liab_su_t24, liability_attribution__developer_liability, suppression_requirement, 24, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the liability_attribution kernel. deployer_liability places primary exposure at the party controlling deployment context, inverting this reading's beneficiary/victim structure. shared_liability distributes exposure proportionally to causal contribution across the value chain, which would reduce this reading's authored extractiveness for developers and its authored beneficiary concentration for deployers. Each reading is authored with its own ε, beneficiaries, and victims per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
