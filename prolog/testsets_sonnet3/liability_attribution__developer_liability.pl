% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer-Bears-Primary-Liability Reading of AI Liability Attribution
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   As AI harms began generating litigation and regulatory attention, one
 *   contested reading of the liability-attribution kernel places primary
 *   responsibility on the developer of the underlying model or capability,
 *   reasoning that they created the source of risk and are best positioned to
 *   understand and disclose its failure modes. This reading is
 *   administratively convenient — it gives plaintiffs and regulators a
 *   stable, identifiable target — but it structurally shields deploying
 *   enterprises and downstream integrators, who make the specific
 *   configuration and deployment-context decisions that often proximately
 *   shape a given harm, from primary exposure. Small developers and
 *   open-source maintainers, who often have the least capacity to absorb
 *   liability risk and the least visibility into downstream deployment, bear
 *   a disproportionate share of the resulting cost. This story authors ONLY
 *   the developer-liability reading as a clean, ε-invariant constraint; the
 *   sibling readings (deployer_liability, shared_liability) are separate
 *   constraints with their own ε, beneficiary/victim structure, and
 *   classification, linked here via network.affects_constraints and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - model_developers: primary target under this reading (organized/constrained) — bears liability for capability creation regardless of deployment context
 *   - open_source_model_maintainers: most exposed target (powerless/trapped) — cannot fund defense, cannot control downstream use, cannot un-release
 *   - small_ai_startups: secondary target (moderate/constrained) — absorbs liability it cannot price or insure
 *   - deploying_enterprises: primary beneficiary (institutional/arbitrage) — externalizes deployment-context risk upstream
 *   - downstream_integrators: secondary beneficiary (powerful/mobile) — shielded for configuration choices
 *   - end_user_harm_plaintiffs: procedural beneficiary and partly excluded voice (powerless/trapped) — gains a traceable defendant but loses visibility into deployment causation
 *   - regulators_and_courts: agenda-setter (institutional/analytical) — chooses the administrable rule
 *   - insurers_and_underwriters: analytical observer (organized/analytical) — prices around whichever rule prevails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.6).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer-Bears-Primary-Liability Reading of AI Liability Attribution").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, 'e3b0ca01-186d-4822-b23e-0aa9b725b333').
narrative_ontology:cs_kernel_codification('e3b0ca01-186d-4822-b23e-0aa9b725b333', distributed).
narrative_ontology:cs_authority_grounding('e3b0ca01-186d-4822-b23e-0aa9b725b333', distributed).
narrative_ontology:cs_reading_relation('e3b0ca01-186d-4822-b23e-0aa9b725b333', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('e3b0ca01-186d-4822-b23e-0aa9b725b333', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('e3b0ca01-186d-4822-b23e-0aa9b725b333', foundational, capability_creation_is_proximate_cause).
narrative_ontology:cs_axiom_status(capability_creation_is_proximate_cause, holdable).
narrative_ontology:cs_axiom_grounding('e3b0ca01-186d-4822-b23e-0aa9b725b333', capability_creation_is_proximate_cause, conventional).
narrative_ontology:cs_axiom('e3b0ca01-186d-4822-b23e-0aa9b725b333', secondary, developer_epistemic_access_justifies_disclosure_duty).
narrative_ontology:cs_axiom_status(developer_epistemic_access_justifies_disclosure_duty, holdable).
narrative_ontology:cs_axiom_grounding('e3b0ca01-186d-4822-b23e-0aa9b725b333', developer_epistemic_access_justifies_disclosure_duty, instrumental).
narrative_ontology:cs_reference_frame('e3b0ca01-186d-4822-b23e-0aa9b725b333', product_liability_manufacturer_analogy).
narrative_ontology:cs_drift_state('e3b0ca01-186d-4822-b23e-0aa9b725b333', post_generative_ai_deployment_diversification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3b0ca01-186d-4822-b23e-0aa9b725b333', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deploying_enterprises).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, downstream_integrators).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_user_harm_plaintiffs_seeking_deep_pocket_defendant).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, model_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_model_maintainers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, small_ai_startups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_user_harm_plaintiffs).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, capability_creator_bears_foreseeable_risk_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains and releases the underlying model or capability. Under this reading, bears primary tort and regulatory liability for downstream harms on the theory that they created the capability and are best positioned (or the only party technically capable) of understanding and disclosing its failure modes. Cannot fully exit liability exposure by releasing the model with disclaimers, since courts and regulators under this reading treat capability-creation itself as the proximate cause. Faces liability regardless of how the model is subsequently deployed or by whom, unless it can affirmatively prove the deployer's modifications were the sole cause.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, model_developers, payer,
    organized, generational, constrained, global).

% Releases weights or code without commercial deployment infrastructure and often without revenue to fund legal defense or insurance. Under this reading, is treated the same as a commercial developer for liability purposes despite having no visibility into, or control over, how the released capability is later deployed. Exit is effectively impossible once a model is released publicly; withdrawal after release does not undo liability exposure for past distribution.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_model_maintainers, payer,
    powerless, biographical, trapped, global).

% Builds a model or fine-tunes a base model for a niche use case with limited capital and no in-house legal or compliance function. Under this reading, absorbs the same primary-liability exposure as large developers with orders of magnitude less capacity to self-insure, litigate, or negotiate contractual liability shifting. Many exit the field entirely rather than accept underwritten liability they cannot price.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, small_ai_startups, payer,
    moderate, biographical, constrained, national).

% Licenses or integrates a third-party model into a commercial product or internal workflow, retaining full control over deployment context, prompting, fine-tuning, and end-user exposure. Under this reading, is substantially shielded from primary liability because the harm is legally traced back to the model's inherent capability rather than the enterprise's deployment choices. Can select among developers, negotiate indemnification clauses, and switch model providers with comparatively low friction, externalizing risk upstream.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deploying_enterprises, beneficiary,
    institutional, generational, arbitrage, global).

% Builds applications on top of licensed models, choosing configuration and guardrails. Under this reading, benefits from a liability shield analogous to deploying enterprises: because attribution collapses to the developer of the underlying capability, integrators face reduced exposure for the specific ways they configured or repackaged the model for their user base.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, downstream_integrators, beneficiary,
    powerful, biographical, mobile, national).

% Suffers concrete harm from an AI-mediated decision or output and seeks compensation. Benefits procedurally from this reading because it identifies a well-resourced, traceable defendant (the developer) rather than requiring them to unwind an opaque deployment chain. But is also partly excluded from the deeper policy conversation about whether this attribution actually reaches the party that made the harm-causing choice, since deployment-context decisions that shaped the actual harm are not examined once liability is fixed on the developer.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, end_user_harm_plaintiffs, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, end_user_harm_plaintiffs, excluded).

% Sets and enforces the liability attribution rule through statute, agency guidance, and case law, choosing to anchor liability at the point of capability creation because it is administratively simpler than tracing causal contribution through a multi-party deployment chain. Can revise the standard through legislation or precedent but currently maintains it as the default because it produces a stable, identifiable defendant class.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulators_and_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Prices liability coverage for AI developers based on the attribution rule currently in force. Observes and adapts pricing models to the developer-liability standard without a stake in which reading prevails, but their pricing behavior itself becomes evidence in the ongoing contest over which reading the legal system should adopt.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, insurers_and_underwriters, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, deploying_enterprises).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchoring liability at the point of capability creation gives courts, regulators, and plaintiffs a single identifiable, well-resourced, technically-informed defendant instead of requiring proof of exactly which downstream configuration or deployment decision caused a given harm — this solves a genuine evidentiary and enforcement problem in a multi-party AI value chain where deployment context is often opaque to outside observers.
% TRANSFER_FUNCTION: Moves litigation risk, compliance cost, insurance burden, and disclosure obligation from the parties who configure and profit from specific deployments (enterprises, integrators) to the parties who built the underlying capability (developers, especially smaller and open-source ones), while end-user plaintiffs gain easier access to a deep-pocketed or traceable defendant.
% ABSENT_VOICES: Deployers who made the specific configuration, prompting, or guardrail decisions that proximately shaped the harm are structurally absent from the liability analysis once attribution collapses to the developer; their decision authority is real but is not examined because the rule does not require it to be. Open-source maintainers with no deployment visibility are also effectively unheard in setting the standard that binds them.
% DISAPPEARANCE_RATIONALE: If developer-primary liability disappeared overnight, deploying enterprises and integrators would lose their principal liability shield and would need to negotiate new indemnification terms, purchase separate deployment-context insurance, and potentially face direct suits for their configuration choices; smaller open-source developers would see a substantial reduction in existential legal exposure, likely increasing release activity; plaintiffs would need to prove causation against a different, harder-to-identify defendant, likely slowing some claims.
% FOUNDING_PROBLEM: Early AI harm litigation struggled to establish causation when harms arose from a chain of model training, licensing, fine-tuning, and deployment decisions spread across multiple firms; courts and regulators needed an administrable rule to avoid every case collapsing under discovery costs and diffuse-responsibility defenses.
% FOUNDING_PROBLEM_CORROBORATION: Consumer-protection litigators and some regulators attest the evidentiary problem remains live and justifies anchoring on the developer. Deploying enterprises, integrators, and independent legal scholars outside the plaintiff bar attest the rule has drifted from solving the evidentiary problem into externalizing risk onto whichever party is technically easiest to sue, regardless of who actually controlled the harm-causing decision — this dissenting corroboration comes from actors who do not benefit from the current attribution.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) is substantial but not extreme: there is a real evidentiary/coordination function (identifying a traceable defendant in a multi-party chain), which keeps this from being a pure snare, but the persistent asymmetry — deployers who make the proximate harm-shaping decisions are shielded while developers who often cannot see downstream deployment absorb liability — makes this tangled_rope rather than rope. Suppression (0.6) reflects that the rule is actively maintained through case law and regulatory guidance rather than emerging from consensus; developers cannot simply contract their way out of it. Theater ratio (0.3) is moderate: the disclosure and documentation obligations placed on developers do real risk-communication work, but a growing share is defensive paperwork aimed at litigation posture rather than actually informing deployment decisions. Accessibility collapse (0.5) is middling — developers can still negotiate indemnification clauses in some commercial contexts, but open-source releases have little room to maneuver once released. Resistance (0.62) reflects active pushback from developer coalitions and open-source advocacy groups.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/court seat, this looks like rope: a workable administrability solution to an intractable multi-party evidentiary problem. From the model-developer seat, especially the open-source maintainer with no deployment visibility, it looks like a snare: liability attaches regardless of what they controlled or could foresee, and exit is impossible once a model ships. The engine should compute these divergently from the same structural data — the coordination function is real (hence tangled_rope, not snare), but the extraction asymmetry is also real and does not require developer misconduct to activate.
 *
 * DIRECTIONALITY LOGIC:
 *   Model developers, and especially open-source maintainers and small startups, are declared victims: the liability transfer runs from the parties who made the specific deployment-context decisions to the party that created the underlying capability, regardless of whether that party controlled or foresaw the specific harm. Deploying enterprises and downstream integrators are declared beneficiaries: the rule's collapse of attribution onto the developer functions as an externalization of their deployment-context risk. End-user plaintiffs sit in an unusual dual position — procedural beneficiaries (easier defendant identification) but also an excluded voice in the deeper causal analysis, since the rule does not require examining the deployment decisions that may have actually shaped their harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an intractable causation-tracing problem across an opaque multi-party AI value chain — remains partly live (contested status), which prevents this from being flatly declared a dead-mandate extraction machine. But the corroboration split matters: deploying enterprises and integrators, who benefit from the current attribution, have no incentive to press for a rule that would expose their own configuration decisions, while independent legal scholars and developer coalitions outside the beneficiary set attest that the rule has drifted from evidentiary convenience into risk externalization. This is exactly the kind of mismatch (contested founding-problem status against a beneficiary structure that could self-perpetuate the arrangement past its justification) that the classification is designed to surface rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the developer_liability reading diverge from deployer_liability and shared_liability, structurally?',
    'This is a committer-frame fact, not an empirical one: the readings diverge on where causal proximity is deemed to terminate — developer_liability terminates it at capability creation, deployer_liability terminates it at deployment-context configuration, shared_liability refuses to terminate it at a single point and apportions by contribution. No data resolves which termination point is correct; each is a distinct normative commitment about what counts as the proximate cause of an AI-mediated harm.',
    'If a jurisdiction adopts deployer_liability instead, model_developers exit this story''s victim set entirely and become near-beneficiaries (shielded by the deployment-context causation rule); deploying_enterprises would enter the victim set. If shared_liability is adopted, both current readings'' clean beneficiary/victim splits dissolve into apportioned, case-by-case liability shares — a structurally different constraint again.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the structural disagreement between the three sibling readings of the liability_attribution kernel at the choice of causal termination point.').

omega_variable(
    opacity_burden_allocation,
    'Under the developer_liability reading, is the burden of managing or disclosing model opacity properly the developer''s to bear, given that deployment context can radically change how an opaque capability manifests as harm?',
    'Comparative empirical study of harm cases where deployment-context modification (fine-tuning, prompting, guardrail removal) was the dominant causal factor versus cases where the base capability''s inherent behavior was dominant, holding attribution rule constant across jurisdictions where possible.',
    'If deployment-context modification dominates most real-world harm cases, the developer_liability reading''s attribution choice is descriptively poorly matched to actual causation, strengthening the case for deployer_liability or shared_liability; if base-capability behavior dominates, developer_liability''s attribution choice is better matched to causation and the extraction framing weakens accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_burden_allocation, empirical, 'Whether developer-borne opacity burden actually tracks the empirical distribution of harm causation.').

omega_variable(
    small_developer_differential_capacity,
    'Should the same primary-liability standard apply uniformly to well-capitalized frontier labs and to open-source maintainers or small startups with no legal or insurance capacity?',
    'Track litigation and settlement outcomes stratified by developer size/resourcing over the next several years of case law; observe whether courts or legislatures carve out differentiated standards.',
    'Uniform treatment across capacity levels concentrates the extraction disproportionately on the least-resourced developers (open source, small startups), which is the structural basis for their inclusion as the most severely affected victims in this story; differentiated treatment would reduce or eliminate their victim status while leaving frontier developers'' exposure largely intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_developer_differential_capacity, preference, 'Whether liability-capacity asymmetry among developers should itself be a legally recognized distinction.').


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
narrative_ontology:measurement(liab_tr_t8, liability_attribution__developer_liability, theater_ratio, 8, 0.24).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__developer_liability, theater_ratio, 12, 0.26).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__developer_liability, theater_ratio, 16, 0.28).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__developer_liability, theater_ratio, 20, 0.29).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__developer_liability, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(liab_be_t4, liability_attribution__developer_liability, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(liab_be_t8, liability_attribution__developer_liability, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(liab_be_t12, liability_attribution__developer_liability, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(liab_be_t16, liability_attribution__developer_liability, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(liab_be_t20, liability_attribution__developer_liability, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(liab_be_t24, liability_attribution__developer_liability, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(liab_su_t4, liability_attribution__developer_liability, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(liab_su_t8, liability_attribution__developer_liability, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(liab_su_t12, liability_attribution__developer_liability, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(liab_su_t16, liability_attribution__developer_liability, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(liab_su_t20, liability_attribution__developer_liability, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(liab_su_t24, liability_attribution__developer_liability, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'AI liability attribution' per the ε-invariance principle: developer_liability, deployer_liability, and shared_liability are structurally distinct claims about where causal/legal responsibility terminates in the AI value chain, each with its own beneficiary/victim structure and ε. developer_liability places model developers (especially open-source and small-startup developers) in the victim set and deploying enterprises/integrators in the beneficiary set. deployer_liability would invert this structure. shared_liability apportions liability by causal contribution, producing a different structure again (likely closer to a rope or tangled_rope with more diffuse beneficiary/victim sets). All three should be read as siblings contesting the same liability_attribution kernel, not as three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
