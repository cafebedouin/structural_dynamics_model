% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Deployer-Primary Liability Attribution Rule (AI Harm Governance)
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This story instantiates the deployer-liability reading of the contested
 *   liability_attribution kernel: when an AI system causes harm, the party
 *   held primarily liable is the one that controlled deployment context — the
 *   entity that chose the use case, configured the system, and put it into
 *   operation — rather than the entity that built the underlying model. This
 *   reading treats deployment context control and decision authority as the
 *   operative proxy for causal responsibility. Under this reading, foundation
 *   model providers and licensing intermediaries are structurally shielded
 *   (they retain proprietary knowledge of failure modes but externalize the
 *   consequences), while downstream deploying firms, small businesses, and
 *   ultimately end users absorb legal and practical exposure they often lack
 *   the visibility to manage. This is a distinct constraint from the
 *   developer_liability and shared_liability readings of the same kernel —
 *   each of those readings identifies a different party as primary bearer and
 *   produces a different victim set; they are not alternative measurements of
 *   this constraint, they are different constraints answering the same
 *   underlying dispute differently.
 *
 * KEY AGENTS:
 *   - foundation_model_providers: primary beneficiary (institutional/arbitrage) — retains internal risk knowledge, shielded from downstream suits
 *   - model_licensing_intermediaries: beneficiary and partial agenda-setter (organized/arbitrage) — drafts terms pushing liability downstream
 *   - downstream_deploying_firms: primary target (moderate/constrained) — bears due-diligence burden for opacity not of their making
 *   - small_business_deployers: secondary target (powerless/trapped) — least equipped to absorb the allocated risk
 *   - end_users_harmed_by_deployed_systems: ultimate victim (powerless/trapped) — redress routed to an under-resourced deployer
 *   - regulators_and_courts: agenda-setter (institutional/analytical) — adopts the administrable but not necessarily causally accurate test
 *   - ai_safety_researchers: analytical observer (organized/analytical) — assesses fit between control and actual causal responsibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.62).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.55).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.62).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer-Primary Liability Attribution Rule (AI Harm Governance)").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, 'c993eaf4-0733-4f8e-9389-1792d96e1213').
narrative_ontology:cs_kernel_codification('c993eaf4-0733-4f8e-9389-1792d96e1213', distributed).
narrative_ontology:cs_authority_grounding('c993eaf4-0733-4f8e-9389-1792d96e1213', distributed).
narrative_ontology:cs_reading_relation('c993eaf4-0733-4f8e-9389-1792d96e1213', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('c993eaf4-0733-4f8e-9389-1792d96e1213', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('c993eaf4-0733-4f8e-9389-1792d96e1213', foundational, control_at_point_of_harm_grounds_liability).
narrative_ontology:cs_axiom_status(control_at_point_of_harm_grounds_liability, holdable).
narrative_ontology:cs_axiom_grounding('c993eaf4-0733-4f8e-9389-1792d96e1213', control_at_point_of_harm_grounds_liability, conventional).
narrative_ontology:cs_axiom('c993eaf4-0733-4f8e-9389-1792d96e1213', secondary, opacity_is_deployer_diligence_failure_not_provider_disclosure_failure).
narrative_ontology:cs_axiom_status(opacity_is_deployer_diligence_failure_not_provider_disclosure_failure, holdable).
narrative_ontology:cs_axiom_grounding('c993eaf4-0733-4f8e-9389-1792d96e1213', opacity_is_deployer_diligence_failure_not_provider_disclosure_failure, instrumental).
narrative_ontology:cs_reference_frame('c993eaf4-0733-4f8e-9389-1792d96e1213', tort_law_proximate_cause_tradition).
narrative_ontology:cs_drift_state('c993eaf4-0733-4f8e-9389-1792d96e1213', contemporary_foundation_model_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c993eaf4-0733-4f8e-9389-1792d96e1213', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, model_licensing_intermediaries).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, downstream_deploying_firms).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, small_business_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, end_users_harmed_by_deployed_systems).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, control_based_liability_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, proximate_cause_traceability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains and licenses the underlying model, retains internal knowledge of training data, architecture, and known failure modes, but is contractually and legally shielded once the model is licensed downstream because liability attaches to the party controlling deployment context. Sets license terms that reinforce this shield and litigates aggressively to preserve it.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Packages and resells access to foundation models via APIs and enterprise agreements, drafts terms of service that push liability downstream to deployers, and lobbies regulators to codify deployment-context control as the liability trigger. Collects licensing revenue without bearing incident costs.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, model_licensing_intermediaries, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, model_licensing_intermediaries, agenda_setter).

% Integrates licensed models into products or internal workflows, configures prompts, guardrails, and use cases, and is legally the first (often only) party investigators and courts pursue when the system causes harm. Cannot fully audit the model's internals to know its failure modes, yet bears the due-diligence burden for opacity they did not create.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, downstream_deploying_firms, payer,
    moderate, biographical, constrained, national).

% Lacks in-house legal or technical capacity to evaluate model risk before deployment; adopts tools via standard commercial licenses with no negotiating leverage. When a deployed system causes harm, faces liability exposure disproportionate to its resources and cannot pass costs upstream because the license disclaims provider responsibility.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, small_business_deployers, payer,
    powerless, immediate, trapped, local).

% Experiences direct harm (denied services, discriminatory outcomes, physical or financial injury) from a deployed AI system and must identify a liable party to seek redress; the rule directs them toward the deployer, who often has fewer resources than the model's original creator and may lack the technical means to fully explain what went wrong.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, end_users_harmed_by_deployed_systems, payer,
    powerless, immediate, trapped, local).

% Adjudicates liability claims and drafts rules; adopts deployment-context control as the operative test because it is administrable (courts can observe who configured and operated the system) even where causal responsibility for the underlying defect lies further upstream.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Studies where harms actually originate in the AI value chain (training data, architecture, fine-tuning, deployment configuration) and can testify to whether deployment control is a good proxy for causal responsibility, without being a party to any specific liability dispute.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_safety_researchers, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives victims, regulators, and courts a single administrable point of contact for redress: the deployer is observable, physically present in the jurisdiction, and identifiable as the entity that chose to put the system into a specific use context, which solves the real problem of otherwise being unable to locate any accountable party in a diffuse multi-party AI supply chain.
% TRANSFER_FUNCTION: Moves the legal and financial exposure for AI-caused harm from foundation model providers and licensing intermediaries (who built and monetized the underlying capability) to the firms and individuals who deployed it in a specific context, and ultimately to end users who bear residual harm when deployer resources or diligence are insufficient.
% ABSENT_VOICES: Foundation model providers' internal safety and red-teaming teams possess model-specific failure-mode knowledge that would materially affect liability allocation but is treated as proprietary and is not compelled into deployer risk assessments; harmed end users rarely have standing or resources to contest the liability rule itself, only individual claims under it.
% DISAPPEARANCE_RATIONALE: If deployer-primary liability were abolished overnight, foundation model providers would face direct suits for downstream harms, licensing terms would shift dramatically (providers would demand deployment audits or refuse indemnification disclaimers), insurance markets for AI deployment would reprice, and many small deployers currently absorbing liability risk would either exit the market or demand contractual protection upstream — the allocation of AI harm costs across the value chain would materially change.
% FOUNDING_PROBLEM: Early AI liability disputes stalled because courts could not identify a single responsible party across a value chain spanning data curation, model training, fine-tuning, and deployment; some accountable, locatable party was needed to make redress possible at all.
% FOUNDING_PROBLEM_CORROBORATION: Foundation model providers and licensing intermediaries attest the rule is functioning as intended — control-based liability is the correct legal proxy for responsibility. Independent AI safety researchers and several consumer-protection litigators (outside the beneficiary set) attest that the rule increasingly functions to externalize known, provider-side failure modes onto parties with no visibility into them, and that the 'deployment control' framing has drifted from its original access-to-redress purpose into a cost-shifting mechanism.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.62 at interval end) reflects that deployers bear costs structurally disproportionate to their causal contribution — they configure and operate the system but frequently cannot audit training data, architecture choices, or known failure modes that originate upstream. Suppression (0.55) is moderate: deployers are not literally barred from seeking indemnification, but standard-form licensing terms and information asymmetry make contesting the allocation practically difficult. Theater ratio is comparatively low (0.28) because the coordination function — giving victims a locatable, accountable party — is genuinely served, not merely performed; the extractive component rides on top of a real coordination need rather than substituting for it entirely. Both accessibility_collapse (0.48) and resistance (0.58) sit at mid-range: deployers retain some negotiating and insurance-market alternatives (moderate accessibility, not full collapse), and there is active, organized resistance from deployer trade associations and safety researchers challenging the control-based test, which is inconsistent with a settled natural-law-like arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Foundation model providers and licensing intermediaries are the structural beneficiaries: they retain the specialized knowledge needed to prevent harm but externalize consequence, and their arbitrage-grade exit options (multiple jurisdictions, contractual disclaimers, diversified licensing revenue) push their derived directionality toward the beneficiary end. Downstream deploying firms and small business deployers are targets: constrained-to-trapped exit options, direct legal exposure, and the requirement to perform due diligence on a system whose internals they cannot fully inspect push their directionality toward the target end. End users are the deepest victims — they neither built nor deployed the system, yet the liability rule routes their redress through the deployer, who is often the least-resourced party actually able to pay or explain the harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no locatable accountable party across a diffuse AI value chain — was real and coordination-genuine. But the fix (deployment-context control as the liability trigger) has drifted from solving 'who can be sued' toward 'who absorbs costs that could have been allocated upstream by parties with better information.' Classifying this as tangled_rope rather than snare preserves the genuine coordination function (redress must be locatable) while flagging the asymmetric extraction (foundation model providers retain safety knowledge without matching liability exposure) that active enforcement (court adoption of the control test, contractual liability-shifting clauses) sustains. A pure snare framing would miss that courts and victims do benefit from having someone to sue; a pure rope framing would ignore that the specific allocation is neither efficient nor causally accurate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deployment_control_kernel_reading_choice,
    'Is deployment-context control the structurally correct proxy for causal responsibility in AI harm, or is it an administrable-but-inaccurate proxy that happens to shield the party with the most specialized risk knowledge?',
    'Comparative case analysis across jurisdictions applying different liability_attribution readings (deployer_liability, developer_liability, shared_liability) tracking whether harm rates, settlement patterns, and upstream safety investment differ measurably by regime.',
    'If deployment control tracks causal responsibility poorly relative to training-time decisions, this reading systematically under-attributes liability to the party best positioned to prevent harm, which would support migrating toward the shared_liability reading. If it tracks well, the deployer_liability reading''s administrability is not purchased at a causal-accuracy cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployment_control_kernel_reading_choice, empirical, 'Whether deployment-context control is a causally accurate liability proxy or a shielding mechanism for developers').

omega_variable(
    opacity_burden_reasonableness,
    'Can a deployer''s due-diligence burden regarding model opacity be discharged given that foundation model providers control what technical information is disclosed?',
    'Track whether disclosure mandates (model cards, evaluation reports, known-limitations documentation) actually reach deployers in a form sufficient to satisfy the legal due-diligence standard, versus remaining boilerplate that shifts nominal but not substantive responsibility.',
    'If disclosure is substantively insufficient, the deployer''s opacity burden is unsatisfiable in practice, which strengthens the case that this reading functions as extraction rather than coordination despite its administrable coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_burden_reasonableness, empirical, 'Whether deployers can actually satisfy the due-diligence standard the rule imposes on them').

omega_variable(
    reading_selection_normativity,
    'Is the choice among deployer_liability, developer_liability, and shared_liability readings a factual/causal question resolvable by evidence, or an irreducibly normative policy choice about who should bear innovation risk versus deployment risk?',
    'This is likely partly conceptual/preference — track whether jurisdictions converge on one reading as evidence accumulates (suggesting an empirical component) or remain persistently split along policy-preference lines (suggesting an irreducibly normative core).',
    'If convergence occurs, the kernel dispute is substantially empirical and this reading''s persistence is evidence-tracking. If persistent split continues, all three readings remain simultaneously defensible and none should be treated as the ''true'' resolution of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_normativity, conceptual, 'Whether the kernel dispute among liability readings is empirically resolvable or normatively irreducible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.12).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__deployer_liability, theater_ratio, 4, 0.15).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__deployer_liability, theater_ratio, 8, 0.18).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.21).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__deployer_liability, theater_ratio, 16, 0.24).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__deployer_liability, theater_ratio, 20, 0.26).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__deployer_liability, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(liab_be_t4, liability_attribution__deployer_liability, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(liab_be_t8, liability_attribution__deployer_liability, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(liab_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(liab_be_t16, liability_attribution__deployer_liability, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(liab_be_t20, liability_attribution__deployer_liability, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(liab_be_t24, liability_attribution__deployer_liability, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(liab_su_t4, liability_attribution__deployer_liability, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(liab_su_t8, liability_attribution__deployer_liability, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(liab_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(liab_su_t16, liability_attribution__deployer_liability, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(liab_su_t20, liability_attribution__deployer_liability, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(liab_su_t24, liability_attribution__deployer_liability, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__deployer_liability, 0.12).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'AI liability attribution' per the ε-invariance principle: deployer_liability (this story), developer_liability, and shared_liability each name a different party as primary liability-bearer and produce different victim sets and different ε values. They are linked as a constraint family via affects_constraints rather than merged into one story with a measurement parameter, because which reading a jurisdiction adopts materially changes who is coordinated, who pays, and how much.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__deployer_liability, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
