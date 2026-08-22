% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer Primary Liability for AI System Harms
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   The developer_liability reading assigns primary legal and financial
 *   responsibility for AI system harms to the entities that create the
 *   underlying model capabilities — model developers, open-source
 *   maintainers, and research institutions. This reading grounds
 *   accountability in the causal primacy of capability creation: the
 *   developer decides what capabilities exist, what guardrails are baked in,
 *   and what information about model behavior is disclosed. As regulatory
 *   frameworks (EU AI Act, US executive orders, state-level proposals) and
 *   tort litigation mature, the practical burden of compliance, insurance,
 *   documentation, and liability exposure falls disproportionately on
 *   developers regardless of deployment context. Deployer organizations
 *   benefit from externalized risk — they control the deployment environment,
 *   user population, and use-case selection but face attenuated liability.
 *   End users gain a clearer redress target. Regulatory authorities gain a
 *   legible enforcement anchor. The constraint operates as a tangled rope: it
 *   coordinates accountability by creating a single liable party per model
 *   lineage (coordination function) while extracting disproportionate
 *   compliance cost and existential risk from developers who lack deployment
 *   control (asymmetric extraction). Active enforcement is required —
 *   liability regimes must be maintained through legislation, regulatory
 *   guidance, and court precedent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.45).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Primary Liability for AI System Harms").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, 'aed0657d-11bb-4450-b0ca-56748d84c2e9').
narrative_ontology:cs_kernel_codification('aed0657d-11bb-4450-b0ca-56748d84c2e9', distributed).
narrative_ontology:cs_authority_grounding('aed0657d-11bb-4450-b0ca-56748d84c2e9', extraction).
narrative_ontology:cs_interpretation_layer_present('aed0657d-11bb-4450-b0ca-56748d84c2e9').
narrative_ontology:cs_reading_relation('aed0657d-11bb-4450-b0ca-56748d84c2e9', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('aed0657d-11bb-4450-b0ca-56748d84c2e9', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('aed0657d-11bb-4450-b0ca-56748d84c2e9', foundational, creator_bears_primary_responsibility).
narrative_ontology:cs_axiom_status(creator_bears_primary_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('aed0657d-11bb-4450-b0ca-56748d84c2e9', creator_bears_primary_responsibility, deontological).
narrative_ontology:cs_axiom('aed0657d-11bb-4450-b0ca-56748d84c2e9', foundational, capability_creation_entails_accountability).
narrative_ontology:cs_axiom_status(capability_creation_entails_accountability, holdable).
narrative_ontology:cs_axiom_grounding('aed0657d-11bb-4450-b0ca-56748d84c2e9', capability_creation_entails_accountability, conventional).
narrative_ontology:cs_reference_frame('aed0657d-11bb-4450-b0ca-56748d84c2e9', primordial_accountability_vacuum).
narrative_ontology:cs_drift_state('aed0657d-11bb-4450-b0ca-56748d84c2e9', post_generative_ai_deployment_wave, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aed0657d-11bb-4450-b0ca-56748d84c2e9', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deployer_organizations).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_users).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, regulatory_authorities).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, model_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_maintainers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, research_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, open_source_maintainers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, end_users).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, creator_responsibility_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, proportional_liability_principle).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, capability_creation_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create frontier AI models and bear primary liability for downstream harms. They invest in safety research, red-teaming, documentation, and compliance infrastructure. Their exit options: stop developing (forfeit competitive position), restrict release (reduce adoption and feedback), or accept liability exposure. Insurance markets for model liability are nascent and expensive. Open-sourcing increases exposure exponentially — they cannot control downstream use but remain the liable creator.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, model_developers, payer,
    powerful, biographical, constrained, global).

% Develop and release models openly, driven by scientific norms and community mission. They benefit from collaborative improvement and distribution but face unbounded liability for uses they never authorized and cannot monitor. Their identity as open-source contributors makes exit (stopping release) professionally and ideologically costly. Some adopt restrictive licenses (RAIL, custom) to limit liability, fragmenting the commons.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_maintainers, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, open_source_maintainers, beneficiary).

% University and nonprofit labs publishing model architectures and weights. They face liability exposure without commercial revenue to fund compliance. Grant funding does not cover liability insurance. Some shift to closed research or industry partnerships, reducing public knowledge diffusion.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, research_institutions, payer,
    moderate, biographical, constrained, national).

% Integrate models into products and services, control deployment context, user population, and use-case selection. They capture the economic value of AI deployment while the developer_liability reading assigns tail-risk liability to model creators. They can switch model providers, fine-tune independently, or move jurisdictions to optimize liability exposure. Their primary cost is integration effort, not fundamental liability.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deployer_organizations, beneficiary,
    institutional, biographical, arbitrage, global).

% Gain a clear, solvent defendant (the model developer) when harms occur. But they indirectly pay through higher service costs, reduced model diversity, and slower innovation as developers internalize liability. Their exit is constrained — they cannot easily avoid AI-integrated services.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, end_users, beneficiary,
    organized, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, end_users, payer).

% Design and enforce liability regimes (EU AI Act, US AI Executive Order, state laws). They benefit from a legible enforcement target (the developer) but face pressure to evolve toward shared liability as deployment harms proliferate. They do not bear the compliance costs they impose.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Develop AI liability insurance products. They price developer risk based on model capability, release strategy, and deployment visibility. Their capacity to underwrite shapes developer exit options — unaffordable insurance is de facto suppression.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, insurance_markets, observer,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, deployer_organizations).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single, legible, solvent accountable party per model lineage for harms caused by AI systems — solving the 'who do we sue/regulate' problem when deployment chains are long and opaque.
% TRANSFER_FUNCTION: Moves compliance costs, insurance premiums, litigation risk, and documentation burden from deployers (who control context) to developers (who created capabilities). Also moves some safety investment from deployers to developers (developers must anticipate deployment risks).
% ABSENT_VOICES: Affected communities in deployment contexts (especially Global South users, marginalized populations) who experience harms but have no voice in liability regime design. Small deployers and startups who would benefit from shared_liability but lack lobbying capacity. Future developers deterred by liability exposure.
% DISAPPEARANCE_RATIONALE: If developer_liability vanished overnight, deployers would face direct liability exposure, insurance markets would reprice dramatically, model release strategies would shift (more open release, less gating), and regulatory focus would shift to deployment-side obligations. The AI development-deployment value chain would reorganize around deployer accountability.
% FOUNDING_PROBLEM: Early AI deployment (2018-2022) created an accountability vacuum: models caused harms (bias, toxicity, security failures) but no single party was clearly liable. Deployers blamed model opacity; developers blamed deployment misuse. Regulators needed a legible enforcement target.
% FOUNDING_PROBLEM_CORROBORATION: Developer coalitions (Partnership on AI, MLCommons) attest the accountability vacuum persists but argue developer_liability overshoots. Deployer associations and insurance industry attest the vacuum is substantially filled and the regime now extracts rent. Regulatory bodies (EU Commission, NIST) attest the problem is live but evolving toward shared liability. Academic legal scholars (outside benefiting parties) are split: product liability traditionalists support developer_liability; innovation policy scholars favor shared_liability.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the substantive shift of financial and operational burden to developers: compliance costs for documentation, testing, monitoring, and insurance scale with model capability, not deployment revenue. Open-source developers face asymmetric exposure — they create capabilities they cannot control once released. Suppression (0.45) is moderate: alternatives (deployer liability, shared liability, no-fault funds) exist in policy discourse but are structurally disadvantaged by the developer_liability reading's enforcement momentum. Theater ratio (0.32) captures the growing gap between 'responsible AI' rhetoric and the actual liability allocation — compliance artifacts (model cards, risk assessments) increasingly serve liability defense rather than harm reduction. Accessibility collapse (0.62) reflects how developer_liability becomes the default framing in regulatory text, making alternative liability architectures harder to instantiate. Resistance (0.58) is significant: developer coalitions, open-source foundations, and academic institutions actively contest the reading through litigation, lobbying, and technical countermeasures (licensing restrictions, capability gating).
 *
 * PERSPECTIVAL GAP:
 *   The developer seat experiences this as a snare — extraction without control. The deployer seat experiences it as a rope — coordination benefit without the cost. The regulator seat sees a scaffold — a transitional liability architecture pending mature shared-liability regimes. The engine computes these divergences from the structural data: same constraint, different seats, different effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Model developers are primary targets (d near 1.0): they bear the compliance cost, insurance burden, and litigation risk for harms they cannot fully control post-release. Their exit is constrained — stopping development forfeits the field; open-sourcing increases exposure. Deployer organizations are primary beneficiaries (d near 0.0): they capture deployment value while externalizing tail risk. Their exit is arbitrage-grade — they can switch models, providers, or jurisdictions. End users are secondary beneficiaries (d ~ 0.2): they gain a solvent defendant but face higher costs and reduced innovation. Regulatory authorities are agenda-setters with analytical position: they structure the liability regime but do not bear its costs. Open-source maintainers and research institutions are trapped victims (identity_locked exit): their professional identity and mission commit them to capability creation, but the liability regime makes that creation existentially risky.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accountability for AI harms) remains live and intensifying. Developer_liability was a pragmatic first move — a legible target when the field was nascent. But the coordination function (single accountable party per model) now conflicts with the extraction function (developers bear costs for deployment decisions they don't make). The constraint persists because no coalition has formed to rewrite it: deployers benefit from the status quo, regulators prefer enforcement simplicity, and developers are too fragmented to force a redesign. This is not mandatrophy — the problem hasn't disappeared — but it is a tangled rope hardening toward snare as capability-deployment decoupling increases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creator_vs_controller_causation,
    'Does causal primacy in capability creation justify primary liability when deployment context determines harm realization?',
    'Empirical analysis of harm causation chains: what fraction of deployed-model harms are attributable to developer choices (architecture, training data, guardrails) vs. deployer choices (fine-tuning, prompting, integration, user population, safeguards)? Legal doctrine evolution on proximate cause in product liability.',
    'If deployment context dominates harm causation, developer_liability over-assigns liability (snare dynamics). If capability creation dominates, the reading''s coordination function is genuinely aligned with causation (rope/tangled_rope). A mixed finding sustains tangled_rope with shifting extraction balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_vs_controller_causation, empirical, 'Whether the developer_liability reading''s causal premise matches empirical harm attribution.').

omega_variable(
    open_source_existential_exposure,
    'Does the developer_liability reading create existential risk for open-source AI development that exceeds the constraint''s coordination benefit?',
    'Track open-source model release rates, contributor diversity, and capability frontier participation under different liability regimes. Compare jurisdictions with strong vs. weak developer liability.',
    'If open-source development collapses or retreats to closed/controlled release, the constraint extracts a public good (open capability commons) for private deployer benefit — strengthening snare classification. If open-source adapts (liability waivers, capability gating, collective insurance), the extraction is managed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_existential_exposure, empirical, 'Whether the constraint''s extraction destroys the open-source capability commons.').

omega_variable(
    regulatory_convergence_on_developer_liability,
    'Will major regulatory regimes converge on developer_liability as the stable equilibrium, or is this a transitional scaffold toward shared_liability?',
    'Monitor EU AI Act implementation, US federal legislation, and state-level regimes for liability allocation shifts. Track judicial precedent on AI product liability.',
    'If developer_liability stabilizes as the global norm, the tangled_rope classification hardens — the coordination function is locked in with its extraction asymmetry. If shared_liability emerges, developer_liability reclassifies as scaffold (transitional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_convergence_on_developer_liability, conceptual, 'Whether the constraint is transitional (scaffold) or stable (tangled_rope/snare).').

omega_variable(
    committer_framing_kernel_liability_attribution,
    'How does the developer_liability reading''s structural classification change if the kernel''s committer frame shifts to deployer_liability or shared_liability?',
    'Generate sibling constraint stories for deployer_liability and shared_liability readings; compare their ε, beneficiary/victim structures, and computed per-seat types.',
    'If sibling readings produce fundamentally different classifications (e.g., deployer_liability as rope, shared_liability as scaffold), the kernel''s classification is reading-dependent — the ''liability attribution'' label masks structural heterogeneity. This validates the ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_kernel_liability_attribution, conceptual, 'Commitment-system framing under-determination: the kernel''s classification depends on which reading instantiates it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t2018, liability_attribution__developer_liability, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(liab_tr_t2020, liability_attribution__developer_liability, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(liab_tr_t2022, liability_attribution__developer_liability, theater_ratio, 2022, 0.24).
narrative_ontology:measurement(liab_tr_t2023, liability_attribution__developer_liability, theater_ratio, 2023, 0.28).
narrative_ontology:measurement(liab_tr_t2024, liability_attribution__developer_liability, theater_ratio, 2024, 0.3).
narrative_ontology:measurement(liab_tr_t2025, liability_attribution__developer_liability, theater_ratio, 2025, 0.31).
narrative_ontology:measurement(liab_tr_t2026, liability_attribution__developer_liability, theater_ratio, 2026, 0.32).

% Extraction over time
narrative_ontology:measurement(liab_be_t2018, liability_attribution__developer_liability, base_extractiveness, 2018, 0.25).
narrative_ontology:measurement(liab_be_t2020, liability_attribution__developer_liability, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(liab_be_t2022, liability_attribution__developer_liability, base_extractiveness, 2022, 0.52).
narrative_ontology:measurement(liab_be_t2023, liability_attribution__developer_liability, base_extractiveness, 2023, 0.58).
narrative_ontology:measurement(liab_be_t2024, liability_attribution__developer_liability, base_extractiveness, 2024, 0.63).
narrative_ontology:measurement(liab_be_t2025, liability_attribution__developer_liability, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement(liab_be_t2026, liability_attribution__developer_liability, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t2018, liability_attribution__developer_liability, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement(liab_su_t2020, liability_attribution__developer_liability, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(liab_su_t2022, liability_attribution__developer_liability, suppression_requirement, 2022, 0.38).
narrative_ontology:measurement(liab_su_t2023, liability_attribution__developer_liability, suppression_requirement, 2023, 0.42).
narrative_ontology:measurement(liab_su_t2024, liability_attribution__developer_liability, suppression_requirement, 2024, 0.44).
narrative_ontology:measurement(liab_su_t2025, liability_attribution__developer_liability, suppression_requirement, 2025, 0.45).
narrative_ontology:measurement(liab_su_t2026, liability_attribution__developer_liability, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__developer_liability, 0.12).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, ai_act_enforcement_regime).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, open_source_ai_sustainability).

% DUAL FORMULATION NOTE:
% The liability_attribution kernel decomposes into three readings with distinct ε and structural profiles: developer_liability (this story, ε=0.68, tangled_rope), deployer_liability (ε≈0.45, likely rope), shared_liability (ε≈0.35, likely scaffold). The developer reading extracts from capability creators; the deployer reading extracts from deployment controllers; the shared reading distributes extraction along the value chain. All three are linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__developer_liability, institutional, 0.15).
constraint_indexing:directionality_override(liability_attribution__developer_liability, organized, 0.2).
constraint_indexing:directionality_override(liability_attribution__developer_liability, moderate, 0.85).
constraint_indexing:directionality_override(liability_attribution__developer_liability, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
