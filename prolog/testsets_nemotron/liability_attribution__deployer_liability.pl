% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Deployer Primary Liability for AI System Harms
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint instantiates the deployer_liability reading of the
 *   liability_attribution kernel. Under this reading, legal and regulatory
 *   frameworks assign primary liability for AI system harms to the deploying
 *   entity — the enterprise, government agency, or organization that puts an
 *   AI system into operational use — on the grounds that deployers control
 *   the deployment context and make operational decisions. The reading
 *   emerged alongside the commercialization of foundation models (2022-2024)
 *   and has been codified in the EU AI Act's deployer obligations, proposed
 *   US algorithmic accountability legislation, and standard contract terms
 *   from major model providers. The constraint presents as coordination
 *   (allocating liability to the party with operational control) but operates
 *   with substantial extraction: foundation model providers and developers
 *   externalize downstream risk through contractual liability caps, opacity
 *   of model internals, and take-it-or-leave-it terms, while deployers bear
 *   open-ended liability for harms they cannot fully understand or control.
 *
 * KEY AGENTS:
 *   - foundation_model_providers: Primary beneficiaries (institutional/arbitrage) — externalize deployment risk via liability caps and opacity
 *   - ai_developers: Secondary beneficiaries (organized/arbitrage) — shift downstream liability to deployers
 *   - enterprise_deployers: Primary victims (organized/constrained) — bear open-ended liability with limited model transparency
 *   - public_sector_deployers: Primary victims (institutional/constrained) — sovereign immunity limits but public accountability creates exposure
 *   - small_business_deployers: Primary victims (moderate/trapped) — least capacity for due diligence, most exposed to model opacity
 *   - affected_individuals: Excluded (powerless/trapped) — harmed by AI systems but not party to liability allocation
 *   - regulators: Observers (institutional/analytical) — design liability frameworks but capture by industry shapes outcomes
 *   - insurers: Beneficiaries (organized/mobile) — new liability markets created by deployer exposure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.72).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.68).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.72).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer Primary Liability for AI System Harms").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, 'ef37aa84-f9e7-4644-af7b-413c81a21717').
narrative_ontology:cs_kernel_codification('ef37aa84-f9e7-4644-af7b-413c81a21717', formalized).
narrative_ontology:cs_authority_grounding('ef37aa84-f9e7-4644-af7b-413c81a21717', extraction).
narrative_ontology:cs_interpretation_layer_present('ef37aa84-f9e7-4644-af7b-413c81a21717').
narrative_ontology:cs_reading_relation('ef37aa84-f9e7-4644-af7b-413c81a21717', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('ef37aa84-f9e7-4644-af7b-413c81a21717', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('ef37aa84-f9e7-4644-af7b-413c81a21717', foundational, deployer_control_justifies_liability).
narrative_ontology:cs_axiom_status(deployer_control_justifies_liability, holdable).
narrative_ontology:cs_axiom_grounding('ef37aa84-f9e7-4644-af7b-413c81a21717', deployer_control_justifies_liability, instrumental).
narrative_ontology:cs_axiom('ef37aa84-f9e7-4644-af7b-413c81a21717', secondary, model_provider_liability_cap_is_legitimate).
narrative_ontology:cs_axiom_status(model_provider_liability_cap_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ef37aa84-f9e7-4644-af7b-413c81a21717', model_provider_liability_cap_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('ef37aa84-f9e7-4644-af7b-413c81a21717', deployer_operational_control_framework).
narrative_ontology:cs_drift_state('ef37aa84-f9e7-4644-af7b-413c81a21717', post_foundation_model_commercialization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ef37aa84-f9e7-4644-af7b-413c81a21717', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, ai_developers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, enterprise_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, public_sector_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, small_business_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, insurers).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, deployer_context_control_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, operational_decision_authority_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide foundation models via API or licensed deployment. Collect revenue from deployment while capping liability at contract value or API fees. Control model access, updates, and transparency. Can retire models, change terms, or shift architectures unilaterally. Their 'exit' is arbitrage-grade: they control the asset and set terms.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Build applications on foundation models or develop specialized models. Shape liability frameworks through industry associations and standards bodies. Benefit from deployer-liability regimes that limit their downstream exposure. Can switch foundation models or deployment strategies with moderate friction.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_developers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, ai_developers, agenda_setter).

% Deploy AI systems for business operations (customer service, hiring, credit scoring, content moderation). Bear liability for discriminatory outcomes, errors, security failures. Have procurement processes but limited leverage on model providers. Due diligence limited to provider documentation and benchmark tests — cannot inspect training data or architecture. Switching providers requires re-engineering integration, retraining staff, regulatory re-approval.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, enterprise_deployers, payer,
    organized, biographical, constrained, national).

% Deploy AI for benefits administration, law enforcement, healthcare, education. Subject to administrative law, constitutional constraints, public accountability. Sovereign immunity provides partial shield but political and legal exposure is high. Procurement rules limit model choice. Cannot 'exit' core functions — must deploy something. Dependent on commercial providers for state-of-the-art capability.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, public_sector_deployers, payer,
    institutional, generational, constrained, national).

% Use AI tools for marketing, operations, customer interaction. No dedicated legal/compliance teams. Rely on consumer-grade AI services with click-wrap terms. Liability exposure is existential — a single harm event can bankrupt the business. No capacity for technical due diligence. Switching costs are low but all alternatives have similar terms. Effectively trapped in the liability regime.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, small_business_deployers, payer,
    moderate, biographical, trapped, local).

% Subject to AI decisions (loan denial, hiring rejection, content removal, benefit termination). Harmed by errors, bias, hallucinations, security failures. No contractual relationship with deployer or developer. Recourse limited to litigation against deployer (who may be judgment-proof) or regulatory complaint (slow, uncertain). Not represented in liability allocation negotiations.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, affected_individuals, excluded,
    powerless, immediate, trapped, local).

% Design and enforce liability frameworks (EU AI Act, US Executive Orders, sectoral rules). Rely on industry expertise for technical standards. Subject to regulatory capture via revolving doors, lobbying, information asymmetry. Their frameworks nominally protect affected individuals but structurally entrench deployer liability.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulators, observer,
    institutional, generational, analytical, national).

% Write AI liability policies for deployers. New market created by deployer liability exposure. Price premiums based on deployer risk profile, not model provider risk. Benefit from the constraint's extraction without bearing its operational costs. Can exit by withdrawing coverage or repricing.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, insurers, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates liability for AI system harms to a identifiable party (the deployer) so that victims have a clear target for redress and deployers have incentive to implement safeguards, monitoring, and human oversight in deployment.
% TRANSFER_FUNCTION: Moves the financial and legal risk of AI harm from foundation model providers and developers (who create and control the underlying model behavior) to deployers (who configure and operate the model in context), via liability caps, indemnification clauses, and regulatory frameworks that treat deployment as the locus of control.
% ABSENT_VOICES: Affected individuals (those harmed by AI systems) are structurally excluded from liability allocation — they have no seat at the table where liability frameworks are negotiated. Small business deployers are effectively excluded by capacity constraints. Civil society organizations representing affected communities are consulted performatively but their input does not shape the core liability allocation.
% DISAPPEARANCE_RATIONALE: If deployer-primary liability vanished overnight, foundation model providers would face direct liability for downstream harms — forcing either massive liability reserves, fundamental architecture changes for verifiability, or withdrawal from high-risk domains. Deployers would lose liability exposure but also lose the (weak) incentive for deployment safeguards. Insurance markets would reprice. The AI deployment ecosystem would reorganize around upstream accountability.
% FOUNDING_PROBLEM: As AI systems moved from research to commercial deployment (2018-2022), harms emerged (bias, errors, security failures) with no clear liability home. Victims had no redress. Deployers had no incentive to implement safeguards. Model providers disclaimed all responsibility. A liability allocation rule was needed to close the accountability gap.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (accountability gap for AI harms) is attested as live by affected individuals' advocates, consumer protection regulators, and independent AI safety researchers — all outside the beneficiary set. Foundation model providers and deployer industry associations attest the problem is substantially solved by current frameworks (deployer liability + voluntary safeguards) — but they are beneficiaries. The contested status reflects this split: the accountability gap persists for the most severe harms (discriminatory systems, critical infrastructure failures) where deployer liability has proven insufficient.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because deployers bear liability disproportionate to their actual causal control over model behavior — the opacity of foundation models makes meaningful due diligence infeasible, yet liability regimes treat deployers as having 'control.' Suppression (0.68) reflects contractual lock-in (standard terms from dominant providers), regulatory capture (industry-shaped liability frameworks), and technical dependency (no viable alternative models for many use cases). Theater ratio (0.45) is significant: the coordination story (deployer control justifies liability) is real but increasingly performative as model capability shifts upstream; compliance rituals (risk assessments, documentation) grow while actual risk management capacity shrinks. Accessibility collapse (0.58) and resistance (0.62) are moderate: alternatives (self-hosted models, liability negotiation) exist but are practically constrained for most deployers. The claimed type is tangled_rope because there IS a genuine coordination function (someone must be liable for deployed systems) AND asymmetric extraction (upstream actors externalize risk onto downstream deployers).
 *
 * PERSPECTIVAL GAP:
 *   From the foundation model provider seat, the arrangement is rope: they provide a capability, deployers control its use, liability follows control. From the deployer seat (especially small business and public sector), the same arrangement is snare: they are held liable for behaviors they cannot inspect, predict, or control, with no viable exit. The engine computes this divergence from the structural data — foundation model providers have arbitrage-grade exit (can shift terms, retire models, change pricing), deployers have constrained-to-trapped exit (switching costs, dependency, regulatory requirements).
 *
 * DIRECTIONALITY LOGIC:
 *   Foundation model providers and AI developers are structural beneficiaries: they collect the value of AI deployment (revenue, adoption, data) while externalizing the tail risk of harm via liability caps, indemnification clauses, and model opacity. Deployers are structural victims: they bear open-ended liability for harms whose causal roots lie upstream in training data, architecture, and alignment choices they cannot access. The 'deployment context control' that nominally justifies liability attribution is, in practice, control over configuration parameters of a system whose deep behavior is determined elsewhere. Affected individuals are excluded — they suffer harms but have no seat in the liability allocation. Regulators observe but the liability frameworks they produce show industry capture signatures. Insurers are incidental beneficiaries: deployer liability creates new premium streams.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating liability for autonomous system harms — remains live (AI deployment is expanding, harms are occurring). But the deployer-liability reading may have outlived its coordination function: as model capability and opacity increase, the deployer's actual control decreases while their liability exposure increases. The constraint persists not because it solves the coordination problem well, but because upstream beneficiaries (foundation model providers) have the power to maintain it. This is mandatrophy: a coordination arrangement whose function has inverted toward extraction but persists through the inertia of the original justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the liability_attribution kernel admit deployer_liability as a stable reading, or is the deployer liability frame itself a strategic construction by foundation model providers to externalize downstream risk?',
    'Trace the genealogy of the deployer-liability framing in policy discourse: identify whether it emerged from independent legal analysis or was promoted through industry lobbying and funded research. Compare the timeline of deployer-liability advocacy with foundation model providers'' liability exposure events.',
    'If the deployer-liability reading is a strategic construction rather than an independent legal conclusion, its claimed coordination function (allocating liability to the party with deployment control) is cover for extraction by developers and foundation model providers. The constraint reclassifies from tangled_rope toward snare for deployer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the deployer-liability reading is an independent legal conclusion or a strategic construction by upstream actors').

omega_variable(
    opacity_due_diligence_gap,
    'Can deployers meaningfully exercise due diligence on foundation model behavior when the model''s training data, architecture, and emergent capabilities are opaque to them?',
    'Empirical study of deployer due diligence practices: audit whether enterprises can actually assess model risks pre-deployment given current transparency regimes. Measure the gap between regulatory due diligence expectations and technical feasibility.',
    'If due diligence is technically infeasible, the deployer''s ''control'' is illusory and the liability attribution extracts from a party that cannot effectively manage the risk — shifting toward snare. If feasible, the coordination function holds and tangled_rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_due_diligence_gap, empirical, 'Whether deployers can actually perform meaningful due diligence on opaque foundation models').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (contractual terms, regulatory capture, technical lock-in) or internalized (deployers accepting liability as inevitable cost of AI adoption)?',
    'Post-exit suppression trajectory: track deployers who attempt to negotiate liability terms or switch providers. If suppression persists after contractual barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — deployers carry the suppression with them through acceptance of unfavorable terms as ''market standard.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for deployer liability acceptance').

omega_variable(
    causal_contribution_vs_control,
    'Does deployment-context control actually map to causal contribution for AI harms, or are the most consequential harms determined upstream (training data, architecture, alignment choices)?',
    'Causal analysis of high-profile AI harm incidents: partition causal contribution between upstream development choices and downstream deployment configurations. Compare with liability allocation under deployer-liability regimes.',
    'If harms are predominantly upstream-caused, deployer liability is misattribution — extraction from the wrong party. The constraint''s coordination claim (liability follows control) fails its own causal test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_vs_control, empirical, 'Whether deployment control correlates with actual causal contribution to AI harms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t3, liability_attribution__deployer_liability, theater_ratio, 3, 0.25).
narrative_ontology:measurement_basis(liab_tr_t3, observed).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__deployer_liability, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(liab_tr_t6, observed).
narrative_ontology:measurement(liab_tr_t9, liability_attribution__deployer_liability, theater_ratio, 9, 0.4).
narrative_ontology:measurement_basis(liab_tr_t9, observed).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.45).
narrative_ontology:measurement_basis(liab_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t3, liability_attribution__deployer_liability, base_extractiveness, 3, 0.48).
narrative_ontology:measurement_basis(liab_be_t3, observed).
narrative_ontology:measurement(liab_be_t6, liability_attribution__deployer_liability, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(liab_be_t6, observed).
narrative_ontology:measurement(liab_be_t9, liability_attribution__deployer_liability, base_extractiveness, 9, 0.65).
narrative_ontology:measurement_basis(liab_be_t9, observed).
narrative_ontology:measurement(liab_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.72).
narrative_ontology:measurement_basis(liab_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t3, liability_attribution__deployer_liability, suppression_requirement, 3, 0.55).
narrative_ontology:measurement_basis(liab_su_t3, observed).
narrative_ontology:measurement(liab_su_t6, liability_attribution__deployer_liability, suppression_requirement, 6, 0.62).
narrative_ontology:measurement_basis(liab_su_t6, observed).
narrative_ontology:measurement(liab_su_t9, liability_attribution__deployer_liability, suppression_requirement, 9, 0.66).
narrative_ontology:measurement_basis(liab_su_t9, observed).
narrative_ontology:measurement(liab_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(liab_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, resource_allocation).
narrative_ontology:boltzmann_floor_override(liability_attribution__deployer_liability, 0.18).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, ai_act_deployer_obligations).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, foundation_model_provider_terms).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, algorithmic_accountability_legislation).

% DUAL FORMULATION NOTE:
% The liability_attribution kernel decomposes into three constraint stories: deployer_liability (this story), developer_liability, and shared_liability. Each has distinct ε values, stakeholder structures, and classifications. The deployer_liability reading has the highest extractiveness (ε=0.72) because it concentrates liability on the party least able to manage upstream causal factors. The developer_liability reading would show lower extractiveness but higher suppression on developers. The shared_liability reading distributes extraction but increases coordination complexity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__deployer_liability, institutional, 0.15).
constraint_indexing:directionality_override(liability_attribution__deployer_liability, organized, 0.75).
constraint_indexing:directionality_override(liability_attribution__deployer_liability, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
