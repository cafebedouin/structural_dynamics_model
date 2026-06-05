% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer Liability Attribution in AI System Governance
 *   domain: technology_governance/legal_regulatory_design
 *
 * SUMMARY:
 *   Liability attribution in AI governance is a foundational problem: when an
 *   AI system causes harm (discrimination in hiring, dangerous output in
 *   safety-critical contexts, misinformation amplification), who bears legal
 *   and financial responsibility? The deployer liability reading allocates
 *   primary responsibility to organizations that select and deploy AI
 *   systems, on the grounds that deployers have deployment-context control
 *   and decision authority. This creates a tangled rope: deployer liability
 *   does coordinate responsible deployment (incentivizes due diligence,
 *   testing, transparency, safety investment) but simultaneously extracts
 *   from deployers — they absorb legal risk for outcomes they cannot fully
 *   predict or control, while upstream actors (foundation model providers)
 *   externalize deployment risk entirely. The constraint operates at the
 *   intersection of technology governance, legal theory, and regulatory
 *   design. The reading's structural claim is that deployment-context control
 *   (choosing which system to deploy, in which configuration, for which
 *   users) entails liability for deployment outcomes — a straightforward
 *   causation and control principle. However, the opacity of foundation
 *   models, the heterogeneity of deployment contexts, and the information
 *   asymmetries between providers (who know training and capability envelope)
 *   and deployers (who know use context but not model internals) create
 *   structural pressure on this reading. The extractiveness has risen over
 *   the measurement interval (0.35 → 0.58) as model capabilities have
 *   increased, making deployment failures more consequential, and as opacity
 *   has increased (larger models are less interpretable), making due
 *   diligence harder. Suppression has also risen (0.40 → 0.62) as regulatory
 *   regimes and liability doctrines have tightened without providing
 *   deployers the information tools to actually exercise the control the
 *   liability regime assumes.
 *
 * KEY AGENTS:
 *   - Deploying Organizations: Primary duty-bearers (institutional/arbitrage into constrained) — bear legal liability under this reading; face coordination incentive (liability drives safety) and extraction (risk they cannot fully control)
 *   - Foundation Model Providers: Primary beneficiaries (institutional/arbitrage) — externalize downstream risk entirely while capturing model sales upside
 *   - End Users and Affected Third Parties: Primary victims (powerless/trapped) — bear deployment harms while having no control over deployment decisions or liability allocation
 *   - Regulatory Bodies: Secondary institutional actor (institutional/arbitrage) — coordinate through delegating enforcement to liability markets while preserving rule-making authority
 *   - Legacy Legal Doctrine: Institutional inertia (institutional/arbitrage) — deployer liability applies standard product/professional liability frameworks despite degraded fit for opaque, emergent AI systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.58).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.62).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.58).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer Liability Attribution in AI System Governance").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, 'e8a809b2-66ad-4088-b8f3-fbc5d20538cf').
narrative_ontology:cs_kernel_codification('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', formalized).
narrative_ontology:cs_authority_grounding('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', extraction).
narrative_ontology:cs_interpretation_layer_present('e8a809b2-66ad-4088-b8f3-fbc5d20538cf').
narrative_ontology:cs_reading_relation('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', foundational, deployer_context_control_entails_liability).
narrative_ontology:cs_axiom_status(deployer_context_control_entails_liability, holdable).
narrative_ontology:cs_axiom_grounding('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', deployer_context_control_entails_liability, deontological).
narrative_ontology:cs_axiom('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', foundational, deployer_due_diligence_controls_risk).
narrative_ontology:cs_axiom_status(deployer_due_diligence_controls_risk, overridden).
narrative_ontology:cs_axiom_grounding('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', deployer_due_diligence_controls_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', common_law_product_liability).
narrative_ontology:cs_drift_state('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', contemporary_ai_opacity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e8a809b2-66ad-4088-b8f3-fbc5d20538cf', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, deployment_platforms).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, deploying_organizations).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, end_users_affected_by_deployment).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, third_parties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USERS / AFFECTED THIRD PARTIES (SNARE) — Bear harm from deployment failures (discrimination, misinformation, safety failures) while having no meaningful control over deployment decisions, model selection, or system design. Cannot exit or influence the constraint. Maximum extraction experienced: full liability burden on deployers means deployers externalize costs to users, and users have no recourse mechanism tied to their own decision authority.
constraint_indexing:constraint_classification(liability_attribution__deployer_liability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPLOYING ORGANIZATIONS (TANGLED ROPE) — Bear primary legal liability under this reading, creating incentives for responsible deployment (genuine coordination function: liability drives due diligence, testing, and transparency). BUT constrained by: (a) incomplete information about model behavior across deployment contexts; (b) cost of comprehensive testing and monitoring; (c) pressure to compete with lower-cost deployers who externalize risk. Mixed function: coordination mechanism (liability incentivizes care) with significant asymmetric extraction (deployers absorb risk they cannot fully control, while upstream actors externalize).
constraint_indexing:constraint_classification(liability_attribution__deployer_liability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOUNDATION MODEL PROVIDERS (ROPE) — Externalize deployment risk entirely. Under deployer liability, providers face minimal downside from downstream harms while capturing all upside from model sales. Pure coordination from provider's view: they coordinate the model development and licensing; deployers absorb the legal liability and deployment-context risks. Net beneficiary — extraction flows toward providers, not away.
constraint_indexing:constraint_classification(liability_attribution__deployer_liability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY BODIES (TANGLED ROPE) — Deployer liability creates coordination incentive: liability regimes concentrate enforcement authority in deployers' due diligence, reducing regulatory burden. BUT also asymmetric extraction: authorities defer enforcement to the market (liability liability mechanism) while preserving the authority to shift rules later (arbitrage: they can change the liability rule, exit the current regime, or impose additional obligations). Moderate extraction because regulators benefit from deployer-side burden-shifting while maintaining rule-making supremacy.
constraint_indexing:constraint_classification(liability_attribution__deployer_liability, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY LIABILITY DOCTRINE (PITON) — Deployer liability represents application of standard product liability or professional negligence doctrine to AI systems. The doctrine persists through institutional inertia (it is how legal systems normally allocate risk) despite degraded function (AI systems have opacity, contextual contingency, and emergent behaviors that existing liability frameworks struggle to capture). Theater ratio ≥ 0.70 because much of the actual enforcement is performative: courts apply legacy doctrines (reasonable care, foreseeability, proximate cause) that were designed for transparent, stable products and cannot actually adjudicate AI-specific failure modes.
constraint_indexing:constraint_classification(liability_attribution__deployer_liability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some allocation of liability is logically necessary: harmful outcomes require causal explanation, and causal explanation requires assignment of responsibility. This perspective risks naturalizing the contingent choice (deployer liability) as inevitable law. The engine will flag this as a false summit: the logical necessity of liability allocation does NOT determine which party bears it. Deployer, developer, shared, and provider liabilities are all logically coherent — the choice is institutional and empirical, not natural law.
constraint_indexing:constraint_classification(liability_attribution__deployer_liability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__deployer_liability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liability_attribution__deployer_liability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liability_attribution__deployer_liability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__deployer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liability_attribution__deployer_liability, TR),
    TR >= 0.70.

:- end_tests(liability_attribution__deployer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. Deployers bear primary legal liability while having incomplete control over system behavior due to foundation model opacity, distribution shift, and emergent capabilities. The extractiveness reflects this control-responsibility gap: deployers assume liability but cannot fully predict or prevent failures. The trajectory upward (0.35 → 0.58) reflects increasing model complexity and opacity — the harder it becomes for deployers to characterize model behavior, the more extractive the liability becomes. At the starting point (ε=0.35), deployer liability was a reasonable coordination mechanism (transparency and testing could reduce risk). As opacity increases, the extractiveness rises toward snare territory because due diligence becomes impossible despite best effort. Suppression (0.62): Moderate-high and rising. Deployers face significant barriers: incomplete information about model internals, inability to access training data details, legal complexity of liability standards, pressure to compete against lower-cost competitors who externalize risk, cost of comprehensive testing and monitoring. The rising trajectory reflects tightening regulatory requirements without corresponding information access — suppression increases when deployers are held liable for factors outside their control. Theater ratio (0.48): Moderate. The deployer liability regime is substantially functional (not purely performative) but contains significant theater: legal standards for 'reasonable care' and 'due diligence' are often applied post-hoc by courts using standards designed for transparent products, not opaque AI systems. Deployers may conduct extensive testing that appears responsible but cannot actually predict emergent failure modes. Theater is lower than legacy doctrine (piton perspective) because actual deployment decisions and safety practices do respond to liability incentives.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap reveals the core structural contradiction in deployer liability. Foundation model providers see a rope (coordination with minimal liability exposure); deployers see tangled rope (coordination incentive mixed with uncontrollable risk); end users see snare (harm with no control or recourse); regulators see tangled rope (efficient delegation of market enforcement); legacy doctrine sees piton (standard application of outdated framework); and the civilizational analytical view risks seeing mountain (someone must bear liability — this is a logical necessity, not institutional choice). The gap shows that deployer liability is a reading of a contested kernel, not a natural law. The analytical perspective's mountain classification is flagged as a false summit: the logical fact that liability must be allocated does NOT determine that it must be allocated to deployers.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from its structural relationship to the constraint. Foundation model providers benefit maximally (d ≈ 0.05) — they gain model sales revenue while externalizing deployment liability. Deployers are both beneficiaries (of easy off-the-shelf models) and victims (of liability for opaque systems); their derived d is moderate (≈ 0.65), reflecting their mixed position. End users and affected third parties are pure victims with no control (d ≈ 0.95) — they bear harm without control or liability recourse. Regulators maintain arbitrage exit (d ≈ 0.25) — they can change the liability rule at any time, so their experience of extraction is dampened. The constraint's effective extractiveness (χ) is computed as ε × f(d) × σ(S), where σ(global) = 1.2. Foundation model providers experience low χ (beneficiary position); deployers experience high χ (victim-adjacent position without full victim status); end users experience maximum χ (powerless victim trapped at global scope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing how deployer liability simultaneously coordinates and extracts. The coordination function is genuine: liability incentivizes safety investment, testing, transparency, and responsible model selection. This is not theater; organizations do invest in due diligence because of liability exposure. The extraction is also genuine: deployers bear risk they cannot fully control due to model opacity and performance heterogeneity across contexts. The constraint persists because both functions operate simultaneously. Removing the liability (pure coordination via incentives or subsidies) would eliminate the extraction but also eliminate the coordination incentive. Shifting liability to providers would remove the extraction on deployers but would likely reduce provider incentive for model quality/safety (moral hazard on provider side). The tangled rope classification is stable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_liability_mismatch,
    'Can deployers meaningfully discharge their due diligence burden when the model''s behavior under deployment conditions is substantially opaque to them?',
    'Comparative analysis of deployer knowledge vs model uncertainty (interpretability gaps, distribution shift detection, emergent failures). Court records of liability cases establishing burden standards. Post-hoc analysis of cases where deployers conducted ''reasonable'' testing but failures occurred.',
    'If deployers cannot meaningfully predict failure modes: liability becomes strict liability in practice (extractive, regardless of care taken). If deployers can predict through testing protocols: deployer liability functions as intended (coordination mechanism). Classification could shift from tangled_rope to snare if the opacity gap makes due diligence impossible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opacity_liability_mismatch, empirical, 'Whether deployer due diligence can actually control AI system deployment risk given model opacity').

omega_variable(
    reading_kernel_ambiguity,
    'Is this deployer liability reading the correct instantiation of the liability_attribution kernel, or does one of the sibling readings (developer_liability, shared_liability) better capture the structural reality of AI governance?',
    'Structural comparison across readings: which allocation produces the strongest incentive alignment for safety? Which is most legally defensible under existing doctrine? Which minimizes moral hazard? Which reading''s axioms are most robust to empirical falsification?',
    'If developer_liability is superior: the deployer reading is overridden (axioms become overridden status). If shared_liability is superior: the deployer reading foreclose''s that shared reading, creating institutional conflict. If coexistence is stable: all three readings remain live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether deployer liability is the correct kernel reading or whether sibling readings better capture AI governance structure').

omega_variable(
    foundation_model_opacity_asymmetry,
    'Should foundation model providers bear some liability for opacity they created, or does deployer liability correctly place that burden entirely on the party with deployment-context information asymmetry advantage?',
    'Analysis of information asymmetries: (a) provider knows training data, training process, and capability envelope; (b) deployer knows use context, user population, harm potential; (c) neither knows interaction effects. Legal precedent for product liability in opaque systems (pharmaceuticals, aircraft). Empirical comparison of failure attribution in cases.',
    'If providers should bear opacity cost: deployer liability is overridden or becomes shared_liability. If deployers rightfully bear it: this axiom (deployer_context_control_entails_liability) holds. If the asymmetry is irreducible: shared liability becomes more coherent than either unilateral reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundation_model_opacity_asymmetry, empirical, 'Whether deployer liability correctly allocates opacity-related risk to the party with deployment-context control').

omega_variable(
    performance_heterogeneity_predictability,
    'Can deployers realistically predict how a foundation model will perform across diverse deployment contexts, or does model behavior heterogeneity make this prediction impossible within reasonable cost bounds?',
    'Empirical evaluation: (a) model behavior variance across deployment contexts (use cases, user populations, data distributions); (b) cost of comprehensive testing relative to deployment revenue; (c) success rate of deployer-conducted pre-deployment testing in predicting post-deployment failures.',
    'If heterogeneity is high and prediction is costly: deployer liability becomes extractive despite good-faith efforts (snare with tangled_rope window). Due diligence burden becomes impossible, shifting classification. If heterogeneity is manageable: tangled_rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_heterogeneity_predictability, empirical, 'Whether performance heterogeneity allows deployers to predict and control deployment outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_deployer_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(liab_deployer_tr_t3, liability_attribution__deployer_liability, theater_ratio, 3, 0.41).
narrative_ontology:measurement(liab_deployer_tr_t6, liability_attribution__deployer_liability, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(liab_deployer_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(liab_deployer_be_t3, liability_attribution__deployer_liability, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(liab_deployer_be_t6, liability_attribution__deployer_liability, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(liab_deployer_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(liab_deployer_su_t3, liability_attribution__deployer_liability, suppression_requirement, 3, 0.51).
narrative_ontology:measurement(liab_deployer_su_t6, liability_attribution__deployer_liability, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, model_opacity__interpretability_asymmetry).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, deployment_context__distributional_heterogeneity).

% DUAL FORMULATION NOTE:
% Deployer liability is one reading of the liability_attribution kernel. Sibling readings (developer_liability and shared_liability) are separate constraints with different ε values and beneficiary/victim structures. All three are linked via network.affects_constraints. The deployer reading presupposes that deployers have sufficient context control to bear responsibility; downstream constraints (model opacity, distributional heterogeneity) examine whether that control assumption holds empirically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
