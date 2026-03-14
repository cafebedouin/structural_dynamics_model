% ============================================================================
% CONSTRAINT STORY: derivative_work_definition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_definition, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: derivative_work_definition
 *   human_readable: Derivative Work Definition in Copyright Law
 *   domain: intellectual_property/copyright_law
 *
 * SUMMARY:
 *   The legal definition of 'derivative work' in copyright law creates a
 *   structural tension between original creator protection and transformation
 *   culture. The constraint exhibits hybrid coordination-extraction
 *   characteristics: copyright holders use the derivative work boundary to
 *   coordinate legitimate licensing and ensure attribution (coordination
 *   function), while simultaneously deploying undefined boundaries to
 *   suppress transformative creativity and extract licensing rents
 *   (extraction function). The extractiveness metric (0.52) reflects that
 *   licensing revenue flows primarily to original rights holders while
 *   transformers absorb legal risk. The theater ratio (0.65) reflects that
 *   enforcement mechanisms increasingly rely on intimidation
 *   (cease-and-desist letters, DMCA abuse) rather than on coordination
 *   necessity, as digital distribution has eroded the original justification
 *   for derivative work control. The constraint shows increasing
 *   extractiveness over the 20-year interval as digital platforms have
 *   enabled higher-volume transformation, leading copyright holders to
 *   tighten definitional boundaries and enforcement.
 *
 * KEY AGENTS:
 *   - Amateur Transformers: Primary victims (powerless/trapped) — fan artists, remix creators, adaptors with no legal recourse; bear full legal and reputational risk
 *   - Professional Adapters: Secondary victims (moderate/constrained) — legitimate licensing exists but requires expensive approval and introduces creative constraint
 *   - Original Copyright Holders: Primary beneficiaries (institutional/arbitrage) — publishers, studios, music labels capturing licensing revenue and derivative work control
 *   - Legacy Publishing Industry: Institutional actor (institutional/constrained) — traditional gatekeepers maintaining derivative work restrictions through litigation theater; practical leverage declining
 *   - Creative Commons Coalition: Organized actors (organized/mobile) — EFF, open-source communities, academic commons building alternative frameworks with clear transformation rights
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating derivative work boundary as inherent logical limit rather than as contingent legal construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_definition, 0.52).
domain_priors:suppression_score(derivative_work_definition, 0.58).
domain_priors:theater_ratio(derivative_work_definition, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_definition, extractiveness, 0.52).
narrative_ontology:constraint_metric(derivative_work_definition, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(derivative_work_definition, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_definition, tangled_rope).
narrative_ontology:human_readable(derivative_work_definition, "Derivative Work Definition in Copyright Law").
narrative_ontology:topic_domain(derivative_work_definition, "intellectual_property/copyright_law").

domain_priors:requires_active_enforcement(derivative_work_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_definition, original_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_definition, major_publishers).
narrative_ontology:constraint_beneficiary(derivative_work_definition, entertainment_corporations).
narrative_ontology:constraint_victim(derivative_work_definition, derivative_creators).
narrative_ontology:constraint_victim(derivative_work_definition, transformation_ecosystem).
narrative_ontology:constraint_victim(derivative_work_definition, cultural_remix_practices).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMATEUR TRANSFORMER (SNARE) — Fan artists, remix creators, and independent adaptors face legal jeopardy from undefined derivative work boundaries. They cannot exit without abandoning their creative practice. Maximum suppression: cease-and-desist letters, takedown notices, threat of litigation. Extraction flows entirely toward rights holders; transformer bears all legal and reputational risk.
constraint_indexing:constraint_classification(derivative_work_definition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROFESSIONAL ADAPTER (TANGLED ROPE) — Publishers adapting works for new markets, filmmakers creating movie franchises, game developers building licensed extensions. Benefit from the derivative work framework (legal clarity for their investments) while bearing significant costs (licensing fees, approval delays, creative constraint). Constrained exit: expensive licensing required; exit means losing market access.
constraint_indexing:constraint_classification(derivative_work_definition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINAL COPYRIGHT HOLDER (ROPE) — Publishers and content owners perceive the derivative work boundary as a coordination mechanism: it enables them to license works, control adaptations, and capture licensing revenue. Net beneficiary through licensing fees and control rights. Arbitrage option: can license or withhold at will. Extraction flows toward this agent.
constraint_indexing:constraint_classification(derivative_work_definition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY PUBLISHING INDUSTRY (PITON) — Traditional gatekeepers (major publishers, studios, music labels) maintain the derivative work definition framework through lobbying and litigation, but the constraint's primary function has atrophied. Digital distribution, user-generated content platforms, and open-access norms have eroded the practical leverage of traditional derivative work control. The industry maintains legal boundaries through theater (cease-and-desist letters, DMCA enforcement) rather than through coordination necessity. Theater ratio: high. Enforcement increasingly performative as original function (controlling physical distribution) decays.
constraint_indexing:constraint_classification(derivative_work_definition, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CREATIVE COMMONS COALITION (SCAFFOLD) — Open-licensing frameworks (CC-BY, CC-BY-SA) create an alternative pathway with explicit permissions and clear transformative rights. Organized actors (EFF, open-source communities, academic publishers) build sunset logic: as more creators choose open licensing, the restrictive derivative work definition loses coercive force. Coordination benefit clear; extraction sunset as alternative frameworks mature.
constraint_indexing:constraint_classification(derivative_work_definition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some boundary between original and derived work is inherent to copyright: distinguishing ownership attribution (whose work this is) from transformative reuse (who changed it) is a logical limit, not a policy choice. This perspective risks naturalizing what is actually a contingent legal construction. The engine's false summit detector will flag this as inappropriate naturalization — the boundary between original and derivative is drawn by institutional choice, not by logic.
constraint_indexing:constraint_classification(derivative_work_definition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_definition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(derivative_work_definition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(derivative_work_definition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_definition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(derivative_work_definition, TR),
    TR >= 0.70.

:- end_tests(derivative_work_definition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The derivative work definition enables licensing revenue capture and control asymmetry favoring original rights holders. However, extractiveness is not extreme because legitimate coordination functions exist — original creator attribution, licensing facilitation, and control over commercial adaptations serve real purposes. The metric reflects that both coordination and extraction coexist. Suppression (0.58): Moderate-high. Substantial barriers to transformation include legal uncertainty (undefined boundaries in fair use doctrine), cease-and-desist threats, DMCA abuse, and career risk for professional creators. However, suppression is not total because some transformers operate openly (licensed fan communities, Creative Commons work, academic adaptation). Theater ratio (0.65): Moderate-high. Enforcement increasingly relies on intimidation theater rather than on administrative necessity. Cease-and-desist letters often threaten consequences exceeding actual legal risk; DMCA takedowns are frequently abused; licensing approval processes impose delays beyond genuine coordination needs. The theater increase over the interval reflects that enforcement has shifted from passive gatekeeping (controlling physical distribution) to active suppression of digital transformation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent perception based on structural position. Copyright holders see coordination: the derivative work definition enables licensing markets, ensures attribution, and prevents unauthorized commercial exploitation. This perception is not false — these functions are real. Transformers see extraction: the definition's vagueness and enforcement threats suppress legitimate creative practice. This perception is also real — the chilling effects are documented. Professional adapters see mixed incentives: licensing frameworks enable their business model while imposing approval delays and creative constraints. Open-licensing advocates see a temporary problem: Creative Commons and open-licensing frameworks are building an exit path for creators who want transformation rights without licensing negotiation. The piton observer sees a degraded ritual: copyright enforcement for derivative works increasingly relies on threatening ambiguous legal standards rather than on coordinating legitimate interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural positions: Copyright holders with arbitrage options (can license or withhold) experience low d — they are beneficiaries with maximum control. Amateur transformers with trapped status (no legal path to proceed) experience high d — they are targets bearing maximum extraction. Professional adapters with constrained options (licensing is expensive but available) experience moderate d — some agency, some extraction. The piton perspective arises because the original function (controlling scarce physical distribution) has decayed, leaving the constraint maintained through inertia and legal theater rather than through coordination necessity. The analytical observer risks conflating the institutional construction with a logical limit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution: the derivative work definition is a genuine Tangled Rope, not a misclassified Rope or Snare. It has both coordination function (enabling licensing, protecting attribution, coordinating legitimate commercial adaptation) AND asymmetric extraction (suppressing amateur transformation, creating licensing rents, imposing legal risk on transformers). The tension between these functions cannot be resolved by declaring one or the other. The perspectival gap between copyright holders (who see primary coordination benefit) and transformers (who see primary extraction) reflects the real structural asymmetry, not observer bias. The appropriate analytical response is not to collapse to a single type but to track which function dominates at which scale: at the professional adaptation scale, coordination may dominate (licensing markets work); at the amateur transformation scale, extraction dominates (legal risk exceeds coordination benefit). The constraint family decomposes into three stories: (1) derivative work licensing for professional adaptation (ε~0.35, Tangled Rope with coordination-dominant), (2) derivative work definition for amateur transformation (ε~0.68, Snare with extraction-dominant), (3) derivative work enforcement theater (ε~0.62, Piton as original function decays). This story represents the aggregate view across all three.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_definition_boundary,
    'Where is the boundary between fair use transformation and copyright infringement in cases of substantial similarity without direct copying?',
    'Case law analysis across jurisdictions; comparison of judicial outcomes in transformative use cases (parody, adaptation, remix); identification of consistent thresholds or persistent doctrinal confusion',
    'If boundary is coherent and predictable: constraint reclassifies as coordination mechanism (Rope). If boundary is vague or case-dependent: constraint reclassifies as pure extraction mechanism (Snare) — unpredictability becomes suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_definition_boundary, empirical, 'Clarity and coherence of derivative work boundary in jurisprudence').

omega_variable(
    licensing_revenue_necessity,
    'Do licensing fees for derivative work authorization capture genuine transaction costs and incentive alignment, or do they function primarily as rent extraction?',
    'Comparative analysis: licensing fee levels across industries and time periods; correlation between fee structures and actual cost of permission/approval processes; measurement of creator retention under mandatory licensing vs. permissive regimes',
    'If fees reflect genuine coordination costs: constraint is Tangled Rope (extraction + coordination coexist). If fees exceed coordination costs: constraint reclassifies as Snare (pure extraction disguised as licensing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_revenue_necessity, empirical, 'Whether derivative work licensing fees reflect coordination costs or rent extraction').

omega_variable(
    cultural_suppression_measurement,
    'How much transformative creative practice is suppressed by uncertainty about derivative work boundaries vs. how much is enabled by clarity of licensing frameworks?',
    'Survey data on creator behavior: (a) creators who avoid certain transformations due to legal risk, (b) creators enabled by clear licensing; (c) comparison of creative output and diversity in high-licensing-clarity vs. low-clarity jurisdictions; analysis of chilling effects in fan communities',
    'If suppression > enablement: net effect is extractive (Snare). If enablement > suppression: net effect is coordinative (Rope or Tangled Rope with lower suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_suppression_measurement, empirical, 'Net effect of derivative work definition on creative practice: suppression vs. enablement').

omega_variable(
    open_licensing_adoption_trajectory,
    'At what adoption rate does Creative Commons and open-licensing frameworks achieve critical mass such that the restrictive derivative work definition loses enforceability?',
    'Longitudinal tracking of CC adoption across content categories; measurement of enforcement cost increases as fraction of licensed work grows; identification of threshold where legal enforcement becomes economically irrational',
    'If scaffold sunset is real and approaching: constraint trajectory shows declining χ over time and expected classification drift toward Piton. If adoption plateaus: scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_licensing_adoption_trajectory, empirical, 'Adoption rate and critical mass threshold for open-licensing alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_definition, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deriv_tr_t0, derivative_work_definition, theater_ratio, 0, 0.45).
narrative_ontology:measurement(deriv_tr_t10, derivative_work_definition, theater_ratio, 10, 0.58).
narrative_ontology:measurement(deriv_tr_t20, derivative_work_definition, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(deriv_be_t0, derivative_work_definition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deriv_be_t10, derivative_work_definition, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(deriv_be_t20, derivative_work_definition, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_definition, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_definition, fair_use_doctrine_boundary).
narrative_ontology:affects_constraint(derivative_work_definition, copyright_term_length).
narrative_ontology:affects_constraint(derivative_work_definition, open_licensing_framework_adoption).

% DUAL FORMULATION NOTE:
% The derivative work definition constraint decomposes along scale lines. Professional licensing coordination and amateur transformation suppression are structurally distinct mechanisms with different ε values. This story represents the aggregate constraint; decomposition into professional_derivative_licensing (ε~0.35) and amateur_transformation_suppression (ε~0.68) would provide higher diagnostic resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_definition, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
