% ============================================================================
% CONSTRAINT STORY: regime_change_structural_break
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regime_change_structural_break, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regime_change_structural_break
 *   human_readable: World3 Regime Change Structural Break Assumption
 *   domain: system_dynamics/industrial_ecology/sustainability_science
 *
 * SUMMARY:
 *   The World3 model, published in 1972's 'The Limits to Growth,' holds
 *   feedback loop structure constant across its century-scale projections
 *   while acknowledging that these relationships will 'rearrange and
 *   reconnect' during collapse transitions. This structural stability
 *   assumption enables tractable computation and parameter recalibration but
 *   creates a validity paradox: the model's projections are most
 *   policy-relevant during the collapse transition, exactly when the authors
 *   acknowledge the assumption breaks down. The constraint exhibits piton
 *   characteristics: it persists not because it is empirically validated
 *   (historical crises show measurable regime shifts in socio-economic
 *   feedback parameters) but because abandoning it would require rebuilding
 *   the entire modeling framework. The theater ratio (0.68) reflects that
 *   recalibration exercises maintain the assumption performatively — updating
 *   parameter values while preserving the structural relationships that may
 *   themselves be regime-dependent. Alternative frameworks (agent-based
 *   models, hybrid approaches) can represent structural change endogenously
 *   but remain computationally intensive and less institutionally
 *   established, creating a scaffold dynamic where the constraint's sunset
 *   depends on methodological maturation rather than empirical resolution.
 *
 * KEY AGENTS:
 *   - Original World3 Modeling Team: Primary beneficiary (institutional/arbitrage) — model's continued influence supports methodological legacy; can exit to alternative frameworks without career penalty
 *   - Limits to Growth Advocacy Coalition: Secondary beneficiary (institutional/arbitrage) — structural stability assumption provides stable reference point for policy communication; can cite alternative models if needed
 *   - Recalibration Research Groups: Mixed position (moderate/constrained) — face genuine coordination problem (maintaining comparability) alongside extraction (career risk of challenging assumptions, resource investment potentially invalidated by regime shifts)
 *   - Policy Makers: Primary victim (powerless/trapped) — bear full cost if regime shifts invalidate projections during critical transition; switching frameworks is politically and administratively costly once planning is institutionalized
 *   - Agent-Based Modeling Coalition: Organized agents (organized/mobile) — building alternative frameworks with sunset logic; see structural stability as temporary limitation of system dynamics paradigm
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing methodological choice as epistemological necessity; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regime_change_structural_break, 0.28).
domain_priors:suppression_score(regime_change_structural_break, 0.35).
domain_priors:theater_ratio(regime_change_structural_break, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regime_change_structural_break, extractiveness, 0.28).
narrative_ontology:constraint_metric(regime_change_structural_break, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(regime_change_structural_break, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(regime_change_structural_break, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(regime_change_structural_break, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regime_change_structural_break, piton).
narrative_ontology:human_readable(regime_change_structural_break, "World3 Regime Change Structural Break Assumption").
narrative_ontology:topic_domain(regime_change_structural_break, "system_dynamics/industrial_ecology/sustainability_science").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regime_change_structural_break, original_world3_modeling_team).
narrative_ontology:constraint_beneficiary(regime_change_structural_break, limits_to_growth_advocacy_coalition).
narrative_ontology:constraint_victim(regime_change_structural_break, policy_makers_relying_on_projections).
narrative_ontology:constraint_victim(regime_change_structural_break, recalibration_research_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(regime_change_structural_break, recalibration_research_groups).
narrative_ontology:constraint_vindicates(regime_change_structural_break, system_dynamics_methodology_validity).
narrative_ontology:constraint_vindicates(regime_change_structural_break, feedback_loop_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The modeling team that developed World3 in 1972 set the methodological framework and structural assumptions. They acknowledged that feedback relationships would 'rearrange and reconnect' during collapse but maintained structural stability for tractability. They can work with alternative modeling frameworks without career penalty and benefit from the model's continued institutional influence and citation authority.
narrative_ontology:constraint_stakeholder(regime_change_structural_break, original_world3_team, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Organizations and researchers using World3 to communicate overshoot risks and advocate for sustainability policies. They benefit from the model's stable, well-known structure as a reference point for policy discourse. The structural stability assumption simplifies communication of core dynamics without requiring audiences to understand regime-dependent complexity. They can cite alternative models if World3 loses credibility.
narrative_ontology:constraint_stakeholder(regime_change_structural_break, limits_to_growth_advocacy_coalition, beneficiary,
    institutional, immediate, arbitrage, continental).

% Researchers updating World3 parameters with contemporary data face a genuine coordination problem: they need to maintain comparability with the original model and its extensive literature. However, they bear costs: career risk if they challenge foundational assumptions, resource investment in recalibration work that may be invalidated if regime shifts occur during the projection period. They can pursue alternative modeling frameworks but face sunk costs and loss of institutional legitimacy.
narrative_ontology:constraint_stakeholder(regime_change_structural_break, recalibration_research_groups, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(regime_change_structural_break, recalibration_research_groups, beneficiary).

% Decision-makers in government agencies, international organizations, and planning bodies who use World3-derived projections for long-term policy and infrastructure planning. They bear the full cost if regime shifts invalidate the model's projections during critical transition periods. Once institutional planning processes are built around World3 scenarios, switching to alternative frameworks is politically and administratively costly. They have no exit: the planning horizon extends decades, and the model's projections shape resource allocation decisions that cannot be easily reversed.
narrative_ontology:constraint_stakeholder(regime_change_structural_break, policy_makers_relying_on_projections, payer,
    powerless, biographical, trapped, global).

% Researchers developing agent-based models, hybrid system dynamics-agent-based frameworks, and other approaches that can represent structural change and regime shifts endogenously. They see the structural stability assumption as a temporary limitation of the system dynamics paradigm that will dissolve as computational capacity increases and alternative methods mature. They have mobile exit options: they can adopt new methodologies without career penalty and are building the frameworks that will replace World3's approach.
narrative_ontology:constraint_stakeholder(regime_change_structural_break, agent_based_modeling_coalition, observer,
    organized, generational, mobile, global).

% Economists, sociologists, and historians who study actual crisis dynamics and have documented regime-dependent shifts in socio-economic feedback parameters during historical perturbations (2008 financial crisis, COVID-19, 1970s oil shocks, Great Depression). Their empirical findings challenge the structural stability assumption but are often excluded from the World3 recalibration discourse, which focuses on parameter updating rather than structural revision. They would object to the assumption's persistence if they were more central to the modeling community's methodological debates.
narrative_ontology:constraint_stakeholder(regime_change_structural_break, empirical_crisis_analysts, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The structural stability assumption coordinates long-term modeling efforts by providing a tractable computational framework and enabling parameter recalibration without requiring complete model reconstruction. It allows researchers to update World3 with contemporary data while maintaining comparability with the original 1972 model and its extensive literature.
% TRANSFER_FUNCTION: The assumption transfers validity risk from the modeling community to policy makers: modelers can continue recalibration work and publish updates without addressing the regime-shift problem, while policy makers bear the cost if projections fail during critical transitions. It also transfers methodological authority from empirical crisis analysts (who document regime-dependent parameter shifts) to system dynamics practitioners (who maintain structural stability as a modeling convention).
% ABSENT_VOICES: Empirical crisis analysts — economists, sociologists, and historians who have documented regime-dependent shifts in feedback parameters during historical perturbations — are underrepresented in World3 recalibration discourse. The modeling community focuses on parameter updating rather than structural revision, marginalizing evidence that challenges the foundational assumption. These analysts would object that the assumption's persistence ignores empirical evidence from actual crises, but they lack institutional standing in the system dynamics modeling tradition.
% DISAPPEARANCE_RATIONALE: If the structural stability assumption disappeared, the World3 modeling framework would require fundamental reconstruction to represent regime-dependent parameter shifts and structural changes endogenously. Recalibration research would shift from parameter updating to developing hybrid or agent-based approaches. Policy makers would need to adopt alternative frameworks or develop scenario planning that explicitly accounts for regime uncertainty. The advocacy coalition would lose a stable reference point for communication. The constraint's disappearance would rearrange the entire institutional ecosystem around long-term sustainability modeling.
% FOUNDING_PROBLEM: The structural stability assumption was adopted in 1972 to make century-scale global modeling computationally tractable with available technology and to create a pedagogically simple framework for communicating overshoot dynamics. Representing regime-dependent structural changes endogenously would have required computational resources and methodological approaches (agent-based modeling, adaptive networks) that did not exist or were not mature at the time.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead: computational capacity has increased by orders of magnitude since 1972, and alternative modeling frameworks (agent-based models, hybrid approaches) that can represent structural change endogenously are now mature research tools. This status is corroborated by the agent-based modeling research community, computational social scientists, and reviews of modeling methodology in sustainability science journals (e.g., Filatova et al. 2013 in Environmental Modelling & Software on agent-based land-use models; Schlüter et al. 2017 in Ecology and Society on modeling social-ecological systems). The original World3 team's own acknowledgment that feedback relationships will 'rearrange during collapse' implicitly concedes that the assumption is a methodological convenience rather than an empirical necessity. However, the assumption persists: recalibration exercises continue to update parameters while preserving structural relationships, and World3 remains the dominant reference model in sustainability discourse despite the availability of alternatives.
narrative_ontology:disappearance_verdict(regime_change_structural_break, world_rearranges).
narrative_ontology:founding_problem_status(regime_change_structural_break, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL MODELING TEAM (PITON) — The structural stability assumption persists through institutional inertia and methodological tradition. The team acknowledges feedback relationships will 'rearrange and reconnect' during collapse but maintains the assumption for tractability. The constraint is maintained theatrically: recalibration exercises preserve the assumption not because it is empirically validated but because abandoning it would require rebuilding the entire modeling framework. Low effective extraction because the team has arbitrage exit options and benefits from the model's continued influence.
constraint_indexing:constraint_classification(regime_change_structural_break, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: RECALIBRATION RESEARCH GROUP (TANGLED ROPE) — Researchers updating World3 parameters face a genuine coordination problem (need to maintain comparability with original model) alongside extraction (career risk of challenging foundational assumptions, resource investment in recalibration that may be invalidated by regime shifts). Constrained exit: can pursue alternative modeling frameworks but face sunk costs and loss of institutional legitimacy. Mixed experience: the assumption enables their work while potentially undermining its validity.
constraint_indexing:constraint_classification(regime_change_structural_break, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLICY MAKERS (SNARE) — Decision-makers using World3-derived projections for long-term planning bear the full cost if regime shifts invalidate the model during the critical transition period. Trapped: once institutional planning is built around World3 scenarios, switching frameworks is politically and administratively costly. High effective extraction: the structural stability assumption extracts from policy reliability without providing exit options or acknowledging the regime-dependence risk.
constraint_indexing:constraint_classification(regime_change_structural_break, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: AGENT-BASED MODELING COALITION (SCAFFOLD) — Researchers developing agent-based and hybrid models see the structural stability assumption as a temporary limitation of the system dynamics paradigm. They are building alternative frameworks that explicitly model regime-dependent parameter shifts and emergent structural changes. Sunset logic: as computational capacity increases and agent-based methods mature, the need to hold feedback structure constant will dissolve. Mobile exit: can adopt new methodologies without career penalty. Low effective extraction because they see a clear transition path.
constraint_indexing:constraint_classification(regime_change_structural_break, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVOCACY COALITION (ROPE) — Organizations using World3 to communicate overshoot risks experience the structural stability assumption as coordination: it provides a stable, well-known reference point for policy discourse. The assumption's simplification enables communication of core dynamics (exponential growth, resource depletion, pollution accumulation) without requiring audiences to understand regime-dependent complexity. Net beneficiary: the assumption's persistence supports their advocacy work. Arbitrage exit: can cite alternative models if World3 loses credibility.
constraint_indexing:constraint_classification(regime_change_structural_break, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / METHODOLOGICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, some degree of structural stability assumption is inherent to any long-term dynamical model: perfect regime-shift prediction would require omniscience about future social reorganization. This perspective sees the constraint as an immutable limitation of forward-looking modeling itself. However, the structural data contradicts this: historical crises show empirically measurable regime shifts in feedback parameters, and alternative modeling frameworks (agent-based, hybrid) can represent structural change endogenously. The mountain classification is a false summit — naturalizing a methodological choice as an epistemological necessity.
constraint_indexing:constraint_classification(regime_change_structural_break, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regime_change_structural_break_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regime_change_structural_break, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regime_change_structural_break, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(regime_change_structural_break, TR),
    TR >= 0.70.

:- end_tests(regime_change_structural_break_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The structural stability assumption extracts primarily from policy makers who rely on projections that may be invalidated during the critical collapse transition. However, extraction is limited because: (1) the assumption was made for tractability, not rent-seeking; (2) alternative frameworks exist but face adoption barriers; (3) the model provides genuine value during growth phases when feedback structure is more stable. The modest increase over time (0.15 to 0.28) reflects growing institutional lock-in as World3 becomes embedded in planning processes despite accumulating evidence of regime-dependent parameter shifts. Suppression (0.35): Low-moderate. Barriers to adopting alternative frameworks include computational cost, institutional path dependence, and the model's pedagogical simplicity. However, suppression is not severe: researchers can and do develop alternatives, and the original team acknowledges the assumption's limitations. Theater ratio (0.68): Moderate-high. Recalibration exercises are substantially performative: they update parameter values (birth rates, resource depletion rates, pollution coefficients) while preserving the feedback structure that may itself shift during crises. The theater has increased steadily (0.35 to 0.68) as the gap between acknowledged limitations and continued use has widened. Historical crises (2008 financial, COVID-19) provide empirical evidence of regime-dependent parameter shifts, yet recalibrations continue to assume structural stability. Accessibility collapse (0.42): Moderate. Alternative modeling frameworks exist and are accessible to researchers, but institutional adoption faces barriers. The constraint is not a natural law — it is a methodological choice — but switching costs are real. Resistance (0.58): Moderate-high. The assumption faces substantial resistance from agent-based modelers, empirical economists studying crisis dynamics, and policy analysts who have observed projection failures during historical perturbations. The resistance is higher than typical for a piton because the assumption's failure mode is empirically observable rather than purely theoretical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a methodological assumption can appear as different constraint types depending on the observer's structural position. The original modeling team sees a piton: the assumption persists through institutional inertia and methodological tradition, maintained theatrically because abandoning it would require rebuilding the framework. Recalibration researchers see tangled rope: genuine coordination needs (comparability) mixed with extraction (invalidation risk). Policy makers see a snare: they are trapped by institutional planning processes and bear the full cost of potential projection failures. The advocacy coalition sees rope: the assumption provides a stable reference point for communication. The agent-based modeling coalition sees scaffold: the constraint is temporary, with a sunset as alternative frameworks mature. The analytical observer risks seeing mountain: structural stability assumptions are inherent to long-term modeling. But this is a false summit — historical crises provide empirical evidence of regime-dependent parameter shifts, and alternative frameworks can represent structural change endogenously. The constraint naturalizes a methodological choice as an epistemological necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The original modeling team and advocacy coalition are net beneficiaries: they benefit from the model's continued influence and institutional authority. The modeling team has arbitrage exit options (can work with alternative frameworks) and low directionality toward the constraint. The advocacy coalition uses the model as a communication tool and can cite alternatives if World3 loses credibility. Recalibration research groups occupy a mixed position: they face genuine coordination needs (maintaining comparability with the original model) but also bear costs (career risk of challenging foundational assumptions, resource investment potentially invalidated by regime shifts). Their constrained exit options and mixed beneficiary/victim status produce moderate directionality. Policy makers are primary victims: they bear the full cost if regime shifts invalidate projections during the critical transition period, and they are trapped by institutional planning processes built around World3 scenarios. Their powerless position and trapped exit options produce high directionality toward the constraint. The agent-based modeling coalition has mobile exit options and is building alternatives, producing low directionality. The analytical observer's mountain classification is a false summit: the constraint is not an immutable epistemological limit but a methodological choice that could be replaced by frameworks representing structural change endogenously.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by revealing that the structural stability assumption's persistence is not due to empirical validation but to institutional inertia and methodological path dependence. The assumption was originally justified by computational tractability and the need for a pedagogically simple model. Over time, as computational capacity increased and historical crises provided evidence of regime-dependent parameter shifts, the assumption's empirical basis weakened while its institutional entrenchment strengthened. The piton classification captures this: the constraint is maintained theatrically, through recalibration exercises that update parameters while preserving the structural relationships that may themselves be regime-dependent. The theater ratio (0.68) reflects the gap between acknowledged limitations (the authors state feedback relationships will 'rearrange during collapse') and continued practice (recalibrations assume structural stability). The constraint is not pure extraction (snare) because it provides genuine coordination value during growth phases and was not designed for rent-seeking. It is not pure coordination (rope) because it extracts from policy makers who bear the cost of potential projection failures. It is a degraded coordination mechanism maintained through institutional inertia — the canonical piton pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_shift_magnitude_threshold,
    'What magnitude of parameter shift during crisis constitutes a regime change that invalidates recalibration, versus normal variation within the model''s tolerance?',
    'Quantitative analysis of feedback parameter stability across historical crises (2008 financial, COVID-19, 1970s oil shocks, Great Depression). Define threshold as the shift magnitude beyond which World3 projections diverge >20% from observed trajectories when recalibrated pre-crisis.',
    'If threshold is low (parameters shift substantially during all major crises): structural stability assumption is empirically untenable, piton classification confirmed. If threshold is high (parameters remain stable): assumption is more defensible, classification shifts toward rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_shift_magnitude_threshold, empirical, 'Parameter shift magnitude threshold for regime change classification').

omega_variable(
    collapse_proximity_effect,
    'Do feedback parameters remain stable during growth phases but shift during collapse transitions, or do they shift during all major perturbations regardless of growth/collapse context?',
    'Comparative analysis: parameter stability during growth-phase crises (2008 recovery, post-WWII reconstruction) versus collapse-phase crises (Soviet dissolution, Bronze Age collapse). If shifts are collapse-specific, World3''s acknowledgment that relationships will ''rearrange during collapse'' is a direct admission of the assumption''s failure at the critical moment.',
    'If collapse-specific: the model''s validity window ends exactly when its predictions matter most, confirming extractive piton classification. If perturbation-general: the assumption fails broadly, not just at collapse, suggesting snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_proximity_effect, empirical, 'Whether regime shifts are collapse-specific or perturbation-general').

omega_variable(
    alternative_framework_maturity,
    'Are agent-based and hybrid models that represent structural change endogenously mature enough to replace World3 for policy guidance, or do they remain research tools?',
    'Assessment of agent-based model validation track record, computational tractability for century-scale projections, and institutional adoption by policy bodies (IPCC, national planning agencies). Survey of modeling practitioners on readiness for operational use.',
    'If mature: scaffold perspective is structural reality, sunset is imminent. If immature: scaffold perspective is aspirational, piton persists through lack of alternatives rather than pure inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_maturity, empirical, 'Maturity and operational readiness of alternative modeling frameworks').

omega_variable(
    institutional_lock_in_strength,
    'Is World3''s persistence due to genuine methodological advantages (tractability, transparency, pedagogical value) or primarily to institutional path dependence and sunk investment?',
    'Comparative analysis of modeling framework adoption patterns: do institutions switch to alternatives when available, or do they maintain World3 despite acknowledged limitations? Interview data on decision factors for framework choice. Quantify citation patterns: is World3 cited for its methodology or for its historical authority?',
    'If path dependence dominates: piton classification confirmed, theater ratio is accurate. If methodological advantages dominate: classification shifts toward rope (genuine coordination value) or tangled_rope (mixed coordination and extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_lock_in_strength, conceptual, 'Whether persistence is methodological or institutional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regime_change_structural_break, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regime_break_theater_1972, regime_change_structural_break, theater_ratio, 0, 0.35).
narrative_ontology:measurement(regime_break_theater_1982, regime_change_structural_break, theater_ratio, 10, 0.48).
narrative_ontology:measurement(regime_break_theater_1992, regime_change_structural_break, theater_ratio, 20, 0.55).
narrative_ontology:measurement(regime_break_theater_2002, regime_change_structural_break, theater_ratio, 30, 0.62).
narrative_ontology:measurement(regime_break_theater_2012, regime_change_structural_break, theater_ratio, 40, 0.66).
narrative_ontology:measurement(regime_break_theater_2026, regime_change_structural_break, theater_ratio, 54, 0.68).

% Extraction over time
narrative_ontology:measurement(regime_break_extract_1972, regime_change_structural_break, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(regime_break_extract_1982, regime_change_structural_break, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(regime_break_extract_1992, regime_change_structural_break, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(regime_break_extract_2002, regime_change_structural_break, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(regime_break_extract_2012, regime_change_structural_break, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(regime_break_extract_2026, regime_change_structural_break, base_extractiveness, 54, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regime_change_structural_break, information_standard).

% DUAL FORMULATION NOTE:
% The regime change structural break assumption is a methodological constraint within the World3 modeling framework. It is distinct from the substantive claims about resource depletion, pollution accumulation, or population overshoot that the model generates. Alternative formulations (agent-based models, hybrid approaches) represent the same substantive dynamics without requiring structural stability assumptions, demonstrating that the constraint is a feature of the modeling methodology rather than the underlying system being modeled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
