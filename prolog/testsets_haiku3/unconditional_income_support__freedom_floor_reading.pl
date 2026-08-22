% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   Unconditional income support (UIS) is a contested policy arrangement that
 *   can be read through multiple normative and empirical lenses. This
 *   constraint story instantiates the FREEDOM-FLOOR READING: UIS as removing
 *   coercive pressure from labor-market participation, enabling genuine
 *   choice about work, care, art, and community engagement. The reading
 *   claims beneficiaries (precarious workers, caregivers, artists, abuse
 *   survivors) experience autonomy gain with no victims (Pareto improvement
 *   via efficiency). It claims the arrangement is a rope (genuine
 *   coordination mechanism solving the collective coercion problem) with
 *   moderate extractiveness. This reading is in direct empirical contest with
 *   the dependency-trap reading (which claims labor-supply distortion and
 *   dependency deepening) and in conceptual contest with the
 *   universality-paradox reading (which claims incompatible implementation
 *   paths mask the shared fiscal outcome). This story generates constraint
 *   classification from the freedom-floor reading's structural commitments,
 *   not from an attempt to adjudicate the dispute. The claim/metric gap is
 *   deliberate: the reading is CLAIMED as rope (autonomy-enabling
 *   coordination) while resistance measurements of 0.45 reflect real
 *   political and empirical contestation — the engine computes the gap; the
 *   story does not reconcile it.
 *
 * KEY AGENTS:
 *   - Precarious workers: power-poor, constrained by labor-market desperation; the floor converts acceptance into choice.
 *   - Caregivers: identity-locked into unpaid care; the floor decouples survival from market participation.
 *   - Artists: moderate power, mobile exit options; the floor permits creative work without market desperation.
 *   - Abuse survivors: trapped by economic dependence; the floor severs the lock mechanism.
 *   - Wage labor market: the institutional structure under contest — the reading reframes it as coercive without the floor.
 *   - Dependency-trap advocates: excluded from this reading's framing; their empirical and normative claims are the primary opposition.
 *   - Analytical observer: external seat examining the structural commitments and empirical grounds of the reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.32).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.15).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, 'fcc04a02-2401-4319-9f72-bc6e2101c96d').
narrative_ontology:cs_kernel_codification('fcc04a02-2401-4319-9f72-bc6e2101c96d', distributed).
narrative_ontology:cs_authority_grounding('fcc04a02-2401-4319-9f72-bc6e2101c96d', diffuse_epistemic).
narrative_ontology:cs_reading_relation('fcc04a02-2401-4319-9f72-bc6e2101c96d', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('fcc04a02-2401-4319-9f72-bc6e2101c96d', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('fcc04a02-2401-4319-9f72-bc6e2101c96d', foundational, labor_market_coercion_via_survival_desperation).
narrative_ontology:cs_axiom_status(labor_market_coercion_via_survival_desperation, holdable).
narrative_ontology:cs_axiom_grounding('fcc04a02-2401-4319-9f72-bc6e2101c96d', labor_market_coercion_via_survival_desperation, empirically_contingent).
narrative_ontology:cs_axiom('fcc04a02-2401-4319-9f72-bc6e2101c96d', foundational, autonomy_enabled_by_removing_external_coercion).
narrative_ontology:cs_axiom_status(autonomy_enabled_by_removing_external_coercion, holdable).
narrative_ontology:cs_axiom_grounding('fcc04a02-2401-4319-9f72-bc6e2101c96d', autonomy_enabled_by_removing_external_coercion, deontological).
narrative_ontology:cs_axiom('fcc04a02-2401-4319-9f72-bc6e2101c96d', secondary, unconditional_income_enables_pareto_improvement).
narrative_ontology:cs_axiom_status(unconditional_income_enables_pareto_improvement, holdable).
narrative_ontology:cs_axiom_grounding('fcc04a02-2401-4319-9f72-bc6e2101c96d', unconditional_income_enables_pareto_improvement, empirically_contingent).
narrative_ontology:cs_reference_frame('fcc04a02-2401-4319-9f72-bc6e2101c96d', labor_market_with_coercive_wage_desperation).
narrative_ontology:cs_drift_state('fcc04a02-2401-4319-9f72-bc6e2101c96d', contemporary_post_pandemic_welfare_expansion, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('fcc04a02-2401-4319-9f72-bc6e2101c96d', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers in gig, seasonal, or low-wage work whose labor market position is fragile. The income floor removes the desperate urgency to accept any wage or condition; they can refuse exploitative offers, negotiate for better terms, or transition into unpaid caregiving, skill development, or artistic work. The constraint's benefit is exit optionality — turning constrained acceptance into genuine choice.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, mobile, national).

% Primarily women performing essential but unpaid care labor (children, elderly parents, community members). Current labor market forces them to choose between care work (economically invisible) and wage work (incompatible with care). The income floor permits them to sustain themselves and their dependents without surrendering care responsibility to the market. The constraint removes the false binary.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, caregivers, beneficiary,
    powerless, biographical, constrained, national).

% Writers, musicians, visual artists, and performers whose work is economically irregular and market-dependent. The floor provides survival security during creative development and allows them to resist commodification pressure (producing work for what will sell rather than what they value creating). They remain dependent on market validation for income above the floor, but not for existence.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists, beneficiary,
    moderate, biographical, mobile, national).

% Individuals in exploitative relationships (intimate partner abuse, labor trafficking, family coercion) where economic dependence is the primary lock. The income floor severs the economic necessity of remaining; they can leave without destitution. The constraint directly addresses the structural coercion mechanism.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, biographical, constrained, national).

% The institutional arrangement through which labor is bought and sold. Under this reading, the income floor makes it a voluntary market rather than a coercive one: workers can refuse bad terms without starvation threat. From the perspective of this reading, removing coercion improves the market's function — voluntary exchange generates better allocation than desperation-driven acceptance.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, wage_labor_market, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__freedom_floor_reading, wage_labor_market).

% The financing base for the income floor, though the reading does not declare them as victims. This reading characterizes the redistribution as enabling Pareto-improvement (nobody worse off) through labor-market efficiency gains and reduced crisis costs. Empirical contention about whether efficiency gains and cost reduction offset the tax burden is the driver of policy dispute, not a structural feature the reading itself claims.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpayers_funding_redistribution, observer,
    organized, biographical, mobile, national).

% Policy voices holding the sibling reading: they argue the income floor distorts incentives, creates dependency, and crowds out targeted aid. They are not in the conversation shaping this reading's implementation but are the primary political opposition to it. Their empirical claims about labor supply response directly contest this reading's core premise.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, dependency_trap_reading_advocates, excluded,
    powerful, generational, mobile, national).

% External analyst examining the constraint: comparing the freedom-floor reading against sibling readings, identifying empirical questions and theoretical commitments, assessing whether the data from Alaska, Kenya, and experimental pilots support the autonomy-enabling framing.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts labor market from a coercive system (work or starve) into a voluntary one (work or sustain yourself at a modest floor). Solves the collective-action problem of wage depression driven by survival desperation: when workers have an outside option, wage competition shifts from desperation-driven bidding to quality-and-preference-driven selection. Enables unpaid labor (care, art, community work) to function as genuine choice rather than default-for-the-powerless.
% TRANSFER_FUNCTION: Moves regular income from the tax base (broadly distributed) to individuals below the floor threshold, removing income inequality below the survival line. Simultaneously transfers agency: from the labor market's desperation logic to individual choice about where labor goes.
% ABSENT_VOICES: Sibling reading advocates (dependency-trap reading and universality-paradox reading) are excluded from the conversation constitutive of this reading's framing. They would contest the core empirical claim (labor supply effects are minimal) and the core normative claim (autonomy is enabled rather than disabled by decoupling income from work). Voices from within precarious-work constituencies split along the same lines.
% DISAPPEARANCE_RATIONALE: If the unconditional income floor disappeared, precarious workers would revert to desperation-driven wage acceptance; caregivers would face the forced choice between care and survival; artists would exit artistic work for wage work at higher rates; abuse survivors would lose the economic exit route. Labor supply to low-wage sectors would increase (fewer workers with exit options), wages would fall, and care-work deficits would grow. The reorganization would concentrate economic coercion where the floor had lifted it.
% FOUNDING_PROBLEM: Labor markets in capitalist economies generate a choice architecture for the powerless: work at offered wages or face destitution. This choice is not free — it is coerced by necessity. Unpaid labor essential to society (care, art, community participation) cannot function as genuine choice within this architecture. The founding problem is the structural coercion of the labor market itself.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying job refusal rates, wage-setting experiments, and qualitative research on precarious work confirm that desperation measurably shapes labor acceptance. Ethnographic work by anthropologists and sociologists documents how caregiving, artistic, and community participation patterns shift when economic necessity is removed (witness: artists with grants or inherited income produce differently than those without). The testimony of abuse survivors and evidence from separation-violence literature document that economic dependence is a primary lock mechanism. Corroboration comes from outside the beneficiary set — from neutral academic research communities, not from advocates of the reading.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.32) under this reading because the income floor represents a genuine coordination benefit: it removes the zero-sum competition driven by survival desperation and enables Pareto improvement (workers gain autonomy, employers gain better-quality voluntary labor, care and art gain social participation). The reading does not claim extraction from any party — the financiers are observers, not victims, because the reading asserts efficiency gains and cost reduction offset the tax burden. Suppression is low (0.15) because the mechanism operates through enabling choice, not through coercion. Theater is minimal (0.08) because the coordination function is transparent: remove desperation, enable genuine choice. Resistance is substantial (0.45) not because the reading is internally incoherent but because the empirical claims about labor-supply elasticity and the normative claims about autonomy are hotly disputed by rival readings. The temporal series is flat because, under this reading, the constraint's structural properties do not drift — the coordination logic remains steady even as political contestation fluctuates. The flat line is a statement about the reading's internal coherence, not about historical politics.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (precarious workers, caregivers, artists, abuse survivors), the constraint removes coercion and enables autonomy — a fundamental shift in choice architecture. From the dependency-trap reading seat, the same constraint distorts incentives and creates dependency — a fundamental inversion of the beneficiary-seat experience. From the universality-paradox seat, the constraint masks incompatible implementation pathways that render its autonomy promise illusory or narrowly available. The engine computes these divergences from the structural data (power, exit_options, beneficiary/victim status); they are NOT reconciled by claiming a single true type. The reading declares what it structurally entails (rope, moderate extraction, no victims); the engine measures how other seats experience that same structure. The gap is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers, caregivers, artists, and abuse survivors all benefit from the floor via expanded exit options (d shifts toward full beneficiary end). The wage-labor market (treated as a non-agent institution that is vindicated by the reading's operation) benefits from the conversion of coercive into voluntary exchange. Taxpayers are declared as observers, not victims, because the reading claims efficiency gains and cost reduction make redistribution a net wash. Dependency-trap advocates are excluded because their reading is structurally incompatible within a single framework — the same policy is either autonomy-enabling or incentive-distorting depending on which empirical claims about labor supply are true. The reading does not declare directionality for excluded parties; their d values would be computed from a different constraint story (the dependency-trap reading, instantiated in its own file).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (labor-market coercion through survival desperation) is live under this reading — precarious work, care deficits, artistic underfunding, and abuse traps all persist and are attributable to the founding problem. The disappearance verdict (world_rearranges) is consistent with a live founding problem: removing the floor would restore desperation-driven labor markets and care/art deficits. There is no mandatrophy signal. The reading's internal coherence holds: it identifies a problem, proposes a mechanism that addresses it, and predicts observable consequences if the mechanism is removed. The contestation is empirical (do labor-supply effects actually remain minimal? do efficiency gains materialize?) and normative (is autonomy the right evaluative frame, or is dependency the frame that fits?), not structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity_empirics,
    'Do precarious workers reduce labor supply when provided unconditional income support, or do they reallocate labor toward preferred work (care, art, skill development) while maintaining aggregate work hours?',
    'Experimental and quasi-experimental evidence from Alaska Permanent Fund, Kenya GiveDirectly RCT, Finland pilot, and Stockton (CA) pilot. Labor supply response can be measured directly; reallocation patterns can be inferred from survey and administrative data.',
    'If labor supply contracts sharply, the reading''s claim of minimal extraction and maintained market function is undermined — the constraint may be extractive rather than coordinative. If labor reallocates without contracting aggregate supply, the reading holds and the coordination mechanism is validated. This is the empirical crux of the freedom-floor vs. dependency-trap dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_empirics, empirical, 'Labor-supply response to unconditional income support: contraction vs. reallocation.').

omega_variable(
    autonomy_as_evaluative_frame,
    'Is autonomy (capacity for self-directed choice) the appropriate evaluative frame for assessing unconditional income support, or does the same policy represent dependence (income decoupled from work, making the individual dependent on collective provision)?',
    'Conceptual clarification: autonomy and dependence are not empirical opposites but frames that can coexist. Autonomy is enabled by the removal of external coercion; dependence is a fact about reliance on collective provisioning. The question is whether the reading can coherently hold both — whether decoupling income from work removes coercion without introducing dependence, or whether the two are structurally linked. Ethnographic work on how beneficiaries experience the arrangement provides phenomenological evidence; normative philosophy (freedom-as-non-domination literature) provides conceptual clarity.',
    'If autonomy and dependence are structurally separable (you can remove coercion without introducing dependence), the freedom-floor reading stands. If they are inseparable (dependence on collective provision is itself a form of domination or loss of autonomy), the reading must be reframed or partially foreclosed by the dependency-trap reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_as_evaluative_frame, conceptual, 'Whether autonomy (freedom from coercion) and dependence (reliance on collective provision) are separable evaluative frames for the same policy.').

omega_variable(
    care_work_and_identity_lock,
    'Does providing income support for care work (caregivers, parents, community workers) remove identity-lock through economic coercion, or does it reinforce identity-lock by making care work economically sustainable without market validation?',
    'Qualitative research on how caregivers experience identity and choice when income is decoupled from care labor; comparative analysis with markets where care is commodified (child care as paid work). If caregivers report expanded sense of choice and reduced desperation, the reading holds. If they report reinforced expectation that care is ''what they do'' (identity solidification), the reading may partially foreclose.',
    'If the income floor genuinely expands caregiver choice — including choice to exit care into wage work or other pursuits — the reading''s autonomy claim holds. If it stabilizes care identity by making unpaid care economically viable, it may represent a different structure (identity-locked coordination rather than autonomy-enabling rope). This affects the reading''s internal coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_work_and_identity_lock, empirical, 'Whether income support for caregiving enables autonomy or reinforces identity-locked care work.').

omega_variable(
    kernel_contest_coexistence,
    'Can the freedom-floor, dependency-trap, and universality-paradox readings coexist as live policy positions held by different parties, or does one reading logically foreclose the others within a single policy framework?',
    'Structural analysis of the readings'' core premises: do they make incompatible claims about the same facts (labor supply, coercion, autonomy), or do they make compatible claims from different evaluative frames? Foreclosure occurs only when one reading''s core premise logically contradicts another''s within a shared framework.',
    'If the readings coexist (each live position from a different normative frame or empirical assumption), they are three constraint stories linked by network.affects_constraints and instantiate genuine policy ambiguity. If one forecloses another (one reading''s core premise logically negates another''s), the cs_structure.reading_relations should reflect that; the foreclosed reading may still exist as a constraint but loses internal coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_coexistence, conceptual, 'Whether sibling readings coexist as live policy positions or whether one structurally forecloses another.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__freedom_floor_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__freedom_floor_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(unco_tr_t25, unconditional_income_support__freedom_floor_reading, theater_ratio, 25, 0.08).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__freedom_floor_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__freedom_floor_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(unco_be_t25, unconditional_income_support__freedom_floor_reading, base_extractiveness, 25, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__freedom_floor_reading, suppression_requirement, 5, 0.14).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__freedom_floor_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__freedom_floor_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__freedom_floor_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement(unco_su_t25, unconditional_income_support__freedom_floor_reading, suppression_requirement, 25, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% The 'unconditional_income_support' kernel admits multiple structurally distinct constraint readings. The freedom-floor reading (this story) characterizes UIS as removing labor-market coercion and enabling autonomy (rope, moderate extraction, no victims, Pareto improvement). The dependency-trap reading (sibling) characterizes the same policy as incentive-distorting subsidy (snare or tangled rope, higher extraction, victims among non-recipients, negative sum). The universality-paradox reading (sibling) characterizes UIS as politically ambiguous, masking incompatible implementation paths behind cross-ideological appeal. Each reading is instantiated as a separate constraint story with its own epsilon, beneficiary/victim structure, and claimed type. They are linked via network.affects_constraints to enable kernel-level analysis: which reading's structural commitments are empirically grounded? Do the readings foreclose or coexist?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
