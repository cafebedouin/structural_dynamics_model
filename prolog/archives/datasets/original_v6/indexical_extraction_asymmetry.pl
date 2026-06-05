% ============================================================================
% CONSTRAINT STORY: indexical_extraction_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indexical_extraction_asymmetry, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indexical_extraction_asymmetry
 *   human_readable: Indexical Extraction Asymmetry in Social Constraint Theory
 *   domain: social_theory/power_asymmetry/agency_depletion
 *
 * SUMMARY:
 *   Indexical extraction asymmetry describes the structural phenomenon where
 *   identical enforcement mechanisms produce radically different experiences
 *   based solely on the observer's index position (power, time horizon, exit
 *   options, scope). This is not a claim about specific constraints but a
 *   meta-constraint about how constraint classification itself varies with
 *   structural position. The constraint is the ASYMMETRY — the fact that χ
 *   (effective extraction) varies systematically with power position even
 *   when ε (base extraction) and the enforcement mechanism are held constant.
 *   A workplace attendance policy, a legal compliance requirement, a social
 *   norm, or a platform terms-of-service can simultaneously coordinate for
 *   powerful agents (who experience low χ due to exit options and resources)
 *   and extract from powerless agents (who experience high χ due to trapped
 *   exit and resource scarcity). The constraint story models this structural
 *   delta as a tangled rope because the coordination function is genuine (the
 *   enforcement mechanism does solve collective action problems) AND the
 *   extraction is genuine (agency depletion is asymmetrically distributed).
 *   The asymmetry is not a bug — it is the mechanism by which coordination
 *   costs are allocated.
 *
 * KEY AGENTS:
 *   - Powerless Agents Without Exit Options: Primary victim (powerless/trapped) — experience pure extraction; agency depletion with no coordination benefit; cannot exit or organize
 *   - Powerful Agents With Exit Options: Primary beneficiary (powerful/arbitrage) — experience coordination; reduced transaction costs and strategic advantage; can exit if unfavorable
 *   - Moderate Agents With Constrained Exit: Mixed position (moderate/constrained) — experience both coordination benefit and extraction cost; can perceive asymmetry but cannot easily escape
 *   - Institutional Designers: Beneficiary (institutional/arbitrage) — designed or maintain the constraint; measure success by aggregate compliance, not cost distribution; asymmetry is invisible from this position
 *   - Organized Coalitions: Secondary victim with agency (organized/mobile) — collective organization reveals asymmetry; can contest but not eliminate extraction
 *   - Analytical Observer: Meta-position (analytical/analytical) — sees the structural coupling of coordination and extraction; recognizes χ variance as the mechanism, not a measurement error
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indexical_extraction_asymmetry, 0.58).
domain_priors:suppression_score(indexical_extraction_asymmetry, 0.68).
domain_priors:theater_ratio(indexical_extraction_asymmetry, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indexical_extraction_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(indexical_extraction_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(indexical_extraction_asymmetry, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indexical_extraction_asymmetry, tangled_rope).
narrative_ontology:human_readable(indexical_extraction_asymmetry, "Indexical Extraction Asymmetry in Social Constraint Theory").
narrative_ontology:topic_domain(indexical_extraction_asymmetry, "social_theory/power_asymmetry/agency_depletion").

domain_priors:requires_active_enforcement(indexical_extraction_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indexical_extraction_asymmetry, powerful_agents_with_exit_options).
narrative_ontology:constraint_victim(indexical_extraction_asymmetry, powerless_agents_without_exit_options).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS/TRAPPED AGENT (SNARE) — Experiences the constraint as pure extraction. Same enforcement mechanism that coordinates powerful agents' behavior extracts from this agent with no coordination benefit. Cannot exit, cannot organize, bears full cost of compliance. The constraint depletes agency resources (time, attention, cognitive capacity) without providing reciprocal coordination value.
constraint_indexing:constraint_classification(indexical_extraction_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MODERATE/CONSTRAINED AGENT (TANGLED ROPE) — Experiences mixed coordination and extraction. The constraint provides some coordination benefit (predictable rules, shared expectations) but also extracts significantly through compliance costs and limited exit options. Can perceive the asymmetry but cannot easily escape it. Agency depletion is real but not total.
constraint_indexing:constraint_classification(indexical_extraction_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POWERFUL/ARBITRAGE AGENT (ROPE) — Experiences the constraint as coordination. Same enforcement mechanism that extracts from powerless agents provides this agent with predictable structure, reduced transaction costs, and strategic advantage. Can exit if the constraint becomes unfavorable. The coordination benefit exceeds any extraction cost.
constraint_indexing:constraint_classification(indexical_extraction_asymmetry, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL/ARBITRAGE AGENT (ROPE) — Designed or maintains the constraint. Experiences it as pure coordination mechanism solving collective action problems. The asymmetric extraction is invisible from this position because the institution measures coordination success by aggregate compliance, not by distribution of costs. Can modify or exit the constraint at will.
constraint_indexing:constraint_classification(indexical_extraction_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED/MOBILE AGENT (TANGLED ROPE) — Collective organization reveals the asymmetry. Sees both the genuine coordination function (which justifies the constraint's existence) and the extractive mechanism (which concentrates costs on powerless members). Has enough power to negotiate modifications but not enough to eliminate the extraction entirely. Agency depletion is visible and contestable.
constraint_indexing:constraint_classification(indexical_extraction_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the constraint as structurally hybrid. The same enforcement mechanism genuinely coordinates (solves collective action problems, reduces transaction costs) AND asymmetrically extracts (depletes agency resources from powerless agents while subsidizing powerful agents). The asymmetry is not a bug but a structural feature: χ variance across power positions is the mechanism by which coordination costs are distributed. This is the canonical tangled rope — irreducible coordination-extraction coupling.
constraint_indexing:constraint_classification(indexical_extraction_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indexical_extraction_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indexical_extraction_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indexical_extraction_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indexical_extraction_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(indexical_extraction_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The base extraction reflects the average agency depletion across all power positions, but this average masks extreme variance. Powerless agents experience ε_effective ≈ 0.85+ (snare-level extraction), while powerful agents experience ε_effective ≈ 0.15 (rope-level coordination). The base ε of 0.58 is the structural average, but the ASYMMETRY is the constraint being modeled. Suppression (0.68): High. Powerless agents face significant barriers to exit (economic dependency, legal constraints, social penalties, lack of alternatives). Powerful agents face low suppression (can exit, can negotiate exceptions, can afford non-compliance). The suppression metric reflects the structural barriers faced by the victim class. Theater ratio (0.45): Moderate and rising. Some of the enforcement mechanism is genuinely functional (solves coordination problems), but an increasing proportion is performative (compliance rituals that signal submission rather than achieve coordination). The rise over time reflects Goodhart drift — as powerful agents learn to game the system, enforcement shifts from functional to theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal and diagnostic. Powerless agents see snare (pure extraction, no coordination benefit, no exit). Powerful agents see rope (pure coordination, minimal cost, exit available). Moderate agents see tangled rope (mixed). Institutional designers see rope (coordination success by their metrics). Organized coalitions see tangled rope (asymmetry is visible and contestable). The analytical observer sees tangled rope (irreducible coupling). The gap reveals that 'coordination vs extraction' is not an objective property of the constraint but an indexical property of the observer's structural position. The constraint coordinates AND extracts, and the ratio depends on where you stand. This is the core insight of indexical classification: the presheaf over observation sites IS the constraint, not any single perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the structural relationship each agent has to the constraint. Powerless/trapped agents are victims with no exit — high d (≈0.95) produces high f(d) (≈1.42), yielding high χ (snare-level extraction). Powerful/arbitrage agents are beneficiaries with exit options — low d (≈0.05) produces negative f(d) (≈-0.12), yielding negative χ (they are subsidized by the constraint). Moderate/constrained agents are mixed — moderate d (≈0.55) produces moderate f(d) (≈0.75), yielding moderate χ (tangled rope experience). The institutional designer is a beneficiary with arbitrage exit — very low d (≈0.00) produces maximum negative f(d) (≈-0.12). The organized coalition is a victim with mobile exit — moderate-high d (≈0.60) produces moderate-high f(d) (≈0.85), but organization reduces experienced extraction. The analytical observer uses canonical d for analytical power (≈0.72), producing f(d) ≈ 1.15, but this is a measurement position, not an experienced position. The key insight: identical ε, identical enforcement mechanism, but χ varies by a factor of 10+ across power positions. This variance IS the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the coordination-extraction distinction is indexical, not objective. The powerful agent's rope classification is not false — they genuinely experience coordination. The powerless agent's snare classification is not false — they genuinely experience extraction. The analytical observer's tangled rope classification is not a compromise — it is the recognition that the constraint has irreducible dual character. The mandatrophy question 'Is this coordination or extraction?' presupposes a single objective answer. The indexical framework rejects the presupposition: the constraint is BOTH, and which aspect dominates depends on the observer's power position, exit options, and time horizon. The asymmetry is not a measurement error or a framing choice — it is the structural mechanism by which coordination costs are distributed. Calling it 'just coordination' erases the powerless agent's experience. Calling it 'just extraction' erases the genuine collective action problem being solved. Tangled rope is the only classification that preserves both truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'What proportion of the constraint''s enforcement mechanism is necessary for coordination vs extractive overhead?',
    'Comparative analysis of alternative coordination mechanisms with different power distributions; measurement of coordination success vs agency depletion across constraint variants',
    'If coordination necessity is high (>0.70): constraint is closer to rope with unfortunate but necessary asymmetry. If low (<0.40): constraint is closer to snare with coordination theater masking extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Proportion of enforcement mechanism necessary for coordination').

omega_variable(
    exit_option_accessibility,
    'Are exit options structurally determined by the constraint or by pre-existing power asymmetries?',
    'Longitudinal tracking of agents who change power positions; analysis of whether exit options change with power or remain fixed by external factors (wealth, social capital, legal status)',
    'If exit options are constraint-determined: the constraint creates its own power asymmetry (snare mechanism). If pre-existing: the constraint exploits but does not create the asymmetry (tangled rope leveraging external structure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_accessibility, empirical, 'Whether exit options are endogenous or exogenous to constraint').

omega_variable(
    agency_depletion_reversibility,
    'Is agency depletion from constraint compliance reversible when the constraint is removed or exit is achieved?',
    'Post-exit longitudinal studies measuring cognitive capacity, decision-making quality, and resource availability after agents escape the constraint; comparison with matched controls who never experienced the constraint',
    'If reversible: extraction is temporary and bounded (lower effective ε). If irreversible or slow-reversing: extraction includes permanent agency damage (higher effective ε, potential reclassification toward snare for trapped agents).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_depletion_reversibility, empirical, 'Reversibility of agency depletion after constraint removal').

omega_variable(
    coordination_benefit_distribution,
    'Do powerless agents receive ANY coordination benefit, or is the benefit entirely concentrated on powerful agents?',
    'Measurement of transaction cost reduction, predictability gains, and collective action success rates stratified by power position; identification of coordination benefits that accrue to all participants vs those captured by powerful agents only',
    'If powerless agents receive measurable coordination benefit: tangled rope classification confirmed (asymmetric but not zero-sum). If zero coordination benefit to powerless: reclassify toward snare (coordination is theater for extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_benefit_distribution, empirical, 'Distribution of coordination benefits across power positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indexical_extraction_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idx_asym_tr_t0, indexical_extraction_asymmetry, theater_ratio, 0, 0.3).
narrative_ontology:measurement(idx_asym_tr_t3, indexical_extraction_asymmetry, theater_ratio, 3, 0.35).
narrative_ontology:measurement(idx_asym_tr_t6, indexical_extraction_asymmetry, theater_ratio, 6, 0.4).
narrative_ontology:measurement(idx_asym_tr_t10, indexical_extraction_asymmetry, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(idx_asym_be_t0, indexical_extraction_asymmetry, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(idx_asym_be_t3, indexical_extraction_asymmetry, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(idx_asym_be_t6, indexical_extraction_asymmetry, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(idx_asym_be_t10, indexical_extraction_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indexical_extraction_asymmetry, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is a meta-constraint about indexical classification itself. It does not decompose into separate stories because the asymmetry (χ variance across power positions) is the single structural phenomenon being modeled. Alternative formulations (e.g., 'workplace attendance policy', 'platform terms of service', 'legal compliance requirement') are instantiations of this pattern, not separate constraints. Each instantiation would be its own constraint story with its own ε, but they would all exhibit this indexical extraction asymmetry structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
