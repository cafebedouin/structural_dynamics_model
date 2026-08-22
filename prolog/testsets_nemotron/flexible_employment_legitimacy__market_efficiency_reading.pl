% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__market_efficiency_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the market_efficiency_reading of the
 *   flexible_employment_legitimacy kernel. It treats flexible employment —
 *   gig work, platform-mediated labor, on-demand staffing — as a legitimate
 *   market-clearing mechanism that matches heterogeneous labor supply to
 *   heterogeneous demand, maximizing worker autonomy through schedule
 *   flexibility and lowering search costs for employers. The reading asserts
 *   that platform algorithms function as neutral coordination infrastructure,
 *   that wage convergence in blue-collar flexible segments signals genuine
 *   scarcity rather than monopsony, and that the arrangement's persistence
 *   reflects revealed preference of both sides. The claimed type is rope: a
 *   coordination mechanism with genuine beneficiaries on both sides, minimal
 *   suppression, and no identified victims in this reading's structural view.
 *   The sibling readings (precarity_extraction_reading,
 *   developmental_state_reading) are separate constraints with different
 *   structural declarations.
 *
 * KEY AGENTS:
 *   - platform_operators: Primary beneficiary (institutional/arbitrage) — operates matching infrastructure, collects fees
 *   - demand_side_employers: Beneficiary (organized/mobile) — accesses flexible labor pool, reduces fixed costs
 *   - workers_seeking_autonomy: Beneficiary (moderate/constrained) — values schedule control, supplemental income
 *   - precarious_workers: Excluded (powerless/trapped) — would object to autonomy framing; not in this reading's beneficiary set
 *   - labor_standards_regulators: Observer (institutional/analytical) — monitors classification boundaries, benefit portability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.18).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.12).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, 'fb378b41-a8f9-4d0d-b890-55597ded8b38').
narrative_ontology:cs_kernel_codification('fb378b41-a8f9-4d0d-b890-55597ded8b38', implicit).
narrative_ontology:cs_authority_grounding('fb378b41-a8f9-4d0d-b890-55597ded8b38', practice).
narrative_ontology:cs_reading_relation('fb378b41-a8f9-4d0d-b890-55597ded8b38', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb378b41-a8f9-4d0d-b890-55597ded8b38', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('fb378b41-a8f9-4d0d-b890-55597ded8b38', foundational, voluntary_participation_reveals_preference).
narrative_ontology:cs_axiom_status(voluntary_participation_reveals_preference, holdable).
narrative_ontology:cs_axiom_grounding('fb378b41-a8f9-4d0d-b890-55597ded8b38', voluntary_participation_reveals_preference, conventional).
narrative_ontology:cs_axiom('fb378b41-a8f9-4d0d-b890-55597ded8b38', foundational, algorithmic_matching_is_neutral_coordination).
narrative_ontology:cs_axiom_status(algorithmic_matching_is_neutral_coordination, holdable).
narrative_ontology:cs_axiom_grounding('fb378b41-a8f9-4d0d-b890-55597ded8b38', algorithmic_matching_is_neutral_coordination, empirically_contingent).
narrative_ontology:cs_reference_frame('fb378b41-a8f9-4d0d-b890-55597ded8b38', emergent_market_matching).
narrative_ontology:cs_drift_state('fb378b41-a8f9-4d0d-b890-55597ded8b38', platform_scale_mature, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb378b41-a8f9-4d0d-b890-55597ded8b38', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, demand_side_employers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, workers_seeking_autonomy).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, market_clearing_efficiency_axiom).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, voluntary_exchange_pareto_improvement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the algorithmic matching infrastructure connecting workers to tasks. Sets platform fees, matching rules, and deactivation policies. Collects a percentage of each transaction as coordination fee. Can pivot to adjacent markets (logistics, delivery, professional services) if any single segment becomes unprofitable. Faces competitive pressure from rival platforms but benefits from network effects and data accumulation.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, beneficiary).

% Businesses and households that source labor through platforms — ride-hail, delivery, freelance marketplaces, on-demand staffing. Gain access to a flexible labor pool without fixed employment costs (benefits, scheduling overhead, minimum hours). Can switch between platforms or revert to traditional hiring if platform costs exceed value. The coordination benefit is real: search costs drop, labor supply scales with demand spikes.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, demand_side_employers, beneficiary,
    organized, biographical, mobile, national).

% Workers who choose flexible platform work for schedule control, supplemental income, or as a bridge between traditional jobs. Includes students, caregivers, semi-retirees, and multi-platform earners. They value the ability to work when and where they want, without a boss setting shifts. Exit is constrained by income need and platform-specific reputation capital, but they are not trapped — many cycle in and out of platform work. This reading treats their participation as revealed preference for autonomy.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, workers_seeking_autonomy, beneficiary,
    moderate, biographical, constrained, national).

% Workers who depend on platform income for primary livelihood, face algorithmic discipline (deactivation risk, acceptance rate thresholds), and lack viable alternatives due to local labor market conditions, immigration status, or skill gaps. They would object to the autonomy framing — their experience is structural precarity, not chosen flexibility. This reading excludes them from the beneficiary set; their structural position is the subject of the precarity_extraction_reading sibling constraint.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, precarious_workers, excluded,
    powerless, immediate, trapped, national).

% Government agencies (labor departments, competition authorities, social security administrations) monitoring whether platform work constitutes employment misclassification, whether benefit portability systems are needed, and whether algorithmic management creates de facto employment relationships. They commission studies, hold hearings, and can impose reclassification or benefit mandates that would alter the constraint's economics. Their analytical seat sees the full kernel contest.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_standards_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches heterogeneous, time-varying labor supply to heterogeneous, time-varying demand through algorithmic platforms — reducing search costs, enabling real-time price discovery, and allowing workers to monetize idle capacity on their own schedule.
% TRANSFER_FUNCTION: Moves platform fees (typically 15-30% of transaction value) from the matched exchange to the platform operator, as the price of the matching infrastructure. Workers receive the residual; employers pay the total. The reading treats this as a competitive fee for coordination services, not rent.
% ABSENT_VOICES: Precarious workers who depend on platforms for primary income and experience algorithmic control — they are excluded from this reading's beneficiary structure but appear as victims in the precarity_extraction_reading. Traditional labor unions and worker advocacy organizations are also absent from this reading's coordination narrative; they appear as agenda_setters or observers in the developmental_state_reading.
% DISAPPEARANCE_RATIONALE: If platform matching vanished overnight, the flexible labor segments would not disappear — they would revert to informal networks, temp agencies, direct hiring, and cash-in-hand arrangements. Search costs would rise, matching speed would fall, and both workers and employers would lose the coordination infrastructure. The world rearranges to less efficient but functionally similar arrangements.
% FOUNDING_PROBLEM: Traditional labor markets fail to match highly heterogeneous, short-duration labor supply (spare hours, variable availability, diverse skills) to equally heterogeneous, short-duration demand (peak-hour delivery, project-based coding, seasonal staffing). The transaction costs of negotiating each match bilaterally were prohibitive.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and demand-side employers attest the problem remains live — matching efficiency continues to improve with scale and data. Academic labor economists (e.g., Katz & Krueger, Hall & Krueger) document the matching function's value. Workers_seeking_autonomy corroborate through participation. The sibling readings do not deny the founding problem exists; they contest whether the current arrangement solves it without extraction.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).
:- end_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because this reading treats platform fees as competitive returns to coordination infrastructure, not rent. Suppression is low (0.12) because participation is formally voluntary and alternatives (traditional employment, other platforms) exist. Theater ratio is moderate (0.25) because some platform communications overstate autonomy while algorithmic management constrains it — but this reading treats that as peripheral noise, not structural extraction. Accessibility collapse (0.3) and resistance (0.4) reflect that workers do push back (strikes, classification lawsuits) but the arrangement's coordination function remains intact. Metrics are authored independently of the claimed_type; the engine computes seat-level types from structural data.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (platform_operators) experiences this as pure coordination (rope); the payer seats (if any exist in this reading) would experience mild extraction; the beneficiary seats (employers, autonomy-seeking workers) experience net subsidy. The engine computes this divergence. The sibling readings produce different seat configurations — the precarity_extraction_reading declares precarious_workers as victims with identity_locked exit, yielding snare/tangled_rope classifications from their seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary declarations and exit options: platform_operators (institutional/arbitrage) sit at the beneficiary end (d ~0.1); demand_side_employers (organized/mobile) near symmetric (d ~0.4); workers_seeking_autonomy (moderate/constrained) moderately beneficiary (d ~0.35) because they gain flexibility but face income volatility. The excluded precarious_workers are not in this reading's beneficiary/victim structure — their structural position appears in the sibling readings. The engine computes effective extraction from these structural inputs.
 *
 * MANDATROPHY ANALYSIS:
 *   The market efficiency reading does not claim a founding mandate that has atrophied; it claims the arrangement solves a live coordination problem (matching supply to demand under heterogeneity). Mandatrophy is not the relevant frame — the contest is whether the coordination function is genuine or a cover for extraction. This reading asserts the former; sibling readings assert the latter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the market_efficiency_reading of the flexible_employment_legitimacy kernel. What structural elements distinguish it from the precarity_extraction_reading and developmental_state_reading?',
    'Comparative structural analysis of the three readings'' beneficiary/victim declarations, exit option profiles, and coordination/transfer function claims. The engine computes per-seat classifications from structural data; divergence in those data produces divergence in computed types.',
    'If the sibling readings declare victims and this reading declares none, the engine will classify this reading as rope and the others as snare or tangled_rope — the classification divergence IS the measurement of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system committer structure: kernel_id, reading_id, and structural delta vs siblings').

omega_variable(
    algorithm_neutrality_claim,
    'Are platform matching algorithms genuinely neutral coordination mechanisms, or do they embed extraction through information asymmetry and dynamic pricing?',
    'Independent audit of platform algorithm outputs against counterfactual competitive benchmarks; worker-side data collection on offer distribution and acceptance patterns.',
    'If algorithms extract surplus via information asymmetry, the constraint''s extractiveness is understated and its type may shift from rope toward tangled_rope; if neutral, the market efficiency reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_neutrality_claim, empirical, 'Whether platform algorithms function as neutral coordinators or extractive mechanisms').

omega_variable(
    wage_convergence_causality,
    'Does observed wage convergence in flexible segments reflect genuine scarcity signaling, or composition effects and platform monopsony power?',
    'Longitudinal labor market data with platform-level granularity; structural estimation separating supply-demand shifts from platform fee incidence.',
    'If wage gains are captured by platform fees rather than passed to workers, the coordination function is compromised and extraction is higher than this reading assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_convergence_causality, empirical, 'Causal interpretation of wage dynamics in flexible employment segments').

omega_variable(
    autonomy_vs_precariy_boundary,
    'Where does worker autonomy end and structural precarity begin in platform-mediated flexible work?',
    'Worker surveys measuring schedule control, income volatility, benefit access, and exit feasibility across platform types; comparative analysis with traditional employment.',
    'If autonomy is largely illusory for the median worker, the beneficiary declaration for workers_seeking_autonomy is overstated and the constraint carries hidden extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_precariy_boundary, conceptual, 'Boundary between genuine autonomy and structured precarity in platform work').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_tr_t0, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_tr_t5, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_tr_t10, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_tr_t15, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_tr_t20, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_be_t5, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_be_t10, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_be_t15, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_be_t20, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 20, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_su_t0, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_su_t5, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 5, 0.08).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_su_t10, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_su_t15, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(flexible_employment_legitimacy__market_efficiency_reading_su_t20, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 20, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__market_efficiency_reading, 0.15).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the flexible_employment_legitimacy kernel per the ε-invariance principle. Each reading instantiates a distinct constraint with its own ε, stakeholder structure, and classification. The market_efficiency_reading declares no victims and low extractiveness (rope); the precarity_extraction_reading declares victims and high extractiveness (snare/tangled_rope); the developmental_state_reading declares transitional coordination with state oversight (scaffold). They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
