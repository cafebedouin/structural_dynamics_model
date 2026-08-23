% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Flexible Employment as Legitimate Market-Clearing Mechanism
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the market_efficiency_reading of the
 *   flexible_employment_legitimacy kernel. The reading asserts that flexible
 *   employment — mediated by algorithmic platforms — is a legitimate
 *   market-clearing mechanism that matches labor supply to demand
 *   efficiently. It treats wage convergence as a market signal of blue-collar
 *   scarcity, platform algorithms as neutral coordination infrastructure, and
 *   worker autonomy as maximized by the absence of rigid schedules. The
 *   claimed type is rope (pure coordination); the authored metrics reflect
 *   the reading's own assessment of the standing arrangement (low extraction,
 *   low suppression). The engine will compute per-seat classifications from
 *   the structural data; this reading's claim does not adjudicate the
 *   outcome.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.22).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.18).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Legitimate Market-Clearing Mechanism").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, 'f4303c07-c5ef-443a-82b7-b15a12727e40').
narrative_ontology:cs_kernel_codification('f4303c07-c5ef-443a-82b7-b15a12727e40', distributed).
narrative_ontology:cs_authority_grounding('f4303c07-c5ef-443a-82b7-b15a12727e40', practice).
narrative_ontology:cs_reading_relation('f4303c07-c5ef-443a-82b7-b15a12727e40', flexible_employment_legitimacy__precarity_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('f4303c07-c5ef-443a-82b7-b15a12727e40', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('f4303c07-c5ef-443a-82b7-b15a12727e40', foundational, flexible_employment_clears_markets_efficiently).
narrative_ontology:cs_axiom_status(flexible_employment_clears_markets_efficiently, holdable).
narrative_ontology:cs_axiom_grounding('f4303c07-c5ef-443a-82b7-b15a12727e40', flexible_employment_clears_markets_efficiently, empirically_contingent).
narrative_ontology:cs_axiom('f4303c07-c5ef-443a-82b7-b15a12727e40', foundational, worker_autonomy_maximized_by_flexibility).
narrative_ontology:cs_axiom_status(worker_autonomy_maximized_by_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('f4303c07-c5ef-443a-82b7-b15a12727e40', worker_autonomy_maximized_by_flexibility, deontological).
narrative_ontology:cs_reference_frame('f4303c07-c5ef-443a-82b7-b15a12727e40', market_clearing_legitimacy).
narrative_ontology:cs_drift_state('f4303c07-c5ef-443a-82b7-b15a12727e40', contemporary_platform_mature_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f4303c07-c5ef-443a-82b7-b15a12727e40', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, gig_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, gig_workers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, market_clearing_efficiency).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, worker_autonomy_maximization).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, algorithmic_neutrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate algorithmic matching platforms that connect workers with tasks. Set platform rules, commission rates, and matching algorithms. Collect platform fees as primary revenue. Argue their platforms create efficient markets where none existed before, reducing search costs and enabling flexible work. Can pivot to new sectors or geographies if regulation tightens in one jurisdiction.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Choose when, where, and how much to work across one or multiple platforms. Value schedule autonomy, low barriers to entry, and ability to supplement income. Bear income volatility, lack of benefits, and algorithmic management. Can exit to traditional employment, other platforms, or self-employment; exit is feasible but involves income disruption. The market_efficiency_reading emphasizes their autonomy gains; the precarity reading emphasizes their risk exposure.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, gig_workers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, gig_workers, payer).

% Access on-demand services (rides, delivery, freelance tasks) at lower prices and higher convenience than traditional alternatives. Benefit from competitive pricing driven by platform-scale matching efficiency. Can substitute across platforms or revert to traditional providers instantly. Their demand elasticity disciplines platform pricing.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers, beneficiary,
    organized, immediate, arbitrage, global).

% Access flexible labor for peak demand, seasonal work, and specialized tasks without full-time employment costs. Use platforms as a contingent workforce layer. Can hire directly, use agencies, or automate; platforms are one option among many. Benefit from labor cost flexibility and reduced hiring friction.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers, beneficiary,
    powerful, biographical, arbitrage, national).

% Monitor platform labor markets for compliance with employment law, minimum wage, safety, and anti-discrimination rules. Conduct inquiries, issue guidance, and enforce classification tests (employee vs. independent contractor). Their analytical seat sees the full structure: coordination benefits, classification ambiguities, and emerging regulatory gaps.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% Organize gig workers for collective bargaining, algorithmic transparency, and employment protections. Are structurally excluded from platform governance and standard labor relations frameworks because workers are classified as independent contractors. Would argue that autonomy is illusory under algorithmic control and that market power is concentrated. Their exclusion is what the 'neutral coordination' claim depends on maintaining.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, worker_collectives, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the bilateral search problem in labor markets: workers need tasks, tasks need workers. Platform algorithms match supply to demand in real time, reducing search costs, enabling instant price discovery, and creating liquidity in previously fragmented or non-existent markets (e.g., on-demand rides, micro-tasks, fractional freelance work).
% TRANSFER_FUNCTION: Moves a platform commission (typically 15-30% of transaction value) from the worker's earnings to the platform operator, as the price of access to the matching infrastructure, payment rails, trust systems, and demand aggregation. Workers retain the residual; consumers pay the full fare; the platform captures the spread.
% ABSENT_VOICES: Worker collectives and displaced traditional workers (e.g., taxi drivers, hotel staff) are structurally excluded from platform governance. They would contest the 'independent contractor' classification, demand algorithmic accountability, and argue that market power is concentrated not competitive. Their absence allows the neutral coordination narrative to persist unchallenged in platform rule-setting.
% DISAPPEARANCE_RATIONALE: If platform matching and its commission structure vanished overnight, on-demand service markets would fragment: search costs would spike, price discovery would slow, many marginal transactions would not occur, and workers would lose a low-barrier income option. The labor market would reorganize around traditional hiring, agencies, and direct contracting — higher friction, lower flexibility.
% FOUNDING_PROBLEM: Pre-platform labor markets for low-skill, variable-demand services (rides, deliveries, odd jobs) suffered from high search costs, trust deficits, and coordination failures. Workers couldn't find tasks efficiently; buyers couldn't find reliable workers instantly. No central clearing mechanism existed.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and economic studies (e.g., Hall & Krueger 2018 on Uber driver flexibility) attest the founding problem is live: matching frictions remain real and platforms solve them. Labor economists (e.g., Katz & Krueger 2019) and worker advocates attest the founding problem is substantially solved for the matching function but the arrangement now persists as a classification and power structure beyond its coordination purpose. Independent academic research supports both readings depending on metric and market segment.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.22) because the reading assesses the platform commission as the competitive price of matching services, not monopoly rent. Suppression is low (0.18) because participation is voluntary and exit to traditional employment or other platforms is feasible. Theater ratio is low (0.12) because the matching function is genuine and measurable. Accessibility collapse is low (0.25) because traditional employment, self-employment, and multi-platform work remain live alternatives. Resistance is moderate (0.35) because worker organizing exists but is fragmented by the independent contractor classification. The slight upward drift in extraction and suppression over the interval reflects platform maturation and increasing algorithmic control — which this reading treats as efficiency gains, not extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (if any worker is read as net payer) and the agenda-setter seat compute differently: from the platform's view, the arrangement is a rope — efficient coordination it built and maintains; from a worker experiencing algorithmic deactivation or wage suppression, the same structure may compute as snare or tangled_rope. The engine computes this divergence from the structural data. This reading's claim (rope) reflects the agenda-setter's experience; the metrics are authored from the reading's own lights, not from the worker's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are structural beneficiaries (collect commissions, set rules, arbitrage across jurisdictions — d near 0). Gig workers are declared beneficiaries (autonomy, flexibility) with a secondary payer aspect (bear risk, pay commission) — the reading emphasizes the beneficiary pole. Consumers and traditional employers are clear beneficiaries (lower prices, flexible labor). Labor regulators are analytical observers. Worker collectives are excluded — their structural position would make them payers under a different reading, but this reading's framing places them outside the coordination circle.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (matching supply to demand) remains live — matching frictions persist in many service markets. The mandate has not atrophied; the arrangement continues to solve its founding problem. However, the reading's boundary is contested: where coordination ends and extraction begins (commission levels, algorithmic control, classification) is the contested zone. The mandatrophy question is not whether the mechanism works, but whether its current form exceeds its coordination mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is flexible employment a single constraint with multiple interpretations, or are market_efficiency_reading, precarity_extraction_reading, and developmental_state_reading structurally distinct constraints with different ε values?',
    'Decompose the kernel into separate constraint stories per the ε-invariance principle: if measuring extraction under the market-efficiency frame yields ε≈0.22 but under the precarity frame yields ε≈0.65, they are different constraints. Author separate JSON files for each reading and link via network.affects_constraints.',
    'If distinct constraints, each gets its own classification, stakeholders, and temporal dynamics. The market_efficiency_reading would be a rope; the precarity_extraction_reading would be a snare or tangled_rope; the developmental_state_reading would be a scaffold. The corpus currently conflates them under one label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel label ''flexible employment legitimacy'' covers one constraint or a constraint family.').

omega_variable(
    algorithmic_neutrality_claim,
    'Are platform matching algorithms genuinely neutral coordination infrastructure, or do they embed extraction via information asymmetry, dynamic pricing, and behavioral nudging?',
    'Audit platform algorithms for: (a) whether matching optimizes for total surplus or platform revenue, (b) whether workers see the same price signals as consumers, (c) whether deactivation/ranking criteria are transparent and contestable. Regulatory mandates for algorithmic transparency (e.g., EU Platform Work Directive) may produce evidence.',
    'If algorithms extract via asymmetry, the constraint''s effective extraction is higher than the reading''s assessment, and the coordination function is contaminated — the constraint may compute as tangled_rope from the worker seat. If genuinely neutral, the rope claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_neutrality_claim, empirical, 'Whether the ''neutral coordination'' claim survives algorithmic audit.').

omega_variable(
    wage_convergence_signal,
    'Does wage convergence in platform markets reflect genuine blue-collar scarcity (supply/demand), or does it reflect monopsony power, algorithmic wage-setting, and the absence of collective bargaining?',
    'Compare wage elasticity and concentration metrics (HHI) in platform markets vs. traditional labor markets for comparable tasks. Test whether platforms act as wage-makers (setting rates unilaterally) or wage-takers (clearing at market rates).',
    'If convergence reflects scarcity, the market-clearing claim is empirically supported. If it reflects monopsony, the reading''s foundational axiom (efficient clearing) is empirically contested, and extraction is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_convergence_signal, empirical, 'Whether wage dynamics validate or falsify the market-clearing claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fel_mer_tr_t0, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fel_mer_tr_t6, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement(fel_mer_tr_t12, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(fel_mer_tr_t18, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 18, 0.11).
narrative_ontology:measurement(fel_mer_tr_t24, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 24, 0.12).

% Extraction over time
narrative_ontology:measurement(fel_mer_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fel_mer_be_t6, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 6, 0.18).
narrative_ontology:measurement(fel_mer_be_t12, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(fel_mer_be_t18, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 18, 0.21).
narrative_ontology:measurement(fel_mer_be_t24, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 24, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(fel_mer_su_t0, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(fel_mer_su_t6, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 6, 0.13).
narrative_ontology:measurement(fel_mer_su_t12, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 12, 0.15).
narrative_ontology:measurement(fel_mer_su_t18, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 18, 0.17).
narrative_ontology:measurement(fel_mer_su_t24, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 24, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__market_efficiency_reading, 0.15).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'flexible_employment_legitimacy' into three readings with different ε values and structural profiles. The market_efficiency_reading (this story) claims ε≈0.22 (rope); the precarity_extraction_reading claims ε≈0.65 (snare/tangled_rope); the developmental_state_reading claims ε≈0.35 (scaffold). They share the same referent (the standing arrangement of platform-mediated flexible employment) but instantiate different constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
