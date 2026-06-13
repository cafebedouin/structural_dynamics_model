% ============================================================================
% CONSTRAINT STORY: performance_legitimacy_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy_flat_control, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy_flat_control
 *   human_readable: CCP Performance Legitimacy Governance Authority
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   The Chinese Communist Party grounds its authority to rule in its capacity
 *   to deliver sustained economic development and rising living standards—the
 *   'performance legitimacy' framework. This arrangement emerged after the
 *   Cultural Revolution as an alternative to ideological legitimacy, offering
 *   a new social contract: the party delivers growth, the population accepts
 *   single-party rule. The constraint is claimed as tangled_rope because it
 *   genuinely coordinates rapid development while extracting asymmetrically
 *   from rural workers, ethnic minorities, and political dissenters. The
 *   metrics describe rising extraction and suppression over the interval as
 *   growth slows and the legitimacy formula requires more enforcement to
 *   maintain. KEY AGENTS (by structural relationship): - party_leadership:
 *   Primary agenda-setter (institutional/arbitrage) — sets targets, controls
 *   narrative, collects rents - state_enterprise_managers: Primary
 *   beneficiary (powerful/mobile) — operate with preferential access, extract
 *   through monopoly positions - urban_middle_class: Secondary beneficiary
 *   (organized/constrained) — gains from development, accepts political
 *   constraints - rural_migrant_workers: Primary victim (powerless/trapped) —
 *   provide low-cost labor, denied urban rights under hukou -
 *   ethnic_minorities: Primary victim (powerless/identity_locked) —
 *   development as assimilation, surveillance intensification -
 *   political_dissidents: Secondary victim (moderate/constrained) —
 *   suppressed for challenging legitimacy narrative -
 *   independent_labor_organizers: Secondary victim (powerless/trapped) —
 *   suppressed for threatening production model - foreign_investors:
 *   Dual-positioned (powerful/mobile) — benefit from stability, pay through
 *   tech transfer - international_development_economists: Analytical observer
 *   (analytical/analytical) — study model, debate stability
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy_flat_control, 0.68).
domain_priors:suppression_score(performance_legitimacy_flat_control, 0.79).
domain_priors:theater_ratio(performance_legitimacy_flat_control, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy_flat_control, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy_flat_control, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(performance_legitimacy_flat_control, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy_flat_control, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(performance_legitimacy_flat_control, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy_flat_control, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy_flat_control, "CCP Performance Legitimacy Governance Authority").
narrative_ontology:topic_domain(performance_legitimacy_flat_control, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy_flat_control, 'ef920efd-664b-4e1e-b421-df06ec5474cd').
narrative_ontology:cs_kernel_codification('ef920efd-664b-4e1e-b421-df06ec5474cd', implicit).
narrative_ontology:cs_authority_grounding('ef920efd-664b-4e1e-b421-df06ec5474cd', extraction).
narrative_ontology:cs_created_at('ef920efd-664b-4e1e-b421-df06ec5474cd', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(performance_legitimacy_flat_control, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy_flat_control, party_leadership).
narrative_ontology:constraint_beneficiary(performance_legitimacy_flat_control, state_enterprise_managers).
narrative_ontology:constraint_beneficiary(performance_legitimacy_flat_control, urban_middle_class).
narrative_ontology:constraint_victim(performance_legitimacy_flat_control, rural_migrant_workers).
narrative_ontology:constraint_victim(performance_legitimacy_flat_control, ethnic_minorities).
narrative_ontology:constraint_victim(performance_legitimacy_flat_control, political_dissidents).
narrative_ontology:constraint_victim(performance_legitimacy_flat_control, independent_labor_organizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy_flat_control, foreign_investors).
narrative_ontology:constraint_victim(performance_legitimacy_flat_control, foreign_investors).
narrative_ontology:constraint_vindicates(performance_legitimacy_flat_control, developmental_state_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy_flat_control, authoritarian_modernization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets development targets, allocates state resources, controls promotion criteria for cadres, and enforces the narrative that economic performance justifies single-party rule. Collects rents through state enterprise control and regulatory discretion. Can exit to offshore assets and foreign residency if the arrangement fails.
narrative_ontology:constraint_stakeholder(performance_legitimacy_flat_control, party_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate large state-owned enterprises with preferential access to credit, land, and regulatory protection. Benefit from the performance legitimacy framework by being positioned as engines of development while extracting rents through monopoly positions and political connections. Can transition to private sector or international roles.
narrative_ontology:constraint_stakeholder(performance_legitimacy_flat_control, state_enterprise_managers, beneficiary,
    powerful, biographical, mobile, national).

% Have experienced substantial income growth, property appreciation, and consumption gains under the development model. Accept constraints on political participation in exchange for economic advancement and stability. Exit options limited by capital controls and professional credentials recognition abroad.
narrative_ontology:constraint_stakeholder(performance_legitimacy_flat_control, urban_middle_class, beneficiary,
    organized, biographical, constrained, regional).

% Provide low-cost labor for urban development and manufacturing export economy under hukou system that denies them urban social services and political rights. Bear the extraction that funds urban infrastructure and middle-class gains. Cannot exit the system without losing livelihood; returning to rural areas means subsistence poverty.
narrative_ontology:constraint_stakeholder(performance_legitimacy_flat_control, rural_migrant_workers, payer,
    powerless, immediate, trapped, local).

% Experience development as cultural assimilation and surveillance intensification justified by the performance legitimacy framework. Economic gains are conditional on political compliance and cultural erasure. Identity-locked because exit means abandoning homeland, language, and community; staying means accepting subordination.
narrative_ontology:constraint_stakeholder(performance_legitimacy_flat_control, ethnic_minorities, payer,
    powerless, generational, identity_locked, regional).

% Face imprisonment, surveillance, and social exclusion for challenging the performance legitimacy narrative or organizing alternative political claims. The constraint's suppression falls most heavily on them because their speech threatens the legitimacy formula itself. Exit through exile means permanent separation from family and professional networks.
narrative_ontology:constraint_stakeholder(performance_legitimacy_flat_control, political_dissidents, payer,
    moderate, biographical, constrained, national).

% Attempt to organize workers for better wages and conditions but are suppressed because independent labor action threatens both the low-cost production model and the party's monopoly on representing workers. Face arrest and blacklisting. Cannot exit because organizing is their livelihood and identity.
narrative_ontology:constraint_stakeholder(performance_legitimacy_flat_control, independent_labor_organizers, payer,
    powerless, immediate, trapped, local).

% Gain access to large consumer market and low-cost production base under the stability the performance legitimacy framework provides. Also pay through technology transfer requirements, joint venture mandates, and regulatory unpredictability. Can exit to other markets but at cost of sunk investments.
narrative_ontology:constraint_stakeholder(performance_legitimacy_flat_control, foreign_investors, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy_flat_control, foreign_investors, payer).

% Study the Chinese development model as a case of authoritarian state-led growth. Debate whether performance legitimacy is a stable governance form or a transitional arrangement that will face crisis when growth slows. Produce competing analyses of whether the model is replicable or sui generis.
narrative_ontology:constraint_stakeholder(performance_legitimacy_flat_control, international_development_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of rapid industrialization and infrastructure development by concentrating decision-making authority and resource allocation in a single party-state apparatus that can override local resistance and coordinate long-term investment.
% TRANSFER_FUNCTION: Moves surplus from rural labor, ethnic peripheries, and suppressed political alternatives to urban development, state enterprise expansion, and party elite enrichment, justified by aggregate GDP growth and rising average living standards.
% ABSENT_VOICES: Independent labor unions, ethnic autonomy movements, democratic reform advocates, and rural communities displaced by development projects are structurally excluded from the legitimacy conversation. They would argue that aggregate growth statistics mask severe distributional injustice and that performance legitimacy is a cover story for extraction.
% DISAPPEARANCE_RATIONALE: If the performance legitimacy framework vanished overnight, the party would lose its primary justification for monopoly rule, alternative political claims would surface immediately, suppressed labor organizing would emerge, and the state enterprise system would face legitimacy crisis. The political economy would reorganize around contested claims about who should govern and how surplus should be distributed.
% FOUNDING_PROBLEM: Post-Mao China faced economic stagnation, loss of ideological legitimacy after Cultural Revolution failures, and need to restore party authority without returning to mass mobilization campaigns. Performance legitimacy offered a new basis for rule: deliver growth and the party stays.
% FOUNDING_PROBLEM_CORROBORATION: Party historians and state media attest the founding problem is still live, citing ongoing development needs and external threats. Independent scholars, international human rights organizations, and suppressed domestic voices attest the founding problem is substantially resolved—China is now a middle-income country with advanced infrastructure—and the arrangement persists as a mechanism for elite enrichment and political control. World Bank data and academic development literature from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(performance_legitimacy_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy_flat_control, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    'c6d6880c39ec6bdfedde2a1d41cc00211f451559', '2026-06-12',
    'performance_legitimacy_kernel_flat_control', 'agent/example_platform_commission.json',
    'claude-sonnet-4-20250514', 'temperature=1.0').
narrative_ontology:story_seed(performance_legitimacy_flat_control, 'none', 0).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the performance legitimacy framework concentrates surplus in party-state hands and urban beneficiaries while rural workers and ethnic minorities bear disproportionate costs. Suppression is high (0.79) because maintaining the arrangement requires active enforcement against independent labor organizing, ethnic autonomy claims, and democratic reform advocacy—alternatives that would redistribute power. Theater ratio is moderate (0.42): genuine development coordination exists, but a growing share of activity is performative target-hitting and propaganda maintenance as actual growth slows. Accessibility collapse is high (0.71) because once the party monopolizes the legitimacy formula, alternative governance claims are structurally foreclosed. Resistance is substantial (0.58) because suppressed groups continue organizing despite costs. The measurement series shows extraction, theater, and suppression all rising over the 45-year interval as the model matures and growth decelerates, requiring more enforcement to sustain the legitimacy claim.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the powerless payer seats should compute radically differently. From the party leadership position, the arrangement is genuine developmental coordination that has lifted hundreds of millions from poverty and justifies continued rule. From the rural migrant and ethnic minority positions, the same structure operates as enforced extraction where their labor and resources fund urban gains while they are denied political voice and social services. The urban middle class beneficiary seat sits between: they experience real gains but under constrained political participation. The engine computes this divergence from the structural data—the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Party leadership is the structural beneficiary and agenda-setter (d near 0.1): they set the rules, collect the rents, and can exit if the arrangement fails. State enterprise managers and urban middle class are beneficiaries (d near 0.2-0.3): they gain from the development model without running it. Rural migrant workers and ethnic minorities are primary targets (d near 0.9): they bear the extraction with minimal exit options—trapped by hukou or identity-locked by homeland attachment. Political dissidents and labor organizers are also targets (d near 0.8): suppression falls on them for threatening the legitimacy formula. Foreign investors sit near symmetric (d near 0.5): they benefit from stability but pay through regulatory extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The performance legitimacy framework risks mandatrophy as China transitions from rapid catch-up growth to mature economy status. The founding problem—post-Mao economic stagnation and legitimacy crisis—is substantially resolved: China is now the world's second-largest economy with advanced infrastructure. But the arrangement persists because it serves elite enrichment and political control, not because the original coordination problem remains live. The rising theater ratio and suppression requirement over the interval indicate the constraint is shifting from genuine coordination toward performative maintenance. The omega variables capture the irreducible uncertainty: is slowing growth a temporary adjustment or a structural shift that will force legitimacy renegotiation? Can the party transition to a new legitimacy basis, or is performance legitimacy identity-locked to the current regime?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_slowdown_legitimacy_crisis,
    'Is China''s growth slowdown a temporary adjustment or a structural shift to lower equilibrium growth rates that will undermine the performance legitimacy formula?',
    'Sustained period (10+ years) of sub-4% GDP growth with rising unemployment and social unrest would indicate structural shift; return to 6%+ growth would indicate temporary adjustment. Demographic data, productivity trends, and debt sustainability metrics are key observables.',
    'If structural, the performance legitimacy framework faces crisis because the social contract depends on continuous improvement. The party would need to find a new legitimacy basis or face intensifying suppression costs. If temporary, the arrangement can persist with modest adjustments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_slowdown_legitimacy_crisis, empirical, 'Whether slowing growth is cyclical or structural, determining legitimacy formula viability.').

omega_variable(
    coordination_extraction_separability,
    'Is the developmental coordination function separable from the political monopoly and extraction structure, or are they structurally inseparable?',
    'Natural experiments from other developmental states (South Korea, Taiwan) that achieved rapid growth and then transitioned to democracy. If they maintained development coordination after political opening, the functions are separable. Comparative institutional analysis of development planning under different regime types.',
    'If separable, the performance legitimacy framework is revealed as extraction riding on coordination—the party could be replaced without losing development capacity. If inseparable, some of the measured extraction is the price of the coordination itself, and the tangled_rope classification is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether authoritarian control is necessary for developmental coordination or is extractive overhead.').

omega_variable(
    middle_income_trap_vs_continued_convergence,
    'Will China escape the middle-income trap and converge to high-income status, or will it stagnate at current levels?',
    'Sustained per-capita income growth reaching OECD levels (>$30,000 PPP) would indicate escape; stagnation at $15,000-20,000 for a decade would indicate trap. Technology innovation metrics, human capital development, and institutional quality indicators are key observables.',
    'Escaping the trap would extend the performance legitimacy formula''s viability by delivering continued gains. Falling into the trap would accelerate legitimacy crisis because the social contract promise would be broken. The trap scenario makes the founding problem clearly obsolete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middle_income_trap_vs_continued_convergence, empirical, 'Whether China achieves high-income status or stagnates, determining legitimacy timeline.').

omega_variable(
    ethnic_minority_suppression_necessity,
    'Is the intensifying suppression of ethnic minorities (Uyghurs, Tibetans) structurally necessary for the performance legitimacy framework, or is it extractive overhead that could be removed without threatening the development model?',
    'Comparative analysis of multi-ethnic developmental states that achieved growth without ethnic suppression. Historical analysis of whether ethnic autonomy claims actually threaten economic coordination or only threaten political monopoly.',
    'If structurally necessary, the ethnic suppression is part of the coordination cost and the tangled_rope classification holds. If it is extractive overhead, the suppression is pure extraction and the constraint is closer to snare for ethnic minority seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethnic_minority_suppression_necessity, conceptual, 'Whether ethnic suppression is coordination cost or extractive political control.').

omega_variable(
    alternative_legitimacy_transition,
    'Can the CCP transition to a new legitimacy basis (rule of law, procedural legitimacy, nationalist identity) if performance legitimacy becomes unsustainable, or is the party identity-locked to the performance formula?',
    'Observation of party discourse and institutional reforms during prolonged growth slowdown. If the party successfully shifts legitimacy narrative and maintains rule, transition is possible. If it doubles down on performance claims and intensifies suppression, it is identity-locked.',
    'If transition is possible, the performance legitimacy constraint is temporary and could be replaced by a different governance arrangement. If the party is identity-locked, the constraint persists until crisis forces regime change, and the rising suppression trajectory continues.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_legitimacy_transition, preference, 'Whether the party can shift legitimacy basis or is locked to performance formula.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy_flat_control, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy_flat_control, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(perf_tr_t0, observed).
narrative_ontology:measurement(perf_tr_t9, performance_legitimacy_flat_control, theater_ratio, 9, 0.26).
narrative_ontology:measurement_basis(perf_tr_t9, observed).
narrative_ontology:measurement(perf_tr_t18, performance_legitimacy_flat_control, theater_ratio, 18, 0.31).
narrative_ontology:measurement_basis(perf_tr_t18, observed).
narrative_ontology:measurement(perf_tr_t27, performance_legitimacy_flat_control, theater_ratio, 27, 0.35).
narrative_ontology:measurement_basis(perf_tr_t27, observed).
narrative_ontology:measurement(perf_tr_t36, performance_legitimacy_flat_control, theater_ratio, 36, 0.39).
narrative_ontology:measurement_basis(perf_tr_t36, observed).
narrative_ontology:measurement(perf_tr_t45, performance_legitimacy_flat_control, theater_ratio, 45, 0.42).
narrative_ontology:measurement_basis(perf_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy_flat_control, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(perf_be_t0, observed).
narrative_ontology:measurement(perf_be_t9, performance_legitimacy_flat_control, base_extractiveness, 9, 0.52).
narrative_ontology:measurement_basis(perf_be_t9, observed).
narrative_ontology:measurement(perf_be_t18, performance_legitimacy_flat_control, base_extractiveness, 18, 0.58).
narrative_ontology:measurement_basis(perf_be_t18, observed).
narrative_ontology:measurement(perf_be_t27, performance_legitimacy_flat_control, base_extractiveness, 27, 0.63).
narrative_ontology:measurement_basis(perf_be_t27, observed).
narrative_ontology:measurement(perf_be_t36, performance_legitimacy_flat_control, base_extractiveness, 36, 0.66).
narrative_ontology:measurement_basis(perf_be_t36, observed).
narrative_ontology:measurement(perf_be_t45, performance_legitimacy_flat_control, base_extractiveness, 45, 0.68).
narrative_ontology:measurement_basis(perf_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy_flat_control, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(perf_su_t0, observed).
narrative_ontology:measurement(perf_su_t9, performance_legitimacy_flat_control, suppression_requirement, 9, 0.62).
narrative_ontology:measurement_basis(perf_su_t9, observed).
narrative_ontology:measurement(perf_su_t18, performance_legitimacy_flat_control, suppression_requirement, 18, 0.67).
narrative_ontology:measurement_basis(perf_su_t18, observed).
narrative_ontology:measurement(perf_su_t27, performance_legitimacy_flat_control, suppression_requirement, 27, 0.72).
narrative_ontology:measurement_basis(perf_su_t27, observed).
narrative_ontology:measurement(perf_su_t36, performance_legitimacy_flat_control, suppression_requirement, 36, 0.76).
narrative_ontology:measurement_basis(perf_su_t36, observed).
narrative_ontology:measurement(perf_su_t45, performance_legitimacy_flat_control, suppression_requirement, 45, 0.79).
narrative_ontology:measurement_basis(perf_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy_flat_control, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy_flat_control, hukou_system).
narrative_ontology:affects_constraint(performance_legitimacy_flat_control, state_enterprise_monopoly).
narrative_ontology:affects_constraint(performance_legitimacy_flat_control, great_firewall_information_control).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy_flat_control, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
