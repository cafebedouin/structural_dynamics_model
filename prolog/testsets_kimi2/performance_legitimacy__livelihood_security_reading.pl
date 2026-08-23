% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy â Livelihood Security Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint is the livelihood-security reading of the performance
 *   legitimacy kernel in state capitalist development planning. It holds that
 *   regime legitimacy derives from delivering tangible, daily-life
 *   improvementsâemployment, healthcare, education, elderly careâdirectly
 *   experienced by citizens. Under this reading, the state redirects fiscal
 *   and planning capacity from capital-intensive industrial expansion and
 *   local infrastructure toward household consumption and social-service
 *   provision. The constraint is contested within the ruling coalition:
 *   growth-first and techno-nationalist factions experience the redirection
 *   as extraction, while service sectors and urban households are the
 *   coordinated beneficiaries. The kernel is formalized through Five-Year
 *   Plans, party congress work reports, and social-policy mandates. This
 *   story instantiates ONE reading; sibling readings (quantitative growth,
 *   qualitative development, techno-nationalism) are separate constraints
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Central development state: Primary agenda-setter (institutional/arbitrage) â administers the legitimacy framework and enforces fiscal reallocation.
 *   - Service sector providers: Primary beneficiary (organized/constrained) â receive prioritized funding and expanded mandates.
 *   - Household consumers: Secondary beneficiary (moderate/constrained) â receive transfers and subsidized services, limited exit by welfare dependency.
 *   - Capital-intensive industries: Primary target (powerful/constrained) â lose credit and budget share to the livelihood mandate.
 *   - Local government infrastructure bureaus: Secondary target (organized/constrained) â face tightened debt controls and reduced project approvals.
 *   - Growth-first reformists: Excluded voice (moderate/constrained) â marginalized within the bureaucratic agenda.
 *   - Political economy analysts: Analytical observer (analytical/analytical) â tracks the stability-productivity trade-off.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.55).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy â Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, 'd6439433-ff93-434c-9813-c87d92a06e85').
narrative_ontology:cs_kernel_codification('d6439433-ff93-434c-9813-c87d92a06e85', formalized).
narrative_ontology:cs_authority_grounding('d6439433-ff93-434c-9813-c87d92a06e85', lineage).
narrative_ontology:cs_interpretation_layer_present('d6439433-ff93-434c-9813-c87d92a06e85').
narrative_ontology:cs_reading_relation('d6439433-ff93-434c-9813-c87d92a06e85', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6439433-ff93-434c-9813-c87d92a06e85', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6439433-ff93-434c-9813-c87d92a06e85', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('d6439433-ff93-434c-9813-c87d92a06e85', foundational, tangible_livelihood_as_legitimacy_base).
narrative_ontology:cs_axiom_status(tangible_livelihood_as_legitimacy_base, holdable).
narrative_ontology:cs_axiom_grounding('d6439433-ff93-434c-9813-c87d92a06e85', tangible_livelihood_as_legitimacy_base, conventional).
narrative_ontology:cs_axiom('d6439433-ff93-434c-9813-c87d92a06e85', foundational, consumption_priority_over_accumulation).
narrative_ontology:cs_axiom_status(consumption_priority_over_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('d6439433-ff93-434c-9813-c87d92a06e85', consumption_priority_over_accumulation, instrumental).
narrative_ontology:cs_reference_frame('d6439433-ff93-434c-9813-c87d92a06e85', developmental_state_livelihood_mandate).
narrative_ontology:cs_drift_state('d6439433-ff93-434c-9813-c87d92a06e85', post_reform_slowdown_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d6439433-ff93-434c-9813-c87d92a06e85', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sector_providers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, household_consumers).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industries).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_bureaus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the policy framework that ties regime legitimacy to measurable livelihood improvements. Directs fiscal transfers, social spending mandates, and planning priorities toward household consumption and service delivery. Can reallocate priorities if political conditions shift, but is locked into the livelihood narrative by its own legitimacy claims.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, central_development_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Healthcare, education, elderly care, and social service providers who receive prioritized state funding and expanded mandates under the livelihood-security framework. Their institutional growth and wage bill depend on continued central commitment to this reading.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sector_providers, beneficiary,
    organized, biographical, constrained, national).

% Citizen households who receive subsidized healthcare, education, pension top-ups, and employment support. They experience tangible improvements directly and are the intended audience of the legitimacy claim. Exit is limited by lack of alternative welfare providers and household registration constraints.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, household_consumers, beneficiary,
    moderate, biographical, constrained, national).

% Heavy industry, real estate development, and strategic manufacturing sectors that see credit allocations, tax privileges, and project approvals curtailed as central priorities shift toward consumption and services. They retain political influence but lose budget share and subsidized financing.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industries, payer,
    powerful, biographical, constrained, national).

% Sub-national government agencies responsible for infrastructure investment and urban development. They face tightened debt controls, reduced project approvals, and shrinking land-finance revenues as central mandates redirect fiscal resources toward social spending and household transfers.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_bureaus, payer,
    organized, biographical, constrained, regional).

% Economists and officials who argue that continued capital-intensive investment and GDP growth should remain the primary legitimacy base. Their policy influence is structurally reduced under the livelihood-security prioritization, though they remain within the bureaucratic system.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, growth_first_reformists, excluded,
    moderate, biographical, constrained, national).

% Academic and policy analysts who study the trade-off between social spending and investment-led growth in state capitalism. They track whether the livelihood shift stabilizes legitimacy or creates fiscal and productivity vulnerabilities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, political_economy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social reproduction and political stability by delivering universal healthcare, education, elderly care, and employment support through centralized planning and fiscal allocation, reducing individual household risk and maintaining regime-citizen trust.
% TRANSFER_FUNCTION: Moves fiscal resources, credit quotas, and bureaucratic priority from capital-intensive industrial investment and local infrastructure expansion toward household consumption support, service-sector wages, and social welfare provision.
% ABSENT_VOICES: Growth-first reformist economists and heavy-industry lobbies are partially included in the bureaucracy but structurally marginalized in agenda-setting; rural migrant workers without full household registration are formally beneficiaries but often partially excluded from service access in practice.
% DISAPPEARANCE_RATIONALE: If the livelihood-security mandate vanished overnight, fiscal flows would revert toward infrastructure and industrial investment, service-sector funding would collapse, household consumption would fall without transfer supports, and the political narrative binding citizens to the regime through tangible daily-life improvements would dissolve â the developmental model would reorganize around the remaining kernel readings.
% FOUNDING_PROBLEM: Post-reform developmental states faced a social services deficit and rising inequality after decades of investment-heavy growth, creating risks of legitimacy erosion and social instability in the absence of universal welfare provision.
% FOUNDING_PROBLEM_CORROBORATION: Official state documents and party congress reports attest the problem is live and being addressed. Independent demographers and political economists note significant remaining gaps in rural healthcare and pension coverage, suggesting the founding problem is partially solved but the arrangement also serves to consolidate regime legitimacy; external academic analysis corroborates the partial-solution reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint actively reallocates credit, budget share, and bureaucratic attention away from industrial and infrastructure actors toward service delivery and household transfers. Suppression (0.55) reflects that growth-first policy alternatives are structurally marginalized within the planning apparatus, though not violently repressed. Theater ratio (0.30) captures performative reporting of service coverage and 'people's livelihood' achievements that may exceed functional delivery. Accessibility collapse (0.40) is moderate: alternative development models (export-led growth, real estate-driven urbanization) remain cognitively available and advocated by residual factions, but are increasingly disfavored in formal agenda-setting. Resistance (0.50) is significant: local governments and heavy-industry lobbies actively resist the fiscal reallocation through lobbying, shadow borrowing, and bureaucratic foot-dragging. The measurement series share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   From the central state's seat, the constraint is a coordination mechanism that secures social stability and regime legitimacy through universal service provision. From the capital-intensive industry and local infrastructure seats, the same constraint operates as extraction: their investment capacity and fiscal autonomy are compressed to fund the livelihood mandate. The engine computes this divergence from structural data; the authored claim (tangled_rope) does not adjudicate between these experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Service-sector providers and household consumers are structural beneficiaries (low d): they receive fiscal flows and subsidized services. Capital-intensive industries and local infrastructure bureaus are structural targets (high d): they bear the costs of reallocation through reduced credit and tightened debt controls. The central state sits near the beneficiary end for its own legitimacy accumulation but also bears the enforcement burden; its institutional power and arbitrage exit keep its effective extraction low. Growth-first reformists are excluded (no d computed).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy because it retains a genuine coordination function: universal healthcare, education, and elderly care solve real collective-action problems in social reproduction that market mechanisms alone have not addressed in this developmental context. If the coordination function were entirely hollowâif service delivery were pure facade while all resources continued to flow to industryâit would compute as a snare. The authored metrics (extractiveness 0.62, theater 0.30) reflect that the coordination is real but partially captured by performative and redistributive dynamics, producing the hybrid tangled-rope profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_livelihood,
    'Can the livelihood-security prioritization be sustained fiscally if capital-intensive investment and land-finance revenues continue to decline?',
    'Longitudinal fiscal incidence analysis tracking central and local government revenue composition against social-spending obligations; natural experiment from provinces with divergent industrial bases.',
    'If unsustainable, the constraint will face either a terminal type shift (toward piton or snare as delivery hollows out) or a reversion to the quantitative growth reading; if sustainable, the tangled-rope profile stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_livelihood, empirical, 'Fiscal sustainability of consumption-first redistribution').

omega_variable(
    delivery_quality_vs_reporting,
    'Do reported improvements in healthcare, education, and elderly care reflect genuine service enhancement or statistical and performative inflation?',
    'Independent audit of service-quality metrics against household-reported outcomes; comparison of budget inputs to measurable health and education outputs.',
    'If performative inflation is high, the theater_ratio understates the true gap and the coordination function is weaker than authored, pushing the computed type toward snare; if genuine, the coordination function is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delivery_quality_vs_reporting, empirical, 'Theater gap between reported and actual service delivery').

omega_variable(
    reading_contingency,
    'Is the livelihood-security reading a durable reframing of the performance legitimacy kernel, or a cyclical corrective that reverts to growth-first under economic stress?',
    'Historical pattern analysis of kernel readings across prior macroeconomic shocks; tracing which reading gains agenda dominance during fiscal contraction versus expansion.',
    'If cyclical, the constraint''s type oscillates with the business cycle and long-term temporal measurements should show periodic reversion; if durable, the extraction profile accumulates monotonically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contingency, conceptual, 'Durability of the livelihood reading within the performance legitimacy kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perfleg_livelihood_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(perfleg_livelihood_tr_t4, performance_legitimacy__livelihood_security_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(perfleg_livelihood_tr_t8, performance_legitimacy__livelihood_security_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(perfleg_livelihood_tr_t12, performance_legitimacy__livelihood_security_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(perfleg_livelihood_tr_t16, performance_legitimacy__livelihood_security_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(perfleg_livelihood_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(perfleg_livelihood_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perfleg_livelihood_be_t4, performance_legitimacy__livelihood_security_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(perfleg_livelihood_be_t8, performance_legitimacy__livelihood_security_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(perfleg_livelihood_be_t12, performance_legitimacy__livelihood_security_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(perfleg_livelihood_be_t16, performance_legitimacy__livelihood_security_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(perfleg_livelihood_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(perfleg_livelihood_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(perfleg_livelihood_su_t4, performance_legitimacy__livelihood_security_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(perfleg_livelihood_su_t8, performance_legitimacy__livelihood_security_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(perfleg_livelihood_su_t12, performance_legitimacy__livelihood_security_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(perfleg_livelihood_su_t16, performance_legitimacy__livelihood_security_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(perfleg_livelihood_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel. The kernel decomposes into four structurally distinct constraints because each reading defines a different epsilon referent, beneficiary/victim structure, and resource allocation pattern. Livelihood security prioritizes consumption and services; quantitative growth prioritizes GDP expansion; qualitative development prioritizes innovation and sustainability; techno-nationalism prioritizes strategic industrial leadership. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
