% ============================================================================
% CONSTRAINT STORY: intelligence_analyst_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intelligence_analyst_autonomy, []).

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
 *   constraint_id: intelligence_analyst_autonomy
 *   human_readable: Intelligence Analyst Autonomy Constraint
 *   domain: political/organizational/institutional
 *
 * SUMMARY:
 *   Intelligence analyst autonomy represents a fundamental structural tension
 *   between legitimate security requirements and extractive suppression of
 *   professional judgment. Intelligence agencies depend on analyst expertise
 *   to provide accurate assessments, yet they simultaneously constrain
 *   analyst autonomy to ensure policy alignment and operational security.
 *   This constraint exhibits both genuine coordination functions —
 *   compartmentalization and classification systems do solve real
 *   counterintelligence problems — and systematic extraction mechanisms that
 *   subordinate expertise to political preference. The constraint has evolved
 *   over the measurement interval: early in the post-WWII period (0-10
 *   years), explicit autonomy constraints were minimal and justified through
 *   transparency; over 40 years, extractiveness has increased as informal
 *   enforcement mechanisms replaced formal oversight, and the theater ratio
 *   has risen as the gap between formal independence rhetoric and actual
 *   career consequence has widened. The individual analyst faces career
 *   consequences for providing analysis that contradicts policy preferences,
 *   yet the system is structured with plausible deniability — no explicit
 *   rule forbids autonomous analysis, but career advancement depends on
 *   alignment with institutional preference.
 *
 * KEY AGENTS:
 *   - Individual Analyst: Primary victim (powerless/trapped) — career security and pension depend on institutional loyalty; exit options blocked by clearance dependency and market access barriers
 *   - Professional Analyst Cohort: Secondary victim (moderate/constrained) — face suppression through institutional pressure and retaliation mechanisms; benefit from coordination infrastructure but bear asymmetric extraction costs
 *   - Intelligence Agency Management: Primary beneficiary (institutional/arbitrage) — benefits from controlled analyst output; genuinely coordinates security and operational functions; has exit options through restructuring
 *   - Political Leadership: Secondary beneficiary (powerful/arbitrage) — extracts convenient analysis aligned with policy; high power and exit options; coercive relationship with intelligence system
 *   - Institutional Oversight: Theater performer (institutional/constrained) — formal committees, inspector generals, congressional oversight provide legitimacy cover while actual enforcement operates through informal mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees hybrid coordination-extraction architecture where legitimate security functions are layered with extractive conformity pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intelligence_analyst_autonomy, 0.58).
domain_priors:suppression_score(intelligence_analyst_autonomy, 0.68).
domain_priors:theater_ratio(intelligence_analyst_autonomy, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intelligence_analyst_autonomy, extractiveness, 0.58).
narrative_ontology:constraint_metric(intelligence_analyst_autonomy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(intelligence_analyst_autonomy, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intelligence_analyst_autonomy, tangled_rope).
narrative_ontology:human_readable(intelligence_analyst_autonomy, "Intelligence Analyst Autonomy Constraint").
narrative_ontology:topic_domain(intelligence_analyst_autonomy, "political/organizational/institutional").

domain_priors:requires_active_enforcement(intelligence_analyst_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intelligence_analyst_autonomy, political_leadership).
narrative_ontology:constraint_beneficiary(intelligence_analyst_autonomy, intelligence_agency_management).
narrative_ontology:constraint_victim(intelligence_analyst_autonomy, analyst_professional_integrity).
narrative_ontology:constraint_victim(intelligence_analyst_autonomy, analytical_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ANALYST (SNARE) — Career advancement, clearance retention, and pension eligibility depend on institutional loyalty. Exit from the intelligence apparatus is economically catastrophic. The analyst faces suppression through classification status, compartmentalization, and explicit institutional pressure to conform analysis to policy preferences. Minimal coordination benefit — the constraint exists to ensure subordination, not to solve a collective action problem.
constraint_indexing:constraint_classification(intelligence_analyst_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROFESSIONAL ANALYST COHORT (TANGLED ROPE) — Constrained by career risk, clearance vulnerability, and retaliation mechanisms (reassignment, investigation, isolation). However, the institutional system genuinely coordinates their work: classification systems, compartmentalization, and hierarchical review do solve real counterintelligence and operational security problems. The extraction is asymmetric — analysts bear suppression costs while leadership captures decision priority. Both coordination and asymmetric extraction are present.
constraint_indexing:constraint_classification(intelligence_analyst_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTELLIGENCE AGENCY MANAGEMENT (ROPE) — Experiences the constraint as pure coordination. The analyst autonomy control system solves the real problem of ensuring operational security, preventing leaks, and maintaining chain-of-command discipline. Management can arbitrage between agencies and has exit options (restructuring, reprioritization). Net beneficiary — extraction flows toward this agent, but the system also genuinely coordinates their security and operational functions.
constraint_indexing:constraint_classification(intelligence_analyst_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL LEADERSHIP (SNARE) — Uses analyst autonomy constraints to extract convenient analysis. Leadership can suppress inconvenient findings (pressure to justify policy decisions, cherry-pick intelligence, suppress dissent). High extraction with minimal coordination benefit — the system serves to subordinate expert judgment to policy preference. Leadership has exit options (restructure intelligence agencies, replace analysts) and high power, but the classification remains snare because the binding mechanism is pure coercion with no genuine coordination function.
constraint_indexing:constraint_classification(intelligence_analyst_autonomy, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL INTELLIGENCE SYSTEM (PITON) — The formal autonomy constraints (classification system, compartmentalization, oversight committees) are largely theatrical. Modern intelligence agencies use informal pressure, cultural norms, and career incentives to enforce conformity rather than explicit restrictions. The formal institutional apparatus persists through inertia — justified as security necessity but functioning primarily as theater for legitimacy. Theater ratio reflects the gap between formal (advisory independence committees, inspector general offices, congressional oversight) and actual (career consequences, clearance threats, reassignment) mechanisms.
constraint_indexing:constraint_classification(intelligence_analyst_autonomy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (CIVILIZATIONAL) — The constraint exhibits both genuine coordination functions (operational security, compartmentalization solving real counterintelligence problems) and systematic extraction (policy conformity pressure, career subordination). The analytical observer sees this as a hybrid mechanism where legitimate security coordination is layered with extractive autonomy suppression. The extraction is not accidental — it is built into the system architecture. Suppression is enforced both structurally (classification creates barrier to exit) and culturally (institutional loyalty norms).
constraint_indexing:constraint_classification(intelligence_analyst_autonomy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intelligence_analyst_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intelligence_analyst_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intelligence_analyst_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intelligence_analyst_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intelligence_analyst_autonomy, TR),
    TR >= 0.70.

:- end_tests(intelligence_analyst_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts analyst autonomy in service of political alignment, but not through pure coercion — the system provides genuine coordination benefits (compartmentalization, security protocols, operational effectiveness). The extraction is sophisticated: it operates through career incentive misalignment rather than explicit prohibition. The value has increased over the interval as informal enforcement (career consequences) has replaced formal oversight (explicit rules), suggesting that the equilibrium has shifted toward greater extraction. Suppression (0.68): High. Multiple suppression mechanisms operate: classification status creates barrier to exit; clearance dependency locks analysts into the system; institutional culture enforces conformity; retaliation against dissent is structural (reassignment, investigation, career termination). The suppression is not absolute — some analysts do dissent and survive — but costs are substantial. Theater ratio (0.64): Moderately high. The formal autonomy constraints (declassification review, oversight committees, independent analysis mandates) provide institutional legitimacy, but actual enforcement operates through informal mechanisms (career gatekeeping, institutional pressure, peer conformity). The gap between formal and actual enforcement has widened over the interval, reflecting increased theater to cover extraction.
 *
 * PERSPECTIVAL GAP:
 *   The individual analyst sees pure extraction (Snare) — institutional loyalty required, exit blocked, autonomy suppressed with minimal coordination benefit. The analyst cohort sees mixed coordination and extraction (Tangled Rope) — the compartmentalization genuinely coordinates security, but extraction is asymmetric. Agency management sees pure coordination (Rope) — the analyst autonomy constraint solves security and operational problems without experiencing extraction themselves. Political leadership sees pure extraction opportunity (Snare) — analysts can be pressured to support preferred analysis; no coordination benefit, pure subordination. The institutional system itself (Piton) — the formal apparatus persists through inertia while informal enforcement does the actual work. The analytical observer at civilizational scope sees the full hybrid (Tangled Rope) — both coordination and extraction are structural, the extraction is not accidental but built into the system architecture. The perspectival gaps reveal who benefits (leadership, management) and who bears costs (analysts, analytical integrity). No single perspective produces Rope — the coordination benefits accrue primarily to management and security institutions, not to the constrained agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual analyst directionality: High victim status + trapped exit → d ≈ 0.92 → f(d) ≈ 1.38. The analyst bears full extraction cost and has minimal exit options. Analyst cohort: Moderate victim status (mixed role) + constrained exit → d ≈ 0.75 → f(d) ≈ 1.08. Some benefit from coordination infrastructure, but bear primary suppression costs. Management: Beneficiary status + arbitrage exit → d ≈ 0.12 → f(d) ≈ -0.02. Benefits from coordination control, has exit options, net negative effective extraction (subsidized by the constraint). Leadership: Beneficiary status (extraction target) + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12. Extracts analyst autonomy, has high exit options, experiences the constraint as favorable. The directionality derivation shows why the individual analyst's chi is high (experienced extraction is substantial) while management's is near zero or negative (the constraint benefits them). The national scope (σ=1.0) does not amplify or dampen the base extractiveness values — national-scope constraints operate at standard verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves potential mandatrophy (Is this coordination or extraction?) by distinguishing the genuine coordination function (compartmentalization, security protocols, operational effectiveness) from the extractive overlay (conformity pressure, career subordination, political alignment). The tangled rope classification captures both: compartmentalization genuinely coordinates intelligence work (Rope element), but the constraint is also actively enforced to extract analyst conformity and suppress dissent (Snare element). The mandatrophy is resolved by recognizing that the constraint serves two functions for two different agents — coordination for management (genuine security need), extraction for analysts (career subordination). A pure Rope reading would miss the extraction. A pure Snare reading would miss the coordination. Tangled Rope is the accurate classification because both are structurally present and neither is reducible to the other. The political leadership's snare perspective is partially distinct — leadership extracts analysis without needing genuine coordination (from their perspective, the system is pure subordination). The analytical observer at civilizational scope sees the full architecture: the constraint is systemically designed to combine coordination and extraction, and this is not a defect but a feature — the extraction IS how the coordination is enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_subordination_boundary,
    'What portion of analyst autonomy constraint serves legitimate operational security vs. serves political subordination?',
    'Comparative analysis of intelligence performance in systems with high analyst autonomy (private research organizations, academic institutes, some allied services) vs low autonomy (intelligence agencies with explicit policy conformity pressure). Measure: accuracy of long-range forecasting, rate of corrected assessments, institutional responsiveness to disconfirming evidence.',
    'If operational security genuinely requires low autonomy: constraint may be partially legitimate coordination. If autonomous analysts match or exceed performance: suppression is extractive rather than functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_subordination_boundary, empirical, 'Proportion of autonomy constraint serving security vs. subordination').

omega_variable(
    exit_option_classification_ambiguity,
    'Is analyst exit truly ''trapped'' or is it ''identity_locked'' — constrained primarily by psychological commitment to institutional role rather than material barriers?',
    'Post-exit trajectory analysis: analysts who leave intelligence work — do they report financial recovery within 2-3 years? Do they adopt alternative professional identities (journalism, academia, consulting)? If yes: exit is constrained/mobile rather than trapped. If barriers persist (security clearance limits job options, institutional liability): exit is materially trapped.',
    'If identity_locked: the constraint''s suppression mechanism is partially internalized; analysts carry institutional control with them even after exit. If trapped: suppression is purely structural; exit would materially improve autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_classification_ambiguity, empirical, 'Whether analyst exit is materially trapped or identity-locked').

omega_variable(
    formal_vs_informal_enforcement_mechanism,
    'Do explicit institutional rules (classification mandates, compartmentalization, oversight) enforce analyst conformity, or do informal mechanisms (career advancement gatekeeping, peer pressure, internalized institutional values) do the actual enforcement?',
    'Documentary analysis of formal mechanisms: explicit conformity requirements in regulations, written policies, classified directives. Ethnographic analysis of actual enforcement: where do career consequences come from? Do analysts report written policy violation as the stated reason for retaliation, or do they report ''informal pressure'' and ''institutional culture''?',
    'If formal mechanisms are primary: constraint is explicit and potentially challengeable through institutional reform. If informal mechanisms dominate: constraint operates through culture and identity, making it more resistant to formal legal challenge (piton classification appropriate). Theater ratio reflects this gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_vs_informal_enforcement_mechanism, empirical, 'Whether enforcement is formal rule-based or informal cultural mechanism').

omega_variable(
    clearance_as_structural_lock,
    'Does security clearance status function as a structural barrier to exit or as a credentialing asset that improves career options outside intelligence?',
    'Labor market analysis: salary premium/penalty for intelligence background in private sector (defense contractors, consulting, tech security roles). Clearance portability data: what percentage of analysts retain clearance utility in civilian roles vs. lose access to post-intelligence markets?',
    'If clearance is barrier: it reduces exit options (makes exit more constrained/trapped). If clearance is asset: it improves options (makes exit more mobile/arbitrage). This affects directionality calculation for individual analyst perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clearance_as_structural_lock, empirical, 'Whether security clearance functions as exit barrier or career asset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intelligence_analyst_autonomy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intanal_tr_t0, intelligence_analyst_autonomy, theater_ratio, 0, 0.48).
narrative_ontology:measurement(intanal_tr_t20, intelligence_analyst_autonomy, theater_ratio, 20, 0.58).
narrative_ontology:measurement(intanal_tr_t40, intelligence_analyst_autonomy, theater_ratio, 40, 0.64).
narrative_ontology:measurement(intanal_tr_t10, intelligence_analyst_autonomy, theater_ratio, 10, 0.53).

% Extraction over time
narrative_ontology:measurement(intanal_be_t0, intelligence_analyst_autonomy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(intanal_be_t20, intelligence_analyst_autonomy, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(intanal_be_t40, intelligence_analyst_autonomy, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(intanal_be_t10, intelligence_analyst_autonomy, base_extractiveness, 10, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intelligence_analyst_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(intelligence_analyst_autonomy, intelligence_collection_prioritization).
narrative_ontology:affects_constraint(intelligence_analyst_autonomy, policy_validation_bias).
narrative_ontology:affects_constraint(intelligence_analyst_autonomy, analyst_career_incentive_alignment).

% DUAL FORMULATION NOTE:
% Intelligence analyst autonomy is a high-level constraint that operates through multiple mechanisms: formal classification/compartmentalization (genuine coordination), informal career gatekeeping (extraction), and institutional culture (identity enforcement). The constraint family includes downstream structural pressures on collection prioritization (resources allocated based on policy alignment rather than intelligence requirement) and policy validation bias (intelligence agencies confirm policy hypotheses more often than would be expected from unbiased analysis). Each family member has distinct ε values reflecting different aspects of the autonomy suppression system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intelligence_analyst_autonomy, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
