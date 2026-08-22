% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Boundary — Platform Workers as Employees
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   This constraint instantiates the substantive employment reading of the
 *   contested employment_boundary kernel. It asserts that platform workers
 *   are employees because they are economically dependent on a single
 *   platform and subject to algorithmic control that functionally replaces
 *   managerial supervision. The constraint is not the legal test itself
 *   (which varies by jurisdiction) but the structural claim that economic
 *   dependence + algorithmic control = employment, regardless of contract
 *   form. Platforms resist because their unit economics depend on the
 *   contractor classification; workers and traditional employers benefit from
 *   reclassification for different reasons; genuine contractors risk
 *   over-inclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.48).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.62).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Boundary — Platform Workers as Employees").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "economic/political/social").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '2242c0db-9347-4e30-874b-19e175a661e9').
narrative_ontology:cs_kernel_codification('2242c0db-9347-4e30-874b-19e175a661e9', distributed).
narrative_ontology:cs_authority_grounding('2242c0db-9347-4e30-874b-19e175a661e9', extraction).
narrative_ontology:cs_interpretation_layer_present('2242c0db-9347-4e30-874b-19e175a661e9').
narrative_ontology:cs_reading_relation('2242c0db-9347-4e30-874b-19e175a661e9', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('2242c0db-9347-4e30-874b-19e175a661e9', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('2242c0db-9347-4e30-874b-19e175a661e9', foundational, economic_dependence_suffices_for_employment).
narrative_ontology:cs_axiom_status(economic_dependence_suffices_for_employment, holdable).
narrative_ontology:cs_axiom_grounding('2242c0db-9347-4e30-874b-19e175a661e9', economic_dependence_suffices_for_employment, deontological).
narrative_ontology:cs_axiom('2242c0db-9347-4e30-874b-19e175a661e9', foundational, algorithmic_control_is_functional_supervision).
narrative_ontology:cs_axiom_status(algorithmic_control_is_functional_supervision, holdable).
narrative_ontology:cs_axiom_grounding('2242c0db-9347-4e30-874b-19e175a661e9', algorithmic_control_is_functional_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('2242c0db-9347-4e30-874b-19e175a661e9', industrial_employment_paradigm).
narrative_ontology:cs_drift_state('2242c0db-9347-4e30-874b-19e175a661e9', platform_economy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2242c0db-9347-4e30-874b-19e175a661e9', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, traditional_employers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, social_insurance_funds).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_operators).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, independent_contractors_genuine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend economically on a single platform for livelihood; work is allocated, rated, and terminated by opaque algorithms. Currently classified as independent contractors, bearing all risk (no sick pay, no unemployment insurance, no collective bargaining). Reclassification as employees would grant full social insurance, job security, and collective rights — but platforms may exit markets or automate tasks in response.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, beneficiary,
    moderate, biographical, constrained, national).

% Operate algorithmic labor markets (ride-hail, delivery, freelance platforms). Currently avoid employment costs by classifying workers as contractors; business models priced on that arbitrage. Reclassification imposes direct payroll costs (social contributions, benefits, severance) and indirect costs (scheduling rigidity, collective bargaining exposure). Can relocate capital, lobby for carve-outs, or automate — exit is structurally easy.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_operators, payer,
    institutional, generational, arbitrage, global).

% Bear full employment costs while platform competitors externalize them. Substantive boundary levels the playing field: if platform workers are employees, platforms cannot undercut on labor cost arbitrage. But they also fear boundary creep — if 'economic dependence' expands, their own contractors (franchisees, agency staff) may be reclassified.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, traditional_employers, beneficiary,
    organized, biographical, constrained, national).

% Genuinely autonomous professionals (consultants, tradespeople, creatives) who choose contracting for flexibility and tax efficiency. A broad 'economic dependence' test sweeps them into employment, stripping chosen autonomy and imposing compliance costs on their clients. They are not platform workers but the boundary captures them unless carefully carved out.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, independent_contractors_genuine, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, independent_contractors_genuine, payer).

% Receive contributions from newly covered platform workers and their platforms. Solvency improves with broader base; administrative simplicity increases if boundary is clear. But they also administer the boundary — ambiguous cases create litigation load and contribution gaps.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, social_insurance_funds, beneficiary,
    institutional, generational, analytical, national).

% Enforce the employment boundary through inspections, audits, and test cases. Substantive test requires fact-intensive inquiry (algorithm transparency, economic dependence metrics) — more resource-heavy than contract review. Political pressure from both platforms and worker advocates shapes enforcement intensity.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Study boundary effects across jurisdictions: California AB5, EU Platform Work Directive, UK worker status cases. Track whether substantive tests reduce misclassification or create new gray zones, and whether platforms adapt by restructuring work or exiting markets.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, policy_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates social protection (insurance, security, voice) to those who bear economic dependence on a single counterparty, regardless of contractual label. Solves the free-rider problem where platforms externalize risk onto workers and the public safety net.
% TRANSFER_FUNCTION: Moves employment costs (social contributions ~20-30% of pay, benefits, severance liability, collective bargaining obligations) from platform workers and the public purse to platform operators. In reverse, moves autonomy and tax advantages from genuine contractors to the state if boundary overreaches.
% ABSENT_VOICES: Platform workers in jurisdictions without test cases (Global South platforms, emerging gig sectors) — they live the dependence but have no regulatory lever. Small-platform operators who cannot absorb compliance costs and simply exit, leaving workers with nothing. Tax authorities who see classification arbitrage erode the contribution base but are not in the labor-law conversation.
% DISAPPEARANCE_RATIONALE: If the substantive boundary vanished overnight, platforms would revert to pure contractor classification within weeks — contributions stop, workers lose coverage, traditional employers face renewed undercutting. The labor market would reorganize around the arbitrage. Conversely, if the boundary hardened globally, platforms would restructure (employment models, franchise partnerships, automation) or withdraw from some markets.
% FOUNDING_PROBLEM: The industrial-era employment contract was built around a single employer, fixed workplace, and direct supervision. Platform work decouples economic dependence from contractual form — workers depend on one platform economically but are supervised by algorithms, not managers. The founding problem is a category mismatch: social protection institutions cannot see the dependence because it wears a contractor's label.
% FOUNDING_PROBLEM_CORROBORATION: The ILO (2021) and OECD (2022) both document the 'dependent self-employment' gap across 30+ countries — independent of platform lobbying. European Court of Justice (Uber, 2020) and UK Supreme Court (Uber, 2021) ruled that subordinate relationship exists despite contractor contracts, citing algorithmic control. These are outside the platform-worker beneficiary set.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the transfer is real but bounded — platforms pay employment costs they previously avoided, but workers gain protections worth more than the raw transfer. Suppression (0.62) reflects active platform resistance: lobbying for carve-outs (Prop 22), litigation, market exits, algorithmic restructuring to weaken 'control' indicators. Theater (0.38) is rising: platforms increasingly adopt 'partnership' rhetoric and limited benefit funds that mimic employment without conceding the classification. Accessibility collapse (0.45) is partial — alternative models (cooperative platforms, franchise hybrids, portable benefits) exist but are marginal. Resistance (0.55) comes from platforms, genuine contractors, and some workers who value flexibility.
 *
 * PERSPECTIVAL GAP:
 *   From the platform-worker seat, the constraint is a protective mountain — it names a natural category (economic dependence) that the law must recognize. From the platform-operator seat, it is an extractive snare — a retrofit that ignores the novelty of algorithmic coordination. From the genuine-contractor seat, it is a tangled rope with a hidden snare component — coordination (clear boundary) mixed with extraction (over-inclusion). The engine computes these per-seat types from the structural data; the claimed type (tangled_rope) reflects the aggregate structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers are structural beneficiaries (d ~0.15) — they receive protections they currently lack, though constrained exit (platform exit risk) moderates this. Platform operators are structural targets (d ~0.85) — they bear the full cost transfer, with arbitrage-grade exit (capital mobility, automation). Traditional employers are moderate beneficiaries (d ~0.3) — they gain competitive parity but fear boundary creep. Genuine contractors are accidental victims (d ~0.6) — over-inclusion risk is real but mobile exit limits damage. Social insurance funds are institutional beneficiaries (d ~0.2) — broader base, clearer administration. Regulators are agenda-setters (d ~0.5) — they enforce but face resource pressure from fact-intensive test.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint solves a live coordination problem (protection allocation for dependent workers) but carries extraction (platform cost transfer, contractor over-inclusion risk). The founding problem remains live — platform work is growing, not shrinking. Mandatrophy is not resolved; the boundary is contested because the coordination function and extraction are both real and the balance shifts with platform adaptation. Theater rise suggests platforms are building performative compliance structures (benefit funds, 'partner' language) that mimic the coordination function without conceding the boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_overreach_risk,
    'Does the ''economic dependence'' test structurally over-capture genuine independent contractors, or can it be precisely calibrated to platform work?',
    'Empirical tracking of reclassification cases in jurisdictions with substantive tests (California AB5, Spain Rider Law, EU Directive) — measure false positive rate on genuine autonomy.',
    'High over-capture makes the constraint a snare for genuine contractors (extraction without coordination benefit); low over-capture keeps it tangled_rope. Affects victim set breadth and platform operator d-value (if over-capture forces platforms to restructure rather than reclassify).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_overreach_risk, empirical, 'Precision of the substantive boundary — coordination vs. collateral extraction.').

omega_variable(
    platform_adaptation_trajectory,
    'Will platforms absorb employment costs (restructure as employers), exit markets, or automate/redesign work to escape the boundary?',
    'Longitudinal study of platform responses post-reclassification: employment model adoption rates, market withdrawals, algorithmic changes that weaken ''control'' indicators, franchise/licensing pivots.',
    'Absorption confirms coordination function dominates (tangled_rope stable). Exit/automation suggests extraction resistance dominates — constraint becomes snare-like for workers who lose access. Redesign to escape boundary (e.g., multi-apping as structural feature) indicates the constraint is driving evasion, not compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_adaptation_trajectory, empirical, 'Platform structural response to substantive boundary enforcement.').

omega_variable(
    kernel_committer_structure,
    'How does the employment_boundary kernel''s committer structure shape the three readings'' stability?',
    'Trace the authority grounding of each reading: formalist = legislative text + precedent (lineage); substantive = purposive interpretation + comparative convergence (extraction-resistant); hybrid = legislative compromise (distributed). Map drift vectors.',
    'If formalist reading''s authority is textual and the text is stable, it resists drift but may lose legitimacy. If substantive reading''s authority is purposive and purpose evolves (what is ''dependence'' in AI-mediated work?), it drifts but stays live. Hybrid reading''s compromise authority may collapse if one side gains legislative supermajority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer-frame analysis of the employment_boundary kernel''s three readings and their authority groundings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t2015, employment_boundary__substantive_employment_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(empl_tr_t2018, employment_boundary__substantive_employment_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(empl_tr_t2021, employment_boundary__substantive_employment_reading, theater_ratio, 2021, 0.28).
narrative_ontology:measurement(empl_tr_t2024, employment_boundary__substantive_employment_reading, theater_ratio, 2024, 0.35).
narrative_ontology:measurement(empl_tr_t2027, employment_boundary__substantive_employment_reading, theater_ratio, 2027, 0.38).
narrative_ontology:measurement(empl_tr_t2030, employment_boundary__substantive_employment_reading, theater_ratio, 2030, 0.38).

% Extraction over time
narrative_ontology:measurement(empl_be_t2015, employment_boundary__substantive_employment_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(empl_be_t2018, employment_boundary__substantive_employment_reading, base_extractiveness, 2018, 0.28).
narrative_ontology:measurement(empl_be_t2021, employment_boundary__substantive_employment_reading, base_extractiveness, 2021, 0.38).
narrative_ontology:measurement(empl_be_t2024, employment_boundary__substantive_employment_reading, base_extractiveness, 2024, 0.45).
narrative_ontology:measurement(empl_be_t2027, employment_boundary__substantive_employment_reading, base_extractiveness, 2027, 0.48).
narrative_ontology:measurement(empl_be_t2030, employment_boundary__substantive_employment_reading, base_extractiveness, 2030, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2015, employment_boundary__substantive_employment_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(empl_su_t2018, employment_boundary__substantive_employment_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(empl_su_t2021, employment_boundary__substantive_employment_reading, suppression_requirement, 2021, 0.55).
narrative_ontology:measurement(empl_su_t2024, employment_boundary__substantive_employment_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement(empl_su_t2027, employment_boundary__substantive_employment_reading, suppression_requirement, 2027, 0.62).
narrative_ontology:measurement(empl_su_t2030, employment_boundary__substantive_employment_reading, suppression_requirement, 2030, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__substantive_employment_reading, 0.12).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, social_insurance_portability).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, algorithmic_management_transparency).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, collective_bargaining_scope).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__hybrid_security_reading).

% DUAL FORMULATION NOTE:
% Part of the employment_boundary kernel family. This reading (substantive) defines employment by economic dependence + algorithmic control. The formalist reading defines it by contract + direct supervision. The hybrid reading creates a third category. All three share the referent (platform worker classification) but instantiate different constraints with different ε, victims, and enforcement logics. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__substantive_employment_reading, institutional, 0.2).
constraint_indexing:directionality_override(employment_boundary__substantive_employment_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
