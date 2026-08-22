% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__formalist_employment_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary (Contract + Supervision)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the formalist_employment_reading of
 *   the contested employment_boundary kernel. Under this reading, employment
 *   is defined strictly by formal contract and direct human supervision;
 *   platform workers engaged via digital intermediaries are classified as
 *   independent contractors, placing them outside the employment relationship
 *   and its protective framework. The reading is not a neutral description —
 *   it is a structural arrangement that excludes platform workers from the
 *   victim set of employment precarity (framed as chosen flexibility),
 *   excludes platforms from the beneficiary obligations of employment (social
 *   insurance, wage floors, collective bargaining), and externalizes costs to
 *   workers and state systems. The high ε (0.78 at interval end) reflects
 *   this extraction. The constraint is actively enforced through
 *   classification litigation, legislative lobbying, and platform Terms of
 *   Service that mandate contractor status. Theater ratio is low-moderate
 *   (0.28) because the coordination function (matching supply/demand) is real
 *   but increasingly overshadowed by the extraction function. Resistance is
 *   moderate (0.58) from worker organizing, misclassification lawsuits, and
 *   legislative pushes (e.g., ABC tests, EU Platform Work Directive).
 *
 * KEY AGENTS:
 *   - platform_companies: Primary beneficiary (institutional/arbitrage) — sets classification rules, captures value, avoids employment costs
 *   - platform_workers: Primary victim (powerless/trapped) — bears precarity, lacks protections, exit options structurally constrained
 *   - state_social_insurance_systems: Secondary victim (institutional/constrained) — absorbs unfunded liabilities for injury, unemployment, health
 *   - traditional_employers: Secondary victim (organized/constrained) — competes against platforms that externalize employment costs
 *   - venture_capital_backers: Beneficiary (powerful/arbitrage) — funds platform models predicated on labor cost arbitrage
 *   - insurance_premium_avoiders: Beneficiary (organized/mobile) — platforms and adjacent firms that avoid workers' comp/unemployment premiums
 *   - labor_regulators: Agenda setter (institutional/analytical) — enforces or contests the boundary through classification tests
 *   - appellate_courts: Observer (analytical/analytical) — adjudicates boundary cases, shaping the constraint's operational meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.78).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.72).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, snare).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary (Contract + Supervision)").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, 'd4cd3e88-ecf8-493f-8209-81b83a9af651').
narrative_ontology:cs_kernel_codification('d4cd3e88-ecf8-493f-8209-81b83a9af651', formalized).
narrative_ontology:cs_authority_grounding('d4cd3e88-ecf8-493f-8209-81b83a9af651', lineage).
narrative_ontology:cs_interpretation_layer_present('d4cd3e88-ecf8-493f-8209-81b83a9af651').
narrative_ontology:cs_reading_relation('d4cd3e88-ecf8-493f-8209-81b83a9af651', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('d4cd3e88-ecf8-493f-8209-81b83a9af651', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('d4cd3e88-ecf8-493f-8209-81b83a9af651', foundational, employment_requires_formal_contract_and_direct_supervision).
narrative_ontology:cs_axiom_status(employment_requires_formal_contract_and_direct_supervision, holdable).
narrative_ontology:cs_axiom_grounding('d4cd3e88-ecf8-493f-8209-81b83a9af651', employment_requires_formal_contract_and_direct_supervision, conventional).
narrative_ontology:cs_axiom('d4cd3e88-ecf8-493f-8209-81b83a9af651', foundational, platform_work_is_voluntary_flexible_entrepreneurship).
narrative_ontology:cs_axiom_status(platform_work_is_voluntary_flexible_entrepreneurship, holdable).
narrative_ontology:cs_axiom_grounding('d4cd3e88-ecf8-493f-8209-81b83a9af651', platform_work_is_voluntary_flexible_entrepreneurship, instrumental).
narrative_ontology:cs_reference_frame('d4cd3e88-ecf8-493f-8209-81b83a9af651', classical_employment_contract_law).
narrative_ontology:cs_drift_state('d4cd3e88-ecf8-493f-8209-81b83a9af651', platform_economy_maturity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d4cd3e88-ecf8-493f-8209-81b83a9af651', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, venture_capital_backers).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, insurance_premium_avoiders).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_social_insurance_systems).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, traditional_employers).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, contractual_freedom_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, entrepreneurial_opportunity_narrative).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, regulatory_neutrality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the platform infrastructure that mediates labor. Write the Terms of Service that classify workers as independent contractors. Control algorithmic dispatch, rating, pricing, and deactivation — the de facto supervision system. Lobby legislatures and litigate to preserve the contractor classification. Capture the spread between transaction value and worker pay while avoiding employment taxes, benefits, and liability.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__formalist_employment_reading, platform_companies, beneficiary).

% Perform the core labor of the platform (rides, deliveries, tasks) under algorithmic control. Formally independent but economically dependent on a single platform for income. Bear all costs of equipment, insurance, downtime, and injury. Cannot negotiate rates or terms; deactivation ends income instantly. 'Flexibility' means choosing when to work within the platform's demand curve, not choosing whether to work. Exit means unemployment or another platform with identical terms.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, biographical, trapped, global).

% Administer unemployment insurance, workers' compensation, disability, and health systems funded by employer premiums. When platforms classify workers as contractors, premiums go unpaid but claims arise — injuries on the job, income loss from deactivation, health crises without employer coverage. The system absorbs these costs as unfunded liabilities or denies claims, shifting burden to general taxation or leaving workers uncovered. Cannot opt out of the liability; political pressure to reform classification is the only structural lever.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_social_insurance_systems, payer,
    institutional, generational, constrained, national).

% Employ workers under formal contracts, paying wages, benefits, payroll taxes, and compliance costs. Compete for labor and customers against platforms that externalize these same costs via contractor classification. Face competitive pressure to reduce own employment standards or adopt platform-like models. Organized in industry associations that lobby for 'level playing field' classification reform, but individual firms also benefit from platform services (delivery, gig labor).
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers, payer,
    organized, biographical, constrained, national).

% Fund platform companies at valuations predicated on labor cost arbitrage — the gap between what platforms would pay under employment law and what they pay under contractor classification. Returns depend on the classification holding. Deploy capital to lobbying, litigation, and PR defending the formalist boundary. Exit via IPO or acquisition before classification risk materializes; portfolio diversification across jurisdictions hedges regulatory risk.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, venture_capital_backers, beneficiary,
    powerful, biographical, arbitrage, global).

% Firms (not only platforms) that structure labor arrangements to avoid workers' compensation, unemployment insurance, and health benefit obligations by using contractor classifications. Benefit from the formalist reading's precedent and the infrastructure of platform labor markets. Mobile across sectors — if one contractor model is challenged, shift to another. Low direct exposure to platform-specific risk.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, insurance_premium_avoiders, beneficiary,
    organized, biographical, mobile, national).

% Enforce labor standards, adjudicate misclassification claims, and implement classification tests (ABC test, economic reality test, EU Directive criteria). Subject to political pressure from platforms (lobbying, litigation) and workers (organizing, complaints). Resource-constrained relative to platform legal teams. Can shift the constraint's operation by changing enforcement priorities or adopting new tests, but face capture risk and legislative override.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate boundary cases: does this platform's algorithmic control constitute 'supervision'? Does economic dependence override contract form? Precedent sets the operational meaning of the formalist definition. Not a party to the extraction but shapes the constraint's enforcement surface. Decisions can expand or contract the victim set without legislative action.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, appellate_courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches sporadic labor supply with real-time demand via digital infrastructure: reduces search costs, enables instant payment, provides reputation systems, coordinates logistics at scale. Solves a genuine coordination problem for on-demand services.
% TRANSFER_FUNCTION: Moves employment costs (social insurance, wage floors, benefits, liability, training) from platforms to workers and state systems. Moves economic surplus from worker labor to platform revenue and investor returns. The formalist boundary is the valve: it defines the employment relationship out of existence for platform work, making the transfer legally permissible.
% ABSENT_VOICES: Platform workers themselves — especially those deactivated without recourse, injured on the job without coverage, or organizing across borders — are structurally excluded from the classification decision. Their voices appear only in litigation or protests, not in the rulemaking that defines their status. Migrant workers on platforms face additional exclusion (language, immigration status, platform dependency).
% DISAPPEARANCE_RATIONALE: If the formalist boundary vanished overnight, platforms would face immediate reclassification exposure: employment taxes, benefits mandates, collective bargaining rights, algorithmic accountability requirements. Worker income would rise (wage floors, overtime, benefits); platform margins would compress; investor returns would fall. Some platforms would shift to employed-fleet models; others would exit markets. The state would see reduced unfunded liabilities. Traditional employers would gain competitive parity. The labor market would reorganize around the true cost of labor.
% FOUNDING_PROBLEM: Early digital platforms (c. 2009–2015) emerged to match underutilized assets (cars, spare time, skills) with demand — a genuine coordination problem where traditional employment was too rigid for sporadic, low-intensity work. The formalist classification allowed rapid scaling without employment law compliance.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and venture capital attest the founding problem is live: flexibility demand persists, traditional employment remains rigid, and the coordination function is still needed. Worker organizations, labor economists, the ILO, the EU Commission, and multiple national inquiries attest the problem has shifted: platforms now mediate primary-income work under algorithmic control, and the formalist classification functions as cost externalization, not coordination enablement. The corroboration split maps to the beneficiary/victim divide.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__formalist_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because platforms capture the economic surplus of labor while externalizing its social costs (insurance, stability, training) to workers and the state. The formalist definition is the mechanism: by defining employment narrowly (contract + supervision), it renders the platform's algorithmic control and economic dependence legally invisible. Suppression is high (0.72) because the constraint's persistence depends on active enforcement — lobbying against ABC tests, litigation to preserve contractor classification, Terms of Service that workers must accept to access the platform. Theater ratio (0.28) reflects that the matching/coordination function is genuine but the arrangement's stability now depends more on the extraction it enables than the coordination it provides. Accessibility collapse (0.65) is significant: once a worker enters platform work, alternative employment paths narrow (skill atrophy, resume gaps, algorithmic reputation lock-in). Resistance (0.58) is real but fragmented — individual lawsuits, localized organizing, legislative fights that platforms resource heavily.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are the structural agenda setters and primary beneficiaries: they write the classification rules, control the algorithmic infrastructure, and capture the extraction (d ≈ 0.1). Venture capital backers and insurance premium avoiders are secondary beneficiaries with high exit options (d ≈ 0.15–0.2). Platform workers are the primary targets: economically dependent, algorithmically controlled, with trapped exit options (d ≈ 0.9). State social insurance systems are institutional targets: they absorb externalized costs but cannot exit the liability (d ≈ 0.8). Traditional employers are constrained targets: they bear competitive disadvantage but have some political voice (d ≈ 0.6). Labor regulators and courts are analytical/institutional observers with analytical exit (d ≈ 0.5). The formalist reading's claim that workers 'choose flexibility' is the ideological cover that dampens perceived extraction for the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reducing transaction costs for sporadic labor — is contested (see six_questions). The formalist reading persists not because it solves that problem better than alternatives, but because it enables a specific extraction structure: platforms capture coordination value + labor surplus while socializing risk. The constraint is a snare, not a tangled rope, because the coordination story (flexibility, entrepreneurship) is cover for asymmetric extraction; the constraint would collapse if workers had genuine exit or if platforms bore the true cost of labor. No party benefits enough to maintain it voluntarily — platforms would prefer the status quo but would adapt if forced; workers would exit if they could. The arrangement persists through active suppression of alternatives (legislative capture, litigation, narrative control).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint one reading of the contested employment_boundary kernel, and if so, which reading?',
    'This omega records the committer frame: this constraint instantiates the formalist_employment_reading of the employment_boundary kernel. Sibling readings are substantive_employment_reading and hybrid_security_reading. The structural disagreement is located in the definition of employment (formal contract + supervision vs. economic dependence + algorithmic control vs. third category) and the resulting victim/beneficiary assignments for platform workers.',
    'If this reading is the only one institutionalized, platform workers are excluded from employment protections, platforms avoid beneficiary obligations, and extraction externalizes to workers and state systems. If a sibling reading gains institutional force, the constraint''s classification and ε would change structurally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Records that this is one reading of a contested kernel, not a standalone constraint.').

omega_variable(
    algorithmic_control_vs_supervision_boundary,
    'Does algorithmic management constitute ''direct supervision'' such that platform workers would fall inside the formalist employment definition?',
    'Case law evolution on whether algorithmic dispatch, rating, and deactivation systems meet the legal test for direct supervision. Legislative clarification of ''supervision'' in platform contexts.',
    'If algorithmic control = supervision, the formalist reading''s own criteria would capture platform workers as employees, collapsing the victim exclusion and shifting ε toward the hybrid/substantive readings. If not, the formalist boundary holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_vs_supervision_boundary, conceptual, 'Whether the formalist definition''s own terms (supervision) expand to include algorithmic management.').

omega_variable(
    flexibility_choice_vs_structural_coercion,
    'Do platform workers genuinely choose flexibility, or is ''flexibility'' a structural coercion where the alternative is unemployment?',
    'Longitudinal labor market data on platform worker entry/exit patterns, reservation wages, and alternative employment availability. Survey evidence on worker preference formation under precarity.',
    'If choice is genuine, the formalist reading''s exclusion of platform workers from victim set has normative plausibility. If coercive, the exclusion is a structural fiction masking extraction — the snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_choice_vs_structural_coercion, empirical, 'Whether the ''chose flexibility'' narrative reflects preference or constrained choice.').

omega_variable(
    state_insurance_externalization_magnitude,
    'What is the quantitative magnitude of cost externalization from platforms to state social insurance systems (unemployment, workers'' comp, health) under the formalist reading?',
    'Fiscal incidence studies comparing platform worker coverage gaps to traditional employee costs. Actuarial modeling of unfunded liabilities transferred to public systems.',
    'High externalization magnitude increases ε for the state_social_insurance_systems victim seat and strengthens the snare classification. Low magnitude would suggest the extraction is primarily worker-borne, shifting the beneficiary/victim balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_insurance_externalization_magnitude, empirical, 'Scale of fiscal transfer from platforms to public systems via the employment boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emp_boundary_formalist_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(emp_boundary_formalist_tr_t0, observed).
narrative_ontology:measurement(emp_boundary_formalist_tr_t5, employment_boundary__formalist_employment_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(emp_boundary_formalist_tr_t5, observed).
narrative_ontology:measurement(emp_boundary_formalist_tr_t10, employment_boundary__formalist_employment_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(emp_boundary_formalist_tr_t10, observed).
narrative_ontology:measurement(emp_boundary_formalist_tr_t15, employment_boundary__formalist_employment_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(emp_boundary_formalist_tr_t15, observed).
narrative_ontology:measurement(emp_boundary_formalist_tr_t20, employment_boundary__formalist_employment_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(emp_boundary_formalist_tr_t20, observed).
narrative_ontology:measurement(emp_boundary_formalist_tr_t25, employment_boundary__formalist_employment_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(emp_boundary_formalist_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(emp_boundary_formalist_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(emp_boundary_formalist_be_t0, observed).
narrative_ontology:measurement(emp_boundary_formalist_be_t5, employment_boundary__formalist_employment_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(emp_boundary_formalist_be_t5, observed).
narrative_ontology:measurement(emp_boundary_formalist_be_t10, employment_boundary__formalist_employment_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(emp_boundary_formalist_be_t10, observed).
narrative_ontology:measurement(emp_boundary_formalist_be_t15, employment_boundary__formalist_employment_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement_basis(emp_boundary_formalist_be_t15, observed).
narrative_ontology:measurement(emp_boundary_formalist_be_t20, employment_boundary__formalist_employment_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(emp_boundary_formalist_be_t20, observed).
narrative_ontology:measurement(emp_boundary_formalist_be_t25, employment_boundary__formalist_employment_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement_basis(emp_boundary_formalist_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(emp_boundary_formalist_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(emp_boundary_formalist_su_t0, observed).
narrative_ontology:measurement(emp_boundary_formalist_su_t5, employment_boundary__formalist_employment_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(emp_boundary_formalist_su_t5, observed).
narrative_ontology:measurement(emp_boundary_formalist_su_t10, employment_boundary__formalist_employment_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(emp_boundary_formalist_su_t10, observed).
narrative_ontology:measurement(emp_boundary_formalist_su_t15, employment_boundary__formalist_employment_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(emp_boundary_formalist_su_t15, observed).
narrative_ontology:measurement(emp_boundary_formalist_su_t20, employment_boundary__formalist_employment_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(emp_boundary_formalist_su_t20, observed).
narrative_ontology:measurement(emp_boundary_formalist_su_t25, employment_boundary__formalist_employment_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(emp_boundary_formalist_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__formalist_employment_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, platform_algorithmic_control).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, social_insurance_funding_gap).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, traditional_employment_erosion).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, worker_classification_litigation).

% DUAL FORMULATION NOTE:
% This constraint (formalist_employment_reading) and substantive_employment_reading are dual formulations of the employment_boundary kernel: same referent (platform work), opposite boundary criteria (formal vs. substantive), mutually exclusive victim/beneficiary assignments. The hybrid_security_reading is a third formulation attempting to resolve the binary. All three form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__formalist_employment_reading, institutional, 0.15).
constraint_indexing:directionality_override(employment_boundary__formalist_employment_reading, powerless, 0.92).
constraint_indexing:directionality_override(employment_boundary__formalist_employment_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
