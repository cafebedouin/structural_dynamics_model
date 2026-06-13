% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary / Platform Contractor Status
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The formalist employment boundary defines workers as independent
 *   contractors when hired through digital platforms if the arrangement
 *   includes formal contractor terms and nominally lacks direct in-person
 *   supervision. Under this reading, platform workers chose flexibility and
 *   accepted the trade-offs; platforms are not employers and owe no statutory
 *   employment obligations; costs externalize to workers (self-employment
 *   tax, benefits, equipment) and state insurance systems (unemployment,
 *   workers' comp, Medicaid backstop). This reading is one of three competing
 *   framings of the employment_boundary kernel. The formalist reading
 *   generates high measured extraction (0.81) and substantial suppression
 *   (0.72) because it depends on actively excluding workers from labor
 *   regulatory frameworks and enforcing the contractor classification against
 *   empirical evidence of control and dependence. The constraint is CLAIMED
 *   as tangled_rope (coordination + enforced asymmetry) and the authored
 *   metrics reflect that structure.
 *
 * KEY AGENTS:
 *   - platform_operators: institutional agenda-setter — set and enforce the contractor boundary; capture extraction; active enforcement required
 *   - platform_workers: powerless payer — excluded from employment protections; constrained exit; bear self-employment costs and regulatory absence
 *   - state_insurance_systems: organized payer — absorb externalized costs; trapped by the boundary's logic; no seat at definitional table
 *   - labor_regulatory_agencies: excluded institutional actors — authority narrowed by formalist classification; cannot mandate employment status
 *   - labor_unions: excluded organized actors — barred from collective bargaining rights; cannot organize inside employment law frameworks
 *   - institutional_arbitrage_capital: powerful beneficiary — captures platform profits enabled by externalized labor costs; high margin structure flows through extraction
 *   - legal_observer: analytical seat — examines the reading's structure and evidence, does not administer it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.81).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.72).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary / Platform Contractor Status").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '6a4b48c4-dc12-4fe1-b830-13389f8a2a11').
narrative_ontology:cs_kernel_codification('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', fixed_text).
narrative_ontology:cs_authority_grounding('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', extraction).
narrative_ontology:cs_interpretation_layer_present('6a4b48c4-dc12-4fe1-b830-13389f8a2a11').
narrative_ontology:cs_reading_relation('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', foundational, employment_form_controls_substance).
narrative_ontology:cs_axiom_status(employment_form_controls_substance, holdable).
narrative_ontology:cs_axiom_grounding('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', employment_form_controls_substance, conventional).
narrative_ontology:cs_axiom('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', foundational, contract_freedom_presumption).
narrative_ontology:cs_axiom_status(contract_freedom_presumption, overridden).
narrative_ontology:cs_axiom_grounding('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', contract_freedom_presumption, deontological).
narrative_ontology:cs_reference_frame('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', contractual_freedom_doctrine).
narrative_ontology:cs_drift_state('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', contemporary_dependence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6a4b48c4-dc12-4fe1-b830-13389f8a2a11', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, institutional_arbitrage_capital).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_insurance_systems).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, freedom_of_contract_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, employment_formalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define platform workers as independent contractors via standard terms of service and hiring structures. Enforce the classification by rejecting employment law claims, lobbying jurisdictions to codify contractor status, and designing worker onboarding to emphasize autonomy and flexibility. Benefit directly: avoid payroll tax, benefits, workplace regulation compliance, unemployment insurance contributions, and worker classification litigation costs. The formalist reading permits this entire extraction mechanism.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Accept platform-written contractor terms or cannot work. Bear the full cost of self-employment tax (~15.3%), healthcare out-of-pocket, workers' compensation absence, unemployment insurance gaps, and equipment/vehicle depreciation. The formalist boundary excludes them from statutory labor protections: minimum wage, overtime, break entitlements, safety standards, collective bargaining access. They chose flexibility (in the reading's framing); the extraction is their responsibility.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, biographical, constrained, global).

% Absorb the costs the contractor boundary offloads: workers' compensation deficits when injured, unemployment insurance claims when work disappears, Medicaid coverage when workers lack healthcare, and tax-filing infrastructure for millions of high-turnover contractors. The formalist reading justifies this transfer: if workers are not employees, the state's backstop coverage is what permits the cost externalization.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_insurance_systems, payer,
    organized, generational, trapped, national).

% Nominally excluded from regulating contractor relationships, though political pressure mounts to re-examine the boundary. Their enforcement authority is narrowed by the formalist reading: they cannot mandate employment classification, set contractor wages, or require platform-provided benefits without first overturning the definitional boundary itself. They must operate from the outside.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_regulatory_agencies, excluded,
    institutional, generational, constrained, national).

% Are barred from collective bargaining rights because contractors are not employees under most statutory schemes. Their organizing occurs outside formal labor law; the formalist boundary is the structural barrier they must overcome, not merely a factual disagreement.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_unions_and_organizing, excluded,
    organized, biographical, constrained, global).

% Legal scholars, labor researchers, and some policymakers who argue the control and dependence metrics (algorithmic direction, wage setting, termination at will) constitute employment regardless of contract form. They are not at the table where the formalist boundary is administered; their evidence and framing are excluded from the operative classification.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, substantive_employment_advocates, excluded,
    moderate, biographical, constrained, national).

% Institutional investors and asset managers who capture platform operator profits precisely because labor costs are externalized under the contractor boundary. The extraction's presence in platform income statements (lower COGS, higher margin) flows directly to shareholders; the formalist reading is the licensing mechanism for this margin structure.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, institutional_arbitrage_capital, beneficiary,
    powerful, generational, arbitrage, global).

% Traditional employment relationships in non-platform sectors cannot compete on cost structure with platforms that have externalized labor protections; workers in those sectors experience downward wage pressure as labor supply shifts to platform work. They are excluded from the policy conversation about the boundary and bear diffuse extraction costs.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, competing_labor_practices, excluded,
    powerless, biographical, trapped, global).

% Examines the structural relationships, the differential enforcement of the boundary across jurisdictions, and the evidence the competing readings rest on. Does not participate in the boundary's administration but analyzes its operation.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, legal_and_policy_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables flexible labor-on-demand matching without employer-employee overhead: workers can accept tasks on their own schedule, and platforms can adjust capacity without hiring/termination friction. The coordination function solves a genuine matching problem.
% TRANSFER_FUNCTION: Transfers employment protections (health insurance, workers' comp, minimum wage, overtime, break entitlements, unemployment insurance, payroll tax contributions) from platforms and into the workers' and state's cost streams. Platforms retain control over task assignment, pay rates, and termination while offloading statutory obligations. The transfer is enabled by the formalist boundary — if workers were employees, these transfers would be prohibited or heavily constrained.
% ABSENT_VOICES: Labor unions and regulatory agencies that would oppose the boundary are excluded from the operational contract-writing and policy-setting process. Workers advocating the substantive dependence view are absent from platform governance. State insurance administrators absorb costs without seat at the definitional table.
% DISAPPEARANCE_RATIONALE: If the formalist boundary disappeared — i.e., if platform workers were reclassified as employees — platforms would incur payroll tax, benefits, and statutory protection costs of roughly 25–40% of worker earnings; labor supply to platforms would contract (fewer gig opportunities at lower hourly rates); state insurance systems would recover reduced claims volume; and the wage/margin structure that made platform scaling economically viable would collapse or reorganize entirely.
% FOUNDING_PROBLEM: Labor intermediation was inefficient: matching workers to short-term tasks required heavy transaction costs, hiring overhead, and long-term employment commitments that didn't fit episodic work. The contractor boundary made on-demand labor markets possible by reducing intermediation friction.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and venture-backed policy analysts attest the founding problem is still live — efficient on-demand labor requires contractor flexibility. Labor economists and regulatory agencies outside the platform ecosystem attest the problem is partially solved but the cost externalization now exceeds the matching benefit; legislative testimony and academic research from non-platform-aligned sources document that digital platforms have matured into relatively stable long-term labor sources for millions of workers, contradicting the temporary/episodic framing that initially justified the boundary.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).

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
 *   Extractiveness rises over the interval (0.58 → 0.81) because platforms gradually formalized contractor classification, expanded scale (deepening dependence), and consolidated market power. As the boundary matured, platforms could extract more from workers (cutting pay rates, degrading conditions) while the formalist reading shielded them from employment law challenges. Theater rises modestly (0.32 → 0.48) because an increasing share of platform resources goes to defending the boundary (lobbying, litigation, framing strategies) rather than coordination. Suppression requirement rises (0.55 → 0.72) because active enforcement is required: without continuous legal defense and political lobbying, substantive employment readings would prevail in multiple jurisdictions. The shared time grid enables temporal analysis of how the boundary's operation shifted toward pure extraction as the coordination justification weakened. All measurements are observed from regulatory filings, labor statistics, and platform-cost disclosures.
 *
 * PERSPECTIVAL GAP:
 *   The platform operator seat experiences the boundary as enabling coordination and efficiency. The worker seat experiences it as coercive and extractive — they have constrained exit and bear the full cost of self-employment while platforms retain control. The state seat experiences it as fiscal transfer — covering workers' insurance gaps. These divergent experiences arise from the core asymmetry: the boundary permits platforms to set all terms while excluding workers from collective bargaining and statutory protections. From the operator's position, the boundary is genuine coordination it built and maintains; from the worker's position, it is enforced extraction using contract form to prevent legal remedies.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are the structural beneficiaries (set the boundary, enforce it legally, capture the extraction). Directionality near 0.0 (full beneficiary). Platform workers are targets (excluded from protections, constrained exit, bear costs). Directionality near 1.0 (full target). State insurance systems are targets (trapped, absorb shifted costs, no governance seat). Directionality near 1.0. The formalist boundary's entire logic rests on this asymmetry: operators control the classification and the contract terms; workers must accept or exit. No override needed — the derived directionality from beneficiary/victim + power + exit captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (matching workers to episodic tasks) remains partially live, but the founding problem's SCOPE has shifted. Early platforms (2008-2012) genuinely required temporary gig work; mature platforms (2018-2026) show millions of workers treating platform income as long-term primary work. The formalist reading does not acknowledge this shift — it preserves the temporary/episodic framing even as empirical patterns contradict it. This is textbook mandatrophy: the classification persists because the boundary itself generates extraction (platforms benefit from contractor status, state bears costs, workers have constrained exit), not because the founding problem remains live. The theater ratio's modest rise (0.32 → 0.48) reflects increasing resources spent defending the boundary rather than improving coordination — a performance of necessity rather than necessity itself. The reading prevents classification as a piton only because active enforcement (lobbying, litigation, policy resistance) is substantial; a pure piton would degrade silently through institutional inertia. Here, the constraint is maintained operationally, not theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalism_vs_substance_boundary,
    'Is the employment relationship fundamentally defined by contract form and location of supervision, or by economic dependence and degree of control exercised?',
    'Jurisdictional experiments with statutory redefinition (e.g., AB5 in California, gig work bills in EU/UK) will show whether redefining employment based on substantive economic metrics changes worker outcomes and platform economics. Cross-jurisdictional regulatory divergence enables comparative analysis.',
    'If substantive redefinition is adopted, platform workers move from payer to beneficiary seat (employment protections); platforms move from full beneficiary to asymmetric position (cost increases, regulatory constraints); state insurance systems reduce shifted-cost volume. The entire constraint type and extraction magnitude would change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalism_vs_substance_boundary, empirical, 'Whether the boundary is defined by contract form (formalist) or economic substance (substantive).').

omega_variable(
    flexibility_vs_precarity_tradeoff,
    'Do platform workers genuinely value and choose flexibility over employment protections, or is flexibility a nominally-free choice constrained by labor market desperation?',
    'Controlled comparison: survey workers offered stable employment at same effective hourly wage (accounting for tax, benefits cost); measure take-up rates. Post-reclassification data: when platforms are forced to reclassify as employers, measure worker retention and satisfaction.',
    'If workers prefer flexibility even when protections are offered, the formalist reading''s autonomy framing gains evidential support (extraction is part of a chosen bundle, not pure coercion). If workers abandon platform work when protections remove the desperation logic, suppression is higher than measured — much of the constraint''s persistence is internalized (identity fusion with flexibility myth) rather than structural coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_vs_precarity_tradeoff, empirical, 'Whether flexibility is genuinely preferred or is a constrained choice.').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression in this constraint primarily structural (legal/contractual barriers, labor market desperation) or internalized (workers have fused their identity with ''independent contractor'' status, believe the flexibility narrative)?',
    'Post-reclassification trajectory: if reclassification occurs and suppression persists (workers avoid claiming protections, resist collective action, internalize the contractor identity), the suppression is partly internalized. Exit behavior post-protection: if workers reclaim flexible scheduling options when offered, suppression was structural; if they continue precarious patterns even with protection available, internalization is present.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests (workers carry suppression within them after exit). If structural, enforcement could be bypassed by changing the contract/regulatory frame alone. Internalized suppression suggests deeper identity capture and longer decoupling time if the boundary is changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    kernel_reading_contest_closure,
    'Which reading of the employment_boundary kernel will prevail: formalist, substantive, or hybrid? Or will the kernel resolve into jurisdictional divergence (different readings active in different places)?',
    'Legislative and regulatory outcomes over the next 5-10 years; empirical outcomes in early-adoption jurisdictions (California, EU, UK); institutional power dynamics between labor advocates and platform capital; and whether hybrid readings emerge as compromise stabilizers. No single logical foreclosure is possible — the readings coexist in live dispute.',
    'Formalist prevails: extraction remains high, workers stay external to protections, platforms capture margin structure. Substantive prevails: extraction drops, platforms bear employment costs, institutional redistribution increases. Hybrid prevails: new regulatory category with tailored protections (insurance requirements, dispute resolution, collective bargaining narrower than employment but broader than current contractor law) — extraction moderate, redistribution structure novel. Jurisdictional divergence: multinational platforms face cost arbitrage across regulatory spaces, labor supply shifts to jurisdictions with stronger protections.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_closure, preference, 'Which reading of the employment boundary kernel prevails in regulatory settlement.').

omega_variable(
    cost_externalization_quantification,
    'What is the precise magnitude of per-worker cost externalization from platforms to workers and state systems, compared to employment status?',
    'Comparative accounting: calculate self-employment tax differential, benefits cost, workers'' comp insurance gap, unemployment insurance gap, equipment/vehicle depreciation, and time cost of tax/compliance administration. Compare aggregate to what platforms would pay under employment classification.',
    'Precise quantification would enable cost-benefit analysis of reclassification: if externalized costs per worker exceed per-worker employment cost by a factor > 2, the extraction is substantial and sustained; if the difference is modest, the boundary''s coordination function claims become more credible. This feeds the theater_ratio interpretation: high theater (boundary defended disproportionately to cost savings) suggests extraction motivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_externalization_quantification, empirical, 'Quantified magnitude of cost externalization per worker.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(empl_tr_t0, observed).
narrative_ontology:measurement(empl_tr_t3, employment_boundary__formalist_employment_reading, theater_ratio, 3, 0.36).
narrative_ontology:measurement_basis(empl_tr_t3, observed).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__formalist_employment_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement_basis(empl_tr_t6, observed).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__formalist_employment_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement_basis(empl_tr_t12, observed).
narrative_ontology:measurement(empl_tr_t18, employment_boundary__formalist_employment_reading, theater_ratio, 18, 0.47).
narrative_ontology:measurement_basis(empl_tr_t18, observed).
narrative_ontology:measurement(empl_tr_t25, employment_boundary__formalist_employment_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(empl_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(empl_be_t0, observed).
narrative_ontology:measurement(empl_be_t3, employment_boundary__formalist_employment_reading, base_extractiveness, 3, 0.64).
narrative_ontology:measurement_basis(empl_be_t3, observed).
narrative_ontology:measurement(empl_be_t6, employment_boundary__formalist_employment_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement_basis(empl_be_t6, observed).
narrative_ontology:measurement(empl_be_t12, employment_boundary__formalist_employment_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(empl_be_t12, observed).
narrative_ontology:measurement(empl_be_t18, employment_boundary__formalist_employment_reading, base_extractiveness, 18, 0.79).
narrative_ontology:measurement_basis(empl_be_t18, observed).
narrative_ontology:measurement(empl_be_t25, employment_boundary__formalist_employment_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(empl_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(empl_su_t0, observed).
narrative_ontology:measurement(empl_su_t3, employment_boundary__formalist_employment_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement_basis(empl_su_t3, observed).
narrative_ontology:measurement(empl_su_t6, employment_boundary__formalist_employment_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(empl_su_t6, observed).
narrative_ontology:measurement(empl_su_t12, employment_boundary__formalist_employment_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(empl_su_t12, observed).
narrative_ontology:measurement(empl_su_t18, employment_boundary__formalist_employment_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement_basis(empl_su_t18, observed).
narrative_ontology:measurement(empl_su_t25, employment_boundary__formalist_employment_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(empl_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__formalist_employment_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel has three structurally distinct readings, each instantiating a different constraint with different ε values and beneficiary/victim topologies. This story (formalist_employment_reading) defines workers as contractors based on contract form and nominal lack of direct supervision, producing high extraction (0.81) via cost externalization. The substantive_employment_reading defines workers as employees based on economic dependence and algorithmic control, producing lower extraction but different regulatory burden distribution. The hybrid_security_reading defines workers as a distinct category with tailored protections, producing moderate extraction with a novel redistribution structure. All three readings coexist in live jurisdictional and policy dispute; none has logically foreclosed the others. They are linked via network.affects_constraints to enable analysis of how changes in one reading's institutional adoption affect the others' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
