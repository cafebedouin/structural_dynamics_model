% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Structural Precarity Enabling Platform Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the precarity-extraction reading of the flexible
 *   employment legitimacy kernel: flexible/gig work arrangements are read as
 *   structural precarity that enables platforms to extract surplus value by
 *   externalizing risk and social-insurance costs onto workers and the state,
 *   while algorithmic management substitutes for formal employment discipline
 *   without granting formal employment protections. This is one of three
 *   readings of the same kernel; the market-efficiency reading treats the
 *   identical arrangement as legitimate market-clearing, and the
 *   developmental-state reading treats it as a transitional form requiring
 *   managed formalization. Each reading is authored as its own constraint
 *   with its own ε, per the ε-invariance principle — this file's ε (0.78)
 *   reflects only how the precarity-extraction reading assesses the standing
 *   arrangement, not a synthesis across readings.
 *
 * KEY AGENTS:
 *   - platform_operators: agenda_setter/beneficiary (institutional/arbitrage) — sets classification and algorithmic control terms
 *   - platform_investors: beneficiary (institutional/arbitrage) — captures valuation premium from externalized labor costs
 *   - gig_platform_workers: payer (powerless/trapped) — bears risk, cost, and algorithmic discipline without employment protections
 *   - displaced_formal_sector_workers: payer (moderate/constrained) — squeezed out by benchmark-to-platform wage compression
 *   - public_social_insurance_systems: payer (institutional/trapped) — absorbs deferred fiscal cost of uninsured workers
 *   - consumer_class_service_users: beneficiary (organized/mobile) — receives subsidized convenience
 *   - labor_regulators: excluded/observer (institutional/constrained) — outmatched by platform legal and political resources
 *   - worker_organizing_collectives: excluded (powerless/constrained) — barred from bargaining by the classification itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.78).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.71).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Structural Precarity Enabling Platform Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '6ec14e06-55eb-4169-ab33-6e72118300a9').
narrative_ontology:cs_kernel_codification('6ec14e06-55eb-4169-ab33-6e72118300a9', distributed).
narrative_ontology:cs_authority_grounding('6ec14e06-55eb-4169-ab33-6e72118300a9', distributed).
narrative_ontology:cs_reading_relation('6ec14e06-55eb-4169-ab33-6e72118300a9', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ec14e06-55eb-4169-ab33-6e72118300a9', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('6ec14e06-55eb-4169-ab33-6e72118300a9', foundational, algorithmic_control_constitutes_employment_relation).
narrative_ontology:cs_axiom_status(algorithmic_control_constitutes_employment_relation, holdable).
narrative_ontology:cs_axiom_grounding('6ec14e06-55eb-4169-ab33-6e72118300a9', algorithmic_control_constitutes_employment_relation, empirically_contingent).
narrative_ontology:cs_axiom('6ec14e06-55eb-4169-ab33-6e72118300a9', foundational, risk_externalization_negates_wage_gain_legitimacy).
narrative_ontology:cs_axiom_status(risk_externalization_negates_wage_gain_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6ec14e06-55eb-4169-ab33-6e72118300a9', risk_externalization_negates_wage_gain_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('6ec14e06-55eb-4169-ab33-6e72118300a9', standard_employment_relation_baseline).
narrative_ontology:cs_drift_state('6ec14e06-55eb-4169-ab33-6e72118300a9', post_gig_platform_expansion_2010s_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ec14e06-55eb-4169-ab33-6e72118300a9', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_investors).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, consumer_class_service_users).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_platform_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, displaced_formal_sector_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, public_social_insurance_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the algorithmic dispatch, pricing, and rating systems that allocate work and discipline performance without a formal employment relationship. Classify workers as independent contractors, which moves the cost of equipment, insurance, downtime, and social contributions onto the worker while retaining managerial control over hours, routes, and acceptance rates through app-based scoring. Captures the margin between what would be a full-time wage-plus-benefits cost and what is actually paid out.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, beneficiary).

% Provide capital in exchange for equity premised on labor-cost structures that would not hold under standard employment classification. Benefit from valuation multiples built on the assumption that the workforce remains classified as flexible/contracted rather than employed.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Work under algorithmic assignment and rating systems that can deactivate them without appeal, absorb vehicle/equipment costs and downtime risk, and receive no employer-side contribution to pensions, health insurance, or unemployment funds. Nominal hourly flexibility is offset by unpredictable income, no collective bargaining channel, and dependence on a single or few apps for livelihood — switching platforms rarely escapes the same structure.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_platform_workers, payer,
    powerless, immediate, trapped, national).

% Held standard jobs (taxi drivers, retail clerks, couriers) with benefits and job security before platform entry compressed prices and normalized contractor status across the sector. Face declining formal-sector openings as employers benchmark against platform labor costs, pressuring them toward the same precarious arrangements or out of the sector entirely.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, displaced_formal_sector_workers, payer,
    moderate, biographical, constrained, national).

% Absorb the fiscal shortfall when workers without employer contributions age into inadequate pensions, seek uninsured emergency care, or draw unemployment support platforms never funded. The cost externalized by classification decisions surfaces years later as a public liability, not a platform one.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, public_social_insurance_systems, payer,
    institutional, generational, trapped, national).

% Receive on-demand rides, delivery, and household services at prices substantially below what a fully-costed, benefits-inclusive labor model would sustain. Their convenience and price benefit is directly funded by the wage and social-insurance gap borne by workers and future public budgets.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, consumer_class_service_users, beneficiary,
    organized, immediate, mobile, national).

% Nominally responsible for classifying employment relationships and enforcing labor standards, but face well-resourced legal and lobbying challenges from platforms, jurisdictional fragmentation across gig markets, and political pressure from consumers who favor low prices. Their rulings are frequently litigated, delayed, or legislatively reversed by ballot initiatives platforms fund directly.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, excluded,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, observer).

% Attempt to organize drivers and couriers into associations or unions to bargain over pay algorithms and deactivation policies, but contractor classification statutorily bars most from formal collective bargaining protections, and platforms can route around organizing hubs by adjusting dispatch algorithms across jurisdictions.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, worker_organizing_collectives, excluded,
    powerless, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches intermittent labor supply to spikes in consumer demand for transport, delivery, and on-demand services without requiring either side to commit to fixed schedules — a genuine scheduling coordination problem exists and flexible arrangements do solve part of it.
% TRANSFER_FUNCTION: Moves the costs of insurance, equipment, downtime, and social-security contributions from the platform (which would bear them under standard employment) onto individual workers and, eventually, onto public social-insurance systems; moves price savings to consumers and margin to platform equity holders.
% ABSENT_VOICES: Labor regulators are structurally out-lobbied and litigated into delay; worker organizing collectives are excluded from the bargaining table by the same contractor classification the constraint depends on — the workers most affected have the least capacity to be heard in the venues that set the rules.
% DISAPPEARANCE_RATIONALE: If contractor classification were reversed overnight and platforms had to bear standard employer costs, unit economics for many platform business models would collapse or require substantial price increases; workers would gain benefits and bargaining protections but some flexible scheduling and market access might contract; consumer prices would rise; public social-insurance systems would see reduced future liability. The arrangement is load-bearing for the current price and margin structure, not incidental to it.
% FOUNDING_PROBLEM: Formal-sector labor markets were seen as too rigid to match the spiky, unpredictable demand patterns of on-demand digital services, and workers seeking supplemental or flexible income lacked low-friction ways to sell intermittent labor.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and industry-funded research attest the flexibility problem remains live and central. Independent labor economists, national labor regulators in several jurisdictions (e.g., misclassification rulings and inquiries), and worker organizing collectives — outside the beneficiary set — attest the scheduling-flexibility problem has been substantially solved by app-based dispatch technology itself, and that the persistence of contractor classification now functions primarily as cost externalization rather than a solution to a live coordination problem.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78 by interval end) because the wage/price gains platforms advertise are read, under this reading, as substantially offset by cost-shifting: equipment, downtime, insurance, and social contributions moved onto workers who have no mechanism to price that risk into their compensation. Suppression (0.71) reflects the combination of algorithmic deactivation power (a structural exit barrier functioning like at-will termination without severance or appeal) and statutory bars on collective bargaining that follow directly from contractor classification. Theater ratio (0.42) captures the growing share of platform 'flexibility' messaging and worker-choice framing that functions as legitimating narrative rather than describing actual scheduling autonomy, which the temporal series shows rising as regulatory scrutiny increased. All three tracked metrics share the same seven-point time grid across the story's 24-unit interval.
 *
 * PERSPECTIVAL GAP:
 *   From platform_operators' seat, this reads as a rope: workers choose to log in, consumers choose to order, and the coordination function (spiky demand matched to available supply) is real and valuable. From gig_platform_workers' seat, the same structure computes as extraction under active enforcement: the algorithmic control substitutes for employment supervision while the classification denies the protections supervision normally implies. The engine should register this divergence structurally, not because either party is wrong about their own experience, but because the beneficiary/victim/enforcement data supports a tangled_rope reading — coordination function plus asymmetric extraction plus active maintenance (litigation, lobbying, algorithmic control) required to hold the arrangement in place.
 *
 * DIRECTIONALITY LOGIC:
 *   platform_operators and platform_investors sit at the low-d/beneficiary end: they design the classification and capture the cost differential. gig_platform_workers sit at the high-d/target end: trapped exit (switching platforms rarely escapes algorithmic control or contractor status), immediate time horizon (income insecurity is a daily reality), no coalition-bargaining channel. public_social_insurance_systems is a delayed-target: the cost lands generationally, not immediately, but the institution has no exit from absorbing it. consumer_class_service_users are genuine beneficiaries but at organized power with mobile exit — they can switch services or absorb price increases, unlike workers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — matching intermittent labor supply to spiky demand — was structurally real at the technology's outset and is now substantially solved by the dispatch algorithms themselves; contractor classification's original justification (flexibility requires non-employment status) has been challenged by rulings and research showing algorithmic control is functionally equivalent to employment supervision. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals a live capture-flag condition: the arrangement persists past the point its coordination justification was clearly dominant, and its removal would materially reorganize cost structures — this is precisely the pattern the R5 genealogy interview is designed to surface for downstream reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence_flexible_employment,
    'Is flexible employment classification best read as market-clearing efficiency, transitional developmental-state policy, or structural extraction — and does the correct reading vary by platform sector, jurisdiction, or worker demographic rather than being uniform across the whole gig economy?',
    'Comparative analysis of misclassification litigation outcomes, worker income volatility studies, and social-insurance fiscal impact assessments across jurisdictions with different regulatory postures (e.g., California AB5 vs. UK Supreme Court Uber ruling vs. jurisdictions with no reclassification).',
    'If extraction dominates uniformly, this reading''s classification (tangled_rope trending toward snare) generalizes; if efficiency or transitional dynamics dominate in specific sectors (e.g., high-skill freelance platforms vs. low-wage delivery platforms), the kernel may need sector-specific sibling readings rather than one precarity-extraction constraint covering the whole gig economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_flexible_employment, conceptual, 'Whether one precarity-extraction reading correctly generalizes across gig-economy sectors or masks sector-specific variation better modeled as additional sibling readings.').

omega_variable(
    algorithmic_control_as_employment_test,
    'Does algorithmic dispatch, rating, and deactivation constitute functional employment supervision sufficient to legally reclassify workers as employees, or is it a qualitatively different form of coordination that does not meet employment-control tests?',
    'Accumulating case law across jurisdictions (UK Uber BV v Aslam, California AB5/Prop 22 sequence, EU Platform Work Directive implementation) provides an empirical trajectory on whether courts and legislatures treat algorithmic control as equivalent to managerial control.',
    'If courts converge on treating algorithmic control as employment-equivalent, the extraction reading gains strong legal corroboration and the constraint moves toward snare; if courts consistently distinguish algorithmic coordination from employment control, the market-efficiency reading gains ground and this reading''s extraction claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_as_employment_test, empirical, 'Whether algorithmic management functionally satisfies employment-control legal tests, which is decisive for which kernel reading the legal system ultimately endorses.').

omega_variable(
    social_insurance_cost_shift_magnitude,
    'What is the actual magnitude of the fiscal cost shifted onto public social-insurance systems by contractor classification, relative to the tax revenue and consumer surplus platforms generate?',
    'Actuarial and public-finance studies estimating the present value of foregone employer social-security contributions against platform tax contributions and consumer surplus in specific national contexts.',
    'A large net fiscal shift would strengthen the case that public_social_insurance_systems are genuine victims bearing deferred extraction; a small or offsetting shift would weaken the tangled_rope/snare-trending classification and support the market-efficiency reading''s framing of the arrangement as net-positive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_insurance_cost_shift_magnitude, empirical, 'Whether the fiscal externality onto public insurance systems is large enough to be a decisive component of the extraction claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(flex_tr_t4, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(flex_tr_t16, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(flex_tr_t24, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(flex_be_t4, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(flex_be_t16, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 16, 0.73).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(flex_be_t24, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(flex_su_t4, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(flex_su_t8, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(flex_su_t16, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(flex_su_t24, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__precarity_extraction_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the flexible_employment_legitimacy kernel (network family). market_efficiency_reading authors the same standing contractor-classification arrangement with low ε (legitimate market-clearing, no victims); developmental_state_reading authors it as a scaffold with a sunset condition (transitional toward formalization); this file (precarity_extraction_reading) authors it as tangled_rope with substantial ε (0.78), naming workers and public insurance systems as victims and platforms/investors as beneficiaries. All three share the same underlying arrangement as referent but diverge in claimed type, ε, and beneficiary/victim structure because they diverge in what they take the arrangement's actual function and persistence mechanism to be — per the ε-invariance principle, this divergence is why they are three files, not one file with a parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
