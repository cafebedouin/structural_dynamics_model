% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
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
 *   human_readable: Formalist Employment Boundary (Contract-and-Supervision Reading)
 *   domain: economic/labor/social_policy
 *
 * SUMMARY:
 *   A body of statutory definitions and common-law tests (agency/control
 *   tests, IRS factor tests, ABC-style variants) fixes employment status by
 *   contract formation and direct supervision. Platform intermediaries
 *   classify drivers and couriers as independent contractors, placing them
 *   outside minimum-wage, overtime, unemployment-insurance, and
 *   workers'-compensation coverage. The avoided cost appears as platform
 *   margin, investor valuation, and somewhat lower consumer prices, while
 *   income volatility, injury risk, and downturn support migrate to workers'
 *   households and public safety-net budgets. Persistence is actively
 *   engineered: arbitration clauses, deactivation discipline, sustained
 *   litigation, and heavily funded ballot initiatives have defended the line
 *   wherever legislatures moved to redraw it. KEY AGENTS (by structural
 *   relationship): - platform_labor_intermediaries: agenda-setting
 *   beneficiary (institutional/arbitrage) — writes the agreements, funds the
 *   defense, collects the labor-cost spread - platform_gig_workers: primary
 *   target (powerless/constrained) — bears volatility, uncovered injury risk,
 *   and unpaid time - state_social_insurance_budgets: payer with latent
 *   agenda capacity (institutional/constrained) — absorbs externalized
 *   downturn and injury costs - compliance_bearing_traditional_employers:
 *   secondary target (powerful/constrained) — competes against rivals freed
 *   from payroll obligations they still carry - on_demand_service_consumers:
 *   incidental beneficiary (moderate/mobile) — receives price and speed
 *   advantages - platform_growth_capital: beneficiary (powerful/arbitrage) —
 *   valuations rest on the subsidized unit economics -
 *   courts_and_labor_regulators: administering seat
 *   (institutional/constrained) — adjudicate the boundary's width -
 *   general_taxpayer_base: excluded seat (moderate/trapped) — funds the
 *   safety net without a procedural voice Family note: this file is one
 *   reading of the employment_boundary kernel. The substantive reading
 *   (economic-dependence criterion) and the hybrid reading (third-category
 *   construction) are separate constraints with different victim sets,
 *   different beneficiary obligations, and different epsilon values; they are
 *   linked through network.affects_constraints rather than averaged into this
 *   file.
 *
 * KEY AGENTS:
 *   - platform_labor_intermediaries — agenda-setting beneficiary (institutional/arbitrage): drafts contractor agreements, funds ballot defenses, collects the spread between contractor terms and employment terms
 *   - platform_gig_workers — primary target (powerless/constrained): bear income volatility, vehicle and insurance costs, uncovered injury risk, and unpaid waiting time
 *   - state_social_insurance_budgets — payer with latent agenda capacity (institutional/constrained): absorb downturn support and injury costs without corresponding contributions; hold legislative power blunted by ballot preemption
 *   - compliance_bearing_traditional_employers — secondary target (powerful/constrained): carry payroll taxes and benefit obligations their platform competitors avoid
 *   - on_demand_service_consumers — incidental beneficiary (moderate/mobile): receive low prices and fast fulfillment partly financed by unbundled labor protections
 *   - platform_growth_capital — beneficiary (powerful/arbitrage): equity returns rest on unit economics priced on the classification spread
 *   - courts_and_labor_regulators — administering seat (institutional/constrained): apply and uphold the tests that set the boundary's operating width
 *   - general_taxpayer_base — excluded seat (moderate/trapped): finance the safety-net absorption without any seat in classification design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.7).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.65).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary (Contract-and-Supervision Reading)").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "economic/labor/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '0087b4a1-ac03-4e91-ae92-f33454d2f48d').
narrative_ontology:cs_kernel_codification('0087b4a1-ac03-4e91-ae92-f33454d2f48d', formalized).
narrative_ontology:cs_authority_grounding('0087b4a1-ac03-4e91-ae92-f33454d2f48d', lineage).
narrative_ontology:cs_interpretation_layer_present('0087b4a1-ac03-4e91-ae92-f33454d2f48d').
narrative_ontology:cs_reading_relation('0087b4a1-ac03-4e91-ae92-f33454d2f48d', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('0087b4a1-ac03-4e91-ae92-f33454d2f48d', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('0087b4a1-ac03-4e91-ae92-f33454d2f48d', foundational, contractual_form_dispositive_of_status).
narrative_ontology:cs_axiom_status(contractual_form_dispositive_of_status, holdable).
narrative_ontology:cs_axiom_grounding('0087b4a1-ac03-4e91-ae92-f33454d2f48d', contractual_form_dispositive_of_status, conventional).
narrative_ontology:cs_axiom('0087b4a1-ac03-4e91-ae92-f33454d2f48d', secondary, supervision_absence_presumes_autonomy).
narrative_ontology:cs_axiom_status(supervision_absence_presumes_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('0087b4a1-ac03-4e91-ae92-f33454d2f48d', supervision_absence_presumes_autonomy, empirically_contingent).
narrative_ontology:cs_reference_frame('0087b4a1-ac03-4e91-ae92-f33454d2f48d', common_law_supervision_test_baseline).
narrative_ontology:cs_drift_state('0087b4a1-ac03-4e91-ae92-f33454d2f48d', post_algorithmic_management_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0087b4a1-ac03-4e91-ae92-f33454d2f48d', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_labor_intermediaries).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_growth_capital).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, on_demand_service_consumers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_gig_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_social_insurance_budgets).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, compliance_bearing_traditional_employers).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, formalist_classification_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, freedom_of_contract_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates marketplaces matching customers to drivers and couriers. Drafts the contractor agreements and arbitration clauses, funds ballot initiatives and litigation that defend the classification, and adjusts app design and terms when rulings threaten it. Collects the difference between contractor-term labor costs and what employment terms would require. When a jurisdiction tightens its test, it can shift operations, renegotiate, or sponsor a statewide ballot measure overriding the legislature.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_labor_intermediaries, agenda_setter,
    institutional, generational, arbitrage, global).

% Holds equity in the intermediaries. The classification spread underwrites the unit economics and growth narrative on which valuations rest. Exits through secondary sales or public offerings once milestones priced on subsidized labor costs are achieved; portfolio mobility means no single jurisdiction's rules bind it for long.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_growth_capital, beneficiary,
    powerful, biographical, arbitrage, global).

% Buys rides, deliveries, and errands at prices and speeds made possible in part by unbundled labor protections. Switches easily between competing apps and bears no direct obligation. Carries a small indirect share through taxes that fund safety-net programs covering platform workers during downturns and injuries.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, on_demand_service_consumers, beneficiary,
    moderate, immediate, mobile, national).

% Drives and delivers under algorithmic dispatch. Bears vehicle costs, insurance gaps, income volatility, and unpaid waiting time; engaged hours have no wage floor, and no unemployment-insurance contributions accrue on their behalf. Can decline trips, but refusal lowers dispatch priority, and account deactivation ends income immediately. Multi-apping spreads risk across apps but every alternative sits inside the same contractor classification, and organizing efforts meet barriers that classification itself creates — no collective-bargaining coverage, arbitration-forced individual disputes. Vehicle purchases lock capital into continuing the work.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_gig_workers, payer,
    powerless, biographical, constrained, global).

% Public unemployment-insurance funds, Medicaid, food assistance, and earned-income credits absorb costs when classified-out workers hit downturns or injuries without employer contributions behind them. Legislatures hold the formal power to redefine the tests — several adopted broader standards — but face funded ballot preemption and interstate competition for platform presence, which blunts exercise of that power. Cannot exit the obligation to serve residents.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_social_insurance_budgets, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__formalist_employment_reading, state_social_insurance_budgets, agenda_setter).

% Taxi fleets, courier firms, restaurants, and logistics companies that pay payroll taxes, unemployment-insurance contributions, minimum wages, and workers'-compensation premiums. Compete against platforms whose prices embed the avoided versions of these same costs. Cannot adopt the contractor form without dismantling the supervised, premises-bound operations that define them, so they lobby for enforcement parity instead.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, compliance_bearing_traditional_employers, payer,
    powerful, biographical, constrained, national).

% Adjudicate classification disputes applying common-law agency tests, statutory definitions, and administrative guidance. Each ruling widens or narrows the boundary's operating width; appellate precedent locks in outcomes beyond a single case. Bound by prior doctrine and by the statutes legislatures enact, they administer the line rather than choose it afresh.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, courts_and_labor_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Funds the safety-net programs that absorb externalized labor costs through general taxation. Has no procedural seat in classification design, rulemaking, or litigation; its interests surface only diffusely through fiscal politics and budget hearings, mediated by representatives balancing many other claims.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, general_taxpayer_base, excluded,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_labor_intermediaries).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a bright-line, low-dispute test separating employment from independent contracting: it cuts classification transaction costs, gives firms and workers predictable status, and preserves legal room for genuinely self-directed contracting that both sides of such relationships value.
% TRANSFER_FUNCTION: Moves the costs of labor protection — wage floors, overtime premiums, unemployment-insurance and workers'-compensation contributions, paid leave — away from platforms onto individual workers (income volatility, unpaid time, equipment, injury risk) and onto public safety-net budgets that backstop the gaps.
% ABSENT_VOICES: Workers deactivated for asserting employee-like claims, workers in jurisdictions where organizing invites retaliation, and the general taxpayer base financing the safety-net absorption have no procedural seat; they stand outside hearing rooms dominated by platform counsel and funded ballot committees. The excluded seat general_taxpayer_base records the fiscal principal's absence; deactivated and retaliated-against workers appear in the record only as anonymized arbitration respondents.
% DISAPPEARANCE_RATIONALE: If the contractor classification collapsed overnight, platform pricing would rise to carry employment-term costs, coverage in thin-margin markets and off-peak hours would contract, thousands of firms' unit economics and valuations would reprice, workers' take-home patterns would shift toward wages-withholdings, and state insurance funds would begin receiving the contributions they currently forgo — the on-demand service economy would reorganize around whichever intermediate forms the successor rules permit.
% FOUNDING_PROBLEM: Industrial labor law needed a line between dependent wage earners, who received protective regulation, and genuinely autonomous traders, who did not; the contract-plus-supervision test drew that line in an era when work meant fixed premises, visible bosses, and verifiable hours.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: labor-law historians trace the test's master-servant and factory-supervision lineage, and international labor-body reporting on platform work documents the mismatch between premises-era supervision and algorithmic dispatch — both attest that the test answered a workplace that no longer matches platform operations. Platform-commissioned surveys attest continuing flexibility demand, so attestation runs along the dispute's own lines; the sources standing outside all benefiting parties support the historical founding problem and document the contest over whether it remains the right question.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.70 because the gap between contractor terms and employment terms is large and persistent — payroll taxes, unemployment-insurance and workers'-compensation contributions, paid leave, and wage floors together are conventionally estimated at roughly a quarter of wage costs — and the burden divides between individual households and public budgets. Suppression is authored at 0.65 and read as a RAW STRUCTURAL PROPERTY, unscaled by power or scope: the boundary persists through arbitration enforcement, deactivation discipline, litigation strategy, and ballot measures, not through participant consensus. Theater_ratio at 0.32 reflects a real but growing performative layer — 'flexibility' and 'entrepreneurship' framing performed as consent while dispatch control tightens — alongside the test's continuing genuine classificatory work for truly autonomous contractors. Accessibility_collapse at 0.40: alternatives remain partly open — leaving platform work, jurisdictions adopting different tests, portable-benefit experiments. Resistance at 0.68: sustained litigation, strike actions, countervailing ballot campaigns, and supranational directive pressure; note that this resistance is coalition-generated, since individual worker seats are powerless but the class has demonstrated mobilizing capacity. The claim (tangled_rope) and these metrics were authored independently: I believe the structure is a genuine coordination function (a low-dispute bright-line test that also serves genuinely independent contractors) fused with asymmetric extraction (cost externalization), and the metrics describe how it currently operates. Temporal series run on ONE shared grid — points 0/3/6/9/12/15 correspond approximately to 2010/2013/2016/2019/2022/2025 — with all three tracked metrics authored at every point. The trajectory is monotonic, not cyclical: platform scale-up deepened the spread, and each legislative challenge (AB5, directive negotiations) triggered an enforcement ratchet (ballot lock-ins, arbitration hardening), which is why suppression_requirement is tracked here — this story specifically traces enforcement-capacity intensification, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the intermediary seat, the arrangement is a coordination structure it built and defends: predictable classification, flexible supply, low dispute costs. From the worker seat, the same structure operates as enforced exclusion from protections that adjacent workers receive for identical tasks. The state-budget seat computes an unfunded mandate: it pays for downturns it did not price. The sharpest same-level contrast is lateral: platform_labor_intermediaries and compliance_bearing_traditional_employers hold similar nominal power (both 'powerful'/institutional-grade commercial actors) yet occupy opposite relations to the boundary — one wrote the rule it benefits from, the other is bound by the older regime the rule circumvents. Their exit options diverge accordingly: intermediaries hold arbitrage (relocation across jurisdictions, agreement redesign, app restructuring), while traditional employers are constrained by their own supervision-intensive business models, which they cannot convert to the contractor form without dismantling themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure maps to directionality as follows. platform_labor_intermediaries and platform_growth_capital sit near the beneficiary pole: they collect the spread and can move capital and operations freely. on_demand_service_consumers are declared beneficiaries with mobile exit, placing them near-symmetric-low; their indirect tax incidence nudges their true position slightly toward symmetric, but not enough to warrant an override. platform_gig_workers are declared victims with nominally 'constrained' exit, but the derivation would UNDERSHOOT their target-position: multi-apping presents as mobility while every alternative sits inside the same contractor classification, and deactivation threat binds conduct within each app. Hence the explicit override: power_atom 'powerless' to d_value 0.85, encoding near-full-target position despite the apparent mobility. state_social_insurance_budgets are victims with constrained exit — high directionality — and their latent agenda-setting capacity does not lower it, because ballot preemption and interstate competition for platform presence keep the capacity mostly unexercised. compliance_bearing_traditional_employers bear a comparative-extraction: the constraint costs them competitiveness relative to exempt rivals, so they compute near the target end despite never signing a platform contract. courts_and_labor_regulators declare no beneficiary or victim position; their seat derives from the canonical fallback for an administering institutional actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — drawing a protect/non-protect line for premises-bound supervised work — is CONTESTED, not dead, so the mismatch consumer finds no clean zombie flag here; but the theater series rising from 0.18 to 0.32 marks early atrophy of the test's descriptive accuracy even as its allocative function hardens. Two drift paths are live. If the flexibility rhetoric detaches fully from observable scheduling control, theatrical maintenance dominates a hardened enforcement core — a piton-flavored shell over snare-flavored mechanics. If instead the substantive reading captures courts or legislatures, the boundary rearranges wholesale. The tangled_rope classification is what prevents mislabeling in both directions: a pure-rope reading would erase the excluded victim set (workers, state budgets, compliant competitors); a pure-snare reading would erase the genuine bright-line service the test performs for genuinely autonomous contractors — consultants, tradespeople, true freelancers — whom the substantive reading would sweep into protection they did not seek. The hybrid reading exists precisely because the coordination and extraction components are hard to separate; this file holds them together as one structure, as the delta specification requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_employment_boundary,
    'This file instantiates the formalist_employment_reading of the employment_boundary kernel; how would the sibling readings (substantive_employment_reading, hybrid_security_reading) restructure the constraint''s victim and beneficiary sets?',
    'Comparative analysis of jurisdictions operating under each reading (ABC-test states versus ballot-locked contractor carve-outs versus EU presumption-of-employment regimes): measure who gains protected status, who inherits contribution obligations, and where externalized costs land.',
    'Under the substantive reading, platform_gig_workers enter the protected set and platform_labor_intermediaries inherit employer obligations, relocating epsilon from workers and state budgets to platform margins; under the hybrid reading a constructed intermediate category splits the current binary and changes which costs are externalized at all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_employment_boundary, conceptual, 'Committer structure: one reading of the employment_boundary kernel; siblings are separate constraints with different epsilon and different party sets.').

omega_variable(
    flexibility_preference_authenticity,
    'How much of surveyed platform-worker preference for contractor status reflects authentic preference versus adaptive rationalization formed under income necessity?',
    'Longitudinal panels comparing stated preferences at entry, under earnings adequacy, and after exposure to equivalent employment offers; discrete-choice designs that decouple schedule freedom from benefit loss.',
    'If preference is largely adaptive, the ''chose flexibility'' justification thins and effective extraction on the worker seat rises; if authentic, part of the measured burden is a price workers knowingly accept, damping chi on that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_preference_authenticity, empirical, 'Authenticity of the consent that legitimizes exclusion from the employment relation.').

omega_variable(
    externalization_fiscal_magnitude,
    'What is the annual magnitude of safety-net spending (Medicaid, SNAP, EITC, uncompensated injury care, UI drawn without employer contributions) attributable to contractor-classified platform work?',
    'Linked administrative datasets matching platform-work participation to public-assistance uptake against matched employee controls; state unemployment-insurance solvency actuarial reviews.',
    'Large magnitudes center the extraction story on the transfer into public budgets (raising the salience of state budgets and diffuse taxpayers as payers); small magnitudes confine the extraction to the worker-intermediary dyad.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_fiscal_magnitude, empirical, 'Size of the fiscal transfer that externalized labor costs impose on public budgets.').

omega_variable(
    operative_constraint_frame_underdetermination,
    'Is the binding constraint the legal classification test, or the app-level control architecture (dispatch algorithms, rating systems, deactivation) that makes the formalist test bite?',
    'Counterfactual from jurisdictions mandating algorithmic-transparency and deactivation-due-process rules without touching classification: if worker outcomes improve materially, the code layer carries coercive weight independent of the legal label.',
    'Framing the app architecture as the operative constraint yields a more directly coercive classification for platform control practices than the legal-boundary frame computed here; this story authors the legal-boundary frame per its constraint_id.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_constraint_frame_underdetermination, conceptual, 'CS-framing under-determination: legal test versus code architecture as the constraint proper.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eb_formalist_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(eb_formalist_tr_t0, observed).
narrative_ontology:measurement(eb_formalist_tr_t3, employment_boundary__formalist_employment_reading, theater_ratio, 3, 0.21).
narrative_ontology:measurement_basis(eb_formalist_tr_t3, observed).
narrative_ontology:measurement(eb_formalist_tr_t6, employment_boundary__formalist_employment_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(eb_formalist_tr_t6, observed).
narrative_ontology:measurement(eb_formalist_tr_t9, employment_boundary__formalist_employment_reading, theater_ratio, 9, 0.27).
narrative_ontology:measurement_basis(eb_formalist_tr_t9, observed).
narrative_ontology:measurement(eb_formalist_tr_t12, employment_boundary__formalist_employment_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(eb_formalist_tr_t12, observed).
narrative_ontology:measurement(eb_formalist_tr_t15, employment_boundary__formalist_employment_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(eb_formalist_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(eb_formalist_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(eb_formalist_be_t0, observed).
narrative_ontology:measurement(eb_formalist_be_t3, employment_boundary__formalist_employment_reading, base_extractiveness, 3, 0.49).
narrative_ontology:measurement_basis(eb_formalist_be_t3, observed).
narrative_ontology:measurement(eb_formalist_be_t6, employment_boundary__formalist_employment_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement_basis(eb_formalist_be_t6, observed).
narrative_ontology:measurement(eb_formalist_be_t9, employment_boundary__formalist_employment_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement_basis(eb_formalist_be_t9, observed).
narrative_ontology:measurement(eb_formalist_be_t12, employment_boundary__formalist_employment_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(eb_formalist_be_t12, observed).
narrative_ontology:measurement(eb_formalist_be_t15, employment_boundary__formalist_employment_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement_basis(eb_formalist_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(eb_formalist_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(eb_formalist_su_t0, observed).
narrative_ontology:measurement(eb_formalist_su_t3, employment_boundary__formalist_employment_reading, suppression_requirement, 3, 0.47).
narrative_ontology:measurement_basis(eb_formalist_su_t3, observed).
narrative_ontology:measurement(eb_formalist_su_t6, employment_boundary__formalist_employment_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement_basis(eb_formalist_su_t6, observed).
narrative_ontology:measurement(eb_formalist_su_t9, employment_boundary__formalist_employment_reading, suppression_requirement, 9, 0.58).
narrative_ontology:measurement_basis(eb_formalist_su_t9, observed).
narrative_ontology:measurement(eb_formalist_su_t12, employment_boundary__formalist_employment_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(eb_formalist_su_t12, observed).
narrative_ontology:measurement(eb_formalist_su_t15, employment_boundary__formalist_employment_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(eb_formalist_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, identity_coordination).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the employment boundary' conflates three structurally distinct claims distinguished by their classification criterion. This file (formalist reading: contract form plus direct supervision decides) is the doctrinally upstream member — the common-law baseline from which the others emerged. substantive_employment_reading (economic dependence plus algorithmic control decides) is downstream, citing precisely the mismatches this reading externalizes; its epsilon is higher on the platform-worker axis and its beneficiary obligations extend to platform_labor_intermediaries. hybrid_security_reading constructs a third category and partially dissolves both binaries. Per the epsilon-invariance principle these are three files with three stable epsilon values and three distinct victim sets, linked bidirectionally through affects_constraints; no single file may hedge across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__formalist_employment_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
