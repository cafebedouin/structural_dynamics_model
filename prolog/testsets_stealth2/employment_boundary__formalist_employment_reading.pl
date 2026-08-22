% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary: Contract-and-Direct-Supervision Test Excluding Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The formalist reading of the employment boundary defines employment by
 *   contractual designation and direct supervisory control. Applied to
 *   platform work, the criterion places ride-hail drivers, couriers, and
 *   similar workers outside every protective mandate attached to employment:
 *   no wage floor, no overtime, no unemployment-insurance accrual, no
 *   workers' compensation, no collective-bargaining hook. The saved cost
 *   components do not vanish — they land on the workers' own households and
 *   on public insurance systems that must absorb the same risks without
 *   contribution streams. This file is ONE reading of the employment_boundary
 *   kernel; the substantive and hybrid readings are separate constraint files
 *   with their own epsilon values, victim sets, and classifications, and
 *   nothing about them is averaged into this one. The epsilon referent is the
 *   standing formalist arrangement itself, assessed by this reading's own
 *   lights: even granting the reading's own premises (contract form is
 *   dispositive, bargains are voluntary), the externalization of labor-risk
 *   costs onto non-consenting third parties — public insurance funds and
 *   their taxpayers — registers as extraction, because involuntary
 *   cost-bearing violates the very voluntariness premise the reading stands
 *   on. Claim and metrics are authored independently: the claimed type is
 *   what I judge structurally true; the metrics describe the arrangement's
 *   actual operation.
 *
 * KEY AGENTS:
 *   - platform_operators: Agenda-setting beneficiary (institutional/arbitrage) — drafts the contracts, runs the algorithmic management, funds the classification defense; collects the avoided-cost margin
 *   - platform_workers: Primary target (powerless/constrained) — carry equipment, injury, insurance, and volatility costs outside every protective mandate
 *   - state_social_insurance_systems: Secondary target (institutional/trapped) — absorb downturn, injury, and old-age costs with no contribution stream from the classified-out workforce
 *   - traditional_compliant_employers: Same-level lateral target (powerful/constrained) — carry obligations their platform rivals avoid, competing at a structural cost disadvantage
 *   - platform_service_consumers: Incidental beneficiary (moderate/mobile) — receive below-employee-cost services and administer nothing
 *   - labor_regulators_and_courts: Analytical observer (institutional/analytical) — adjudicate the boundary; their divergent rulings across jurisdictions feed the sibling readings
 *   - gig_worker_organizers: Excluded voice (organized/trapped) — no statutory bargaining hook; present in the dispute only as ballot-fight opponents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.72).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.65).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary: Contract-and-Direct-Supervision Test Excluding Platform Workers").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '8652f8c9-9c9f-4600-936e-5a14d9d7fbdf').
narrative_ontology:cs_kernel_codification('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', fixed_text).
narrative_ontology:cs_authority_grounding('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', lineage).
narrative_ontology:cs_interpretation_layer_present('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf').
narrative_ontology:cs_reading_relation('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', foundational, contract_form_determines_status).
narrative_ontology:cs_axiom_status(contract_form_determines_status, holdable).
narrative_ontology:cs_axiom_grounding('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', contract_form_determines_status, conventional).
narrative_ontology:cs_axiom('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', foundational, voluntary_bargain_presumptively_optimal).
narrative_ontology:cs_axiom_status(voluntary_bargain_presumptively_optimal, holdable).
narrative_ontology:cs_axiom_grounding('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', voluntary_bargain_presumptively_optimal, deontological).
narrative_ontology:cs_reference_frame('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', contract_form_direct_supervision_test).
narrative_ontology:cs_drift_state('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', contemporary_algorithmic_management_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8652f8c9-9c9f-4600-936e-5a14d9d7fbdf', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_service_consumers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_social_insurance_systems).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, traditional_compliant_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the independent-contractor agreements, set the algorithmic management parameters that dispatch, rate, and deactivate workers, and fund the litigation, lobbying, and ballot campaigns that defend the classification. Collect the margin created by not carrying payroll taxes, insurance contributions, benefits, or wage floors. Can restructure subsidiaries, shift operations between jurisdictions, or reprice services to absorb adverse rulings in any single market.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Perform dispatched work under app-set rates, acceptance-rate incentives, and deactivation risk. Carry vehicle, equipment, fuel, self-employment tax, private insurance, and injury costs personally; receive no sick leave, no employer retirement match, no workers' compensation, and no unemployment-insurance accrual. Can multi-app across platforms or take traditional jobs, but income needs keep them inside the channel, and challenging classification individually requires legal resources they do not have.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, immediate, constrained, national).

% Receive rides, deliveries, and errands at prices below what employee-based provision would support, because labor-risk costs sit outside the price. Bear none of the labor-risk costs and administer nothing. Can switch providers instantly and would face higher prices or longer waits under reclassification.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_service_consumers, beneficiary,
    moderate, immediate, mobile, global).

% Operate unemployment insurance, workers'-compensation fallbacks, Medicaid, food assistance, and old-age support. Platforms contribute little or nothing for contractor-classified workers, yet the systems must absorb those workers' downturns, injuries, and old-age poverty. Cannot decline the obligation and are funded by other taxpayers, who silently subsidize the classification.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_social_insurance_systems, payer,
    institutional, generational, trapped, national).

% Restaurants, retailers, care providers, and logistics firms that classify staff as employees and carry payroll taxes, benefits, and comp premiums. Compete against platform rivals whose unit costs exclude those items. Can lobby for parity or defect into misclassification at legal and reputational risk, but cannot shed existing obligations unilaterally.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_compliant_employers, payer,
    powerful, biographical, constrained, national).

% Adjudicate classification disputes, issue guidance, and decide whether to enforce. Reach divergent outcomes across jurisdictions — some courts find the written contract contradicts the actual relationship, others defer to it — producing the cross-jurisdiction variation that sibling readings exploit. Take testimony from all seats and commission the economic analyses the dispute turns on.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_regulators_and_courts, observer,
    institutional, generational, analytical, national).

% Attempt collective bargaining, portable-benefits proposals, and reclassification campaigns. Contractor status removes the statutory bargaining hook, so their proposals rarely enter the drafting conversation except as opponents in ballot fights funded against them. Would contest the premise that workers chose this arrangement freely, but sit outside the room where the classification is maintained.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, gig_worker_organizers, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a bright-line, administrable criterion separating protected employees from autonomous contractors: courts apply one consistent test, businesses can know their obligations before hiring, and genuinely independent freelancers keep multi-client autonomy without being swept into employment mandates designed for dependent workers.
% TRANSFER_FUNCTION: Moves labor-cost components — payroll taxes, unemployment-insurance and workers'-compensation contributions, benefits, wage floors, overtime premia — off platform balance sheets and onto individual workers' own accounts and public insurance systems; moves price surplus to consumers through service pricing that excludes those components.
% ABSENT_VOICES: Gig-worker organizers and deactivated or injured workers, filtered out by arbitration clauses and lacking a statutory bargaining hook, plus future taxpayers who will fund the safety-net shortfall. All three would contest the 'they chose flexibility' premise; none sits inside the conversation where the classification is maintained.
% DISAPPEARANCE_RATIONALE: If the formalist exclusion vanished overnight — platform workers reclassified as employees — platform pricing, availability, and unit economics would rearrange immediately: labor costs would rise, prices and wait times with them, marginal services would fold or automate faster, insurance funds would gain contribution streams, and compliant traditional employers would regain cost parity. The platform economy would reorganize around a different cost structure within quarters.
% FOUNDING_PROBLEM: Courts and administrators needed a workable line between dependent employees, who receive protective mandates, and autonomous contractors, who do not. The formalist test answered with observable contract terms and direct supervisory control, deliberately trading sociological accuracy for administrability and ex-ante predictability.
% FOUNDING_PROBLEM_CORROBORATION: Labor-law historians corroborate that the original administrability problem was real. Sources outside the benefiting parties — ILO reports, academic labor-law scholarship, and the UK Supreme Court's Uber judgment, which expressly found the written contract terms contradicted the actual working relationship — attest that the criteria no longer track economic dependence under algorithmic management. Attestation that the test remains fit for purpose comes almost entirely from platform operators and their funded campaigns, i.e., from inside the benefiting set.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.72 at interval end) because the classification strips mandated cost components from the platform price structure and relocates them onto workers and public systems — a transfer that grows with platform scale, hence the rising series from 0.45 to 0.72 across the interval. Suppression (0.65) is substantial but non-criminal: mandatory arbitration clauses, class-action waivers, deactivation power, and heavily funded legislative defense. The suppression_requirement series rises monotonically (0.35 to 0.65) because this is a genuine enforcement ratchet — the machinery defending the classification (litigation budgets, ballot campaigns, lobbying) matured and hardened as reclassification threats accumulated; the story specifically tracks enforcement-capacity build-up, which is why the series is authored rather than left to the scalar. Theater ratio (0.40) captures the widening gap between 'driver partner / you are your own boss' rhetoric and algorithmically managed dispatch, ratings, and deactivation — the flexibility is partly real (schedule choice survives), so the ratio stays well below piton territory while climbing as the rhetoric decouples from control. Accessibility collapse is moderate (0.50): workers retain exits to traditional employment and between platforms, but within the platform channel the classification is uniform — no major operator offers employee status at scale, so dependence on platform income collapses the alternative in practice. Resistance (0.60) is real and sustained: classification lawsuits, unionization drives, the UK Uber litigation, California's AB5 cycle and Proposition 22 counter-campaign, the EU Platform Work Directive — enough to impose costs, not yet enough to displace the arrangement in its strongholds. All three series run on one shared time grid (points 0, 3, 6, 9, 12, 15) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the operator seat, the arrangement is a contracting framework it built and defends: parties signed, terms are stated, exit exists — a near-coordination picture. From the worker seat, the same documents read as the instrument that stripped every mandate while algorithmic management retained the control; from the state seat, it reads as contribution avoidance with a downstream bill. Same-level lateral dynamics matter: traditional compliant employers and platform operators hold comparable market power in overlapping markets, but the classification differentiates their cost structures — the compliant employer's exit (defect into misclassification) is available only at legal risk, which is why its exit atom is constrained rather than arbitrage. Inter-institutional dynamics: courts and regulators across jurisdictions read the same kernel differently (deferential contract-form readings versus substance-controlling rulings), and that divergence is precisely the raw material the sibling readings institutionalize. Coalition potential for the powerless seat is real but costly: workers can coordinate through organizing and ballot measures, and have, but each coalition attempt has met order-of-magnitude counter-spending, which is itself evidence about the enforcement requirement.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation and no overrides are needed. Platform operators sit nearest the beneficiary pole: they collect the avoided-cost margin, set the rules, and hold arbitrage-grade exit (restructuring, jurisdiction shopping) — derived d near 0.0. Platform service consumers are incidental beneficiaries with mobile exit — low d, lightly damped. Platform workers sit near the target pole: they bear the relocated costs with constrained exit — high d, amplified. State social insurance systems are targets with trapped exit (the obligation cannot be declined) — high d, amplified further by their inability to pass the cost on. Traditional compliant employers are targets with a partial defection discount: their constrained (not arbitrage) exit keeps d elevated but below the workers'. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an administrable line between dependent and autonomous work — has not died; courts still need a criterion, and genuine freelancers still benefit from a bright line. But its platform-era application has drifted: the criterion now functions substantially as the instrument that keeps a large, controlled workforce outside the mandate system, and the enforcement budget spent defending it exceeds what its administrability value alone would justify. Authoring founding_problem_status as contested (not dead) alongside disappearance_verdict world_rearranges records exactly this half-live state: the boundary problem persists, but the arrangement's center of gravity has moved from administering the line to defending the exclusion. The tangled_rope claim prevents both classification errors: calling the arrangement a snare would erase the real administrability and freelancer-autonomy value that makes the coordination story partly true; calling it a rope would erase the asymmetric cost relocation that makes the extraction story equally true. Both functions run through the same structure — courts apply one test, and the same test sorts a controlled workforce out of protection — which is the tangled-rope signature: coordinated and charged by the same mechanism, held in place by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_employment_boundary,
    'This constraint is one reading of the employment_boundary kernel — the formalist reading, under which contract designation and direct supervisory control fix employment status. Would instantiating a sibling reading change the structural data? Under substantive_employment_reading (economic dependence and algorithmic control are dispositive), platform workers enter the victim set of employment precarity and platforms enter the obligated set; under hybrid_security_reading, a tailored third category creates an intermediate band of obligations and both current poles lose part of their claim.',
    'Comparative-jurisdiction analysis: compile classification outcomes, worker coverage rates, and public-cost incidence under formalist regimes (most US states pre-reform), substantive regimes (UK post-Uber ruling, California ABC test), and hybrid regimes (EU Platform Work Directive), and measure divergence in coverage and cost incidence.',
    'The disagreement is located in the classification criterion itself — contract form versus economic substance versus a third category. Whichever criterion governs redistributes the victim set, the beneficiary obligations, and therefore the per-seat extraction profile across the whole family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_employment_boundary, conceptual, 'Which reading of the employment-boundary kernel governs, and how victim/beneficiary sets shift across readings.').

omega_variable(
    flexibility_preference_genuineness,
    'Is platform workers'' revealed preference for contractor status a genuine ranking of autonomy over protection, or an adaptive preference formed under income necessity and the absence of employee-track alternatives?',
    'Longitudinal worker studies offering explicitly priced trade-offs — matched earnings with employee protections versus contractor status — and observing switching behavior when the exchange is stated rather than ambient.',
    'If preferences are adaptive, the formalist consent premise weakens and the reading''s own normative foundation (voluntary exchange) stops licensing the exclusion; if preferences are genuine and informed, part of the measured cost-shifting is a price workers knowingly accept and the coordination component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_preference_genuineness, empirical, 'Whether worker consent to contractor status is informed preference or adaptation to necessity.').

omega_variable(
    externalization_cost_magnitude,
    'What share of platform labor cost is shifted onto workers'' own households and onto public insurance systems rather than carried in platform prices?',
    'Fiscal-incidence studies comparing public-assistance uptake and uncompensated injury costs among contractor-classified platform workers against employee baselines; actuarial accounting of unemployment-insurance and workers''-compensation contribution shortfalls attributable to contractor classification.',
    'A large externalized share raises the burden on the state seat and supports contribution mandates or reclassification; a small share supports the claim that the arrangement is a low-overhead way of organizing flexible work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_cost_magnitude, empirical, 'Magnitude of cost shifting from platforms to workers and public systems.').

omega_variable(
    formalism_proxy_decay,
    'Does the formalist test still function as a good-faith proxy for dependence, or has algorithmic management widened the form/substance gap until the test operates as a systematic exclusion device?',
    'Audit a worker sample: classify by the formalist test, then measure against a substantive dependence gold standard (earnings concentration, control intensity, substitution possibility); compute error rates and their distributional incidence.',
    'If misclassification concentrates on the most dependent workers, the test functions as targeted exclusion and the arrangement slides toward pure extraction despite its coordination surface; if errors are modest and randomly distributed, the administrability value dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalism_proxy_decay, empirical, 'Whether the bright-line test remains a defensible proxy or has become a sorting mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eb_formalist_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(eb_formalist_tr_t3, employment_boundary__formalist_employment_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(eb_formalist_tr_t6, employment_boundary__formalist_employment_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(eb_formalist_tr_t9, employment_boundary__formalist_employment_reading, theater_ratio, 9, 0.36).
narrative_ontology:measurement(eb_formalist_tr_t12, employment_boundary__formalist_employment_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(eb_formalist_tr_t15, employment_boundary__formalist_employment_reading, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(eb_formalist_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(eb_formalist_be_t3, employment_boundary__formalist_employment_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(eb_formalist_be_t6, employment_boundary__formalist_employment_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(eb_formalist_be_t9, employment_boundary__formalist_employment_reading, base_extractiveness, 9, 0.62).
narrative_ontology:measurement(eb_formalist_be_t12, employment_boundary__formalist_employment_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(eb_formalist_be_t15, employment_boundary__formalist_employment_reading, base_extractiveness, 15, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(eb_formalist_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(eb_formalist_su_t3, employment_boundary__formalist_employment_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(eb_formalist_su_t6, employment_boundary__formalist_employment_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(eb_formalist_su_t9, employment_boundary__formalist_employment_reading, suppression_requirement, 9, 0.55).
narrative_ontology:measurement(eb_formalist_su_t12, employment_boundary__formalist_employment_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(eb_formalist_su_t15, employment_boundary__formalist_employment_reading, suppression_requirement, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'employment status of platform workers' covers three structurally distinct claims, not one constraint viewed from angles. The formalist reading (this file) fixes status by contract form and direct supervision; the substantive reading fixes it by economic dependence and algorithmic control; the hybrid reading builds a third category with portable protections. Each has its own epsilon, its own victim set, and its own classification — the formalist file authors high extraction via cost externalization precisely because its criterion excludes platform workers from every protective mandate, while the substantive file would author the same arrangement with platform workers inside the victim set. The formalist reading is the incumbent: its enforcement shapes the legitimacy conditions and resource environment in which the siblings operate, which is why the family links run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
