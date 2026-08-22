% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   Since the early 1990s a working doctrine has taken hold across the
 *   lending, aid, and security apparatuses: statehood is treated as a matter
 *   of degree. Composite indices score governments on bureaucratic
 *   effectiveness, rule of law, and control of territory; financing,
 *   diplomatic standing, and at the extreme the legitimacy of outside
 *   military or administrative action are calibrated to the scores. The
 *   arrangement answers a real problem, since states that cannot protect
 *   their residents create spillovers no neighbor can ignore, while
 *   installing a standing class of evaluators whose judgments others must
 *   absorb and whose own arrangements are exempt from equivalent scoring.
 *   Low-scoring governments experience the arrangement as supervised
 *   sovereignty: formal equality in the General Assembly, negotiated
 *   subordination wherever finance and security are decided. KEY AGENTS (by
 *   structural relationship): - multilateral_lending_institutions: Primary
 *   agenda-setter and receipt-of-gain seat (institutional/arbitrage) — writes
 *   the capacity criteria, collects program revenue -
 *   permanent_security_council_members: Agenda-setter with secondary
 *   beneficiary position (institutional/arbitrage) — authorizes actions
 *   calibrated to graded assessments, exempt from scoring -
 *   donor_government_agencies: Beneficiary (powerful/mobile) — converts
 *   evaluations into leverage without running daily enforcement -
 *   governance_indicator_producers: Beneficiary (organized/arbitrage) —
 *   produces the ranking infrastructure the hierarchy runs on -
 *   heavily_indebted_aid_dependent_states: Primary target (moderate/trapped)
 *   — absorbs conditionality and autonomy loss -
 *   transitional_administration_populations: Secondary target
 *   (powerless/trapped) — lives under externally administered authority -
 *   target_state_civil_societies: Excluded voice (powerless/trapped) —
 *   governed by metrics it never helped define -
 *   international_legal_theorists: Analytical observer
 *   (analytical/analytical) — sees the full three-reading structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.72).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.63).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, 'c7369d43-0399-4c27-bcbc-49fed0e4d918').
narrative_ontology:cs_kernel_codification('c7369d43-0399-4c27-bcbc-49fed0e4d918', distributed).
narrative_ontology:cs_authority_grounding('c7369d43-0399-4c27-bcbc-49fed0e4d918', distributed).
narrative_ontology:cs_reading_relation('c7369d43-0399-4c27-bcbc-49fed0e4d918', westphalia_sovereignty__westphalia_absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('c7369d43-0399-4c27-bcbc-49fed0e4d918', westphalia_sovereignty__westphalia_conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('c7369d43-0399-4c27-bcbc-49fed0e4d918', foundational, sovereignty_is_scalar_not_categorical).
narrative_ontology:cs_axiom_status(sovereignty_is_scalar_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('c7369d43-0399-4c27-bcbc-49fed0e4d918', sovereignty_is_scalar_not_categorical, empirically_contingent).
narrative_ontology:cs_axiom('c7369d43-0399-4c27-bcbc-49fed0e4d918', foundational, intervention_legitimacy_tracks_capacity_deficit).
narrative_ontology:cs_axiom_status(intervention_legitimacy_tracks_capacity_deficit, holdable).
narrative_ontology:cs_axiom_grounding('c7369d43-0399-4c27-bcbc-49fed0e4d918', intervention_legitimacy_tracks_capacity_deficit, instrumental).
narrative_ontology:cs_reference_frame('c7369d43-0399-4c27-bcbc-49fed0e4d918', capacity_graduated_state_system).
narrative_ontology:cs_drift_state('c7369d43-0399-4c27-bcbc-49fed0e4d918', contemporary_multipolar_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c7369d43-0399-4c27-bcbc-49fed0e4d918', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, multilateral_lending_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, donor_government_agencies).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, governance_indicator_producers).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, heavily_indebted_aid_dependent_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, transitional_administration_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, comparable_governance_measurement).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, capacity_deficit_intervention_warrant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five governments holding veto power over Security Council mandates. They decide which outside actions proceed, frame the resolutions that cite capacity shortfalls as grounds for action, and shield allies and themselves from equivalent scrutiny. Their own domestic arrangements are never submitted to the assessment frameworks they authorize for others, and they can redirect the machinery toward preferred targets.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members, beneficiary).

% Set the criteria by which member economies are scored, attach policy conditions to concessional finance, and collect interest, fees, and repayment streams on the resulting programs. Their staff produce the country assessments that other actors cite. A government that declines their programs loses its principal emergency financing channel; acceptance places fiscal and regulatory decisions under negotiated supervision. Headquarters budgets and staffing depend on a continuing pipeline of programs.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, multilateral_lending_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% National development ministries and aid agencies that allocate budget support and technical assistance according to partner-country scorecards. They gain influence over recipient policy without administering it day to day, and claim domestic political credit for measurable reforms abroad. They can shift portfolios between countries at will; the countries they assess cannot reposition themselves relative to the scorecards.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, donor_government_agencies, beneficiary,
    powerful, biographical, mobile, global).

% Think tanks, university centers, and commercial rating firms that compile composite governance and fragility indices. Their rankings circulate in donor legislation, Security Council debate, and market analysis. Funding and professional standing flow to organizations whose indicators get cited; methodology choices rest with the producers, and the ranked governments rarely see the underlying coding before publication.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, governance_indicator_producers, beneficiary,
    organized, biographical, arbitrage, global).

% Governments whose budgets depend on concessional external finance and whose debt service crowds out discretionary spending. They accept negotiated policy benchmarks to keep financing flowing; declining the benchmarks means arrears, cutoffs, and lost diplomatic standing. Formal equality in the General Assembly does not translate into leverage inside the institutions that score them. Some have begun sourcing unconditional loans elsewhere, at higher cost and with new dependencies attached.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, heavily_indebted_aid_dependent_states, payer,
    moderate, biographical, trapped, national).

% People living under internationally run interim governments, as in Kosovo after 1999, Timor-Leste before independence, and Bosnia under the Office of the High Representative. Laws, budgets, and senior appointments are decided by external officials or by locally vetted officeholders; residents vote within tightly limited frames and cannot remove the administering authority. Leaving means displacement.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, transitional_administration_populations, payer,
    powerless, biographical, trapped, local).

% Domestic organizations, journalists, and scholars in assessed countries who live with the consequences of the rankings and attached conditions but were not consulted when the indicators were designed. They contest methodologies publicly and occasionally force revisions, but sit outside the boards and working groups where the criteria are set.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, target_state_civil_societies, excluded,
    powerless, biographical, trapped, national).

% Academics and legal advisers analyzing how the sovereign-equality norm is being reworked in practice. They watch the full structure, including the Charter text, the competing doctrines, and the operational record, and publish critiques and reconstructions, but hold no vote in any of the forums they study.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_legal_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, multilateral_lending_institutions).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, if contested, method for answering a real collective problem: when a government cannot secure territory, deliver services, or protect residents, external actors need some common basis for deciding who responds, with what mandate, and when outside administration is preferable to collapse. Capacity scoring supplies that common basis for allocating peacekeeping mandates, humanitarian financing, and reconstruction responsibility.
% TRANSFER_FUNCTION: Moves decision authority over domestic fiscal, security, and institutional policy from scored governments to the institutions and agencies that score them; moves concessional finance and diplomatic standing toward governments that rate well; moves reputational and professional rewards to the producers of the rankings.
% ABSENT_VOICES: Residents of administered territories and civil society in assessed states would object that the criteria were written without them; post-colonial legal scholars outside the policy circuit argue the framework reinstates guardianship under new vocabulary. They are absent from the boards, working groups, and veto-wielding chambers where the calibration is set.
% DISAPPEARANCE_RATIONALE: If capacity-calibrated legitimacy vanished overnight, ongoing international administrations would lose their legal warrant, conditionality-based lending would collapse into either open-ended charity or outright refusal, peacekeeping mandate logic would revert to categorical consent requirements, and the indicator industry would lose its citation base, while governments currently scored low would reclaim unqualified inviolability claims. The crisis-response architecture would have to be rebuilt on some other warrant.
% FOUNDING_PROBLEM: After Rwanda and the Kosovo war, the system faced a double failure: categorical inviolability had shielded mass atrocities and state collapse from outside help, while unauthorized unilateral intervention lacked legitimacy. The graded reading was built to answer how necessary external action could be authorized where domestic authority fails, without formally abandoning sovereign equality.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian operators outside the beneficiary set, including Medecins Sans Frontieres field reporting and UN OCHA appeal documents, attest that state-failure emergencies persist and response capacity remains needed. From the opposing side, Non-Aligned Movement summit declarations and Group of 77 statements, issued by governments that bear the scoring, attest that the framework has extended well past its humanitarian warrant into routine policy supervision. Scholarly accounts of the 2005 World Summit negotiation record member states deliberately narrowing the doctrine, corroborating that the founding consensus was contested at birth.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because decision authority over domestic policy transfers to external evaluators and the transfer is decoupled from any demonstrated service the scored government receives in exchange; it stops short of pure extraction because the crisis-response coordination function is real and some governments seek good grades instrumentally, the way borrowers seek credit ratings. Suppression (0.63) is a raw structural figure, unscaled by power or scope: exit from the evaluation regime means losing concessional finance and diplomatic standing, though the recent availability of unconditional alternative credit partially reopens the exit door and caps the figure. Theater ratio (0.44) reflects an indicator industry whose composite methodologies are noisy and whose benchmarking exercises frequently substitute for action, while real allocation decisions still ride on parts of the apparatus. Accessibility collapse is low (0.35) because the sibling readings remain live and widely invoked; the graded frame has not closed off the categorical or threshold alternatives. Resistance (0.58) is sustained and organized: Non-Aligned critique, the deliberate narrowing at the 2005 World Summit, and refusal episodes. The three measurement series run on one shared time grid (t=0,5,10,15,20,25,30) so every tracked metric is authored at every examined point; the series show enforcement machinery hardening through the first two-thirds of the interval and plateauing as multipolar alternatives emerge. Fixing cost is authored prohibitive: dismantling the calibration apparatus would not dissolve the state-failure problem underneath it, no replacement warrant for crisis response is ready, and the shareholder governments who could fix it bear systemic risk from removal that exceeds the benefit.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute differently. From the lending institutions and Council members, the arrangement looks like the machinery they built to answer Rwanda-shaped emergencies, and the coordination half of the structure is what they see. From the indebted governments and administered populations, the same structure operates as supervised subordination: rules written elsewhere, applied to them, exempt from them. Excluded civil societies see paternalism without participation. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The lending institutions sit nearest the beneficiary end: they write the criteria, collect the program revenue, and are never scored themselves. Donor agencies and indicator producers also sit low: they convert the scores into influence and citations without bearing evaluation. Permanent Council members are dual-positioned, authorizing the machinery while standing outside it, which keeps them near the beneficiary end despite their agenda-setting primacy. Heavily indebted governments sit near the full-target end, amplified by trapped exit; administered-territory populations sit at the extreme target end, since every element of the arrangement operates on them and none operates through them. Civil societies in scored states are excluded rather than coordinated, and their exclusion is maintained by the same board structures that set the criteria.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite errors. Reading the arrangement as pure extraction erases the genuine coordination function that even target states sometimes invoke, namely the answer to the Rwanda-paralysis problem that categorical inviolability produced. Reading it as pure coordination erases the asymmetry: the evaluators do not bear the costs of misgrading, the scored cannot review the criteria, and the hierarchy feeds itself. Holding both halves is what the tangled-rope structure records. On obsolescence: the founding problem is contested rather than dead, since state-failure emergencies continue, but the apparatus has extended from emergency response into routine policy supervision that the founding warrant never covered. The status-by-verdict pair (contested x world_rearranges) correctly avoids the dead-mandate zombie flag while leaving the overextension visible for downstream analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the graded_sovereignty reading of the westphalia_sovereignty kernel. Do the sibling readings, absolute_non_intervention (categorical inviolability regardless of conduct) and conditional_responsibility (forfeiture on specified protection failures), better describe operative state practice, and where exactly do the readings diverge structurally?',
    'Code the warrant language of Security Council resolutions, loan program documents, and member-state objections across 1990-2025; classify which reading''s logic each operative text deploys.',
    'If the categorical reading dominates practice, this reading''s hierarchy is discursive overlay and its measured extraction falls; if the threshold reading dominates, extraction concentrates at atrocity triggers instead of running continuously; if the graded reading pervades operative documents, the standing evaluator class and continuous tiering stand as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the sovereignty kernel governs operative practice versus discursive claim-making.').

omega_variable(
    capacity_metric_neutrality,
    'Do the capacity metrics calibrating intervention legitimacy measure state capacity neutrally, or do they encode the institutional preferences of the evaluating powers?',
    'Out-of-sample validation: test whether composite governance indices predict welfare, violence, and growth outcomes better than simple fiscal and administrative measures, across regions, controlling for evaluator proximity.',
    'If the metrics encode institutional form rather than function, the calibration apparatus is preference dressed as measurement and effective extraction rises toward the pure-extraction range; if they carry genuine predictive signal, part of the measured burden is the price of the coordination service itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metric_neutrality, empirical, 'Neutrality of the capacity-measurement apparatus that drives the hierarchy.').

omega_variable(
    tiering_self_perpetuation,
    'Does the grading regime cause the capacity deficits it penalizes, such that conditionality-driven austerity and reform fatigue depress measured capacity and feed lower grades?',
    'Panel analysis of states entering and exiting lender and donor programs with baseline-trend controls, comparing capacity trajectories against matched non-program states.',
    'If self-perpetuating, the arrangement manufactures its own justification and the coordination defense collapses, pushing the structure toward pure extraction; if grades track exogenous shocks, the coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tiering_self_perpetuation, empirical, 'Whether the hierarchy reproduces the deficits it measures.').

omega_variable(
    calibration_discretion_bounds,
    'Where on the capacity spectrum does inviolability actually yield, and who decides: is the calibration bound by articulated criteria or by evaluator discretion?',
    'Codification audit comparing the ICISS criteria, the 2005 World Summit paragraphs, and subsequent Security Council practice; measure variance in treatment of similarly rated states.',
    'Unbounded discretion concentrates the burden on scored states above what formal rules suggest and may justify raising the derived directionality of evaluator seats; bounded criteria would cap discretionary burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(calibration_discretion_bounds, conceptual, 'Whether the capacity-to-legitimacy calibration is bounded by criteria or by evaluator discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.18).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__graded_sovereignty, theater_ratio, 5, 0.25).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__graded_sovereignty, theater_ratio, 10, 0.32).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__graded_sovereignty, theater_ratio, 15, 0.38).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__graded_sovereignty, theater_ratio, 20, 0.41).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__graded_sovereignty, theater_ratio, 25, 0.43).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__graded_sovereignty, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 25, 0.63).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 30, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_conditional_responsibility).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Westphalian sovereignty' decomposes into three structurally distinct claims with different epsilon values. absolute_non_intervention (parity norm, no evaluator class, negligible extraction) is upstream: its Charter text is cited by every party including this reading's critics. conditional_responsibility (threshold forfeiture at atrocity triggers, moderate extraction) sits between. This graded reading (continuous hierarchy, standing evaluator class, highest extraction) is downstream: it consumes the capacity-measurement apparatus and extends legitimacy calibration from exceptional triggers to ordinary policy supervision. The epsilon differences are stable across observables within each file; the label conflated them because public debate slides between the three without marking the move.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
