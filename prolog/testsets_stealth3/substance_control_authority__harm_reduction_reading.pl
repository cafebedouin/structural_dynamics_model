% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: Harm Reduction Regime: Decriminalization-with-Services Reading of Substance Control Authority
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the substance_control_authority
 *   kernel: the state accepts that drug use will occur and organizes public
 *   health services as the primary response, decriminalizing personal use and
 *   possession while leaving commercial supply illegal. Under this reading,
 *   users exit the criminal victim set but remain a partial victim set
 *   through health harms (unregulated product, adulteration-driven overdose
 *   and infection); third parties near service concentrations bear
 *   disease-transmission and disorder burdens; taxpayers fund the apparatus;
 *   and the use-legal/supply-illegal split preserves a gray-market margin for
 *   illicit distributors. The arrangement requires active maintenance
 *   throughout: police stand-down directives, site licensing renewals, budget
 *   appropriations defended annually against repeal campaigns. Per the
 *   claim/metric independence rule, the claimed type below is what I believe
 *   structurally true of this reading, authored separately from the metric
 *   values, which describe the arrangement's actual operation. KEY AGENTS (by
 *   structural relationship): - people_who_use_drugs: primary target
 *   (powerless/trapped) - bears the health-harm burden, receives services -
 *   illicit_suppliers: primary beneficiary (organized/arbitrage) - collects
 *   the preserved gray-market margin - public_health_service_agencies: agenda
 *   setter and institutional beneficiary (institutional/constrained) - runs
 *   the machinery, grows with it - national_legislature: agenda setter
 *   (institutional/arbitrage) - writes and rewrites the enabling statutes -
 *   host_neighborhood_residents: secondary target (moderate/mobile) - hosts
 *   the concentrated externality without consent - taxpayers: diffuse cost
 *   bearer with diffuse benefit (organized/constrained) -
 *   recovery_community_advocates: excluded voice (moderate/mobile) - objects
 *   to the acceptance posture from outside the room -
 *   independent_epidemiological_evaluators: analytical observer
 *   (institutional/analytical) - sees the full outcome record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.52).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.42).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "Harm Reduction Regime: Decriminalization-with-Services Reading of Substance Control Authority").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '5a60b686-2ecd-406e-af3f-41db54d999ec').
narrative_ontology:cs_kernel_codification('5a60b686-2ecd-406e-af3f-41db54d999ec', formalized).
narrative_ontology:cs_authority_grounding('5a60b686-2ecd-406e-af3f-41db54d999ec', expertise).
narrative_ontology:cs_interpretation_layer_present('5a60b686-2ecd-406e-af3f-41db54d999ec').
narrative_ontology:cs_reading_relation('5a60b686-2ecd-406e-af3f-41db54d999ec', substance_control_authority__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('5a60b686-2ecd-406e-af3f-41db54d999ec', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('5a60b686-2ecd-406e-af3f-41db54d999ec', foundational, use_is_a_health_condition_not_a_crime).
narrative_ontology:cs_axiom_status(use_is_a_health_condition_not_a_crime, holdable).
narrative_ontology:cs_axiom_grounding('5a60b686-2ecd-406e-af3f-41db54d999ec', use_is_a_health_condition_not_a_crime, empirically_contingent).
narrative_ontology:cs_axiom('5a60b686-2ecd-406e-af3f-41db54d999ec', secondary, punishment_worsens_health_outcomes).
narrative_ontology:cs_axiom_status(punishment_worsens_health_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('5a60b686-2ecd-406e-af3f-41db54d999ec', punishment_worsens_health_outcomes, instrumental).
narrative_ontology:cs_reference_frame('5a60b686-2ecd-406e-af3f-41db54d999ec', state_public_health_stewardship).
narrative_ontology:cs_drift_state('5a60b686-2ecd-406e-af3f-41db54d999ec', contemporary_fentanyl_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a60b686-2ecd-406e-af3f-41db54d999ec', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, illicit_suppliers).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_service_agencies).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, host_neighborhood_residents).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Obtain psychoactive substances through unregulated channels and use them. Since decriminalization they carry no possession charge, can enter supervised consumption rooms, exchange sterile supplies, and walk into treatment without fear of arrest at the door. They still consume product of unknown composition: potency swings and adulteration drive the overdose and infection burden they bear in their own bodies. Regulated purchase remains unavailable to them. Dependence binds the heaviest users to the scene; lighter users circulate more freely.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_who_use_drugs, beneficiary).

% Distribute product through the covert chains that predate the regime. Because possession no longer draws user-facing penalty while sale remains criminal, retail prices stay far above legal-market equivalents and enforcement attention is redirected elsewhere; the arrangement preserves their margin while shrinking their customer-facing risk. They shift routes, compounds, and territory quickly, answer to no licensing body, and take no responsibility for adulterated batches.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, illicit_suppliers, beneficiary,
    organized, biographical, arbitrage, global).

% Live around open scenes and service sites: discarded paraphernalia in doorways, congregating crowds, occasional violence, constant ambulance traffic. They did not choose to host the concentrated externality; siting decisions were made above their heads and announced to them. They petition councils, litigate conditions of operation, or move away, with moving expensive but genuinely available.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, host_neighborhood_residents, payer,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, host_neighborhood_residents, excluded).

% Fund the service apparatus through general revenue: staffing, sterile supplies, site leases, treatment subsidies, naloxone stockpiles. In exchange, emergency-room intake slows somewhat, jail populations shrink, and disease spread among users does not reach the general population at prior rates. They cannot decline the levy individually; their recourse is periodic voting and taxpayer-association advocacy.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, taxpayers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, taxpayers, beneficiary).

% Design and operate the machinery: license and staff consumption sites, set supply-exchange protocols, negotiate police stand-down memoranda, publish monitoring dashboards, brief ministers. Every program expansion enlarges their budget, headcount, and international standing; every crisis lands on their desk and their press conference. They depend politically on the arrangement's continued existence.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_service_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, public_health_service_agencies, beneficiary).

% Writes and rewrites the enabling statutes: personal-use thresholds, the list of decriminalized conduct, site-authorization powers, and, under electoral pressure, re-criminalization amendments. Holds hearings, absorbs lobbying from every other seat, claims credit in quarters when overdose statistics improve, and blames agencies in quarters when they worsen.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, national_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Organizations and individuals whose lives were rebuilt through abstinence-oriented recovery. They argue the official posture teaches society that use is acceptable, that treatment-on-demand is starved while consumption rooms are funded, and that their pathway is treated as an embarrassment in official communications. They were not seated on the advisory boards that shaped the current design.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, recovery_community_advocates, excluded,
    moderate, biographical, mobile, national).

% University research teams and international monitoring bodies running cohort studies and publishing the transmission, mortality, and crime curves. They are neither funders nor operators of the regime; their publications are quoted selectively by every other seat whenever the numbers flatter that seat's position.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, independent_epidemiological_evaluators, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, illicit_suppliers).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public health response around drug use that continues regardless of legal status: sterile supply exchange, supervised consumption, infectious-disease screening, naloxone distribution, and treatment linkage are delivered once through managed contact points instead of scattering untreated risk across streets, courts, and emergency rooms. Police effort is redirected from possession to supply-side and disorder enforcement by standing directive.
% TRANSFER_FUNCTION: Moves money from general tax revenue into service delivery; moves health risk onto the bodies of users themselves (unregulated product) while removing criminal-sanction risk from them; concentrates nuisance and disease exposure on host neighborhoods; preserves retail margin for illicit suppliers via continued supply illegality; and moves users out of court dockets into clinical contact.
% ABSENT_VOICES: Host-neighborhood residents were absent from siting decisions and learn of consumption rooms after leases are signed. Recovery-community advocates were absent from the advisory boards that defined program mix, and report being unheard since. Users themselves were absent from early program design, entering only as service recipients rather than co-designers. All three currently organize outside the formal process: resident associations at council chambers, recovery organizations in parallel coalitions, user unions in nascent form.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, users return to fully unsupervised use: needle-sharing resumes at prior rates, overdose fatalities climb with no naloxone saturation, and possession arrests resume filling courts and jails. Harms currently concentrated at managed sites disperse back into parks and stairwells. The illicit supply chain persists essentially untouched since it never depended on the arrangement. Agency budgets, site leases, and stand-down memoranda unwind; the epidemiological curve bends back toward the prohibition-era baseline within years.
% FOUNDING_PROBLEM: Built to solve the collateral damage of enforcement-first control: the HIV outbreak among injection drug users, escalating overdose mortality, mass incarceration of possessors, and court systems clogged with use offenses, at a moment when criminalization had demonstrably failed to reduce use while amplifying its harms.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting seats: WHO and UNAIDS surveillance series document the transmission catastrophe under enforcement-first regimes and the measured reductions where services deployed; independent academic cohort evaluations (the Vancouver and Lisbon evaluation literatures) attest both the founding damage and partial amelioration; municipal coroner records corroborate the continuing mortality problem that keeps the founding problem alive. The supplier seat attests nothing (silence serves its margin). Prohibition-aligned voices dispute that the founding problem justifies acceptance rather than intensified enforcement, so corroboration is broad on the facts and contested on the inference, along the kernel's fault line.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) and rose monotonically across the interval: the arrangement's demand-side relief without supply-side relief lets distributor margins compound as use de-stigmatizes, while the fentanyl era raised the health-harm burden users personally carry. Suppression is moderate-low (0.42) because the largest coercive instrument (possession prosecution) was dismantled, but alternatives remain partly closed: regulated supply is unavailable, service capacity is rationed, and the legalization exit is politically foreclosed in most jurisdictions adopting this reading. Theater is low-moderate (0.30): needle exchange and supervised consumption demonstrably function, but a growing share of activity is demonstration-project politics, ribbon-cutting, and evidence-branding over stagnant budgets. Accessibility collapse (0.45) and resistance (0.60) reflect an arrangement that neither closes alternatives like a natural limit nor floats free like a pure standard: treatment pathways exist inside it, while neighborhood associations, recovery advocates, and repeal movements actively contest it. The temporal series share one grid (points 0, 4, 8, 12, 16, 20, 24) so no metric is silently substituted at another metric's time points. The suppression_requirement series is deliberately U-shaped and documents a real cycle: enforcement capacity stood down through the first decade, stabilized at a low plateau, then re-ratcheted under fentanyl-era political panic (site restrictions, public-use recriminalization, partial statutory reversals). The cycle's driver is crisis-attention budgeting: each mortality spike purchases a burst of coercive correction and a burst of service funding, then both decay until the next spike. The oscillation functions as intermittent reinforcement for the service sector, keeping agencies perpetually mobilizing rather than consolidating, and it is documented here rather than smoothed away.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data is arranged to let them. From the agency seat, the arrangement is a mandate it built and defends, with every expansion confirming its competence; from the user seat, the same arrangement is a lifeline attached to a poisoned supply, protection from handcuffs paired with exposure to adulterants; from the resident seat, it is disorder management that chose their doorstep as the management site; from the supplier seat, it is the best possible enforcement environment, better than prohibition ever delivered. Same statute, four different constraints. The evaluator seat sees the whole outcome record that each partisan seat samples selectively.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary and victim declarations drive the derivation. Illicit suppliers sit nearest the beneficiary pole (d near 0.1): the arrangement subsidizes them by suppressing their customer-facing risk while protecting their margin. Users sit near the target pole (d roughly 0.7): they bear the health-harm burden in their own bodies, their exit is bound by dependence, and the services they receive offset only part of the exposure. Host residents are mid-to-target (d roughly 0.55-0.6): real uncompensated burden, but mobile exit moderates their trap. Taxpayers sit near symmetric (d roughly 0.5): compulsory funding against diffuse offsets. The directionality_overrides entry corrects the institutional seats: deriving from the agency's beneficiary listing alone would place it near d 0.15-0.2, but the agency's institutional interest in problem persistence (budget and mandate scale with the problem's salience) and the legislature's credit-claiming position raise their true directionalities to roughly 0.3 - they are coordinated-and-collecting seats, not passive beneficiaries, and the override encodes that mild capture.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification guards against two opposite mislabels. Read charitably, the arrangement looks like a pure rope: humanitarian services solving a collective-action problem at modest overhead, and indeed its coordination function is real and evidenced. Read cynically, it looks like a snare: a taxpayer-financed subsidy that warehouses a stigmatized population while a criminal industry harvests the margin, with residents conscripted as unpaid hosts. The truth contains both strands, which is exactly the tangled_rope signature: a genuine coordination function (measurably lower transmission and mortality wherever services actually deploy) braided with asymmetric extraction (supplier margin preservation, user health-risk retention, resident externality imposition). Mandatrophy is not yet resolved: the founding problem is live, arguably larger than at founding, so the arrangement has not outlived its function. But the theater_ratio trend (0.14 to 0.30) marks the leading edge of decay: if demonstration projects continue to substitute for scale-up, the performance share grows and the arrangement drifts toward maintaining the appearance of response rather than the response itself. Watching that ratio is watching the piton boundary approach.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates only the harm_reduction_reading of the substance_control_authority kernel. Would the prohibition_reading or legalization_reading govern instead, and what structurally changes if they do?',
    'Jurisdictional natural experiments and cross-jurisdiction longitudinal outcome comparison (criminalized-user cohorts versus decriminalized-user cohorts versus regulated-market cohorts) reveal which reading''s victim set and mechanism actually dominate welfare.',
    'Switching readings swaps the entire victim set: prohibition criminalizes users outright; legalization converts users into consumers and shifts victims to market-externalities; this reading leaves users health-exposed but legally free. Classification, beneficiaries, and epsilon are all reading-relative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which sibling reading of the substance-control kernel governs is undetermined within this story; this file fixes one reading only.').

omega_variable(
    gray_market_rent_persistence,
    'How large is the supplier margin structurally preserved by the decriminalize-use/keep-sale-illegal split, and is that margin a stable property of this reading or an artifact of transitional enforcement?',
    'Street-price and purity surveillance data compared against estimated legal-production cost benchmarks in jurisdictions that later legalized supply, isolating the premium attributable to continued supply illegality.',
    'Large persistent premiums strengthen the illicit-supplier capture seat and push the arrangement deeper into tangled_rope/snare territory; negligible premiums move it toward rope with a residual public-good funding question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_market_rent_persistence, empirical, 'Size and stability of the gray-market rent the arrangement''s demand/supply asymmetry preserves.').

omega_variable(
    third_party_risk_attribution,
    'Are the disease-transmission and disorder burdens borne by host-neighborhood residents intrinsic to accepting use without regulating supply, or artifacts of under-resourced services and poor siting?',
    'Compare resident-outcome data across well-funded dispersed-service models versus starved concentrated-site models; if burdens track resourcing rather than the acceptance posture itself, they are contingent.',
    'If intrinsic, residents are structural victims and the extraction asymmetry is permanent; if contingent, the arrangement is correctable within its own logic and the victim set thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_risk_attribution, empirical, 'Whether third-party burdens are structural to the reading or implementation-contingent.').

omega_variable(
    rollback_trajectory_direction,
    'Does the post-fentanyl recriminalization pressure (partial reversals of decriminalization statutes) revert this reading toward the prohibition_reading, stabilize as a durable punitive-hybrid, or catalyze passage through to the legalization_reading?',
    'Track successive legislative sessions, ballot measures, and site-licensing decisions over the next decade; the direction of statutory amendment is the observable.',
    'Reversion dissolves this constraint into its prohibition sibling; stabilization entrenches the current tangled profile; passage-through transfers the victim set and retires this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rollback_trajectory_direction, empirical, 'Trajectory of the reading under fentanyl-era political pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__harm_reduction_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__harm_reduction_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__harm_reduction_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__harm_reduction_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__harm_reduction_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__harm_reduction_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__harm_reduction_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__harm_reduction_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__harm_reduction_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__harm_reduction_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__harm_reduction_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__harm_reduction_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__harm_reduction_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__harm_reduction_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__harm_reduction_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'drug policy'. The single natural-language concept covers three structurally distinct claims with distinct epsilon values and victim sets: the prohibition_reading (criminalization to protect third parties; users as criminal victims; high suppression), this harm_reduction_reading (acceptance plus services; users as partial health victims; third parties as externality bearers; moderate extraction), and the legalization_reading (regulation as commerce; users converted to consumers; victims shift to market externalities and tax incidence). The prohibition_reading is historically upstream (the international treaty architecture predates and constrains both siblings); this reading sits midstream, citing prohibition's failure as its own warrant and supplying the evidentiary and constituency groundwork legalization draws on. Each story carries its own epsilon; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
