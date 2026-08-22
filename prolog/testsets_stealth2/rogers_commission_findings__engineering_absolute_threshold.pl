% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [RESOLVED_TERMINATED_ON_CERTIFICATION]
% ============================================================================

:- module(constraint_rogers_commission_findings__engineering_absolute_threshold, []).

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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Commission Engineering Absolute Threshold — Flight Halt Until O-Ring Redesign Certification
 *   domain: organizational safety/technology governance/regulatory compliance
 *
 * SUMMARY:
 *   After the loss of Challenger in January 1986, the Rogers Commission's
 *   findings were read in at least three incompatible ways. This file
 *   instantiates the engineering_absolute_threshold reading: the findings
 *   establish a technical safety boundary under which flight operations cease
 *   until the solid rocket booster field-joint O-ring redesign is certified
 *   by engineering authority. Engineers hold veto power over Flight Readiness
 *   Reviews; the hold suppresses launch cadence for thirty-two months; the
 *   protected party is the flight crew. KEY AGENTS (by structural
 *   relationship): rogers_commission_investigators — founding agenda-setter
 *   (institutional/analytical), authored the mandate in 1986;
 *   srb_engineering_community — operating gate administrator
 *   (organized/constrained), holds the veto and signs certification;
 *   nasa_launch_management — primary target (institutional/constrained),
 *   bears cadence suppression while administering the process the veto sits
 *   in; shuttle_flight_crews — primary beneficiary
 *   (organized/identity_locked); commercial_payload_customers — secondary
 *   target (moderate/arbitrage), absorb delays then exit to expendable
 *   launchers; shuttle_prime_contractors — target (organized/constrained),
 *   ride flight rate; american_taxpayers — diffuse beneficiary-payer
 *   (powerless/trapped); european_launch_providers — incidental beneficiary
 *   (organized/mobile); congressional_oversight_committees — analytical
 *   observer (institutional/analytical). The claim/metric gap is deliberate:
 *   the constraint is CLAIMED as scaffold (its own text carries its
 *   termination) while the metrics describe a high-cost enforced halt that
 *   decays after certification — the engine computes per-seat types from the
 *   structural data; the claim is not reconciled to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.35).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.4).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.35).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, scaffold).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission Engineering Absolute Threshold — Flight Halt Until O-Ring Redesign Certification").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational safety/technology governance/regulatory compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:has_sunset_clause(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, '25654f25-193e-4e95-87d1-537599259a28').
narrative_ontology:cs_kernel_codification('25654f25-193e-4e95-87d1-537599259a28', formalized).
narrative_ontology:cs_authority_grounding('25654f25-193e-4e95-87d1-537599259a28', expertise).
narrative_ontology:cs_interpretation_layer_present('25654f25-193e-4e95-87d1-537599259a28').
narrative_ontology:cs_reading_relation('25654f25-193e-4e95-87d1-537599259a28', rogers_commission_findings__management_compliance_narrative, coexists_with).
narrative_ontology:cs_reading_relation('25654f25-193e-4e95-87d1-537599259a28', rogers_commission_findings__actuarial_risk_acceptance, influences).
narrative_ontology:cs_axiom('25654f25-193e-4e95-87d1-537599259a28', foundational, known_unresolved_hazard_bars_flight).
narrative_ontology:cs_axiom_status(known_unresolved_hazard_bars_flight, holdable).
narrative_ontology:cs_axiom_grounding('25654f25-193e-4e95-87d1-537599259a28', known_unresolved_hazard_bars_flight, empirically_contingent).
narrative_ontology:cs_axiom('25654f25-193e-4e95-87d1-537599259a28', secondary, engineering_certification_authority_is_final).
narrative_ontology:cs_axiom_status(engineering_certification_authority_is_final, holdable).
narrative_ontology:cs_axiom_grounding('25654f25-193e-4e95-87d1-537599259a28', engineering_certification_authority_is_final, conventional).
narrative_ontology:cs_reference_frame('25654f25-193e-4e95-87d1-537599259a28', engineering_certified_release_baseline).
narrative_ontology:cs_drift_state('25654f25-193e-4e95-87d1-537599259a28', post_return_to_flight_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('25654f25-193e-4e95-87d1-537599259a28', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, shuttle_flight_crews).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, american_taxpayers).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, european_launch_providers).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, nasa_launch_management).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, commercial_payload_customers).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, shuttle_prime_contractors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, american_taxpayers).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, engineering_certification_precedence).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, protected_technical_dissent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The presidential commission that wrote the June 1986 findings and recommendations; its report is the text from which this boundary descends. It prescribed the redesign-before-flight sequence, delivered its report, and disbanded; its members held no continuing enforcement role.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_investigators, agenda_setter,
    institutional, biographical, analytical, national).

% Design and test the solid rocket motor field joints. After the 1986 accident their written dissent acquired formal force: no Flight Readiness Review closes over their unresolved objection, and the fleet stays down until they certify the redesigned joint. They staff the verification test matrix — subscale firings, full-duration static tests, joint temperature cycling — and sign the certification package. Leaving the program means abandoning the problem they own; several spent careers on the joint seal.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, srb_engineering_community, agenda_setter,
    organized, biographical, constrained, national).

% Chair the Flight Readiness Reviews and own the launch schedule. During the grounding they absorbed the political cost of an idle fleet — fixed operations burn continued with zero flights — and after return-to-flight they rebuilt cadence under tighter review gates. They administer the very process in which the engineering veto sits, so they enforce a boundary that taxes their own core deliverable.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, nasa_launch_management, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, nasa_launch_management, agenda_setter).

% Fly the vehicle whatever its state. They gained the binding assurance that no future flight would be scheduled over a known, uncorrected seal vulnerability. They cannot individually opt out of the next mission's risk envelope, and leaving the corps means giving up the role they trained decades for.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, shuttle_flight_crews, beneficiary,
    organized, biographical, identity_locked, national).

% Satellite operators who booked Shuttle deployments and watched slots vanish for thirty-two months. Several rebooked on European and other expendable rockets; those who stayed absorbed insurance, financing, and market-timing costs. Their exposure to the hold ended when they left the manifest.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, commercial_payload_customers, payer,
    moderate, immediate, arbitrage, global).

% Orbiter and booster contractors on support contracts: revenue continued, but milestone fees, follow-on awards, and workforce stability rode on flight rate, which went to zero for the grounding and recovered only partially afterward.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, shuttle_prime_contractors, payer,
    organized, biographical, constrained, national).

% Fund the program through appropriations. They carried the cost of a standing, unflying fleet — roughly a billion dollars a year in fixed operations — and received in exchange a credible guarantee that the crewed program would not fly on a known lethal defect. Their only lever is electoral, exercised at generational intervals.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, american_taxpayers, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, american_taxpayers, payer).

% Operate the expendable launcher that inherited dozens of commercial missions the grounded fleet could not fly. They signed new contracts during the grounding window and converted the halt into durable market share. They had no seat in any American review, and their gain arrived through customer choice, not through the review process.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, european_launch_providers, beneficiary,
    organized, biographical, mobile, continental).

% Hold hearings, control appropriations, and monitored the redesign program through audit reports and return-to-flight certification reviews. They observe and fund but neither operate the gate nor fly through it.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, congressional_oversight_committees, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__engineering_absolute_threshold, diffuse).
narrative_ontology:fixing_cost_class(rogers_commission_findings__engineering_absolute_threshold, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes 'wait for verified hardware' the only permissible move for every actor at once: when each link in the chain faces separate incentives to fly despite a known uncorrected seal vulnerability, a single certification gate binds managers, engineers, and crews to the same hold, removing the case-by-case renegotiation in which the January 1986 launch decision failed.
% TRANSFER_FUNCTION: Moves launch opportunity, schedule certainty, milestone revenue, and commercial deployment slots away from the launch-side actors and converts them into verification testing, redesigned hardware, and a certified flight-worthiness basis; no participant receives the forgone value as income or advantage — it is spent on the redesign program or destroyed as delay.
% ABSENT_VOICES: Future flight crews spoke only through the astronaut office's single seat in the restructured reviews; commercial customers were represented by lost-manifest signals rather than testimony; and the actuarial and compliance constituencies testified in the hearings but had no vote in how the engineering finding was codified. Conversely, the seal engineers who were excluded from the 1986 launch decision were, for the first time, placed inside this one — the boundary is partly a correction of their prior exclusion.
% DISAPPEARANCE_RATIONALE: Without the certification gate, the 1986–88 sequence replays differently: internal records show schedule pressure building toward resumption on the existing field joints, and the pre-accident decision structure had already once overridden a documented no-fly objection. The SRB redesign program, the return-to-flight certification reviews, and the restructured readiness process all presuppose the hold; remove it overnight in early 1987 and flights resume on unredesigned hardware with the known low-temperature failure mode still in place.
% FOUNDING_PROBLEM: Challenger was lost to a seal vulnerability that was known, documented, and overruled: engineers had forecast O-ring blow-by at the forecast launch temperature, the objection reached the launch decision, and schedule considerations carried it. The boundary was built so that flight could not proceed until that specific failure mode was engineered out and independently certified.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the National Research Council's independent oversight panels reviewed the redesign verification and reported the field-joint fix adequate before return-to-flight; GAO audits of the redesign program reached the same conclusion; and the redesigned joint flew the remainder of the program — more than a hundred missions — without recurrence of the pre-redesign blow-by. No party, including the former dissenting engineers, has contended the original field-joint hazard remained live after 1988.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).
:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.35 as the interval-end state of a series peaking at 0.62 during the grounding, when the entire launch-side economy bore the hold's cost at maximum intensity; the scalar follows the shared-grid end-state convention while the series carries the operative-window peak. Suppression is a raw structural property, unscaled by power or scope: it peaked at 0.82 when the halt was absolute and decayed to 0.40 as certification lifted the hold and enforcement relaxed into ordinary review conservatism — the suppression_requirement series is authored because this story specifically tracks enforcement-capacity change (ratchet-up at the halt, decay after certification). Theater is low through the redesign years (the verification work was load-bearing) and rises to 0.40 after certification as review activity partially ritualizes — documenting safety rather than deciding it. Accessibility collapse is 0.70: within this reading's frame the physical fact admits no workaround short of redesign, but the sibling readings persist as framing-level alternatives, so collapse stops short of the near-total values proper to natural limits. Resistance is 0.60: schedule coalitions, political pressure, and actuarial reframing pressed against the hold throughout its life. Fixing cost was cheap relative to its benefit — the redesign program was funded and executed once the will existed — which is why the boundary terminated rather than persisting. All three metric series run on one shared time grid (1986, 1988, 1990, 1992, 1994, 1996).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute a different type than the beneficiary and administrator seats. From nasa_launch_management and shuttle_prime_contractors the arrangement is an imposed, enforced cessation taxing their core deliverable; from shuttle_flight_crews it is the difference between flying over a known defect and not; from srb_engineering_community it is professional duty given institutional teeth. Management's dual position — administering the process in which the veto sits while bearing the cadence cost — generates divergence even within a single seat. The engine computes these per-seat classifications from power, exit, and declared position; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure maps to directionality as follows. shuttle_flight_crews: declared beneficiary, identity-locked exit — full beneficiary end. american_taxpayers: dual-declared beneficiary/payer, trapped — mildly below symmetric. european_launch_providers: declared beneficiary with mobile exit — near the beneficiary end, though the benefit is incidental (see the displaced_demand_attribution omega). srb_engineering_community: administrator with no declared beneficiary or victim position; their alignment is protective and professional rather than rent-collecting, sitting below symmetric. nasa_launch_management: declared victim/payer with constrained exit — near the full-target end. shuttle_prime_contractors: payer, constrained — high target side. commercial_payload_customers: payer but with arbitrage-grade exit (rebooking to expendable launchers), which damps their effective extraction relative to the trapped payers. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct relationships without correction. On the receipt surface, gain_flow is authored 'diffuse' as an affirmative checked finding: the cadence value the hold destroys converts into verification testing and redesigned hardware, and no named seat receives it as gain — european_launch_providers acquire displaced market demand, a competitive windfall from the halt's side effects, not receipt of the extracted value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem died at certification: the redesigned field joint was verified and flown in October 1988, and the boundary's own termination condition fired. Unlike an atrophied arrangement kept alive by inertia, this one ended when its trigger cleared — the sunset was honored, which is why the claimed type is scaffold rather than piton. The R5 mismatch (founding_problem_status dead x disappearance_verdict world_rearranges) will raise the capture/zombie flag; here the flag reads correctly as dependence-during-life, not post-mortem persistence: arrangements depended on the hold while it operated, and it stopped operating when its problem closed. The honest residue is in the theater series — a rising performative share in successor review processes after 1988 — which belongs to the downstream standing-review arrangement, not to this bounded halt. No mandate-outlived-function declaration is warranted: the mandate completed and terminated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the Rogers mandate instantiate a hardware-verified release gate (this reading), a documentation-sufficiency compliance process (management_compliance_narrative), or a quantified-risk acceptance requirement (actuarial_risk_acceptance)?',
    'Comparative institutional tracing of which reading the post-1988 Flight Readiness reforms actually embodied: whether a named unresolved objection could halt a flight absent hardware certification, whether documented awareness alone ever authorized flight, and whether signed probability acceptance ever substituted for resolution.',
    'If a sibling reading governs, this story''s termination condition and victim set change: the compliance reading ends the hold with paperwork and shifts victims toward overruled engineers; the actuarial reading ends it with a signed acceptance and shifts victims toward those exposed to residual risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which of three readings of the Rogers findings governs flight-release authority.').

omega_variable(
    veto_persistence_after_certification,
    'Did engineer veto authority over flight readiness survive the certification event as a standing arrangement, or did it dissolve with the specific O-ring mandate?',
    'Organizational analysis of readiness-review authority after STS-26: whether a dissenting engineer could again halt a flight absent a named hardware hazard, and how later decisions (e.g., foam-strike acceptance before Columbia) treated engineering objections.',
    'If the veto persisted, a second, standing downstream arrangement exists and this story''s transitional classification covers only the bounded halt; if it dissolved, the transitional reading is complete and the post-1988 theater rise marks ritualization without authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_persistence_after_certification, empirical, 'Whether the engineering veto was bounded to the redesign episode or persisted institutionally.').

omega_variable(
    counterfactual_grounding_baseline,
    'Would the fleet have resumed flight on unredesigned hardware absent the certification gate, or would prudence have grounded it anyway — making part of the measured cadence cost attributable to the gate and part to the accident itself?',
    'Decision-trace reconstruction of 1986–87 resumption deliberations under counterfactual removal of the gate, using internal schedule-pressure records and the pre-accident decision precedent.',
    'If the fleet would have been grounded regardless, the gate''s marginal extraction is overstated and its protective credit understated; if flights would have resumed, the measured cadence cost is genuinely the gate''s imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_grounding_baseline, empirical, 'Attribution of grounding cost between the gate and the underlying accident.').

omega_variable(
    displaced_demand_attribution,
    'Does european_launch_providers'' market-share gain count as benefit flowing from the halt, or from independent competitive dynamics (pricing, reliability record) that would have shifted contracts anyway?',
    'Contract-award timing analysis against grounding windows, controlling for launcher pricing and failure history over the same period.',
    'If halt-attributed, that seat''s derived directionality sits nearer the beneficiary end than its incidental role warrants; if independent, its beneficiary declaration overstates its structural stake.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_demand_attribution, empirical, 'Whether competitor gains are attributable to the halt or to independent dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 1986, 1996).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t1986, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 1986, 0.18).
narrative_ontology:measurement_basis(roge_tr_t1986, observed).
narrative_ontology:measurement(roge_tr_t1988, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 1988, 0.16).
narrative_ontology:measurement_basis(roge_tr_t1988, observed).
narrative_ontology:measurement(roge_tr_t1990, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 1990, 0.22).
narrative_ontology:measurement_basis(roge_tr_t1990, observed).
narrative_ontology:measurement(roge_tr_t1992, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 1992, 0.28).
narrative_ontology:measurement_basis(roge_tr_t1992, observed).
narrative_ontology:measurement(roge_tr_t1994, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 1994, 0.34).
narrative_ontology:measurement_basis(roge_tr_t1994, observed).
narrative_ontology:measurement(roge_tr_t1996, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 1996, 0.4).
narrative_ontology:measurement_basis(roge_tr_t1996, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t1986, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 1986, 0.62).
narrative_ontology:measurement_basis(roge_be_t1986, observed).
narrative_ontology:measurement(roge_be_t1988, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 1988, 0.5).
narrative_ontology:measurement_basis(roge_be_t1988, observed).
narrative_ontology:measurement(roge_be_t1990, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement_basis(roge_be_t1990, observed).
narrative_ontology:measurement(roge_be_t1992, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 1992, 0.38).
narrative_ontology:measurement_basis(roge_be_t1992, observed).
narrative_ontology:measurement(roge_be_t1994, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 1994, 0.36).
narrative_ontology:measurement_basis(roge_be_t1994, observed).
narrative_ontology:measurement(roge_be_t1996, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 1996, 0.35).
narrative_ontology:measurement_basis(roge_be_t1996, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t1986, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 1986, 0.82).
narrative_ontology:measurement_basis(roge_su_t1986, observed).
narrative_ontology:measurement(roge_su_t1988, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 1988, 0.66).
narrative_ontology:measurement_basis(roge_su_t1988, observed).
narrative_ontology:measurement(roge_su_t1990, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 1990, 0.56).
narrative_ontology:measurement_basis(roge_su_t1990, observed).
narrative_ontology:measurement(roge_su_t1992, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 1992, 0.49).
narrative_ontology:measurement_basis(roge_su_t1992, observed).
narrative_ontology:measurement(roge_su_t1994, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 1994, 0.44).
narrative_ontology:measurement_basis(roge_su_t1994, observed).
narrative_ontology:measurement(roge_su_t1996, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 1996, 0.4).
narrative_ontology:measurement_basis(roge_su_t1996, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% Kernel decomposition note: 'the Rogers findings' is one colloquial label over three structurally distinct constraints. This file instantiates the engineering_absolute_threshold reading (hardware-verified release gate; termination by certification; victims = launch cadence). The sibling files instantiate management_compliance_narrative (documentation suffices to proceed; victims = dissenting engineers' authority) and actuarial_risk_acceptance (quantified acceptance legitimizes flight; victims = those exposed to residual probability). The label conflates them because all three descend from the same June 1986 report; their epsilon values, termination conditions, and victim sets differ, so they are authored as separate stories linked through reading_relations and this affects_constraints edge set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
