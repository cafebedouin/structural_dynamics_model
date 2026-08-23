% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__actuarial_risk_acceptance, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Documented Failure-Probability Acceptance Requirement (Rogers Findings, Actuarial Reading)
 *   domain: organizational safety / technology governance / regulatory compliance
 *
 * SUMMARY:
 *   Following the Challenger accident, the Rogers Commission findings became
 *   the governing kernel for how human spaceflight adjudicates risk. This
 *   file instantiates ONE reading of that kernel: the actuarial reading,
 *   under which the findings establish a risk-quantification requirement —
 *   flight is acceptable when the failure probability is documented and
 *   consciously accepted by informed decision-makers. Under this reading the
 *   standing arrangement is a documented-acceptance regime: probability
 *   bounds, acceptance signatures, and an authority chain defining who may
 *   accept residual risk on behalf of those who bear it. The epsilon referent
 *   is that standing arrangement, assessed by this reading's own lights —
 *   never the categorical-hold arrangement the engineering_absolute_threshold
 *   sibling would substitute. The claim (tangled_rope) and the metrics are
 *   authored independently: the quantification function is genuinely
 *   necessary coordination no launch authority can operate without, while the
 *   same ledger measurably shifts consequence-bearing onto crews and
 *   displaces categorical safety standards. The sibling readings
 *   (engineering_absolute_threshold, management_compliance_narrative) are
 *   separate constraint files with their own epsilon and victim structures,
 *   linked through network.affects_constraints; they are deliberately NOT
 *   described inside this constraint.
 *
 * KEY AGENTS:
 *   - - nasa_mission_management: Agenda setter (institutional/arbitrage) — defines 'informed', controls which figures reach the record, signs acceptance; collects the regime's principal gains
 *   - - flight_crews_astronauts: Primary target (moderate/trapped) — flies the accepted residuals; bears physical realization
 *   - - aerospace_working_engineers: Secondary target (moderate/identity_locked) — produces the data; watches categorical voice convert into accepted decimals
 *   - - schedule_dependent_contractors: Primary beneficiary (powerful/mobile) — cadence-dependent revenue legitimized by the acceptance ledger
 *   - - categorical_safety_norms: Displaced standard (non-agent seat) — eroded by the actuarial frame, carried by the actor-seats above
 *   - - aerospace_safety_advisory_panel: Excluded monitor (institutional/trapped) — sees enough to object, lacks standing in the acceptance conversation
 *   - - independent_investigation_boards: Analytical observer (institutional/analytical) — reconstructs decision chains after failures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.66).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.6).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.66).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Documented Failure-Probability Acceptance Requirement (Rogers Findings, Actuarial Reading)").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational safety / technology governance / regulatory compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, 'bb6ac33f-bf99-46a2-b430-4b97eafa5b3a').
narrative_ontology:cs_kernel_codification('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', formalized).
narrative_ontology:cs_authority_grounding('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', expertise).
narrative_ontology:cs_interpretation_layer_present('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a').
narrative_ontology:cs_reading_relation('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', foundational, residual_risk_governable_by_documented_quantification).
narrative_ontology:cs_axiom_status(residual_risk_governable_by_documented_quantification, holdable).
narrative_ontology:cs_axiom_grounding('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', residual_risk_governable_by_documented_quantification, empirically_contingent).
narrative_ontology:cs_axiom('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', secondary, informed_authority_acceptance_confers_operational_legitimacy).
narrative_ontology:cs_axiom_status(informed_authority_acceptance_confers_operational_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', informed_authority_acceptance_confers_operational_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', documented_probabilistic_acceptance_baseline).
narrative_ontology:cs_drift_state('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', post_columbia_caib_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bb6ac33f-bf99-46a2-b430-4b97eafa5b3a', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, nasa_mission_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, schedule_dependent_contractors).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, flight_crews_astronauts).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, aerospace_working_engineers).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, schedule_dependent_contractors).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, probabilistic_risk_assessment_doctrine).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, informed_authority_proxy_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the flight-readiness and acceptance boards, defines who qualifies as an informed decision-maker, controls which failure figures reach the acceptance record, and signs the risk-acceptance documentation. The arrangement concentrates acceptance authority in this seat and supplies documented cover for continuing operations under known hazards; the same seat absorbs career catastrophe when an accepted probability realizes.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, nasa_mission_management, agenda_setter,
    institutional, biographical, arbitrage, national).

% Revenue and follow-on contracts tie to sustained launch cadence; the documented-acceptance regime legitimizes flying through known-hazard windows that a categorical-hold regime would close. Contractors also produce much of the failure data their customer adjudicates, and they bear vehicle-loss and schedule-penalty exposure when an accepted probability realizes.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, schedule_dependent_contractors, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, schedule_dependent_contractors, payer).

% Fly the missions whose residual failure probability others have documented and accepted on their behalf. They hold no formal veto in the acceptance chain, cannot exit mid-flight, and face career and identity termination if they leave the corps; they bear the physical realization of every accepted decimal.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, flight_crews_astronauts, payer,
    moderate, biographical, trapped, national).

% Produce the hazard analyses and failure estimates that feed the acceptance ledger, then watch categorical judgments ('this configuration is not acceptable') convert into quantified acceptances ('the documented probability falls within the accepted envelope'). Once a figure is signed, further categorical dissent reads as insubordination or bad faith. Their professional self-concept is fused with speaking for the hardware, so exiting the chain means abandoning the identity that drew them to the work.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, aerospace_working_engineers, payer,
    moderate, biographical, identity_locked, national).

% The pre-quantitative standard holding that certain configurations are unacceptable regardless of documentation. Each accepted-probability flight erodes its standing, because any hazard can be restated as a survivable statistic. It cannot advocate for itself; its displacement is carried by the engineers and crews named above, who are the actors through which it bears cost.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).

% Chartered to independently monitor program risk, but its data access, staffing, and relevance depend on the agency it audits, and its repeated risk warnings have historically failed to enter the acceptance record. It sees enough to object and lacks the standing to be counted among the informed decision-makers whose acceptance legitimizes flight.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, aerospace_safety_advisory_panel, excluded,
    institutional, generational, trapped, national).

% Convened after failures (the pattern running from the Rogers Commission to the Columbia board), they reconstruct the decision chains, compare official figures against working estimates, and publish findings that feed back into the regime's legitimacy. Analytical seat: they observe the full structure without collecting from it or bearing its consequences.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, independent_investigation_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, nasa_mission_management).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the acceptance problem for irreducibly hazardous operations: it establishes a shared quantified ledger of failure probabilities, routes each flight's residual risk to a defined acceptance authority, and gives engineers, managers, contractors, and overseers one auditable record of who knew what risk and who accepted it.
% TRANSFER_FUNCTION: Moves decision authority upward, from working engineers to accepting managers, and moves realized risk outward onto flight crews; it converts open-ended safety obligations into bounded, signed statistical liabilities whose documentation shields the accepting seat while the consequences land elsewhere.
% ABSENT_VOICES: Working-level engineers whose higher failure estimates were filtered below the acceptance boards; crew representatives without standing in acceptance reviews; chartered external monitors (the Aerospace Safety Advisory Panel pattern) whose repeated warnings never entered the record. Board unanimity partly reflects that these seats were never in the conversation where 'informed decision-makers' are defined.
% DISAPPEARANCE_RATIONALE: Launch adjudication would immediately reorganize: either toward categorical engineering holds (fly only after certified redesign) or toward unstructured manager discretion. The probabilistic-risk-assessment practitioner base, contractor scheduling commitments, crew training risk models, and oversight reporting lines all presuppose the documented-acceptance ledger; removing it overnight strands each of them.
% FOUNDING_PROBLEM: After Challenger, the Rogers Commission found that operational decisions rested on failure probabilities nobody honestly quantified and risks nobody consciously accepted: management cited reliability figures orders of magnitude better than working engineers' estimates, and no defined authority had knowingly accepted the true residual before flight.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: the Rogers Commission report itself (including Feynman's Appendix F reliability analysis) establishes the founding problem; the Columbia Accident Investigation Board attests its recurrence under the reformed regime; organizational scholarship on normalization of deviance independently corroborates the dynamic. Mission management, the primary beneficiary seat, disputes the problem's persistence; its attestation is not relied upon.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.66: the regime's quantification delivers real decision value, but the acceptance step converts categorical prohibitions into acceptable statistics, shifts realized consequence onto trapped and identity-locked seats, and displaces the categorical-safety standard — substantial, asymmetric, but riding a load-bearing coordination core. Suppression 0.60 is procedural: review-board control of figures, career gating, and the reframing of post-signature dissent as insubordination; note that suppression is authored as a raw structural property and is intentionally left unscaled — the engine scales only extractiveness by directionality and scope. Theater 0.44: a large and growing fraction of risk-documentation activity functions as compliance performance (figures produced to ratify decisions already made), though PRA retains genuine analytic work. Accessibility_collapse 0.55: categorical-hold alternatives remain conceptually available but become institutionally expensive to voice once the actuarial frame is the sole decision currency. Resistance 0.55: recurring engineer pushback, external-monitor warnings, and post-accident investigation pressure. Temporal series run on one shared seven-point year grid (1987-2026) with all three metrics authored at every point; trajectories show reform sincerity at establishment, machinery maturation, a pre-Columbia laundering peak (2003), post-accident correction, and partial re-normalization with a small late theater decay as harder-edged quantitative culture entered the commercial-crew era. The dynamics are monotonic-drift with shock corrections rather than cyclical, so no cycle-length series was authored. The 2026 endpoint carries basis 'projected'; earlier points 'observed'.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat the ledger reads as professionalized stewardship: due diligence, traceability, mature risk discipline — the regime computes as coordination from that chair. From the crew and engineer chairs the identical ledger reads as laundering: their categorical objections were converted into signed decimals and closed. Two seats at nominally the same institutional power level diverge on access rather than rank: the chartered advisory panel perceives the structure whole but cannot move a figure, while mission management moves figures but perceives them through schedule commitment. The engine computes these per-seat classifications from power, exit, and role declarations; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map directly onto the directionality gradient: nasa_mission_management sits near the beneficiary pole (collects acceptance authority and liability documentation; its residual career downside when accidents occur keeps d from reaching zero); schedule_dependent_contractors sit near-beneficiary with mobile exit damping extraction, offset somewhat by vehicle-loss exposure borne as secondary payers; flight_crews_astronauts sit near the full-target pole, pushed further by trapped exit (no exit mid-flight, career-terminating departure); aerospace_working_engineers sit high-target with identity_lock amplifying effective extraction beyond their procedural position. categorical_safety_norms is authored agent:false and therefore contributes no directionality — its displacement is registered structurally through the actor seats that carry it. Observer and excluded-analytical seats fall outside the chi arithmetic. No directionality_overrides were needed: declared beneficiary/victim structure plus exit options already separate the seats, and a power-atom-keyed override would have smeared across the several institutional seats that differ here by access rather than by power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — honest quantification plus conscious acceptance at the right level — is contested rather than dead: the machinery persists at full scale, and both the Columbia record and continuing figure-integrity disputes indicate the original failure mode recurs inside the reformed apparatus. Because founding_problem_status='contested' pairs with disappearance_verdict='world_rearranges', the mismatch consumer registers capture-risk scrutiny rather than a clean zombie flag, which is the correct reading for a regime whose coordination core is still load-bearing while its acceptance step degrades toward ritual. The tangled_rope claim prevents the two classic mislabels: classifying the whole regime as snare would erase the quantification function that no launch authority can dispense with; classifying it as rope would erase the documented asymmetry by which acceptance-authority flows up while consequence-bearing flows down. Receipt-surface facts sharpen the picture without reclassifying it: gains demonstrably accrue to the management seat, and fixing (honest figures implying flight rates incompatible with committed budgets) is prohibitively costly relative to diffuse, slow-accruing benefits — the captured-plus-prohibitive cell, recorded as fact while the type claim stays tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment_rogers,
    'Which reading of the Rogers Commission findings does the operative acceptance regime actually instantiate: documented-probability acceptance (this file), an absolute engineering threshold (cease flight until certified redesign), or a management compliance narrative (documented awareness sufficient to proceed)?',
    'Trace which criterion governed actual go/no-go decisions across the interval: certified-redesign holds (threshold reading), signed probability acceptances (actuarial reading), or documented-awareness packages (compliance reading); cross-reference flight-readiness-review minutes against the criterion each launch actually turned on.',
    'Under the threshold reading this constraint''s extraction collapses toward zero and the regime reads as coordination; under the compliance reading theater_ratio dominates and the regime trends toward piton/snare territory; per-seat classifications shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_assignment_rogers, conceptual, 'Committer-frame ambiguity: one kernel, three readings, three structurally distinct constraints with different epsilon.').

omega_variable(
    failure_figure_integrity,
    'Do the documented failure probabilities reflect genuine engineering estimates, or figures negotiated backward from schedule and budget commitments?',
    'Compare contemporaneous working-engineer failure estimates against official published risk figures using the Feynman Appendix F comparison method; audit the revision trail of probability figures ahead of major schedule milestones.',
    'If figures are routinely negotiated, the quantification function becomes laundering: effective extraction on crew and engineer seats rises sharply and the regime trends from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(failure_figure_integrity, empirical, 'Whether the quantified basis of acceptance is honest or schedule-fitted.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is engineer deference within the acceptance chain structural (procedural filtering of figures, career gating, review-board control) or internalized (professional habituation to deferring categorical judgment to quantified authority)?',
    'Post-exit testimony trajectory: if engineers speak categorically after leaving the acceptance chain (the Boisjoly pattern) while remaining silent inside it, suppression is predominantly structural; persistence of self-censorship after exit indicates internalized residue.',
    'If internalized, measured suppression understates durable lock-in: structural reforms of the post-Columbia type would underdeliver because the carried deference survives the procedural fix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of measured suppression between structural filtering and internalized deference.').

omega_variable(
    quantification_necessity_ambiguity,
    'Is documented-probability acceptance the only workable acceptance mechanism for irreducibly nonzero-risk flight operations (approaching natural necessity), or a constructed institutional choice that happens to serve schedule-dependent interests?',
    'Counterfactual institutional comparison: examine programs that operated categorical-threshold regimes without actuarial acceptance (post-Apollo-fire redesign holds, certification-by-analysis limits in commercial aviation) for sustained viability without a quantified acceptance ledger.',
    'If necessity, part of measured extraction is the unavoidable price of flying at all and the effective coordination floor rises; if constructed, the extraction is discretionary and removable by institutional choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantification_necessity_ambiguity, conceptual, 'Whether the actuarial frame is a natural feature of hazardous operations or a contingent construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 1987, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t1987, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1987, 0.25).
narrative_ontology:measurement_basis(roge_tr_t1987, observed).
narrative_ontology:measurement(roge_tr_t1992, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1992, 0.3).
narrative_ontology:measurement_basis(roge_tr_t1992, observed).
narrative_ontology:measurement(roge_tr_t1998, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1998, 0.36).
narrative_ontology:measurement_basis(roge_tr_t1998, observed).
narrative_ontology:measurement(roge_tr_t2003, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2003, 0.44).
narrative_ontology:measurement_basis(roge_tr_t2003, observed).
narrative_ontology:measurement(roge_tr_t2010, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2010, 0.46).
narrative_ontology:measurement_basis(roge_tr_t2010, observed).
narrative_ontology:measurement(roge_tr_t2017, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2017, 0.47).
narrative_ontology:measurement_basis(roge_tr_t2017, observed).
narrative_ontology:measurement(roge_tr_t2026, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2026, 0.44).
narrative_ontology:measurement_basis(roge_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(roge_be_t1987, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1987, 0.48).
narrative_ontology:measurement_basis(roge_be_t1987, observed).
narrative_ontology:measurement(roge_be_t1992, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1992, 0.52).
narrative_ontology:measurement_basis(roge_be_t1992, observed).
narrative_ontology:measurement(roge_be_t1998, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1998, 0.56).
narrative_ontology:measurement_basis(roge_be_t1998, observed).
narrative_ontology:measurement(roge_be_t2003, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2003, 0.63).
narrative_ontology:measurement_basis(roge_be_t2003, observed).
narrative_ontology:measurement(roge_be_t2010, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement_basis(roge_be_t2010, observed).
narrative_ontology:measurement(roge_be_t2017, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2017, 0.65).
narrative_ontology:measurement_basis(roge_be_t2017, observed).
narrative_ontology:measurement(roge_be_t2026, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2026, 0.66).
narrative_ontology:measurement_basis(roge_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t1987, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1987, 0.45).
narrative_ontology:measurement_basis(roge_su_t1987, observed).
narrative_ontology:measurement(roge_su_t1992, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement_basis(roge_su_t1992, observed).
narrative_ontology:measurement(roge_su_t1998, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1998, 0.54).
narrative_ontology:measurement_basis(roge_su_t1998, observed).
narrative_ontology:measurement(roge_su_t2003, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2003, 0.6).
narrative_ontology:measurement_basis(roge_su_t2003, observed).
narrative_ontology:measurement(roge_su_t2010, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement_basis(roge_su_t2010, observed).
narrative_ontology:measurement(roge_su_t2017, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2017, 0.61).
narrative_ontology:measurement_basis(roge_su_t2017, observed).
narrative_ontology:measurement(roge_su_t2026, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2026, 0.6).
narrative_ontology:measurement_basis(roge_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Rogers findings establish X' decomposes into three structurally distinct claims sharing one kernel: actuarial_risk_acceptance (quantify and accept — this file, epsilon ~0.66, victims include crews and engineers carrying displaced categorical norms), engineering_absolute_threshold (cease until certified — low epsilon, coordination-dominant), and management_compliance_narrative (document awareness and proceed — high theater, epsilon concentrated in ritual maintenance). The upstream artifact is the shared findings report; this reading is downstream of it and exerts downstream pressure on the compliance reading, whose documentation substrate the quantification regime created. All three files link mutually through network.affects_constraints; epsilon values differ because the claims differ, not because one constraint is measured inconsistently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
