% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Post-Challenger Certification Gate on Shuttle Flight Operations (Engineering Absolute Threshold Reading)
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   Following the loss of Challenger in January 1986, the Rogers Commission
 *   investigated the solid-rocket-booster field-joint failure. This story
 *   instantiates ONE reading of those findings — the
 *   engineering_absolute_threshold reading: the findings establish a hard
 *   technical boundary under which flight operations cease until the joint is
 *   redesigned and certified, with veto authority over Flight Readiness
 *   Reviews vested in the engineering chain. Under this reading the
 *   arrangement runs for the 32-month stand-down (interval 0-32, months from
 *   January 1986): launch operations suppressed, the joint redesigned and
 *   qualification-tested, crews protected, and launch cadence — program
 *   schedules, payload deployments, contractor employment — bearing the cost.
 *   The claim and the metrics are authored independently: claimed_type states
 *   the structure I believe true (a genuine protective coordination function
 *   enforced actively, with asymmetric costs landing on launch-cadence
 *   holders); the metric values state what I believe descriptively accurate
 *   of the arrangement's actual operation. The two sibling readings of the
 *   same kernel are separate constraint files, not averaged here.
 *
 * KEY AGENTS:
 *   - - srbo_project_engineers: Agenda-setting seat (organized/identity_locked) — hold veto authority over Flight Readiness Reviews; the boundary is theirs to invoke
 *   - - shuttle_flight_crews: Primary beneficiary (organized/identity_locked) — the protection the boundary purchases
 *   - - nasa_program_managers: Primary target (powerful/constrained) — bear schedule, political, and career costs; administer the reviews they cannot overrule
 *   - - commercial_payload_owners: Secondary target (moderate/arbitrage) — stranded payloads and lost deployment windows; eventual exit to foreign launch providers
 *   - - contractor_launch_workforce: Diffuse target (powerless/trapped) — layoffs and regional economic damage with no seat in the process
 *   - - congressional_oversight_committees: Analytical observer (institutional/analytical) — fund the redesign, audit the delay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.48).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.75).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.48).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Post-Challenger Certification Gate on Shuttle Flight Operations (Engineering Absolute Threshold Reading)").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, '29c33522-c645-4685-82f4-62b8beded42b').
narrative_ontology:cs_kernel_codification('29c33522-c645-4685-82f4-62b8beded42b', fixed_text).
narrative_ontology:cs_authority_grounding('29c33522-c645-4685-82f4-62b8beded42b', expertise).
narrative_ontology:cs_interpretation_layer_present('29c33522-c645-4685-82f4-62b8beded42b').
narrative_ontology:cs_reading_relation('29c33522-c645-4685-82f4-62b8beded42b', rogers_commission_findings__management_compliance_narrative, forecloses).
narrative_ontology:cs_reading_relation('29c33522-c645-4685-82f4-62b8beded42b', rogers_commission_findings__actuarial_risk_acceptance, forecloses).
narrative_ontology:cs_axiom('29c33522-c645-4685-82f4-62b8beded42b', foundational, uncertified_critical_anomaly_prohibits_flight).
narrative_ontology:cs_axiom_status(uncertified_critical_anomaly_prohibits_flight, holdable).
narrative_ontology:cs_axiom_grounding('29c33522-c645-4685-82f4-62b8beded42b', uncertified_critical_anomaly_prohibits_flight, deontological).
narrative_ontology:cs_axiom('29c33522-c645-4685-82f4-62b8beded42b', secondary, engineering_final_authority_on_flight_readiness).
narrative_ontology:cs_axiom_status(engineering_final_authority_on_flight_readiness, holdable).
narrative_ontology:cs_axiom_grounding('29c33522-c645-4685-82f4-62b8beded42b', engineering_final_authority_on_flight_readiness, instrumental).
narrative_ontology:cs_reference_frame('29c33522-c645-4685-82f4-62b8beded42b', certification_gated_flight_operations).
narrative_ontology:cs_drift_state('29c33522-c645-4685-82f4-62b8beded42b', late_shuttle_program_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29c33522-c645-4685-82f4-62b8beded42b', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, shuttle_flight_crews).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, nasa_program_managers).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, commercial_payload_owners).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, contractor_launch_workforce).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, absolute_design_envelope_limits).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, engineering_veto_authority).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_causal_findings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solid-rocket-booster and seal engineers at the motor contractor and inside the agency. After the accident their joint-temperature analyses became the gate: they certify the redesigned field joint or flight does not proceed, and an engineering objection at a Flight Readiness Review halts the manifest. Before the accident the same analyses were overruled in a late-night teleconference vote; the post-accident structure reverses that burden. Going quiet or leaving the profession would abandon the guardian role several of them had already testified at personal career cost to establish.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, srbo_project_engineers, agenda_setter,
    organized, biographical, identity_locked, national).

% The astronaut corps assigned to upcoming missions. They receive the margin the boundary purchases: a redesigned field joint, tested across the temperature range, certified before anyone straps in. They cannot opt out of flying — flying is the job they trained decades for — and their office gained a formal voice in flight reviews after the accident. Their protection is the stated purpose of the whole arrangement.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, shuttle_flight_crews, beneficiary,
    organized, biographical, identity_locked, national).

% Agency officials who own the launch manifest, the budget, and the political relationship with Congress and the White House. The boundary costs them the thing they are evaluated on: flights flown. They chair the readiness reviews but cannot overrule an engineering hold, and every quarter of stand-down draws oversight hearings and budget risk. Resigning forfeits careers built on the program; proceeding without certification is barred.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, nasa_program_managers, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, nasa_program_managers, agenda_setter).

% Satellite operators whose spacecraft were queued for shuttle deployment. The stand-down stranded completed satellites, froze revenues, and triggered insurance claims; several eventually contracted with the European Ariane launcher instead, but switching took years and did not recover the lost deployment windows. While queued, they bore the full cost of every additional month of delay.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, commercial_payload_owners, payer,
    moderate, immediate, arbitrage, global).

% Technicians and production staff at the launch site, the booster plant, and the propulsion facilities. The stand-down eliminated overtime and then positions — thousands of layoffs across the Florida coast, the Wasatch front, and northern Alabama. Skills and mortgages tied them to regions with no comparable aerospace employment; they had no practical way out and no seat in the flight-review process.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, contractor_launch_workforce, payer,
    powerless, immediate, trapped, regional).

% House and Senate committees that fund the agency and ran the parallel investigations. They appropriate the redesign budget, demand schedule accountability, and audit the certification evidence. They neither fly nor build; their interest is institutional accountability and constituent employment.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, congressional_oversight_committees, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__engineering_absolute_threshold, srbo_project_engineers).
narrative_ontology:fixing_cost_class(rogers_commission_findings__engineering_absolute_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes the go/no-go flight judgment in the seat holding the relevant failure-mode knowledge, so that no mission proceeds past a known, uncertified catastrophic mode. It solves the collective problem that schedule-driven institutions systematically discount low-probability, high-consequence engineering warnings.
% TRANSFER_FUNCTION: Moves schedule certainty, launch revenue, and career capital away from launch-cadence holders — program management, payload owners, contractor workforce — and converts them into redesign time, test campaigns, and flight-envelope margin held for the crews. Decision rights over flight move from program management to the engineering chain.
% ABSENT_VOICES: The Challenger crew cannot speak. Senior NASA and contractor management — the natural holders of the sibling readings — were present but outvoted rather than absent, and their objection survives as political pressure on the return-to-flight timeline. Stranded payload customers and laid-off workers have no seat in the readiness reviews at all; they would object that the boundary prices their livelihoods at zero, and their dissent routes through insurers, foreign competitors, and constituent complaints rather than the technical forum.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, the manifest would reopen under intact schedule pressure and the known joint vulnerability would again be managed by judgment calls in teleconferences — the exact decision structure that produced the accident. Every downstream arrangement (review choreography, certification test campaigns, the astronaut office's review voice, the redesign program itself) exists because the threshold stands.
% FOUNDING_PROBLEM: Flightworthiness decisions made under schedule pressure had repeatedly overridden documented engineering warnings about the solid-rocket-booster field joint, culminating in the destruction of Challenger and the deaths of her seven crew members on 1986-01-28.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Columbia Accident Investigation Board (2003) found the same schedule-pressure-over-engineering-objection pattern recurring with a different failure mode, and Diane Vaughan's independent sociological study of the launch decision documents the normalization dynamic the boundary was built to interrupt. No party outside the engineering and crew-safety set attests that the founding problem is resolved.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.48 at interval end) is substantial but bounded: the stand-down removed roughly two and a half years of launch cadence, stranded the commercial payload queue, and cut thousands of jobs, yet the burden decayed as certification completed rather than ratcheting. Suppression (0.75) is high and unscaled-by-design: flight is prohibited outright, with no alternative path to orbit through this system — the only exit from the suppression is certification itself. Theater ratio (0.20) is low: unlike the pre-accident review process, whose sign-offs were largely ceremonial, the post-accident gate is backed by real test articles, fracture analysis, and hot-fire verification; the modest rise late in the interval reflects documentation burden accreting as the crisis urgency faded. Accessibility collapse (0.68): once the joint's thermodynamics are understood, fly-as-it-is collapses as a defensible option within this frame, though quantified-risk alternatives remain conceptually available — they are precisely the sibling readings. Resistance (0.55): sustained schedule and political pressure on the return-to-flight timeline, short of open defiance. Enforcement capacity (suppression_requirement series) intensifies sharply after the commission reports, peaks during redesign oversight, and plateaus as the veto settles into review structure. All three series share one time grid (t = 0, 4, 8, 16, 24, 32) so no metric is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   From the engineering seat the arrangement is the minimum honest governance: physics sets the terms, certification is the receipt. From the program-management seat the same structure is a veto held over their deliverables by colleagues who bear none of the schedule cost — and who chair meetings they no longer control. From the payload owner's seat it is a closure they pay for without a vote. The engine computes these per-seat classifications from power, exit, and role; the three sibling readings of the kernel are this perspectival gap hardened into three incompatible doctrines.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared structure maps to directionality as follows. Shuttle flight crews are declared beneficiaries with no cost-bearing role — a near-full subsidy seat. The engineering chain is the administering seat: it collects decision authority rather than paying schedule costs, and the receipt surface records that concentration of gain. Program managers are declared victims with constrained exit — near-full target. Payload owners are declared victims whose arbitrage-grade exit (foreign launch vehicles) would mislead the automatic derivation toward the beneficiary end; the directionality override pins the moderate power atom near full target because their losses were fully absorbed before any usable exit matured. The contractor workforce is victim and trapped — maximal effective exposure at minimal power. Suppression is authored as a raw structural property (0.75) and is not scaled by power or scope; only extractiveness is scaled downstream by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandate-atrophy declaration is authored: the founding problem is live, corroborated externally by the Columbia investigation, which found schedule pressure again overriding known hazards fourteen years later. The classification work here is keeping two truths simultaneous. The boundary performs a real, irreplaceable protective function — which blocks a pure-extraction reading, since the coordination story is not cover. And it transfers heavy, asymmetric costs onto actors with little recourse — which blocks a pure-coordination reading, since participants are not net beneficiaries in common. Active enforcement is constitutive, not incidental: the historical record shows the identical review forum producing the opposite answer once the veto was removed, so the arrangement holds only while the enforcement structure holds. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the Rogers findings is structurally operative for shuttle flight decisions — the absolute engineering threshold instantiated here, the management compliance narrative, or actuarial risk acceptance?',
    'Comparative classification across the three linked reading-stories, plus observed Flight Readiness Review conduct in collision cases: an uncertified anomaly accompanied by documented management acceptance of the risk.',
    'If a sibling reading becomes operative, this constraint''s suppression of launch operations drops sharply (process documentation or signed risk acceptance replaces hard gating), the victim set shifts from launch cadence toward dissenting engineers, and the classification migrates accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which of three rival readings of the Rogers findings governs the flight decision.').

omega_variable(
    engineering_veto_durability,
    'Does engineer veto authority over Flight Readiness Reviews persist under renewed schedule pressure, or decay into advisory status?',
    'Track review conduct in subsequent anomaly disputes: whether schedule-holding managers can proceed over engineering objection, as before the accident, or whether objection halts the manifest.',
    'If the veto decays to advisory, the gate''s enforcement becomes largely performative and the arrangement drifts toward inertial-theatrical signatures; if durable, the hybrid coordination-plus-cost structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_veto_durability, empirical, 'Durability of the engineering veto under renewed schedule pressure.').

omega_variable(
    safety_margin_vs_institutional_self_protection,
    'Is the boundary''s operative beneficiary flight-crew safety, or institutional self-protection of the program and the engineering apparatus?',
    'Compare the gate''s application across cases matched for technical risk but differing in political salience; equal rigor across cases supports crew-safety primacy, differential rigor supports self-protection.',
    'If self-protection dominates, part of the cost borne by launch cadence functions as rent for the safety apparatus rather than purchased protection, pushing the classification toward the extraction-heavy end of the hybrid range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_margin_vs_institutional_self_protection, conceptual, 'Whether the protective function serves crews primarily or the institution secondarily.').

omega_variable(
    redesign_closure_sufficiency,
    'Did the certified joint redesign eliminate the cold-weather failure mode, or relocate risk into new interfaces (joint heaters, the capture feature, assembly variability)?',
    'Long-run flight history of the redesigned joint combined with independent mechanics review of the qualification test campaign.',
    'If the redesign relocated rather than closed the hazard, the boundary''s protective yield is smaller than claimed and its cost-to-protection ratio worsens, increasing pressure toward the extraction-heavy end of the classification range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redesign_closure_sufficiency, empirical, 'Whether certification closed or merely relocated the joint hazard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcf_eng_thresh_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(rcf_eng_thresh_tr_t0, observed).
narrative_ontology:measurement(rcf_eng_thresh_tr_t4, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 4, 0.15).
narrative_ontology:measurement_basis(rcf_eng_thresh_tr_t4, observed).
narrative_ontology:measurement(rcf_eng_thresh_tr_t8, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(rcf_eng_thresh_tr_t8, observed).
narrative_ontology:measurement(rcf_eng_thresh_tr_t16, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 16, 0.14).
narrative_ontology:measurement_basis(rcf_eng_thresh_tr_t16, observed).
narrative_ontology:measurement(rcf_eng_thresh_tr_t24, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 24, 0.17).
narrative_ontology:measurement_basis(rcf_eng_thresh_tr_t24, observed).
narrative_ontology:measurement(rcf_eng_thresh_tr_t32, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 32, 0.2).
narrative_ontology:measurement_basis(rcf_eng_thresh_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(rcf_eng_thresh_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(rcf_eng_thresh_be_t0, observed).
narrative_ontology:measurement(rcf_eng_thresh_be_t4, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 4, 0.55).
narrative_ontology:measurement_basis(rcf_eng_thresh_be_t4, observed).
narrative_ontology:measurement(rcf_eng_thresh_be_t8, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 8, 0.63).
narrative_ontology:measurement_basis(rcf_eng_thresh_be_t8, observed).
narrative_ontology:measurement(rcf_eng_thresh_be_t16, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(rcf_eng_thresh_be_t16, observed).
narrative_ontology:measurement(rcf_eng_thresh_be_t24, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 24, 0.54).
narrative_ontology:measurement_basis(rcf_eng_thresh_be_t24, observed).
narrative_ontology:measurement(rcf_eng_thresh_be_t32, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 32, 0.48).
narrative_ontology:measurement_basis(rcf_eng_thresh_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(rcf_eng_thresh_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(rcf_eng_thresh_su_t0, observed).
narrative_ontology:measurement(rcf_eng_thresh_su_t4, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 4, 0.7).
narrative_ontology:measurement_basis(rcf_eng_thresh_su_t4, observed).
narrative_ontology:measurement(rcf_eng_thresh_su_t8, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 8, 0.8).
narrative_ontology:measurement_basis(rcf_eng_thresh_su_t8, observed).
narrative_ontology:measurement(rcf_eng_thresh_su_t16, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 16, 0.82).
narrative_ontology:measurement_basis(rcf_eng_thresh_su_t16, observed).
narrative_ontology:measurement(rcf_eng_thresh_su_t24, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 24, 0.78).
narrative_ontology:measurement_basis(rcf_eng_thresh_su_t24, observed).
narrative_ontology:measurement(rcf_eng_thresh_su_t32, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 32, 0.75).
narrative_ontology:measurement_basis(rcf_eng_thresh_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Rogers Commission findings' decomposes, per epsilon-invariance, into three structurally distinct constraints sharing one fixed-text kernel: this engineering_absolute_threshold reading (hard certification gate; victims = launch cadence), management_compliance_narrative (documented risk awareness sufficient to proceed; different epsilon and a different victim set — dissenting engineers), and actuarial_risk_acceptance (quantified-risk sign-off by informed decision-makers). Each is authored as its own story with its own epsilon, beneficiaries, and victims; all three link here. This reading is upstream of its siblings in operating-environment terms: once certification becomes the reference condition, the compliance reading describes activity around the gate and the actuarial reading describes attempts to waive it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__engineering_absolute_threshold, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
