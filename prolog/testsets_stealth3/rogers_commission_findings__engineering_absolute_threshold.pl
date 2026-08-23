% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [DISCHARGED (sunset fired at certification, Oct 1988)]
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
 *   human_readable: Rogers Findings, Engineering Absolute Threshold Reading: Flight Halt Until O-Ring Redesign Certified
 *   domain: organizational/regulatory/aerospace-safety
 *
 * SUMMARY:
 *   After the loss of Challenger on 28 January 1986, the Rogers Commission
 *   report became the fixed text from which the post-accident order was read.
 *   This story instantiates the engineering_absolute_threshold reading: the
 *   findings establish a technical safety boundary under which flight
 *   operations cease until the solid rocket booster field joint is physically
 *   redesigned and the redesign is certified by test. Under this reading,
 *   booster engineers hold veto authority in Flight Readiness Reviews, no
 *   waiver or documented-risk path exists, and the fleet stands down for
 *   roughly thirty-two months until certification opens the gate (return to
 *   flight, October 1988). The epsilon referent is the standing arrangement
 *   under contest, the certification-gated flight ban itself, assessed by
 *   this reading's own lights: from the engineering seat the gate tracks a
 *   physical failure mode, so the arrangement is held to extract little
 *   beyond what the hardware owes. The claim and the metrics are independent
 *   authored facts: the constraint is CLAIMED as scaffold (transitional
 *   support whose justification is the safe return to flight, carrying a
 *   certification-triggered sunset that actually fired), while the authored
 *   metrics describe its actual operation, a hard, highly suppressive gate
 *   whose costs fell on identifiable seats for the duration of the
 *   stand-down.
 *
 * KEY AGENTS:
 *   - srb_joint_engineers: Agenda-setting seat (organized/identity_locked) — hold veto authority over Flight Readiness Reviews under this reading; set the certification evidence standard
 *   - nasa_launch_schedule_management: Primary target (institutional/trapped) — bears the grounded-manifest cost with no waiver path
 *   - shuttle_astronaut_corps: Primary beneficiary (organized/constrained) — receives the risk reduction the gate produces
 *   - morton_thiokol_srb_division: Dual-positioned payer/beneficiary (institutional/constrained) — absorbs the redesign burden and collects the funded redesign program
 *   - dod_national_security_payload_offices: Secondary target (institutional/constrained) — stranded heavy-payload plans, lobbies for earliest resumption
 *   - commercial_payload_customers: Target with partial exit (organized/arbitrage) — shifts contracts to expendable vehicles where possible
 *   - challenger_51l_crew_families: Excluded moral authority (organized/trapped) — no seat in the review structure, speaks through hearings and press
 *   - rogers_presidential_commission: Analytical observer — source of the kernel text, operates no gate
 *   - congress_oversight_committees: Observing enforcer (institutional/analytical) — converts public reaction into budgetary enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.26).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.82).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.17).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.26).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.17).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, scaffold).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Findings, Engineering Absolute Threshold Reading: Flight Halt Until O-Ring Redesign Certified").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational/regulatory/aerospace-safety").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:has_sunset_clause(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, 'c04b5434-8087-4014-aa86-9bb020bb597b').
narrative_ontology:cs_kernel_codification('c04b5434-8087-4014-aa86-9bb020bb597b', fixed_text).
narrative_ontology:cs_authority_grounding('c04b5434-8087-4014-aa86-9bb020bb597b', expertise).
narrative_ontology:cs_interpretation_layer_present('c04b5434-8087-4014-aa86-9bb020bb597b').
narrative_ontology:cs_reading_relation('c04b5434-8087-4014-aa86-9bb020bb597b', rogers_commission_findings__management_compliance_narrative, coexists_with).
narrative_ontology:cs_reading_relation('c04b5434-8087-4014-aa86-9bb020bb597b', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('c04b5434-8087-4014-aa86-9bb020bb597b', foundational, physical_verification_precedes_flight_resumption).
narrative_ontology:cs_axiom_status(physical_verification_precedes_flight_resumption, holdable).
narrative_ontology:cs_axiom_grounding('c04b5434-8087-4014-aa86-9bb020bb597b', physical_verification_precedes_flight_resumption, empirically_contingent).
narrative_ontology:cs_axiom('c04b5434-8087-4014-aa86-9bb020bb597b', foundational, engineer_veto_over_schedule_authority).
narrative_ontology:cs_axiom_status(engineer_veto_over_schedule_authority, holdable).
narrative_ontology:cs_axiom_grounding('c04b5434-8087-4014-aa86-9bb020bb597b', engineer_veto_over_schedule_authority, deontological).
narrative_ontology:cs_reference_frame('c04b5434-8087-4014-aa86-9bb020bb597b', physical_certification_gate).
narrative_ontology:cs_drift_state('c04b5434-8087-4014-aa86-9bb020bb597b', contemporary_post_caib_era, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('c04b5434-8087-4014-aa86-9bb020bb597b', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, shuttle_astronaut_corps).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, nasa_launch_schedule_management).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, commercial_payload_customers).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, dod_national_security_payload_offices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_srb_division).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_srb_division).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, physical_certification_before_flight).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, engineer_veto_authority_in_readiness_review).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solid rocket booster engineers at Morton Thiokol and NASA (the seal and joint specialists who had documented field-joint rotation and O-ring blow-by before the accident). Under this reading they hold veto authority in Flight Readiness Reviews: no resumption recommendation passes without their concurrence, and they set the test evidence that certification requires. Their professional identity fused with the seal problem years before the accident; Roger Boisjoly's post-accident testimony effectively ended his career, which illustrates that exiting the safety mandate reads from inside as betraying professional duty. What flows to them is authority and the obligation to withhold it.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, srb_joint_engineers, agenda_setter,
    organized, biographical, identity_locked, national).

% Headquarters and program-office managers whose performance is measured in flights per year. The gate removes every lever they normally use to restore cadence: no waivers, no accepted-deviation memos, no schedule trades, no documented-risk sign-off path. Their careers are tied to a manifest that is frozen for the duration, and leaving the program would mean abandoning the institutions they run. They bear the grounded-manifest cost for as long as certification takes.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, nasa_launch_schedule_management, payer,
    institutional, biographical, trapped, national).

% Active and future flight crews who would ride the redesigned booster. After the accident they pressed internally for hardware fixes before any resumption (the chief astronaut's February 1986 memo cataloging outstanding hazards). They receive the risk reduction the gate produces; they cannot opt out of the manifest and would not want to opt out of the protection. Their leverage is moral and professional, exercised through the astronaut office rather than through the review boards themselves.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, shuttle_astronaut_corps, beneficiary,
    organized, biographical, constrained, national).

% Sole-source solid rocket booster contractor. Absorbs the redesign engineering burden, schedule liability, and the reputational damage of the failure attribution to its joint design. Simultaneously receives the funded redesign and recertification program the gate creates, which keeps its booster line and workforce intact through the stand-down. Walking away from the contract would forfeit its core business, so it works the gate rather than resisting it.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_srb_division, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_srb_division, beneficiary).

% Defense users depending on the shuttle for heavy and classified payloads. The grounding strands deployment plans with no substitute heavy-lift capacity ready; they lobby for the earliest possible resumption and accelerate expendable-vehicle programs as a partial hedge. They cannot fly anything through the gate and bear the delay directly.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, dod_national_security_payload_offices, payer,
    institutional, biographical, constrained, national).

% Satellite operators with contracted shuttle launches. The indefinite grounding forces renegotiation of every manifest slot; many shift contracts to the European Ariane and to expendable vehicles, a partial exit that dampens what the gate ultimately costs them. Those without credible alternatives absorb the delay in full.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, commercial_payload_customers, payer,
    organized, biographical, arbitrage, global).

% Families of the lost crew. The people whose deaths anchor the boundary's moral authority have no seat in Flight Readiness Reviews or certification boards; they speak through memorial statements, congressional testimony, and the press, and they oppose any resumption that carries the smell of schedule pressure. Their position cannot be voted out of the conversation because they were never in it.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, challenger_51l_crew_families, excluded,
    organized, generational, trapped, national).

% The presidential commission whose report supplies the kernel text. It documented the joint failure physics, the temperature sensitivity, and the approval chain that normalized escalating erosion, then disbanded. It operates no gate and collects nothing; its findings are subsequently interpreted by the contending readings.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, rogers_presidential_commission, observer,
    institutional, biographical, analytical, national).

% House and Senate committees that fund the agency and hold hearings. They convert post-accident public reaction into budgetary enforcement of the stand-down, and later press for cadence restoration as the redesign matures. They observe, fund, and pressure; they do not operate the gate.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, congress_oversight_committees, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_srb_division).
narrative_ontology:fixing_cost_class(rogers_commission_findings__engineering_absolute_threshold, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an incredible promise into a verifiable gate. After a loss of vehicle caused by a known joint defect waved through under schedule pressure, no internal assurance that the program will be careful is credible; the arrangement solves the flight-resumption commitment problem by requiring physical certification evidence before any crew flies, anchoring resumption authority to test results rather than to managerial assertion.
% TRANSFER_FUNCTION: Moves resumption authority from schedule-holding program management to certification-gated engineering review; moves roughly thirty-two months of grounded operations out of the launch manifest; moves redesign and recertification funding from NASA and contractor budgets into the booster joint redesign program. The arrangement's operation collects no ongoing rent; what moves is authority, time, and budget.
% ABSENT_VOICES: The Challenger 51-L crew, whose deaths anchor the boundary, and their families had no seat in Flight Readiness Reviews or certification boards; they speak only through memorials, hearings, and press. Scientific and commercial users whose missions were indefinitely deferred had no vote on the certification timetable; their interests entered only through congressional pressure and contract renegotiation.
% DISAPPEARANCE_RATIONALE: Within the constraint's operating window, overnight removal means resumption on the as-flown field joint, the configuration with a demonstrated cold-temperature catastrophic failure mode, before any fix exists. The manifest restarts immediately, the redesign program loses its forcing function, and the fleet resumes operating the exact defect the Rogers report documented. Every arrangement built on the stand-down (crew assignments, manifest replans, the funded redesign itself) presupposes the gate.
% FOUNDING_PROBLEM: The solid rocket booster field joint O-rings lost sealing function under cold temperatures and ignition pressure transients, a defect engineers had documented and warned about for years while readiness reviews repeatedly accepted escalating erosion as acceptable risk. The arrangement was built to break the coupling between a known lethal hardware defect and a decision process that had normalized its warning signs.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Rogers Commission report itself, a presidential body external to NASA and Thiokol, documented the joint failure physics and the normalized-deviance approval chain; Richard Feynman's commissioned appendix demonstrated the temperature-resilience loss with a simple ice-water demonstration; House Science and Technology Committee hearings and the post-redesign flight record corroborate that the specific defect was remediated. No party inside the arrangement attests that the founding problem is still live.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.26, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.26, reading-indexed: this reading holds the gate as tracking physical necessity (the joint demonstrably failed within its qualified envelope), so the arrangement extracts little that is not owed to the hardware; the residual reflects judgment-laden certification stringency and delay beyond strict necessity. Suppression is high (0.82) because the gate left launch-cadence seats with no alternative path to flight at all; suppression is a raw structural property and is deliberately NOT scaled by power or scope, unlike extractiveness. Theater is low (0.17): the stand-down was substantively functional (real redesign, real full-scale testing, real certification), with ritual accretion near return to flight. Accessibility collapse is 0.70: within the reading's framework, once the joint physics are accepted, alternatives to the gate largely foreclose, but political alternatives (program cancellation, vehicle replacement) remained open, so collapse is substantial but not total. Resistance is 0.55: real pressure came from defense payload offices, commercial customers, and headquarters schedule advocates, blunted by the post-disaster accountability climate. The temporal series run on one shared nine-point grid (months from the January 1986 grounding to the October 1988 return to flight) so every tracked metric is authored at every examined time point. Base extractiveness is hump-shaped rather than accumulating: it peaks mid-interval as compounded delay maximizes the cost of the gate, then discharges as certification approaches, which is the signature of a scaffold completing its transition rather than of rent accumulation. Suppression requirement plateaus near 0.9 through the crisis-shock phase, erodes gradually as redesign milestones land, and releases sharply at certification. Theater rises gently as documentation and demonstration rituals accumulate ahead of the public return to flight. The base_properties scalars describe the constraint's operative phase (the hard-gate plateau), not the terminal discharge point, which is why the series endpoints sit below the scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (booster engineers), the constraint is legitimate authority they wield: the gate is the instrument through which their pre-accident warnings finally bind. From the primary payer seat (launch schedule management), the same structure is an immovable wall that removed every lever they ordinarily use, experienced as extraction of their manifest and their careers. The beneficiary seat (astronaut corps) experiences it as protection purchased with someone else's schedule. The arbitrage seat (commercial customers) experiences it as a negotiable delay they can partially route around. The engine computes these per-seat classifications from the structural data; the authored scaffold claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The astronaut corps is declared beneficiary and derives a low directionality (the gate subsidizes their survival). Launch schedule management, defense payload offices, and commercial customers are declared victims and derive high directionality; the trapped exit of schedule management and defense offices amplifies their effective extraction toward the full-target end, while the commercial customers' arbitrage-grade exit (shift to Ariane and expendables) damps theirs. The booster contractor sits mid-scale: declared payer on the stakeholder surface with a secondary beneficiary role, absorbing redesign cost while collecting the funded redesign program, so its costs and gains partially offset. The engineers, as agenda setters whose authority exists only through the constraint's continuation, derive beneficiary-side directionality without collecting rents. Observers derive analytical directionality and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a scaffold that discharged correctly, and the classification matters precisely because it prevents two opposite mislabels. Reading the gate as a snare (pure extraction on launch cadence) misses that nobody collected the extracted time as rent: the grounded months were deadweight consumed as delay and rework, and the gate dissolved the moment its justification was satisfied. Reading it as a permanent rope misses that its whole justification was transitional; keeping the absolute ban past certification would have converted it into inertial maintenance. The sunset clause is load-bearing: certification was the sunset trigger, and it fired. Note on the R5 mismatch signal: the founding problem is dead (the defect was remediated and corroborated from outside the benefiting parties) while the disappearance verdict is world_rearranges (within the operating window, removal would have restarted flight on the defective joint). That combination flags mechanically as capture-or-zombie, but here it is a completion signature, not a capture signature: the arrangement rearranged the world while alive and then ended when its function ended, which is what a successful scaffold does. The zombie risk in this kernel lives in the sibling readings, where compliance apparatus and quantified-risk process persist past their justification; those stories carry their own classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the Rogers findings does the report text actually compel: an absolute technical boundary, a compliance process, or a quantified-risk gate?',
    'Comparative institutional outcome analysis across organizations that implemented each reading, plus close textual analysis of the report''s recommendation language against its evidentiary chapters; the text underdetermines the reading, so resolution comes from which implementation the subsequent record vindicates.',
    'If the management reading is adopted, suppression drops sharply (documentation suffices to proceed) and the constraint reclassifies toward a compliance-process hybrid; if the actuarial reading is adopted, the gate becomes a quantified threshold with a waiver path; only under this reading does the absolute no-flight boundary with engineer veto exist at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'This constraint is one of three rival readings of the rogers_commission_findings kernel; the choice of reading determines the constraint''s entire suppression and victim structure.').

omega_variable(
    certification_standard_stringency,
    'What verification standard counted as certified, and how much of the thirty-two-month stand-down was fixed by the physics versus chosen by the certifying authorities?',
    'Archival record of the certification criteria debates (full-scale static firing requirements, joint environmental testing, hot-fire verification) and the schedule trade studies NASA and the contractor ran against them.',
    'A stricter-than-necessary standard lengthens the suppression of launch cadence and raises the extraction the gate imposes on payer seats; a looser standard shortens the stand-down but raises residual crew risk, moving the constraint between scaffold and a weaker, riskier gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_standard_stringency, empirical, 'How much of the gate''s duration and cost was discretionary judgment rather than physical necessity.').

omega_variable(
    veto_authority_durability,
    'Did the engineer veto authority embedded in this reading persist beyond the stand-down, or decay under renewed schedule pressure?',
    'Longitudinal record of Flight Readiness Review governance from return to flight onward, cross-checked against the Columbia Accident Investigation Board''s findings on the recurrence of normalized deviance in a later program era.',
    'If the veto decayed, this reading''s constraint converts over time from a lived gate into theatrical compliance (piton drift in the successor arrangements); if it held, the post-certification steady state is a durable rope-like safety standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_authority_durability, empirical, 'Durability of the reading''s enforcement foundation after its sunset fired.').

omega_variable(
    suppression_basis_structural_vs_political,
    'Was the gate''s suppressive force structural (anchored in certification physics that no manager could argue away) or political (anchored in the post-disaster accountability climate, which fades)?',
    'Counterfactual comparison: enforcement behavior in the months immediately after the accident versus late in the stand-down, when public attention had moved on but the gate held; plus the record of what happened to similar safety boundaries in agencies without an equivalent catalyzing report.',
    'If the suppression was mostly political, the constraint would have lifted as memory faded regardless of certification status (piton or abandoned-boundary trajectory); if structural, the gate persists exactly until certification and no longer, which is what the record shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_basis_structural_vs_political, conceptual, 'Whether the measured suppression was carried by the hardware argument or by the accountability climate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcf_eat_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(rcf_eat_tr_t0, observed).
narrative_ontology:measurement(rcf_eat_tr_t4, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 4, 0.1).
narrative_ontology:measurement_basis(rcf_eat_tr_t4, observed).
narrative_ontology:measurement(rcf_eat_tr_t8, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 8, 0.13).
narrative_ontology:measurement_basis(rcf_eat_tr_t8, observed).
narrative_ontology:measurement(rcf_eat_tr_t12, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 12, 0.15).
narrative_ontology:measurement_basis(rcf_eat_tr_t12, observed).
narrative_ontology:measurement(rcf_eat_tr_t16, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 16, 0.15).
narrative_ontology:measurement_basis(rcf_eat_tr_t16, observed).
narrative_ontology:measurement(rcf_eat_tr_t20, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(rcf_eat_tr_t20, observed).
narrative_ontology:measurement(rcf_eat_tr_t24, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 24, 0.19).
narrative_ontology:measurement_basis(rcf_eat_tr_t24, observed).
narrative_ontology:measurement(rcf_eat_tr_t28, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 28, 0.22).
narrative_ontology:measurement_basis(rcf_eat_tr_t28, observed).
narrative_ontology:measurement(rcf_eat_tr_t32, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 32, 0.25).
narrative_ontology:measurement_basis(rcf_eat_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(rcf_eat_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(rcf_eat_be_t0, observed).
narrative_ontology:measurement(rcf_eat_be_t4, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 4, 0.31).
narrative_ontology:measurement_basis(rcf_eat_be_t4, observed).
narrative_ontology:measurement(rcf_eat_be_t8, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 8, 0.35).
narrative_ontology:measurement_basis(rcf_eat_be_t8, observed).
narrative_ontology:measurement(rcf_eat_be_t12, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 12, 0.38).
narrative_ontology:measurement_basis(rcf_eat_be_t12, observed).
narrative_ontology:measurement(rcf_eat_be_t16, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 16, 0.4).
narrative_ontology:measurement_basis(rcf_eat_be_t16, observed).
narrative_ontology:measurement(rcf_eat_be_t20, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(rcf_eat_be_t20, observed).
narrative_ontology:measurement(rcf_eat_be_t24, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 24, 0.34).
narrative_ontology:measurement_basis(rcf_eat_be_t24, observed).
narrative_ontology:measurement(rcf_eat_be_t28, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 28, 0.28).
narrative_ontology:measurement_basis(rcf_eat_be_t28, observed).
narrative_ontology:measurement(rcf_eat_be_t32, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 32, 0.22).
narrative_ontology:measurement_basis(rcf_eat_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(rcf_eat_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.9).
narrative_ontology:measurement_basis(rcf_eat_su_t0, observed).
narrative_ontology:measurement(rcf_eat_su_t4, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 4, 0.88).
narrative_ontology:measurement_basis(rcf_eat_su_t4, observed).
narrative_ontology:measurement(rcf_eat_su_t8, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 8, 0.86).
narrative_ontology:measurement_basis(rcf_eat_su_t8, observed).
narrative_ontology:measurement(rcf_eat_su_t12, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 12, 0.84).
narrative_ontology:measurement_basis(rcf_eat_su_t12, observed).
narrative_ontology:measurement(rcf_eat_su_t16, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 16, 0.82).
narrative_ontology:measurement_basis(rcf_eat_su_t16, observed).
narrative_ontology:measurement(rcf_eat_su_t20, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 20, 0.8).
narrative_ontology:measurement_basis(rcf_eat_su_t20, observed).
narrative_ontology:measurement(rcf_eat_su_t24, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 24, 0.74).
narrative_ontology:measurement_basis(rcf_eat_su_t24, observed).
narrative_ontology:measurement(rcf_eat_su_t28, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 28, 0.62).
narrative_ontology:measurement_basis(rcf_eat_su_t28, observed).
narrative_ontology:measurement(rcf_eat_su_t32, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 32, 0.35).
narrative_ontology:measurement_basis(rcf_eat_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, shuttle_commercial_payload_phaseout).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, nasa_independent_safety_overhead).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, frr_engineering_veto_persistence).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Rogers findings' covers three structurally distinct constraints corresponding to the three readings of the kernel: this story (engineering_absolute_threshold, a transitional certification gate), management_compliance_narrative (a documentation-sufficiency compliance process), and actuarial_risk_acceptance (a quantified-risk acceptance gate). Each has its own epsilon, beneficiary/victim structure, and classification; they are linked as readings of one kernel through cs_structure.reading_relations rather than merged into one story. Downstream, this reading's operation structurally influenced the commercial payload phaseout (the indefinite grounding made shuttle commercial manifest commitments untenable), the creation of independent safety overhead in the agency, and the persistence of engineering veto authority in readiness reviews after the gate opened; those constraints are declared as network dependents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
