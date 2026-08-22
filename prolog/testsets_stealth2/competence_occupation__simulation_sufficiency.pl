% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation-Sufficiency Doctrine for Competence Occupation
 *   domain: safety_training/high_reliability_organizations
 *
 * SUMMARY:
 *   In high-reliability domains (nuclear operation, aviation, medicine,
 *   emergency response), rare catastrophic events are too infrequent for
 *   ordinary experience to maintain crisis competencies; the
 *   competence_occupation kernel names the commitment that readiness must be
 *   actively exercised. This story instantiates the simulation_sufficiency
 *   reading: the claim that scheduled, high-fidelity simulation drills
 *   constitute sufficient occupation to prevent skill decay. Institutionally
 *   the reading hardened into license-requalification mandates keyed to
 *   simulator hours, making training compliance the operative observable and
 *   elevating the simulation industry to primary beneficiary. Claim and
 *   metrics are authored independently: claimed_type tangled_rope states my
 *   structural belief that the arrangement combines a genuine coordination
 *   function (safe rehearsal of scenarios production cannot provide) with
 *   asymmetric extraction (concentrated vendor receipt, compliance
 *   substituted for verification) held together by active enforcement; the
 *   metric values describe the arrangement's actual operation without being
 *   tuned to any predicted engine output. This is one member of a three-story
 *   constraint family; the sibling readings carry different epsilon values
 *   and beneficiary structures and are linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - national_regulator: Agenda-setter (institutional/analytical) — writes and enforces the simulator-hour requalification rules, collects the audit trail
 *   - simulation_training_vendors: Primary beneficiary (organized/arbitrage) — revenue scales with mandated hours and marketed fidelity upgrades
 *   - internal_training_departments: Secondary beneficiary and day-to-day administrator (organized/constrained) — budget and headcount justified by mandated hours
 *   - licensed_operators: Primary target (moderate/constrained) — duty time and license standing ride on drill completion
 *   - host_organizations: Dual payer-beneficiary (powerful/constrained) — funds the regime, receives the compliance record and rehearsed crews
 *   - end_user_public: Excluded residual-risk bearer (powerless/trapped) — no seat in standard-setting, bears the sufficiency gap
 *   - safety_science_researchers: Analytical observer (analytical/analytical) — documents decay and transfer evidence with no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.6).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.55).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.6).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Sufficiency Doctrine for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "safety_training/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '7710474a-d1b0-421a-83a5-9619d9f0e80c').
narrative_ontology:cs_kernel_codification('7710474a-d1b0-421a-83a5-9619d9f0e80c', formalized).
narrative_ontology:cs_authority_grounding('7710474a-d1b0-421a-83a5-9619d9f0e80c', extraction).
narrative_ontology:cs_interpretation_layer_present('7710474a-d1b0-421a-83a5-9619d9f0e80c').
narrative_ontology:cs_reading_relation('7710474a-d1b0-421a-83a5-9619d9f0e80c', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('7710474a-d1b0-421a-83a5-9619d9f0e80c', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('7710474a-d1b0-421a-83a5-9619d9f0e80c', foundational, simulated_conditions_transfer_to_field_performance).
narrative_ontology:cs_axiom_status(simulated_conditions_transfer_to_field_performance, holdable).
narrative_ontology:cs_axiom_grounding('7710474a-d1b0-421a-83a5-9619d9f0e80c', simulated_conditions_transfer_to_field_performance, empirically_contingent).
narrative_ontology:cs_axiom('7710474a-d1b0-421a-83a5-9619d9f0e80c', foundational, scheduled_drill_frequency_prevents_skill_decay).
narrative_ontology:cs_axiom_status(scheduled_drill_frequency_prevents_skill_decay, holdable).
narrative_ontology:cs_axiom_grounding('7710474a-d1b0-421a-83a5-9619d9f0e80c', scheduled_drill_frequency_prevents_skill_decay, empirically_contingent).
narrative_ontology:cs_axiom('7710474a-d1b0-421a-83a5-9619d9f0e80c', secondary, compliance_documentation_evidences_competence).
narrative_ontology:cs_axiom_status(compliance_documentation_evidences_competence, holdable).
narrative_ontology:cs_axiom_grounding('7710474a-d1b0-421a-83a5-9619d9f0e80c', compliance_documentation_evidences_competence, conventional).
narrative_ontology:cs_reference_frame('7710474a-d1b0-421a-83a5-9619d9f0e80c', simulation_as_canonical_occupation).
narrative_ontology:cs_drift_state('7710474a-d1b0-421a-83a5-9619d9f0e80c', contemporary_post_incident_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7710474a-d1b0-421a-83a5-9619d9f0e80c', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_training_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, internal_training_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, national_regulator).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, licensed_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, end_user_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, host_organizations).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, host_organizations).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, transfer_of_training_hypothesis).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, deliberate_practice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the requalification rules specifying how many simulator hours licensed operators must complete to keep their licenses, and audits training records for completeness. Collects an auditable compliance trail that makes its oversight demonstrable to legislatures and courts. Changing the recognized exercise mechanism requires formal rulemaking against an installed base of certified simulators and familiar audit procedures.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, national_regulator, agenda_setter,
    institutional, generational, analytical, national).

% Designs and sells full-scope simulators, scenario libraries, instructor certification, and maintenance contracts to regulated operators worldwide. Revenue scales with mandated simulator hours and with fidelity upgrades marketed as closing the gap to real conditions. Sells across industries and jurisdictions, so demand conditions in any one sector do not bind it.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_training_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Runs the drill schedule, staffs instructor positions, and produces the completion records the regulator audits. Its budget and headcount are justified by the mandated hours, and its career paths are built inside the training function. It shapes how scenarios are scoped, scheduled, and scored day to day.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, internal_training_departments, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, internal_training_departments, agenda_setter).

% Crews the plants, aircraft, and units. Must accumulate mandated simulator hours to retain licenses, spending duty time in scheduled scenarios scored for completion. Gains genuine practice on rare procedures but is assessed on attendance and checklist performance rather than demonstrated field readiness. Moving employers does not escape the hour requirements, and leaving the profession forfeits the career.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, licensed_operators, payer,
    moderate, biographical, constrained, national).

% Owns the assets and buys the simulators, the drill downtime, and the training overhead. Receives in return a defensible compliance record before its regulator and crews that have rehearsed rare procedures. Cannot decline the mandate without exiting the regulated business itself, and cannot easily substitute unrecognized exercise mechanisms for the recognized ones.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, host_organizations, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, host_organizations, beneficiary).

% Rides the aircraft, lives downstream of the plants, and undergoes the care. Has no seat in the standard-setting rooms where sufficiency is defined and no visibility into drill records; bears whatever gap remains between rehearsed and authentic performance. Cannot opt out of dependence on operated infrastructure.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, end_user_public, excluded,
    powerless, generational, trapped, global).

% Studies skill decay and transfer of training, publishes cohort comparisons between drill dosage and real-incident performance, and reviews incident reports for rehearsal-versus-reality gaps. Holds no enforcement power; its findings enter the system through advisory channels the benefiting seats partly staff.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_science_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_training_vendors).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a safe, schedulable, repeatable mechanism for exercising rare hazardous scenarios that production operations cannot rehearse; standardizes exposure across sites, shifts, and cohorts; and produces a uniform auditable record of who has practiced what.
% TRANSFER_FUNCTION: Moves training budgets, simulator capital expenditure, and crew duty time from host organizations to simulation vendors and training departments; moves the assurance currency from demonstrated field performance to documented drill completion; leaves the residual risk of unexercised failure modes with frontline operators and the public.
% ABSENT_VOICES: End users — patients, passengers, plant neighbors — have no seat anywhere in the standard-setting chain and would object to sufficiency being defined without their risk tolerance on the record. Licensed operators occupy the drills but not the rooms where sufficiency is defined; safety researchers critical of transfer validity are consulted through advisory channels the benefiting seats partly staff.
% DISAPPEARANCE_RATIONALE: If the sufficiency doctrine and its mandate apparatus vanished overnight, license requalification would lose its operative mechanism, vendor revenue and training-department budgets would collapse, hosts would rebuild exercise programs around whichever occupation account their regulators next recognized, and the compliance-audit economy built on drill records would dissolve — the readiness arrangements of every regulated high-reliability sector would reorganize.
% FOUNDING_PROBLEM: Rare catastrophic failures are too infrequent for ordinary experience to maintain the skills that prevent them; between events, unused crisis competencies decay silently, and organizations needed a way to exercise those competencies safely, repeatably, and provably.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the skill-decay research literature documents measurable degradation of rarely used competencies within months without exercise, and accident-investigation bodies repeatedly cite unreadiness in scenarios crews had nominally drilled. What no external source corroborates is the sufficiency answer itself — the transfer question remains open, and the strongest external evidence consists of cohort studies the regime has not resolved either way.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.60 because the arrangement delivers genuine exercise value — rare-event rehearsal that production operations cannot safely provide — while simultaneously channeling a large surplus to the simulation industry and substituting completion records for demonstrated readiness. Suppression is 0.55: the mechanism is compulsory through licensure rather than chosen, and budget logic crowds out complementary exercise mechanisms, but nothing bans organizations from adding line audits or procedural reinforcement on top. Theater_ratio 0.52 reflects a mature compliance culture in which a large share of drill hours are repeat scenarios run to satisfy hour thresholds and produce audit artifacts, while core scenario execution retains real training function. Accessibility_collapse 0.40: the alternative readings remain intellectually live and operationally present in pockets — alternatives have been marginalized, not eliminated. Resistance 0.50: safety-science critique, incident-report friction, and occasional workforce pressure are real but have not threatened the mandate structure. All three temporal series share one grid (points 0-30, step 5). Suppression_requirement is authored because this story specifically traces enforcement maturation — simulator hours were progressively written into license requalification and audit intensity hardened over the interval — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different arrangements from identical facts. From the vendor seat the regime is its product line and the sufficiency premise is the sales proposition; from the training-department seat it is budget justification and professional turf; from the operator seat it is compulsory time scored on completion; from the host seat it is a defensible record purchased at capital cost; from the public seat it is invisible — experienced only as whatever gap remains between rehearsed and authentic performance. Vendors and training departments share the organized power atom yet diverge sharply in exit (arbitrage across industries versus careers bound to the training function) and in what they collect (market revenue versus institutional budget), so equal nominal power does not produce equal exposure. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: vendors and training departments sit at the beneficiary end (low d); operators and the public at the target end (high d); the regulator collects auditability and sits low-moderate. Three overrides correct derivations the declarations alone would misplace. host_organizations (powerful) would derive near-full-target from its cost-bearing position, but it concurrently receives the compliance record and rehearsed crews — overridden to 0.50, near-symmetric. licensed_operators (moderate) would derive near-full-target, but drills confer genuine procedural and team practice they would otherwise never receive — overridden to 0.70, strongly targeted but not maximally. end_user_public (powerless) would derive near-full-target from victim declaration, but any occupation regime confers protective benefit on them; their exposure is the sufficiency gap specifically — overridden to 0.72. Suppression is authored as a raw structural property and is deliberately unscaled; only extractiveness rides directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — skill decay between rare events — is live and externally corroborated, so no dead-mandate declaration is authored; the mismatch consumer should find status=live paired with verdict=world_rearranges and no zombie flag. What has drifted is the observable: the regime was built to maintain competence and now measures completion, a substitution the theater_ratio series tracks crossing 0.5 late in the interval. Mandatrophy resolution runs through the transfer axiom: if cohort evidence shows simulated exercise fails to occupy the kernel that authentic conditions occupy, the mandate outlives its function while its enforcement persists — the decay path runs toward theatrical maintenance of a dead premise or open rent collection on it, and a sibling reading displaces this one. If the axiom holds, the arrangement is a working tangled rope whose extraction is the price of auditable readiness. The classification prevents both mislabels: a pure-coordination framing (the vendor account) erases the compliance-proxy substitution and the concentrated receipt; a pure-extraction framing erases the genuine rehearsal function nothing else safely replicates. Receipt concentrates in one named seat (the vendors) while fixing is prohibitive — rebuilding requalification around multi-mechanism occupation would require rewriting license rules across jurisdictions, unwinding vendor contracts, and retraining the instructor corps, a cost dwarfing any single seat's benefit from fixing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does scheduled simulation exercise actually occupy the competence kernel that authentic rare-event conditions occupy, or is this reading one contestable answer among siblings (real_incident_necessity, hybrid_occupation) to the competence_occupation kernel?',
    'Longitudinal cohort studies comparing real-incident performance across regimes differing in occupation mechanism, plus natural experiments where jurisdictions shifted between simulation-only and hybrid requalification.',
    'If sufficiency fails, this reading loses its warrant, the constraint drifts toward rent collection on a false premise, and a sibling reading displaces it; if it holds, the measured extraction is largely the price of auditable readiness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Whether the simulation_sufficiency reading of the competence_occupation kernel survives empirical contest by its siblings.').

omega_variable(
    fidelity_transfer_gap,
    'Does high-fidelity simulation produce durable, field-transferable skill for rare-event competencies, or does it primarily train scenario recognition that degrades under authentic stress and novelty?',
    'Transfer-of-training meta-analysis correlating simulator dosage and fidelity with blinded field-performance measures; incident coding for rehearsal-present-yet-failed events.',
    'Determines whether the coordination function is substantive (the tangled-rope structure holds) or largely nominal (extraction dominates and computed type drifts upward).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_transfer_gap, empirical, 'Size and durability of the transfer gap between simulated rehearsal and authentic performance.').

omega_variable(
    compliance_proxy_validity,
    'Is drill-completion a valid observable for maintained competence, or has Goodhart substitution occurred in which completion is optimized while competence is not?',
    'Unannounced blind competency assessments compared against completion-record standing for matched crews.',
    'If the proxy is invalid, theater_ratio understates functional loss, the compliance observable maintains paperwork rather than readiness, and the cost of fixing rises further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_proxy_validity, empirical, 'Validity of training-compliance metrics as evidence of maintained competence.').

omega_variable(
    vendor_capture_directionality,
    'Is the sufficiency premise sustained by transfer evidence or by vendor influence over standards committees, certification curricula, and regulator advisory panels?',
    'Disclosure analysis of advisory-panel composition, funding flows, and procurement lobbying records across jurisdictions.',
    'If capture is substantial, effective extraction exceeds the authored base and the regulator seat''s directionality shifts upward from its derived low value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_directionality, empirical, 'Degree to which the reading''s institutional stability reflects evidence versus interested-party influence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is operator non-exit from the drill-centric regime structural (licensure legally routes through mandated simulator hours) or internalized (professional identity fused with completion culture such that operators defend the regime that measures them)?',
    'Cross-jurisdiction comparison where mandate stringency differs, and post-career interviews: if criticism surfaces only after license dependence ends, the lock is substantially internalized.',
    'If internalized, suppression persists even where mandates relax, and relaxing the mandate would not restore alternative mechanisms without identity-level change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized component of the regime''s hold on operators.').

omega_variable(
    residual_risk_tolerance,
    'Is the residual decay risk left by simulation-only occupation acceptable given the cost of hybrid multi-mechanism regimes, or does the public''s excluded position invalidate a tradeoff made on its behalf?',
    'Quantified risk-cost analysis subjected to a public regulatory process that actually seats the affected public rather than deferring to incumbent parties.',
    'A rejected tradeoff converts the arrangement''s residual risk from accepted coordination cost into imposed harm, raising effective extraction for the excluded seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_risk_tolerance, preference, 'Value question over the acceptable residual risk of the sufficiency settlement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__simulation_sufficiency, theater_ratio, 5, 0.24).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__simulation_sufficiency, theater_ratio, 10, 0.3).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__simulation_sufficiency, theater_ratio, 15, 0.37).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.43).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__simulation_sufficiency, theater_ratio, 25, 0.48).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__simulation_sufficiency, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comp_be_t5, competence_occupation__simulation_sufficiency, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(comp_be_t10, competence_occupation__simulation_sufficiency, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(comp_be_t15, competence_occupation__simulation_sufficiency, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(comp_be_t25, competence_occupation__simulation_sufficiency, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(comp_be_t30, competence_occupation__simulation_sufficiency, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(comp_su_t5, competence_occupation__simulation_sufficiency, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(comp_su_t10, competence_occupation__simulation_sufficiency, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(comp_su_t15, competence_occupation__simulation_sufficiency, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(comp_su_t25, competence_occupation__simulation_sufficiency, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(comp_su_t30, competence_occupation__simulation_sufficiency, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, hybrid_occupation).

% DUAL FORMULATION NOTE:
% The colloquial label 'keeping operators competent' decomposes into three structurally distinct claims about what occupies the competence kernel (epsilon-invariance): this file is the simulation_sufficiency instantiation; real_incident_necessity and hybrid_occupation are separate stories with their own epsilon, beneficiaries, and victims. Family links run through affects_constraints in all three files. Upstream/downstream structure: the transfer-of-training evidence base is cited BY this reading as warrant, so empirical results propagate from the research layer into this reading's legitimacy conditions without resolving the dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, powerful, 0.5).
constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, moderate, 0.7).
constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
