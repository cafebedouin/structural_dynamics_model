% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Drill-and-Inspection Readiness Regime (Competence Reading)
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   This file instantiates the competence_reading of the
 *   preparedness_persistence kernel: the claim that drills and inspections
 *   are live exercised knowledge — that practice maintains operational
 *   readiness. The standing arrangement under contest (and therefore the
 *   epsilon referent, fixed across all sibling readings) is the institutional
 *   regime of mandated drill hours, scheduled inspections, deficiency
 *   citations, and corrective-action loops. Assessed by this reading's own
 *   lights, the regime solves a genuine collective-action problem —
 *   perishable-skill maintenance under rarity and turnover — at approximately
 *   the cost of the coordination itself, with participants as net
 *   beneficiaries and no identifiable extraction victim. Per Rule 1, the
 *   contest with the husk and hybrid readings is NOT described inside the
 *   constraint; the siblings are separate files linked through network and
 *   cs_structure. The claim/metric independence rule is honored: claimed_type
 *   is rope because that is what this reading holds structurally true, and
 *   the metrics are authored at the low values this reading believes
 *   descriptively true — the engine computes per-seat classifications from
 *   the structural data, and any divergence is signal, not error. The
 *   physical-infrastructure layer of preparedness (a mountain-flavored
 *   component) is excluded from this file per the epsilon-invariance
 *   principle and noted in the network decomposition.
 *
 * KEY AGENTS:
 *   - fire_safety_regulators: agenda-setting administrator (institutional/constrained) — writes inspection codes and drill mandates, compels corrective action
 *   - frontline_responders: primary cost-bearing participant (organized/constrained) — executes drills, surrenders duty hours, holds the perishable skills
 *   - facility_operators: organizing payer (powerful/constrained) — schedules drills, funds remediation, bears citation and liability exposure
 *   - protected_populations: principal beneficiary (powerless/constrained) — receives maintained readiness, bears incidental drill disruption
 *   - insurance_underwriters: secondary beneficiary (institutional/arbitrage) — converts inspection records into priced risk signal
 *   - disaster_research_community: analytical observer (analytical/analytical) — supplies the transfer-validity evidence the other seats argue with
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.2).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Drill-and-Inspection Readiness Regime (Competence Reading)").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "institutional/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_persistence__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, 'fd3fb541-f084-4ac3-a97a-b4ba76a37fbe').
narrative_ontology:cs_kernel_codification('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', formalized).
narrative_ontology:cs_authority_grounding('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', expertise).
narrative_ontology:cs_interpretation_layer_present('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe').
narrative_ontology:cs_reading_relation('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', foundational, rehearsal_preserves_operational_readiness).
narrative_ontology:cs_axiom_status(rehearsal_preserves_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', rehearsal_preserves_operational_readiness, empirically_contingent).
narrative_ontology:cs_axiom('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', foundational, inspection_reports_real_condition).
narrative_ontology:cs_axiom_status(inspection_reports_real_condition, holdable).
narrative_ontology:cs_axiom_grounding('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', inspection_reports_real_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', practice_preserves_competence).
narrative_ontology:cs_drift_state('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', contemporary_transfer_validity_debate, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('fd3fb541-f084-4ac3-a97a-b4ba76a37fbe', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, frontline_responders).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, protected_populations).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, insurance_underwriters).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, facility_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, facility_operators).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, protected_populations).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, skill_decay_without_practice).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, rehearsal_transfers_to_performance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Statutory bodies — fire marshals, occupational-safety agencies, accreditation surveyors — that write inspection codes, set required drill hours and intervals, schedule surveys, cite deficiencies, and compel corrective action. They do not fight fires or run hospitals; their instrument is citation and closure authority. Agency budgets and staffing scale with the inspection mandate, and they are bound to their own statutes: they can tighten or relax requirements but cannot step outside the system they administer.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, fire_safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Firefighters, nurses, plant operators, and teachers who execute the drills and live with inspection findings. They surrender recurring duty hours to rehearsal, carry the physical and cognitive load of realistic exercises, and absorb scenario stress. In exchange they hold perishable skills that protect them first when an event occurs. Many belong to unions that negotiate drill frequency and realism ceilings; transferring to administrative roles is possible but ends the operational career they trained for.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, frontline_responders, beneficiary).

% Hospital administrators, school districts, plant managers, and building owners who must schedule drills, document compliance, and fund the corrective work that inspections surface. They allocate the production time the arrangement consumes and bear citation and liability exposure for lapses. They can reshape drill format and frequency within mandated floors and routinely do so in budget cycles, but liability, accreditation, and insurance terms close off outright nonparticipation.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, facility_operators, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, facility_operators, payer).

% Building occupants, patients, students, and residents whose safety the maintained readiness serves. They receive the benefit without designing anything, and some bear incidental costs — students undergoing lockdown drills, patients whose care paths are diverted during hospital exercises. They neither schedule nor execute the activities and encounter them mainly when one interrupts their day or saves their lives.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, protected_populations, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, protected_populations, payer).

% Property and casualty carriers that price fire and liability coverage against inspection records and drill documentation. Verified inspection histories shrink their loss exposure and sharpen their risk models; they discount premiums for compliant buildings, converting the arrangement's output into a priced signal. They are not bound to any jurisdiction's program and can reprice or withdraw from a market at will.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, insurance_underwriters, beneficiary,
    institutional, generational, arbitrage, national).

% Emergency-management researchers and after-action analysts who study whether drilled organizations outperform undrilled ones. They publish transfer-validity findings, criticize announcement distortions, and supply the evidence base the other seats argue with. They hold no operational stake in any particular drill program and can redirect attention across the whole domain.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, disaster_research_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps rare-event response capability executable across long quiescent periods and personnel turnover: rehearsals maintain fluent execution of shared emergency procedures, inspections verify that protective equipment and structural safeguards remain functional, and both preserve institutional memory that attrition and disuse would otherwise erase.
% TRANSFER_FUNCTION: Moves recurring staff hours and operational attention from routine production into rehearsal and corrective work; moves inspection findings into mandated repair and remediation spending; and moves verified-readiness information outward to regulators, insurers, and the public as certified or priced signal.
% ABSENT_VOICES: Populations subjected to drills without consent — students in lockdown drills, patients near hospital exercises — bear costs but rarely sit on the committees that design drill programs; their objections surface obliquely as drill-anxiety complaints and care-disruption grievances rather than in agenda-setting seats. Front-line staff shape drill execution far more than drill design. Both groups are represented here as seated stakeholders, but their design-time voice inside real programs is thin.
% DISAPPEARANCE_RATIONALE: If the drill-and-inspection regime vanished overnight, nothing visible breaks immediately — which is exactly why its value is chronically undervalued in any single budget cycle. Within months, unannounced-response proficiency measurably decays; within years, equipment defects accumulate undetected, turnover dilutes institutional memory, and major-event outcomes drift toward the undrilled baseline documented in historical loss records. The world rearranges badly, on a lag.
% FOUNDING_PROBLEM: Emergencies are rare relative to working life, and response capability decays without use: organizations built drill-and-inspection regimes to keep rare-event competence alive despite long quiescence, staff turnover, and equipment aging.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national fire-incident and disaster-epidemiology databases record performance differences between drilled and undrilled organizations; after-action investigations of major incidents compare trained and untrained unit outcomes; insurance actuarial tables price inspected buildings differently from uninspected ones; and the academic emergency-management literature treats skill decay as an established finding predating and independent of any particular regime. The founding problem's existence is attested by loss records, not by the parties the regime employs.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).
:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because no rent gap is identifiable in this reading: the regime's costs — duty hours, remediation spending, administrative overhead — track the coordination being purchased, and the regime's product (executable readiness) flows back to the same parties that pay. Suppression is low (0.20): mandates exist and citations bite, but drill format, frequency above the floor, and training method are genuinely negotiable, and workable substitutes (tabletop exercises, high-fidelity simulation, after-action review) persist — hence accessibility_collapse at 0.30 rather than the near-total collapse of a natural law. Resistance is mild (0.25): drill fatigue, scheduling friction, and occasional gaming of announced inspections, but no organized opposition, because no seat experiences net harm. Theater_ratio is low (0.15) — the competence reading's defining assertion — while remaining nonzero because announced drills and checklist formalization introduce real performative residue even under favorable assumptions. The measurement series run on one shared time grid (points 0-30 at intervals of 6) with both tracked metrics authored at every point; both show slow upward creep (compliance documentation accretes as regimes mature) that stays well below drift thresholds. Suppression_requirement is deliberately NOT tracked as a series: the enforcement picture is stable across the interval, so the scalar in base_properties carries it. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream by directionality and spatial scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the regulator's position the arrangement is a functioning verification system it administers; from the responder's position it is a recurring time tax repaid in skill security — the same hours read as burden or as insurance depending on which side of the exercise the seat stands. Facility operators experience the regime as a compliance cost with liability upside; insurers experience it as information; the protected population experiences it almost entirely through its outputs (interruptions and rescues) rather than its operation. A note on identity: drill participation is woven into responder professional identity ('we drill because we are the kind of people who show up ready'), but exit is not identity-locked in this reading — responders can and do transfer to administrative roles — so the identity dimension colors willingness without binding it. The engine computes these divergences from power, exit, and role data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (responders, populations, insurers, operators) derive low directionality — the arrangement subsidizes them in the competence reading's account. Frontline responders carry the largest cost share (recurring duty hours) but recoup it in retained capability, placing them near symmetric with a slight target tilt; their organized power and constrained-but-real exit keep them from the full-target end. Protected populations sit nearest the beneficiary pole: they pay only incidental, uncompensated costs (flagged in an omega). Insurers' arbitrage-grade exit pushes them firmly toward the beneficiary end — they monetize the regime's output without bearing its obligations. Regulators, as administrators, derive low-to-moderate d: they spend enforcement effort and collect mandate-relevant budgets. The structural signature of this reading is the ABSENCE of any full-target seat: no participant is systematically extracted-from, which is precisely what distinguishes it from the husk_reading file, where the same activities leave drilled populations holding empty performance while someone else collects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rare-event competence decay under quiescence and turnover — remains live: emergencies still occur, staff still turn over, equipment still ages, so no sunset logic applies and mandatrophy is not resolved. The rope claim performs the classification work in both directions: it prevents the regime's genuine coordination cost (real hours, real remediation spending) from being misread as pure extraction, and the omega battery keeps the husk alternative live so that ritualization drift — rising theater_ratio, decaying transfer validity — would be detected as data rather than assumed away by the flattering origin story. The slow upward creep in both tracked series is the drift channel worth watching: if theater_ratio crosses toward 0.5 while extractiveness accumulates, this file's reading loses ground to the husk_reading and the computed classification should migrate toward piton for the affected components.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_adjudication,
    'This constraint is one reading of the preparedness_persistence kernel. Is the drill-and-inspection regime live exercised knowledge (this file), memorial performance (husk_reading), or stratified across components (hybrid_reading) — and what evidence would adjudicate among the readings?',
    'Component-level transfer measurement: paired announced/unannounced drill performance deltas, predictive validity of inspection findings against subsequent actual failures, and cross-organization outcome comparisons controlling for baseline capability.',
    'Adjudication toward husk_reading migrates this file''s classification toward piton (high theater, inertial persistence, no functioning beneficiary); toward hybrid_reading, the corpus should split component-level stories with divergent epsilons; toward this reading, the rope classification stands and the sibling files register as minority framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_adjudication, empirical, 'Which reading of the preparedness_persistence kernel the evidence supports.').

omega_variable(
    drill_transfer_validity,
    'Does performance rehearsed under exercise conditions transfer to degraded real-event conditions (stress, noise, partial staffing, cascading failure), or does a transfer gap separate drill competence from field competence?',
    'After-action datasets comparing drilled-unit and undrilled-unit outcomes in matched events, plus high-fidelity simulation studies that degrade exercise conditions incrementally.',
    'A wide transfer gap means part of the regime''s cost purchases less readiness than assumed: effective extractiveness rises above the authored 0.18 because staff time is surrendered without proportional capability returned, eroding the rope claim from inside.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_transfer_validity, empirical, 'Whether rehearsed competence survives contact with real-event conditions.').

omega_variable(
    announcement_distortion,
    'What fraction of drill activity is distorted by advance notice — participants staging for the exercise rather than exercising readiness — and therefore measures rehearsal-of-the-drill rather than readiness?',
    'Paired announced/unannounced drill designs within the same organizations, measuring performance deltas attributable to notice alone.',
    'High distortion would push theater_ratio materially above the authored 0.15 and undercut the inspection_reports_real_condition axiom at the margin, shifting weight toward the husk_reading without requiring full ritualization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(announcement_distortion, empirical, 'How much announcement erodes the measurement value of drills and inspections.').

omega_variable(
    incidental_population_cost,
    'Are the costs borne by non-consenting populations — lockdown-drill anxiety in students, care diversion during hospital exercises — negligible coordination prices, or systematic uncompensated burdens concentrated on seats with no design voice?',
    'Population-level studies of drill-related psychological distress and hospital throughput data during exercise windows, compared against the frequency and intensity of current drill mandates.',
    'If the burdens are systematic, a victims declaration becomes warranted for this story, the directionality derivation gains a genuine target seat, and the classification drifts from rope toward tangled_rope with protected_populations as payers — dissolving this reading''s no-extraction signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_population_cost, empirical, 'Whether incidental third-party drill costs are noise or structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_competence_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(prep_competence_tr_t0, observed).
narrative_ontology:measurement(prep_competence_tr_t6, preparedness_persistence__competence_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement_basis(prep_competence_tr_t6, observed).
narrative_ontology:measurement(prep_competence_tr_t12, preparedness_persistence__competence_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement_basis(prep_competence_tr_t12, observed).
narrative_ontology:measurement(prep_competence_tr_t18, preparedness_persistence__competence_reading, theater_ratio, 18, 0.13).
narrative_ontology:measurement_basis(prep_competence_tr_t18, observed).
narrative_ontology:measurement(prep_competence_tr_t24, preparedness_persistence__competence_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(prep_competence_tr_t24, observed).
narrative_ontology:measurement(prep_competence_tr_t30, preparedness_persistence__competence_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(prep_competence_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(prep_competence_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(prep_competence_be_t0, observed).
narrative_ontology:measurement(prep_competence_be_t6, preparedness_persistence__competence_reading, base_extractiveness, 6, 0.15).
narrative_ontology:measurement_basis(prep_competence_be_t6, observed).
narrative_ontology:measurement(prep_competence_be_t12, preparedness_persistence__competence_reading, base_extractiveness, 12, 0.16).
narrative_ontology:measurement_basis(prep_competence_be_t12, observed).
narrative_ontology:measurement(prep_competence_be_t18, preparedness_persistence__competence_reading, base_extractiveness, 18, 0.16).
narrative_ontology:measurement_basis(prep_competence_be_t18, observed).
narrative_ontology:measurement(prep_competence_be_t24, preparedness_persistence__competence_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement_basis(prep_competence_be_t24, observed).
narrative_ontology:measurement(prep_competence_be_t30, preparedness_persistence__competence_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(prep_competence_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, information_standard).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drills and inspections maintain readiness' decomposes along two axes. First, by component: the physical-infrastructure layer (stations, apparatus, protective systems) is a separate natural-limit-plus-maintenance story and is deliberately EXCLUDED from this file per the epsilon-invariance principle — this story covers only the drill-and-inspection regime as a knowledge-maintenance arrangement. Second, by reading: the preparedness_persistence kernel is contested between competence_reading (this file: practice maintains operational readiness), husk_reading (form persists while competence atrophies), and hybrid_reading (stratified: some components competent, others ritualized). Each reading instantiates a different constraint over the SAME referent — the standing drill-and-inspection regime — with reading-indexed epsilon values over that fixed referent. This file authors the regime as the competence reading assesses it: low extraction, low theater, genuine transfer. The sibling files author the same regime by their own lights and carry correspondingly different metric profiles; the classification divergence across the family is the measurement the kernel contest exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
