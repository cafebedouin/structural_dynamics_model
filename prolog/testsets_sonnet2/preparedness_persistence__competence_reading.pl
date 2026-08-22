% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Drill and Inspection Regime as Live Exercised Readiness
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This story instantiates the competence_reading of the
 *   preparedness_persistence kernel: drills, fire alarms, evacuation
 *   exercises, and structural/equipment inspections are read as live
 *   exercised knowledge that actively maintains operational readiness against
 *   low-frequency, high-consequence events (fires, seismic events, industrial
 *   accidents). On this reading, the coordination function is real and
 *   current: the drill discovers gaps (blocked exits, malfunctioning alarms,
 *   unfamiliar procedures among new staff), the inspection catches
 *   deterioration before failure, and both feed a genuine correction loop.
 *   This is NOT the same claim as the husk_reading (same practices, but form
 *   has decoupled from function and persists as memorial performance) or the
 *   hybrid_reading (some components — engineering inspection — remain
 *   competent while others — evacuation drills — have ritualized). Those are
 *   different constraints with different ε profiles, authored separately and
 *   linked via network.affects_constraints. This story's ε is low because,
 *   under this reading's own lights, the standing arrangement it describes is
 *   one where the correction loop functions: deficiencies found are
 *   deficiencies fixed.
 *
 * KEY AGENTS:
 *   - building_occupants: primary beneficiaries (moderate/mobile) — protected by functioning evacuation knowledge and structural soundness
 *   - facility_operators: agenda_setters (institutional/constrained) — administer and fund the drill/inspection regime, bear compliance cost, benefit from reduced catastrophic liability
 *   - emergency_response_agencies: beneficiaries and co-administrators (institutional/analytical) — rely on facility-level readiness to reduce their own operational burden during actual events
 *   - regulatory_inspectors: agenda_setters (institutional/analytical) — certify compliance, the interpretive layer between codified standard and site-specific practice
 *   - insurers: indirect beneficiaries (organized/arbitrage) — price risk based on demonstrated readiness, benefit from lower claim frequency
 *   - frontline_staff: payers of the compliance burden and beneficiaries of the protection (moderate/constrained) — bear the time cost of drills, benefit from the competence those drills build
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.06).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.1).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Drill and Inspection Regime as Live Exercised Readiness").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_persistence__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2').
narrative_ontology:cs_kernel_codification('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', formalized).
narrative_ontology:cs_authority_grounding('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', expertise).
narrative_ontology:cs_interpretation_layer_present('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2').
narrative_ontology:cs_reading_relation('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', foundational, drilled_repetition_produces_verifiable_correction).
narrative_ontology:cs_axiom_status(drilled_repetition_produces_verifiable_correction, holdable).
narrative_ontology:cs_axiom_grounding('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', drilled_repetition_produces_verifiable_correction, empirically_contingent).
narrative_ontology:cs_axiom('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', secondary, compliance_cost_and_safety_benefit_accrue_to_same_population).
narrative_ontology:cs_axiom_status(compliance_cost_and_safety_benefit_accrue_to_same_population, holdable).
narrative_ontology:cs_axiom_grounding('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', compliance_cost_and_safety_benefit_accrue_to_same_population, empirically_contingent).
narrative_ontology:cs_reference_frame('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', live_correction_loop_baseline).
narrative_ontology:cs_drift_state('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('341b7fec-d8df-4ed2-8fb9-aef77c5fe7d2', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, building_occupants).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, facility_operators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, insurers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, frontline_staff).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, building_occupants).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, facility_operators).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, frontline_staff).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live or work in facilities subject to the drill/inspection regime. Bear the minor recurring cost of participating in drills (disrupted time, minor inconvenience) and are the primary intended beneficiaries of the readiness the drills build — if an actual event occurs, their survival odds and injury rates depend on whether the drilled knowledge is genuinely current. Exit from the regime is not really available; they can leave the building but not opt out of readiness requirements while present.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, building_occupants, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, building_occupants, payer).

% Administer and fund the drill and inspection schedule, hire or contract inspectors, schedule drills, and correct deficiencies found. Bear the direct compliance cost (staff time, inspection fees, remediation expense) but also capture the benefit of reduced catastrophic liability and continued licensing/insurability. On this reading their enforcement of the regime is functional administration of a real coordination good, not extraction — the cost they impose on staff and occupants (drill time) maps to the same population's safety gain.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, facility_operators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, facility_operators, payer).

% Certify compliance with codified fire, structural, and safety standards; sit as the interpretive layer that translates the formalized code into site-specific judgments about what passes. Their authority is grounded in expertise and continuity of standard; on this reading their judgments track real deterioration and real correction, not ritual sign-off.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, regulatory_inspectors, agenda_setter,
    institutional, generational, analytical, national).

% Fire departments, EMS, and disaster-response bodies rely on facility-level preparedness to reduce their own operational burden during real incidents — a building whose occupants know evacuation routes and whose structure has been inspected is a lower-risk, more tractable incident for first responders. They benefit from the regime without administering it directly.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_response_agencies, beneficiary,
    institutional, biographical, analytical, regional).

% Price risk and set premiums partly based on documented compliance with drill and inspection regimes. Benefit from the regime's function without bearing its administration cost; can adjust pricing or underwriting terms as evidence, giving them the most exit flexibility of any seat in this story.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, insurers, beneficiary,
    organized, generational, arbitrage, national).

% Employees required to participate in and often help execute drills (fire wardens, floor marshals). Bear the direct time and attention cost of repeated exercises, and are also the population whose competence is what the drill is supposed to build — if the drill is live-exercised knowledge rather than theater, it is disproportionately their competence being maintained.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, frontline_staff, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, frontline_staff, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that low-frequency, high-consequence failure modes (fire, structural failure, industrial accident) cannot be learned from real experience often enough to keep response competence current, and cannot be verified as absent without active checking. Regular drilling and inspection substitute for the missing natural feedback loop, artificially generating the practice repetitions and deficiency-detection events that a rare-event domain would not otherwise produce.
% TRANSFER_FUNCTION: Moves time, attention, and money from occupants, staff, and facility operators into the maintenance of readiness capacity; in return it moves reduced catastrophic risk back to the same population, plus lowered liability exposure to operators and lowered incident severity to responders. Under this reading the flow is circular rather than extractive: the payers of the compliance cost are substantially the same population that receives the safety benefit.
% ABSENT_VOICES: Workers and occupants in facilities where drills are chronically under-resourced or under-scheduled are not directly represented in this story's stakeholder set — their situation would more plausibly be described by the husk_reading or hybrid_reading sibling. This story does not claim their absence is irrelevant; it claims they are describing a different constraint.
% DISAPPEARANCE_RATIONALE: If drilling and inspection vanished overnight, the correction loop that currently catches blocked exits, deteriorating equipment, and unfamiliar staff procedures would disappear with it. Facilities would continue operating with no observable change until an actual event occurred, at which point outcomes would diverge sharply from the drilled baseline — response times would lengthen, structural failures would go undetected longer, and insurers would reprice risk upward once the absence of verification became evident. The world does not look different tomorrow, but the readiness it depends on for the rare event is gone.
% FOUNDING_PROBLEM: Historical mass-casualty fires and structural collapses (early 20th-century factory and theater fires, later building collapses) revealed that neither occupants nor operators could be trusted to maintain evacuation knowledge or structural soundness without externally imposed, repeated verification — memory and vigilance decay between rare events faster than the risk does.
% FOUNDING_PROBLEM_CORROBORATION: Independent fire-safety engineering research and post-incident investigations (conducted by bodies with no operational stake in any single facility's compliance record) continue to document that facilities lapsing on drill/inspection cadence show measurably worse outcomes in actual incidents, corroborating that the founding problem remains active rather than historical. This corroboration comes from outside the facility-operator beneficiary set, which is precisely the check the husk_reading would fail to produce for its own claim.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.06, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.06) because, under the competence_reading, there is no identifiable party extracting rent through the drill/inspection structure — the costs borne (time, disruption, inspection fees) map back into the same population's safety benefit, which is the Rope signature. Suppression is low (0.10): compliance is enforced (fire codes, OSHA-style regimes) but the enforcement exists to guarantee the coordination good is actually produced, not to suppress an alternative that participants would otherwise prefer. Theater ratio is low and stable (0.10-0.12) reflecting genuine, non-degrading exercise of the practice across the interval — this is the key metric distinguishing this reading from the husk_reading, where theater_ratio would be authored rising toward or past 0.5. Accessibility collapse is authored moderately high (0.72): once you understand that structural inspection and drilled evacuation knowledge are what stands between routine operation and catastrophic failure, there genuinely isn't a viable alternative path to the same safety outcome — this is the Mountain component (physical/informational readiness has real floor requirements) blended with the Rope component (the coordination of who drills when, who inspects what, is a solved coordination problem, not an imposed extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Facility operators and regulatory inspectors are agenda_setters with institutional power and largely analytical-to-constrained exit — they administer the standard because doing so is required, but the requirement's coordination function is real from their seat too (avoiding catastrophic liability, avoiding the response burden of an unprepared event). Building occupants and frontline staff are near-symmetric: they bear the time/disruption cost of the drill and receive the safety benefit through the same channel, which is the textbook Rope directionality profile (d near 0.5, not pushed toward either pole by asymmetric extraction). No agent is authored as a target of extraction under this reading — that is the structural claim the reading makes, and it is why victims[] is empty.
 *
 * MANDATROPHY ANALYSIS:
 *   The competence_reading's central claim is precisely that mandatrophy has NOT occurred here: the founding problem (undiscovered structural or behavioral failure surfacing catastrophically during a real event) remains live, and the mechanism built to address it (repeated, corrective, live-exercised drilling and inspection) is still discharging that function. Classifying this as Rope (not Mountain alone, not Snare) prevents two mislabeling errors: treating it as pure natural law (which would erase the real coordination cost and effort required to keep it live) and treating it as pure extraction (which would be the husk_reading's error if forced onto this reading's facts). The explicit sibling readings exist so that when the correction loop does decay in a given case, the story can migrate to husk_reading or hybrid_reading rather than this reading being stretched past what its own metrics support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_boundary,
    'At what point does a drill program''s demonstrated correction rate (near-misses caught, procedures revised after failure) fall low enough that the ''live exercised knowledge'' reading stops being empirically defensible for a given facility or jurisdiction?',
    'Audit trail comparing drill-identified deficiencies to subsequent corrective actions over multiple cycles; a program with a persistently near-zero correction rate despite recurring identical failures is evidence for the sibling husk_reading, not this one.',
    'If correction rates are near-zero across a domain this reading claims to describe, the competence_reading does not apply there and the story should be re-scoped to the husk_reading or hybrid_reading instead of stretched to cover it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_boundary, empirical, 'Whether demonstrated correction from drills is the operative mechanism, distinguishing this reading from the husk reading.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''preparedness'' better modeled as a single kernel with contested readings (this framework''s choice), or as two distinct structural claims (physical-infrastructure inspection vs. behavioral drill exercises) that happen to share a label but have different failure modes and different ε profiles?',
    'Compare inspection-only failure data (structural, engineering-driven) against drill-only failure data (behavioral, participation-driven) across several sectors; if the two show persistently divergent extraction/theater profiles regardless of institutional context, they are better decomposed into separate constraints rather than treated as one kernel under contest.',
    'If decomposition is warranted, this story''s claim of ''Mountain + Rope, no extraction'' may only hold for the inspection component, while the drill component would need its own story with potentially different metrics — the hybrid_reading sibling already gestures at this, but as a reading of one kernel rather than a full decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel framing itself, versus a full ε-invariance decomposition, is the right level of analysis for preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__competence_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__competence_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__competence_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__competence_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(prep_tr_t25, preparedness_persistence__competence_reading, theater_ratio, 25, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__competence_reading, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__competence_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__competence_reading, base_extractiveness, 15, 0.06).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__competence_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(prep_be_t25, preparedness_persistence__competence_reading, base_extractiveness, 25, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the preparedness_persistence kernel (competence_reading, husk_reading, hybrid_reading), each authored as a separate story per the epsilon-invariance principle. All three share the same underlying practices (drills, inspections) but assign different epsilon and claimed_type values because they make different structural claims about whether the correction loop is currently functioning. competence_reading (this story) claims the loop is live and authors epsilon = 0.06, rope. husk_reading claims the loop has decayed into memorial performance and would author substantially higher theater_ratio and likely a piton classification. hybrid_reading claims a stratified mixture and would author intermediate, component-differentiated metrics. The three are linked bidirectionally via affects_constraints because evidence bearing on one (e.g., a documented correction-loop failure) is directly relevant to adjudicating between the readings for a given real-world facility or jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
