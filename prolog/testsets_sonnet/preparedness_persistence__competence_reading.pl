% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Preparedness Drills and Inspections as Live Exercised Knowledge
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This story instantiates the competence reading of the
 *   preparedness_persistence kernel: drills and inspections are live
 *   exercised knowledge whose repeated performance maintains genuine
 *   operational readiness against physical and organizational failure modes.
 *   In this reading, evacuation drills keep muscle memory current, structural
 *   inspections verify real engineering tolerances, and the coordination
 *   overhead (scheduling, participation, documentation) is low relative to
 *   the safety benefit produced. The claim is Rope with a Mountain-adjacent
 *   floor: the physical failure modes drills and inspections guard against
 *   (fire spread rates, structural fatigue, egress bottlenecks) are not
 *   negotiable by social consensus, even though the specific drill/inspection
 *   protocol is a constructed coordination mechanism layered on top of that
 *   physical reality. Two sibling constraints exist in the same kernel
 *   contest and are NOT this story: husk_reading (drills as memorial
 *   performance with atrophied competence) and hybrid_reading (stratified
 *   competence — engineering inspection remains sound while evacuation drills
 *   degrade to ritual). Those siblings have their own ε values and their own
 *   stakeholder structures; this story does not average with them or hedge
 *   toward them.
 *
 * KEY AGENTS:
 *   - building_occupants: Primary beneficiary (moderate/constrained) — participate in drills, receive the safety benefit of tested egress routes
 *   - facility_operators: Primary agenda_setter (organized/constrained) — schedule and run drills and inspections, bear the direct compliance cost, receive liability protection and continuity of operations
 *   - emergency_response_agencies: Beneficiary and coordination partner (institutional/mobile) — rely on drilled facility behavior to plan external response, gain predictability
 *   - municipal_governments: Beneficiary (institutional/analytical) — collect the public-safety benefit of a prepared building stock without running individual drills
 *   - inspection_engineers: Secondary agenda_setter (moderate/mobile) — perform the technical verification against physical standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.08).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Preparedness Drills and Inspections as Live Exercised Knowledge").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "disaster_preparedness/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '8d79adf6-fd29-4afa-8c5f-ecd73d0c2401').
narrative_ontology:cs_kernel_codification('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', formalized).
narrative_ontology:cs_authority_grounding('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', expertise).
narrative_ontology:cs_interpretation_layer_present('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401').
narrative_ontology:cs_reading_relation('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', foundational, repeated_practice_preserves_operational_capability).
narrative_ontology:cs_axiom_status(repeated_practice_preserves_operational_capability, holdable).
narrative_ontology:cs_axiom_grounding('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', repeated_practice_preserves_operational_capability, empirically_contingent).
narrative_ontology:cs_axiom('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', foundational, inspection_criteria_track_real_physical_failure_modes).
narrative_ontology:cs_axiom_status(inspection_criteria_track_real_physical_failure_modes, holdable).
narrative_ontology:cs_axiom_grounding('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', inspection_criteria_track_real_physical_failure_modes, empirically_contingent).
narrative_ontology:cs_reference_frame('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', engineering_verified_readiness_standard).
narrative_ontology:cs_drift_state('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', contemporary_compliance_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8d79adf6-fd29-4afa-8c5f-ecd73d0c2401', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, building_occupants).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, facility_operators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, municipal_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, building_occupants).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, practice_maintains_operational_readiness).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, physical_infrastructure_degrades_without_verification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in scheduled fire drills and evacuation exercises, losing a small amount of work time per drill. In return they get an egress route that has actually been walked and timed under simulated conditions, and a building whose structural systems have been checked against physical failure thresholds. They cannot opt out of occupancy-conditioned drills without leaving the building, but the drills themselves impose minimal cost relative to the benefit of a verified evacuation plan.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, building_occupants, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, building_occupants, payer).

% Schedule and run the drills, hire inspectors, and maintain compliance documentation. They bear the direct scheduling and administrative cost. In the competence reading, this cost is offset by genuine liability protection, insurance rate benefits, and lower probability of catastrophic incident cost — the arrangement pays for itself through avoided tail risk, not through extraction from occupants.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, facility_operators, agenda_setter,
    organized, biographical, constrained, local).

% Perform the technical structural and systems inspections against engineering codes derived from materials science and failure physics. Their assessments are portable across facilities and jurisdictions (mobile exit options), and their professional standing depends on the inspection actually tracking real structural risk rather than becoming a rubber stamp — this is the professional-identity mechanism that keeps the competence reading's Mountain-adjacent floor intact.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, inspection_engineers, agenda_setter,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, inspection_engineers, observer).

% Plan external response (fire, medical, structural collapse) partly on the assumption that facilities have drilled occupants and verified structures. A facility with genuinely maintained competence reduces the agency's own operational risk and resource burden during a real incident. They do not run the drills themselves but depend on their integrity.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_response_agencies, beneficiary,
    institutional, generational, mobile, regional).

% Set the baseline regulatory requirement for drills and inspections and collect the aggregate public-safety benefit of a prepared building stock, without directly running any individual facility's program. Their interest in the competence reading being true is high, which is why independent incident-outcome data rather than self-report is needed to corroborate the founding problem's continued live status.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, municipal_governments, beneficiary,
    institutional, generational, analytical, national).

% Not-yet-realized occupants and responders who will experience the next actual emergency and will bear the consequences if the competence reading turns out to be false for their specific building. They have no voice in current drill design or inspection scheduling and cannot verify, before the fact, whether the practice they will depend on is genuinely competence-preserving or has quietly degraded.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, future_incident_populations, excluded,
    powerless, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that untested evacuation plans and unverified structural systems fail unpredictably during real emergencies, by periodically exercising the plan and re-checking the structure against fixed physical and procedural standards.
% TRANSFER_FUNCTION: Moves scheduling time and administrative cost from facility operators and inspection engineers to the production of verified readiness, which is then consumed as reduced tail risk by occupants, response agencies, and municipal government collectively. No party is a net payer without a corresponding safety benefit in this reading.
% ABSENT_VOICES: Future incident populations — the specific occupants and responders who will face the next real emergency at a given facility — have no seat in current drill design and cannot audit, in advance, whether the practiced competence will actually hold when needed. Their absence is structural to any preparedness regime, not specific to this reading, but it means the competence claim is always somewhat unfalsifiable until an actual incident occurs.
% DISAPPEARANCE_RATIONALE: If drills and inspections vanished overnight, facility operators would lose liability protection and insurance benefits, occupants would face untested egress routes, response agencies would lose the planning assumption they currently rely on, and municipal governments would need some other mechanism to verify building-stock safety. The arrangement is load-bearing for multiple parties' current plans, which is why the competence reading claims Rope (removal would harm real coordination), not pure Mountain (removal of a true natural law would be impossible, not merely costly).
% FOUNDING_PROBLEM: Buildings and organizations historically failed to perform as expected during real fires, structural failures, and evacuations because plans and structures were never verified under realistic conditions before the emergency arrived — the gap between paper plan and actual performance was discovered only during the incident itself, at maximum cost.
% FOUNDING_PROBLEM_CORROBORATION: Fire investigation reports and post-incident structural failure analyses from independent engineering review boards (outside both facility operators and municipal regulators, who both have an interest in claiming current practice is adequate) continue to document incidents where untested or under-inspected systems failed in ways drilled/inspected systems did not — this is external corroboration that the founding problem remains live, distinct from operator or regulator self-report.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.08, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.08) and essentially flat across the interval because in the competence reading there is no asymmetric transfer riding on the drill/inspection apparatus — the parties who bear the cost of running drills (facility operators) are the same parties who capture the safety and liability benefit. Suppression is low (0.12): participation in drills is not coerced by threat of exit-collapse, it is a condition of occupancy that most occupants would choose given the alternative (untested egress). Theater ratio is low but nonzero and slowly rising (0.10 to 0.15) — some performative compliance creep is honest to author even in the competence reading (paperwork accumulates even where practice stays real), but it stays well below the 0.5 threshold that would indicate Goodhart substitution. Accessibility collapse is authored moderately high (0.7): once a facility adopts a drilled evacuation plan and passes inspection, ad hoc alternatives (untested exits, undocumented structural assumptions) genuinely become unavailable as live options — this is the Mountain-adjacent floor, not suppression.
 *
 * PERSPECTIVAL GAP:
 *   Facility operators (agenda_setter) and building occupants (beneficiary/payer) are expected to compute similarly under this reading precisely because the reading's claim is that competence is genuinely maintained — there is no seat that experiences hidden extraction. The engine may compute a wider gap if the structural data (moderate power, constrained exit for occupants) generates more directionality drift than the narrative claim anticipates; that divergence, if it appears, is data about how well the competence reading's low-suppression claim holds structurally, not an error to correct.
 *
 * DIRECTIONALITY LOGIC:
 *   No victims are declared in this reading because the competence reading's defining claim is that the coordination cost and the safety benefit land on the same parties. Facility operators pay the direct cost of running drills and inspections and receive liability protection and continuity in return; occupants bear minor time cost (participating in a drill) and receive the tested egress route in return. This is what distinguishes the competence reading structurally from any tangled_rope reading of the same kernel: if a systematic net-payer without offsetting benefit existed, that would be a different, more extractive constraint (closer to the husk reading's degraded-function analysis) and would require its own story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (untested infrastructure and untrained personnel failing during actual emergencies) remains live under this reading by construction — that is what distinguishes it from the husk reading, where the founding problem persists but the arrangement no longer solves it. Corroboration for the founding-problem-status claim should come from incident outcome data (do drilled facilities perform measurably better during real emergencies), not from operator self-report, since operators have an interest in claiming their drills remain effective regardless of actual competence retention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the drill/inspection regime, as generally practiced, the competence reading (this constraint), the husk reading (memorial performance), or the hybrid reading (stratified competence)?',
    'Comparative audit: cross-check drill outcomes against actual incident response performance across a sample of facilities; components where drilled behavior predicts incident outcomes support the competence reading, components where it does not support the husk or hybrid readings.',
    'If the empirical record for a given jurisdiction or facility class shows drills failing to predict incident performance, that population''s constraint should be authored as husk_reading or hybrid_reading instead — this story''s ε (0.08) would not transfer to that population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, empirical, 'Whether observed drill/inspection practice instantiates the competence, husk, or hybrid reading of the preparedness_persistence kernel.').

omega_variable(
    verification_infrastructure_naturalness,
    'Is the coordination function of inspection regimes (verifying that physical infrastructure meets a fixed engineering standard) closer to a Mountain — an irreducible check against physical failure modes that would exist under any social arrangement — or is it a constructed institutional practice that happens to track physical reality well in this reading?',
    'Examine whether inspection criteria derive directly from materials science and structural failure physics (Mountain-like) versus from negotiated regulatory compromise that could vary without changing physical safety (constructed-Rope-like).',
    'If inspection criteria are substantially physics-derived, the Mountain component of this constraint is stronger than authored; if substantially negotiated, the Rope component dominates and the mountain framing in commentary should be read as approximate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_infrastructure_naturalness, conceptual, 'Whether the physical-infrastructure component of preparedness is genuinely mountain-like or a well-fitted construction.').

omega_variable(
    beneficiary_vs_no_extraction,
    'Does the presence of declared beneficiaries (occupants, agencies, operators, governments) indicate hidden extraction, or is this a case of genuine positive-sum coordination where beneficiaries exist without any corresponding victim?',
    'Check whether any party bears a cost from the drill/inspection regime that is not offset by a corresponding safety benefit they also receive — e.g., compare compliance cost burden on facility operators against their own risk reduction.',
    'If a systematic net-payer group without offsetting benefit is identified, this story should be re-authored as tangled_rope with a victims array; absent such a group, the rope classification with no victims stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_vs_no_extraction, empirical, 'Whether beneficiary declarations here mask an unauthored victim class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__competence_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__competence_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__competence_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__competence_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__competence_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__competence_reading, base_extractiveness, 8, 0.06).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__competence_reading, base_extractiveness, 16, 0.07).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__competence_reading, base_extractiveness, 24, 0.07).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__competence_reading, base_extractiveness, 32, 0.08).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__competence_reading, base_extractiveness, 40, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__competence_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the preparedness_persistence kernel. competence_reading (this file) claims low extraction, genuine Rope/Mountain-hybrid coordination, no victims. husk_reading claims the same drill/inspection form persists while underlying competence has atrophied — expect substantially higher theater_ratio and a piton or tangled_rope classification. hybrid_reading claims stratification: engineering inspection remains competence_reading-like while evacuation drills degrade toward husk_reading-like. All three share the same underlying institutional form (scheduled drills, periodic inspections) but diverge on whether the practice still verifies the competence it claims to verify — this is the structural disagreement the kernel contest is designed to surface, and it is why each reading gets its own ε rather than one averaged value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
