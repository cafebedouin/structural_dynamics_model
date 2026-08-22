% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission (Hybrid Reading — Infrastructure Intact, Coordination Decayed)
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This story reads the preparedness-transmission kernel as a bifurcated
 *   system: physical infrastructure competence (engineering design,
 *   inspection, code compliance) has remained genuinely high and continuously
 *   re-validated, while civilian coordination knowledge (evacuation drilling,
 *   incident command practice, public readiness) has quietly decayed
 *   underneath a certification regime that still reports full readiness.
 *   Unlike the husk_reading (which claims the entire preparedness apparatus,
 *   drills included, has hollowed into memorial ritual) and the
 *   competence_reading (which claims drills and inspections both remain live
 *   exercised knowledge), the hybrid_reading asserts a structural SPLIT: the
 *   D5 break — the point where designed function diverges from actual
 *   function — exists specifically in the coordination layer, not the
 *   physical layer. Infrastructure performs exactly as engineered when
 *   tested; coordination protocols look intact on paper but fail when
 *   actually exercised under stress. This is why the constraint computes as
 *   tangled_rope rather than mountain, husk-piton, or clean rope: there IS a
 *   genuine, currently-functioning coordination function (infrastructure
 *   investment coordination), but it now also serves as cover — visible
 *   structural success is used to certify a coordination readiness that no
 *   longer exists, transferring risk onto residents and frontline personnel.
 *
 * KEY AGENTS:
 *   - engineering_agencies: institutional beneficiary — infrastructure competence genuinely maintained, collects budget and credit
 *   - infrastructure_contractors: organized beneficiary — captures disproportionate investment because their output is measurable
 *   - emergency_management_leadership: institutional agenda_setter — administers the whole system, could rebalance investment, bears political cost of admitting the gap
 *   - civilian_residents_in_hazard_zones: powerless payer — trapped in hazard zones, discovers coordination gap only during an actual event
 *   - frontline_evacuation_volunteers: moderate payer/beneficiary — trained on outdated protocols, absorbs coordination failure directly
 *   - municipal_first_responders: moderate payer — executes evacuation through architecture that is intact on paper, untested in practice
 *   - independent_disaster_researchers: analytical observer — documents the stratification pattern from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.48).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission (Hybrid Reading — Infrastructure Intact, Coordination Decayed)").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, '9aaedf3b-7f95-4ff1-b055-16858d396ace').
narrative_ontology:cs_kernel_codification('9aaedf3b-7f95-4ff1-b055-16858d396ace', formalized).
narrative_ontology:cs_authority_grounding('9aaedf3b-7f95-4ff1-b055-16858d396ace', practice).
narrative_ontology:cs_interpretation_layer_present('9aaedf3b-7f95-4ff1-b055-16858d396ace').
narrative_ontology:cs_reading_relation('9aaedf3b-7f95-4ff1-b055-16858d396ace', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('9aaedf3b-7f95-4ff1-b055-16858d396ace', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_axiom('9aaedf3b-7f95-4ff1-b055-16858d396ace', foundational, preparedness_competence_is_layer_separable).
narrative_ontology:cs_axiom_status(preparedness_competence_is_layer_separable, holdable).
narrative_ontology:cs_axiom_grounding('9aaedf3b-7f95-4ff1-b055-16858d396ace', preparedness_competence_is_layer_separable, empirically_contingent).
narrative_ontology:cs_axiom('9aaedf3b-7f95-4ff1-b055-16858d396ace', secondary, coordination_knowledge_requires_distinct_transmission_mechanism_from_engineering_knowledge).
narrative_ontology:cs_axiom_status(coordination_knowledge_requires_distinct_transmission_mechanism_from_engineering_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('9aaedf3b-7f95-4ff1-b055-16858d396ace', coordination_knowledge_requires_distinct_transmission_mechanism_from_engineering_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('9aaedf3b-7f95-4ff1-b055-16858d396ace', unified_civil_defense_mandate).
narrative_ontology:cs_drift_state('9aaedf3b-7f95-4ff1-b055-16858d396ace', contemporary_post_event_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9aaedf3b-7f95-4ff1-b055-16858d396ace', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_contractors).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, emergency_management_leadership).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_residents_in_hazard_zones).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, frontline_evacuation_volunteers).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, municipal_first_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, frontline_evacuation_volunteers).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, physical_hardening_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, inspect, and certify seawalls, levees, structural retrofits, and warning hardware. Their competence is continuously re-validated through licensing exams, code updates, and physical inspection regimes that have real stakes (structural failure is visible and attributable). They receive budget, prestige, and political credit for the arrangement's apparent success, and are not the ones exposed when evacuation coordination fails.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, engineering_agencies, beneficiary,
    institutional, generational, arbitrage, regional).

% Win recurring contracts to build and maintain hardened infrastructure. Their work is measurable, fundable, and politically uncontroversial compared to coordination training, so budget flows disproportionately to their domain even when the marginal risk reduction from coordination investment would be higher.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_contractors, beneficiary,
    organized, biographical, mobile, regional).

% Sets drill schedules, certifies evacuation plans, and reports readiness upward to political leadership. Under budget and attention pressure, leadership has let civilian coordination drills degrade into infrequent, low-fidelity tabletop exercises while continuing to invest in physical infrastructure it can point to as evidence of preparedness. They administer the whole system and could redirect resources toward coordination retraining, but the political and career cost of admitting the coordination gap is high, while the cost of quietly under-investing is currently borne elsewhere.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_management_leadership, agenda_setter,
    institutional, biographical, constrained, regional).

% Live in flood, seismic, or wildfire zones behind engineered defenses they cannot personally evaluate. When an event exceeds design margins or requires actual evacuation, they discover in real time that they do not know the assembly point, the route, or who is coordinating what — because the last coordination drill they experienced was years ago and communicated mostly through signage, not practiced movement. They cannot exit the hazard zone easily and cannot substitute for the missing coordination knowledge themselves.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_residents_in_hazard_zones, payer,
    powerless, immediate, trapped, local).

% Community members and low-level responders nominally trained to marshal evacuations. They receive some benefit from being credentialed and included in the system, but discover under real stress that the coordination protocols they were taught assume institutional support (working radios, staffed checkpoints, rehearsed roles) that has not been kept current. They absorb the coordination failure directly, often improvising against outdated plans while being held informally responsible for outcomes.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, frontline_evacuation_volunteers, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, frontline_evacuation_volunteers, beneficiary).

% Fire, police, and EMS personnel who must execute evacuation orders through a coordination architecture that looks intact on paper (org charts, mutual-aid agreements, communication protocols) but has not been stress-tested against realistic civilian behavior in years. They pay in the form of chaotic incident command, contradictory instructions, and personal risk when the coordination layer fails under load that the physical infrastructure survived.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, municipal_first_responders, payer,
    moderate, immediate, constrained, local).

% After-action researchers who study post-event reports and find a consistent pattern: structures hold, warnings arrive on time, but evacuation and shelter coordination breaks down in ways that infrastructure metrics do not capture. They have no formal power to redirect resources but their findings are the primary external evidence for the stratification claim.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, independent_disaster_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, engineering_agencies).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement genuinely coordinates two things that could otherwise be uncoordinated: capital investment in physical hazard mitigation (seawalls, retrofits, warning systems) and periodic civilian readiness activity (drills, evacuation planning). The infrastructure half of this coordination function is real and actively exercised.
% TRANSFER_FUNCTION: Attention, budget, and political credit flow toward physical infrastructure (visible, measurable, contractible) and away from civilian coordination training (diffuse, hard to measure, politically unrewarding to fund). The cost of this misallocation is transferred to residents, volunteers, and responders who bear the consequences of coordination failure during an actual event, while engineering agencies and contractors continue to collect resources and credit regardless of coordination outcomes.
% ABSENT_VOICES: Civilian residents and volunteer evacuation marshals are notionally represented by emergency management leadership but have no direct voice in budget allocation between infrastructure and coordination training. Independent researchers document the gap in after-action reports, but those reports rarely reach budget-setting forums before the next cycle repeats the same allocation.
% DISAPPEARANCE_RATIONALE: If the current arrangement (continued infrastructure investment plus nominal, under-resourced coordination drilling) disappeared overnight and were not replaced by anything, residents would lose even the partial warning and structural protection they currently have — infrastructure genuinely reduces exposure. But if only the coordination-side pretense disappeared (i.e., leadership stopped certifying coordination readiness it does not actually possess), budget and political pressure would have to rearrange toward funding real coordination capacity, because the gap would become visible and politically live rather than absorbed into infrastructure's reflected credibility.
% FOUNDING_PROBLEM: The founding problem was dual: prevent structural failure during hazard events (engineering) and ensure people could be moved out of harm's way in time (coordination). Both halves were originally built and funded together as one civil defense mandate.
% FOUNDING_PROBLEM_CORROBORATION: Engineering agencies and leadership attest the founding problem remains fully live and addressed, pointing to infrastructure performance records. Independent disaster researchers and post-event review boards — outside the beneficiary set — attest that the engineering half of the founding problem is being actively solved while the coordination half has been allowed to lapse into unexercised, outdated protocol, based on after-action reports showing evacuation confusion despite structural survival.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).
:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than high, because the infrastructure half of the arrangement is a real, non-extractive coordination success — the extraction is concentrated specifically in the mismatch between claimed and actual coordination readiness, not in the whole system. Theater ratio rises substantially (0.20 to 0.55) because as coordination drilling has been allowed to decay, the proportion of 'readiness' activity that is genuinely exercised competence versus performative certification (tabletop exercises reported as full-scale readiness, drills scheduled but under-resourced) has grown. Suppression is moderate (0.48): there is no active coercion preventing residents from learning coordination gaps exist, but there is a structural information asymmetry — readiness reports aggregate infrastructure and coordination metrics together, obscuring which half is failing. Accessibility collapse is moderate (0.50): residents and volunteers cannot easily verify coordination readiness independently of leadership's own certification, but the infrastructure half remains independently verifiable (visible structures, published inspection records), which caps how completely alternatives have collapsed. Resistance is moderate (0.40), driven mainly by independent researchers and post-event review boards rather than by the directly affected residents, who typically lack the standing or information to resist before an event occurs.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineering agencies and infrastructure contractors sit near the beneficiary end: they collect budget, credit, and repeat contracts regardless of whether coordination readiness holds, because their function is measured independently and favorably. Emergency management leadership is the agenda_setter with the clearest structural view and the clearest capacity to rebalance — but faces asymmetric incentives (infrastructure failure is catastrophic and attributable; coordination failure is diffuse and deniable until an actual event), which is why the arrangement persists despite the setter's formal capacity to fix it. Civilian residents, volunteers, and first responders sit near the target end: they bear the coordination failure's consequences directly and cannot exit the hazard zone or substitute their own coordination knowledge for what the institution has let lapse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate was dual (structural survival AND safe evacuation), and one half of the mandate remains genuinely live and well-served while the other has quietly gone obsolete in function while remaining formally certified. Classifying this as tangled_rope rather than a clean mountain (infrastructure alone) or a pure snare (total extraction) prevents two mislabeling errors: treating the whole preparedness system as failed (which would ignore the real, continuing engineering coordination success) and treating it as fully functional (which would ignore that the coordination half has hollowed out under the cover of the infrastructure half's continued success). The tangled_rope classification specifically captures that a real coordination function persists at the same time as an asymmetric extraction — the cost of unaddressed coordination decay is displaced onto exactly the people the system claims to protect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_decay_measurement_validity,
    'Is the claimed coordination decay actually measurable and distinguishable from infrastructure competence, or is ''coordination knowledge'' too diffuse a category to verify independently of self-report by emergency management leadership?',
    'Compare full-scale, unannounced evacuation exercises (not tabletop drills) against self-reported readiness certifications over a multi-year window; a persistent gap between exercised and certified readiness would corroborate the hybrid reading''s core claim.',
    'If coordination decay cannot be independently verified, the hybrid_reading''s central distinguishing claim collapses and the constraint should be re-evaluated against the competence_reading (no real break) rather than treated as an established tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_decay_measurement_validity, empirical, 'Whether the coordination-layer break is independently verifiable or only inferable from leadership''s own reporting.').

omega_variable(
    stratification_versus_uniform_decay,
    'Is preparedness transmission genuinely stratified (infrastructure high, coordination low), or does infrastructure competence ALSO quietly decay in ways that simply have not yet been stress-tested by a sufficiently large event?',
    'Track infrastructure performance against design specifications during genuinely extreme, low-frequency events (beyond code-design return periods); infrastructure surprises under extreme stress would suggest the stratification claim understates decay in the physical layer too.',
    'If infrastructure competence proves to be more fragile than assumed under extreme conditions, this reading''s core distinguishing premise (a clean split between the two layers) weakens, and the constraint moves structurally closer to the husk_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stratification_versus_uniform_decay, empirical, 'Whether the infrastructure/coordination split is a stable structural fact or an artifact of insufficiently extreme testing to date.').

omega_variable(
    budget_reallocation_feasibility,
    'Could emergency management leadership actually rebalance investment toward coordination training at reasonable cost, or does the political economy of visible infrastructure spending make such reallocation practically infeasible regardless of formal authority?',
    'Case comparison across jurisdictions that have attempted to shift budget share toward coordination training versus those that have not, tracking political and budgetary obstacles encountered.',
    'If reallocation is genuinely infeasible given political incentives, the fixing_cost for this arrangement is closer to prohibitive despite leadership''s nominal administrative capacity, strengthening the tangled_rope reading over a simple negligence framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(budget_reallocation_feasibility, empirical, 'Whether the agenda_setter''s formal capacity to fix the coordination gap is practically exercisable given political economy constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__hybrid_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__hybrid_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__hybrid_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(prep_tr_t32, preparedness_transmission__hybrid_reading, theater_ratio, 32, 0.51).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__hybrid_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__hybrid_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__hybrid_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__hybrid_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t8, preparedness_transmission__hybrid_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(prep_su_t16, preparedness_transmission__hybrid_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(prep_su_t24, preparedness_transmission__hybrid_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(prep_su_t32, preparedness_transmission__hybrid_reading, suppression_requirement, 32, 0.45).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'preparedness transmission' kernel per the ε-invariance principle: competence_reading (mountain-leaning, no D5 break), husk_reading (piton-leaning, D5 break throughout), and this hybrid_reading (tangled_rope, D5 break confined to the coordination layer). Each carries its own ε, its own beneficiary/victim structure, and its own claimed_type; they are linked here rather than merged because measuring 'preparedness' by the infrastructure observable versus the coordination observable yields substantially different extraction values — exactly the signal that indicates decomposition rather than a single parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
