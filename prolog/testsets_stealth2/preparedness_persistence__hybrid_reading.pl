% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster-Preparedness Maintenance Regime (Hybrid Reading)
 *   domain: institutional/civic-safety
 *
 * SUMMARY:
 *   Disaster-preparedness regimes require organizations in hazard-prone
 *   regions to maintain two kinds of readiness activity on statutory
 *   schedules: technical inspection of physical infrastructure (dams, levees,
 *   bridges, critical facilities) and operational rehearsal (evacuation
 *   drills, tabletop exercises, multi-agency simulations). Over the
 *   observation interval these two components have diverged sharply.
 *   Engineering inspection has retained its disciplinary teeth — findings
 *   carry professional liability, defects are traceable to named signatories,
 *   and repairs follow. Scheduled drills have drifted toward documentation
 *   performance: scenarios are scripted to conclude successfully,
 *   participation is recorded, certificates are filed, and the exercise
 *   calendar doubles as the agency's public proof of function. This story
 *   instantiates the HYBRID READING of the preparedness-persistence kernel:
 *   the regime is neither uniformly alive nor uniformly hollow — it is
 *   stratified, with genuine function and ritual occupying different
 *   subsystems of the same enforcement structure. Per the one-reading rule,
 *   this file authors only the hybrid reading as a single epsilon-invariant
 *   constraint; the husk and competence readings are separate constraints
 *   with their own epsilon values, and the disagreement between readings is
 *   routed to omega variables rather than averaged here. The epsilon referent
 *   is the standing stratified regime as it actually operates — not the
 *   reformed, performance-based alternative this reading would endorse. KEY
 *   AGENTS (by structural relationship): - emergency_management_agencies:
 *   Agenda-setting administrator (institutional/identity_locked) — runs the
 *   calendar, certifies compliance, collects budget and legitimation -
 *   municipal_elected_officials: Visible-reassurance collector
 *   (powerful/mobile) — appears at drills, cites certifications, bears no
 *   administration - compliance_training_vendors: Mandate-fed supplier
 *   (organized/arbitrage) — revenue scales with required exercise volume -
 *   structural_inspection_professionals: Competent-subsystem carriers
 *   (organized/mobile) — licensed engineers whose signatures bind them to
 *   real outcomes - residents_in_hazard_zones: Residual-risk bearer
 *   (powerless/trapped) — consumes readiness signals, absorbs losses when
 *   performance outruns capability - frontline_responders: Dual-positioned
 *   participant (organized/constrained) — supplies the personnel drills
 *   consume, benefits from the parts that work - post_event_review_boards:
 *   Analytical observer (analytical/analytical) — reconstructs after losses
 *   what inspections caught and what drills failed to predict
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda_setter (institutional power, identity_locked exit, national scope) — administers exercise calendars and certification standards; exercise volume feeds budget requests and legislative testimony
 *   - municipal_elected_officials: beneficiary (powerful, mobile exit, regional scope) — collect visible preparedness credentials within electoral horizons without administering the programs
 *   - compliance_training_vendors: beneficiary (organized, arbitrage exit, national scope) — sell mandated courses and certification preparation; revenue tracks mandate volume, not measured readiness
 *   - structural_inspection_professionals: beneficiary (organized, mobile exit, continental scope) — licensed engineers performing statutory inspections whose findings bind their licenses and reputations to physical outcomes
 *   - residents_in_hazard_zones: payer (powerless, trapped exit, local scope) — bear injury and property loss when documented readiness outruns actual capability; place-bound by housing, employment, and family land
 *   - frontline_responders: payer with secondary beneficiary position (organized, constrained exit, regional scope) — shifts supply the personnel for scheduled exercises; the same calendar funds equipment checks and inter-agency familiarity that function in real incidents
 *   - post_event_review_boards: observer (analytical, analytical exit, national scope) — accident boards and inquiry committees that reconstruct the gap between exercised and required capability after losses occur
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.52).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.42).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster-Preparedness Maintenance Regime (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/civic-safety").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '27133b68-ba4e-4beb-a2a1-08347cdff755').
narrative_ontology:cs_kernel_codification('27133b68-ba4e-4beb-a2a1-08347cdff755', formalized).
narrative_ontology:cs_authority_grounding('27133b68-ba4e-4beb-a2a1-08347cdff755', practice).
narrative_ontology:cs_interpretation_layer_present('27133b68-ba4e-4beb-a2a1-08347cdff755').
narrative_ontology:cs_reading_relation('27133b68-ba4e-4beb-a2a1-08347cdff755', preparedness_persistence__husk_reading, influences).
narrative_ontology:cs_reading_relation('27133b68-ba4e-4beb-a2a1-08347cdff755', preparedness_persistence__competence_reading, influences).
narrative_ontology:cs_axiom('27133b68-ba4e-4beb-a2a1-08347cdff755', foundational, component_level_functional_assessment).
narrative_ontology:cs_axiom_status(component_level_functional_assessment, holdable).
narrative_ontology:cs_axiom_grounding('27133b68-ba4e-4beb-a2a1-08347cdff755', component_level_functional_assessment, empirically_contingent).
narrative_ontology:cs_axiom('27133b68-ba4e-4beb-a2a1-08347cdff755', foundational, ritualized_rehearsal_fails_readiness).
narrative_ontology:cs_axiom_status(ritualized_rehearsal_fails_readiness, holdable).
narrative_ontology:cs_axiom_grounding('27133b68-ba4e-4beb-a2a1-08347cdff755', ritualized_rehearsal_fails_readiness, empirically_contingent).
narrative_ontology:cs_reference_frame('27133b68-ba4e-4beb-a2a1-08347cdff755', functional_stratified_readiness).
narrative_ontology:cs_drift_state('27133b68-ba4e-4beb-a2a1-08347cdff755', contemporary_post_audit_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27133b68-ba4e-4beb-a2a1-08347cdff755', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, municipal_elected_officials).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, compliance_training_vendors).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, structural_inspection_professionals).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, residents_in_hazard_zones).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, frontline_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets exercise calendars, writes compliance standards, certifies facilities and municipalities, and publishes readiness scores. Exercise volume feeds budget requests and legislative testimony; after-action documents are the agency's public proof of function. The agency's mission identity is constituted by administering these programs — abandoning the calendar would mean telling its overseers that the schedule it built measures little, which the institution experiences as self-annulment rather than reform.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, identity_locked, national).

% Appear at annual drills, open new emergency operations centers, and cite readiness certifications in campaigns. They collect visible reassurance without administering anything, and term limits mean they are typically elsewhere by the time long-horizon readiness questions mature. Their engagement with the regime is ceremonial attendance plus budget approval.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, municipal_elected_officials, beneficiary,
    powerful, immediate, mobile, regional).

% Sell mandated courses, tabletop exercise packages, and certification preparation. Revenue scales with the volume of required exercises and documentation rather than with measured readiness outcomes, and product lines pivot to whatever the next mandate wave requires. Exit from any single contract is trivial because the demand is statutory.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, compliance_training_vendors, beneficiary,
    organized, biographical, arbitrage, national).

% Licensed engineers who perform dam, levee, bridge, and critical-facility inspections under statutory schedules. Findings carry professional liability: a structure they signed off that later fails is traceable to their seal, so the inspection record binds their reputation and license to physical outcomes. The mandate generates demand for their credential; the license gives them market mobility across jurisdictions and sectors.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, structural_inspection_professionals, beneficiary,
    organized, biographical, mobile, continental).

% Live downstream of dams, on floodplains, and along wildland interfaces. Published readiness scores and drill coverage inform whether they buy insurance, maintain stockpiles, or evacuate early on warnings. They bear injury and property loss when documented capability outruns delivered capability, and they fund the regime through taxation. Moving away means leaving homes, jobs, and family land, so their exposure is effectively permanent.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, residents_in_hazard_zones, payer,
    powerless, generational, trapped, local).

% Fire, EMS, and rescue personnel whose shifts supply the staffing for scheduled exercises. Scripted scenarios consume training hours that unannounced, realistic exercises would allocate differently, and union contracts govern how much exercise duty can be assigned. The same calendar also funds equipment checks, mutual-aid familiarity, and command-staff relationships that show up measurably in real incidents, so their net position depends on which half of the schedule a given quarter emphasizes.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, frontline_responders, beneficiary).

% Accident boards, legislative inquiry committees, and academic research teams that reconstruct, after losses occur, which inspections flagged the relevant defects and what the exercised response actually achieved against what the event required. Their reports carry no operating authority and arrive too late to prevent the event under study, but they are the principal outside check on the regime's self-assessment.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, post_event_review_boards, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, emergency_management_agencies).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Catastrophic-risk readiness is a decaying public good: physical infrastructure degrades silently, response coordination rusts between incidents, and no individual actor captures the full benefit of maintenance. The regime solves this by scheduling verification (inspections that force defect repair before failure) and rehearsal (exercises that keep multi-agency response paths practiced), on statutory calendars that no single organization would sustain voluntarily.
% TRANSFER_FUNCTION: Moves staff time, training hours, and appropriations from operating units and general funds into scheduled exercise and inspection activity; moves assurance upward as documentation, certifications, and readiness scores to oversight bodies and legislatures; moves reassurance outward to the public through published preparedness ratings; and, in the ritualized subsystem, moves residual risk implicitly onto residents who calibrate their own precautions to official signals.
% ABSENT_VOICES: Residents in hazard zones sit on no planning committee and learn exercise results only through press releases; their objections surface as litigation after losses. Frontline responders' critiques of scripted drills are collected through after-action channels administered by the same agencies that run the drills, and unfavorable findings are routinely reframed as resource requests. Independent engineers who identify systemic inspection-pressure problems speak mainly through professional societies, at one further remove. The unanimity of official readiness assessments partly reflects these missing seats.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, statutory inspection schedules would lapse and latent structural defects would accumulate undetected until they surfaced as failures; exercise calendars, vendor contracts, and certification economies would dissolve within a budget cycle; agencies would lose the documentation that anchors their appropriations; and residents would lose the readiness signals that currently shape their insurance and evacuation behavior. Multi-agency response coordination would need to be rebuilt ad hoc during the next major event.
% FOUNDING_PROBLEM: A sequence of catastrophic failures — dam collapses, industrial fires, flood events where paper plans met no practiced response — established that organizations do not maintain tested readiness spontaneously: plans rot, equipment goes unchecked, and inter-agency coordination decays without scheduled exercise. The regime was built to force maintained, verified readiness on institutions that would otherwise let it decay.
% FOUNDING_PROBLEM_CORROBORATION: Post-event review boards outside the benefiting parties corroborate both halves: accident inquiries repeatedly credit statutory inspections with catching and forcing repair of defects that preceded major events (attesting the founding problem is real and the competent subsystem serves it), while the same inquiries document gaps between exercised and delivered response capability in the drill subsystem (attesting that part of the apparatus no longer serves it). Academic disaster research and professional engineering society reviews corroborate from further outside; no corroboration exists for the claim that scheduled drills as currently formatted maintain operational readiness — that claim is attested only by the agencies and vendors that run and supply them.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 — moderate, because the regime's largest cost lines are split between genuine function (inspection finding and forcing repair of real defects) and localized waste (drill hours, documentation overhead, vendor fees decoupled from outcomes). Suppression is authored at 0.42 as a RAW structural property, unscaled by power or scope: participation is compelled by statute, audit, funding condition, and liability exposure rather than by preference, but organizations retain meaningful latitude to exceed minimums and some jurisdictions run stronger voluntary programs. Theater_ratio at 0.48 reflects the hybrid claim directly: close to half of regime activity is oriented toward producing records rather than capability, concentrated in the drill subsystem while inspection remains predominantly functional. Accessibility_collapse at 0.40 — alternatives (unannounced exercises, scenario-based assessment, third-party audit) remain conceivable and are occasionally piloted, but the compliance apparatus makes them costly and reputationally awkward, so they collapse only partly. Resistance at 0.45 — after-action reviews, union grievances over exercise duty, journalistic scrutiny following visible failures, and recurring reform proposals supply steady friction without ever displacing the calendar. The temporal series run on ONE shared grid (points 0, 4, 8, 12, 16, 20, 24; every tracked metric authored at every point) and show punctuated rather than smooth drift: plateaus interrupted by jumps at points 12 and 16 corresponding to post-disaster mandate waves, each wave adding documentation requirements and audit layers faster than it added realism. The claim/metric relationship is deliberately unreconciled: the regime is CLAIMED as tangled_rope (genuine coordination function plus asymmetric, localized extraction through one enforced structure) while the metrics describe a structure hovering near the boundary where the ritualized subsystem's share of activity threatens to define the whole.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the regime is a functioning program it built and defends: the calendar is its product, the certifications its deliverables, and the budget trajectory its evidence of success. From the vendor seat it is a stable demand curve. From the responder seat it is a time tax with occasional real payoff — scripted exercises consume training hours that realistic ones would spend differently, yet the same apparatus funds equipment checks and inter-agency familiarity that demonstrably matter in actual incidents. From the resident seat the regime is invisible until it fails, at which point the gap between documented readiness and delivered capability lands as injury and loss. Same-level differentiation is sharpest between municipal officials and residents: both face the same hazards locally, but officials operate on electoral horizons with mobile exit (they will be elsewhere when long-horizon readiness questions mature), while residents are place-bound across generations and consume the regime's reassurance signals directly. The engine computes these per-seat classifications from the structural data; this commentary explains the asymmetry without adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. Emergency management agencies, elected officials, vendors, and inspection professionals sit toward the beneficiary end (low d): the regime subsidizes their budgets, visibility, revenue, and professional standing respectively. Residents in hazard zones sit near the full-target end (high d): they pay in complacency-shaped tail risk and in taxes funding the performance, with trapped exit amplifying their effective burden. Frontline responders are genuinely dual-positioned — the derivation from payer-plus-secondary-beneficiary risks landing them at symmetric when their net position is modestly target-side, hence the override to d=0.60 for the organized power atom. Emergency management agencies get an explicit override to d=0.22 for the institutional atom: although they also expend staff effort inside the regime, the flows that matter (appropriations, mandate expansion, legitimation) run toward them, and the derivation cannot distinguish this administrator-beneficiary fusion from a neutral administrator. Effective extraction is amplified for the trapped, place-locked resident seat and damped for the mobile official and arbitrage-capable vendor seats; scope effects apply at the national scale of the mandate. Suppression, again, enters the computation unscaled — only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading exists precisely to prevent two opposite mislabels. Read through the competence lens, the whole regime looks like live coordination and the drill subsystem's waste and false-confidence externalities disappear from the ledger. Read through the husk lens, the whole regime looks like inertial performance and the inspection subsystem's genuine preventive function — defects found, repairs forced, failures averted — is written off as theater. The stratified classification keeps both halves on the books: coordination function sufficient to satisfy the tangled-rope gate (inspection retains teeth, and parts of the drill apparatus still build inter-agency familiarity), asymmetric extraction sufficient to require naming who pays (responders' time, residents' tail risk, taxpayers' vendor bills). Mandatrophy here is subsystem-local rather than regime-wide: the drill mandate's founding function — building rehearsed capability — has atrophied in place, while the inspection mandate's founding function persists. Accordingly the regime-level mandatrophy flag is left unset and the founding problem is rated live; the open question is whether the ritualized subsystem's appetite for budget and staff cannibalizes the competent one, which the talent_cannibalization_risk omega tracks. If that omega resolves affirmatively, the regime-level classification should be revisited toward the degraded pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the hybrid_reading of kernel preparedness_persistence: preparedness is stratified, with engineering inspection remaining functionally competent and evacuation drills becoming ritualized. How would the sibling readings restructure the classification?',
    'Component-level outcome studies resolving the competence status of each subsystem; the husk_reading (all form, atrophied function) would classify the whole regime as inertial performance with theater_ratio above 0.7, while the competence_reading (all live exercised knowledge) would classify it as low-extraction coordination near the Boltzmann floor.',
    'Under husk_reading the regime loses its coordination gate entirely and migrates toward the degraded/inertial category; under competence_reading the extraction attributed to the drill subsystem disappears and epsilon falls toward coordination cost. The hybrid reading holds both possibilities open at the subsystem level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Reading-of-kernel position: stratification claim versus the two blanket sibling readings').

omega_variable(
    stratification_boundary_stability,
    'Is the boundary between the competent subsystem (statutory engineering inspection) and the ritualized subsystem (scheduled evacuation drills) stable over time, or is it drifting?',
    'Longitudinal component-level audit correlating each preparedness activity with measured outcomes in subsequent actual events (defects found and repaired per inspection hour; response-time and casualty deltas attributable to drill participation).',
    'If the boundary drifts toward ritualization (inspection hours converting to documentation hours), the regime reclassifies toward the degraded/inertial pole; if drills are restored to realistic formats, the regime migrates toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary_stability, empirical, 'Whether the competent/ritualized split is a stable property or a transitional state').

omega_variable(
    false_confidence_externality,
    'How much residual risk does published readiness documentation transfer onto residents in hazard zones through complacency — insurance uptake, evacuation lag, stockpiling decisions keyed to official scores?',
    'Matched-jurisdiction comparison of casualty and damage outcomes between high drill-compliance and low drill-compliance areas, controlling for hazard exposure and inspection intensity.',
    'A measurable complacency effect raises the effective burden on the resident seat substantially above its direct-cost share and pushes the regime toward the purely extractive pole; a null effect confines the harm to wasted staff time and budget.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_confidence_externality, empirical, 'Magnitude of the complacency risk-transfer borne by hazard-zone residents').

omega_variable(
    talent_cannibalization_risk,
    'Does the ritualized subsystem draw budget and skilled staff away from the competent subsystem, given that both run on the same institutional substrate (the same agencies, training pipelines, and appropriations)?',
    'Budget and staffing allocation analysis across the two subsystems within shared agencies, plus attrition tracking of licensed inspectors into exercise-administration roles.',
    'If cannibalization is real, the hybrid configuration is transient and the whole regime follows the atrophy trajectory of the drill subsystem; if the subsystems are fiscally and professionally separable, the stratification is durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talent_cannibalization_risk, empirical, 'Whether the ritualized subsystem erodes the substrate supporting the competent one').

omega_variable(
    liability_cover_motivation,
    'Do scheduled drills persist because they shift legal liability (a documented exercise is a defense in post-event litigation) rather than because anyone believes they build capability?',
    'Compare drill-format decisions before and after changes in liability rules; examine whether counsel, rather than operations staff, controls exercise design; review insurer requirements referencing documented exercises.',
    'If liability cover is the operative driver, the ritualized subsystem is deliberately maintained against known ineffectiveness, which sharpens its extractive character; if the driver is inertia and habit, the subsystem is better read as unmaintained residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_cover_motivation, conceptual, 'Deliberate liability-shifting versus inertial habit as the driver of drill ritualization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_persist_hybrid_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prep_persist_hybrid_tr_t4, preparedness_persistence__hybrid_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(prep_persist_hybrid_tr_t8, preparedness_persistence__hybrid_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(prep_persist_hybrid_tr_t12, preparedness_persistence__hybrid_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(prep_persist_hybrid_tr_t16, preparedness_persistence__hybrid_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(prep_persist_hybrid_tr_t20, preparedness_persistence__hybrid_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(prep_persist_hybrid_tr_t24, preparedness_persistence__hybrid_reading, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(prep_persist_hybrid_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_persist_hybrid_be_t4, preparedness_persistence__hybrid_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(prep_persist_hybrid_be_t8, preparedness_persistence__hybrid_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(prep_persist_hybrid_be_t12, preparedness_persistence__hybrid_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(prep_persist_hybrid_be_t16, preparedness_persistence__hybrid_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(prep_persist_hybrid_be_t20, preparedness_persistence__hybrid_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(prep_persist_hybrid_be_t24, preparedness_persistence__hybrid_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prep_persist_hybrid_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prep_persist_hybrid_su_t4, preparedness_persistence__hybrid_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement(prep_persist_hybrid_su_t8, preparedness_persistence__hybrid_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(prep_persist_hybrid_su_t12, preparedness_persistence__hybrid_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(prep_persist_hybrid_su_t16, preparedness_persistence__hybrid_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(prep_persist_hybrid_su_t20, preparedness_persistence__hybrid_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(prep_persist_hybrid_su_t24, preparedness_persistence__hybrid_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The colloquial label 'disaster preparedness regime' covers at least two structurally distinct claims that this hybrid reading holds together by design: (1) statutory technical inspection of physical infrastructure, which retains genuine preventive function and would author as a low-extraction coordination constraint on its own; (2) scheduled operational rehearsal, which has drifted toward documentation performance and would author as a substantially more extractive, possibly inertial constraint on its own. Per the epsilon-invariance principle, a fully decomposed corpus would write these as two stories with separate epsilon values, separate beneficiary/victim structures, and a network edge from the inspection story to the drill story (the inspection mandate's credibility is routinely cited as evidence for the regime's overall legitimacy, which the drill subsystem rides). This file intentionally holds the regime-level hybrid reading as one constraint because the kernel contest is precisely about whether the stratification is real; the per-subsystem decomposition is the natural next step if stratification_boundary_stability resolves the boundary as durable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, organized, 0.6).
constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
