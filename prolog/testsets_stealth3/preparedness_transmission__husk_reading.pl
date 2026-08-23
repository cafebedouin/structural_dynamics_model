% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Flood-Response Drill Cycle as Memorial Ritual (Husk Reading)
 *   domain: disaster risk management/institutional memory/civil defense
 *
 * SUMMARY:
 *   A national statutory cycle of flood-response drills and inspections,
 *   founded after catastrophic river floods to re-validate municipal response
 *   capability each generation, now runs at high form compliance and low
 *   adaptive capacity: scenario libraries froze a generation ago, roles are
 *   assigned years in advance, and inspections certify presence and currency
 *   against a fixed checklist of historically known failure modes. Units
 *   score well on everything the cycle measures, while post-event inquiries
 *   repeatedly find decision failure under exactly the conditions the cycle
 *   never rehearses. Under this husk reading, the arrangement persists
 *   because abandoning it would require the administering authority to
 *   concede publicly that decades of certified exercises did not produce the
 *   readiness the certificates imply, and because the cycle still carries
 *   organizational memory - names, dates, badges, anniversaries - that
 *   participants experience as the substance of the service. This story is
 *   ONE READING of the preparedness_transmission kernel; the competence and
 *   hybrid readings are separate constraints with their own epsilon values,
 *   linked through network.affects_constraints. The claimed type and the
 *   authored metrics are independent facts: the metrics describe what the
 *   arrangement does, the claim states what I believe it structurally is, and
 *   the engine computes per-seat classifications from the structural data.
 *   KEY AGENTS (by structural relationship): -
 *   national_civil_defence_authority: Agenda-setting administrator
 *   (institutional/constrained) - owns the calendar, could redesign it,
 *   inherits transition liability - compliance_inspection_body:
 *   Co-administrator (institutional/constrained) - certifies form against
 *   pre-specified failure modes only - municipal_response_units: Principal
 *   paying seat (organized/identity_locked) - spends its working days in the
 *   cycle; identity fused with it - elected_municipal_officials: Incidental
 *   beneficiary (powerful/mobile) - collects visible-assurance credit without
 *   defending the machinery - floodplain_households: Silent paying seat
 *   (powerless/trapped) - carries unpriced false-confidence risk; absent from
 *   design - retired_response_veterans: Excluded voice (moderate/mobile) -
 *   holds living memory of the live-capacity era -
 *   disaster_research_community: Analytical observer (analytical/analytical)
 *   - documents the form-function gap across decades
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.52).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.38).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.74).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.74).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Flood-Response Drill Cycle as Memorial Ritual (Husk Reading)").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster risk management/institutional memory/civil defense").

domain_priors:requires_active_enforcement(preparedness_transmission__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '31baace4-25d2-4c19-8ef5-36d0a986ef6f').
narrative_ontology:cs_kernel_codification('31baace4-25d2-4c19-8ef5-36d0a986ef6f', formalized).
narrative_ontology:cs_authority_grounding('31baace4-25d2-4c19-8ef5-36d0a986ef6f', lineage).
narrative_ontology:cs_interpretation_layer_present('31baace4-25d2-4c19-8ef5-36d0a986ef6f').
narrative_ontology:cs_reading_relation('31baace4-25d2-4c19-8ef5-36d0a986ef6f', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('31baace4-25d2-4c19-8ef5-36d0a986ef6f', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('31baace4-25d2-4c19-8ef5-36d0a986ef6f', foundational, ritual_form_outlived_operational_content).
narrative_ontology:cs_axiom_status(ritual_form_outlived_operational_content, holdable).
narrative_ontology:cs_axiom_grounding('31baace4-25d2-4c19-8ef5-36d0a986ef6f', ritual_form_outlived_operational_content, empirically_contingent).
narrative_ontology:cs_axiom('31baace4-25d2-4c19-8ef5-36d0a986ef6f', secondary, organizational_memory_carried_by_performed_cycle).
narrative_ontology:cs_axiom_status(organizational_memory_carried_by_performed_cycle, holdable).
narrative_ontology:cs_axiom_grounding('31baace4-25d2-4c19-8ef5-36d0a986ef6f', organizational_memory_carried_by_performed_cycle, instrumental).
narrative_ontology:cs_reference_frame('31baace4-25d2-4c19-8ef5-36d0a986ef6f', protocol_performance_as_readiness).
narrative_ontology:cs_drift_state('31baace4-25d2-4c19-8ef5-36d0a986ef6f', post_event_inquiry_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('31baace4-25d2-4c19-8ef5-36d0a986ef6f', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, elected_municipal_officials).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, municipal_response_units).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, floodplain_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, municipal_response_units).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns the statutory drill calendar and the flood-response doctrine every municipality trains against. Sets annual exercise themes, approves scenario scripts, and reports completion statistics upward. Inherited the regime from the post-flood commission reforms and staffs it through career civil servants whose evaluations rest on delivering the cycle. Rewriting the curriculum would mean publicly conceding that decades of recorded exercises did not produce the readiness the records imply, and would expose any transition window to blame if a flood arrived mid-reform; the authority therefore maintains and incrementally annotates the existing programme.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, national_civil_defence_authority, agenda_setter,
    institutional, generational, constrained, national).

% Conducts scheduled inspections of municipal drill programmes against a published checklist of equipment, documentation, and sequence items. The checklist enumerates specific failure modes agreed after historical incidents; inspectors verify presence, currency, and sign-off. Findings feed funding eligibility and public readiness ratings. Extending assessment to open-ended or novel-failure evaluation would require competencies and mandate changes the office has not been given; its inspectors are trained and resourced for enumeration against the fixed list.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, compliance_inspection_body, agenda_setter,
    institutional, generational, constrained, national).

% Uniformed and volunteer squads in riverine municipalities who perform the scheduled exercises: sandbagging sequences, pump deployments, and evacuation walk-throughs run to script with roles assigned years in advance. Members spend several working days per cycle on exercises whose scenarios repeat annually, and between exercises few independently rehearse decisions outside the printed scenarios. Many joined through family service traditions and measure belonging by drill proficiency badges and anniversary ceremonies; transferring out of a response unit means leaving the service community, not swapping an assignment.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, municipal_response_units, payer,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, municipal_response_units, beneficiary).

% Mayors and council members who attend the flagship annual exercise, address the assembled units, and cite passing inspection grades in constituent communications and budget hearings. The visible cycle hands them concrete evidence of protective action without requiring them to fund or design alternative training. Their attention moves with election timing; none chairs the bodies that set drill content, and few could describe the scenario scripts in detail.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, elected_municipal_officials, beneficiary,
    powerful, immediate, mobile, regional).

% Residents of designated flood-risk zones whose safety planning implicitly relies on the municipal readiness the drill cycle advertises. They receive public assurances grounded in inspection grades and exercise photographs, and calibrate insurance, evacuation assumptions, and home-retention decisions accordingly. They are not consulted on scenario design, do not attend planning sessions, and learn of capability gaps only when an actual event departs from the rehearsed script; moving out of the floodplain would mean selling homes whose market price embeds the advertised protection.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, floodplain_households, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, floodplain_households, excluded).

% Former squad leaders and district commanders from the decades when exercises were staged against unknown opposing scenarios and graded on improvisation. They remember the training culture the current scripts replaced, compare it unfavorably at reunions and in letters to service journals, and occasionally mentor younger members informally. They hold no seat on curriculum boards, their memoranda reach the authority as correspondence rather than agenda items, and their standing derives from personal history rather than office.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, retired_response_veterans, excluded,
    moderate, generational, mobile, national).

% Academic groups and institute analysts who compile post-event comparisons between exercised scenarios and realized floods, survey response units on decision autonomy, and publish readiness-gap assessments. They see the full record across municipalities and decades, including which failure classes the inspection checklist has never covered. Their recommendations enter the system as cited literature and conference proceedings; they hold no enforcement or budget authority.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, disaster_research_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every riverine municipality a common annual rhythm, a shared scenario vocabulary, standardized equipment and documentation lists, and mutually legible records that let districts, insurers, and government read one another's preparedness claims; it also fixes the dates on which dispersed units assemble and stand alongside one another.
% TRANSFER_FUNCTION: Moves staff days and municipal budget into scripted exercise and inspection activity; converts that activity into signed checklists, grades, and public readiness statements; the resulting assurance flows outward from response organizations to officials and residents, while the unexamined gap between rehearsed and required capability is carried as silent risk by flood-zone households.
% ABSENT_VOICES: Floodplain households bear the residual risk yet sit outside every design forum; retired veterans who remember live-opposition exercises write letters that arrive as correspondence, not agenda items; neighboring mutual-aid districts assume interoperability from published grades without ever exercising jointly. Structured consultation occurs mainly as post-event testimony after failures have already surfaced.
% DISAPPEARANCE_RATIONALE: Administratively the world rearranges quickly: audit calendars, funding eligibility rules, training budget lines, ceremonial schedules, and official communications all reference the cycle, and numerous municipal roles exist to run it. Operationally little would change immediately - realized flood outcomes would diverge from published readiness only when a real event met unrehearsed conditions - which is precisely why the rearrangement would be administrative long before it became substantive.
% FOUNDING_PROBLEM: After catastrophic river floods overwhelmed uncoordinated municipal responses, the reform commission concluded that response capability evaporates between events unless each generation of responders physically rehearses coordination under pressure; the statutory drill-and-inspection cycle was built to guarantee that rehearsal and to prove, on a fixed schedule, that it had occurred.
% FOUNDING_PROBLEM_CORROBORATION: Post-event inquiry commissions seated outside the drill programme found exercised scenarios matched realized events only where events repeated known patterns, and documented decision failures in exactly the novel conditions the checklist never covered; university disaster-studies groups replicate the finding across municipalities and decades; retired commanders from the pre-scripted era independently attest the transmission broke as scenario libraries froze. The authority itself attests the founding problem is managed, citing continuous compliance records; no corroboration of that attestation exists from outside the offices that produce the records.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.52: the cycle consumes real staff days, budgets, and equipment wear while returning mostly certification artifacts, but it is not rent - no seat converts the flow into defended private gain, and the post-2015 plateau reflects saturation, since budget ceilings cap the ritual's appetite. Suppression is 0.38 and is authored as a raw structural property, unscaled: statutory mandation and funding-linked inspection create real compulsion, but most compliance is habitual. Theater is 0.74, the defining number for this reading: frozen scenario libraries, pre-assigned roles, inspections that enumerate a fixed failure-mode list, and a growing share of cycle activity that defends the appearance of readiness rather than readiness itself. Accessibility_collapse is 0.30: nothing forbids scenario-injected adaptive training, and reform proposals recur after every inquiry, but the ritual occupies the calendar slot such alternatives need. Resistance is 0.22: veteran letters, researcher documentation, and post-event criticism are persistent but unorganized, because costs are diffuse and no constituency loses visibly enough to build a coalition. The temporal series share one seven-point grid so every metric is asserted at every examined year; theater's monotonic climb against extraction's saturation dates the hollowing transition to roughly the turn of the century, with enforcement machinery formalizing gently throughout rather than ratcheting. Receipt surface: I checked every seat before asserting gain_flow as diffuse - officials collect assurance they do not convert into defended rents, the authority collects audit-legible legitimacy it does not monetize, and no named seat captures the flow. Fixing_cost is prohibitive: the one actor who could rewrite the doctrine faces admission-plus-transition costs far exceeding what it personally bears from continuing.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently because their structural positions differ, not because the written rule varies. From the authority's seat the cycle is inherited duty executed diligently - near-routine, low personal cost, and prohibitively expensive to change. From the response-unit seat the same calendar is compulsory performance that consumes working days and fuses with professional identity, so the pressure that seat experiences is amplified by the identity lock. The identity lock is relational-professional: membership, proficiency badges, and anniversary ceremonies constitute belonging, so exit means leaving the service community rather than swapping assignments; were that frame to break - for instance if a respected peer district visibly thrived under adaptive training - the lock would loosen and the units' exposure would fall toward that of a mobile squad. From the household seat the cycle is invisible except as reassuring grades: a seat that pays without knowing it pays. From the research seat the whole structure is legible at once: form rising, function flat. Same nominal system, four different experienced arrangements, driven by exit-option and information asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation. elected_municipal_officials are the sole declared beneficiary and receive only incidental assurance without defending the cycle, so their directionality lands near but not at the beneficiary end. The two administering institutions are neither declared beneficiaries nor victims; the canonical institutional fallback places them mid-range, matching their position - they run the machine without collecting its output. municipal_response_units and floodplain_households are declared victims: identity lock amplifies the units' effective pressure above what a mobile squad would bear, and residential immobility amplifies the households' exposure. Spatial scope matters at the margins: the national reach of the inspection regime modestly amplifies verified extraction relative to a local analogue, while the households' local trapping concentrates their exposure. Suppression stays raw and unscaled throughout; only extractiveness is scaled, by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton claim prevents two symmetrical mislabels. Read as rope, the cycle's paper coordination function - shared calendar, common vocabulary, legible records - would mask that the capacity it was built to re-validate has hollowed: coordination of form is not coordination of capability. Read as snare, the visible assurance flowing to officials would suggest deliberate capture; but the assurance is diffuse, undefended, and unmonetized, and no seat's income depends on cycle volume - the hidden_capture_check omega tests exactly this residual doubt. The decisive test here is the cost asymmetry, not the theatricality: the authority could rewrite the doctrine, but the cost to it - public admission of hollow certification plus blame exposure during any transition - exceeds what it bears from continuing, so it continues. Theater at 0.74 is the symptom, recorded honestly; the cost asymmetry is the classification-bearing fact. Trajectories from here: if the founding-problem dispute resolves toward renewed novel-scenario capacity, the arrangement migrates back toward live coordination; if a concentrated defender emerges, it migrates toward capture; if enforcement decays faster than ceremony, the suppression trajectory bends down while the ritual outlives its own auditing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_resolution,
    'Which reading of the preparedness_transmission kernel correctly characterizes the standing drill regime: live exercised competence (competence_reading), sector-stratified decay (hybrid_reading), or hollowed memorial ritual (husk_reading)?',
    'Blind, unannounced cross-district exercises scored against novel flood scenarios, disaggregated by failure class and unit type; convergence of independent assessors on adaptive-capacity scores selects among the readings.',
    'If the competence reading holds, measured extraction collapses toward coordination cost and the arrangement re-reads as ordinary routine; if the hybrid reading holds, this story decomposes into separate infrastructure-engineering and civilian-coordination constraints with distinct epsilon values; the husk reading stands only if adaptive capacity fails broadly while form compliance holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_resolution, empirical, 'Kernel-level dispute over which reading instantiates the standing arrangement.').

omega_variable(
    hidden_capture_check,
    'Does any actor covertly profit from the ritual cycle sufficiently to defend it - drill-logistics vendors, training-academy posts, consultancy recipients - such that the arrangement is maintained by interest rather than inertia?',
    'Procurement and payroll tracing around the exercise calendar: identify recurring contracts and posts whose existence scales with cycle volume, then test whether their holders mobilize against reform proposals.',
    'A concentrated defender whose income rides on cycle volume converts the picture from inertia-maintenance to interest-maintenance, moving the classification toward capture-shaped categories and making gain_flow a named seat rather than diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_capture_check, empirical, 'Whether an undetected beneficiary defends the hollowed cycle.').

omega_variable(
    residual_skill_boundary,
    'How much genuine skill transmission survives inside the scripted cycle - motor skills, equipment handling, assembly discipline - as distinct from adaptive command capacity under novel conditions?',
    'Skill-ladder assessment separating physical proficiency from decision-making under novel injects, administered to units at differing tenure depths.',
    'Robust physical-skill transmission would lower the theater measure, shrink measured extraction, and shift weight toward the hybrid reading; uniformly hollow results would confirm the husk characterization and raise effective pressure on participating units.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_skill_boundary, conceptual, 'Where functional transmission ends and performative repetition begins inside the same exercises.').

omega_variable(
    identity_fusion_depth,
    'Is squad members'' attachment to the scripted cycle professional habit that would yield to a credible replacement, or identity fusion that makes abandonment feel like leaving the service community?',
    'Volunteer pilot districts substituting scenario-based adaptive training with equivalent ceremony and badge structures; observe attrition, grievance rates, and informal retention of old scripts.',
    'Shallow fusion would let replacement proceed cheaply once the authority commits, lowering the cost side of the fix and weakening the inertia explanation; deep fusion would keep units carrying the ritual even after mandates lapse, extending the arrangement''s life beyond its administrative scaffolding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_depth, empirical, 'Depth of the identity lock binding responders to performed form.').

omega_variable(
    authority_grounding_frame_choice,
    'Is the administering authority''s legitimacy best read as lineage (custodian of the founding reform''s transmission duty) or as extraction (an interpretation layer whose staffing and mandate size depend on keeping the kernel un-revised)?',
    'Classify the authority''s interpretive updates over the interval: revisions that improve scenario fidelity versus revisions that expand checklist volume, headcount, and reporting obligations while leaving scenario libraries frozen.',
    'An extraction-grounded reading raises the capture weighting on the administration seats and predicts resistance to any reform that shrinks the inspection apparatus; a lineage-grounded reading predicts reform responsiveness once the founding-duty frame is publicly broken.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_frame_choice, conceptual, 'Two coherent framings of the same authority structure yield different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1975, preparedness_transmission__husk_reading, theater_ratio, 1975, 0.24).
narrative_ontology:measurement(prep_tr_t1985, preparedness_transmission__husk_reading, theater_ratio, 1985, 0.34).
narrative_ontology:measurement(prep_tr_t1995, preparedness_transmission__husk_reading, theater_ratio, 1995, 0.46).
narrative_ontology:measurement(prep_tr_t2005, preparedness_transmission__husk_reading, theater_ratio, 2005, 0.57).
narrative_ontology:measurement(prep_tr_t2015, preparedness_transmission__husk_reading, theater_ratio, 2015, 0.66).
narrative_ontology:measurement(prep_tr_t2020, preparedness_transmission__husk_reading, theater_ratio, 2020, 0.71).
narrative_ontology:measurement(prep_tr_t2025, preparedness_transmission__husk_reading, theater_ratio, 2025, 0.74).

% Extraction over time
narrative_ontology:measurement(prep_be_t1975, preparedness_transmission__husk_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement(prep_be_t1985, preparedness_transmission__husk_reading, base_extractiveness, 1985, 0.34).
narrative_ontology:measurement(prep_be_t1995, preparedness_transmission__husk_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(prep_be_t2005, preparedness_transmission__husk_reading, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement(prep_be_t2015, preparedness_transmission__husk_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(prep_be_t2020, preparedness_transmission__husk_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(prep_be_t2025, preparedness_transmission__husk_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1975, preparedness_transmission__husk_reading, suppression_requirement, 1975, 0.16).
narrative_ontology:measurement(prep_su_t1985, preparedness_transmission__husk_reading, suppression_requirement, 1985, 0.22).
narrative_ontology:measurement(prep_su_t1995, preparedness_transmission__husk_reading, suppression_requirement, 1995, 0.27).
narrative_ontology:measurement(prep_su_t2005, preparedness_transmission__husk_reading, suppression_requirement, 2005, 0.32).
narrative_ontology:measurement(prep_su_t2015, preparedness_transmission__husk_reading, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement(prep_su_t2020, preparedness_transmission__husk_reading, suppression_requirement, 2020, 0.37).
narrative_ontology:measurement(prep_su_t2025, preparedness_transmission__husk_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'flood-preparedness drills' names one kernel with at least three structurally distinct readings. This file instantiates the husk reading: it authors epsilon for the standing drill-and-inspection arrangement as the hollowed-ritual diagnosis sees it (moderate diffuse extraction, high theater), never for the live-capacity arrangement its siblings describe. The competence reading is the upstream member - higher confidence in the founding era, supplying the origin story this reading degrades from - and the hybrid reading refines the husk diagnosis by splitting preserved infrastructure competence from decayed civilian coordination. Family members link through affects_constraints; the epsilon differences across the family are the data the corpus exists to take, not an inconsistency to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
