% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real-Incident Competence Maintenance Trap
 *   domain: organizational/safety
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear power, emergency
 *   medicine, emergency response) maintain practitioner competence through
 *   prescribed training curricula, simulation drills, and periodic
 *   certification. The real-incident-necessity reading asserts that these
 *   mechanisms cannot actually occupy the competence kernel — only genuine
 *   catastrophic incidents provide the authentic conditions (time pressure,
 *   genuine stakes, irreversible consequences, full-spectrum environmental
 *   complexity) necessary to exercise and test the deepest layers of
 *   competence. This reading creates an unsolvable trap: practitioners are
 *   required to maintain competence through mechanisms the reading claims
 *   cannot maintain it; they cannot be trained on real catastrophes without
 *   accepting catastrophic risk; and the reading itself cannot be proven or
 *   falsified without authorizing the training mechanism (live incidents)
 *   that makes the claim catastrophically real. The constraint's victims are
 *   the practitioners locked into the training regime; the beneficiaries are
 *   the training institutions whose funding and legitimacy depend on the
 *   belief that simulation and drills work. The extracted value is
 *   practitioner labor (time spent in theater training), organizational
 *   resources (funding of prescribed systems), and authority (the right to
 *   define competence) — all justified by a kernel claim that may be false.
 *
 * KEY AGENTS:
 *   - flight_crews: highly skilled, moderately powerful, but constrained by FAA/ICAO certification — cannot exit; drilled intensively but trapped in doubt about whether drills occupy the kernel
 *   - nuclear_operators: organized labor, identity-locked (nuclear professional identity makes exit unthinkable) — face the trap most acutely since incidents are catastrophic by definition and forbidden as training
 *   - emergency_response_personnel: powerless relative to institutional mandate, constrained exit (career sunk cost, social commitment) — mandate is to prevent the only 'real test' from happening
 *   - emergency_medicine_staff: moderate power, constrained by legal/ethical limits — cannot use real patients as training substrate; drills use surrogates whose adequacy is precisely what the reading contests
 *   - regulatory_authority: institutional power, sets and enforces the standards — must administer a system they know is contested; suppresses the real-incident-necessity reading to maintain their own authority
 *   - training_institutions: beneficiaries of the prescribed regime, dependent on regulatory mandate for revenue — have incentive to suppress real-incident-necessity reading and promote simulation-sufficiency reading
 *   - incident_investigation_bodies: observers who generate data about actual competence performance — their investigations are the reading's hidden empirical ground, but they cannot openly advocate for using incidents as training
 *   - public/insurance mechanism: excluded, bears catastrophic cost if competence is not truly occupied
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.88).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.72).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.88).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, snare).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident Competence Maintenance Trap").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety").

domain_priors:requires_active_enforcement(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '98351cba-4737-4aaa-b428-c6543781b376').
narrative_ontology:cs_kernel_codification('98351cba-4737-4aaa-b428-c6543781b376', implicit).
narrative_ontology:cs_authority_grounding('98351cba-4737-4aaa-b428-c6543781b376', extraction).
narrative_ontology:cs_interpretation_layer_present('98351cba-4737-4aaa-b428-c6543781b376').
narrative_ontology:cs_reading_relation('98351cba-4737-4aaa-b428-c6543781b376', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('98351cba-4737-4aaa-b428-c6543781b376', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('98351cba-4737-4aaa-b428-c6543781b376', foundational, authentic_stress_necessary_for_kernel_occupation).
narrative_ontology:cs_axiom_status(authentic_stress_necessary_for_kernel_occupation, holdable).
narrative_ontology:cs_axiom_grounding('98351cba-4737-4aaa-b428-c6543781b376', authentic_stress_necessary_for_kernel_occupation, empirically_contingent).
narrative_ontology:cs_axiom('98351cba-4737-4aaa-b428-c6543781b376', secondary, simulation_inadequate_for_stress_authenticity).
narrative_ontology:cs_axiom_status(simulation_inadequate_for_stress_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('98351cba-4737-4aaa-b428-c6543781b376', simulation_inadequate_for_stress_authenticity, empirically_contingent).
narrative_ontology:cs_reference_frame('98351cba-4737-4aaa-b428-c6543781b376', competence_as_deep_psychological_readiness).
narrative_ontology:cs_drift_state('98351cba-4737-4aaa-b428-c6543781b376', contemporary_regulatory_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('98351cba-4737-4aaa-b428-c6543781b376', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, flight_crews).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, nuclear_operators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, emergency_response_personnel).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, emergency_medicine_staff).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, training_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate aircraft under FAA/ICAO certification requirements. Their competence is declared maintained through mandated simulator drills, refresher curricula, and line checks. The real-incident-necessity reading asserts their competence is actually only fully occupied during genuine emergency events — creating a trap: they cannot maintain genuine competence without catastrophic incidents, yet incidents are unacceptable and rare. They drill intensively and pass certifications, but cannot know if the drills occupy the kernel or merely theater it.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, flight_crews, payer,
    moderate, biographical, constrained, global).

% Manage reactor systems under NRC licensing requirements. Their competence must be maintained through prescribed simulator work, classroom refresher, and operational checkouts. The constraint asserts only genuine reactor incidents (which are catastrophic failures) truly test and occupy their competence kernel. Yet the regulatory framework forbids using actual incidents as training. They are locked into an identity (nuclear professional) that makes exit nearly unthinkable; they cannot resolve the tension between the reading's claim and their mandate.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, nuclear_operators, payer,
    organized, generational, identity_locked, national).

% Firefighters, paramedics, police tactical teams operate under training curricula and certification requirements. Their competence must be maintained through prescribed scenarios, drills, and line supervision. The constraint asserts genuine emergencies are the only authentic test; drills are theater. Yet their mandate is to prevent catastrophes from occurring. They face the trap directly: their professional duty is to make the 'only real test' impossible.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, emergency_response_personnel, payer,
    powerless, biographical, constrained, regional).

% Physicians, nurses, trauma surgeons maintain competence through simulation (mannequins, part-task trainers), case review, and periodic resuscitation certifications. The constraint asserts only genuine cardiac arrests, massive hemorrhage, and critical airway emergencies truly occupy competence — yet these are rare, unschedulable, and involve actual risk to patients. Their option set: drill with surrogates (theater by the reading's lights) or use real emergencies (unethical and unacceptable).
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, emergency_medicine_staff, payer,
    moderate, biographical, constrained, local).

% Sets competence maintenance standards and oversees certification. They author the curriculum, mandate the frequency and format of drills, and define what 'occupied competence' means operationally. They enforce through licensing, audit, and sanctions. By the real-incident-necessity reading, their entire framework is theater — their drills do not actually occupy the kernel. Yet they cannot mandate actual incidents as training and remain legitimate. They administer a system they must simultaneously claim works and know is fundamentally challenged.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulatory_authority, agenda_setter,
    institutional, generational, mobile, national).

% Flight training academies, nuclear simulator facilities, emergency medicine residencies, fire academies operate and profit from the prescribed training regime. Their business model depends on the regulatory mandate for periodic certification and drill completion. If the real-incident-necessity reading were widely believed, their institutional legitimacy would collapse — they would be exposed as operators of theater. They have incentive to reinforce belief in simulation sufficiency and disincentive to investigate whether it occupies the kernel.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, training_institutions, beneficiary,
    organized, biographical, mobile, regional).

% NTSB, CSB, medical examiner offices investigate actual incidents to determine causation. Their role is to analyze what happened when real emergencies occurred. The real-incident-necessity reading positions them as the hidden empirical validators: their investigation findings about human performance during actual catastrophes are the only authentic data about competence occupation. Yet their mandate is incident prevention, not competence certification.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_investigation_bodies, observer,
    institutional, generational, analytical, national).

% Passengers, patients, the general public bear the catastrophic cost when competence is not truly occupied. They are structurally excluded from the competence maintenance debate: they cannot credibly argue for more incidents to test competence, cannot participate in the reading's logic. Their interest (genuine competence without catastrophe) is inexpressible within the constraint's terms.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, public_insurance_mechanism, excluded,
    powerful, generational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, training_institutions).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for declaring when practitioners have occupied the competence kernel — defining what 'ready' means, in what contexts, with what renewal cycles. Enables organizational accountability and inter-organizational consistency (all aviation crews certify under same standard).
% TRANSFER_FUNCTION: Moves authority to define competence from individual practitioners and local organizations to centralized regulatory and training institutions. Transfers the implicit burden of proof: practitioners must demonstrate they are ready through prescribed mechanisms, rather than institutions demonstrating their standards work.
% ABSENT_VOICES: Practitioners who believe the real-incident-necessity reading but fear career consequences for saying so (competence assessors who suspect their own drills are theater but cannot act on the suspicion without delegitimizing their institutions). Catastrophe victims who never existed because incidents were prevented — their absence is the reading's unspeakable problem. Families of those who died in the rare catastrophic incident that testing would have prevented, had real incidents been authorized as training.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared, organizations would either institute authorized 'live training incidents' (ethically catastrophic) or collapse into genuine competence maintenance crisis. The regulatory regime would fragment: some organizations might lower bar to favor simulation, others might demand higher-cost hybrid approaches, others might face liability exposure if competence gaps are revealed. Professional identity would destabilize for practitioners locked into the identity_locked exit category.
% FOUNDING_PROBLEM: Complex operations require practitioners to maintain physical and cognitive skills across scenarios too rare for natural experience (aviation crew coordination in multi-engine failure, nuclear plant response to loss-of-coolant accident, resuscitation in exsanguinating trauma). No organization can let untrained crews or operators handle real catastrophes. A mechanism was needed to certify they were ready.
% FOUNDING_PROBLEM_CORROBORATION: Incident investigations routinely discover human-factor failures (failure to recall procedure, degraded physical coordination, ineffective team communication) — establishing that real competence does decay and must be maintained. NTSB reports, medical error analyses, near-miss investigations from outside the benefiting training institutions attest the problem persists. However, the same investigations typically do not establish that the prescribed training actually prevented the incident or would have prevented it.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88 at interval end) and rising because the constraint extracts labor, resources, and authority from practitioners and organizations through a mechanism the reading questions — if the reading is true, practitioners spend time in theater, organizations spend resources on theater, regulators enforce theater. Suppression is steady-high (0.72) because the real-incident-necessity reading is actively suppressed: regulatory authority enforces the belief that prescribed training works; training institutions' funding depends on that belief; incident investigation bodies do not openly advocate for using incidents as training (which would be ethically catastrophic); practitioners who doubt the system face career consequences for saying so. Theater ratio is high and rising (0.48 → 0.61) because the proportion of mandated training activity that is performative (simulating high-stakes conditions rather than occupying them) grows as the reading gains adherents and organizations layer on additional 'assurance' drills and metrics to defend the system against the doubt. Accessibility to alternatives is nearly collapsed (0.79) because practitioners cannot simply refuse certification or use unapproved training methods — certification is mandatory, alternatives are forbidden by regulation. Resistance is substantial (0.68) because practitioners know the reading has empirical plausibility (they see gaps between drills and real performance), and some regulatory voices (incident investigators, reform advocates) push back against the suppression. The grid shows individual-level suppression higher than organizational (individuals can be fired for expressing the real-incident-necessity reading; organizations are supposed to be compliant but some harbor doubt). Class-level resistance rises over the interval as the reading spreads through professional networks despite suppression. Structural-level accessibility collapse stays near-maximum because the reading itself is constitutionally blocked: any framework that mandates incident prevention cannot also mandate incident-based training.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory authority's and training institutions' perspective, the constraint is benign coordination — establishing standards, enabling accountability, producing demonstrable skill maintenance. From the practitioner's perspective (especially the identity-locked nuclear operators and emergency responders), the constraint is a trap: they are required to maintain competence through mechanisms that the reading suggests cannot maintain competence, yet they cannot express the doubt without career consequences. From the incident investigator's perspective, the constraint is ambiguous — they see actual competence gaps in real emergencies, which makes the reading plausible, but they cannot advocate the resolution (using incidents as training) without ethically catastrophic implications. The engine should compute these divergent classifications from the structural data: regulatory authority and training institutions should show low d (beneficiaries); practitioners should show high d (targets); incident investigators should show mid-range d (they benefit from the structured framework but bear reputational risk if the real-incident-necessity reading spreads).
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authority (institutional power, mobile exit, sets the standards) is a beneficiary: they collect authority and deflect accountability risk by declaring that their prescribed standards work. Training institutions (organized power, mobile exit through regulatory capture) are beneficiaries: they depend on the mandate for revenue and legitimacy. Practitioners (powerless to moderate power, constrained-to-identity-locked exit) are targets: they pay in labor, time, career risk (if they express doubt), and ongoing anxiety about whether they are actually competent. The public/insurance mechanism is excluded: they bear the catastrophic cost if the reading is true and practitioners are not actually competent. Incident investigators occupy an ambiguous position: they are observers (analytical power, analytical exit) officially, but they are also targets in another sense — if the real-incident-necessity reading becomes dominant, their investigations become evidence that prescribed training fails, which would delegitimize their own institutional base (safety-through-training narratives). The directionality should reflect this: regulatory authority near 0.0 (full beneficiary), practitioners near 1.0 (full target), incident investigators near 0.4 (slightly beneficiary because they officially validate the system, but at risk if the reading spreads).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not exhibiting mandatrophy in the classic sense (founding problem solved but mechanism persists). The founding problem (practitioners lack authentic competence maintenance mechanism) is disputed but live — the real-incident-necessity reading precisely asserts the founding problem is unsolved. However, the reading creates a paradox that resembles mandatrophy: the founding problem cannot be solved using the available institutional mechanisms (because the solution would require authorizing real incidents as training, which is catastrophic), yet the system persists through suppression of the reading and enforcement of belief in simulation sufficiency. This is not mandatrophy (the problem has not been solved) but it is a zombie-like state: the system is defended not because it provably works but because the alternative (admitting the kernel cannot be occupied through institutional means) is unthinkable. The real mandatrophy question lies at the kernel level: has the competence-kernel definition itself become obsolete? If practitioners can achieve sufficiently reliable performance through simulation, procedural discipline, and organizational checks, has the reading's core claim (that only real incidents occupy the kernel) been superseded? The commentary should not answer this — it belongs in an omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_occupancy_empirical_gap,
    'What empirical evidence would establish whether simulation-based drills actually occupy the competence kernel or merely provide theatrical reassurance?',
    'Comparative analysis of near-miss patterns and actual incident performance across organizations with different training regimes (full simulation, hybrid, minimal). If incident causation traces back to skill factors that simulation should have addressed, the reading is supported; if incidents trace to rare scenario types or external factors, the reading is weakened.',
    'If real-incident-necessity is empirically supported, the constraint shifts from snare to something worse — an institutional configuration that cannot solve its founding problem through legitimate means. If simulation-sufficiency is empirically supported, the reading is falsified and training institutions'' beneficiary status is legitimized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_occupancy_empirical_gap, empirical, 'Whether simulation drills occupy the competence kernel or are theater.').

omega_variable(
    identity_lock_mechanism_interpretation,
    'For nuclear operators and emergency responders marked as identity_locked, is the lock mechanism professional identity fusion (self-concept as nuclear operator), careerist sunk cost, or something else?',
    'Post-exit trajectory analysis: do practitioners who leave the field report that professional identity was the barrier, or financial/social factors? Do they report retained belief in the real-incident-necessity reading after exit?',
    'If lock is identity fusion, the constraint is even more extractive (it fuses the target into the trap and makes exit cognitively impossible). If lock is sunk cost, exit is theoretically possible if incentives change. If lock is ideological commitment to the reading itself, exiting the field is exit from belief, not just career.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpretation, empirical, 'What mechanism binds identity-locked practitioners into the constraint.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of the real-incident-necessity reading primarily structural (regulatory prohibition, career consequences, funding dependence) or internalized (practitioners self-suppress doubt to maintain professional identity)?',
    'Post-suppression-removal trajectory: if regulatory environment shifted to allow open discussion of the reading, would practitioners immediately voice the doubt, or does self-suppression persist? Are there proxy signals (anonymous surveys, off-the-record conversations, exit interviews) where practitioners express the doubt despite suppression?',
    'If structural, regulatory reform could shift the constraint''s operation. If internalized, the constraint is more deeply held and would persist even if formal suppression ended. If mixed, the internalied component represents the reading''s penetration and adherence even among those it extracts from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is maintained externally or internalized by practitioners.').

omega_variable(
    training_institution_genuine_belief,
    'Do training institution leaders genuinely believe simulation occupies the competence kernel, or do they believe the real-incident-necessity reading and suppress it for institutional survival?',
    'Documentary analysis of internal communications (email, meeting notes, curriculum design rationales) where the belief is expressed without external audience. Interviews with retired training leaders after they have left institutional incentive structures.',
    'If genuine belief, the beneficiary status is legitimate (they are not knowing extractors, just participants in a shared coordination framework). If suppressed belief, they are knowing exploiters and the constraint''s extractiveness is amplified by the institutional lie. If mixed (some believe, some suppress), the constraint is internally contested at the beneficiary level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(training_institution_genuine_belief, preference, 'Whether training institutions genuinely believe the simulation-sufficiency reading or suppress the real-incident-necessity reading for institutional reasons.').

omega_variable(
    kernel_definition_obsolescence,
    'Has the competence kernel itself been redefined or made obsolete over the interval? If the definition of ''occupied competence'' has shifted from ''psychological readiness to handle authentic stress'' to ''demonstrable knowledge and procedural compliance,'' has the kernel definition changed such that drills now legitimately occupy a new kernel?',
    'Genealogy of regulatory standards and organizational policy: trace how ''competence'' was defined in founding period vs. now. If definition has shifted from kernel occupancy (deep preparedness) to performance on defined metrics (checklist completion), the reading is about an outdated kernel.',
    'If the kernel definition has shifted, the real-incident-necessity reading is historically valid but now moot — it is critiquing a kernel that is no longer the operative one. If definition is stable, the reading remains live and the constraint remains a trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_definition_obsolescence, conceptual, 'Whether the competence kernel definition has shifted over the interval such that drills now validly occupy a redefined kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__real_incident_necessity, theater_ratio, 5, 0.51).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.54).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__real_incident_necessity, theater_ratio, 15, 0.57).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__real_incident_necessity, theater_ratio, 25, 0.6).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.61).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_occupation__real_incident_necessity, base_extractiveness, 5, 0.7).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_occupation__real_incident_necessity, base_extractiveness, 15, 0.82).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t25, competence_occupation__real_incident_necessity, base_extractiveness, 25, 0.87).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.88).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_occupation__real_incident_necessity, suppression_requirement, 5, 0.69).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_occupation__real_incident_necessity, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t25, competence_occupation__real_incident_necessity, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(comp_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(comp_grid_01, competence_occupation__real_incident_necessity, accessibility_collapse(class), 0, 0.7).
narrative_ontology:measurement(comp_grid_02, competence_occupation__real_incident_necessity, accessibility_collapse(class), 40, 0.76).
narrative_ontology:measurement(comp_grid_03, competence_occupation__real_incident_necessity, accessibility_collapse(individual), 0, 0.75).
narrative_ontology:measurement(comp_grid_04, competence_occupation__real_incident_necessity, accessibility_collapse(individual), 40, 0.82).
narrative_ontology:measurement(comp_grid_05, competence_occupation__real_incident_necessity, accessibility_collapse(organizational), 0, 0.82).
narrative_ontology:measurement(comp_grid_06, competence_occupation__real_incident_necessity, accessibility_collapse(organizational), 40, 0.88).
narrative_ontology:measurement(comp_grid_07, competence_occupation__real_incident_necessity, accessibility_collapse(structural), 0, 0.85).
narrative_ontology:measurement(comp_grid_08, competence_occupation__real_incident_necessity, accessibility_collapse(structural), 40, 0.88).
narrative_ontology:measurement(comp_grid_09, competence_occupation__real_incident_necessity, resistance(class), 0, 0.58).
narrative_ontology:measurement(comp_grid_10, competence_occupation__real_incident_necessity, resistance(class), 40, 0.65).
narrative_ontology:measurement(comp_grid_11, competence_occupation__real_incident_necessity, resistance(individual), 0, 0.62).
narrative_ontology:measurement(comp_grid_12, competence_occupation__real_incident_necessity, resistance(individual), 40, 0.68).
narrative_ontology:measurement(comp_grid_13, competence_occupation__real_incident_necessity, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(comp_grid_14, competence_occupation__real_incident_necessity, resistance(organizational), 40, 0.72).
narrative_ontology:measurement(comp_grid_15, competence_occupation__real_incident_necessity, resistance(structural), 0, 0.68).
narrative_ontology:measurement(comp_grid_16, competence_occupation__real_incident_necessity, resistance(structural), 40, 0.7).
narrative_ontology:measurement(comp_grid_17, competence_occupation__real_incident_necessity, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(comp_grid_18, competence_occupation__real_incident_necessity, stakes_inflation(class), 40, 0.68).
narrative_ontology:measurement(comp_grid_19, competence_occupation__real_incident_necessity, stakes_inflation(individual), 0, 0.68).
narrative_ontology:measurement(comp_grid_20, competence_occupation__real_incident_necessity, stakes_inflation(individual), 40, 0.72).
narrative_ontology:measurement(comp_grid_21, competence_occupation__real_incident_necessity, stakes_inflation(organizational), 0, 0.75).
narrative_ontology:measurement(comp_grid_22, competence_occupation__real_incident_necessity, stakes_inflation(organizational), 40, 0.81).
narrative_ontology:measurement(comp_grid_23, competence_occupation__real_incident_necessity, stakes_inflation(structural), 0, 0.78).
narrative_ontology:measurement(comp_grid_24, competence_occupation__real_incident_necessity, stakes_inflation(structural), 40, 0.84).
narrative_ontology:measurement(comp_grid_25, competence_occupation__real_incident_necessity, suppression(class), 0, 0.65).
narrative_ontology:measurement(comp_grid_26, competence_occupation__real_incident_necessity, suppression(class), 40, 0.67).
narrative_ontology:measurement(comp_grid_27, competence_occupation__real_incident_necessity, suppression(individual), 0, 0.71).
narrative_ontology:measurement(comp_grid_28, competence_occupation__real_incident_necessity, suppression(individual), 40, 0.74).
narrative_ontology:measurement(comp_grid_29, competence_occupation__real_incident_necessity, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(comp_grid_30, competence_occupation__real_incident_necessity, suppression(organizational), 40, 0.71).
narrative_ontology:measurement(comp_grid_31, competence_occupation__real_incident_necessity, suppression(structural), 0, 0.7).
narrative_ontology:measurement(comp_grid_32, competence_occupation__real_incident_necessity, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.18).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel admits three structurally distinct constraint readings. real_incident_necessity asserts only catastrophic events occupy the kernel — high extractiveness, snare classification. simulation_sufficiency asserts drills occupy the kernel — lower extractiveness, rope classification likely. hybrid_occupation asserts calibrated multi-mechanism exercise occupies the kernel — moderate extractiveness, tangled_rope classification likely. Each reading has different beneficiary/victim sets and different narratives about what the founding problem is. They cannot coexist in a single constraint; decomposition into three linked stories is required by ε-invariance (different readings yield different ε values relative to the standing arrangement). The network edges show real_incident_necessity influencing the other two: it creates structural pressure on hybrid_occupation (if real incidents are necessary, the hybrid is incomplete) and forecloses simulation_sufficiency (if only real incidents occupy, drills alone cannot).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
