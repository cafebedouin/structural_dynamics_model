% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Standing Drill and Training Regime (Competence Reading)
 *   domain: institutional/disaster-preparedness
 *
 * SUMMARY:
 *   A response agency operates a standing regime of mandated drills, scenario
 *   exercises, certification gates, and structured instruction, designed to
 *   keep rare-event operational capacity alive across continuous generational
 *   turnover. This story instantiates ONE reading of the
 *   preparedness_commitment kernel — the competence_reading, under which
 *   these routines are live exercised knowledge that maintains real capacity.
 *   Per the epsilon-referent rule, extractiveness is authored for the
 *   standing drill-and-training arrangement itself, assessed by this
 *   reading's own lights: the regime's costs are real (drill hours,
 *   appropriated budget, instructional overhead) but are the price of the
 *   coordination, sitting just above the enforcement-mechanism floor. The
 *   sibling readings (husk_reading, hybrid_reading) are separate constraints
 *   in separate files with their own epsilon values over the same observable
 *   corpus; they are not averaged into this one. KEY AGENTS (by structural
 *   relationship): - emergency_response_agency: agenda-setting coordinator
 *   (institutional/constrained) — administers the drill calendar, passes
 *   through the funding, cannot abandon its statutory mandate -
 *   frontline_responders: dual-positioned participant (organized/constrained)
 *   — bears recurring drill-hour costs and collects the competence those
 *   hours build - veteran_practitioners: knowledge holders and instructors
 *   (moderate/identity_locked) — transmit the tacit craft; their standing is
 *   fused with the teaching role - incoming_recruits: turnover intake
 *   (powerless/mobile) — absorb the transmitted routines; their attrition is
 *   what the training system absorbs - protected_communities: diffuse
 *   intended recipients (powerless/mobile) — fund and are shielded by the
 *   capacity - budget_authorities: cost-bearing funder (powerful/mobile) —
 *   appropriates the budget and retains the defunding lever -
 *   mutual_aid_partners: excluded interdependent parties
 *   (moderate/constrained) — inherit drilled procedure without a seat in its
 *   design - preparedness_researchers: analytical observers
 *   (analytical/analytical) — test whether exercise performance predicts
 *   incident performance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.21).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.25).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.21).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Standing Drill and Training Regime (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "institutional/disaster-preparedness").

domain_priors:requires_active_enforcement(preparedness_commitment__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '6a45bcc2-6990-4bc1-9191-c67582865a11').
narrative_ontology:cs_kernel_codification('6a45bcc2-6990-4bc1-9191-c67582865a11', formalized).
narrative_ontology:cs_authority_grounding('6a45bcc2-6990-4bc1-9191-c67582865a11', lineage).
narrative_ontology:cs_interpretation_layer_present('6a45bcc2-6990-4bc1-9191-c67582865a11').
narrative_ontology:cs_reading_relation('6a45bcc2-6990-4bc1-9191-c67582865a11', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a45bcc2-6990-4bc1-9191-c67582865a11', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('6a45bcc2-6990-4bc1-9191-c67582865a11', foundational, routines_are_operationally_load_bearing).
narrative_ontology:cs_axiom_status(routines_are_operationally_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('6a45bcc2-6990-4bc1-9191-c67582865a11', routines_are_operationally_load_bearing, empirically_contingent).
narrative_ontology:cs_axiom('6a45bcc2-6990-4bc1-9191-c67582865a11', foundational, structured_practice_transmits_competence_across_generations).
narrative_ontology:cs_axiom_status(structured_practice_transmits_competence_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('6a45bcc2-6990-4bc1-9191-c67582865a11', structured_practice_transmits_competence_across_generations, empirically_contingent).
narrative_ontology:cs_axiom('6a45bcc2-6990-4bc1-9191-c67582865a11', secondary, ceremonial_residue_is_cost_not_function).
narrative_ontology:cs_axiom_status(ceremonial_residue_is_cost_not_function, holdable).
narrative_ontology:cs_axiom_grounding('6a45bcc2-6990-4bc1-9191-c67582865a11', ceremonial_residue_is_cost_not_function, instrumental).
narrative_ontology:cs_reference_frame('6a45bcc2-6990-4bc1-9191-c67582865a11', live_exercised_knowledge_regime).
narrative_ontology:cs_drift_state('6a45bcc2-6990-4bc1-9191-c67582865a11', contemporary_audit_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('6a45bcc2-6990-4bc1-9191-c67582865a11', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, protected_communities).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, veteran_practitioners).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, incoming_recruits).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, emergency_response_agency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, budget_authorities).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, budget_authorities).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, skill_decay_empirical_law).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, deliberate_practice_transfer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the standing drill calendar, certifies unit readiness, and employs the instructor corps. Receives appropriated training funds and passes them through to instruction, simulation, and exercise delivery. It cannot walk away from its statutory preparedness mandate, and its continuity across leadership turnovers is the vehicle the routines ride on.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, emergency_response_agency, agenda_setter,
    institutional, generational, constrained, national).

% Staff the response units. They spend recurring shift-hours in drills and scenario exercises and carry the schedule and physical costs of that time; the same hours build the competence they rely on when an event overwhelms ordinary operations. Leaving the profession is possible but means abandoning career, seniority, and a working life built inside the service.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, frontline_responders, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, frontline_responders, payer).

% Senior members who hold the tacit craft — judgment calls, informal cues, habits formed in real incidents — and deliver most of the instruction. Their standing inside the service rests on being the people others learn from; stepping back from teaching would cost them the role that organizes their late careers. Retirement is the one exit, and each retirement removes knowledge the written manuals only partially capture.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, veteran_practitioners, beneficiary,
    moderate, biographical, identity_locked, national).

% New hires who arrive without incident experience and absorb the routines through academy and probationary drilling. They can quit in the early years at comparatively low cost, and their attrition is precisely the turnover the training system exists to absorb.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, incoming_recruits, beneficiary,
    powerless, biographical, mobile, national).

% Residents of the hazard zone who fund the service through taxes and are the intended recipients of response capacity when events strike. Individual households have little leverage over drill design; relocating out of the hazard zone is available but costly and disruptive, so most stay and depend on the system's readiness.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, protected_communities, beneficiary,
    powerless, generational, mobile, regional).

% The legislature and finance ministry that appropriate the training budget. In lean cycles they trim exercises and defer simulator purchases because the returns are invisible until a disaster validates them. They retain the standing ability to defund the arrangement, and recurring fiscal pressure keeps that lever live. Their constituents are among those the capacity protects.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, budget_authorities, payer,
    powerful, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, budget_authorities, beneficiary).

% Neighboring jurisdictions whose units plug into joint responses under mutual-aid compacts. They inherit whatever procedures, terminology, and radio habits the standing drills encode, but sit outside the body that writes the drill program. Incompatible tactics discovered mid-incident is the failure they would raise if they had a seat at the table.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, mutual_aid_partners, excluded,
    moderate, biographical, constrained, regional).

% Academic and independent investigators who study whether exercise performance predicts incident performance and whether skills decay on the schedule the training assumes. They publish outside the service's chain of command and hold no operational stake in the answer.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, preparedness_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains perishable operational skills and shared decision protocols across continuous personnel turnover, so that rare-event response capacity exists despite years of disuse between events. It solves the collective-action problem that no individual responder would privately sustain the rehearsal burden for capacities only the assembled unit ever uses.
% TRANSFER_FUNCTION: Moves shift-hours and attention from routine operations into rehearsal and instruction; moves tacit knowledge from retiring veteran cohorts to incoming recruits; moves appropriated funds from general revenue into simulators, exercise logistics, and instructor time.
% ABSENT_VOICES: Mutual-aid partners are structurally outside the drill-program body that writes the procedures they must operate under. Mitigation advocates — engineers and planners who argue hardened infrastructure and land-use policy dominate rehearsed response — are likewise absent from the training calendar's design, and would contest the balance of spend. Protected communities appear as funding taxpayers but not as participants in exercise design.
% DISAPPEARANCE_RATIONALE: If the drill and training regime vanished overnight, certification ladders, shift-handover protocols, mutual-aid compatibility, and the instructor corps would unravel within a few turnover cycles. The first major event after the break would meet units running on decayed individual skill and unshared procedure; the surrounding arrangements — staffing plans, equipment caches sized to drilled doctrine, inter-jurisdiction compacts — all presuppose the standing regime.
% FOUNDING_PROBLEM: Successive disasters exposed that response organizations lose capability between events: veterans retired carrying unwritten craft, replacements arrived unseasoned, and improvised coordination failed at scale. The standing regime was built to convert episodic catastrophe lessons into routine that survives generational turnover.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident-investigation boards and the peer-reviewed skill-decay literature attest from outside the service both that the founding problem recurs and that rehearsed units outperform unrehearsed ones in comparable events; actuarial loss data corroborate the same pattern. No credible outside source attests the founding problem is solved — its recurrence between events is exactly what the standing arrangement answers.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.21, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.21 at interval end): the regime's costs are dominated by genuine skill-maintenance and instruction delivery, with a thin layer of compliance paperwork accreting over the decades. Suppression is low-moderate (0.25): participation is mandated through certification and employment gates, but the mandate is broadly consented to, informal alternatives are not suppressed, and enforcement is light. As a structural property, suppression is authored unscaled — the engine applies directionality and scope scaling to extractiveness only. Theater ratio is low (0.15): anniversary ceremonies and commemorative exercises exist, but the core drill cycle tests real decision-making under graded scenarios, per this reading's defining claim. Accessibility_collapse is moderate (0.40): decentralized self-training and reduced-frequency regimes remain workable alternatives, so understanding the regime does not foreclose substitutes — it merely makes them worse. Resistance is low-moderate (0.25): drill fatigue, union grievances over hours, and cyclical budget trimming, but no sustained opposition, because the value proposition is accepted between disasters. The measurement series run on one shared time grid (t=0,8,16,24,32,40) with both tracked metrics authored at every point; suppression_requirement is deliberately not serialized because the enforcement picture is static — the scalar covers it. The trajectories show a mature-regime pattern: extraction dipped as the curriculum shed redundancy in its first two decades, then crept upward mildly as documentation and compliance overhead accreted; theater crept upward in parallel as commemorative elements layered onto the functional core. Both creeps are bounded and acknowledged by the regime's own after-action audits — the signature of a contained rather than avoided D5 break.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agency's chair, the regime is a coordination achievement it built and stewards across leadership turnovers. From the responders' chairs, it is a worthwhile but genuinely costly obligation — recurring hours surrendered to rehearsal against a benefit that arrives only in rare events. From the budget authorities' chair, it is a line item whose returns are invisible between disasters, which is why the defunding lever stays live and why extraction ticks upward in lean cycles. From the mutual-aid partners' position outside the room, the same routines look like an externality: procedure written unilaterally that they must operate under mid-incident. Same structure, four different experienced types.
 *
 * DIRECTIONALITY LOGIC:
 *   All five declared beneficiaries derive low directionality from the beneficiary declarations: the agency (coordinator-collector), responders, veterans, recruits, and the protected public all sit near the subsidized end. Two overrides correct places where the derivation would misread the structure. First, budget_authorities are the sole powerful seat and would derive a high target-side d from their payer role; but their constituents share the protection benefit and they authorize the spend democratically, so their true position is mildly cost-bearing consent — overridden to 0.40. Second, frontline_responders are the sole organized seat and would derive a near-full-beneficiary d from the beneficiary declaration alone; but they bear the direct recurring time and physical costs in their own bodies, so their true position is dual — overridden to 0.30. The excluded mutual-aid partners sit near symmetric (they neither fund nor collect much directly) but no override is authored because the moderate power atom is shared with veteran_practitioners, whose beneficiary position requires low d; the qualitative symmetry is recorded here instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: rare events continue, skills continue to decay, turnover continues, and outside investigators corroborate all three. Status=live crossed with disappearance_verdict=world_rearranges is the coherent cell — no zombie flag. The classification resists both characteristic mislabelings. Reading the regime as pure extraction would misprice the irreducible cost of maintaining perishable skill as rent; reading its ceremonial fringe (commemorative exercises, milestone observances) as the whole would mistake a functioning coordination core for theatrical residue. The theater_ratio series is the tripwire in both directions: if commemorative accretion accelerates past functional content, or if drill activity decouples from incident outcomes, the husk_reading's description starts to fit and this file's classification should drift — the omegas below specify what evidence would show it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates the competence_reading of the preparedness_commitment kernel: are the standing routines live exercised knowledge, or would the husk_reading (memorial performance lacking operational competence) or the hybrid_reading (layered stabilization-plus-function) describe the same observable corpus better?',
    'Outcome-coupled audit: correlate exercise-fidelity scores with blind-evaluated incident performance across units and decades, and adjudicate which reading the correlation supports.',
    'Under the husk_reading the same routines carry high theater and sharply higher epsilon and the classification shifts toward inertial types; under the hybrid_reading intermediate values obtain. This file''s authored metrics are valid only within the competence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, empirical, 'Which reading of the preparedness kernel the observable routines actually instantiate.').

omega_variable(
    drill_to_incident_transfer,
    'What fraction of drill-hour investment transfers to incident-time performance rather than producing only exercise-context fluency?',
    'Post-incident after-action comparisons of trained versus untrained cohorts, plus controlled studies of skill-decay intervals against the training calendar''s assumed cadence.',
    'Weak transfer would mean part of the regime''s cost buys no capacity, raising effective epsilon and shifting weight toward the hybrid reading; strong transfer confirms this reading''s low-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_to_incident_transfer, empirical, 'Whether exercise performance converts to incident capability at the rate the competence reading assumes.').

omega_variable(
    generational_absorption_capacity,
    'Can structured instruction absorb tacit-knowledge loss when veteran cohorts retire faster than replacements season, or does a fast-retirement window open a competence break the manuals cannot close?',
    'Track unit capability metrics across documented retirement waves and compare against modeled decay curves for unwritten craft.',
    'A failed absorption window would temporarily push the arrangement toward husk dynamics and date a type transition; successful absorption across successive waves supports the generational-turnover claim this reading rests on.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_absorption_capacity, empirical, 'Whether the training pipeline scales to the actual pace of veteran cohort loss.').

omega_variable(
    participation_suppression_character,
    'Is the mandated participation held by structural gates (certification, employment condition) or partly by internalized professional identity that would persist if enforcement lapsed?',
    'Observe participation during enforcement lapses — administrative shutdowns, labor actions, record-keeping gaps: if drill uptake holds when attendance stops being checked, the internalized component dominates.',
    'An internalized majority means the arrangement could persist as ceremony after enforcement decays — the precise precursor the husk_reading predicts; a structural majority ties persistence to active enforcement and keeps the regime''s fate coupled to its administration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_suppression_character, empirical, 'Structural versus internalized character of the participation mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_competence_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(prep_competence_tr_t0, observed).
narrative_ontology:measurement(prep_competence_tr_t8, preparedness_commitment__competence_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement_basis(prep_competence_tr_t8, observed).
narrative_ontology:measurement(prep_competence_tr_t16, preparedness_commitment__competence_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement_basis(prep_competence_tr_t16, observed).
narrative_ontology:measurement(prep_competence_tr_t24, preparedness_commitment__competence_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(prep_competence_tr_t24, observed).
narrative_ontology:measurement(prep_competence_tr_t32, preparedness_commitment__competence_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement_basis(prep_competence_tr_t32, observed).
narrative_ontology:measurement(prep_competence_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(prep_competence_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_competence_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(prep_competence_be_t0, observed).
narrative_ontology:measurement(prep_competence_be_t8, preparedness_commitment__competence_reading, base_extractiveness, 8, 0.19).
narrative_ontology:measurement_basis(prep_competence_be_t8, observed).
narrative_ontology:measurement(prep_competence_be_t16, preparedness_commitment__competence_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement_basis(prep_competence_be_t16, observed).
narrative_ontology:measurement(prep_competence_be_t24, preparedness_commitment__competence_reading, base_extractiveness, 24, 0.19).
narrative_ontology:measurement_basis(prep_competence_be_t24, observed).
narrative_ontology:measurement(prep_competence_be_t32, preparedness_commitment__competence_reading, base_extractiveness, 32, 0.2).
narrative_ontology:measurement_basis(prep_competence_be_t32, observed).
narrative_ontology:measurement(prep_competence_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement_basis(prep_competence_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'organizational preparedness' decomposes, per the epsilon-invariance principle, into three structurally distinct claims over one observable routine corpus. This file authors the competence_reading (epsilon 0.21: costs are the price of live coordination). preparedness_commitment__husk_reading authors the same corpus as memorial performance (high theater, high epsilon, inertial persistence). preparedness_commitment__hybrid_reading authors the layered account (intermediate values: memorial stabilizers plus a functional core). The competence reading is upstream: demonstrated drill-to-incident coupling is the evidentiary input that disciplines both siblings' claims, which is why this file's edges run to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__competence_reading, powerful, 0.4).
constraint_indexing:directionality_override(preparedness_commitment__competence_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
