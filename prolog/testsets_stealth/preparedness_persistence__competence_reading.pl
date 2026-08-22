% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Drill-and-Inspection Regime as Exercised Competence (Competence Reading)
 *   domain: institutional/safety-preparedness
 *
 * SUMMARY:
 *   A standing drill-and-inspection regime — scheduled evacuation and
 *   response exercises plus equipment and procedure inspections, mandated by
 *   fire and workplace-safety codes and verified by inspection bodies — is
 *   the institutional machinery through which organizations convert written
 *   emergency plans into exercised capability. This story instantiates the
 *   competence_reading of the preparedness_persistence kernel: on this
 *   reading the regime is live exercised knowledge, and the
 *   practice-performance link it rests on holds. The epsilon referent is the
 *   standing arrangement under contest — the regime as it actually operates —
 *   assessed by this reading's own lights; the sibling readings (husk,
 *   hybrid) author the same referent from their own lights in separate files
 *   and are not adjudicated here. Claim and metrics are authored
 *   independently: the claim is rope; the metrics describe a low-extraction,
 *   lightly-coerced, predominantly functional regime with slow documentation
 *   accretion.
 *
 * KEY AGENTS:
 *   - regulatory_inspection_bodies: agenda-setter (institutional/analytical) — writes and verifies the requirements the regime runs on
 *   - operating_organizations: payer-and-beneficiary (institutional/constrained) — funds the cycle, absorbs its work-time, carries the loss reduction
 *   - emergency_response_teams: primary beneficiary (organized/identity-locked) — the rehearsed responders whose competence the cycle maintains
 *   - facility_occupants: beneficiary with incidental payer role (moderate/mobile) — walk the drills, receive the practiced escape
 *   - safety_training_specialists: beneficiary (moderate/mobile) — design and run the exercises; the mandate is their market
 *   - liability_insurers: beneficiary (institutional/arbitrage) — price the verified readiness the regime produces
 *   - emergency_management_researchers: analytical observer — the outside check on the practice-performance link
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.14).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.18).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Drill-and-Inspection Regime as Exercised Competence (Competence Reading)").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "institutional/safety-preparedness").

domain_priors:requires_active_enforcement(preparedness_persistence__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '4be25b0a-3592-4316-ab6f-8522f3ac73d8').
narrative_ontology:cs_kernel_codification('4be25b0a-3592-4316-ab6f-8522f3ac73d8', formalized).
narrative_ontology:cs_authority_grounding('4be25b0a-3592-4316-ab6f-8522f3ac73d8', expertise).
narrative_ontology:cs_interpretation_layer_present('4be25b0a-3592-4316-ab6f-8522f3ac73d8').
narrative_ontology:cs_reading_relation('4be25b0a-3592-4316-ab6f-8522f3ac73d8', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('4be25b0a-3592-4316-ab6f-8522f3ac73d8', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('4be25b0a-3592-4316-ab6f-8522f3ac73d8', foundational, drill_practice_transfers_to_real_event_performance).
narrative_ontology:cs_axiom_status(drill_practice_transfers_to_real_event_performance, holdable).
narrative_ontology:cs_axiom_grounding('4be25b0a-3592-4316-ab6f-8522f3ac73d8', drill_practice_transfers_to_real_event_performance, empirically_contingent).
narrative_ontology:cs_axiom('4be25b0a-3592-4316-ab6f-8522f3ac73d8', secondary, readiness_is_perishable_and_must_be_renewed).
narrative_ontology:cs_axiom_status(readiness_is_perishable_and_must_be_renewed, holdable).
narrative_ontology:cs_axiom_grounding('4be25b0a-3592-4316-ab6f-8522f3ac73d8', readiness_is_perishable_and_must_be_renewed, empirically_contingent).
narrative_ontology:cs_reference_frame('4be25b0a-3592-4316-ab6f-8522f3ac73d8', exercised_competence_standard).
narrative_ontology:cs_drift_state('4be25b0a-3592-4316-ab6f-8522f3ac73d8', contemporary_compliance_documentation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4be25b0a-3592-4316-ab6f-8522f3ac73d8', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_response_teams).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, facility_occupants).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, operating_organizations).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, safety_training_specialists).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, liability_insurers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, operating_organizations).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, facility_occupants).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, skill_decay_without_practice).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, stress_inoculation_through_rehearsal).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, equipment_reliability_requires_scheduled_verification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fire marshals, workplace-safety agencies, and accrediting bodies write the drill-frequency and inspection-interval requirements, run scheduled and unannounced verifications, and cite organizations that fall short. They set the regime's tempo and hold its enforcement levers; they bear response burden and reputational exposure when protected premises fail, but not the drill time or the direct disaster losses.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, regulatory_inspection_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Hospitals, plants, schools, and large employers schedule the exercises, fund the training, maintain the equipment that gets inspected, and absorb the work-time the cycle consumes. In return they carry lower disaster losses, better insurance terms, and defensible compliance positions. They cannot exit the mandates where they operate, and abandoning the cycle would hand them the liability and the losses the exercises exist to prevent.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, operating_organizations, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, operating_organizations, beneficiary).

% Internal fire brigades, medical response teams, and plant emergency squads are the people the exercises rehearse. The cycle builds their stress-tested competence and keeps it current against turnover, and their standing as the ones who can act when the alarm is real is constituted by that maintained capability. Leaving the role means leaving the identity; staying, the exercises are the substance of the craft rather than an imposition on it.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_response_teams, beneficiary,
    organized, biographical, identity_locked, local).

% Employees, students, and residents walk the evacuations and sit through the shelter drills. They give work-time and routine to the cycle and get back practiced escape routes and a body that knows what the alarm means. Any one of them can leave a building or an employer, but the next premises runs its own cycle — the practice follows them across addresses.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, facility_occupants, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, facility_occupants, payer).

% Internal safety officers and external training and drill-design firms build the scenarios, run the exercises, and prepare organizations for inspection. The mandated cycle is their market: its budgets are their revenue. They are mobile across employers and clients, and their livelihood tracks the cycle staying live rather than any single organization's compliance.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, safety_training_specialists, beneficiary,
    moderate, biographical, mobile, national).

% Underwriters price demonstrated preparedness: drill records and clean inspection histories lower premiums and shape coverage terms. They receive the verified risk information the cycle generates and can reprice or withdraw from markets that let preparedness slide; their exposure is portfolio-wide and their exit is arbitrage across jurisdictions.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, liability_insurers, beneficiary,
    institutional, generational, arbitrage, global).

% Disaster sociologists, fire-protection engineers, and after-action analysts test whether exercised units outperform unexercised comparators in real events. They hold no stake in the mandate's survival and no seat in its administration; their findings are the outside check on the practice-performance link the whole cycle rests on.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_management_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes recurring practice and verification so that a workforce's emergency skills stay current despite turnover and decay, and so that equipment, exits, and procedures are checked on a schedule rather than trusted from last year's check.
% TRANSFER_FUNCTION: Moves scheduled work-time and budget from operating organizations and their occupants into rehearsal, inspection, and corrective maintenance; a fee share accrues to the safety-training and inspection sector; the return flows back as verified readiness and reduced disaster losses.
% ABSENT_VOICES: Shift workers and high-tempo units that experience drill time as pure disruption have thin representation in rule-setting; disabled occupants whose evacuation needs standard drill scripts under-serve are largely outside the conversation; downstream communities protected by a facility's readiness benefit without any seat at the table — and would likely ask for more drilling, not less. Comment periods on the codes are the partial channel for the first two.
% DISAPPEARANCE_RATIONALE: If the exercise cycle stopped overnight, competence would decay on the turnover clock: within months evacuation routes would be half-remembered, response teams would lose procedural synchronization, and equipment defects would go unspotted between annual checks. Insurers would reprice unverified risk, regulators would rebuild mandates, and the first post-gap disasters would land on unexercised organizations — the regime's disappearance is exactly the scenario its founding problem describes.
% FOUNDING_PROBLEM: A century of industrial and public-building disasters showed that written plans and installed equipment do not execute themselves: occupants freeze at unfamiliar exits, teams improvise incompatibly, and unmaintained equipment fails at the moment of use. The drill-and-inspection regime was built to convert paper preparedness into exercised, verified capability before the event.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: post-incident investigation boards (fire and chemical-safety investigators) repeatedly document that exercised organizations outperform unexercised comparators in the same event, and peer-reviewed emergency-behavior research on evacuation timing and stress performance independently supports the practice-performance link. Insurer loss data corroborates the differential but sits inside the beneficiary set; the investigation boards and the research literature are the outside corroborators. No party outside the regime's beneficiaries disputes that the founding problem recurs.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.14: the cycle's costs — drill time, training budget, inspection fees — are real but roughly reciprocal; participants and organizations receive the readiness the spend purchases, and the residual above coordination cost is thin in a competitive training market. Suppression 0.18: participation is mandated and inspected, but coercive overhead is light because compliance is broadly endorsed; the mandate binds organizations (constrained exit) more than individuals. Theater 0.12: under this reading the exercise cycle is predominantly functional; the performative share is documentation accretion — sign-off sheets, compliance binders — and it is managed rather than dominant. Accessibility_collapse 0.62: once skill decay is understood, the no-practice alternative collapses (readiness cannot be stored), though format and frequency alternatives persist. Resistance 0.22: time-cost grumbling and frequency objections, little organized resistance, because the function is endorsed by the seats that bear it. Coordination type: identity_coordination — the regime's dominant function is maintaining currency of qualification (organizational readiness licensure, in effect); the FNL gaming check was performed and passes under this reading, since the identity framing is not a cover story when theater is low and the function is live; the default 0.08 floor is used, no override. Both tracked series run on one shared six-point grid (T0-T40); suppression_requirement is deliberately not authored as a series because enforcement capacity is static across the interval — the base_properties scalar carries it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. The agenda-setter seat experiences a verification regime it administers and can tighten. Operating organizations experience a compliance cost that returns as loss reduction and insurance terms — near-symmetric. The identity-locked response teams experience the cycle as the substance of their craft rather than an imposition on it. Occupants experience mild disruption against diffuse protection. Insurers, with arbitrage exit, sit nearest the beneficiary end. Under this reading no seat experiences the regime as extraction — the divergence the engine should compute is between 'load-bearing coordination' and 'cheap verification,' not between beneficiary and victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: every declared group sits toward the beneficiary end, with distance set by exit. Insurers (arbitrage) sit nearest d=0; mobile training specialists next; organizations (constrained exit, payer primary) are the nearest-to-symmetric seat — they pay the direct cost but hold the secondary beneficiary position; identity-locked response teams are beneficiaries whose lock deepens their stake without pushing them toward target, since the regime subsidizes rather than taxes them. No victim class is declared because under this reading none exists — the costs are reciprocal. Receipt-surface check (performed before authoring gain_flow): drill spend reaches safety_training_specialists as payment for delivered services at market rates; organizations retain the capability they pay to build; occupants' time converts into their own competence; insurers receive information, not transfers. No named seat accrues the regime's thin residual above coordination cost, so gain_flow is an affirmative 'diffuse,' not a default. fixing_cost is 'prohibitive': rescinding the cycle is mechanically trivial for the agenda-setter, but the consequence cost — decaying competence, unverified equipment, repriced risk, the loss profile the founding problem describes — exceeds any benefit of removal. The diffuse-plus-prohibitive combination is the piton cell's signature, and this story is deliberately not a piton: the discriminators are the low theater_ratio, the live founding problem, and the absence of atrophy — the receipt facts describe a load-bearing coordination regime whose removal would be costly, not a maintained husk.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — written plans that no one can execute under stress — is live: disasters recur and turnover keeps decaying competence, so there is no outlived mandate to resolve. The mandatrophy question is exactly where the sibling readings diverge: the husk_reading's answer is that the mandate HAS outlived its function and what persists is form. This story's classification guards against the opposite misread as well — defunding live practice on the strength of theater anecdotes would convert a functional regime into the decay the husk reading predicts. The falsifier is internal to this file: a rising theater_ratio in the measurement series is the early signature that the practice core is hollowing; the authored series rises only slowly (0.08 to 0.12) and the reading stands or falls on that trajectory staying flat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preparedness_kernel_reading_indexicality,
    'This story is one reading of the preparedness_persistence kernel — the competence_reading, on which drills and inspections are live exercised knowledge. Do the data support this reading over its siblings, husk_reading (form persists while competence atrophies) and hybrid_reading (stratified competence)?',
    'Matched comparison of drill-exercise scores against after-action performance in real events at the same organizations; unannounced-versus-announced drill deltas; independent inspection findings versus self-reported readiness.',
    'If husk_reading is right, the regime''s theater share and extraction are far higher than authored here and the classification migrates toward piton; if hybrid_reading is right, this story''s single epsilon fragments into per-component stories. The low-extraction profile authored here holds only under the competence_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preparedness_kernel_reading_indexicality, empirical, 'Committer indexicality: this constraint instantiates the competence_reading of the preparedness_persistence kernel; sibling readings would restructure epsilon and type.').

omega_variable(
    practice_transfer_coefficient,
    'What fraction of drill-exercised competence survives the stress, noise, crowding, and novelty of a real event — the transfer coefficient the competence claim rests on?',
    'After-action studies comparing exercised and unexercised units in comparable incidents; evacuation-timing studies against drill baselines; meta-analysis of the stress-inoculation literature.',
    'High transfer confirms the coordination reading and the low theater_ratio; low transfer means drill time is partly wasted — theater_ratio understated, and part of what this story books as coordination cost is extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_transfer_coefficient, empirical, 'The practice-performance transfer coefficient underlying the competence claim.').

omega_variable(
    mandate_vs_endorsement_persistence,
    'Is the regime''s persistence carried by regulatory mandate (coercion) or by organizational endorsement of the function (coordination)?',
    'Drill frequency and quality in comparable-risk settings before mandates or outside their reach; organizational behavior when enforcement lapses or is deferred.',
    'Endorsement-carried: the authored suppression of 0.18 is if anything overstated and the coordination reading strengthens. Mandate-carried: suppression understated, enforcement-dependence higher, and the profile drifts toward enforced-hybrid territory at the margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_vs_endorsement_persistence, empirical, 'Whether mandate or endorsement carries the regime''s persistence.').

omega_variable(
    component_stratification_boundary,
    'This reading authors one epsilon for the drill-and-inspection regime as a whole; if components diverge systematically (engineering inspection versus evacuation drills), does the hybrid_reading''s decomposition become the correct frame?',
    'Component-level after-action and inspection-failure data: if inspection-detected defect rates and drill-performance deltas diverge sharply by component, stratification is real.',
    'A confirmed stratified structure would split this constraint into per-component stories with divergent epsilon — the hybrid file''s territory — and this story''s single-epsilon profile would be an artifact of aggregation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_stratification_boundary, conceptual, 'Whether the regime is homogeneous enough for one epsilon or stratified as the hybrid reading holds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__competence_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__competence_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__competence_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__competence_reading, theater_ratio, 32, 0.11).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__competence_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__competence_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__competence_reading, base_extractiveness, 16, 0.12).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__competence_reading, base_extractiveness, 24, 0.12).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__competence_reading, base_extractiveness, 32, 0.13).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__competence_reading, base_extractiveness, 40, 0.14).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the preparedness_persistence kernel decomposes into three readings — competence_reading (this file), husk_reading, and hybrid_reading. The colloquial claim 'drills and inspections maintain readiness' is epsilon-ambiguous across readings: the same standing regime assesses as low-extraction live coordination (this file), as maintained husk (husk file), or as a stratified mix (hybrid file). Per the epsilon-invariance principle the label was disambiguated into three stories rather than one observable-parameterized story; all three link here via affects_constraints. The competence reading is the institutional default from which the other two dissent; after-action evidence flows from research into all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
