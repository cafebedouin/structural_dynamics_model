% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Memorial-Performance Preparedness Regime (Husk Reading of the Preparedness Commitment)
 *   domain: institutional/civic-safety/commitment-systems
 *
 * SUMMARY:
 *   A civil-protection agency maintains, decades after its founding flood, a
 *   recurring exercise program: the annual flagship simulation, quarterly
 *   tabletop sessions, and the anniversary commemorative walk-through. On
 *   paper the program satisfies every statutory, insurer, and mutual-aid
 *   documentation requirement; in substance the scenarios are scripted,
 *   pre-decided, and handed down from the founding generation, and the
 *   responding crews perform them flawlessly without learning anything
 *   transferable. This file is ONE READING of the contested kernel
 *   preparedness_commitment — the husk_reading, which holds that the routines
 *   feel like retention but lack operational competence. It is a member of a
 *   three-story constraint family: competence_reading (same routines read as
 *   live exercised knowledge, low epsilon), hybrid_reading (memorial layer
 *   stabilizes commitment while competence elements function, intermediate
 *   epsilon), and this file (epsilon 0.42 over the same standing
 *   arrangement). The family is linked via network.affects_constraints; the
 *   historically upstream claim (competence_reading) originally justified the
 *   program's funding, and this downstream diagnosis gains force as
 *   post-event inquiries accumulate. The claim/metric pairing here is
 *   deliberate and unreconciled: claimed_type is piton because the reading's
 *   own lights show a function that atrophied behind a persisting form, while
 *   the metrics describe that operation descriptively — high theater,
 *   moderate extraction, low resistance. Where the engine computes divergent
 *   per-seat types from the structural data, that divergence is the datum.
 *   KEY AGENTS (by structural relationship): -
 *   emergency_management_directorate: agenda_setter
 *   (institutional/constrained) — administers the calendar, certifies
 *   completion upward - veteran_instructors: custodian-payers
 *   (organized/identity_locked) — inherited the scenario canon, identity
 *   fused with carrying the memory - frontline_response_units: primary payers
 *   (organized/constrained) — surrender crew-hours to scripted performance -
 *   recruit_cohorts: payers-in-training (powerless/constrained) —
 *   credentialed into form - at_risk_communities: payers (powerless/trapped)
 *   — consume reassurance, bear the event-day gap - exercise_design_vendors:
 *   incidental beneficiaries (moderate/arbitrage) -
 *   legislative_oversight_committees: analytical observers
 *   (institutional/analytical) - past_disaster_survivor_families: excluded
 *   (powerless/trapped) — invoked to sanctify the ritual, seated nowhere
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.42).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.4).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Memorial-Performance Preparedness Regime (Husk Reading of the Preparedness Commitment)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "institutional/civic-safety/commitment-systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, '11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6').
narrative_ontology:cs_kernel_codification('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', fixed_text).
narrative_ontology:cs_authority_grounding('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', lineage).
narrative_ontology:cs_interpretation_layer_present('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6').
narrative_ontology:cs_reading_relation('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', preparedness_commitment__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', preparedness_commitment__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', foundational, routine_form_decoupled_from_capability).
narrative_ontology:cs_axiom_status(routine_form_decoupled_from_capability, holdable).
narrative_ontology:cs_axiom_grounding('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', routine_form_decoupled_from_capability, empirically_contingent).
narrative_ontology:cs_axiom('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', foundational, commemoration_has_displaced_rehearsal).
narrative_ontology:cs_axiom_status(commemoration_has_displaced_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', commemoration_has_displaced_rehearsal, empirically_contingent).
narrative_ontology:cs_reference_frame('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', documented_readiness_regime).
narrative_ontology:cs_drift_state('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', novel_stress_first_contact, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('11eb3cb3-0ca1-4bb8-9194-9ab44b8342d6', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, veteran_instructors).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_response_units).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, recruit_cohorts).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, at_risk_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, exercise_design_vendors).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, emergency_management_directorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the civil-protection agency's exercise calendar: schedules the annual flagship simulation, the quarterly tabletop sessions, and the commemorative joint walk-through held each anniversary of the founding flood. Certifies completion upward to the ministry, the insurer, and the mutual-aid partners. Its own staff sit inside the same drills, so the office spends its own hours on them; reshaping the calendar would mean confronting ministry deliverables and the office's own founding history. Abandoning the duties outright is not a live option — the statutory mandate and public expectation attach to the office itself.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, emergency_management_directorate, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__husk_reading, emergency_management_directorate, payer).

% Senior officers, many second-generation responders, who inherited scenario design from the founders. Their standing inside the service rests on being the ones who carry the memory of the founding disaster; the anniversary walk-through they lead is the emotional center of the institutional year. Questioning whether the scenarios still teach anything would indict their own life's work and their mentors' legacy, so they defend the form fiercely and treat proposals for no-notice adaptive exercises as disrespect to the dead. Leaving the service is available on paper; leaving the identity is not.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, veteran_instructors, payer,
    organized, generational, identity_locked, national).

% Professional fire, rescue, and medical crews. Each year they surrender several hundred crew-hours to scripted scenarios whose outcomes are pre-written in the master scenario book handed down from the founding generation. Crews know the script beats, perform them correctly, and return to station. Opting out is not permitted, and requests to change scenarios route through the same office that writes them. Union energy goes to rosters and pay, not to pedagogy.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_response_units, payer,
    organized, biographical, constrained, regional).

% New hires complete the same certification sequence their supervisors completed: classroom modules, the scripted tabletop, the anniversary walk-through. They graduate credentialed in form. Most assume the credential tracks ability until their first real incident teaches otherwise; by then many have absorbed the rhythm and become its next defenders. Transferring out of the service is possible but uncommon, and the certification travels with them everywhere they go inside it.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, recruit_cohorts, payer,
    powerless, biographical, constrained, national).

% Towns in the floodplain and industrial corridor the agency serves. They receive the public-facing output: press releases after each exercise, school visits, the anniversary ceremony. Households report high confidence that the plan works. They cannot relocate out of the hazard zone cheaply, and they learn the distance between the documents and the capability only during the rare event that tests it.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, at_risk_communities, payer,
    powerless, generational, trapped, regional).

% Consultancies supplying scenario packages, facilitation, and after-action templates compliant with the national exercise doctrine. Revenue follows the documentation cycle regardless of which philosophy governs the program — they would happily sell harder, adaptive designs to a buyer who wanted them. Their fees are a modest slice of the exercise budget and their contracts renew on delivery of documents, not on measured capability.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, exercise_design_vendors, beneficiary,
    moderate, biographical, arbitrage, national).

% Audit committee staff who review exercise completion certificates and after-action filings each cycle. Their instruments count documents produced, not capabilities demonstrated, so each review returns a clean bill of health. They convene hearings only after major failures, at which point the record they hold is a complete archive of everything that was rehearsed and nothing that was learned.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, legislative_oversight_committees, observer,
    institutional, generational, analytical, national).

% Families of those lost in the founding flood. They are invited to lay wreaths at the anniversary ceremony; their loss is cited in every funding request and in every defense of the current program. They hold no seat in scenario design or program evaluation, and several have begun asking publicly why the drills their relatives' deaths paid for never seem to change anything.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, past_disaster_survivor_families, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns agency calendars around recurring shared occasions; satisfies the statutory exercise mandate and the insurer's and mutual-aid partners' documentation requirements; provides the annual ritual that carries organizational identity across leadership turnover; produces the certificate trail that ministries and courts accept as evidence of diligence.
% TRANSFER_FUNCTION: Moves several hundred crew-hours per unit per year from operational availability into scripted performance; moves exercise-budget funds outward to facilitation and template vendors; moves assurance artifacts — completion certificates, after-action reports — upward to ministries, auditors, and insurers; moves reassurance outward to the public.
% ABSENT_VOICES: Past-disaster survivor families attend the ceremonies but hold no seat in scenario design or evaluation and have started asking publicly what the drills changed; independent resilience researchers running comparative no-notice studies sit outside the program's information loop; junior crew members have no channel to propose scenario reform except up a chain of command staffed by the form's custodians.
% DISAPPEARANCE_RATIONALE: Grant disbursements, insurance premium terms, mutual-aid accreditations, and ministry reporting lines all reference the program's artifacts; overnight removal would break accreditation chains within a quarter, orphan the commemorative calendar that anchors the institutional year, and force the agency either to rebuild a program quickly or to defend an undocumented readiness posture before the next audit. Daily operations would not halt — trucks roll, crews respond — but the entire assurance economy wrapped around the service would have to reorganize immediately.
% FOUNDING_PROBLEM: After the founding flood, the service faced the imminent retirement of the entire veteran cohort that had commanded the live response; the program was built to transmit that cohort's operational judgment to successors through recurring rehearsed scenarios before its carriers aged out.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the program's own offices: two post-event public inquiries found units fully certified under the program yet unable to execute basic adaptive tasks on first contact with the real event; the national audit office's methodology reviews concluded the exercise instruments measure documentation produced, not capability demonstrated; survivor-family associations have testified to legislative committees that commemoration has outgrown rehearsal. The directorate attests the founding mission is honored; no seat outside the benefiting-side bureaucracy attests that it is still being performed.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).
:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.42 decomposes into three components: crew-hours consumed by scenarios that produce no transferable capability (the dominant term), budget diverted into documentation production that satisfies auditors rather than responders, and the false-confidence externality pushed onto at-risk communities. It is far below snare-grade because no seat captures the yield — the value dissipates as spent time and misplaced trust. Suppression 0.40 is a blend, estimated roughly 60 percent internalized (custodial identity, loyalty to the founding dead, the shame economics of admitting the drills teach nothing) and 40 percent structural (ministry reporting requirements, insurer certification gates, grant conditions tied to exercise completion); omega suppression_mechanism_split carries the split as an open question rather than resolving it by fiat. Theater ratio 0.78 is the reading's defining signature: of scheduled preparedness activity, roughly three-quarters is commemorative or documentation-serving rather than capability-building. Accessibility collapse is LOW (0.30) — and that low value is diagnostic, not conciliatory: adaptive no-notice exercise formats are doctrinally permitted, visibly practiced by peer agencies, and affordable, so alternatives do not disappear when the husk is understood; they remain in plain sight, unadopted, which is what makes the arrangement legible as inertia rather than necessity. Resistance 0.20 reflects the harm's latency: between events nobody is hurt enough to organize, grumbling stays private, whistleblower memos surface sporadically, and survivor-family questions are absorbed as ceremony logistics. The temporal series run on ONE shared nine-point grid (t=0,3,...,24) with all three metrics authored at every point. The suppression_requirement series is authored because this story specifically traces an enforcement-capacity dynamic: the documentation ratchet (completion certificates, after-action filing requirements) built up through mid-interval and then saturated — enforcement OF THE FORM matured and plateaued, while enforcement of any function remained absent throughout. Theater and extractiveness rise monotonically (Goodhart drift: the audit instrument counts documents, so document production crowds out capability); the anniversary-cycle pulsation in drill intensity is seasonal noise superimposed on the trend, not the operating mechanism, and is deliberately not modeled as oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different classifications from identical structural data. From the directorate's chair the program is faithful stewardship: mandates met, insurers satisfied, the founding promise kept — low perceived extraction, coordination-framed. From the veteran instructors' chair the same routines are fiduciary duty and inheritance, and because their exit is identity_locked the derivation pushes them toward the target end even though their subjective experience is custodial pride rather than victimhood — the sharpest perception/reality gap in the story. Frontline crews experience the hours as dead time but lack both the standing and the vocabulary to name it. At-risk communities compute NEGATIVE effective extraction — the program subsidizes them with confidence — until first contact with a novel event flips them retroactively to full targets; the subsidy is real ex ante and fraudulent ex post, and the engine can only price the ex ante seat. Oversight committees see a healthy compliance picture because their instruments were designed by the same logic they audit. The engine computes this divergence from power, exit, and role data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim-declared groups (veteran_instructors, frontline_response_units, recruit_cohorts, at_risk_communities) derive high directionality toward the target end; among them, identity_locked exit pushes the instructors nearest full-target despite their self-understanding, and trapped exit pins the communities there permanently. The exercise_design_vendors hold beneficiary role with arbitrage exit, deriving near-beneficiary directionality — but their relationship is orthogonal rather than parasitic: they sell documentation compliance to whichever philosophy governs the program and would sell more under a competence regime, so they collect no rent specific to the husk form; this is why no directionality override is declared for them and why gain_flow is not pointed at their seat. The directorate, agenda_setter with a secondary payer position (its own staff sit inside the drills), sits mid-range — it administers the arrangement, bears some of its costs, and collects assurance rather than money. Legislative oversight holds the analytical seat with no chi stake. Past-disaster survivor families are excluded: they feed the ritual's emotional capital but appear in no derivation, which absence is itself part of the structure the story documents. The structural derivation from beneficiary/victim declarations plus exit options suffices throughout; no explicit overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting the founding generation's operational judgment before that cohort retired — was live at t=0 and is dead as a function now: the carriers are gone, and what the program transmits is the memory of having transmitted. The R5 mismatch consumer reads founding_problem_status=dead against disappearance_verdict=world_rearranges and flags a zombie constraint; that flag firing is the CORRECT result for this story and was not tuned away — the compliance economy genuinely depends on the husk's continued existence even though its function is gone. The classification prevents two mislabels. It is not a snare: nobody captures the extraction, exits are not suppressed for anyone's gain, and the arrangement would not survive an attempt to make enforcement primary (enforcement here is auxiliary scaffolding on the documentation spine only — the anniversary rites need no enforcement at all, as anniversaries never do). It is not a rope: the residual coordination the form still performs (calendar alignment, inter-agency acquaintance) is too thin to ground a coordination claim once the competence core is gone. It is a piton by the cost-asymmetry test: the directorate COULD rebuild the program as adaptive no-notice training, but the certain near-term costs (publicly exposing current incapacity during the transition, failing audits mid-conversion, fighting budget lines defined by deliverable documents, shattering the custodial identity that holds the senior ranks together) exceed the diffuse, probabilistic, unbudgeted future benefit; meanwhile no payer is hurt often enough between events to force repair. The mandate outlived the function and the form outlived the mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates the husk_reading of the preparedness_commitment kernel; would the competence_reading or hybrid_reading of the same standing drill program classify differently, and where exactly is the disagreement located?',
    'Cross-reading comparison over the same referent: instrumented capability assessment of the identical drill program evaluated under each sibling reading''s premises. The disagreement is located in a single structural element — whether the transmitted drill-form retains causal coupling to operational capability.',
    'Under the competence_reading the same routines would author low epsilon and likely compute as rope; under the hybrid_reading epsilon would be intermediate and the profile possibly tangled_rope-shaped; this file''s piton-flavored profile is conditional on the husk premise holding. The readings are separate constraint files, linked via network edges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one kernel, three readings, three constraints; this story''s values are valid only under the husk premise.').

omega_variable(
    form_function_coupling_latency,
    'Does the scripted drill form retain any latent capability transfer — decision sequencing under load, inter-agency name-recognition, radio discipline — that would activate under novel stress, or is the decoupling total?',
    'No-notice instrumented exercises using novel scenarios, comparing decision latency and error rates against peer agencies running adaptive, unscripted programs.',
    'Partial coupling would move the story toward the hybrid reading''s structure and reduce effective theater; total decoupling confirms the husk profile and predicts severe first-contact failure under novel stress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_coupling_latency, empirical, 'Residual coupling between memorial drill-form and operational capability.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression (0.40) carried by structural instruments (ministry reporting requirements, insurer certification gates, grant conditions) or by internalized commitments (custodial identity, loyalty to the founding dead)?',
    'Retirement-curve and exit-interview analysis: if criticism of the program surfaces mainly after members leave the service, the binding is predominantly internalized; if serving members criticize openly but budgets and audit dependencies bind action, it is structural.',
    'If predominantly internalized, dismantling the audit instruments alone would not release the ritual — the constraint travels with its holders — and effective suppression exceeds the structural measure; reform requires narrative and identity work, not just instrument redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized composition of the suppression holding the husk in place.').

omega_variable(
    false_confidence_discount,
    'How much household-level protective behavior in the served communities is displaced by the documented-preparedness assurance the program publishes?',
    'Survey of preparedness behavior against actuarial exposure, comparing households inside versus outside the program''s publicity footprint.',
    'A large displacement effect raises effective extraction borne by communities above the authored base value and sharpens expected first-contact harm; a negligible effect leaves the reassurance transfer benign in welfare terms even while wasteful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_confidence_discount, empirical, 'Magnitude of the false-confidence externality on at-risk communities.').

omega_variable(
    memorial_value_legitimacy,
    'Does the commemorative function produce intrinsic goods — grief integration for survivor families, recruitment cohesion, public solidarity — that partially legitimate the resource spend independently of any capability outcome?',
    'Preference elicitation across stakeholder seats including survivor families and recruits, plus comparative study of agencies that deliberately separated commemoration from training.',
    'If commemorative goods are substantial, part of the measured waste is payment for a different good and effective extractiveness falls, lending support to the hybrid reading''s stabilizer claim; if negligible, the husk reading''s epsilon stands unreduced and the entire spend is deadweight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_value_legitimacy, preference, 'Whether the memorial layer carries standalone social value that offsets measured waste.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prep_tr_t3, preparedness_commitment__husk_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(prep_tr_t6, preparedness_commitment__husk_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement(prep_tr_t9, preparedness_commitment__husk_reading, theater_ratio, 9, 0.42).
narrative_ontology:measurement(prep_tr_t12, preparedness_commitment__husk_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__husk_reading, theater_ratio, 15, 0.63).
narrative_ontology:measurement(prep_tr_t18, preparedness_commitment__husk_reading, theater_ratio, 18, 0.7).
narrative_ontology:measurement(prep_tr_t21, preparedness_commitment__husk_reading, theater_ratio, 21, 0.75).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__husk_reading, theater_ratio, 24, 0.78).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prep_be_t3, preparedness_commitment__husk_reading, base_extractiveness, 3, 0.2).
narrative_ontology:measurement(prep_be_t6, preparedness_commitment__husk_reading, base_extractiveness, 6, 0.24).
narrative_ontology:measurement(prep_be_t9, preparedness_commitment__husk_reading, base_extractiveness, 9, 0.28).
narrative_ontology:measurement(prep_be_t12, preparedness_commitment__husk_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__husk_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(prep_be_t18, preparedness_commitment__husk_reading, base_extractiveness, 18, 0.38).
narrative_ontology:measurement(prep_be_t21, preparedness_commitment__husk_reading, base_extractiveness, 21, 0.4).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__husk_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__husk_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(prep_su_t3, preparedness_commitment__husk_reading, suppression_requirement, 3, 0.14).
narrative_ontology:measurement(prep_su_t6, preparedness_commitment__husk_reading, suppression_requirement, 6, 0.2).
narrative_ontology:measurement(prep_su_t9, preparedness_commitment__husk_reading, suppression_requirement, 9, 0.26).
narrative_ontology:measurement(prep_su_t12, preparedness_commitment__husk_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__husk_reading, suppression_requirement, 15, 0.33).
narrative_ontology:measurement(prep_su_t18, preparedness_commitment__husk_reading, suppression_requirement, 18, 0.36).
narrative_ontology:measurement(prep_su_t21, preparedness_commitment__husk_reading, suppression_requirement, 21, 0.38).
narrative_ontology:measurement(prep_su_t24, preparedness_commitment__husk_reading, suppression_requirement, 24, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'preparedness' decomposes under the epsilon-invariance principle into three readings of the single kernel preparedness_commitment, each a separate file with its own epsilon, beneficiary/victim structure, and classification over the SAME standing drill-program arrangement. competence_reading is the historically upstream claim (its assertions justified the program's funding and still anchor its self-description); this husk_reading is downstream, its evidence base accumulating through post-event public inquiries and audit-methodology reviews; hybrid_reading mediates, borrowing the husk's diagnosis of the memorial layer while contesting the totality of the decoupling. Edges are declared in all three files; the upstream-downstream citation gradient runs competence_reading -> husk_reading, since the competence claim's public credibility is precisely what the husk diagnosis erodes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
