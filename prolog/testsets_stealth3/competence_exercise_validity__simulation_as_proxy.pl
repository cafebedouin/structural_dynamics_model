% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Scheduled Simulation as Valid Competence Exercise (Drills as Proxy-Catastrophe)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Safety-critical industries (nuclear, aviation, chemical process,
 *   emergency response) operate under an arrangement in which periodically
 *   completing scheduled simulations, drills, and tabletop exercises counts
 *   as valid exercise of rare-event competence, with the resulting drill
 *   record and safety statistics treated as proof that competence remains
 *   occupied and that regulatory compliance establishes adequacy. The
 *   arrangement solves a real problem — events too rare or dangerous to
 *   experience cannot be rehearsed by waiting for them — and simultaneously
 *   generates a compliance economy: scenario packages, evaluator
 *   accreditation, audit-ready completion logs. Over the interval the
 *   exercise function has persisted while a growing share of activity has
 *   shifted to defending the metric: scenario selection that avoids failure
 *   modes, scoring conventions that certify success, after-action
 *   boilerplate. KEY AGENTS (by structural relationship): regulated_operators
 *   (agenda setter, institutional/constrained), safety_regulators (co-agenda
 *   setter, institutional/constrained), frontline_operators (primary bearer
 *   of readiness risk, moderate/identity_locked), downstream_public (diffuse
 *   bearer of residual event risk, powerless/trapped), simulation_vendors
 *   (beneficiary, organized/mobile), independent_safety_engineers (excluded
 *   critic, moderate/mobile), accident_investigation_bodies (analytical
 *   observer, institutional/analytical). This is one reading of the
 *   competence_exercise_validity kernel (see kernel_context); the sibling
 *   stories real_catastrophe_only and continuous_refresh_hybrid author the
 *   same standing arrangement with different epsilon and victim structure —
 *   this reading grants the coordination credit the catastrophe-only reading
 *   withholds and asserts the sufficiency the hybrid withholds, yielding a
 *   mid-range epsilon rather than the siblings' poles.
 *
 * KEY AGENTS:
 *   - - regulated_operators: Agenda setter (institutional/constrained) — designs, schedules, and certifies the exercise record; the completed drill log is the compliance artifact, and the compliance surplus (avoiding costlier continuous or unannounced programs) accrues here
 *   - - safety_regulators: Co-agenda setter (institutional/constrained) — mandates exercise frequency and documentation, collects the legibility dividend of a uniform audit surface
 *   - - frontline_operators: Primary target (moderate/identity_locked) — certification-linked careers fuse professional standing with drill passage; readiness risk concentrates where scripted rehearsal replaces varied experience
 *   - - downstream_public: Diffuse target (powerless/trapped) — bears the consequences of events that exceed rehearsed envelopes; no seat in scenario design
 *   - - simulation_vendors: Beneficiary (organized/mobile) — revenue scales with mandated exercise volume across industries; mobile across sectors
 *   - - independent_safety_engineers: Excluded voice (moderate/mobile) — publishes on skill decay and fidelity limits from outside the exercise-design and accreditation rooms
 *   - - accident_investigation_bodies: Analytical observer (institutional/analytical) — reconstructs real events and compares the drill record against what actually failed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.48).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.42).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.48).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Scheduled Simulation as Valid Competence Exercise (Drills as Proxy-Catastrophe)").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, 'd0d1196d-3909-4392-aa33-2dd69e75ad6d').
narrative_ontology:cs_kernel_codification('d0d1196d-3909-4392-aa33-2dd69e75ad6d', formalized).
narrative_ontology:cs_authority_grounding('d0d1196d-3909-4392-aa33-2dd69e75ad6d', expertise).
narrative_ontology:cs_interpretation_layer_present('d0d1196d-3909-4392-aa33-2dd69e75ad6d').
narrative_ontology:cs_reading_relation('d0d1196d-3909-4392-aa33-2dd69e75ad6d', competence_exercise_validity__real_catastrophe_only, forecloses).
narrative_ontology:cs_reading_relation('d0d1196d-3909-4392-aa33-2dd69e75ad6d', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_axiom('d0d1196d-3909-4392-aa33-2dd69e75ad6d', foundational, simulated_exercise_satisfies_retention_requirement).
narrative_ontology:cs_axiom_status(simulated_exercise_satisfies_retention_requirement, holdable).
narrative_ontology:cs_axiom_grounding('d0d1196d-3909-4392-aa33-2dd69e75ad6d', simulated_exercise_satisfies_retention_requirement, empirically_contingent).
narrative_ontology:cs_axiom('d0d1196d-3909-4392-aa33-2dd69e75ad6d', secondary, safety_record_validates_competence_adequacy).
narrative_ontology:cs_axiom_status(safety_record_validates_competence_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('d0d1196d-3909-4392-aa33-2dd69e75ad6d', safety_record_validates_competence_adequacy, empirically_contingent).
narrative_ontology:cs_reference_frame('d0d1196d-3909-4392-aa33-2dd69e75ad6d', scheduled_simulation_validity_baseline).
narrative_ontology:cs_drift_state('d0d1196d-3909-4392-aa33-2dd69e75ad6d', contemporary_skill_decay_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d0d1196d-3909-4392-aa33-2dd69e75ad6d', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulated_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, simulation_vendors).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, downstream_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, safety_regulators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the facility portfolio. Designs and schedules exercises within the regulator's envelope, selects scenarios, appoints evaluators, and files completion records. The completed drill log is the artifact presented at audits, and building it costs far less than running unannounced or continuously refreshed programs would. Operating licenses condition on documented exercise history, so leaving the regime is unavailable; staying minimally compliant is a managed choice. The savings from meeting the mandate at minimum viable rigor accumulate here.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, regulated_operators, agenda_setter,
    institutional, biographical, constrained, national).

% Writes the exercise-frequency and documentation requirements, accredits evaluators, and audits the records. The drill log provides a uniform, inspectable compliance surface across thousands of licensees — legibility that would be expensive to obtain any other way. The mandate comes from statute; tightening or relaxing the exercise standard is politically visible and slow.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, safety_regulators, beneficiary).

% Sells scenario libraries, simulators, evaluator training, and audit-preparation services across industries. Revenue scales with the number and frequency of mandated exercises. Customers are replaceable across regulated sectors, so a shift in validity standards would redirect product lines rather than end the business.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Staffs the control rooms, flight decks, and response teams. Maintains certification by passing scheduled exercises; pay progression and advancement track drill performance. Professional standing is built on the certification record, and voicing doubt about whether the exercises measure real readiness reads as questioning one's own qualification. Opting out ends employment; the difference between rehearsed scenarios and actual event demands lands on this seat first.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Lives and works within reach of the facilities and services whose operators drill. Bears the consequences if real events exceed rehearsed envelopes. Receives drill notices but never fidelity assessments; has no seat in scenario design and no way to relocate away from reliance on regulated infrastructure.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, downstream_public, payer,
    powerless, generational, trapped, regional).

% Human-factors researchers and veteran engineers who publish on skill decay and simulation fidelity limits. They argue for unannounced, adversarial, and continuously refreshed formats and for publishing exercise failure rates. They sit outside exercise-design committees and accreditation panels; their access runs through journals and post-incident testimony, years behind the decisions those venues critique.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, independent_safety_engineers, excluded,
    moderate, biographical, mobile, national).

% Reconstructs real events after the fact and compares the drill record against what actually failed. Sees the full arc — scenario selection, scoring conventions, the distance between passed objectives and event outcome — and feeds findings back into standards on a multi-year lag.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, accident_investigation_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__simulation_as_proxy, regulated_operators).
narrative_ontology:fixing_cost_class(competence_exercise_validity__simulation_as_proxy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Exercises readiness for events too rare, hazardous, or expensive to experience directly, and gives regulators, insurers, and the public a uniform, schedulable, inspectable readiness record that many organizations can produce simultaneously.
% TRANSFER_FUNCTION: Moves training budget and staff hours from operators into scheduled simulation infrastructure and vendor services; moves assurance and legibility up to regulators; leaves the distance between rehearsed and actual event demands — wherever fidelity falls short — resting on frontline staff and nearby publics.
% ABSENT_VOICES: Independent safety engineers and adversarial-training advocates stand outside the exercise-design and accreditation rooms; residents near facilities receive drill notices but never fidelity assessments; survivors of real events that outran rehearsed scenarios reach the table only years later, through investigation reports rather than scenario redesign.
% DISAPPEARANCE_RATIONALE: Overnight removal would force every licensee to rebuild its readiness program around some other validity standard — continuous refresh, unannounced adversarial exercises, or accumulated experience — vendors would lose the mandated-volume market, regulators would lose their uniform audit surface, and certification-linked careers would lose their currency. The compliance economy around exercise validity would regroup around a successor standard rather than dissolve.
% FOUNDING_PROBLEM: Rare severe events cannot be summoned for practice: before simulation-based exercise, industries had no way to rehearse or demonstrate readiness for catastrophes that must not happen, leaving both competence and compliance unverifiable between events.
% FOUNDING_PROBLEM_CORROBORATION: Professional engineering societies and accident-investigation archives corroborate that the founding problem is real: rare severe events cannot be produced on demand, and the pre-simulation record shows readiness failures no amount of intent prevented. Corroboration that the problem remains live — as distinct from solved — comes from outside the benefiting parties: published skill-decay research and investigation findings of events exceeding rehearsed envelopes. No party outside the regime attests that the current simulation-based answer closes the problem.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the arrangement demonstrably coordinates (it rehearses what cannot be experienced and produces a uniform readiness record) AND demonstrably carries enforced asymmetric burden (the gap between rehearsed and actual event demands rests on staff and publics, while the compliance surplus banks at the operator seat); this reading endorses the validity core without denying the asymmetry, so the claim is not tuned down to rope nor up to snare. Extractiveness 0.48: roughly half the arrangement's operating cost purchases genuine rehearsal value (which bounds epsilon well below snare range even from a sympathetic seat), while the remainder is compliance surplus and choreography margin. Suppression 0.42 is a raw, unscaled structural property: statutory mandate plus career dependence on the certification record suppresses internal contest; it is not amplified by power or scope. Theater_ratio 0.52 sits just past half — scenario curation and generous evaluation now defend the metric as much as they build readiness, marking Goodhart substitution onset. Accessibility_collapse 0.35: alternatives (unannounced, adversarial, continuously refreshed formats) remain lawful and are practiced by leading operators, so understanding the arrangement does not collapse exits. Resistance 0.45: steady scholarly critique and post-incident findings, short of organized refusal. The measurement series run on ONE shared eight-point grid (all three metrics authored at every time point); rising base_extractiveness models Goodhart accumulation; rising suppression_requirement models enforcement-capacity maturation — each major accident ratcheted mandate intensity and audit depth, a stepwise ratchet smoothed into trend rather than a cyclical oscillation.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the operator and regulator seats the arrangement is coordination they built and administer: a schedulable, auditable readiness record. From the frontline operator seat — identity_locked, certification-fused — the same structure operates as a script whose passage substitutes for varied experience, computing near the full-target end. The trapped, powerless public seat computes the heaviest effective burden: verification of readiness quality is hardest precisely where the consequence of error lands. The vendor seat, mobile across sectors, sits near the beneficiary end despite collecting fees. The engine derives these divergences from the structural data; the authored tangled_rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (regulated_operators, simulation_vendors) derive low directionality; victims (frontline_operators, downstream_public) derive high. Trapped and identity_locked exit positions push the two target seats toward the full-target end; the vendor's arbitrage-grade mobility damps its effective burden toward subsidy despite its revenue position. Receipt of gains is distinct from beneficiary role: fees flow transactionally to vendors, but the concentrated capture — the compliance surplus of meeting the mandate at minimum viable rigor instead of running costlier continuous programs — accrues at the regulated_operators seat, which is why gain_flow names it. National and regional scopes raise verification difficulty modestly, scaling effective extraction upward for the target seats; suppression, again, is unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   No obsolescence declaration: the founding problem (readiness for unexperienceable events) remains fully live, so the R5 mismatch consumer reads status=live x verdict=world_rearranges — no zombie flag. The classification earns its keep by keeping both sides of the ledger on one constraint: crediting the arrangement's real rehearsal function (preventing mislabel as pure extraction) while naming the enforced asymmetry (preventing mislabel as pure coordination). Watch item rather than resolution: with theater_ratio crossing 0.5, continued choreography drift could atrophy the exercise function until the arrangement persists as record-maintenance — at that point the piton signature (administrator-could-change-it, cost-asymmetry against fixing) would become the honest description, and this story's temporal series is structured to date such a transition if it occurs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_gap,
    'How far does competence exercised on rehearsed scenarios transfer to events outside the rehearsed envelope?',
    'Match drill-score histories against subsequent real-event performance across licensees; compare readiness measures from licensees using unannounced or adversarial formats against scheduled-format peers.',
    'A wide transfer gap voids the substitution premise, raises the burden borne on the public side, and pulls the arrangement toward the sibling readings'' territory; a narrow gap supports this reading''s validity claim and lowers the target-side burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_gap, empirical, 'Size of the transfer gap between drilled and unrehearsed event classes.').

omega_variable(
    drill_choreography_share,
    'What share of recorded exercise outcomes reflect genuine rehearsal versus scenario design and scoring practices that preclude failure?',
    'Audit scenario-selection archives and evaluator-score distributions; blind re-scoring of recorded exercises by external evaluators.',
    'A high choreographed share means theater_ratio is understated and the arrangement is drifting toward performative maintenance of a record rather than production of readiness; a low share confirms the exercise function remains primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_choreography_share, empirical, 'Fraction of drill success attributable to scenario design rather than operator readiness.').

omega_variable(
    kernel_reading_contest,
    'This story instantiates the simulation_as_proxy reading of the competence_exercise_validity kernel; would the sibling readings (real_catastrophe_only, continuous_refresh_hybrid) restructure who bears the arrangement''s costs and how heavily?',
    'Author the sibling stories against the same standing arrangement and compare their epsilon and victim structures; the disagreement is located at the substitution premise — whether simulated events can occupy the exercise role that real catastrophe would occupy.',
    'Under real_catastrophe_only the standing arrangement''s unvalidated-readiness deficit becomes the headline cost and the public-side burden rises sharply with no coordination credit for drills; under continuous_refresh_hybrid the arrangement becomes a transitional floor requiring continuous supplementation, recasting the burden as chronic under-training rather than acute mismatch. Either adoption forces re-authoring of epsilon and the victim set for this story''s subject matter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading among three of the exercise-validity kernel.').

omega_variable(
    identity_vs_structural_operator_lock,
    'Is frontline operators'' inability to contest the exercise standard structural (licensure and employment dependence) or internalized (professional identity fused with the certification record)?',
    'Post-exit survey of former operators: if readiness-doubt expression appears once employment consequences are removed, the lock was substantially internalized.',
    'An internalized lock raises the effective burden on the operator seat beyond what structural barriers alone predict, and predicts slow reform uptake even after exercise standards loosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_vs_structural_operator_lock, empirical, 'Mechanism of operator-side lock: career structure versus certification-fused identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cev_sim_proxy_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cev_sim_proxy_tr_t5, competence_exercise_validity__simulation_as_proxy, theater_ratio, 5, 0.16).
narrative_ontology:measurement(cev_sim_proxy_tr_t10, competence_exercise_validity__simulation_as_proxy, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cev_sim_proxy_tr_t15, competence_exercise_validity__simulation_as_proxy, theater_ratio, 15, 0.29).
narrative_ontology:measurement(cev_sim_proxy_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.36).
narrative_ontology:measurement(cev_sim_proxy_tr_t25, competence_exercise_validity__simulation_as_proxy, theater_ratio, 25, 0.42).
narrative_ontology:measurement(cev_sim_proxy_tr_t30, competence_exercise_validity__simulation_as_proxy, theater_ratio, 30, 0.48).
narrative_ontology:measurement(cev_sim_proxy_tr_t35, competence_exercise_validity__simulation_as_proxy, theater_ratio, 35, 0.52).

% Extraction over time
narrative_ontology:measurement(cev_sim_proxy_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(cev_sim_proxy_be_t5, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(cev_sim_proxy_be_t10, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(cev_sim_proxy_be_t15, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(cev_sim_proxy_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(cev_sim_proxy_be_t25, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(cev_sim_proxy_be_t30, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(cev_sim_proxy_be_t35, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 35, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cev_sim_proxy_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cev_sim_proxy_su_t5, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(cev_sim_proxy_su_t10, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(cev_sim_proxy_su_t15, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(cev_sim_proxy_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(cev_sim_proxy_su_t25, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 25, 0.39).
narrative_ontology:measurement(cev_sim_proxy_su_t30, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 30, 0.41).
narrative_ontology:measurement(cev_sim_proxy_su_t35, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 35, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'competence exercise' decomposes into three readings of one kernel, each a separate story with its own epsilon, beneficiaries, and victims. This story (simulation_as_proxy) sits between the siblings: it grants the coordination credit that real_catastrophe_only withholds and asserts the sufficiency that continuous_refresh_hybrid denies, producing a mid-range epsilon over the same standing arrangement. Edges mark family membership, not agreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
