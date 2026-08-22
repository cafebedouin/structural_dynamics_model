% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Conditional Humane Treatment Standard (Contextual Necessity Reading)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This story authors the contextual_necessity arrangement itself: the
 *   Common Article 3 humane-treatment floor stands, and a necessity channel
 *   beside it permits enhanced interrogation of detainees classified as
 *   security-imperative cases, with the content of 'humane treatment' fixed
 *   operationally by the agencies that run custody. Protection is therefore
 *   conditional — unconditional for detainees outside the channel, suspended
 *   for those inside it. The claim and the metrics are independent authored
 *   facts: the reading CLAIMS tangled_rope because it genuinely retains a
 *   working protective floor for the detainee majority while the same
 *   authorization structure strips protection from a classified subset —
 *   coordination and imposed cost ride one structure, actively enforced
 *   through legal memoranda, classification decisions, and closed oversight.
 *   The metrics describe the arrangement's actual operation as this reading
 *   appraises it: mid-range base extraction (the floor does real protective
 *   work; the channel concentrates severe imposition), high suppression
 *   (detainees cannot exit or contest classification), and a theater share
 *   that has grown as oversight activity increasingly documents proper
 *   authorization rather than changing outcomes.
 *
 * KEY AGENTS:
 *   - intelligence_security_agencies: agenda-setting collector (institutional/arbitrage) — defines 'humane' operationally, runs the necessity channel, relocates programs under legal pressure
 *   - national_executives: beneficiary with authorization duty (institutional/mobile) — collects policy flexibility and security credit, bears episodic political cost
 *   - high_value_detainees: primary target (powerless/trapped) — classification suspends their baseline protections for the duration of custody
 *   - ordinary_conflict_detainees: dual-positioned (powerless/trapped) — receives the floor's visible protection, carries reclassification exposure
 *   - humanitarian_monitoring_bodies: observer (organized/analytical) — confidential custody access, limited reach into the classified channel
 *   - independent_medical_professionals: excluded voice (organized/constrained) — ethical objections kept outside technique design and approval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.44).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.66).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.44).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Conditional Humane Treatment Standard (Contextual Necessity Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'fbb83d9f-061f-4b33-be30-690d26ae325a').
narrative_ontology:cs_kernel_codification('fbb83d9f-061f-4b33-be30-690d26ae325a', fixed_text).
narrative_ontology:cs_authority_grounding('fbb83d9f-061f-4b33-be30-690d26ae325a', lineage).
narrative_ontology:cs_interpretation_layer_present('fbb83d9f-061f-4b33-be30-690d26ae325a').
narrative_ontology:cs_reading_relation('fbb83d9f-061f-4b33-be30-690d26ae325a', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('fbb83d9f-061f-4b33-be30-690d26ae325a', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('fbb83d9f-061f-4b33-be30-690d26ae325a', foundational, security_necessity_overrides_humane_floor).
narrative_ontology:cs_axiom_status(security_necessity_overrides_humane_floor, holdable).
narrative_ontology:cs_axiom_grounding('fbb83d9f-061f-4b33-be30-690d26ae325a', security_necessity_overrides_humane_floor, instrumental).
narrative_ontology:cs_axiom('fbb83d9f-061f-4b33-be30-690d26ae325a', foundational, humane_content_fixed_by_competent_authority).
narrative_ontology:cs_axiom_status(humane_content_fixed_by_competent_authority, holdable).
narrative_ontology:cs_axiom_grounding('fbb83d9f-061f-4b33-be30-690d26ae325a', humane_content_fixed_by_competent_authority, conventional).
narrative_ontology:cs_reference_frame('fbb83d9f-061f-4b33-be30-690d26ae325a', baseline_floor_with_bounded_necessity_channel).
narrative_ontology:cs_drift_state('fbb83d9f-061f-4b33-be30-690d26ae325a', post_disclosure_oversight_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fbb83d9f-061f-4b33-be30-690d26ae325a', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, intelligence_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_executives).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, ordinary_conflict_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, high_value_detainees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, ordinary_conflict_detainees).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, military_necessity_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, executive_interpretive_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the interrogation programs and make the classification decisions that route detainees into the necessity channel. Draft the operational definitions of 'humane treatment' that govern their own custody conduct, subject to executive and occasionally judicial sign-off. When domestic legal pressure rises they can relocate facilities, re-badge programs, or rebuild oversight relationships in other jurisdictions rather than abandon the practices. They collect the intelligence product and the operational latitude the channel produces.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, intelligence_security_agencies, agenda_setter,
    institutional, biographical, arbitrage, global).

% Sign the authorizations and legal opinions that open the necessity channel, and collect the political credit for security results while delegating operational detail to the agencies. They bear episodic political cost when programs surface publicly. They can close the channel by directive — at least one administration has done so — though successors may reopen it, so their exit from the arrangement is real but reversible.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_executives, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, national_executives, agenda_setter).

% Classified as security-necessity cases and routed into the exception channel. Their baseline protections are suspended for as long as their classification holds; they cannot contest the classification in most frameworks, cannot leave custody, and frequently cannot reach counsel or monitors. They bear the full physical and psychological cost that the channel's authorized techniques impose, for the duration of detention.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, high_value_detainees, payer,
    powerless, immediate, trapped, global).

% Remain under the Common Article 3 floor — humane conditions, forbearance from violence and humiliation, ordinary trial guarantees — and are the population the floor visibly protects day to day. Their protection is conditional on never being reclassified into the necessity channel; the possibility of reclassification prices their cooperation and compliance throughout custody.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, ordinary_conflict_detainees, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, ordinary_conflict_detainees, payer).

% Visit places of detention under confidentiality agreements, interview detainees without witnesses, and report privately to detaining authorities. Their access to the necessity channel is partial or denied where programs are classified, so their knowledge of the channel's interior is limited to what detainees tell them and what authorities disclose. Their leverage rests on persistent presence and private remonstration rather than public findings.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, humanitarian_monitoring_bodies, observer,
    organized, generational, analytical, global).

% Professional bodies hold ethical rules barring members from designing or calibrating coercive interrogation. Program design proceeded without them; the few clinicians involved served under directives that subordinated clinical judgment to operational purpose. Their objections arrive after the fact, in ethics journals and licensing debates, outside the rooms where techniques are approved.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, independent_medical_professionals, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, intelligence_security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a common floor of minimum treatment for detainees in non-international armed conflict — humane conditions, forbearance from violence and humiliation, basic judicial guarantees — giving all parties a shared, predictable standard. The necessity channel extends the coordination by giving security agencies a sanctioned, reviewable route for exceptional interrogations instead of leaving them to operate wholly outside the legal frame.
% TRANSFER_FUNCTION: Moves bodily security, dignity, and procedural protection away from detainees classified into the necessity channel and toward state security agencies as operational latitude and intelligence product; moves assured baseline treatment to detainees who remain outside the channel.
% ABSENT_VOICES: Detainees routed into the necessity channel are absent from every forum where 'humane' is defined — classification decisions, legal memoranda, and closed oversight sessions proceed without them. Their experience enters only through counsel where access is granted, monitor interviews under confidentiality, or post-hoc litigation. Independent medical ethicists were likewise outside program design; their objections surface in professional literature after techniques are already authorized.
% DISAPPEARANCE_RATIONALE: If the conditional regime vanished overnight, the surrounding arrangements reorganize immediately: either the absolute floor fills the space and every detainee's protection becomes unconditional, closing the agencies' exception channel and forcing interrogation practice onto rapport-based methods, or the agencies continue the practices extralegally and the entire authorization-and-oversight apparatus built around the channel loses its object. Detention policy, interrogation training pipelines, and pending litigation all rearrange around whichever replacement obtains.
% FOUNDING_PROBLEM: Common Article 3 was drafted to stop civil-war atrocities: in non-international conflicts, captured persons fell outside prisoner-of-war protections and faced summary execution and torture as default practice. The contextual reading's specific founding problem arrived later — reconciling that absolute-seeming floor with states' asserted need to coerce intelligence from irregular adversaries who embed in civilian populations and direct attacks from custody.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: ICRC custody-visit reporting and UN special-procedure findings attest the floor's continuing necessity while disputing that any security imperative requires suspending it; the declassified legislative committee study of the detention and interrogation program — produced by overseers, not the operating agencies — found the coercive program's intelligence yield largely redundant with prior reporting, cutting against the necessity premise; veteran military interrogator associations attest that rapport-based methods sufficed in their operational record. No corroborating source outside the beneficiary set attests that the override channel is required.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).
:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.44, the terminal value of the shared measurement grid: the protective floor performs real coordination work for the detainee majority, while the necessity channel concentrates suspension of protection on a classified minority — from this reading's own appraisal the channel is priced necessity rather than rent, which holds epsilon mid-range rather than high. Suppression (0.66) is a raw structural property, unscaled by power or scope: detainees inside the channel have no exit, no classification contest, and often no counsel; challenge is absorbed by secrecy doctrines rather than answered. Theater_ratio (0.40) reflects oversight whose output is authorization paperwork — reviews that certify process rather than alter technique. Accessibility_collapse (0.58): for a detainee, alternatives collapse almost completely once classification lands; for states, the rival legal position remains available, keeping collapse below natural-law range. Resistance (0.65) is sustained: litigation, treaty-body findings, and professional-refusal campaigns. The three temporal series share one seven-point grid (T0-T24). Base_extractiveness traces crisis expansion, partial rollback, and a settled plateau. Suppression_requirement is authored because this story specifically tracks enforcement-capacity change: rapid machinery build-up in the early interval, formal relaxation after disclosure, then hardening into legal insulation at a plateau above the origin. Theater_ratio climbs monotonically as documentation substitutes for protection. End-state scalars match the grid's terminal values.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setting seats compute different types from identical structure. From inside the agencies, the arrangement is a maintained floor plus a lawful relief valve they staffed, justified, and can defend provision by provision — coordination they operate. From inside the channel, the same structure is the suspension of protection itself: the floor is real precisely in excluding them, and its conditionality is experienced as abandonment with paperwork attached. Ordinary detainees occupy the hinge: they receive the floor's visible benefit while living under a classification regime that could reroute them, so their conduct in custody is priced by the channel's existence. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   intelligence_security_agencies sit nearest the beneficiary end: they collect the channel's product and latitude and hold arbitrage-grade exit — sites, programs, and legal theories can be relocated or rebuilt in other jurisdictions. national_executives collect policy flexibility and security credit but carry episodic political cost when programs surface, holding their derived directionality slightly off the pure-beneficiary pole. high_value_detainees sit at the full-target end: trapped, unable to contest classification, bearing everything the channel authorizes. ordinary_conflict_detainees derive mid-low: net recipients of the floor's protection, discounted by reclassification exposure, which is declared through their secondary payer role rather than a directionality override (an override keyed to their power atom would also capture the high-value detainees, so the dual-role declaration is the correct instrument). humanitarian_monitoring_bodies and independent_medical_professionals hold observational and excluded positions; neither collects from the channel, and the medical profession's exclusion is what keeps technique design insulated from external ethical review.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what keeps both mislabels from landing. Reading the arrangement as pure coordination erases the categorical abandonment embedded in the channel — a floor that suspends itself for the people most at risk is not costless coordination. Reading it as pure extraction erases the daily protective work the floor performs for the detainee majority, work with an observable record in custody conditions outside the channel. The genealogy interview locates the residual mandate: the founding problem of the floor (civil-war minimums) is live and corroborated from outside the beneficiary set; the founding problem of the channel (coercion yields necessary intelligence) is contested, with the strongest external evidence currently running against it. That asymmetry — live floor, contested channel — is the mandatrophy-relevant fact this story hands the engine, and it explains why the arrangement persists by enforcement and reinterpretation rather than by settled function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the contextual_necessity reading of the humane_treatment_standard kernel; which reading governs the standing arrangement, and what would each sibling change structurally?',
    'Doctrinal settlement: treaty-body general comments, domestic apex-court holdings, or controlling executive doctrine adopting one reading as authoritative.',
    'Under the absolute_prohibition sibling the necessity channel closes and the victim set empties, dropping the protection arrangement''s extraction toward the coordination floor; under the proportionality_balancing sibling the categorical exclusion dissolves into case-by-case weighing, making the victim set indeterminate and shifting enforcement into adjudication.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel membership: this story is one of three readings of the humane-treatment kernel; sibling adoption restructures victim set and enforcement.').

omega_variable(
    derogability_disagreement_location,
    'Where in the kernel''s structure do the readings disagree — is the dispute located in derogability (whether the floor admits any override at all) or in definitional authority (who fixes the content of ''humane'')?',
    'Drafting-history analysis of the 1949 Diplomatic Conference together with the logical form of each sibling''s axioms: the absolute and contextual readings contradict directly on derogability; the contextual and proportionality readings diverge on whether the trigger is categorical or weighed.',
    'Locating the dispute in derogability frames reform as binary (channel open or closed) and drives the foreclosure computation between this reading and the absolute sibling; locating it in definitional authority frames reform as institutional (who decides), producing different remedial structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derogability_disagreement_location, conceptual, 'Structural location of the inter-reading disagreement within the kernel.').

omega_variable(
    necessity_efficacy_premise,
    'Does coercive interrogation inside the necessity channel actually produce actionable intelligence unobtainable through rapport-based methods — the empirical premise this reading''s instrumental grounding rests on?',
    'Declassified program assessments, cross-program outcome audits, and interrogation-science literature comparing intelligence yield per technique class.',
    'If the efficacy premise fails, the foundational axiom loses its instrumental ground and the reading collapses toward its siblings; if it holds, the override claim gains force and the channel''s measured imposition reads as priced coordination rather than rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_efficacy_premise, empirical, 'Empirical ground of the necessity override claim.').

omega_variable(
    definitional_discretion_trajectory,
    'Is the agencies'' discretion to define ''humane'' exercised within the necessity bounds this reading declares, or has the exception channel expanded past its stated trigger?',
    'Longitudinal comparison of authorized-technique lists and classification criteria against published necessity thresholds; audit of the share of channel admissions that meet the stated trigger.',
    'Channel expansion beyond stated triggers converts the carve-out from bounded necessity into open-ended imposition, pushing the arrangement from tangled_rope toward snare; disciplined exercise keeps the protective floor dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_discretion_trajectory, empirical, 'Whether the necessity channel stays within its declared bounds over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t4, humane_treatment_standard__contextual_necessity, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(huma_tr_t4, observed).
narrative_ontology:measurement(huma_tr_t8, humane_treatment_standard__contextual_necessity, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(huma_tr_t8, observed).
narrative_ontology:measurement(huma_tr_t12, humane_treatment_standard__contextual_necessity, theater_ratio, 12, 0.34).
narrative_ontology:measurement_basis(huma_tr_t12, observed).
narrative_ontology:measurement(huma_tr_t16, humane_treatment_standard__contextual_necessity, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(huma_tr_t16, observed).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(huma_tr_t20, observed).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__contextual_necessity, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(huma_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t4, humane_treatment_standard__contextual_necessity, base_extractiveness, 4, 0.38).
narrative_ontology:measurement_basis(huma_be_t4, observed).
narrative_ontology:measurement(huma_be_t8, humane_treatment_standard__contextual_necessity, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(huma_be_t8, observed).
narrative_ontology:measurement(huma_be_t12, humane_treatment_standard__contextual_necessity, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(huma_be_t12, observed).
narrative_ontology:measurement(huma_be_t16, humane_treatment_standard__contextual_necessity, base_extractiveness, 16, 0.47).
narrative_ontology:measurement_basis(huma_be_t16, observed).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(huma_be_t20, observed).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__contextual_necessity, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(huma_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t4, humane_treatment_standard__contextual_necessity, suppression_requirement, 4, 0.6).
narrative_ontology:measurement_basis(huma_su_t4, observed).
narrative_ontology:measurement(huma_su_t8, humane_treatment_standard__contextual_necessity, suppression_requirement, 8, 0.72).
narrative_ontology:measurement_basis(huma_su_t8, observed).
narrative_ontology:measurement(huma_su_t12, humane_treatment_standard__contextual_necessity, suppression_requirement, 12, 0.66).
narrative_ontology:measurement_basis(huma_su_t12, observed).
narrative_ontology:measurement(huma_su_t16, humane_treatment_standard__contextual_necessity, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(huma_su_t16, observed).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(huma_su_t20, observed).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__contextual_necessity, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(huma_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Common Article 3 humane treatment' covers three structurally distinct claims, authored as separate stories per the epsilon-invariance principle. This story (contextual_necessity) authors the conditional arrangement: a real floor plus a necessity channel, with its own epsilon, victim set (high_value_detainees), and enforcement machinery. The absolute_prohibition sibling authors the non-derogable-floor arrangement (empty victim set, negligible extraction); the proportionality_balancing sibling authors the weighed arrangement (indeterminate victim set, adjudication-centered enforcement). The upstream story is absolute_prohibition — the drafting-era textual consensus — which the downstream readings each cite or depart from; this reading influences the proportionality sibling by supplying the categorical-trigger position that balancing frameworks define themselves against. Each file links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
