% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 Absolute Prohibition Reading (Non-Derogable Humane Treatment Standard)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This story instantiates the absolute-prohibition reading of the Common
 *   Article 3 humane-treatment kernel: no circumstance — including claimed
 *   national-security necessity — permits crossing the threshold into torture
 *   or degrading treatment. Under this reading detainees are full
 *   rights-holders regardless of the security context of their capture, and
 *   state interrogation methods face a hard, non-derogable ceiling. This is
 *   one of three sibling readings of the same textual kernel
 *   (contextual_necessity, proportionality_balancing are separate constraint
 *   stories); this file authors ONLY the absolute reading's own structure, ε,
 *   and stakeholder set, per the ε-invariance discipline. Extraction here
 *   refers to the cost this reading's own operation imposes on security
 *   services and interrogation personnel who are absolutely barred from
 *   methods they believe would sometimes be effective, and on detainees held
 *   under states applying rival readings who receive no practical benefit
 *   from the standard's existence.
 *
 * KEY AGENTS:
 *   - detained_persons: primary beneficiary of the absolute bar (powerless/trapped) — the standard is the only thing between them and coercive treatment
 *   - intelligence_and_security_services: primary payer (institutional/constrained) — absolutely barred from methods regardless of assessed necessity
 *   - interrogation_personnel_under_prosecution_exposure: secondary payer (moderate/constrained) — bears personal criminal liability under command pressure
 *   - international_courts_and_monitoring_bodies: agenda-setter and analytical observer (institutional/analytical) — administers the boundary through jurisprudence
 *   - detainees_under_derogating_states: payer of the kernel's contestedness (powerless/trapped) — held under a rival reading, receives no practical benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.68).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.55).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 Absolute Prohibition Reading (Non-Derogable Humane Treatment Standard)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '1114ba33-efee-45a1-933a-87c7eef194cf').
narrative_ontology:cs_kernel_codification('1114ba33-efee-45a1-933a-87c7eef194cf', fixed_text).
narrative_ontology:cs_authority_grounding('1114ba33-efee-45a1-933a-87c7eef194cf', lineage).
narrative_ontology:cs_interpretation_layer_present('1114ba33-efee-45a1-933a-87c7eef194cf').
narrative_ontology:cs_reading_relation('1114ba33-efee-45a1-933a-87c7eef194cf', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('1114ba33-efee-45a1-933a-87c7eef194cf', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('1114ba33-efee-45a1-933a-87c7eef194cf', foundational, non_derogability_admits_no_security_exception).
narrative_ontology:cs_axiom_status(non_derogability_admits_no_security_exception, holdable).
narrative_ontology:cs_axiom_grounding('1114ba33-efee-45a1-933a-87c7eef194cf', non_derogability_admits_no_security_exception, deontological).
narrative_ontology:cs_axiom('1114ba33-efee-45a1-933a-87c7eef194cf', secondary, dignity_claim_independent_of_reciprocity).
narrative_ontology:cs_axiom_status(dignity_claim_independent_of_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('1114ba33-efee-45a1-933a-87c7eef194cf', dignity_claim_independent_of_reciprocity, deontological).
narrative_ontology:cs_reference_frame('1114ba33-efee-45a1-933a-87c7eef194cf', post_ww2_non_derogable_floor_consensus).
narrative_ontology:cs_drift_state('1114ba33-efee-45a1-933a-87c7eef194cf', post_9_11_enhanced_interrogation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1114ba33-efee-45a1-933a-87c7eef194cf', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detained_persons).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, future_captured_state_personnel).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, international_law_compliant_states).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, detainees_under_derogating_states).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, intelligence_and_security_services).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, interrogation_personnel_under_prosecution_exposure).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, human_dignity_as_non_negotiable_floor).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, reciprocity_based_law_of_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held by a detaining state or armed group with no ability to invoke rights directly; the absolute prohibition is the only thing standing between them and torture or degrading treatment during interrogation. They cannot exit the detention relationship and depend entirely on external enforcement (courts, monitors, reciprocal state practice) to make the standard real rather than aspirational.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detained_persons, beneficiary,
    powerless, immediate, trapped, national).

% Operate under the absolute bar even where they assess a detainee holds actionable, time-critical threat information. They experience the standard as removing a tool they believe would sometimes prevent mass casualties; their institutional performance metrics (intelligence yield, prevented attacks) are structurally decoupled from the prohibition's enforcement, and they cannot lawfully negotiate around it even in extremis.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, intelligence_and_security_services, payer,
    institutional, immediate, constrained, national).

% Individual soldiers, contractors, and officers who conduct interrogations bear personal criminal liability if the line is crossed, including under command pressure. They cannot exit the chain of command that may push toward the line, and the absolute standard gives them no discretion to plead necessity even where superiors claim security justification.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, interrogation_personnel_under_prosecution_exposure, payer,
    moderate, biographical, constrained, national).

% States that maintain compliant detention practices benefit from the reciprocal expectation that their own captured personnel will be treated humanely by adversaries, and from the diplomatic and coalition-legitimacy value of clean human-rights standing. They can shape enforcement mechanisms and treaty interpretation from a position of relative strength.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_law_compliant_states, beneficiary,
    institutional, generational, mobile, global).

% Soldiers and operatives who may themselves be captured in a future conflict; the absolute prohibition's persistence is the mechanism by which they hope to be protected if their own state ever loses local advantage. They have no present voice in the constraint's operation, only a structural stake in its survival.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, future_captured_state_personnel, beneficiary,
    powerless, civilizational, analytical, global).

% Held by states or armed actors that reject the absolute reading and apply contextual-necessity or proportionality frameworks instead; for them the prohibition exists on paper but is not the operative standard in the facility where they are actually held. They pay the cost of the kernel's contestedness directly, in treatment.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees_under_derogating_states, payer,
    powerless, immediate, trapped, national).

% Tribunals, the ICRC, and treaty bodies interpret and enforce the non-derogable standard, ruling on individual cases and state practice. They administer the boundary between lawful and unlawful treatment and can tighten or loosen its practical bite through jurisprudence, but cannot compel compliance from non-cooperative states beyond reputational and limited legal remedies.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_courts_and_monitoring_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Populations under active terror threat who might prefer their security services retain interrogation latitude are not parties to treaty interpretation; their preferences surface only indirectly through domestic political pressure on states to adopt contextual-necessity or proportionality readings instead.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, domestic_publics_facing_terror_threat, excluded,
    organized, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, diffuse).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, non-negotiable floor of treatment for anyone in custody during armed conflict, so that no party to a conflict needs to individually verify or negotiate treatment terms — the floor is fixed and universal, which lets adversaries extend minimal trust to each other's captured personnel without case-by-case bargaining.
% TRANSFER_FUNCTION: Moves discretion away from detaining-state security services and interrogators and toward detainees and international monitoring bodies: security services lose the option of coercive interrogation regardless of assessed necessity; detainees gain an unconditional (if unevenly enforced) claim against certain treatment; individual interrogators absorb personal legal risk that would otherwise sit with institutional policy.
% ABSENT_VOICES: Domestic publics facing acute terror threats, and the security officials who answer to them, are not parties to the treaty-interpretation process that fixes the absolute reading; their preference for contextual flexibility is expressed only through downstream political pressure to adopt the sibling readings, not within this reading's own framework.
% DISAPPEARANCE_RATIONALE: If the absolute-prohibition reading vanished as the operative interpretation, detaining states would have a live legal basis to authorize enhanced interrogation under claimed necessity; interrogation personnel would lose their strongest legal shield against command pressure to cross the line; and the reciprocal-protection logic that gives future captured state personnel a stake in the standard would collapse into case-by-case bargaining.
% FOUNDING_PROBLEM: Common Article 3 was drafted after WWII-era atrocities against prisoners and civilians in the hands of custodial powers, to establish a floor of treatment that would apply even in non-international armed conflicts where the fuller Geneva Convention protections did not reach, and to close the gap where 'security necessity' had been invoked to justify systematic abuse.
% FOUNDING_PROBLEM_CORROBORATION: ICRC commentary and post-WWII tribunal records, produced by bodies outside any single detaining state's security establishment, attest the founding problem (unconstrained coercive treatment under claimed necessity) remains live — citing continued documented use of enhanced interrogation techniques and inconsistent state compliance since 2001. States favoring the contextual-necessity and proportionality readings dispute that the absolute floor is still fit for purpose against contemporary asymmetric threats, but this dispute is itself evidence the underlying problem the article addressed has not been resolved, only relitigated.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.42 to 0.68) because the absolute reading's real-world bite depends on enforcement infrastructure (tribunals, monitoring, prosecutions) that matured substantially post-2001 and post-Guantanamo/Abu Ghraib litigation, sharpening the cost this reading imposes on security services who cannot claim necessity as a defense. Theater ratio climbs (0.2 to 0.4) reflecting a growing gap between formal state adherence to the absolute standard in policy documents and actual practice in classified interrogation programs — a moderate but real theatrical layer, not the dominant feature. Suppression (settling at 0.55) reflects the real coercive force of international prosecution and reputational sanction used to hold states to the bar, but capped below dominant because compliance is substantially voluntary/normative rather than centrally enforced — no single body can compel a non-cooperating state.
 *
 * PERSPECTIVAL GAP:
 *   From the international-courts/monitoring seat and from the detained-persons seat, the absolute standard reads as settled law performing its coordination function cleanly. From the intelligence-services and interrogation-personnel seats, the same structure reads as an externally imposed, non-negotiable cost center that ignores operational judgment about specific threats — the engine should compute these seats divergently given their opposed directionality and different exit constraints, not because one seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons and future captured state personnel are structural beneficiaries: the standard subsidizes their safety at the direct cost of interrogation flexibility, giving them low d. Intelligence services and interrogation personnel are structural targets: the absolute bar removes tools/discretion they would otherwise exercise, giving them high d — interrogation personnel especially, since their exit option (refuse the order) is constrained by command hierarchy and their liability exposure is personal and immediate. Detainees under derogating states occupy an unusual position: nominally beneficiaries of the kernel's text, but as payers of this reading's non-universal enforcement — they are victims of the CONTEST, not of this reading's operation, which is why they are listed among victims of this particular reading's incomplete reach rather than beneficiaries of its content.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coercive treatment justified by claimed necessity) remains live by external corroboration (ICRC, tribunal record), which forecloses treating this as mandatrophy — the mandate has not outlived its function even though the mandate is now widely contested by states seeking the sibling readings. The tangled_rope classification (rather than a clean rope or mountain) captures that the absolute reading performs genuine coordination (a universal, non-negotiable floor lowers verification costs for reciprocal treatment across a conflict) while simultaneously extracting real cost from a specific class of agents (security services, interrogators) who cannot opt out even where they hold a good-faith necessity claim — both the coordination function and the asymmetric extraction are structurally present and require active tribunal/monitoring enforcement to hold, which is exactly the tangled_rope gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Where, structurally, is the disagreement between the absolute_prohibition reading and its siblings (contextual_necessity, proportionality_balancing) actually located — in the interpretation of ''degrading treatment,'' in the derogability of the norm itself, or in the legitimacy of claimed necessity as a defense?',
    'Comparative doctrinal analysis of state practice, ICJ/ICTY/ICTR jurisprudence, and reservations/interpretive declarations attached to Common Article 3 ratifications across jurisdictions applying each reading.',
    'If the disagreement is located in the derogability question, the contextual_necessity reading directly forecloses this reading''s core premise (non-derogability) and the relation should be reassessed toward tension rather than mere coexistence in state practice, even though both remain live in international discourse. If located only in defining the treatment threshold, the readings can coexist as differing thresholds under a shared non-derogability premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locating the structural site of disagreement among the three kernel readings.').

omega_variable(
    enforcement_asymmetry_effect,
    'Does the absolute reading''s dependence on international courts and monitoring bodies for practical enforcement mean its real-world extraction profile is effectively lower in powerful, sanction-resistant states and higher in weaker, aid-dependent states — making the reading''s operative bite geopolitically asymmetric rather than universal as its text claims?',
    'Cross-national comparison of prosecution rates, ICRC access grants, and documented compliance outcomes stratified by state power/aid-dependency.',
    'If enforcement is asymmetric, the reading''s coordination claim (a universal floor) is partly a false universal — the standard functions closer to a snare on weak states and closer to an unenforced norm on powerful ones, which would push the classification toward tangled_rope with a stronger extraction weighting for less powerful signatory states specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_effect, empirical, 'Whether enforcement of the absolute standard is geopolitically asymmetric.').

omega_variable(
    reciprocity_versus_dignity_grounding,
    'Is the absolute reading''s authority ultimately grounded in reciprocal self-interest (states protect enemy detainees because they want their own captured personnel protected) or in an intrinsic dignity claim independent of reciprocity — and does this distinction matter for how the reading survives asymmetric conflicts where no reciprocity exists (e.g., against non-state actors who will not reciprocate)?',
    'Analysis of state justificatory rhetoric and behavior specifically in conflicts against non-reciprocating non-state armed groups, where the reciprocity incentive is absent.',
    'If grounding is primarily reciprocal, the absolute reading should show measurably weaker compliance in non-reciprocal conflicts, which would validate treating its non-derogability as aspirational rather than structural in those contexts. If grounding is dignity-based and reciprocity-independent, compliance should hold roughly steady, supporting the reading''s own non-derogability premise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_versus_dignity_grounding, conceptual, 'Whether the reading''s non-derogability rests on reciprocity or intrinsic dignity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__absolute_prohibition, theater_ratio, 0, 0.2).
narrative_ontology:measurement(huma_tr_t8, humane_treatment_standard__absolute_prohibition, theater_ratio, 8, 0.25).
narrative_ontology:measurement(huma_tr_t16, humane_treatment_standard__absolute_prohibition, theater_ratio, 16, 0.32).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__absolute_prohibition, theater_ratio, 24, 0.38).
narrative_ontology:measurement(huma_tr_t32, humane_treatment_standard__absolute_prohibition, theater_ratio, 32, 0.4).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__absolute_prohibition, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__absolute_prohibition, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(huma_be_t8, humane_treatment_standard__absolute_prohibition, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(huma_be_t16, humane_treatment_standard__absolute_prohibition, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__absolute_prohibition, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(huma_be_t32, humane_treatment_standard__absolute_prohibition, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__absolute_prohibition, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__absolute_prohibition, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(huma_su_t8, humane_treatment_standard__absolute_prohibition, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(huma_su_t16, humane_treatment_standard__absolute_prohibition, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__absolute_prohibition, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(huma_su_t32, humane_treatment_standard__absolute_prohibition, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__absolute_prohibition, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__absolute_prohibition, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the humane_treatment_standard kernel (Common Article 3's non-derogable minimum treatment text). All three share the same textual kernel but instantiate structurally different constraints with different ε, different beneficiary/victim sets, and different classifications: absolute_prohibition (this file, tangled_rope — coordination via universal floor plus real extraction from security services and interrogators) is the upstream, most-established reading and is cited as the doctrinal baseline against which contextual_necessity and proportionality_balancing are argued as departures. contextual_necessity and proportionality_balancing should each declare this constraint_id in their own network.affects_constraints, completing the family linkage per the BGS decomposition pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
