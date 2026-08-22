% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Common Article 3 Contextual-Necessity Reading (Enhanced Interrogation Discretion)
 *   domain: international_humanitarian_law/state_security
 *
 * SUMMARY:
 *   This story authors the contextual-necessity reading of the Common Article
 *   3 humane-treatment kernel: the position that the baseline standard is
 *   real but permits displacement by an agency-determined national security
 *   necessity finding. Under this reading, 'humane' is not fixed at
 *   treaty-drafting but is filled in situationally by the classifying
 *   security apparatus. This produces a genuine coordination function (a
 *   workable operational standard for emergency scenarios) riding alongside
 *   an asymmetric extraction structure (agencies gain unreviewable
 *   discretion; detainees who cannot contest classification bear the cost).
 *   The claim is authored as tangled_rope, not snare, because the
 *   coordination function — operational latitude for genuinely time-critical
 *   scenarios — is structurally real even though the metrics show it is
 *   heavily outweighed by extraction in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.72).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.68).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.72).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Common Article 3 Contextual-Necessity Reading (Enhanced Interrogation Discretion)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'e51d6905-4913-4ddb-a242-fff73fb83636').
narrative_ontology:cs_kernel_codification('e51d6905-4913-4ddb-a242-fff73fb83636', fixed_text).
narrative_ontology:cs_authority_grounding('e51d6905-4913-4ddb-a242-fff73fb83636', extraction).
narrative_ontology:cs_interpretation_layer_present('e51d6905-4913-4ddb-a242-fff73fb83636').
narrative_ontology:cs_reading_relation('e51d6905-4913-4ddb-a242-fff73fb83636', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('e51d6905-4913-4ddb-a242-fff73fb83636', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('e51d6905-4913-4ddb-a242-fff73fb83636', foundational, security_necessity_can_override_baseline_humane_treatment).
narrative_ontology:cs_axiom_status(security_necessity_can_override_baseline_humane_treatment, holdable).
narrative_ontology:cs_axiom_grounding('e51d6905-4913-4ddb-a242-fff73fb83636', security_necessity_can_override_baseline_humane_treatment, instrumental).
narrative_ontology:cs_axiom('e51d6905-4913-4ddb-a242-fff73fb83636', secondary, classifying_agency_is_competent_arbiter_of_necessity).
narrative_ontology:cs_axiom_status(classifying_agency_is_competent_arbiter_of_necessity, holdable).
narrative_ontology:cs_axiom_grounding('e51d6905-4913-4ddb-a242-fff73fb83636', classifying_agency_is_competent_arbiter_of_necessity, conventional).
narrative_ontology:cs_reference_frame('e51d6905-4913-4ddb-a242-fff73fb83636', cold_war_era_state_security_primacy).
narrative_ontology:cs_drift_state('e51d6905-4913-4ddb-a242-fff73fb83636', post_9_11_detention_program_disclosure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e51d6905-4913-4ddb-a242-fff73fb83636', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, executive_branch_officials).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, interrogation_program_architects).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, designated_high_value_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_misclassified_as_high_value).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, field_interrogators_bearing_legal_exposure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines internally what counts as 'humane' under exigent circumstances, classifies detainees as posing imminent threats, and authorizes interrogation techniques accordingly. Controls the classification process that determines whose treatment is contextually reviewable, and controls the record of what was done. Faces no external body with binding authority to overturn its necessity determinations in real time.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Issues legal opinions and directives establishing the necessity threshold, gains political and intelligence benefits attributed to the program's claimed successes, and bears diffuse accountability because the necessity standard is inherently retrospective and hard to falsify.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, executive_branch_officials, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, executive_branch_officials, agenda_setter).

% Held outside ordinary legal process once classified as high-value; the humane-treatment baseline that would otherwise apply is displaced by a necessity finding made by the same agency holding them. Have no independent forum to contest the classification before treatment is administered, and often no forum after.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, designated_high_value_detainees, payer,
    powerless, biographical, trapped, global).

% Suffer enhanced interrogation on the basis of intelligence that later proves mistaken or overstated. The contextual standard offers no mechanism to distinguish them from correctly classified detainees before the fact, since the necessity determination and the treatment happen inside the same closed institutional loop.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees_misclassified_as_high_value, payer,
    powerless, biographical, trapped, global).

% Carry out techniques authorized under the necessity framework and later face potential prosecution, congressional inquiry, or reputational ruin if the legal or political winds shift and the necessity finding is retroactively repudiated. Have limited ability to independently verify the necessity determinations they are ordered to act on.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, field_interrogators_bearing_legal_exposure, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, field_interrogators_bearing_legal_exposure, agenda_setter).

% ICRC and treaty bodies seek access to detainees and records to verify compliance with Common Article 3's baseline, but the necessity framework routes classification and treatment decisions through closed national-security channels that limit or delay independent verification. Their objections are documented but structurally unable to halt ongoing operations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_monitoring_bodies, excluded,
    organized, generational, constrained, global).

% Adjudicate the legality of specific programs after disclosure, sometimes years later, and can impose retrospective constraints or damages. Their review is structurally after-the-fact relative to the treatment decisions themselves.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, domestic_courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, national_security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides state security institutions a workable operational standard for detainee treatment during time-pressured intelligence-gathering, avoiding a rigid rule that agencies argue could not survive contact with genuine ticking-clock scenarios.
% TRANSFER_FUNCTION: Moves discretion over what counts as humane treatment from a fixed external baseline to the classifying security agency itself, and moves the practical risk of erroneous or excessive treatment onto detainees who cannot contest their classification in advance.
% ABSENT_VOICES: Detainees themselves have no seat in the necessity determination. International monitoring bodies are structurally delayed or denied access. Former detainees later found not to have possessed the alleged intelligence value are not consulted in constructing the standard that governed their treatment.
% DISAPPEARANCE_RATIONALE: If the contextual-necessity reading disappeared and Common Article 3's baseline applied without a security override, detention and interrogation programs built on necessity findings would require restructuring, prior authorized techniques would need re-justification under a fixed floor, and the classification apparatus that currently determines who receives baseline versus enhanced treatment would lose its legal footing.
% FOUNDING_PROBLEM: States argued that a uniform, non-derogable humane treatment floor could not accommodate genuine emergency scenarios involving detainees believed to hold time-critical intelligence about imminent attacks, and sought interpretive room to authorize measures beyond the ordinary baseline in those cases.
% FOUNDING_PROBLEM_CORROBORATION: Security agencies and executive officials attest the ticking-clock scenario is a live, recurring operational reality. Independent inquiries (the US Senate Intelligence Committee's own investigation, ICRC assessments, and academic interrogation-efficacy research) attest that documented invocations rarely matched the imminent-threat scenario used to justify the doctrine, and that intelligence yield from enhanced techniques was frequently unverified or contradicted by conventional-interrogation results — corroboration from outside the benefiting agencies is skeptical of the founding scenario's empirical frequency, though not of its rhetorical persistence.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.45 to 0.72) as the doctrine moves from occasional invocation to institutionalized program with dedicated legal architecture defending it. Theater ratio also rises (0.2 to 0.45) as post-hoc legal memoranda and classification procedures increasingly perform compliance with a humane-treatment floor that the same documents simultaneously suspend. Suppression climbs as the closed classification loop hardens into standard operating procedure, making external verification structurally harder over time, not easier.
 *
 * DIRECTIONALITY LOGIC:
 *   Security agencies and executive officials sit near the beneficiary end: they set the necessity standard, control its invocation, and capture the operational and political benefit of appearing tough on emergent threats, without proportionate accountability exposure. Detainees — especially those misclassified — sit at the full-target end: trapped exit, no voice in the classification decision that determines the standard applied to them. Field interrogators occupy an intermediate, unstable position: they act under the same authority structure but absorb downstream legal risk the agency itself is shielded from, which is why they carry a dual agenda_setter/payer role.
 *
 * MANDATROPHY ANALYSIS:
 *   The contextual-necessity reading resists lazy mislabeling in both directions. It is not pure coordination (a rope) because the same discretion that solves the emergency-scenario problem is also the mechanism by which erroneous and excessive treatment becomes unreviewable in real time — an genuine coordination function coexists with genuine asymmetric extraction, which is exactly the tangled_rope signature. It is also not simply relabeled as a pure snare, because doing so would erase the reading's own internal claim that some detentions involve authentic imminent-threat scenarios; the story's engine-computed classification should register the extraction that dominates in practice without denying the coordination premise the reading rests on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_reviewability,
    'Is the security agency''s necessity determination genuinely unreviewable in practice, or does the possibility of later judicial/legislative review functionally constrain agency discretion in real time?',
    'Comparative case analysis of how many necessity-based classification decisions were later overturned, and whether ex post review measurably deterred subsequent classification decisions (a chilling-effect study).',
    'If ex post review meaningfully constrains real-time discretion, the effective extraction is lower than the closed-loop model assumes; if review is consistently too late or too weak to matter, the tangled_rope classification understates how close this reading sits to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_determination_reviewability, empirical, 'Whether retrospective accountability mechanisms meaningfully discipline real-time necessity classification.').

omega_variable(
    genuine_vs_pretextual_necessity,
    'What proportion of invoked necessity determinations reflect authentic imminent-threat scenarios versus pretextual invocation to authorize predetermined interrogation techniques?',
    'Declassified program review comparing the stated urgency at time of classification against subsequently confirmed intelligence value and timeline of the underlying threat.',
    'A high pretextual proportion would support reclassifying this reading as substantially closer to snare (coordination function is mostly cover); a high genuine proportion would support the coordination function claim underlying the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_vs_pretextual_necessity, empirical, 'Empirical frequency of genuine versus pretextual invocation of the necessity override.').

omega_variable(
    kernel_framing_choice,
    'Is Common Article 3 more accurately modeled as a fixed baseline with a contested override clause (this reading), or as inherently requiring the balancing test the proportionality_balancing reading describes — such that the ''baseline plus override'' framing itself is a rhetorical construction favoring the discretion-holder?',
    'Textual and travaux préparatoires analysis of Common Article 3''s drafting history, and comparison with how other non-derogable IHL provisions have been interpreted by international tribunals absent a security carve-out.',
    'If the drafting history supports a non-derogable reading, the contextual_necessity framing itself may be better understood as a post-hoc interpretive move by security agencies rather than a defensible independent reading — which would not change this story''s authored ε but would strengthen the case that absolute_prohibition is the historically grounded sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the contextual-necessity framing is a defensible independent reading or a constructed interpretive overlay favoring security agencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.2).
narrative_ontology:measurement(huma_tr_t4, humane_treatment_standard__contextual_necessity, theater_ratio, 4, 0.28).
narrative_ontology:measurement(huma_tr_t8, humane_treatment_standard__contextual_necessity, theater_ratio, 8, 0.35).
narrative_ontology:measurement(huma_tr_t12, humane_treatment_standard__contextual_necessity, theater_ratio, 12, 0.4).
narrative_ontology:measurement(huma_tr_t16, humane_treatment_standard__contextual_necessity, theater_ratio, 16, 0.43).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.45).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__contextual_necessity, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(huma_be_t4, humane_treatment_standard__contextual_necessity, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(huma_be_t8, humane_treatment_standard__contextual_necessity, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(huma_be_t12, humane_treatment_standard__contextual_necessity, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(huma_be_t16, humane_treatment_standard__contextual_necessity, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__contextual_necessity, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(huma_su_t4, humane_treatment_standard__contextual_necessity, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(huma_su_t8, humane_treatment_standard__contextual_necessity, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(huma_su_t12, humane_treatment_standard__contextual_necessity, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(huma_su_t16, humane_treatment_standard__contextual_necessity, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__contextual_necessity, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, proportionality_balancing).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language 'Common Article 3 humane treatment standard' per the ε-invariance principle. absolute_prohibition authors ε near the low end (non-derogable floor, minimal legitimate extraction) with victims limited to those subjected to unlawful treatment despite the prohibition. proportionality_balancing authors an intermediate ε reflecting a structured balancing test with procedural safeguards. This story (contextual_necessity) authors the highest ε of the three, reflecting unreviewable agency discretion over the humane-treatment definition itself. The three share a kernel (the humane_treatment_standard) but are not interchangeable measurements of one constraint — they are three structurally distinct arrangements with different beneficiary/victim sets and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__contextual_necessity, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
