% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Contextual Necessity Reading: Humane Treatment Standard with National Security Override
 *   domain: international_humanitarian_law/security_operations
 *
 * SUMMARY:
 *   This constraint instantiates the contextual-necessity reading of Common
 *   Article 3's humane-treatment standard. In this reading, the baseline
 *   prohibition on torture and degrading treatment is preserved in nominal
 *   form but permits national security agencies to override baseline
 *   protections through necessity determinations. When an agency designates a
 *   detainee as 'high-value' and claims interrogation is necessary to prevent
 *   imminent harm, the baseline protections collapse to whatever the agency
 *   determines is necessary and therefore permissible. The reading produces
 *   two structural classes of detainees: ordinary prisoners protected by
 *   baseline standards, and high-value targets whose protections are
 *   conditional on threat assessment. This reading generates substantial
 *   extraction (0.81 at interval end) because the necessity doctrine's
 *   beneficiaries (security agencies) control both the threat determinations
 *   and the definitions of permissible interrogation intensity. The
 *   constraint is claimed as tangled_rope because it preserves a coordination
 *   function (baseline protection for detainees who are not deemed security
 *   threats) while enabling systematic extraction from a victim set
 *   (high-value targets designated for intensive interrogation). The
 *   measurement series shows extraction accumulating over the interval as
 *   necessity determinations become more routinized and agency discretion
 *   expands; theater rises as justifications for interrogation intensity
 *   become more elaborately documented while actual baseline-protection
 *   compliance declines. This reading is ONE instantiation of a contested
 *   kernel (humane_treatment_standard); sibling readings
 *   (absolute_prohibition and proportionality_balancing) emit different
 *   constraints with different ε values and victim sets.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.81).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.78).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.81).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Contextual Necessity Reading: Humane Treatment Standard with National Security Override").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/security_operations").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '2a697692-4be4-4753-a61b-5137f23d027d').
narrative_ontology:cs_kernel_codification('2a697692-4be4-4753-a61b-5137f23d027d', fixed_text).
narrative_ontology:cs_authority_grounding('2a697692-4be4-4753-a61b-5137f23d027d', extraction).
narrative_ontology:cs_interpretation_layer_present('2a697692-4be4-4753-a61b-5137f23d027d').
narrative_ontology:cs_reading_relation('2a697692-4be4-4753-a61b-5137f23d027d', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('2a697692-4be4-4753-a61b-5137f23d027d', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('2a697692-4be4-4753-a61b-5137f23d027d', foundational, security_imperatives_override_baseline).
narrative_ontology:cs_axiom_status(security_imperatives_override_baseline, holdable).
narrative_ontology:cs_axiom_grounding('2a697692-4be4-4753-a61b-5137f23d027d', security_imperatives_override_baseline, empirically_contingent).
narrative_ontology:cs_axiom('2a697692-4be4-4753-a61b-5137f23d027d', foundational, agency_necessity_assessment_authority).
narrative_ontology:cs_axiom_status(agency_necessity_assessment_authority, holdable).
narrative_ontology:cs_axiom_grounding('2a697692-4be4-4753-a61b-5137f23d027d', agency_necessity_assessment_authority, instrumental).
narrative_ontology:cs_reference_frame('2a697692-4be4-4753-a61b-5137f23d027d', necessity_override_framework).
narrative_ontology:cs_drift_state('2a697692-4be4-4753-a61b-5137f23d027d', contemporary_post_2015_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a697692-4be4-4753-a61b-5137f23d027d', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_in_custody).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, high_value_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_executives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and execute interrogation protocols within security operations. Claim that contextual necessity permits enhanced interrogation when detainees possess information about imminent threats. Control the classification of threat severity and the operational circumstances that trigger necessity overrides. Directly benefit from discretion to conduct intensive questioning without binding baseline prohibitions. Operate under claim that humane treatment is flexible standard that must adapt to security imperatives.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Subject to questioning under protocols justified by necessity doctrine. Physical and psychological experience varies based on security agency determination of threat level and interrogation necessity. No legal recourse during detention; protection collapses to whatever agency determines is necessary and therefore permissible. Bear the direct cost of enhanced interrogation techniques that would violate absolute-prohibition standards.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees_in_custody, payer,
    powerless, immediate, trapped, local).

% Designated by security agencies as possessing critical intelligence. Explicitly moved outside baseline humane-treatment protections by necessity determinations. Subject to intensive interrogation justified by their informational value and threat assessment. Structural victimhood is manufactured by classification: the label 'high-value target' triggers the necessity override and removes ordinary protections. Cannot challenge the classification from within the interrogation environment.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, high_value_targets, payer,
    powerless, immediate, trapped, global).

% Claim authority to authorize necessity-driven interrogation as component of national security. Benefit from the security outcome (intelligence extraction) and the legal coverage that contextual necessity provides. Can frame deviation from baseline standards as adaptation to emergency rather than violation of law. Maintain discretion over threat determinations that trigger overrides.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_executives, beneficiary,
    institutional, generational, arbitrage, national).

% Interpret Common Article 3 in light of the constraint's reading. Face structural pressure from necessity doctrine: either accept contextual flexibility (endorsing this reading) or maintain absolute-prohibition interpretation (competing reading). Their pronouncements affect legitimacy but do not operationally control the agencies conducting interrogations. Sit outside the enforcement chain.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_humanitarian_law_authorities, observer,
    institutional, generational, analytical, global).

% Would argue for absolute-prohibition reading and challenge necessity determinations as post-hoc cover for torture. Formally excluded from operational decisions about interrogation protocols and threat classifications. Can investigate and publicize; cannot access detention facilities in real time or override agency necessity claims. Resistance is structural but exterior to the enforcement mechanism.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainee_advocacy_organizations, excluded,
    moderate, generational, constrained, global).

% Navigate the same contextual-necessity reading in their own operations. Benefit from the precedent that permits necessity overrides; simultaneously exposed to interrogation if captured. Their adoption of the same reading creates structural pressure toward acceptance (coordinating states around the same framework) while their opposition to it generates counter-precedent (absolute-prohibition readers).
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, competing_states, observer,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes baseline humane-treatment floor that permits enforcement agencies to preserve and protect detainees during custody, while preserving flexibility to intensify interrogation when national security determinations justify necessity override. Coordinates multiple state actors around a shared framework that permits calibrated response to security threats without absolute prohibition that would prevent threat intelligence gathering.
% TRANSFER_FUNCTION: Moves from detainees (the powerless subjects of interrogation) compliance, information, and physical/psychological endurance to security agencies (in the form of intelligence, operational advantage, and institutional authority). Moves from state executives the ability to claim legal coverage for interrogation practices that would violate absolute-prohibition standards. Moves to international humanitarian law interpreters legitimacy pressure: their endorsement of contextual flexibility sustains the reading.
% ABSENT_VOICES: Detainees cannot contest threat classifications that justify interrogation intensity; detainee-advocacy organizations and absolute-prohibition interpreters are structurally excluded from real-time determinations about necessity and interrogation protocols. Their objections remain outside the enforcement chain and do not constrain agency discretion in operational moments.
% DISAPPEARANCE_RATIONALE: If the contextual-necessity reading and its enforcement machinery disappeared overnight, security agencies would revert to absolute-prohibition baseline; interrogation protocols would narrow; many detainees classified as 'high-value targets' under the current reading would receive baseline protections; states would face stronger legal barriers to intensive questioning. The organizational infrastructure that defines threat severity and authorizes necessity overrides would be dismantled; the entire interrogation classification system rides on the necessity doctrine.
% FOUNDING_PROBLEM: Early post-9/11 security environment faced threats assessed as imminently catastrophic (mass-casualty attacks, weapons proliferation). Absolute-prohibition standards were read as constraining interrogation of detainees believed to possess critical intelligence about imminent plots. The constraint emerged to permit flexible response to what security agencies framed as exceptional threat circumstances while maintaining nominal baseline protections.
% FOUNDING_PROBLEM_CORROBORATION: Security agencies and state executives attest the threat level that justified the necessity reading has persisted over two decades. Independent analysts and international humanitarian law scholars attest the original threat assessments were substantially over-calibrated (detainee intelligence quality was poor, many 'high-value targets' yielded minimal actionable information, alternative intelligence sources existed). The founding problem—unprecedented security threat requiring interrogation flexibility—is substantially resolved in scholarly and activist assessment but remains claimed as 'live' by the agencies that benefit from the discretion it permits.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81) because the necessity doctrine permits agencies to intensify interrogation beyond baseline standards whenever threat determination justifies it; the agencies control both the threat assessment and the interrogation authorization, creating a closed loop where discretion converts into extraction. Suppression is also high (0.78) because detainees have no independent recourse to challenge necessity determinations; resistance from detainee-advocacy organizations and absolute-prohibition interpreters is structurally exterior to operational decisions. Theater is moderate (0.42) because the interrogation protocols are documented and justified with necessity rationales, creating a veneer of legal compliance even as baselines are exceeded; the elaboration of justifications grows over the interval (T17 theater-rise pattern) even as extraction itself becomes more routine. Accessibility_collapse is lower (0.48) than expected for a high-extraction constraint because alternatives exist in principle (absolute-prohibition reading, international law challenges) but are suppressed in practice by the agencies' institutional position and state-executive backing. Resistance is moderate-high (0.62) because the constraint meets real legal and advocacy pushback from the absolute-prohibition and proportionality-balancing readers; this resistance does not prevent the constraint's operation but does create friction and generate competing narratives.
 *
 * PERSPECTIVAL GAP:
 *   The security-agency seat (agenda_setter, beneficiary) reads this constraint as necessary coordination that preserves baseline protections while permitting proportional intensity. The detainee seat (powerless, trapped) reads it as systematic deprivation of protection: the 'humane treatment' baseline they depend on becomes conditional on an agency determination they cannot contest. The state-executive seat (beneficiary) reads it as legal coverage for security operations; the international-humanitarian-law interpreter seat (observer) reads it as erosion of non-derogable standards. The engine computes these seats separately; the measured directionality divergence — high d for security agencies (low effective extraction on them) and very high d for detainees (high effective extraction on them) — models this perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Security agencies: d ≈ 0.15 (full beneficiary side). They control necessity determinations, set interrogation protocols, benefit from reduced baseline constraints, face no effective suppression of their own operations. Exit options are arbitrage (they could adopt absolute-prohibition reading but choose not to; their 'exit' is purely ideological, not structural). Detainees designated as high-value targets: d ≈ 0.95 (full target side). They are trapped in custody, subject to agency discretion over interrogation intensity, have no avenue to contest necessity determinations, and bear all the cost of the discretionary extraction. Time horizon is immediate (interrogation is happening now, not deferred). Exit is literally unavailable (imprisoned). State executives: d ≈ 0.20 (beneficiary side). They authorize the framework and benefit from it politically (claim to be protecting security) but do not directly conduct or experience interrogations; their exit is also arbitrage (they choose the reading). International-law observers: d ≈ 0.50 (symmetric). They interpret the constraint but do not control operational decisions; they face pressure to endorse the reading to remain institutionally relevant but do not benefit or suffer materially from interrogation protocols. The mapping from beneficiary/victim declarations to directionality is clean: beneficiaries get low d, victims get high d, observers get near-symmetric d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was authorized to solve the founding problem (post-9/11 security threats requiring flexible interrogation). The founding problem is now dead (threat assessments have been substantially downgraded by independent analysis; intelligence from detainees proved poor-quality). The constraint persists because agencies benefit from the discretion it provides, not because the mandate lives. This is mandatrophy: the original reason for the constraint's existence has expired, but the constraint persists due to institutional inertia and concentrated benefits. The theater ratio, which rises over the interval (0.28 to 0.42), captures this decay: as the founding problem recedes, more of the constraint's enforcement is theatrical justification (necessity rationales that elaborate law-compliance theater) and less is functional response to live security threats. The measured extraction (0.81, high and rising) combined with a dead founding problem and rising theater signals that the constraint has transitioned from genuine coordination (during the post-9/11 period when the threat assessment was live) to extraction justified by expired mandate. Declaring mandatrophy_resolved: true flags this for the system; it prevents misclassification of a structurally extractive constraint as legitimate coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_objectivity,
    'Are threat assessments and necessity determinations made by security agencies subject to independent verification, or do agencies control the entire chain of assessment and authorization?',
    'Comparative institutional analysis: do independent humanitarian bodies (ICRC, UN fact-finding missions) have access to assess necessity claims in real time? Do detainees have recourse to challenge necessity determinations before independent arbiters?',
    'If agencies control the full assessment chain without independent review, necessity determinations are systematically biased toward overstatement of threat (principal-agent problem: agencies authorize their own extraction). If independent verification exists, the necessity framework could function as genuine conditional protection (moving the constraint toward tangled_rope or proportionality_balancing). Absent independent review, the constraint is structurally snare-like despite its tangled_rope framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_determination_objectivity, empirical, 'Whether necessity determinations are subject to independent verification or are entirely controlled by the extractive party.').

omega_variable(
    contextual_necessity_vs_absolute_prohibition_foreclosure,
    'Does the contextual-necessity reading logically foreclose the absolute-prohibition reading, or do they represent genuinely coexisting positions held by different institutional actors?',
    'Doctrinal analysis: can a single state legal system hold both readings (absolute prohibition as domestic law, contextual necessity as operational doctrine)? Do international law interpreters maintain both readings, or must one be selected?',
    'If foreclosed: the readings are genuinely incompatible, and the constraint''s operation structurally forecloses the competing reading, which should be documented in cs_structure.reading_relations as forecloses. If coexisting: both readings remain live in international law, and competition between them is ongoing, documented as coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_necessity_vs_absolute_prohibition_foreclosure, conceptual, 'Logical relationship between contextual-necessity and absolute-prohibition readings of Common Article 3.').

omega_variable(
    internalized_suppression_postdetention,
    'Among detainees released after intensive interrogation under necessity protocols, how much of the measured suppression persists post-release? Is suppression structural (external barriers, custody loss) or partially internalized (trauma, identity fusion with interrogator, loss of reality-testing capacity)?',
    'Longitudinal study of released detainees: post-release patterns of agency, willingness to testify, psychological functioning, reintegration success, resistance capacity.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure (0.78) indicates—the target carries the suppression into freedom. If suppression is purely structural (reversible at release), the measured 0.78 captures the true extractive force. Internalization raises the extraction floor and suggests identity_locked exit characterization even post-detention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_postdetention, empirical, 'Structural vs. internalized suppression mechanism in intensive interrogation contexts.').

omega_variable(
    high_value_target_classification_drift,
    'Has the classification ''high-value target'' drifted from genuine high-threat individuals to broader categories as necessity determinations become routinized? What proportion of ''high-value targets'' actually possessed actionable intelligence related to imminent threats?',
    'Post-hoc analysis of interrogation records and intelligence assessments: what percentage of ''high-value targets'' yielded intelligence confirming threat severity? How have classification criteria changed over the interval?',
    'If classification has drifted substantially, necessity determinations are increasingly detached from the founding problem justification, and the constraint is more purely extractive. Rising classification drift supports mandatrophy reading and increases theater_ratio prediction. High drift magnitude (>50% misclassification) would suggest the constraint has fully transitioned from conditional protection to systematic extraction dressed in conditional language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(high_value_target_classification_drift, empirical, 'Whether ''high-value target'' classification has drifted from threat-based criteria to broader categories as protocols routinize.').

omega_variable(
    reading_specific_axiom_contestation_kernel,
    'Is the contextual-necessity axiom (security_imperatives_override_baseline) genuinely ''holdable'' in contemporary international humanitarian law discourse, or has it been formally overridden by state practice shifts or tribunal rulings?',
    'Doctrinal survey: do states continue to endorse contextual necessity in treaty interpretations and judicial filings, or have they abandoned the axiom after tribunal findings or evidence of abuse?',
    'If the axiom remains holdable, the reading is live and coexists with absolute_prohibition. If overridden (abandoned by major state actors or rejected by international courts), the reading is zombie-doctrine—maintained theatrically but not genuinely held—and should be reclassified to piton or snare based on theater and institutional maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specific_axiom_contestation_kernel, empirical, 'Whether the contextual-necessity axiom is contemporaneously endorsed or has been formally overridden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.28).
narrative_ontology:measurement(huma_tr_t3, humane_treatment_standard__contextual_necessity, theater_ratio, 3, 0.31).
narrative_ontology:measurement(huma_tr_t6, humane_treatment_standard__contextual_necessity, theater_ratio, 6, 0.35).
narrative_ontology:measurement(huma_tr_t12, humane_treatment_standard__contextual_necessity, theater_ratio, 12, 0.39).
narrative_ontology:measurement(huma_tr_t18, humane_treatment_standard__contextual_necessity, theater_ratio, 18, 0.41).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__contextual_necessity, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(huma_be_t3, humane_treatment_standard__contextual_necessity, base_extractiveness, 3, 0.7).
narrative_ontology:measurement(huma_be_t6, humane_treatment_standard__contextual_necessity, base_extractiveness, 6, 0.74).
narrative_ontology:measurement(huma_be_t12, humane_treatment_standard__contextual_necessity, base_extractiveness, 12, 0.78).
narrative_ontology:measurement(huma_be_t18, humane_treatment_standard__contextual_necessity, base_extractiveness, 18, 0.8).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__contextual_necessity, base_extractiveness, 24, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(huma_su_t3, humane_treatment_standard__contextual_necessity, suppression_requirement, 3, 0.69).
narrative_ontology:measurement(huma_su_t6, humane_treatment_standard__contextual_necessity, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(huma_su_t12, humane_treatment_standard__contextual_necessity, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(huma_su_t18, humane_treatment_standard__contextual_necessity, suppression_requirement, 18, 0.77).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__contextual_necessity, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__contextual_necessity, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% The humane_treatment_standard kernel generates three constraint stories, each representing a different reading. The contextual_necessity reading (this constraint) declares that Common Article 3 permits flexibility for national security; it forecloses absolute_prohibition (incompatible core premises within a single legal framework) and influences proportionality_balancing (creates structural pressure toward accepting discretionary rather than fixed baselines). All three readings share the referent—the standing arrangement of Common Article 3 interrogation protocols—but author different ε values depending on what the reading's framework permits. ε for contextual_necessity (0.81) is high because necessity doctrine enables substantial extraction; ε for absolute_prohibition is near-zero (reading forbids the extraction entirely); ε for proportionality_balancing is mid-range (reading permits balancing but not unlimited discretion). The stories are separate constraint problems with separate metrics, linked via network.affects_constraints to enable constraint-family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
