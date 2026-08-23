% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Narrow Armed Attack Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates the narrow_armed_attack_reading of the
 *   article_51_self_defense kernel. Sibling readings include the
 *   expansive_preventive_reading (which broadens self-defense to preemptive
 *   or preventive force) and the unable_unwilling_doctrine_reading (which
 *   relaxes state attribution for non-state actor threats). The narrow
 *   reading treats Article 51 as strictly limiting unilateral force to
 *   responses against actual or imminent armed attacks by states attributable
 *   under international law, preserving the authority of weaker states and
 *   multilateral institutions while constraining the strategic freedom of
 *   powerful states.
 *
 * KEY AGENTS:
 *   - weaker_states: Beneficiary (moderate/constrained) â gain territorial integrity guarantees and legal shelter from unilateral intervention
 *   - powerful_states: Payer (powerful/constrained) â strategic autonomy is legally constrained by the armed attack and attribution requirements
 *   - multilateral_security_institutions: Agenda-setter/beneficiary (institutional/constrained) â adjudicate disputes and benefit from preserved authority under the Charter
 *   - international_legal_scholars: Observer (analytical) â interpret state practice and monitor compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.68).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.55).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Narrow Armed Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '95c9ff21-d77c-4161-acb6-6041bb7fd211').
narrative_ontology:cs_kernel_codification('95c9ff21-d77c-4161-acb6-6041bb7fd211', fixed_text).
narrative_ontology:cs_authority_grounding('95c9ff21-d77c-4161-acb6-6041bb7fd211', lineage).
narrative_ontology:cs_interpretation_layer_present('95c9ff21-d77c-4161-acb6-6041bb7fd211').
narrative_ontology:cs_reading_relation('95c9ff21-d77c-4161-acb6-6041bb7fd211', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('95c9ff21-d77c-4161-acb6-6041bb7fd211', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('95c9ff21-d77c-4161-acb6-6041bb7fd211', foundational, armed_attack_prerequisite).
narrative_ontology:cs_axiom_status(armed_attack_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('95c9ff21-d77c-4161-acb6-6041bb7fd211', armed_attack_prerequisite, conventional).
narrative_ontology:cs_axiom('95c9ff21-d77c-4161-acb6-6041bb7fd211', foundational, state_attribution_required).
narrative_ontology:cs_axiom_status(state_attribution_required, holdable).
narrative_ontology:cs_axiom_grounding('95c9ff21-d77c-4161-acb6-6041bb7fd211', state_attribution_required, conventional).
narrative_ontology:cs_reference_frame('95c9ff21-d77c-4161-acb6-6041bb7fd211', un_charter_collective_security).
narrative_ontology:cs_drift_state('95c9ff21-d77c-4161-acb6-6041bb7fd211', post_9_11_security_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('95c9ff21-d77c-4161-acb6-6041bb7fd211', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_security_institutions).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sovereign states with limited material power that benefit from legal prohibitions on unilateral military intervention by stronger states. Their territorial integrity is formally protected by the requirement of an actual or imminent armed attack and state attribution before force may be used lawfully.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    moderate, generational, constrained, global).

% Militarily capable states whose strategic freedom to conduct preventive strikes, cross-border counter-terrorism, and unilateral uses of force is legally constrained. They bear the cost of demonstrating an actual or imminent armed attack by a state and face legal exposure when acting outside the narrow reading.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states, payer,
    powerful, generational, constrained, global).

% The UN Security Council, International Court of Justice, and related bodies that adjudicate, monitor, and enforce the narrow reading of Article 51. They administer the legal framework and their authority and budget depend on states continuing to treat unilateral force as exceptional.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_security_institutions, agenda_setter,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, multilateral_security_institutions, beneficiary).

% Academic and practitioner experts who interpret state practice, case law, and Charter text. They monitor compliance with the narrow reading and provide the analytical vocabulary through which contest over Article 51 is conducted.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unilateral military escalation by limiting lawful self-defense to responses against actual or imminent armed attacks attributable to states, thereby preserving a decentralized but rule-bound international order and protecting weaker states from powerful intervention.
% TRANSFER_FUNCTION: Transfers authority over the decision to use military force from individual state discretion to the UN Charter framework and its interpretive institutions; transfers security costs onto powerful states that must absorb or address threats through non-forcible or collective means.
% ABSENT_VOICES: Non-state actors whose attacks do not trigger Article 51 under this reading and who have no standing to challenge its scope; populations in weak states facing non-state threats who might prefer external intervention but are represented only through state governments; powerful state military strategists who view the reading as dangerously restrictive.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished overnight, powerful states would expand unilateral preventive and cross-border strikes, weaker states would lose territorial integrity guarantees, the UN Security Council's primacy would erode, and the post-1945 legal architecture against aggressive war would unravel.
% FOUNDING_PROBLEM: Unchecked interstate aggression and the use of unilateral military force as routine instruments of national policy, which generated systemic instability and culminated in World War II.
% FOUNDING_PROBLEM_CORROBORATION: Historical record and Charter travaux prÃ©paratoires are attested by international legal historians outside the benefiting parties; however, strategic studies scholars and counter-terrorism practitioners from powerful state traditions contest that interstate aggression remains the primary threat, arguing that non-state actors have displaced the founding problem.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the rule structurally transfers security decision-making autonomy from powerful states to the collective legal framework, substantially constraining their available responses. Suppression is 0.55: enforcement relies on decentralized legal interpretation, ICJ adjudication, diplomatic pressure, and reputational costs rather than centralized coercion, so the active suppressive force is moderate. Theater ratio is 0.28 because powerful states often maintain rhetorical compliance while planning or executing expansive operations, but the core legal architecture remains substantively operative and is not yet predominately performative. Accessibility collapse is 0.50: legal alternatives such as unilateral preventive war are formally closed but remain practically accessible to powerful states, so alternatives are only partially collapsed. Resistance is 0.60 because powerful states and their legal advisors consistently contest, evade, and seek to reinterpret the narrow reading.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of weaker states and multilateral institutions, the narrow reading appears as protective legal certainty preserving sovereignty and limiting aggressive war. From the seat of powerful states facing non-state or emergent threats, it reads as an extractive constraint disabling legitimate security responses. The engine computes this divergence from the same structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states are structural beneficiaries (d near 0.0): the constraint subsidizes their territorial integrity by limiting powerful state intervention. Multilateral security institutions are also beneficiaries with agenda-setting authority (d near 0.1). Powerful states are structural targets (d near 1.0): they bear the constraint through restricted strategic options and legal exposure. International legal scholars occupy an analytical seat (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâuncontrolled interstate aggressionâhas not fully disappeared, but its character is contested by the rise of non-state threats. The constraint persists not merely by inertia but because concentrated beneficiaries (weaker states, UN institutions) actively defend it and the coordination function (preventing wars of aggression) remains partially live. The theater ratio remains below 0.5 and beneficiaries continue to capture real coordination gains, so the constraint is not a piton despite contested adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_51_kernel_contest,
    'Does Article 51 of the UN Charter definitively restrict self-defense to actual or imminent armed attacks by states, or does the text permit expansive and hybrid readings that the narrow reading suppresses?',
    'Comparative textual analysis of the Charter travaux prÃ©paratoires and systematic review of state practice and ICJ jurisprudence to determine whether the narrow reading is a faithful interpretation or a strategic construction.',
    'If the text is genuinely ambiguous, the narrow reading''s high extractiveness may be a constructed constraint benefiting weaker states and institutions rather than a natural legal limit; if the text is determinate, the extraction is the legitimate cost of legal coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_51_kernel_contest, conceptual, 'Ambiguity over whether the Charter text compels the narrow reading or permits siblings.').

omega_variable(
    attribution_threshold_ambiguity,
    'What evidentiary and causal threshold satisfies ''attributable to a host state'' for non-state actor attacks under the narrow reading, and does the unwilling/unable doctrine fall inside or outside that threshold?',
    'ICJ advisory proceedings or international criminal tribunal rulings establishing clear attribution standards, combined with empirical review of state practice following non-state attacks.',
    'If unwilling/unable is classified as attribution, the narrow reading partially absorbs the sibling reading and its extraction profile shifts; if not, the narrow reading forecloses a major post-9/11 security practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_threshold_ambiguity, empirical, 'Uncertainty about the legal boundary of state attribution for non-state actor attacks.').

omega_variable(
    extraction_or_coordination_balance,
    'Does the narrow reading''s constraint on powerful states represent necessary coordination to prevent abuse of force, or asymmetric extraction that disables legitimate security responses?',
    'Counterfactual analysis of interstate conflict rates under alternative legal regimes and assessment of whether powerful states'' security losses are offset by systemic stability gains.',
    'If primarily coordination, the constraint is a tangled rope; if primarily extraction with coordination as cover, it trends toward snare. The classification boundary depends on this balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_or_coordination_balance, conceptual, 'Whether the constraint is fundamentally coordinative or extractive in character.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(arti_tr_t40, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(arti_tr_t60, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(arti_tr_t70, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 70, 0.35).
narrative_ontology:measurement(arti_tr_t80, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 80, 0.28).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(arti_be_t40, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(arti_be_t60, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(arti_be_t70, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 70, 0.5).
narrative_ontology:measurement(arti_be_t80, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 80, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_51_self_defense__narrow_armed_attack_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
