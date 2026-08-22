% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable/Unwilling Host-State Doctrine
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This story instantiates the unable/unwilling reading of the Article 51
 *   self-defense kernel: force in response to a completed or ongoing
 *   non-state actor attack is lawful against the host state's territory when
 *   that state is shown to be unwilling or unable to suppress the threat
 *   itself. This is the middle reading of the kernel — it requires an actual
 *   attack (unlike the expansive_preventive_reading, which permits action
 *   against emerging threats without attribution to a state) but does not
 *   require attribution of the attack to the host state itself (unlike the
 *   narrow_armed_attack_reading, which demands state responsibility under
 *   classical attribution rules). The doctrine has been invoked increasingly
 *   since the early 2000s by states conducting cross-border counterterrorism
 *   operations, with rising state practice but persistently contested
 *   customary status.
 *
 * KEY AGENTS:
 *   - intervening_states_with_counterterrorism_mandates: agenda-setter and beneficiary — invokes the doctrine, controls the threshold determination
 *   - host_states_with_contested_sovereignty: primary payer — territorial integrity bypassed on an externally-made finding
 *   - civilian_populations_in_host_territory: powerless payer — absorbs strike costs with no voice
 *   - international_court_of_justice and un_security_council: nominal checks that rarely bind in practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.58).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.62).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable/Unwilling Host-State Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'ca4415a0-c559-4a30-b032-cbc542114177').
narrative_ontology:cs_kernel_codification('ca4415a0-c559-4a30-b032-cbc542114177', fixed_text).
narrative_ontology:cs_authority_grounding('ca4415a0-c559-4a30-b032-cbc542114177', distributed).
narrative_ontology:cs_reading_relation('ca4415a0-c559-4a30-b032-cbc542114177', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca4415a0-c559-4a30-b032-cbc542114177', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('ca4415a0-c559-4a30-b032-cbc542114177', foundational, host_state_capacity_conditions_sovereignty_protection).
narrative_ontology:cs_axiom_status(host_state_capacity_conditions_sovereignty_protection, holdable).
narrative_ontology:cs_axiom_grounding('ca4415a0-c559-4a30-b032-cbc542114177', host_state_capacity_conditions_sovereignty_protection, conventional).
narrative_ontology:cs_axiom('ca4415a0-c559-4a30-b032-cbc542114177', foundational, attribution_to_host_state_is_not_required_for_lawful_response).
narrative_ontology:cs_axiom_status(attribution_to_host_state_is_not_required_for_lawful_response, holdable).
narrative_ontology:cs_axiom_grounding('ca4415a0-c559-4a30-b032-cbc542114177', attribution_to_host_state_is_not_required_for_lawful_response, empirically_contingent).
narrative_ontology:cs_reference_frame('ca4415a0-c559-4a30-b032-cbc542114177', post_charter_interstate_attribution_framework).
narrative_ontology:cs_drift_state('ca4415a0-c559-4a30-b032-cbc542114177', post_2001_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca4415a0-c559-4a30-b032-cbc542114177', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, regional_security_alliances).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_contested_sovereignty).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_territory).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, customary_international_law_evolves_through_state_practice).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, sovereignty_is_conditional_on_effective_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the unable/unwilling doctrine to justify cross-border strikes against non-state armed groups sheltering in another state's territory. They control the intelligence assessment of the host state's willingness/capacity, control the timing and scale of the response, and bear no binding external check on their own determination that the threshold was met.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, beneficiary).

% Have their territorial integrity bypassed on the intervening state's unilateral finding that they are unwilling or unable to suppress a threat. Often lack the military capacity to prevent either the underlying non-state actor's presence or the resulting strikes, and have no neutral forum that can bindingly adjudicate the 'unable/unwilling' determination before force is used against them.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_contested_sovereignty, payer,
    moderate, biographical, constrained, national).

% Live in the areas where non-state armed groups operate and where cross-border strikes land. They did not choose the presence of the non-state actor, cannot compel their own government's suppression capacity, and cannot exit the strike zone; they absorb the collateral costs of both the original attack and the responsive force.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_territory, payer,
    powerless, immediate, trapped, local).

% Gain a doctrinal basis for coordinated cross-border counterterrorism operations among allied states, expanding the legal cover available for joint strikes and intelligence-sharing arrangements framed as collective self-defense.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, regional_security_alliances, beneficiary,
    organized, generational, arbitrage, continental).

% Are the proximate cause invoked to trigger the doctrine but are not parties to the interstate legal dispute; they relocate across borders in response to strikes, which in practice can prolong the underlying instability the doctrine is invoked to end.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_actors, excluded,
    moderate, immediate, mobile, regional).

% Has never squarely endorsed the unable/unwilling standard in a binding judgment and has in related cases (Armed Activities, Nicaragua) applied a narrower attribution-based reading; it can only adjudicate disputes brought before it and cannot compel states to submit their self-defense determinations for prior review.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% Article 51 nominally requires reporting self-defense measures to the Council, but the Council rarely takes binding corrective action against a permanent member or its allies invoking the doctrine, leaving the reporting requirement largely a formality rather than a live check.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework allowing a state under attack from a non-state actor to respond to the actual source of ongoing violence when the territorial state genuinely cannot or will not act, avoiding a rule that would leave victim states with no lawful recourse against attacks staged from ungoverned or complicit territory.
% TRANSFER_FUNCTION: Moves the practical burden of managing non-state violence from the host state's sovereign prerogative to the intervening state's unilateral judgment, and moves the physical and political costs of enforcement from the intervening state's own territory onto the host state's territory and population.
% ABSENT_VOICES: The civilian populations who live where the non-state actors operate, and the host state's own domestic constituencies, have no seat in either the intervening state's threshold determination or in any binding international forum; the un_security_council and international_court_of_justice are structurally positioned to check the determination but rarely do so in practice.
% DISAPPEARANCE_RATIONALE: If the unable/unwilling doctrine were abandoned, states facing non-state actor attacks from ungoverned or complicit territory would be forced back onto either the narrow armed-attack/attribution standard (requiring proof of host-state responsibility) or onto Security Council authorization for cross-border force — both of which would significantly reduce the frequency and legal cover of unilateral cross-border counterterrorism strikes and shift practice toward multilateral consent-based responses.
% FOUNDING_PROBLEM: Classical Article 51 doctrine, built around interstate armed attack, left an apparent gap when attacks came from non-state actors based in territory the nominal sovereign could not or would not police (e.g., failed states, ungoverned borderlands), leaving victim states seemingly without lawful recourse against ongoing violence.
% FOUNDING_PROBLEM_CORROBORATION: Intervening states and allied international law scholars attest the gap is real and ongoing, citing cases like post-2001 counterterrorism operations. Independent international law scholars outside those states' governments, along with several host states and the ICJ's own more cautious jurisprudence, dispute that the doctrine has achieved settled customary status and argue it functions as a permissive gloss that expands unilateral force beyond what the founding gap requires.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because the doctrine transfers real costs — sovereignty bypass, strike damage, political destabilization — onto host states and their populations, but the transfer is bounded by the requirement of an actual prior attack, distinguishing it from the more extractive preventive reading. Suppression (0.62) reflects that the doctrine's persistence depends on intervening states' practical ability to act without a binding external check, not on host-state consent. Theater ratio (0.40) captures that reporting to the Security Council under Article 51 occurs but rarely produces substantive review — a partially performative compliance layer over a substantively unilateral practice. accessibility_collapse (0.45) is moderate: alternative frameworks (Security Council authorization, host-state consent, narrow attribution) remain legally available and are sometimes used, so alternatives have not fully collapsed. resistance (0.60) reflects genuine, sustained pushback from host states, many international law scholars, and non-aligned blocs at the UN who dispute the doctrine's customary status.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening state's seat, this is legitimate self-defense filling a genuine gap in classical doctrine. From the host state's seat, the same structure is a unilateral sovereignty override dressed in self-defense language, with the 'unable/unwilling' finding functioning as a discretionary trigger the intervening state itself controls. The engine's per-seat computation should reflect this divergence from the declared power/exit/scope data rather than from any narrative reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states sit near the beneficiary end: they set the threshold, control the intelligence and timing, and face no binding sanction for erroneous or self-serving unable/unwilling findings — their exit option is effectively arbitrage (they can choose whether and when to invoke the doctrine). Host states and their civilian populations sit near the target end: they bear the costs of an external determination that they had no part in making, and both have constrained-to-trapped exit options. Regional security alliances derive secondary benefit through expanded doctrinal cover without directly bearing the invocation costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (non-state violence from ungoverned or complicit territory leaving victim states without recourse) remains partly live, which prevents a clean mandatrophy verdict — this is why founding_problem_status is authored as contested rather than dead. The classification as tangled_rope rather than pure snare reflects that genuine coordination function persists (states facing real non-state threats from failed-state territory do lack good alternatives), while the asymmetric cost-imposition on host states and civilians, absent any binding external check on the threshold determination, constitutes the extractive component riding on that coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_status_of_unable_unwilling_standard,
    'Has the unable/unwilling standard achieved the status of binding customary international law, or does it remain a contested doctrinal claim advanced primarily by states that benefit from its permissiveness?',
    'A definitive ICJ ruling squarely addressing the standard (rather than the narrower attribution question addressed in Nicaragua and Armed Activities), or a clear, near-universal pattern of opinio juris among both intervening and host states rather than a pattern confined to a subset of militarily powerful states.',
    'If not yet customary, invocations under this doctrine are better characterized as unilateral practice seeking to generate custom rather than applications of settled law — raising the effective extraction and suppression scores further, since the constraint would then rest on power rather than shared legal obligation. If customary, the coordination function is more firmly established and the tangled_rope classification''s beneficiary/victim asymmetry would need to be weighed against a stronger claim of legal necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_status_of_unable_unwilling_standard, conceptual, 'Whether the unable/unwilling standard is settled custom or contested unilateral practice.').

omega_variable(
    threshold_determination_neutrality,
    'Can the ''unwilling or unable'' determination be made by a process meaningfully independent of the intervening state''s own strategic interest, or is self-determination of the threshold structurally inseparable from the doctrine as currently practiced?',
    'Comparative study of instances where intervening states'' unable/unwilling findings were later reviewed or contested by neutral international bodies, versus instances where no external review occurred at all.',
    'If threshold determination is structurally inseparable from self-interested unilateral judgment, the doctrine functions closer to a discretionary license than a legal standard, pushing the classification toward snare; if credible independent review mechanisms exist or emerge, the tangled_rope''s coordination component is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_determination_neutrality, empirical, 'Whether the unable/unwilling threshold can be neutrally adjudicated or is inherently self-judged.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the same Article 51 text and the same body of state practice can be read as instantiating the narrow, unable/unwilling, or expansive readings, is the choice among these three readings itself a legal determination or a policy preference dressed as legal interpretation?',
    'Track whether states'' choice of reading correlates more strongly with their military capability and target-state relationships (suggesting policy preference) or with consistent application across cases regardless of the state''s own interests (suggesting genuine legal interpretation).',
    'If reading selection correlates with capability and target relationships, this reading''s account of ''genuine coordination gap'' is weaker than authored, and the doctrine is closer to opportunistic reading-selection than principled interpretation of a stable kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether kernel-reading choice tracks legal reasoning or state capability and interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t5, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(arti_tr_t10, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(arti_tr_t25, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(arti_be_t5, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(arti_be_t10, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(arti_be_t25, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(arti_su_t5, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(arti_su_t10, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(arti_su_t25, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, expansive_preventive_reading).

% DUAL FORMULATION NOTE:
% This story is the middle reading of a three-member kernel family (article_51_self_defense). narrow_armed_attack_reading has lower ε (requires state attribution, tightly bounded scope of lawful response) and is structurally the most rope-like of the three. expansive_preventive_reading has higher ε (permits preventive force without a completed attack, weakest sovereignty protection for the targeted state) and is structurally the most snare-like. This unable_unwilling_doctrine_reading sits between them: it requires an actual attack (bounding it below the preventive reading) but relaxes attribution to the host state (extending it beyond the narrow reading). Each reading is authored with its own stable ε and its own beneficiary/victim structure per the ε-invariance principle; they are linked here rather than merged into one variable-ε story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
