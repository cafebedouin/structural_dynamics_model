% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense â Unable or Unwilling Doctrine Reading
 *   domain: international law/security studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'unable or unwilling' reading of
 *   Article 51 self-defense under the UN Charter. In this reading, a
 *   non-state actor attack originating from a host state that is unwilling or
 *   unable to suppress the threat triggers the victim state's right to use
 *   unilateral military force in the host state's territory, bypassing host
 *   state consent and UNSC authorization. The constraint is a hybrid: it
 *   coordinates counterterrorism action against safe havens while
 *   asymmetrically extracting sovereignty from host states. The story is
 *   authored from this reading only; sibling readings (narrow armed attack,
 *   expansive preventive) are separate constraints linked via the commitment
 *   system structure.
 *
 * KEY AGENTS:
 *   - intervening_states: Primary agenda-setter (institutional/global/arbitrage) â invokes the doctrine, conducts cross-border operations, captures legal justification.
 *   - host_states: Primary payer (institutional/national/constrained) â sovereignty bypassed, territorial integrity compromised, limited exit.
 *   - un_security_council: Observer (institutional/global/analytical) â collective security authority circumvented.
 *   - international_legal_scholars: Observer (organized/global/analytical) â debate legitimacy without enforcement capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.6).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense â Unable or Unwilling Doctrine Reading").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international law/security studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '230ce95a-f364-4477-a986-20c8703c8216').
narrative_ontology:cs_kernel_codification('230ce95a-f364-4477-a986-20c8703c8216', formalized).
narrative_ontology:cs_authority_grounding('230ce95a-f364-4477-a986-20c8703c8216', lineage).
narrative_ontology:cs_interpretation_layer_present('230ce95a-f364-4477-a986-20c8703c8216').
narrative_ontology:cs_reading_relation('230ce95a-f364-4477-a986-20c8703c8216', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('230ce95a-f364-4477-a986-20c8703c8216', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_axiom('230ce95a-f364-4477-a986-20c8703c8216', foundational, non_state_attacks_qualify_as_armed_attack).
narrative_ontology:cs_axiom_status(non_state_attacks_qualify_as_armed_attack, holdable).
narrative_ontology:cs_axiom_grounding('230ce95a-f364-4477-a986-20c8703c8216', non_state_attacks_qualify_as_armed_attack, conventional).
narrative_ontology:cs_axiom('230ce95a-f364-4477-a986-20c8703c8216', foundational, host_state_incapacity_permits_territorial_bypass).
narrative_ontology:cs_axiom_status(host_state_incapacity_permits_territorial_bypass, holdable).
narrative_ontology:cs_axiom_grounding('230ce95a-f364-4477-a986-20c8703c8216', host_state_incapacity_permits_territorial_bypass, conventional).
narrative_ontology:cs_reference_frame('230ce95a-f364-4477-a986-20c8703c8216', sovereign_responsibility_to_suppress).
narrative_ontology:cs_drift_state('230ce95a-f364-4477-a986-20c8703c8216', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('230ce95a-f364-4477-a986-20c8703c8216', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that invoke Article 51 under the unable or unwilling doctrine to conduct cross-border military operations against non-state actors in host states. They gain legal justification for unilateral force without host state consent or UNSC authorization, framing their actions as counterterrorism necessity.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states, agenda_setter,
    institutional, generational, arbitrage, global).

% States from whose territory non-state actor threats originate and whose sovereignty is bypassed when intervening states unilaterally use force. They bear the costs of territorial violation, civilian casualties, and loss of regulatory control, with limited legal or practical means to prevent the intervention.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states, payer,
    institutional, generational, constrained, national).

% The primary body for authorizing collective security measures under the UN Charter, often bypassed by intervening states invoking the unable or unwilling doctrine. Its Chapter VII functions are circumvented, weakening the collective security architecture.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% Academic and practitioner jurists who debate whether the doctrine constitutes a valid interpretation of Article 51 or an expansionist rewriting of the Charter. Their opinions shape but do not determine state practice.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_legal_scholars, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international counterterrorism action against non-state actor safe havens when the territorial state lacks the capacity or will to suppress the threat, preventing terrorist bases from enjoying sovereign sanctuary.
% TRANSFER_FUNCTION: Transfers legal authority to use unilateral military force from the host state (territorial sovereignty) to the intervening state (counterterrorism self-defense), bypassing the requirement for host state consent or UNSC authorization.
% ABSENT_VOICES: Host state civilian populations, non-state actor constituencies, and Global South states skeptical of Western counterterrorism practices are largely excluded from the legal formalism; the UNSC is structurally bypassed rather than consulted.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, intervening states would lose their primary legal justification for unilateral cross-border strikes against non-state actors; host states would regain stronger territorial sovereignty protections; counterterrorism practice would shift toward host-state-consent models or UNSC authorization, fundamentally rearranging the global security architecture.
% FOUNDING_PROBLEM: The emergence of transnational non-state actor safe havens in weak, failed, or complicit states after the Cold War, which existing state-on-state self-defense frameworks and cumbersome UNSC authorization processes could not effectively address.
% FOUNDING_PROBLEM_CORROBORATION: Western counterterrorism officials and some jurists attest the problem remains live. However, the UN General Assembly and many Global South states contest that the doctrine was the appropriate solution, arguing it manufactures exceptions to sovereignty; ICJ advisory opinions and dissenting opinions have questioned the doctrinal expansion. Corroboration from outside the beneficiary set is mixed and politically divided.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) reflects the substantial cost to host state sovereignty and the systematic bypass of consent mechanisms. Suppression (0.60) captures the doctrinal suppression of traditional territorial sovereignty claims and the lack of effective legal remedies for host states. Theater ratio (0.40) indicates moderate performative legalism: intervening states produce elaborate legal opinions to justify strikes, but the underlying driver is strategic interest as much as security necessity. Accessibility collapse (0.50) reflects that alternatives (UNSC authorization, host state consent) remain formally open but are practically marginalized by the doctrine's availability. Resistance (0.55) captures persistent objections from host states, the Global South, and some ICJ judges. The temporal series show extraction and theater rising through the 2000s-2010s, plateauing as the doctrine became normalized.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening state seat, the constraint is a necessary coordination mechanism to close safe-haven gaps left by weak governance; from the host state seat, it is an enforced extraction of sovereignty that removes their legal veto over foreign military operations on their territory. The engine computes this divergence from the structural asymmetry in exit options (arbitrage vs constrained) and the beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states are declared beneficiaries with arbitrage-grade exit (they can choose whether to invoke the doctrine or pursue alternatives); this drives d toward the beneficiary end, reducing effective extraction for them. Host states are declared victims with constrained exit (they cannot easily leave the UN Charter system or prevent territorial incursions); this drives d toward the target end, amplifying effective extraction. The UNSC and legal scholars occupy analytical exit with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â transnational terrorist safe havens â is contested as still live, and the doctrine has expanded beyond its original counterterrorism context. However, the constraint retains a genuine coordination function (preventing safe havens) that prevents classification as pure snare. The divergence between the coordination story and the sovereignty extraction marks it as tangled rope rather than rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the unable or unwilling doctrine represent a structurally distinct legal standard or merely a post-hoc justification for geopolitical intervention by powerful states?',
    'Comparative state practice analysis examining whether intervening states apply the doctrine consistently across all unable or unwilling host states or selectively based on strategic alignment.',
    'If selective application is demonstrated, the constraint functions more as a snare (legal cover for extraction) than a tangled rope; if consistently applied, the coordination function against safe havens is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the doctrine is a genuine legal standard or post-hoc geopolitical cover.').

omega_variable(
    customary_vs_constructed_norm,
    'Is the unable or unwilling doctrine an emergent customary norm reflecting broad state consensus, or a constructed exception to sovereignty manufactured by militarily powerful states?',
    'ICJ rulings, UN General Assembly resolution votes, and systematic review of state practice and opinio juris regarding cross-border self-defense claims.',
    'If constructed without broad consent, classification shifts toward snare or piton; if emergent customary law, classification may remain tangled rope or approach rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_vs_constructed_norm, empirical, 'Customary law emergence vs manufactured exception.').

omega_variable(
    sovereignty_suppression_mechanism,
    'Is the suppression of host state sovereignty under this doctrine structural (the legal doctrine removes their veto in international law) or internalized (weak host states accept the doctrine as legitimate)?',
    'Pattern analysis of host state diplomatic protests, ICJ cases brought by host states, and acquiescence versus objection in state practice.',
    'If internalized, effective suppression exceeds structural metrics; if structural, host state resistance should manifest openly when capacity permits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_suppression_mechanism, empirical, 'Structural vs internalized suppression of host state sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t5, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(arti_tr_t10, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(arti_tr_t25, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(arti_be_t5, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(arti_be_t10, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(arti_be_t25, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(arti_su_t5, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(arti_su_t10, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(arti_su_t25, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 25, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
