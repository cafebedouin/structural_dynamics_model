% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Customary Freedom of Navigation Enforced by Naval Presence (Non-Ratifier Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the non-ratifier enforcement reading of the
 *   unclos_sovereignty_boundary kernel. It treats freedom of navigation as
 *   customary international law binding all states regardless of UNCLOS
 *   ratification, enforced through physical naval presence and FONOPS. Naval
 *   powers (notably the United States) benefit from unimpeded global mobility
 *   without treaty obligations; coastal states attempting to enforce EEZ
 *   exclusivity bear the costs of overridden regulatory authority. The
 *   constraint's structure decouples maritime legal order from the UNCLOS
 *   text, generating tension with ratifier states that view the treaty as the
 *   comprehensive framework. The claim/metric independence is maintained: the
 *   reading claims a coordination function (open sea lanes) while the metrics
 *   register substantial extraction from coastal states and rising
 *   enforcement dependence.
 *
 * KEY AGENTS:
 *   - Naval powers (institutional/arbitrage): Primary agenda-setters and beneficiaries â enforce customary navigation rights through naval operations without UNCLOS ratification.
 *   - Coastal states EEZ claimants (organized/constrained): Primary payers â seek to enforce EEZ authority but are structurally overridden by naval presence.
 *   - Merchant marine operators (moderate/mobile): Secondary beneficiaries â gain open routes without enforcement burden.
 *   - UNCLOS ratifier coalition (organized/analytical): Observers â defend treaty framework but lack enforcement leverage over non-ratifiers.
 *   - International tribunals (institutional/analytical): Analytical observers â interpret law without enforcement capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.72).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.78).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Customary Freedom of Navigation Enforced by Naval Presence (Non-Ratifier Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46').
narrative_ontology:cs_kernel_codification('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', distributed).
narrative_ontology:cs_authority_grounding('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', practice).
narrative_ontology:cs_interpretation_layer_present('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46').
narrative_ontology:cs_reading_relation('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', unclos_sovereignty_boundary__strict_eez_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', foundational, customary_navigation_independent_of_treaty).
narrative_ontology:cs_axiom_status(customary_navigation_independent_of_treaty, holdable).
narrative_ontology:cs_axiom_grounding('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', customary_navigation_independent_of_treaty, conventional).
narrative_ontology:cs_axiom('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', foundational, naval_presence_as_legitimate_enforcement).
narrative_ontology:cs_axiom_status(naval_presence_as_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', naval_presence_as_legitimate_enforcement, conventional).
narrative_ontology:cs_reference_frame('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', customary_maritime_openness).
narrative_ontology:cs_drift_state('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', contemporary_eez_contest_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4cb6bcbc-4d35-4cf0-b8df-bd5bf636eb46', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, merchant_marine_operators).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_claimants).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, customary_international_law_supremacy).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, freedom_of_navigation_universal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert freedom of navigation as binding customary international law independent of UNCLOS ratification. Conduct freedom-of-navigation operations (FONOPS) to challenge coastal state EEZ claims, deploying naval assets to physically demonstrate non-acceptance of excessive maritime claims. Derive strategic mobility and global power projection from unimpeded maritime access without treaty obligation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Seek to exercise regulatory authority over exclusive economic zones per UNCLOS or historical entitlement, including resource management and maritime law enforcement. Face operational challenge by foreign naval vessels asserting transit rights; diplomatic protest is the primary available response, while military exclusion of major naval powers is not viable.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_claimants, payer,
    organized, generational, constrained, regional).

% Rely on legally protected freedom of navigation for international commercial shipping routes. Benefit from naval powers' enforcement of open sea lanes without directly bearing the enforcement cost or political friction with coastal states.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, merchant_marine_operators, beneficiary,
    moderate, biographical, mobile, global).

% Uphold UNCLOS as the comprehensive legal framework for maritime rights. View non-ratifier assertions of customary navigation law as eroding treaty-based order, but lack coercive mechanism to compel naval powers to ratify or desist from unilateral enforcement.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, uncos_ratifier_coalition, observer,
    organized, generational, analytical, global).

% Adjudicate maritime disputes on the basis of UNCLOS and customary international law. Jurisprudence on EEZ rights versus navigation freedoms is cited by all parties, but tribunals lack enforcement capacity against non-ratifying naval powers.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_tribunals, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unilateral coastal state enclosure of international maritime corridors; maintains open sea lanes for global commercial and naval mobility without requiring universal treaty ratification.
% TRANSFER_FUNCTION: Transfers EEZ regulatory authority from coastal states to the international maritime community as enforced by naval powers; transfers strategic mobility advantage to naval powers and commercial operators.
% ABSENT_VOICES: Small island developing states whose EEZ resources constitute their primary economic base but lack naval capacity to participate in enforcement discourse; indigenous maritime communities with traditional sea-use rights overridden by both EEZ claims and freedom-of-navigation assertions.
% DISAPPEARANCE_RATIONALE: If customary freedom of navigation enforced by naval presence vanished, coastal states would assert fuller EEZ exclusivity, major maritime chokepoints might face restricted access, global commercial routing would face regulatory fragmentation, and the UNCLOS treaty framework would become the sole legitimate reference point for maritime boundaries.
% FOUNDING_PROBLEM: Prevented unilateral coastal state enclosure of maritime corridors that would fragment global sea lanes and enable coastal powers to deny passage to rival states.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers and international law scholars in the Anglo-American tradition attest to the ongoing need for open sea lanes. UNCLOS negotiators and ratifier states outside the major naval-power bloc attest that the problem was addressed by treaty and that customary-law assertions now undermine the negotiated framework; international tribunal jurisprudence provides mixed corroboration.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically transfers EEZ regulatory authority from coastal states to naval powers under a legal framework the latter have not ratified. Suppression (0.78) exceeds extractiveness because persistence depends on active naval enforcement and the diplomatic marginalization of coastal state objections; the constraint would collapse without continuous naval operations. Theater ratio (0.45) reflects that a growing share of FONOPS activity is performative assertion of legal position rather than response to specific threats. Accessibility collapse (0.65) captures that coastal states have limited structural alternatives to accepting naval transit. Resistance (0.70) registers active diplomatic and legal pushback from major coastal states. The measurement series share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The naval-power seat and the coastal-state seat compute divergent classifications. From the naval-power perspective, the arrangement is necessary coordination preventing coastal enclosure of global commons; from the coastal-state perspective, it is asymmetric extraction of sovereignty by actors who declined to bind themselves to the treaty they enforce. The engine computes this divergence from the structural data â beneficiary declarations, constrained exit, and active enforcement â without reconciling the contradiction.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers are structurally positioned as beneficiaries (low d) because the constraint subsidizes their global mobility and power projection; their arbitrage-grade exit (can adjust enforcement intensity or legal framing) further dampens effective extraction. Coastal states are victims (high d) because the constraint extracts regulatory sovereignty from them and their exit is constrained to diplomatic protest. Merchant marine operators sit near symmetric: genuine coordination benefit from open lanes, but no enforcement cost. The UNCLOS ratifier coalition and tribunals are analytical seats with no directional extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing coastal enclosure of sea lanes) is contested: naval powers claim it is still live, while UNCLOS ratifiers argue it was solved by treaty. The mismatch between contested founding_problem_status and world_rearranges disappearance_verdict flags the constraint as potentially carrying mandatrophy risk â it may persist beyond its treaty-substitution rationale. However, the active enforcement profile and genuine coordination benefit to global shipping prevent automatic piton classification; the theater ratio does not yet dominate function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_authenticity,
    'Is the freedom of navigation norm genuinely binding customary international law independent of UNCLOS, or a post-hoc naval-power privilege framed as universal law?',
    'Systematic state-practice survey measuring opinio juris among non-naval states; ICJ advisory opinion on the relationship between UNCLOS treaty text and parallel customary navigation obligations.',
    'If merely naval-power privilege, the constraint''s coordination story is cover and the engine should recompute toward snare; if genuine customary law, the coordination function is structurally grounded and tangled_rope remains valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_authenticity, conceptual, 'Whether customary law status is authentic or constructed privilege').

omega_variable(
    naval_enforcement_character,
    'Does naval presence (FONOPS) constitute legitimate enforcement of international law or unilateral military coercion dressed in legal language?',
    'Comparative analysis of coastal state consent to such operations; UN General Assembly resolution patterns condemning or affirming specific operations; proportionality of naval response to excessive maritime claims.',
    'If coercion, suppression and theater_ratio are higher than structurally described; if legitimate enforcement, the active enforcement requirement is procedurally validated rather than extractive overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naval_enforcement_character, empirical, 'Naval operations as law enforcement vs unilateral coercion').

omega_variable(
    kernel_reading_position,
    'This constraint is the non-ratifier enforcement reading of the unclos_sovereignty_boundary kernel. How would sibling readings redistribute beneficiaries and victims?',
    'Comparative structural analysis of the sibling constraints generated from this kernel (strict_eez_reading and historical_rights_reading).',
    'Confirms that this reading is structurally distinct in decoupling enforcement from treaty text and in locating naval powers as beneficiaries rather than UNCLOS-compliant coastal states or historical-occupation claimants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural position of this reading within the kernel family').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_nonratifier_tr_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(unclos_nonratifier_tr_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(unclos_nonratifier_tr_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(unclos_nonratifier_tr_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(unclos_nonratifier_tr_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(unclos_nonratifier_tr_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(unclos_nonratifier_be_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(unclos_nonratifier_be_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(unclos_nonratifier_be_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(unclos_nonratifier_be_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(unclos_nonratifier_be_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(unclos_nonratifier_be_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(unclos_nonratifier_su_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(unclos_nonratifier_su_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(unclos_nonratifier_su_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(unclos_nonratifier_su_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(unclos_nonratifier_su_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(unclos_nonratifier_su_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, historical_rights_reading).

% DUAL FORMULATION NOTE:
% The kernel unclos_sovereignty_boundary decomposes into three structurally distinct constraints: strict_eez_reading (treaty-text grounding), historical_rights_reading (occupation-based grounding), and this reading (customary-law naval-enforcement grounding). Each has a different beneficiary/victim structure and epsilon value; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
