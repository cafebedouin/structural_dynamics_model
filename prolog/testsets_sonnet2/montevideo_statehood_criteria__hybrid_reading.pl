% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Hybrid (Objective-Criteria-Plus-Legitimacy) Reading of Statehood
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the Montevideo statehood
 *   kernel: statehood requires not only the classic objective criteria
 *   (defined territory, permanent population, effective government, capacity
 *   to enter relations) but also normative legitimacy — democratic
 *   governance, human rights compliance, non-aggression. Under this reading,
 *   entities that satisfy the objective test but govern illiberally or
 *   emerged through force are treated as legally deficient claimants. The
 *   reading is distinct from, and not a synthesis of, the declaratory reading
 *   (objective criteria alone establish statehood as fact) and the
 *   constitutive reading (recognition by the existing community of states is
 *   itself constitutive). Each of the three readings is authored as its own
 *   constraint with its own epsilon; this file does not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.66).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.58).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Hybrid (Objective-Criteria-Plus-Legitimacy) Reading of Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, 'f7d9f291-731c-487f-8dd8-edadb5dbb4ec').
narrative_ontology:cs_kernel_codification('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', formalized).
narrative_ontology:cs_authority_grounding('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', distributed).
narrative_ontology:cs_reading_relation('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', foundational, normative_legitimacy_is_a_statehood_prerequisite).
narrative_ontology:cs_axiom_status(normative_legitimacy_is_a_statehood_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', normative_legitimacy_is_a_statehood_prerequisite, deontological).
narrative_ontology:cs_axiom('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', secondary, objective_criteria_remain_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(objective_criteria_remain_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', objective_criteria_remain_necessary_but_insufficient, conventional).
narrative_ontology:cs_reference_frame('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', montevideo_1933_objective_baseline).
narrative_ontology:cs_drift_state('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', post_cold_war_liberal_order, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f7d9f291-731c-487f-8dd8-edadb5dbb4ec', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_recognition_bloc).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, intervening_powers).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, existing_un_member_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_de_facto_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, populations_of_unrecognized_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, aspiring_democratic_secessionists).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, aspiring_democratic_secessionists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Western-aligned states and blocs that condition recognition of new or aspirant states on evidence of democratic governance, human rights protection, and non-aggression. They administer the additional normative layer on top of the Montevideo criteria, deciding case by case which aspirant polities qualify, and gain a legally articulable basis for withholding recognition from governments they oppose on other grounds.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_recognition_bloc, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_recognition_bloc, beneficiary).

% States and coalitions that invoke the normative-legitimacy layer to justify humanitarian intervention, non-recognition of governments arising from coups or authoritarian consolidation, and support for regime change. The hybrid standard supplies legal cover that a purely objective four-criteria test would not.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, intervening_powers, beneficiary,
    institutional, biographical, arbitrage, global).

% Incumbent states benefit from a standard that raises the bar for new entrants, especially secessionist movements from within their own or allied territory, slowing the proliferation of new claimants and preserving the current membership's relative influence in international bodies.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, existing_un_member_states, beneficiary,
    institutional, generational, constrained, global).

% Groups that otherwise satisfy the objective Montevideo criteria (defined territory, permanent population, government, capacity for relations) but whose internal governance is non-democratic, illiberal, or organized along lines the recognizing bloc distrusts. Under the hybrid standard their claim to statehood is denied or deferred not for factual insufficiency but for failing an added normative test they had no voice in setting.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    powerless, biographical, trapped, regional).

% Territories exercising effective, functioning control that meet the classic four criteria but govern through authoritarian or human-rights-violating structures. They are treated as legally deficient states — denied normal diplomatic status, financial access, and treaty capacity — despite functioning exactly as the declaratory test would recognize.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, authoritarian_de_facto_states, payer,
    moderate, biographical, constrained, regional).

% Ordinary residents of territories caught in the recognition gap bear the practical costs — no passports honored abroad, no access to international financial systems, no standing in international courts, no protection under many treaties — regardless of their own preferences about their government's character.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, populations_of_unrecognized_territories, payer,
    powerless, biographical, trapped, local).

% Academics and jurists who analyze whether the added normative layer is a coherent extension of customary international law or a discretionary tool dressed in legal language. They document the divergence between the hybrid standard's stated criteria and its case-by-case application.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% Secessionist or independence movements that do organize along democratic and rights-respecting lines benefit from the hybrid standard relative to non-liberal rivals, gaining a stronger claim to recognition — but they still pay in delay, conditionality, and dependence on external powers' judgment of their legitimacy, which can be revoked or contested.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, aspiring_democratic_secessionists, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, aspiring_democratic_secessionists, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the international community with a shared, articulable standard for deciding which entities may participate as states in the state system, coordinating recognition decisions across many actors so that recognition is not purely ad hoc or unilateral.
% TRANSFER_FUNCTION: Moves the practical benefits of statehood — treaty capacity, financial system access, diplomatic standing, territorial integrity guarantees — toward entities whose internal governance satisfies liberal-democratic normative criteria, and withholds those same benefits from entities that meet the objective territorial/governmental criteria but fail the normative overlay.
% ABSENT_VOICES: Non-liberal secessionist movements and authoritarian de facto states have no seat in defining what counts as sufficient 'democratic governance' or 'human rights compliance' — the criteria are set and applied by the very bloc whose recognition they are seeking, with no independent adjudicative body binding on all parties.
% DISAPPEARANCE_RATIONALE: If the normative-legitimacy overlay disappeared and recognition reverted to the pure declaratory (objective-criteria) standard, several de facto authoritarian states and non-liberal secessionist movements would gain grounds for recognition claims they currently lack, intervening powers would lose a legal vocabulary for regime-change justification, and the current pattern of selective non-recognition would collapse into a more mechanical test.
% FOUNDING_PROBLEM: The interwar and postwar international order needed a way to distinguish genuine, functioning polities from puppet states, colonial fictions, and territorial claims lacking real governmental capacity — the original Montevideo criteria (territory, population, government, capacity for relations) solved this by naming objective, checkable facts.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars attest that the objective four-criteria problem was substantially solved by the 1933 Montevideo formulation itself; the added normative layer addresses a different and later-arising problem (delegitimizing governance, denying recognition to human-rights-violating regimes) that liberal democratic states and intervening powers assert independently, while non-liberal secessionist movements and authoritarian de facto states dispute that any additional problem needed solving beyond the original objective test.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.66 by interval end because the normative overlay functions as a discretionary gate layered on top of a nominally objective test, systematically denying the practical fruits of statehood to entities that meet the classic criteria but fail a criterion set and adjudicated by the recognizing bloc itself. Suppression (0.58) reflects that non-recognition is enforced through exclusion from financial systems, treaty regimes, and diplomatic channels — a real coercive mechanism, not merely rhetorical. Theater ratio (0.42) captures that a substantial share of invoked 'legitimacy' assessment functions as post-hoc justification for recognition decisions already made on strategic grounds.
 *
 * PERSPECTIVAL GAP:
 *   From the recognizing bloc's seat, the hybrid standard is principled coordination — a refinement that keeps statehood tethered to legitimate governance rather than mere territorial control. From the seat of a functioning but illiberal de facto state, the identical structure is enforced extraction: a moving target that denies recognized benefits despite factual qualification. The engine's per-seat computation is expected to diverge sharply between the agenda_setter/beneficiary seats and the payer seats — that divergence is the data point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states, existing UN members, and intervening powers are the structural beneficiaries: they set the normative criteria, apply them selectively, and gain both a slowed rate of new-entrant proliferation and a legal vocabulary for intervention and non-recognition. Non-liberal secessionists and authoritarian de facto states are targets: they meet the classic four criteria yet are denied the resulting standing, with no voice in defining the additional test. Populations within unrecognized territories are collateral targets, bearing costs regardless of their preferences about their own government.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem the hybrid layer purports to solve — distinguishing genuine governance capacity from puppet or captured territorial control — is largely already addressed by the objective declaratory criteria's 'effective government' prong. The additional normative layer arguably targets a different, later problem (delegitimizing regimes on human-rights or democratic grounds) that has its own justification but is not required to complete the original 1933 project. Treating the hybrid standard as a mandatory extension of statehood law, when it functions as a discretionary foreign-policy tool with legal vocabulary, is the mandatrophy risk this story flags.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_versus_siblings_location,
    'Where exactly does the hybrid reading''s disagreement with the declaratory and constitutive readings live — in the criteria themselves, in who adjudicates them, or in both?',
    'Compare state practice: cases where the objective criteria are met but recognition is withheld on normative grounds (hybrid vs. declaratory divergence) versus cases where normative compliance is present but recognition is withheld purely on political will (hybrid vs. constitutive divergence).',
    'If most contested cases turn on the criteria (normative overlay doing the work), the hybrid reading is functionally a stricter declaratory test. If most contested cases turn on adjudicator discretion regardless of stated criteria, the hybrid reading collapses toward the constitutive reading in practice despite its declared objective floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_versus_siblings_location, conceptual, 'Locating the structural disagreement between the three sibling readings of the Montevideo kernel.').

omega_variable(
    normative_criteria_content_indeterminacy,
    'Is ''democratic governance, human rights, non-aggression'' a stable, checkable normative standard, or is it indeterminate enough that its application reduces to the political preference of the recognizing bloc?',
    'Track consistency of application across cases with similar governance profiles but different geopolitical alignment with the recognizing bloc — inconsistency under similar facts would indicate the standard functions as discretion dressed as law.',
    'If the standard is genuinely stable and checkable, the hybrid reading is a defensible extension of statehood law with real normative content. If it is indeterminate and applied inconsistently by alignment, the reading functions primarily as a discretionary extraction and intervention-justification mechanism, supporting the tangled_rope classification over a rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_criteria_content_indeterminacy, empirical, 'Whether the added normative criteria have stable content or collapse into political discretion.').

omega_variable(
    beneficiary_versus_genuine_legitimacy_concern,
    'Do liberal democratic states and intervening powers hold the normative overlay because they sincerely believe democratic governance and human rights are prerequisites for legitimate statehood, or because the overlay serves their strategic interest in gatekeeping new entrants and justifying intervention?',
    'Examine cases where the normative overlay would require the recognizing bloc to withhold recognition from its own strategic allies who fail the same normative tests — consistent application against self-interest would support sincere belief; inconsistent application would support strategic use.',
    'Sincere and consistent application would push the constraint toward a genuine rope (coordination on shared values); documented strategic inconsistency would support the tangled_rope or even snare-leaning reading authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_versus_genuine_legitimacy_concern, empirical, 'Sincerity versus strategic instrumentalization of the normative criteria by their primary beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mont_tr_t8, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(mont_tr_t16, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(mont_tr_t24, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(mont_tr_t32, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(mont_tr_t40, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mont_be_t8, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(mont_be_t16, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(mont_be_t24, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(mont_be_t32, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(mont_be_t40, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 40, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mont_su_t8, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(mont_su_t16, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(mont_su_t24, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(mont_su_t32, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(mont_su_t40, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'Montevideo statehood criteria' concept per the ε-invariance principle. The declaratory reading (objective criteria alone) carries substantially lower extraction — it is closer to a rope, since meeting checkable facts is not itself discretionary. The constitutive reading (recognition by the existing community) carries a different extraction profile again, since it makes NO objective floor claim at all and is driven entirely by incumbent-state will. The hybrid reading authored here sits between them structurally but is not an average of their epsilon values — it is a distinct constraint with its own beneficiary/victim structure (non-liberal secessionists newly enter the victim set specifically because of the normative overlay, which neither sibling reading imposes in the same way).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
