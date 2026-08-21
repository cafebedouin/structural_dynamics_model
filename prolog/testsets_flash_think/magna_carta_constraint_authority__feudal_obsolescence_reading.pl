% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta's Feudal Obsolescence as a Constraint on Modern Sovereignty
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'feudal obsolescence' reading of Magna
 *   Carta, which asserts that the 13th-century baronial compact has no
 *   binding authority over modern sovereignty structures. This reading, often
 *   advanced by those in power, effectively removes a historical check on
 *   executive and parliamentary discretion. The constraint is classified as a
 *   Snare because the claim of obsolescence, while historically grounded in
 *   its original context, serves as a cover story to enable the extraction of
 *   power and discretion by modern sovereign entities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.65).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.7).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, snare).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta's Feudal Obsolescence as a Constraint on Modern Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__feudal_obsolescence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'c849b4aa-5b83-4353-8f9d-e3f6907a43d7').
narrative_ontology:cs_kernel_codification('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', fixed_text).
narrative_ontology:cs_authority_grounding('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', extraction).
narrative_ontology:cs_interpretation_layer_present('c849b4aa-5b83-4353-8f9d-e3f6907a43d7').
narrative_ontology:cs_reading_relation('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', foundational, magna_carta_is_feudal_compact).
narrative_ontology:cs_axiom_status(magna_carta_is_feudal_compact, holdable).
narrative_ontology:cs_axiom_grounding('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', magna_carta_is_feudal_compact, conventional).
narrative_ontology:cs_axiom('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', foundational, modern_sovereignty_unbound_by_feudal_compacts).
narrative_ontology:cs_axiom_status(modern_sovereignty_unbound_by_feudal_compacts, holdable).
narrative_ontology:cs_axiom_grounding('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', modern_sovereignty_unbound_by_feudal_compacts, conventional).
narrative_ontology:cs_reference_frame('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', feudal_compact_1215).
narrative_ontology:cs_drift_state('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', contemporary_sovereignty_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('c849b4aa-5b83-4353-8f9d-e3f6907a43d7', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_power).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_majority).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, citizens_seeking_rights_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the claim that Magna Carta holds no binding authority over modern sovereignty, allowing for maximal executive discretion and minimizing historical constitutional checks on power.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_power, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the argument that Magna Carta's relevance is absorbed into statute law or is entirely obsolete, reinforcing parliamentary supremacy and reducing external constitutional constraints on legislative action.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_majority, beneficiary,
    institutional, generational, arbitrage, national).

% Bear the cost of their arguments for Magna Carta's enduring relevance being dismissed. They seek to invoke its principles as a living document or a foundational text for popular sovereignty, but face institutional resistance.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists, payer,
    organized, generational, constrained, national).

% Argue for judicial enforcement of constitutional principles derived from Magna Carta. Their efforts are undermined by the claim of its feudal obsolescence, limiting the scope for judicial review based on historical charters.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_advocates, payer,
    organized, generational, constrained, national).

% Experience the practical consequences of reduced constitutional checks on power, as avenues for challenging state action based on historical rights or due process principles derived from Magna Carta are foreclosed.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, citizens_seeking_rights_protection, payer,
    powerless, biographical, constrained, national).

% Analyze the historical context and original intent of Magna Carta, often corroborating its feudal origins, which is then leveraged by political actors to argue for its modern irrelevance.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, historical_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_power).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading primarily coordinates the dismissal of historical constitutional constraints, allowing modern sovereign entities to operate with greater perceived freedom from ancient charters.
% TRANSFER_FUNCTION: Transfers discretion and power from historical constitutional limits (as represented by Magna Carta) to modern executive and parliamentary bodies, effectively removing a potential check on their authority.
% ABSENT_VOICES: Those who adhere to a 'higher law' tradition, natural rights philosophy, or an evolutionary view of constitutionalism, whose arguments for Magna Carta's enduring moral or legal force are actively marginalized by the obsolescence claim.
% DISAPPEARANCE_RATIONALE: If the claim of Magna Carta's feudal obsolescence vanished, the debate over its modern relevance would intensify dramatically. This would likely lead to new legal challenges, shifts in constitutional interpretation, and potentially a re-assertion of historical checks on executive and parliamentary power, fundamentally reorganizing the legal and political landscape.
% FOUNDING_PROBLEM: To resolve specific feudal grievances between King John and his barons in 13th century England, primarily concerning arbitrary taxation, feudal dues, and the administration of justice.
% FOUNDING_PROBLEM_CORROBORATION: Historical consensus among scholars (e.g., historical_scholars) widely corroborates the specific feudal context and limited scope of Magna Carta's original intent, supporting the claim that its founding problems are no longer live in a modern context. This historical fact is then used by executive and parliamentary powers to argue for its modern irrelevance.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the degree to which modern power operates without the restraint that Magna Carta's principles might otherwise impose. Suppression (0.70) is high because this reading actively dismisses and marginalizes alternative interpretations that seek to apply Magna Carta's principles to contemporary governance. The theater ratio (0.45) indicates that while Magna Carta may be ceremonially acknowledged, its substantive role as a constraint is largely performative, with its historical context emphasized to deny its modern legal force. The increasing trends in extractiveness and suppression over the interval reflect a growing assertion of modern sovereign power and a more active dismissal of historical constitutional checks.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of executive and parliamentary power, this reading is a pragmatic recognition of historical fact, allowing for efficient modern governance. From the perspective of those advocating for constitutional restraint, it is a strategic dismissal of foundational principles that enables unchecked power. The engine's classification as a Snare highlights this divergence, showing how a historical interpretation can become an instrument of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive power and parliamentary majorities are the primary beneficiaries, gaining increased discretion and freedom from historical constitutional limits. Popular constitutionalists, juridical restraint advocates, and citizens seeking rights protection are the victims, as their efforts to invoke Magna Carta's principles are undermined. Historical scholars, while often providing the factual basis for the feudal context, act as observers whose work is then selectively leveraged by beneficiaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modern_relevance_ambiguity,
    'Is Magna Carta''s lack of binding authority over modern sovereignty a purely historical fact, or is it a constructed claim actively used to justify unchecked power?',
    'Analysis of legislative and judicial discourse: if the claim of obsolescence is consistently invoked to dismiss challenges to power, it supports the ''constructed claim'' interpretation.',
    'If purely historical, the constraint might be closer to a Piton (atrophied function). If actively constructed for power, it reinforces the Snare classification (obsolescence as cover for extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_relevance_ambiguity, conceptual, 'Distinguishing historical fact from instrumental claim regarding Magna Carta''s modern authority.').

omega_variable(
    impact_of_living_constitutionalism_reading,
    'How would the adoption of the `living_constitutionalism_reading` structurally alter this constraint?',
    'Counterfactual analysis: if Magna Carta were widely accepted as an evolving constitutional text, it would reintroduce historical principles as active constraints on modern power, shifting beneficiaries and victims.',
    'The `living_constitutionalism_reading` would likely transform the constraint from a Snare (enabling extraction) into a Rope or Tangled Rope (coordinating restraint), with executive and parliamentary power becoming targets rather than beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impact_of_living_constitutionalism_reading, conceptual, 'Structural impact of an alternative reading of Magna Carta''s authority.').

omega_variable(
    impact_of_parliamentary_sovereignty_reading,
    'How would the adoption of the `parliamentary_sovereignty_reading` structurally alter this constraint?',
    'Comparative legal analysis: if Magna Carta''s principles were seen as fully absorbed into and revisable by parliamentary statute, it would maintain parliamentary supremacy but potentially allow for a more explicit legislative re-engagement with its principles.',
    'The `parliamentary_sovereignty_reading` would likely shift the constraint towards a Rope or Tangled Rope, where Parliament is the primary agenda-setter, potentially balancing historical principles with modern legislative will, rather than simply dismissing them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impact_of_parliamentary_sovereignty_reading, conceptual, 'Structural impact of an alternative reading of Magna Carta''s authority.').

omega_variable(
    locus_of_disagreement,
    'Where is the fundamental disagreement located regarding Magna Carta''s authority?',
    'Analysis of legal and political debates: pinpointing whether the core dispute is over historical interpretation, the nature of constitutionalism, or the source of sovereign legitimacy.',
    'Identifying the locus of disagreement clarifies which foundational axioms are in conflict across different readings, informing the potential for resolution or continued contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(locus_of_disagreement, conceptual, 'The specific structural element readings differ on: historical interpretation vs. constitutional theory vs. source of sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(magn_tr_t1965, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(magn_tr_t1980, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(magn_tr_t1995, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(magn_tr_t2010, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(magn_tr_t2020, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(magn_be_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(magn_be_t1965, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(magn_be_t1980, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement(magn_be_t1995, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(magn_be_t2010, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(magn_be_t2020, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(magn_su_t1965, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1965, 0.63).
narrative_ontology:measurement(magn_su_t1980, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1980, 0.66).
narrative_ontology:measurement(magn_su_t1995, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(magn_su_t2010, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(magn_su_t2020, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'Magna Carta Constraint Authority' kernel. Each reading represents a different structural claim about Magna Carta's binding force on modern sovereignty, leading to different ε values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
