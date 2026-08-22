% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Statehood Criteria â Declaratory Reading
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   Kernel: montevideo_statehood_criteria. This constraint instantiates the
 *   declaratory reading: an entity that meets the four objective Montevideo
 *   criteria (permanent population, defined territory, government, capacity
 *   to enter relations with other states) is a state in international law as
 *   a matter of legal fact, independent of recognition by other states.
 *   Sibling readings include the constitutive reading (recognition by
 *   existing states is constitutive of statehood) and the hybrid reading
 *   (objective criteria plus normative legitimacy are required). The
 *   declaratory reading treats international law as self-executing rather
 *   than consensus-dependent.
 *
 * KEY AGENTS:
 *   - Secessionist movements (beneficiary; moderate power, constrained exit) â gain a legal roadmap to statehood without needing permission.
 *   - Parent states (payer; institutional power, constrained exit) â lose structural leverage to block statehood through non-recognition.
 *   - De facto authorities (payer; moderate power, trapped exit) â meet the criteria but are denied the practical benefits of statehood, trapped in legal limbo.
 *   - International judicial bodies (agenda_setter; institutional power, analytical exit) â administer and interpret the declaratory doctrine.
 *   - Great powers (observer; institutional power, arbitrage exit) â selectively apply the theory based on geopolitical interest.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.65).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.55).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Statehood Criteria â Declaratory Reading").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '5bf4d639-d358-4e2f-98ba-f26ce6ff8176').
narrative_ontology:cs_kernel_codification('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', formalized).
narrative_ontology:cs_authority_grounding('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', lineage).
narrative_ontology:cs_interpretation_layer_present('5bf4d639-d358-4e2f-98ba-f26ce6ff8176').
narrative_ontology:cs_reading_relation('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', foundational, statehood_objective_fact).
narrative_ontology:cs_axiom_status(statehood_objective_fact, holdable).
narrative_ontology:cs_axiom_grounding('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', statehood_objective_fact, conventional).
narrative_ontology:cs_axiom('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', foundational, recognition_non_constitutive).
narrative_ontology:cs_axiom_status(recognition_non_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', recognition_non_constitutive, conventional).
narrative_ontology:cs_reference_frame('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', montevideo_factuality).
narrative_ontology:cs_drift_state('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', contemporary_international_law, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5bf4d639-d358-4e2f-98ba-f26ce6ff8176', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political movements seeking independent statehood. They invoke the four Montevideo criteria as an objective legal standard that can establish statehood without the consent of the parent state or the international community. The declaratory theory gives them a legal roadmap and rhetorical framework, though they remain dependent on territorial control and governance capacity to satisfy the criteria.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, secessionist_movements, beneficiary,
    moderate, generational, constrained, national).

% States asserting sovereignty over territories where secessionist movements or de facto authorities have established control. Under the declaratory reading, they lose the structural leverage to block statehood purely by withholding recognition, though they retain diplomatic, economic, and military tools. They bear the cost of diminished gatekeeping authority in international law.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states, payer,
    institutional, generational, constrained, national).

% Governments of territories that meet the four Montevideo criteria but lack recognition by the broader international community. They are legally states under the declaratory theory yet are denied UN membership, treaty benefits, diplomatic relations, and access to international financial institutions. They are trapped in a limbo of legal facthood without practical effect, carrying state obligations while receiving few state prerogatives.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities, payer,
    moderate, biographical, trapped, national).

% The International Court of Justice and international arbitral tribunals that interpret and apply the Montevideo criteria in advisory opinions and contentious cases. They actively maintain the declaratory theory through legal reasoning and precedent, treating the criteria as a self-executing legal test. Their authority derives from continuity with the Montevideo Convention and subsequent international legal practice.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_judicial_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% Permanent members of the UN Security Council and other major powers. They selectively invoke or ignore the declaratory theory based on geopolitical interest, recognizing some entities that meet the criteria while blocking others. They observe the legal framework without being bound by it in practice, arbitrating between legal formalism and political convenience.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, great_powers, observer,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an objective, fact-based test for statehood that reduces reliance on the political discretion of existing states, creating predictability and a clear threshold for international legal personality.
% TRANSFER_FUNCTION: Transfers the authority to determine statehood from the collective recognition of existing states to the factual circumstances of the claimant entity; moves legal standing and the capacity to act on the international plane from gatekeepers to claimants.
% ABSENT_VOICES: Advocates of the constitutive theory and diplomats from states practicing political recognition are structurally marginalized in the declaratory legal framework; their objections are treated as political rather than legal. De facto authorities excluded from UN forums are also absent from the doctrinal conversation.
% DISAPPEARANCE_RATIONALE: If the declaratory criteria vanished overnight, statehood would revert to a purely political act of recognition, eliminating the legal floor that protects factual statehood from arbitrary denial. Secessionist movements would lose their primary legal argument, parent states would regain full gatekeeping leverage, and de facto authorities would lose even their nominal legal status.
% FOUNDING_PROBLEM: The interwar period lacked clear criteria for statehood, leading to arbitrary recognition, instability in international relations, and great-power competition over which entities to admit to the international community.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the 1933 Montevideo Conference attest the founding problem. However, post-colonial scholars, unrecognized state representatives, and critical international lawyers argue the criteria now serve established states' interests by masking continued political gatekeeping. Traditional international lawyers attest the problem remains live. No fully independent corroboration exists outside the benefiting legal tradition.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderately high because the declaratory theory extracts gatekeeping power from parent states and imposes legal obligations on de facto authorities while failing to deliver corresponding benefits. Suppression (0.55) reflects the active marginalization of the constitutive alternative in formal legal discourse. Theater ratio (0.50) captures the growing performative dimension: the criteria are ritually cited while recognition practice remains deeply political. Accessibility collapse (0.65) indicates that the constitutive alternative, though known, is largely excluded from formal legal argumentation. Resistance (0.55) comes from parent states and great powers that continue to treat recognition as a political tool. The measurement series tracks rising extraction and theater from 1933 to the present as the gap between declaratory legal theory and constitutive political practice has widened.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (international judicial bodies) experiences the constraint as genuine coordination â a clear legal test that reduces arbitrariness. The payer seats (parent states and de facto authorities) experience it as extraction: parent states lose territorial control leverage, while de facto authorities carry state burdens without receiving state benefits. The engine computes this divergence from the structural asymmetry in power, exit options, and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Secessionist movements are structural beneficiaries (low directionality): the constraint subsidizes their claims by providing an objective legal standard. Parent states and de facto authorities are structural targets (high directionality): the constraint extracts gatekeeping authority from parent states and traps de facto authorities in a limbo of legal facthood without practical recognition. International judicial bodies sit near the analytical center, while great powers arbitrage between positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both a coordination function and asymmetric extraction. The declaratory theory is not pure extraction (it solves a genuine coordination problem in international legal personality) and not pure coordination (it systematically disadvantages parent states and creates victimhood among de facto authorities). The founding problem â lack of clear statehood criteria â is contested, and the constraint persists beyond its original decolonization context into an era of secessionist conflict, suggesting potential mandatrophy that the temporal measurements track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_constitutive_gap,
    'Is the declaratory theory descriptively accurate about international practice, or is it a legal fiction that masks a constitutive political reality?',
    'Systematic empirical analysis of recognition patterns: if recognition tracks objective criteria independently of political interest, the declaratory theory is descriptive; if recognition correlates with geopolitical alignment, the constitutive theory describes actual practice.',
    'If the gap is purely ideological, the constraint''s extraction is higher than its coordination function suggests, moving it toward snare; if the gap is narrow, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_constitutive_gap, empirical, 'Whether declaratory statehood describes practice or masks political gatekeeping.').

omega_variable(
    de_facto_victimhood_mechanism,
    'Are de facto authorities victims of the declaratory legal structure itself, or merely of non-compliance with it?',
    'Comparative legal analysis of de facto authority trajectories: if authorities meeting the criteria but denied recognition systematically suffer legal incapacities that authorities with recognition do not, the structure itself generates the victimhood.',
    'If the structure generates victimhood, the declaratory reading is a tangled rope coordinating some while extracting from others; if victimhood results from non-compliance, the constraint is closer to a rope with external interference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_victimhood_mechanism, conceptual, 'Whether de facto authority limbo is structural or an enforcement failure.').

omega_variable(
    normative_criteria_override,
    'Has the foundational declaratory axiom â that four objective criteria suffice â been substantially overridden by the emergence of normative legitimacy requirements in contemporary practice?',
    'Survey of recognition practice and international organization admission criteria for systematic references to democracy, human rights, or non-aggression as prerequisites beyond the Montevideo criteria.',
    'If overridden, the declaratory reading''s reference frame has drifted toward the hybrid reading, and the constraint''s classification may shift as its axioms lose hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_criteria_override, empirical, 'Whether normative criteria have overridden the pure objective test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(montevideo_decl_tr_t0, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(montevideo_decl_tr_t18, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(montevideo_decl_tr_t36, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement(montevideo_decl_tr_t54, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 54, 0.4).
narrative_ontology:measurement(montevideo_decl_tr_t72, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 72, 0.46).
narrative_ontology:measurement(montevideo_decl_tr_t90, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 90, 0.5).

% Extraction over time
narrative_ontology:measurement(montevideo_decl_be_t0, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(montevideo_decl_be_t18, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 18, 0.38).
narrative_ontology:measurement(montevideo_decl_be_t36, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 36, 0.45).
narrative_ontology:measurement(montevideo_decl_be_t54, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 54, 0.54).
narrative_ontology:measurement(montevideo_decl_be_t72, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 72, 0.6).
narrative_ontology:measurement(montevideo_decl_be_t90, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 90, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(montevideo_decl_su_t0, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(montevideo_decl_su_t18, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 18, 0.32).
narrative_ontology:measurement(montevideo_decl_su_t36, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 36, 0.4).
narrative_ontology:measurement(montevideo_decl_su_t54, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 54, 0.48).
narrative_ontology:measurement(montevideo_decl_su_t72, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 72, 0.55).
narrative_ontology:measurement(montevideo_decl_su_t90, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 90, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the Montevideo statehood criteria constraint family. The declaratory, constitutive, and hybrid readings are structurally distinct constraints linked by their common kernel. The declaratory reading treats the four criteria as sufficient; the constitutive reading treats recognition as necessary; the hybrid reading adds normative legitimacy conditions. Each has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
