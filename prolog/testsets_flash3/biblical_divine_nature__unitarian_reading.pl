% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Biblical Divine Nature: Unitarian Reading
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This constraint represents the Unitarian reading of the divine nature,
 *   emphasizing the numerical singularity of God, with the Father alone as
 *   God, and the Son and Spirit being subordinate or created. It is one
 *   reading of the 'biblical_divine_nature' kernel. This reading challenges
 *   established institutional hierarchies and credal orthodoxies, leading to
 *   high resistance and active suppression from those institutions. The
 *   claimed type is 'snare' because its persistence relies on actively
 *   undermining and extracting from the established trinitarian framework,
 *   rather than merely coordinating an alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.6).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.7).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, snare).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Biblical Divine Nature: Unitarian Reading").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '831ea2b4-4004-49d6-a372-c3155f12a1af').
narrative_ontology:cs_kernel_codification('831ea2b4-4004-49d6-a372-c3155f12a1af', fixed_text).
narrative_ontology:cs_authority_grounding('831ea2b4-4004-49d6-a372-c3155f12a1af', practice).
narrative_ontology:cs_interpretation_layer_present('831ea2b4-4004-49d6-a372-c3155f12a1af').
narrative_ontology:cs_reading_relation('831ea2b4-4004-49d6-a372-c3155f12a1af', biblical_divine_nature__trinitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('831ea2b4-4004-49d6-a372-c3155f12a1af', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('831ea2b4-4004-49d6-a372-c3155f12a1af', foundational, god_is_numerically_one_person).
narrative_ontology:cs_axiom_status(god_is_numerically_one_person, holdable).
narrative_ontology:cs_axiom_grounding('831ea2b4-4004-49d6-a372-c3155f12a1af', god_is_numerically_one_person, deontological).
narrative_ontology:cs_axiom('831ea2b4-4004-49d6-a372-c3155f12a1af', foundational, son_and_spirit_are_subordinate_or_created).
narrative_ontology:cs_axiom_status(son_and_spirit_are_subordinate_or_created, holdable).
narrative_ontology:cs_axiom_grounding('831ea2b4-4004-49d6-a372-c3155f12a1af', son_and_spirit_are_subordinate_or_created, deontological).
narrative_ontology:cs_reference_frame('831ea2b4-4004-49d6-a372-c3155f12a1af', scriptural_monotheism_of_father).
narrative_ontology:cs_drift_state('831ea2b4-4004-49d6-a372-c3155f12a1af', post_nicene_creed_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('831ea2b4-4004-49d6-a372-c3155f12a1af', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_adherents).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, individual_conscience).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Find theological clarity and simplicity in the singular nature of God, aligning with their interpretation of scripture. They benefit from a flat ecclesiology that emphasizes individual interpretation over hierarchical dogma.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_adherents, beneficiary,
    moderate, biographical, mobile, global).

% Bears the cost of theological challenge to established credal formulations. This reading undermines the authority of councils and traditional interpretations, requiring active defense of trinitarian orthodoxy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_hierarchy, payer,
    institutional, generational, constrained, global).

% Represents the established doctrinal framework that defines God as a Trinity. This reading directly challenges its foundational claims, forcing a re-evaluation or defense of centuries of theological development.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy, payer,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(biblical_divine_nature__unitarian_reading, credal_orthodoxy).

% Invest careers in defending and elaborating trinitarian doctrine. The unitarian reading directly contests their work, requiring them to engage in apologetics and re-articulate their positions against a perceived simplification or misinterpretation of scripture.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_theologians, payer,
    organized, biographical, constrained, global).

% Benefits from the freedom to interpret scripture directly without the imposition of complex, non-biblical credal formulations. This reading offers a path to theological understanding that feels more accessible and less mediated.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, individual_conscience, beneficiary,
    powerless, immediate, mobile, local).
narrative_ontology:stakeholder_non_agent(biblical_divine_nature__unitarian_reading, individual_conscience).

% While also non-trinitarian, modalists are excluded from the unitarian framing because their view of God as one person manifesting in different modes is distinct from the unitarian emphasis on the Father's sole divinity and the Son/Spirit's subordination/creation.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, modalist_adherents, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates theological understanding around a simplified, numerically singular concept of God, aiming for scriptural fidelity and avoiding perceived philosophical complexities of trinitarianism.
% TRANSFER_FUNCTION: Transfers theological authority from institutional hierarchies and credal traditions to individual scriptural interpretation, and from complex trinitarian formulations to a singular divine identity.
% ABSENT_VOICES: Modalist adherents, who also reject trinitarianism but offer a different non-trinitarian model, are often excluded from the unitarian discourse, which focuses on its own specific interpretation of divine singularity.
% DISAPPEARANCE_RATIONALE: If the unitarian reading vanished, the theological landscape would significantly shift. Trinitarian orthodoxy would face less internal challenge, and the emphasis on individual scriptural interpretation as a primary theological authority would diminish, leading to a re-consolidation of credal authority.
% FOUNDING_PROBLEM: The perceived philosophical complexity and lack of explicit scriptural support for trinitarian formulations, leading to a desire for a simpler, more biblically direct understanding of God's nature.
% FOUNDING_PROBLEM_CORROBORATION: Unitarian scholars and adherents attest that the problem of trinitarian complexity and perceived scriptural ambiguity remains live. Critics from trinitarian traditions argue the problem is resolved by established theology, but the persistence of unitarian movements corroborates the ongoing nature of the perceived problem from an external perspective.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the cost imposed on established theological systems by this challenge. Suppression (0.7) is high because institutional orthodoxy actively works to suppress unitarian views through doctrinal condemnation and social exclusion. Resistance (0.8) is also high, as unitarianism represents a persistent counter-narrative within Christian history. Theater ratio (0.2) is low, indicating that the core theological claims are genuinely held and debated, not merely performed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of unitarian adherents, this reading is a 'rope' or even a 'mountain' of scriptural truth, offering liberation from complex dogma. From the perspective of institutional orthodoxy, it is a 'snare' that undermines foundational tenets and extracts legitimacy. The engine's classification as 'snare' reflects the structural impact on the established order.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian adherents and individual conscience are beneficiaries, gaining theological clarity and interpretive freedom. Institutional hierarchies, credal orthodoxy, and trinitarian theologians are victims, as their authority and doctrines are directly challenged and undermined by this reading. The constraint extracts from the established order by eroding its legitimacy and forcing it to expend resources in defense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_interpretation_ambiguity,
    'Is the unitarian reading a more faithful interpretation of scripture, or does it selectively emphasize certain passages while neglecting others?',
    'Comprehensive historical-critical and theological analysis of all relevant biblical texts, considering linguistic, cultural, and historical contexts, with consensus from diverse, non-partisan scholarly bodies.',
    'If deemed more faithful, the constraint''s legitimacy would increase, potentially reducing its perceived extractiveness from institutional orthodoxy. If deemed selective, its claims to scriptural authority would weaken, increasing its perceived suppression and extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scriptural_interpretation_ambiguity, empirical, 'Ambiguity regarding the definitive scriptural basis for unitarianism versus trinitarianism.').

omega_variable(
    institutional_authority_legitimacy,
    'Is the institutional hierarchy''s claim to doctrinal authority (e.g., through ecumenical councils) a legitimate source of theological truth, or an imposed structure that suppresses alternative interpretations?',
    'Historical and sociological analysis of the development of credal authority, combined with a philosophical assessment of the nature of religious authority and its relationship to individual conscience.',
    'If institutional authority is deemed illegitimate, the unitarian reading''s resistance would be reclassified as justified, and the ''snare'' classification would be reinforced. If deemed legitimate, the unitarian reading''s challenge would be seen as a deviation, increasing its perceived extractiveness from the ''beneficiary'' seats of orthodoxy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_legitimacy, conceptual, 'Ambiguity regarding the legitimacy of institutional doctrinal authority versus individual interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bibl_tr_t25, biblical_divine_nature__unitarian_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(bibl_tr_t50, biblical_divine_nature__unitarian_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(bibl_tr_t75, biblical_divine_nature__unitarian_reading, theater_ratio, 75, 0.19).
narrative_ontology:measurement(bibl_tr_t100, biblical_divine_nature__unitarian_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bibl_be_t25, biblical_divine_nature__unitarian_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(bibl_be_t50, biblical_divine_nature__unitarian_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(bibl_be_t75, biblical_divine_nature__unitarian_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement(bibl_be_t100, biblical_divine_nature__unitarian_reading, base_extractiveness, 100, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(bibl_su_t25, biblical_divine_nature__unitarian_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(bibl_su_t50, biblical_divine_nature__unitarian_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(bibl_su_t75, biblical_divine_nature__unitarian_reading, suppression_requirement, 75, 0.68).
narrative_ontology:measurement(bibl_su_t100, biblical_divine_nature__unitarian_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'biblical_divine_nature' kernel, each representing a distinct theological position on the nature of God. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
