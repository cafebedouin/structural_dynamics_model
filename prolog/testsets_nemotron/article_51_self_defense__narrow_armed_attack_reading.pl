% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Self-Defense — Narrow Armed Attack Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   The narrow armed attack reading of Article 51 constrains self-defense to
 *   responses to actual or imminent armed attacks by a state, with non-state
 *   actor attacks requiring attribution to a host state under international
 *   law. This reading treats the UN Charter's text as a hard limit on
 *   unilateral force, preserving the Security Council's primary
 *   responsibility for international peace. The constraint operates as a
 *   coordination mechanism: it solves the collective action problem of
 *   regulating interstate force by establishing a clear, textually grounded
 *   trigger that all states can reference. Beneficiaries are weaker states
 *   (who gain protection from powerful states' unilateral force) and
 *   multilateral institutions (whose authority is preserved). Powerful states
 *   bear the constraint on their strategic freedom — they cannot lawfully
 *   invoke self-defense against non-attributable non-state actor threats or
 *   emerging threats without Security Council authorization. The reading's
 *   low extractiveness (0.18) reflects that the constraint primarily
 *   restrains rather than extracts; its low suppression (0.22) reflects that
 *   compliance is largely voluntary and reputation-based rather than
 *   coercively enforced. Theater ratio remains low because the constraint's
 *   legal form matches its operative function — there is no performative gap
 *   between the Charter text and the reading's application.
 *
 * KEY AGENTS:
 *   - weaker_states: Primary beneficiaries (institutional/moderate) — gain legal protection from unilateral intervention
 *   - multilateral_institutions: Primary beneficiaries (institutional/generational) — Security Council authority preserved
 *   - powerful_states: Primary payers (institutional/civilizational) — constrained strategic freedom, limited unilateral options
 *   - non_state_actors: Excluded (powerless/constrained) — attacks by them do not trigger Article 51 absent state attribution
 *   - international_legal_scholars: Observers (analytical/analytical) — interpret and contest the reading's boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.18).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.22).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Self-Defense — Narrow Armed Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies/constitutional_interpretation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, 'b89c85e6-4f7c-4391-93b0-caebbadf3866').
narrative_ontology:cs_kernel_codification('b89c85e6-4f7c-4391-93b0-caebbadf3866', fixed_text).
narrative_ontology:cs_authority_grounding('b89c85e6-4f7c-4391-93b0-caebbadf3866', lineage).
narrative_ontology:cs_interpretation_layer_present('b89c85e6-4f7c-4391-93b0-caebbadf3866').
narrative_ontology:cs_reading_relation('b89c85e6-4f7c-4391-93b0-caebbadf3866', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('b89c85e6-4f7c-4391-93b0-caebbadf3866', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('b89c85e6-4f7c-4391-93b0-caebbadf3866', foundational, armed_attack_requires_state_attribution).
narrative_ontology:cs_axiom_status(armed_attack_requires_state_attribution, holdable).
narrative_ontology:cs_axiom_grounding('b89c85e6-4f7c-4391-93b0-caebbadf3866', armed_attack_requires_state_attribution, conventional).
narrative_ontology:cs_axiom('b89c85e6-4f7c-4391-93b0-caebbadf3866', foundational, self_defense_exception_is_narrow_and_temporary).
narrative_ontology:cs_axiom_status(self_defense_exception_is_narrow_and_temporary, holdable).
narrative_ontology:cs_axiom_grounding('b89c85e6-4f7c-4391-93b0-caebbadf3866', self_defense_exception_is_narrow_and_temporary, conventional).
narrative_ontology:cs_reference_frame('b89c85e6-4f7c-4391-93b0-caebbadf3866', un_charter_1945_collective_security).
narrative_ontology:cs_drift_state('b89c85e6-4f7c-4391-93b0-caebbadf3866', post_9_11_state_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b89c85e6-4f7c-4391-93b0-caebbadf3866', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, un_charter_article_51_textualism).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, state_monopoly_on_force).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, sovereign_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with limited military capability that rely on the UN Charter's prohibition on force and the narrow self-defense exception as their primary legal protection against intervention by powerful states. They cannot exit the international system; their security depends on the constraint holding. They benefit when powerful states are legally barred from invoking self-defense against non-attributable threats.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    moderate, generational, constrained, global).

% The UN Security Council and associated collective security machinery. Their authority to determine threats to peace and authorize force (Chapter VII) is preserved when self-defense is narrowly construed. They set the agenda for lawful force but do not collect rents from the constraint. Their exit is analytical — they observe and administer the system from within.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions, agenda_setter).

% Militarily dominant states (permanent Security Council members and other major powers) whose strategic freedom to use force unilaterally is constrained by the narrow reading. They bear the cost of forgone military options against non-attributable non-state actor threats and emerging threats. They cannot exit the legal order without losing legitimacy, but they shape its interpretation through practice and veto power.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states, payer,
    institutional, civilizational, constrained, global).

% Armed groups, terrorist organizations, insurgents whose attacks do not trigger Article 51 self-defense unless attributable to a host state. They have no voice in the legal regime that governs responses to their actions. They are trapped in the sense that the constraint defines the legal space of state responses to them, but they cannot participate in shaping it.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, non_state_actors, excluded,
    powerless, immediate, trapped, global).

% Academic and practitioner interpreters who contest the boundary between actual/imminent armed attack and preventive force, debate attribution standards, and document state practice. They neither collect nor pay; they provide the interpretive infrastructure through which the constraint's meaning evolves.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__narrow_armed_attack_reading, diffuse).
narrative_ontology:fixing_cost_class(article_51_self_defense__narrow_armed_attack_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, textually grounded, universally referenceable rule for when interstate force is lawful, reducing miscalculation, escalation, and unilateral war-making by establishing that only actual or imminent armed attacks by states trigger the self-defense exception to the Article 2(4) prohibition.
% TRANSFER_FUNCTION: The constraint does not transfer resources. It restrains: powerful states forgo unilateral military options; weaker states gain legal protection; multilateral institutions retain authority. The 'transfer' is strategic freedom from powerful states to systemic stability.
% ABSENT_VOICES: Non-state actors (terrorist groups, insurgents, liberation movements) whose attacks are excluded from triggering Article 51 unless attributable to a state. States facing imminent non-attributable threats (e.g., 9/11-type attacks from non-state actors in failed states) who would argue for a broader trigger but are not seated in the Charter framework. Populations in weak states who suffer from both non-state actor violence and the constraint's limitation on external intervention.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished overnight, powerful states would immediately invoke self-defense against a vastly expanded range of threats (non-attributable non-state actors, emerging WMD programs, cyber operations). The prohibition on force would collapse into a permissive regime of unilateral 'necessity' claims. The Security Council's Chapter VII authority would be hollowed out. The international legal order governing force would fundamentally reorganize.
% FOUNDING_PROBLEM: The UN Charter was built to solve the problem of interstate war by prohibiting force (Article 2(4)) and channeling collective response through the Security Council (Chapter VII), with self-defense (Article 51) as a narrow, temporary exception for actual armed attacks pending Council action.
% FOUNDING_PROBLEM_CORROBORATION: The ICJ in Nicaragua (1986) and Oil Platforms (2003) affirmed the narrow reading as the Charter's design. The UN Secretary-General's High-Level Panel (2004) endorsed the 'imminent armed attack' standard. Weaker states consistently invoke the narrow reading in General Assembly debates. No credible external corroboration exists for the claim that the founding problem is dead — the expansive readings' proponents argue the problem has *changed* (new threats), not that it is *gone*.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).
:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type 'rope' reflects the constraint's genuine coordination function: it provides a clear, shared rule for when force is lawful, reducing miscalculation and escalation. The metrics support this: extractiveness is low because the constraint does not transfer resources from payers to beneficiaries — it restrains action. Suppression is low because compliance is driven by legitimacy and reciprocity, not coercion. Theater is low because the reading's legal articulation matches its operative content. The divergence from 'mountain' is that the constraint is a human-made legal rule, not a natural law (emerges_naturally: false), and it has identifiable beneficiaries and victims. The divergence from 'tangled_rope' is the absence of asymmetric extraction — powerful states are constrained but do not pay rents to weaker states; the benefit is systemic (stability, predictability) not transferred.
 *
 * PERSPECTIVAL GAP:
 *   From the powerful state seat, the constraint appears as a restriction on legitimate security responses — the 'victim' experience is strategic foreclosure. From the weaker state seat, the constraint appears as the primary legal shield against intervention — the 'beneficiary' experience is protection. From the multilateral institution seat, the constraint appears as the foundation of the collective security architecture. The engine computes per-seat effective extraction from the structural data: powerful states have high directionality (d near 1.0 as constrained payers), weaker states and institutions have low directionality (d near 0.0 as beneficiaries). The analytical observer seat sees the coordination function without bearing costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: weaker_states and multilateral_institutions. These actors gain systemic benefits (protection from unilateral force, preserved institutional authority) without bearing the constraint's costs. Their exit options are constrained/analytical — they cannot exit the international system, but they do not need to. Victims declared: powerful_states. These actors bear the cost of constrained strategic freedom — they cannot lawfully use force against non-attributable threats. Their exit options are constrained: they remain in the system but the constraint binds their most valued capabilities (unilateral force). The directionality derivation assigns high d to powerful_states (targets), low d to weaker_states and multilateral_institutions (beneficiaries), producing per-seat effective extraction that reflects this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regulating interstate force to prevent war — remains live (founding_problem_status: live). The narrow reading has not atrophied into piton because its coordination function (clear trigger for lawful force) is actively invoked in state practice and Security Council deliberations. Theater ratio has crept up slightly (0.05→0.12) as states invoke self-defense more expansively while paying lip service to the narrow reading, but the core constraint remains functionally operative. No mandatrophy resolution needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel article_51_self_defense, or a freestanding constraint?',
    'Committee frame: the SCOPE manifest tagged this generation with kernel_id=article_51_self_defense and reading_id=narrow_armed_attack_reading. The structural delta specified for this reading (high constraint on unilateral force; non-state actor threats do not trigger Article 51 unless attributable to host state) is baked into the authored beneficiary/victim structure and metrics.',
    'If this is a kernel reading, sibling readings (expansive_preventive_reading, unable_unwilling_doctrine_reading) are separate constraints with their own ε and stakeholder surfaces, linked via network.affects_constraints and cs_structure.reading_relations. If freestanding, the decomposition is not applicable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment of this constraint to the kernel/reading frame.').

omega_variable(
    attribution_threshold_ambiguity,
    'Does ''attributable under international law'' require effective control (Nicaragua standard) or overall control (Tadić standard), and how does that threshold change the constraint''s extraction profile?',
    'ICJ/ICC jurisprudence convergence or a clarifying UNGA resolution on attribution standards for non-state actor armed attacks.',
    'A higher threshold (effective control) makes the constraint more restrictive on powerful states (lower extractiveness from their perspective, higher constraint on their strategic freedom). A lower threshold (overall control) expands the trigger and reduces the constraint''s protective function for weaker states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_threshold_ambiguity, conceptual, 'Attribution standard determines the boundary of the armed attack trigger.').

omega_variable(
    imminence_interpretation,
    'Does ''imminent armed attack'' permit anticipatory self-defense under the Caroline standard, or is it limited to attacks already in motion (missiles launched, troops crossing borders)?',
    'State practice convergence on anticipatory self-defense claims; Security Council response patterns to anticipatory strikes.',
    'A narrow imminence reading (attacks already in motion) increases the constraint''s restrictiveness on powerful states. A broad imminence reading (Caroline necessity) blurs the boundary with the expansive_preventive_reading sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_interpretation, conceptual, 'Temporal boundary of the armed attack trigger.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_tr_t1962, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1962, 0.07).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_tr_t1986, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1986, 0.09).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.11).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_tr_t2014, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_tr_t2024, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.12).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_be_t1962, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1962, 0.14).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_be_t1986, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1986, 0.15).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.17).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_be_t2014, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2014, 0.18).
narrative_ontology:measurement(article_51_self_defense__narrow_armed_attack_reading_be_t2024, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2024, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_51_self_defense__narrow_armed_attack_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__narrow_armed_attack_reading, 0.1).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, un_chapter_vii_security_council_authorization).

% DUAL FORMULATION NOTE:
% This constraint is one member of the article_51_self_defense kernel family. The three readings instantiate distinct constraints with different ε, beneficiary/victim structures, and claimed types. This reading (narrow) has low ε (0.18) and claims rope; the expansive reading will have higher ε and likely claim tangled_rope or snare; the unable/unwilling reading sits between. All three are linked via affects_constraints and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, institutional, 0.85).
constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
