% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Marker
 *   domain: systematic_theology/ecclesiology/history_of_christian_doctrine
 *
 * SUMMARY:
 *   This constraint models the Nicene Creed's function as a liturgical
 *   identity marker, where its regular recitation in worship services
 *   primarily serves to habituate communal identity and belonging, rather
 *   than to enforce strict metaphysical assent. This reading emphasizes the
 *   performative and social aspects of the creed, independent of its
 *   cognitive content. It is one reading of the broader
 *   'nicene_creed_authority' kernel, which also includes strict orthodox and
 *   symbolic confessional interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.15).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Marker").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "systematic_theology/ecclesiology/history_of_christian_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, 'cd4455e4-a238-4c04-a86c-56c6355cf302').
narrative_ontology:cs_kernel_codification('cd4455e4-a238-4c04-a86c-56c6355cf302', fixed_text).
narrative_ontology:cs_authority_grounding('cd4455e4-a238-4c04-a86c-56c6355cf302', practice).
narrative_ontology:cs_interpretation_layer_present('cd4455e4-a238-4c04-a86c-56c6355cf302').
narrative_ontology:cs_reading_relation('cd4455e4-a238-4c04-a86c-56c6355cf302', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('cd4455e4-a238-4c04-a86c-56c6355cf302', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('cd4455e4-a238-4c04-a86c-56c6355cf302', foundational, liturgical_performance_forms_identity).
narrative_ontology:cs_axiom_status(liturgical_performance_forms_identity, holdable).
narrative_ontology:cs_axiom_grounding('cd4455e4-a238-4c04-a86c-56c6355cf302', liturgical_performance_forms_identity, conventional).
narrative_ontology:cs_axiom('cd4455e4-a238-4c04-a86c-56c6355cf302', foundational, cognitive_assent_is_secondary_to_practice).
narrative_ontology:cs_axiom_status(cognitive_assent_is_secondary_to_practice, holdable).
narrative_ontology:cs_axiom_grounding('cd4455e4-a238-4c04-a86c-56c6355cf302', cognitive_assent_is_secondary_to_practice, conventional).
narrative_ontology:cs_reference_frame('cd4455e4-a238-4c04-a86c-56c6355cf302', communal_liturgical_identity).
narrative_ontology:cs_drift_state('cd4455e4-a238-4c04-a86c-56c6355cf302', contemporary_pluralist_context, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cd4455e4-a238-4c04-a86c-56c6355cf302', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, congregants).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, denominational_institutions).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, communal_identity_formation).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, liturgical_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the communal recitation of the creed, which reinforces a sense of shared identity and belonging, often without requiring deep cognitive assent to every metaphysical claim. Exit involves finding another faith community or leaving organized religion.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, congregants, beneficiary,
    moderate, biographical, mobile, local).

% Lead the liturgical performance of the creed, maintaining its place in worship. While they may hold diverse theological views, their role is to facilitate the communal practice. Exit involves leaving ordained ministry or seeking a different denomination.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, clergy, agenda_setter,
    organized, biographical, constrained, regional).

% Benefit from the creed's function in maintaining a stable, recognizable identity for their member churches, providing a basis for inter-church relations and theological education. Exit involves a fundamental redefinition of their institutional identity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, denominational_institutions, beneficiary,
    institutional, generational, constrained, national).

% Analyze the historical, theological, and sociological functions of the creed, including its role in identity formation. Their work may influence how the creed is understood and taught, but they do not directly enforce its liturgical use.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, theologians_and_scholars, observer,
    analytical, generational, analytical, global).

% Would object to the idea that metaphysical assent is secondary to liturgical performance, viewing the creed primarily as a binding statement of objective truth. They are excluded from the interpretive frame of this reading, which downplays their core concern.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, strict_orthodox_adherents, excluded,
    moderate, generational, identity_locked, global).

% Would object to any reading that grants the creed inherent authority beyond its function as a historical witness or a symbol of communal aspiration. They are excluded from the interpretive frame of this reading, which still grants the creed a functional authority in identity formation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, symbolic_confessional_adherents, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates communal identity and belonging within a faith tradition by providing a shared ritual text and performance, allowing for diverse individual theological interpretations while maintaining a cohesive group identity.
% TRANSFER_FUNCTION: Transfers a sense of shared identity, historical continuity, and communal belonging to congregants and institutions, in exchange for participation in a common liturgical practice.
% ABSENT_VOICES: Strict orthodox adherents would argue that this reading dilutes the creed's metaphysical authority, while symbolic confessional adherents might argue it still grants too much authority to a historical text. Both are present in broader theological discourse but are not the primary focus of this specific reading's functional analysis.
% DISAPPEARANCE_RATIONALE: If the Nicene Creed vanished from liturgical use overnight, Christian denominations would lose a primary, widely recognized marker of historical and communal identity. While individual faith might persist, the institutional and social fabric of many churches would need to find new ways to articulate and coordinate their shared identity, leading to significant rearrangement.
% FOUNDING_PROBLEM: The early Christian church faced internal theological disputes and external pressures, requiring a clear statement of core beliefs to define its identity and maintain unity amidst diversity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of doctrine and sociologists of religion corroborate that the need for communal identity and unity, even with diverse theological interpretations, remains a live concern for many Christian communities, independent of the specific metaphysical claims. This is attested by ongoing ecumenical dialogues and studies of religious practice.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because this reading frames the creed as a beneficial coordination mechanism for identity, with minimal cost to participants beyond participation. Suppression is low (0.15) as adherence is largely voluntary and driven by a desire for communal belonging, not coercion. Theater ratio is negligible (0.05) as the liturgical performance is genuinely functional for identity formation. The metrics reflect a 'rope' classification, consistent with a coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This reading intentionally downplays the metaphysical and coercive aspects that other readings emphasize. From the perspective of strict orthodox adherents, this reading would be seen as undermining the creed's true purpose, while symbolic confessional adherents might still find it too prescriptive. This constraint focuses solely on the identity-coordinating function.
 *
 * DIRECTIONALITY LOGIC:
 *   Congregants and denominational institutions are beneficiaries, gaining communal identity and stability. Clergy act as agenda-setters, facilitating the liturgical practice. There are no direct 'victims' in this reading, as the constraint is understood to be net-beneficial for participants. Strict orthodox and symbolic confessional adherents are 'excluded' in the sense that their primary interpretive frames are not centered in this reading, though they may still participate in the liturgical practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_assent_threshold,
    'At what point does the lack of cognitive metaphysical assent to the creed''s claims undermine its function as an identity marker, even in a liturgical context?',
    'Sociological studies of religious communities where creedal recitation is common but doctrinal belief is low: does communal identity persist, or does it erode over generations?',
    'If identity erodes, the extractiveness of this ''rope'' reading might subtly increase over time as the coordination function weakens, potentially drifting towards a ''piton'' of empty ritual. If it persists, the ''rope'' classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_assent_threshold, empirical, 'The threshold at which cognitive dissonance impacts liturgical identity formation.').

omega_variable(
    coordination_vs_coercion_boundary,
    'Is the ''identity coordination'' function of the creed truly voluntary, or does social pressure within a community subtly coerce participation and nominal assent, even without explicit enforcement?',
    'Ethnographic studies of individual experiences within liturgical communities, focusing on perceived freedom to dissent or disengage from creedal recitation without social cost.',
    'If subtle coercion is significant, the ''suppression'' metric for this reading would need to be adjusted upward, potentially shifting the classification from ''rope'' towards ''tangled_rope'' for individual congregants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_coercion_boundary, empirical, 'Distinguishing voluntary identity coordination from subtle social coercion.').

omega_variable(
    reading_framing_legitimacy,
    'Is this ''liturgical habituation'' reading a legitimate interpretation of the Nicene Creed''s function, or does it fundamentally misrepresent the creed''s historical and theological intent?',
    'Historical-theological scholarship on the intent of the Nicene Fathers and subsequent reception history, weighed against contemporary sociological analysis of religious practice. This is a conceptual debate within theology.',
    'If this reading is deemed illegitimate by a consensus of scholars, its analytical utility for understanding the creed''s function would be diminished, and the ''strict_orthodox_reading'' or ''symbolic_confessional_reading'' might gain explanatory dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'The conceptual legitimacy of the liturgical habituation reading itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(nice_tr_t400, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 400, 0.04).
narrative_ontology:measurement(nice_tr_t800, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 800, 0.04).
narrative_ontology:measurement(nice_tr_t1200, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1200, 0.05).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1700, 0.05).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(nice_be_t400, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 400, 0.06).
narrative_ontology:measurement(nice_be_t800, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 800, 0.07).
narrative_ontology:measurement(nice_be_t1200, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1200, 0.07).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1700, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(nice_su_t400, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 400, 0.12).
narrative_ontology:measurement(nice_su_t800, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 800, 0.13).
narrative_ontology:measurement(nice_su_t1200, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1200, 0.14).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1700, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nicene_creed_authority' kernel. It provides the social and performative substrate upon which both strict metaphysical enforcement and pluralist reinterpretation operate, influencing both sibling readings by establishing a common liturgical ground.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
