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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Marker
 *   domain: Systematic Theology / Ecclesiology / History of Christian Doctrine
 *
 * SUMMARY:
 *   This constraint story analyzes the Nicene Creed as a mechanism for
 *   identity formation through its liturgical performance, distinct from its
 *   role as a strict metaphysical statement. In this reading, the creed
 *   functions as a shared ritual practice that habituates communal identity
 *   and belonging, providing a stable social substrate for Christian
 *   communities. This function is largely independent of whether individual
 *   participants fully assent to every metaphysical proposition of the creed,
 *   and it serves as a foundation that 'feeds' both stricter orthodox and
 *   more symbolic confessional interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.15).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Marker").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "Systematic Theology / Ecclesiology / History of Christian Doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, 'a27188dd-3d72-4102-91e1-c6ae1d2be6fb').
narrative_ontology:cs_kernel_codification('a27188dd-3d72-4102-91e1-c6ae1d2be6fb', fixed_text).
narrative_ontology:cs_authority_grounding('a27188dd-3d72-4102-91e1-c6ae1d2be6fb', practice).
narrative_ontology:cs_interpretation_layer_present('a27188dd-3d72-4102-91e1-c6ae1d2be6fb').
narrative_ontology:cs_reading_relation('a27188dd-3d72-4102-91e1-c6ae1d2be6fb', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('a27188dd-3d72-4102-91e1-c6ae1d2be6fb', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('a27188dd-3d72-4102-91e1-c6ae1d2be6fb', foundational, liturgical_performance_identity_formation).
narrative_ontology:cs_axiom_status(liturgical_performance_identity_formation, holdable).
narrative_ontology:cs_axiom_grounding('a27188dd-3d72-4102-91e1-c6ae1d2be6fb', liturgical_performance_identity_formation, conventional).
narrative_ontology:cs_reference_frame('a27188dd-3d72-4102-91e1-c6ae1d2be6fb', early_church_liturgical_practice).
narrative_ontology:cs_drift_state('a27188dd-3d72-4102-91e1-c6ae1d2be6fb', contemporary_ecclesial_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a27188dd-3d72-4102-91e1-c6ae1d2be6fb', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, church_members).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, ecclesiastical_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, symbolic_confessional_adherents).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, communal_identity_formation).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, liturgical_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in liturgical performance of the creed, gaining a sense of shared identity, belonging, and continuity within the Christian community, often independent of deep cognitive metaphysical assent. Exit means losing this communal identity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, church_members, beneficiary,
    moderate, biographical, constrained, local).

% Administer and perpetuate the liturgical use of the creed, benefiting from the social cohesion, historical continuity, and stable identity it provides for their communities. Changing this practice would be highly disruptive.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, ecclesiastical_institutions, beneficiary).

% Study, interpret, and articulate the function of the creed within liturgical practice and its role in identity formation. They analyze its effects without directly benefiting or paying in the same way as other actors.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, theologians_liturgists, observer,
    analytical, generational, analytical, global).

% Adhere to a view where the creed primarily binds believers to a strict metaphysical ontology. They are excluded from enforcing this as the *sole* or *dominant* function of the creed within broader liturgical practice, but they observe and critique this more expansive reading. Their 'cost' is the non-realization of their preferred, stricter enforcement.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, strict_orthodox_adherents, excluded,
    powerful, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, strict_orthodox_adherents, observer).

% View the creed as a historically contingent witness whose authority derives from community discernment and personal faith. This liturgical habituation reading supports their pluralist interpretation by emphasizing communal practice over strict dogmatic assent, making them beneficiaries of its broad acceptance.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, symbolic_confessional_adherents, beneficiary,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__liturgical_habituation_reading, ecclesiastical_institutions).
narrative_ontology:fixing_cost_class(nicene_creed_authority__liturgical_habituation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates communal identity and belonging through shared ritual practice, providing a stable framework for participation in Christian communities regardless of individual cognitive metaphysical interpretation.
% TRANSFER_FUNCTION: Transfers a sense of shared identity, belonging, and historical continuity to participants, and social cohesion and stability to ecclesiastical institutions.
% ABSENT_VOICES: Those who demand strict cognitive assent to every metaphysical proposition of the creed as a condition of membership might object, arguing that this reading undermines doctrinal truth and discipline. They are often present in theological discourse but excluded from defining the primary liturgical function.
% DISAPPEARANCE_RATIONALE: If the Nicene Creed vanished overnight as a liturgical identity marker, many Christian communities would lose a central, unifying ritual practice and a key symbol of shared identity, leading to fragmentation of communal bonds and a significant disruption to worship and catechesis.
% FOUNDING_PROBLEM: Early Christian communities needed a concise, authoritative statement of faith to define Christian identity, distinguish from heresies (especially Arianism), and provide a common language for worship and teaching, thereby fostering unity and continuity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of early Christianity, liturgical scholars, and sociologists of religion attest to the creed's enduring role in identity formation, boundary maintenance, and communal cohesion, often highlighting its function beyond strict metaphysical assent. This corroboration comes from outside the immediate beneficiaries of the creed's current institutional use.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is very low (0.08) because the primary function is coordination of identity and belonging, with minimal direct transfer of resources or imposition of costs. Suppression is low (0.15) as participation is largely voluntary and driven by a desire for communal identity, rather than coercion. Theater ratio is low (0.10) because the liturgical performance *is* the function; there's little performative maintenance detached from its core purpose. Accessibility collapse is moderate (0.40) as alternatives for communal identity exist, but for those within the tradition, the creed offers a deeply embedded and widely accepted path. Resistance is low (0.10) because this function is generally accepted as beneficial for communal life.
 *
 * PERSPECTIVAL GAP:
 *   While this reading emphasizes the creed's coordinating function, strict orthodox adherents may perceive it as a 'dilution' or 'compromise' of doctrinal truth, experiencing a gap between their desired function and the observed liturgical habituation. However, this is a conceptual disagreement about the creed's primary purpose, not a direct extraction of resources from them by this specific constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Church members are beneficiaries, gaining communal identity and belonging. Ecclesiastical institutions are agenda-setters and beneficiaries, as they administer the liturgy and benefit from the cohesion and continuity it provides. Theologians and liturgists act as observers, analyzing the constraint's function. Strict orthodox adherents are 'excluded' in the sense that their preferred, stricter interpretation is not universally enforced by this liturgical function, representing a conceptual 'cost' of non-realization. Symbolic confessional adherents are beneficiaries, as this reading aligns with their emphasis on communal discernment and personal faith over strict dogma.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'rope' prevents mislabeling the creed's liturgical function as a 'snare' or 'tangled rope' by acknowledging its genuine coordination of identity and belonging, with very low extraction. It distinguishes this function from other readings that might involve higher extraction or suppression related to doctrinal enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_metaphysical_independence,
    'To what extent is the creed''s function as an identity boundary marker through liturgical performance truly independent of cognitive metaphysical assent, or does it implicitly reinforce specific metaphysical claims over time?',
    'Longitudinal sociological and theological studies tracking the correlation between liturgical participation, explicit metaphysical belief, and identity formation across diverse Christian communities. Ethnographic research on how participants articulate their understanding of the creed''s meaning.',
    'If strong implicit reinforcement is found, the extractiveness and suppression metrics might need upward adjustment, as the constraint would subtly coerce metaphysical assent through habituation, even without explicit enforcement. If independence is robust, the current low metrics are affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_metaphysical_independence, empirical, 'Ambiguity regarding the degree of independence between liturgical identity formation and implicit metaphysical reinforcement.').

omega_variable(
    suppression_of_alternative_expressions,
    'Does the pervasive liturgical habituation of the Nicene Creed, even in this broad reading, inadvertently suppress the development or expression of alternative theological or confessional formulations within Christian communities?',
    'Historical and contemporary analysis of theological innovation and dissent in traditions with strong liturgical creedal use versus those without. Comparative studies of theological diversity in different ecclesial contexts.',
    'If significant suppression of alternatives is identified, the ''suppression'' metric might need upward adjustment, as the constraint, while coordinating, would also be subtly limiting the ''exit options'' for new theological expressions. This would shift its classification closer to a ''tangled_rope'' for those seeking alternative formulations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_alternative_expressions, conceptual, 'Whether liturgical habituation, despite its coordinating function, inadvertently limits theological diversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 325, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 325, 0.05).
narrative_ontology:measurement(nice_tr_t800, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 800, 0.07).
narrative_ontology:measurement(nice_tr_t1300, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1300, 0.08).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1700, 0.09).
narrative_ontology:measurement(nice_tr_t1950, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(nice_tr_t2024, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 325, 0.05).
narrative_ontology:measurement(nice_be_t800, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 800, 0.06).
narrative_ontology:measurement(nice_be_t1300, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1300, 0.07).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1700, 0.07).
narrative_ontology:measurement(nice_be_t1950, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1950, 0.08).
narrative_ontology:measurement(nice_be_t2024, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 2024, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 325, 0.1).
narrative_ontology:measurement(nice_su_t800, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 800, 0.12).
narrative_ontology:measurement(nice_su_t1300, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1300, 0.13).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1700, 0.14).
narrative_ontology:measurement(nice_su_t1950, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(nice_su_t2024, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nicene_creed_authority' kernel, focusing on its liturgical and identity-forming function. It provides a social substrate that influences both the 'strict_orthodox_reading' (by providing a common practice that can be interpreted strictly) and the 'symbolic_confessional_reading' (by providing a communal framework for pluralistic interpretation). Each reading has distinct ε values and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
