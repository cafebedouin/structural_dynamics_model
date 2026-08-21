% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconodule_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Decalogue Image Prohibition (Iconodule Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint represents the 'iconodule' reading of the Decalogue's
 *   image prohibition, which permits the veneration of images (dulia) as
 *   conduits to the divine, distinct from the worship due to God alone
 *   (latria). This reading is grounded in the theological understanding of
 *   the Incarnation, which sanctifies matter as a valid medium for divine
 *   presence. It functions as a Rope, coordinating the visual culture of the
 *   church and enabling spiritual practice without significant extraction,
 *   provided the theological distinctions are maintained. This is one reading
 *   of the 'decalogue_image_prohibition' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.15).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.2).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Decalogue Image Prohibition (Iconodule Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '439ac647-726f-4b87-ae24-1f9c9b916be6').
narrative_ontology:cs_kernel_codification('439ac647-726f-4b87-ae24-1f9c9b916be6', fixed_text).
narrative_ontology:cs_authority_grounding('439ac647-726f-4b87-ae24-1f9c9b916be6', lineage).
narrative_ontology:cs_interpretation_layer_present('439ac647-726f-4b87-ae24-1f9c9b916be6').
narrative_ontology:cs_reading_relation('439ac647-726f-4b87-ae24-1f9c9b916be6', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('439ac647-726f-4b87-ae24-1f9c9b916be6', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('439ac647-726f-4b87-ae24-1f9c9b916be6', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('439ac647-726f-4b87-ae24-1f9c9b916be6', incarnation_sanctifies_matter, theological).
narrative_ontology:cs_axiom('439ac647-726f-4b87-ae24-1f9c9b916be6', foundational, dulia_distinct_from_latria).
narrative_ontology:cs_axiom_status(dulia_distinct_from_latria, holdable).
narrative_ontology:cs_axiom_grounding('439ac647-726f-4b87-ae24-1f9c9b916be6', dulia_distinct_from_latria, theological).
narrative_ontology:cs_reference_frame('439ac647-726f-4b87-ae24-1f9c9b916be6', second_council_of_nicaea_doctrine).
narrative_ontology:cs_drift_state('439ac647-726f-4b87-ae24-1f9c9b916be6', contemporary_secular_context, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('439ac647-726f-4b87-ae24-1f9c9b916be6', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, clergy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a sanctioned means of visual devotion and spiritual connection through icons, which serve as conduits to the divine. Their participation in worship is enhanced by visual aids, but they must adhere to prescribed forms of veneration.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, laity, beneficiary,
    moderate, biographical, constrained, local).

% Administers and interprets the theological distinction between latria (worship) and dulia (veneration), ensuring orthodox practice. They benefit from a stable framework for religious instruction and liturgical practice, but bear the responsibility of preventing idolatry.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, clergy, agenda_setter,
    institutional, generational, constrained, regional).

% Benefits from a legitimate and demand-driven profession, creating sacred art for churches and private devotion. Their work is guided by strict theological and artistic canons, ensuring the icons serve their proper spiritual function.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_creators, beneficiary,
    moderate, biographical, constrained, local).

% Are excluded from the dominant religious discourse and practice, as their rejection of all religious imagery is deemed heretical by this reading. They would argue for a stricter interpretation of the Decalogue, but their views are suppressed within the established church.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_factions, excluded,
    organized, generational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual practice of the laity and the liturgical functions of the clergy by providing a theologically sound framework for the use of sacred images, enabling a consistent and unified approach to visual devotion within the church.
% TRANSFER_FUNCTION: Facilitates the transfer of spiritual understanding and devotion from the material image to its divine prototype, channeling reverence and prayer through sanctioned visual conduits. It also transfers authority over religious art from individual interpretation to ecclesiastical oversight.
% ABSENT_VOICES: Strict iconoclast factions, who believe all religious imagery constitutes idolatry, are absent from the conversation. They would argue for the destruction of all icons and the cessation of their veneration, but their theological position is deemed heterodox by this reading.
% DISAPPEARANCE_RATIONALE: If this theological distinction vanished, the entire visual culture of the church would collapse. Icons would either be treated as idols (leading to their destruction) or as mere art (losing their spiritual function), fundamentally altering worship practices and the relationship between the material and the divine.
% FOUNDING_PROBLEM: The early church faced the challenge of integrating visual culture into worship without falling into idolatry, while also affirming the Incarnation's sanctification of matter.
% FOUNDING_PROBLEM_CORROBORATION: Theological scholars and church historians attest to the ongoing relevance of this problem, as the tension between material representation and spiritual worship remains a perennial concern in religious thought and practice, corroborated by centuries of theological debate and artistic development.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconodule_reading_tests).
:- end_tests(decalogue_image_prohibition__iconodule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary function is coordination of spiritual practice and theological understanding, with minimal coercive overhead. Suppression (0.2) is present to enforce the theological distinction and prevent actual idolatry, but it is not primarily extractive. Theater ratio is low (0.05) as the practices are genuinely functional for spiritual life. The metrics are stable over time, reflecting the enduring theological consensus of this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the iconodule, this constraint is a necessary and beneficial framework for spiritual life. From an iconoclast perspective (an excluded voice), the same constraint would be seen as a dangerous compromise with idolatry, leading to spiritual corruption. The engine's classification of this reading as a Rope reflects its internal coherence and coordination function, not an adjudication of the broader theological dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The laity, clergy, and icon creators are beneficiaries, as the constraint enables their spiritual and professional lives. There are no direct 'victims' within this reading, as the constraint aims to prevent idolatry, which is seen as harmful to all. Iconoclast factions are 'excluded' as their views are deemed heterodox, but they are not directly 'victimized' by the operation of this specific constraint, which is about permissible practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_distinction_clarity,
    'Is the theological distinction between latria (worship) and dulia (veneration) sufficiently clear and consistently applied in practice to prevent actual idolatry among the laity?',
    'Empirical studies of lay religious practice and belief, theological surveys, and historical analysis of periods of perceived abuse of images.',
    'If the distinction is consistently blurred in practice, the constraint''s effective extractiveness (spiritual harm from idolatry) would be higher, potentially shifting its classification towards a Tangled Rope or Snare from a theological perspective, as it would fail its primary protective function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_distinction_clarity, empirical, 'Clarity and practical application of the latria/dulia distinction.').

omega_variable(
    incarnation_sanctification_scope,
    'To what extent does the Incarnation''s sanctification of matter extend to all forms of material representation, or are there inherent limits that would exclude certain types of images (e.g., three-dimensional statues)?',
    'Further theological development and ecumenical dialogue on the implications of Christology for visual theology, potentially informed by historical precedent and patristic consensus.',
    'If the Incarnation''s sanctification is found to have inherent limits that exclude certain image types, this reading would be foreclosed by a more restrictive (e.g., moderate_iconoclast) reading, leading to a re-evaluation of permissible visual forms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incarnation_sanctification_scope, conceptual, 'Scope of Incarnation''s sanctification of matter for visual representation.').

omega_variable(
    iconoclast_suppression_justification,
    'Is the suppression of iconoclast factions by this reading justified as protecting orthodoxy, or does it constitute an undue extraction of religious freedom and expression?',
    'Analysis from a political theology perspective, examining the power dynamics and historical consequences of suppressing dissenting religious views, potentially drawing on secular human rights frameworks.',
    'If the suppression is deemed an undue extraction, the overall extractiveness of the broader religious authority structure (which includes this reading) would be higher, and the ''excluded'' status of iconoclasts would shift towards ''victims'' in a more critical analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iconoclast_suppression_justification, preference, 'Justification of suppressing iconoclast views.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(deca_tr_t300, decalogue_image_prohibition__iconodule_reading, theater_ratio, 300, 0.05).
narrative_ontology:measurement(deca_tr_t600, decalogue_image_prohibition__iconodule_reading, theater_ratio, 600, 0.05).
narrative_ontology:measurement(deca_tr_t900, decalogue_image_prohibition__iconodule_reading, theater_ratio, 900, 0.05).
narrative_ontology:measurement(deca_tr_t1200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1200, 0.05).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(deca_be_t300, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 300, 0.15).
narrative_ontology:measurement(deca_be_t600, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 600, 0.15).
narrative_ontology:measurement(deca_be_t900, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 900, 0.15).
narrative_ontology:measurement(deca_be_t1200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(deca_su_t300, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 300, 0.2).
narrative_ontology:measurement(deca_su_t600, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 600, 0.2).
narrative_ontology:measurement(deca_su_t900, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 900, 0.2).
narrative_ontology:measurement(deca_su_t1200, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1200, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'decalogue_image_prohibition' kernel. Its classification as a Rope reflects its internal coherence and coordination function, distinct from other readings that may classify differently due to different theological premises and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
