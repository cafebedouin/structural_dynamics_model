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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Interpretation of Image Veneration
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint represents the iconodule (venerator of images) reading of
 *   the Decalogue's prohibition against idolatry, as articulated and defended
 *   by various Christian traditions, particularly in the aftermath of the
 *   Iconoclast controversies. It permits the veneration of images (dulia) as
 *   honor directed to their divine prototypes, while strictly forbidding
 *   their worship (latria). This interpretation is grounded in the
 *   theological understanding that the Incarnation sanctifies matter, making
 *   it a valid conduit to the divine. The constraint functions as a Rope,
 *   coordinating the faithful's visual culture and devotional practices
 *   within defined theological boundaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.22).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.28).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Interpretation of Image Veneration").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'df46d057-d395-47ee-b2a9-1b2c30ef4611').
narrative_ontology:cs_kernel_codification('df46d057-d395-47ee-b2a9-1b2c30ef4611', fixed_text).
narrative_ontology:cs_authority_grounding('df46d057-d395-47ee-b2a9-1b2c30ef4611', lineage).
narrative_ontology:cs_interpretation_layer_present('df46d057-d395-47ee-b2a9-1b2c30ef4611').
narrative_ontology:cs_reading_relation('df46d057-d395-47ee-b2a9-1b2c30ef4611', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('df46d057-d395-47ee-b2a9-1b2c30ef4611', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('df46d057-d395-47ee-b2a9-1b2c30ef4611', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('df46d057-d395-47ee-b2a9-1b2c30ef4611', incarnation_sanctifies_matter, theological).
narrative_ontology:cs_axiom('df46d057-d395-47ee-b2a9-1b2c30ef4611', foundational, distinction_between_latria_and_dulia).
narrative_ontology:cs_axiom_status(distinction_between_latria_and_dulia, holdable).
narrative_ontology:cs_axiom_grounding('df46d057-d395-47ee-b2a9-1b2c30ef4611', distinction_between_latria_and_dulia, conventional).
narrative_ontology:cs_reference_frame('df46d057-d395-47ee-b2a9-1b2c30ef4611', patristic_icon_veneration).
narrative_ontology:cs_drift_state('df46d057-d395-47ee-b2a9-1b2c30ef4611', post_iconoclast_resolution, gap(stable, minor, true)).
narrative_ontology:cs_created_at('df46d057-d395-47ee-b2a9-1b2c30ef4611', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, clergy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, artists).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, church_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the theological distinction between worship (latria) and veneration (dulia) of images, ensuring orthodox practice and preventing idolatry. It provides the doctrinal framework that permits and guides the use of sacred art.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, church_magisterium, agenda_setter,
    institutional, generational, analytical, universal).

% Benefits from sacred images as aids to devotion, spiritual instruction, and connection to the divine prototypes. Their worship is facilitated and enriched by visual culture, within the boundaries set by the Church.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, laity, beneficiary,
    moderate, biographical, constrained, global).

% Mediates the use of sacred images in liturgical practice, teaches the theological distinctions, and guides the faithful in proper veneration. They benefit from a clear doctrinal framework for their pastoral duties.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, clergy, beneficiary,
    organized, biographical, constrained, global).

% Are commissioned to create sacred images (icons, frescoes, mosaics) that conform to theological and artistic canons. They benefit from the patronage and cultural significance afforded by the sanctioned use of religious art.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, artists, beneficiary,
    moderate, biographical, constrained, regional).

% House, preserve, and display sacred images, integrating them into their architecture and liturgical life. They benefit from the cultural heritage and devotional practices enabled by the iconodule position.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, church_institutions, beneficiary,
    institutional, generational, constrained, global).

% Historically and ideologically oppose any use of religious imagery in worship, viewing it as idolatry. They are excluded from the doctrinal and practical framework of the iconodule reading, their arguments having been formally rejected by ecumenical councils.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_factions, excluded,
    organized, biographical, constrained, global).

% Study, interpret, and articulate the theological foundations of the iconodule position, engaging with historical debates and contemporary challenges to its understanding and practice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theologians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, diffuse).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a sanctioned and theologically coherent means for the faithful to honor divine prototypes through sacred images, facilitating spiritual devotion, instruction, and the integration of visual culture into religious life.
% TRANSFER_FUNCTION: Transfers spiritual understanding, devotion, and grace from the divine prototype to the laity via the material image; transfers cultural significance, patronage, and theological expression to artists and church institutions.
% ABSENT_VOICES: Iconoclast factions who would argue for a complete prohibition of religious imagery, seeing any use as idolatry. They are structurally excluded from the doctrinal framework of this reading, their positions having been formally condemned.
% DISAPPEARANCE_RATIONALE: If the iconodule interpretation vanished, the entire visual culture of many Christian traditions would collapse, leading to widespread destruction of sacred art, profound disruption of liturgical and devotional practices, and a fundamental shift in theological understanding of the Incarnation and the role of matter in salvation.
% FOUNDING_PROBLEM: To reconcile the Old Testament prohibition against idolatry with the Christian practice of venerating sacred images, particularly in light of the Incarnation's affirmation of matter as a conduit for the divine.
% FOUNDING_PROBLEM_CORROBORATION: The Second Council of Nicaea (787 AD) formally articulated and defended this position, drawing on patristic writings and established liturgical practice. The theological problem, while doctrinally settled for many traditions, remains a live point of discussion in broader religious and interfaith contexts, corroborated by ongoing theological scholarship and historical studies.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.22, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint's extractiveness is low because its primary function is to enable and guide a form of worship, not to extract resources. Suppression is moderate, reflecting the need for active theological instruction and occasional enforcement to maintain the distinction between dulia and latria, and to ensure images conform to orthodox canons. Theater ratio is low, indicating that the theological and pastoral functions are genuine and not merely performative. The metrics reflect a stable, well-established coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Church Magisterium, this is a foundational Rope, enabling orthodox worship. From the perspective of the laity, it is a beneficial guide for devotion. Iconoclast factions, if included, would experience this as a Snare, as it legitimizes practices they consider idolatrous and suppresses their alternative interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church Magisterium acts as the agenda-setter, defining and enforcing the doctrinal boundaries. The laity, clergy, artists, and church institutions are all beneficiaries, as the constraint enables and enriches their spiritual and cultural lives. There are no direct 'victims' within this reading, as it defines permissible practice; those who reject it (iconoclast factions) are 'excluded' from its framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_decalogue_prohibition,
    'Is this constraint accurately representing the ''iconodule_reading'' of the Decalogue''s image prohibition, or does it conflate aspects of other readings?',
    'Detailed textual analysis of patristic and conciliar documents, comparing the articulated theological positions with the structural properties of this constraint.',
    'If conflated, the constraint would need to be decomposed into more precise readings, each with its own distinct ε and structural properties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_decalogue_prohibition, conceptual, 'Verifies the fidelity of this constraint to the specific iconodule reading.').

omega_variable(
    iconoclast_structural_delta,
    'What would be the structural changes if the ''iconoclast_reading'' were adopted instead of this ''iconodule_reading''?',
    'Historical analysis of periods of iconoclasm, documenting the destruction of art, suppression of veneration practices, and persecution of icon-venerators.',
    'The ''iconoclast_reading'' would likely compute as a Snare or Tangled Rope, with high extraction (destruction of property, seizure of assets) and high suppression, and a large victim set including artists, laity, and clergy who practice veneration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iconoclast_structural_delta, empirical, 'Compares the structural outcomes of the iconodule vs. iconoclast interpretations.').

omega_variable(
    dulia_latria_distinction_in_practice,
    'How consistently is the theological distinction between dulia (veneration) and latria (worship) maintained in actual practice by the laity?',
    'Ethnographic studies of devotional practices, surveys of religious belief, and theological education effectiveness assessments.',
    'If the distinction is frequently blurred in practice, the effective extractiveness and suppression of the constraint might be higher, as it would be extracting ''improper'' worship or suppressing ''true'' understanding, potentially shifting the classification towards a Tangled Rope due to unintended extraction from the faithful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dulia_latria_distinction_in_practice, empirical, 'Assesses the practical application and understanding of the dulia/latria distinction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(deca_tr_t200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 200, 0.09).
narrative_ontology:measurement(deca_tr_t400, decalogue_image_prohibition__iconodule_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement(deca_tr_t600, decalogue_image_prohibition__iconodule_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(deca_tr_t800, decalogue_image_prohibition__iconodule_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(deca_tr_t1000, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(deca_tr_t1200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(deca_be_t200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 200, 0.2).
narrative_ontology:measurement(deca_be_t400, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 400, 0.21).
narrative_ontology:measurement(deca_be_t600, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 600, 0.22).
narrative_ontology:measurement(deca_be_t800, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 800, 0.22).
narrative_ontology:measurement(deca_be_t1000, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1000, 0.22).
narrative_ontology:measurement(deca_be_t1200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1200, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(deca_su_t200, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 200, 0.22).
narrative_ontology:measurement(deca_su_t400, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 400, 0.25).
narrative_ontology:measurement(deca_su_t600, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 600, 0.28).
narrative_ontology:measurement(deca_su_t800, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 800, 0.28).
narrative_ontology:measurement(deca_su_t1000, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1000, 0.28).
narrative_ontology:measurement(deca_su_t1200, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1200, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
