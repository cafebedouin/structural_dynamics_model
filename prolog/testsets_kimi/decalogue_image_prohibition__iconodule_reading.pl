% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Iconodule Decalogue Reading: Dulia through Material Mediation
 *   domain: theological/religious_authority
 *
 * SUMMARY:
 *   This constraint instantiates the iconodule_reading of the
 *   decalogue_image_prohibition kernel. It holds that the Decalogue forbids
 *   worship of images (latria) but permits honor through images to their
 *   prototypes (dulia), grounded in the Incarnation's sanctification of
 *   matter. The sibling readings are the iconoclast_reading (absolute
 *   prohibition of all religious imagery) and the moderate_iconoclast_reading
 *   (prohibition of three-dimensional statuary only). This reading functions
 *   as a coordination mechanism for orthodox Christian visual culture,
 *   distinguishing permissible devotion from forbidden idolatry through
 *   magisterial authority and orthodox intent.
 *
 * KEY AGENTS:
 *   - Ecclesiastical magisterium: agenda_setter (institutional/global/constrained) â defines the latria/dulia boundary and orthodox depiction norms.
 *   - Iconodule community: primary beneficiary (organized/global/identity_locked) â practices sanctioned material devotion depending on the doctrinal distinction.
 *   - Iconoclast dissidents: excluded (organized/global/trapped) â reject material mediation entirely and are structurally outside the normative framework.
 *   - Interfaith monotheist observers: analytical observers (organized/global/analytical) â corroborate the founding problem from outside the benefiting parties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.22).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.2).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Decalogue Reading: Dulia through Material Mediation").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theological/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '0a336d7f-c4ed-4fa4-8fb5-d93b261f0446').
narrative_ontology:cs_kernel_codification('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', fixed_text).
narrative_ontology:cs_authority_grounding('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', lineage).
narrative_ontology:cs_interpretation_layer_present('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446').
narrative_ontology:cs_reading_relation('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', incarnation_sanctifies_matter, theological).
narrative_ontology:cs_axiom('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', foundational, dulia_distinct_from_latria).
narrative_ontology:cs_axiom_status(dulia_distinct_from_latria, holdable).
narrative_ontology:cs_axiom_grounding('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', dulia_distinct_from_latria, theological).
narrative_ontology:cs_reference_frame('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', nicaean_iconodulia_framework).
narrative_ontology:cs_drift_state('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', contemporary_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0a336d7f-c4ed-4fa4-8fb5-d93b261f0446', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, iconodule_community).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, ecclesiastical_magisterium).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnation_theology).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, nicaean_orthodoxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the authority to define orthodox Christian doctrine, adjudicating which material representations are valid channels of divine honor (dulia) versus forbidden idolatry (latria). Maintains the interpretive tradition that the Incarnation sanctifies matter, setting the boundaries for sanctioned visual culture across the Church.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, ecclesiastical_magisterium, agenda_setter,
    institutional, civilizational, constrained, global).

% Comprises clergy, monastics, and laity who practice devotion through icons, relics, and sanctified matter. Their worship depends on the doctrinal distinction that permits dulia while forbidding latria; the constraint coordinates their access to the divine through material mediation validated by orthodox intent.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconodule_community, beneficiary,
    organized, generational, identity_locked, global).

% Theologians and communities who reject any material mediation in worship as idolatrous. Under regimes where the iconodule reading is enforced, their voices are structurally excluded from normative discourse; they would reject the latria/dulia distinction entirely but are not parties to the internal doctrinal settlement.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_dissidents, excluded,
    organized, generational, trapped, global).

% Jewish, Muslim, and non-orthodox Christian commentators who observe the Decalogue prohibition from outside the iconodule tradition. They corroborate that image-use in worship poses a genuine theological problem but do not accept the iconodule solution; their observation attests the founding problem without benefiting from this arrangement.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, interfaith_monotheist_observers, observer,
    organized, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes impermissible divine worship (latria) from permissible material honor (dulia), enabling a communal visual culture that mediates sacred presence without collapsing into idolatry; coordinates orthodox laity, clergy, and artisans around sanctioned devotional objects and shared intent.
% TRANSFER_FUNCTION: Moves spiritual legitimacy and access from direct immaterial encounter to material representations that meet orthodox intent and content standards; moves definitional authority to the magisterium to adjudicate which images, postures, and intentions are valid.
% ABSENT_VOICES: Iconoclast theologians who reject any material mediation as idolatrous; Jewish and Muslim interpreters who view the Decalogue prohibition as absolute and non-negotiable; non-orthodox Christian communities who see the latria/dulia distinction as doctrinal overreach; these voices are present in interfaith and ecumenical space but structurally excluded from the internal norm-setting conversation.
% DISAPPEARANCE_RATIONALE: If the distinction between latria and dulia vanished, iconodule devotional practice would lose its theological justification and reorganizeâeither toward aniconic worship (iconoclast pole), toward unregulated material devotion risking latria, or toward a purely interiorized piety. The global material culture of Orthodox and Catholic devotion depends on this boundary.
% FOUNDING_PROBLEM: How to maintain monotheistic worship of an immaterial God while preserving the accessibility and Incarnational reality of divine presence through material creation, without replicating pagan idolatry.
% FOUNDING_PROBLEM_CORROBORATION: Iconoclast theologians within the Christian tradition attest the idolatry risk as a live problem but reject this solution. Jewish and Muslim commentators on the Decalogue corroborate that image-use in worship poses a genuine theological problem. External corroboration exists; the solution is contested.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.22, 'kimi-k2.6', 'none', direct).

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
 *   The constraint scores low on extractiveness (0.22) because its primary operation is coordinating devotional practice around a shared theological distinction rather than extracting surplus from participants. Suppression is low (0.20) because persistence depends mainly on tradition and consensus rather than active coercion, though iconoclast dissent is marginalized. Theater ratio is low (0.15): the distinction is functionally operative in liturgy, iconography, and catechesis. Accessibility collapse is moderate (0.45): once the Incarnation-based framework is accepted, iconoclastic alternatives lose internal plausibility, but external alternatives remain live. Resistance is low (0.25): ongoing iconoclast critique and secular skepticism keep resistance non-zero but not dominant.
 *
 * PERSPECTIVAL GAP:
 *   From the iconodule seat, the arrangement is genuine coordination that prevents idolatry while enabling Incarnational devotion. From the iconoclast seat, the same structure is a snare that sanctifies idolatry under theological cover. The engine computes this divergence from structural data: the iconodule seat has beneficiary role and identity-locked exit, while the iconoclast seat is excluded with trapped exit under iconodule regimes.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical magisterium and iconodule community are structural beneficiaries: the constraint subsidizes their devotional and interpretive practice (low d). The iconoclast dissidents, when considered as excluded parties under this framework, sit at high d because the constraint's operation depends on their theological position being ruled out. Interfaith observers occupy the analytical atom with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists mandatrophy because its founding problemânegotiating monotheistic worship with material mediationâremains live across multiple traditions. It has not atrophied into a piton because the coordination function is actively exercised in liturgy and art production; the low theater ratio confirms that performance has not replaced function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_boundary_empirical_resolvability,
    'Can the distinction between latria and dulia be operationalized independently of the magisterium''s interpretive authority, or does the constraint''s coordination function collapse into authority-dependent extraction when no external verification exists?',
    'Comparative ethnography of devotional practice across Orthodox communities to determine whether the boundary is practiced consistently without centralized enforcement; historical analysis of pre-Nicaean II material devotion.',
    'If the boundary is only enforceable through magisterial authority, the constraint leans toward tangled_rope (coordination fused with agenda-setter extraction); if the boundary is self-executing in practice, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_boundary_empirical_resolvability, empirical, 'Whether the latria/dulia boundary has independent operational content.').

omega_variable(
    kernel_reading_relationship,
    'Does the iconodule reading foreclose the iconoclast reading within a unified theological framework, or do they remain logically co-tenable as different communities'' commitments?',
    'Formal analysis of the axioms: if the Incarnation-sanctifies-matter premise is held, does it entail the negation of the absolute prohibition premise? Historical evidence from Nicaea II anathemas suggests forecloses, but hermeneutic pluralism suggests coexists_with.',
    'Foreclosure makes the iconodule reading structurally brittle (one must be heretical); coexistence makes it a negotiated coordination mechanism whose persistence depends on institutional dominance rather than logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Logical relationship between iconodule and iconoclast readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iconodule_dec_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(iconodule_dec_tr_t200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 200, 0.11).
narrative_ontology:measurement(iconodule_dec_tr_t400, decalogue_image_prohibition__iconodule_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement(iconodule_dec_tr_t600, decalogue_image_prohibition__iconodule_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement(iconodule_dec_tr_t800, decalogue_image_prohibition__iconodule_reading, theater_ratio, 800, 0.11).
narrative_ontology:measurement(iconodule_dec_tr_t1000, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1000, 0.13).
narrative_ontology:measurement(iconodule_dec_tr_t1200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1200, 0.15).

% Extraction over time
narrative_ontology:measurement(iconodule_dec_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(iconodule_dec_be_t200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 200, 0.21).
narrative_ontology:measurement(iconodule_dec_be_t400, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 400, 0.2).
narrative_ontology:measurement(iconodule_dec_be_t600, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 600, 0.22).
narrative_ontology:measurement(iconodule_dec_be_t800, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 800, 0.21).
narrative_ontology:measurement(iconodule_dec_be_t1000, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1000, 0.23).
narrative_ontology:measurement(iconodule_dec_be_t1200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1200, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(decalogue_image_prohibition__iconodule_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is the iconodule reading of the decalogue_image_prohibition kernel, distinct from the iconoclast_reading (absolute prohibition) and moderate_iconoclast_reading (3D prohibition). The same Decalogue text grounds multiple structurally distinct constraints; decomposition follows the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
