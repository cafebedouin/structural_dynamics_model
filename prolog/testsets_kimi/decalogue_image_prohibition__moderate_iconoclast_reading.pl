% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__moderate_iconoclast_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Decalogue Image Prohibition (Moderate Iconoclast Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint instantiates the moderate_iconoclast_reading of the
 *   decalogue_image_prohibition kernel. The kernel is the Second Commandment
 *   prohibition against graven images. This reading distinguishes
 *   three-dimensional statuary (forbidden as bearing higher idolatry risk)
 *   from two-dimensional images (permitted under strict regulation). It
 *   presents itself as a prudent theological safeguard but operates
 *   structurally as a snare: the regulatory apparatus extracts compliance
 *   costs and concentrates gatekeeping power in the ecclesiastical hierarchy.
 *   The sibling readings are the total iconoclast prohibition (all imagery
 *   forbidden) and the iconodule position (matter sanctified, images honored
 *   as conduits). This story authors ONLY the moderate iconoclast reading as
 *   a clean Îµ-invariant constraint.
 *
 * KEY AGENTS:
 *   - ecclesiastical_regulators: Agenda-setter/beneficiary (institutional/arbitrage) â derive authority from dimensional interpretation and maintain gatekeeping power.
 *   - worship_communities: Payer (moderate/constrained) â bear compliance costs and restricted worship practice.
 *   - artisans_and_image_producers: Payer (powerless/constrained) â bear regulatory overhead and creative restriction.
 *   - iconodule_dissenters: Excluded (moderate/trapped) â would advocate for unrestricted material mediation but are marginalized.
 *   - theological_scholars: Observer (analytical/analytical) â document the interpretive addition of the dimensional distinction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.72).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.78).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Decalogue Image Prohibition (Moderate Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, 'd5b25284-3040-4a22-a007-63a3e00663aa').
narrative_ontology:cs_kernel_codification('d5b25284-3040-4a22-a007-63a3e00663aa', fixed_text).
narrative_ontology:cs_authority_grounding('d5b25284-3040-4a22-a007-63a3e00663aa', lineage).
narrative_ontology:cs_interpretation_layer_present('d5b25284-3040-4a22-a007-63a3e00663aa').
narrative_ontology:cs_reading_relation('d5b25284-3040-4a22-a007-63a3e00663aa', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5b25284-3040-4a22-a007-63a3e00663aa', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('d5b25284-3040-4a22-a007-63a3e00663aa', foundational, three_dimensional_idolatry_distinction).
narrative_ontology:cs_axiom_status(three_dimensional_idolatry_distinction, holdable).
narrative_ontology:cs_axiom_grounding('d5b25284-3040-4a22-a007-63a3e00663aa', three_dimensional_idolatry_distinction, theological).
narrative_ontology:cs_axiom('d5b25284-3040-4a22-a007-63a3e00663aa', foundational, regulated_two_dimensional_permissibility).
narrative_ontology:cs_axiom_status(regulated_two_dimensional_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('d5b25284-3040-4a22-a007-63a3e00663aa', regulated_two_dimensional_permissibility, theological).
narrative_ontology:cs_reference_frame('d5b25284-3040-4a22-a007-63a3e00663aa', dimensional_aniconism_baseline).
narrative_ontology:cs_drift_state('d5b25284-3040-4a22-a007-63a3e00663aa', post_regulatory_elaboration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d5b25284-3040-4a22-a007-63a3e00663aa', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulators).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, worship_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, artisans_and_image_producers).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, dimensional_idolatry_distinction).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, hierarchical_image_gatekeeping).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the commandment to forbid three-dimensional statuary as higher-risk idolatry while permitting two-dimensional images under strict regulation. Maintain doctrinal commissions, approve image designs, inspect worship spaces, and derive institutional authority from their exclusive gatekeeping role over visual culture.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Must remove or avoid three-dimensional devotional objects and submit two-dimensional images for ecclesiastical approval. Bear the cognitive burden of distinguishing licit from illicit representations, the material cost of replacing statuary, and the spiritual cost of restricted worship practice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, worship_communities, payer,
    moderate, biographical, constrained, local).

% Create religious art under dimensional constraints and doctrinal supervision. Must avoid sculptural forms and secure approval for paintings or icons. Bear wasted materials, delayed production, and lost commissions when works are rejected; their creative and economic survival depends on regulator favor.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, artisans_and_image_producers, payer,
    powerless, biographical, constrained, local).

% Hold that matter is sanctified and three-dimensional statuary is a legitimate conduit to the divine. They are structurally excluded from the regulatory conversation, their communities marginalized or driven underground. Were they present, they would argue the dimensional distinction is arbitrary and the regulation itself is the abuse.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_dissenters, excluded,
    moderate, generational, trapped, regional).

% Analyze the textual and historical basis for the 3D/2D distinction in the kernel commandment. They document that the prohibition against graven images does not explicitly differentiate dimensions, and that the regulatory hierarchy has added an interpretive layer that concentrates authority in the adjudicating institution.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, theological_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulators).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevent idolatry by restricting material representations of the divine to forms deemed less likely to be worshipped as divine themselves, while permitting regulated two-dimensional images as pedagogical or devotional aids subordinate to ecclesiastical oversight.
% TRANSFER_FUNCTION: Moves compliance burden, creative restriction, and deference from worship communities and artisans to the ecclesiastical regulatory authority; transfers gatekeeping power and the sole right to adjudicate licit images to the hierarchical institution.
% ABSENT_VOICES: Iconodule communities who regard matter as sanctified and statuary as a valid conduit of grace; artisans who would produce unrestricted religious art; worship communities that experience the dimensional rule as spiritually arbitrary. They are kept out of the regulatory conversation because their position is classified as idolatrous or abusive.
% DISAPPEARANCE_RATIONALE: If the prohibition and its regulatory apparatus vanished overnight, worship communities would restore three-dimensional statuary and unregulated two-dimensional imagery; the ecclesiastical gatekeeping monopoly would collapse; the distinction between licit and illicit visual culture would revert to communal or individual judgment rather than hierarchical monitoring.
% FOUNDING_PROBLEM: The risk that material representations of the divine become objects of worship in themselves (idolatry), substituting the image for the divine prototype and corrupting monotheistic devotion.
% FOUNDING_PROBLEM_CORROBORATION: Iconoclast theologians and the regulatory authority attest the problem is still live. Historians of religion and iconodule theologians from outside the benefiting party attest that the founding problem is either addressed through proper catechesis rather than dimensional regulation, or that the regulation itself has become the greater obstacle to authentic devotion.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the constraint extracts compliance costs and concentrates gatekeeping power rather than merely preventing idolatry. Suppression is higher (0.78) because persistence depends on active monitoring, approval regimes, and the exclusion of three-dimensional alternatives. Theater_ratio is substantial (0.55): a growing share of regulatory activity is performative maintenance of the gatekeeping function rather than genuine spiritual protection. Accessibility_collapse is high (0.68) because alternatives (unregulated images, statuary) are structurally closed by doctrinal classification. Resistance is moderate (0.45) because excluded iconodule communities and constrained artisans mount episodic pushback. The measurement series share one time grid, showing extraction and theater accumulating as the regulatory apparatus elaborates over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulators) experiences the constraint as necessary coordination protecting worship from idolatry. The payer seats (communities, artisans) experience it as enforced extraction of deference, material cost, and creative restriction. The excluded iconodule seat would experience it as heretical suppression of legitimate devotion. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical_regulators are the structural beneficiary: they collect gatekeeping power and compliance deference (low d). Worship_communities and artisans_and_image_producers are structural targets: they bear the compliance burden and have constrained exit (high d). Iconodule_dissenters are excluded and trapped within the heresy framework (very high d). Theological_scholars occupy an analytical seat with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâidolatry riskâmay have been genuine in origin, but the constraint's mandate has outlived its protective function. The regulatory apparatus now sustains itself through bureaucratic elaboration rather than spiritual safeguarding. The moderate reading prevents mislabeling the constraint as a rope (it is not benign coordination) or as a mountain (it is constructed interpretation, not natural law). The snare classification captures that the coordination story (preventing idolatry) serves as cover for extraction (gatekeeping power and compliance deference).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalness,
    'Is the moderate iconoclast reading a faithful derivation from the kernel text, or a constructed compromise that serves regulatory gatekeeping?',
    'Textual, archaeological, and historical analysis of the kernel commandment and early aniconic practice to determine whether a 3D/2D distinction is intrinsic or post-hoc.',
    'If constructed, the constraint is a false summit or snare using textual veneer; if faithful, it is a legitimate interpretive tradition whose extraction may represent necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_naturalness, conceptual, 'Whether the reading derives from the kernel or from institutional interest').

omega_variable(
    dimensional_distinction_basis,
    'Does the theological tradition provide a principled basis for the three-dimensional prohibition, or is it a regulatory convenience that extracts compliance through arbitrary classification?',
    'Comparative analysis of patristic and scriptural sources for dimensional distinctions in worship, versus the historical record of regulatory elaboration.',
    'A principled basis would lower extractiveness toward coordination cost; a regulatory convenience would confirm snare classification and raise theater_ratio.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dimensional_distinction_basis, empirical, 'Empirical basis for the 3D versus 2D theological distinction').

omega_variable(
    sibling_reading_boundary,
    'How would the structural classification change if the total iconoclast or iconodule sibling reading were adopted instead of this moderate reading?',
    'Cross-reference the compiled constraint family: compare beneficiaries, victims, and enforcement structures across the three sibling stories.',
    'Total iconoclast would likely shift toward mountain-like stricture or high-theater piton; iconodule would shift toward rope or scaffold with different beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Structural delta across the kernel''s three readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(deca_tr_t5, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(deca_tr_t15, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(deca_tr_t25, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(deca_be_t5, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(deca_be_t15, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(deca_be_t25, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 25, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(deca_su_t5, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(deca_su_t15, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(deca_su_t25, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the decalogue_image_prohibition kernel. The iconoclast_reading asserts total prohibition; the iconodule_reading asserts sanctified matter; this reading asserts a regulated dimensional compromise. Each reading carries a different Îµ, beneficiary/victim structure, and classification. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
