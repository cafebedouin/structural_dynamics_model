% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   This constraint represents the 'moderate iconoclast' reading of the
 *   Decalogue's prohibition on images, which forbids three-dimensional
 *   statuary due to higher idolatry risk but permits two-dimensional images
 *   under strict regulation to prevent abuse. It is a Snare-type constraint
 *   because it maintains a genuine coordination function (preventing
 *   idolatry) but extracts compliance costs through detailed regulation and
 *   monitoring, benefiting the regulatory authority by maintaining
 *   gatekeeping power. The claimed type is 'snare' because while it offers a
 *   'middle path,' it does so by imposing significant bureaucratic overhead
 *   and limiting expressive freedom, rather than genuinely coordinating all
 *   parties for mutual benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.65).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.75).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Decalogue Image Prohibition (Moderate Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, 'c0da37fe-8741-41fb-b04c-51d4c436597c').
narrative_ontology:cs_kernel_codification('c0da37fe-8741-41fb-b04c-51d4c436597c', fixed_text).
narrative_ontology:cs_authority_grounding('c0da37fe-8741-41fb-b04c-51d4c436597c', lineage).
narrative_ontology:cs_interpretation_layer_present('c0da37fe-8741-41fb-b04c-51d4c436597c').
narrative_ontology:cs_reading_relation('c0da37fe-8741-41fb-b04c-51d4c436597c', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0da37fe-8741-41fb-b04c-51d4c436597c', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('c0da37fe-8741-41fb-b04c-51d4c436597c', foundational, three_dimensional_images_inherently_idolatrous).
narrative_ontology:cs_axiom_status(three_dimensional_images_inherently_idolatrous, holdable).
narrative_ontology:cs_axiom_grounding('c0da37fe-8741-41fb-b04c-51d4c436597c', three_dimensional_images_inherently_idolatrous, theological).
narrative_ontology:cs_axiom('c0da37fe-8741-41fb-b04c-51d4c436597c', foundational, two_dimensional_images_permissible_with_regulation).
narrative_ontology:cs_axiom_status(two_dimensional_images_permissible_with_regulation, holdable).
narrative_ontology:cs_axiom_grounding('c0da37fe-8741-41fb-b04c-51d4c436597c', two_dimensional_images_permissible_with_regulation, conventional).
narrative_ontology:cs_reference_frame('c0da37fe-8741-41fb-b04c-51d4c436597c', regulated_visual_piety).
narrative_ontology:cs_drift_state('c0da37fe-8741-41fb-b04c-51d4c436597c', contemporary_pluralistic_society, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c0da37fe-8741-41fb-b04c-51d4c436597c', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, moderate_iconoclast_clergy).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, artists_and_artisans).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, devotional_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the prohibition, issuing detailed regulations for permissible two-dimensional images. Benefits from maintaining gatekeeping power over visual culture and defining acceptable religious practice. Their identity is fused with upholding the 'correct' interpretation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority, agenda_setter,
    institutional, generational, identity_locked, national).

% Adheres to and promotes this reading, gaining legitimacy and authority by navigating the nuanced rules. They benefit from a clear, albeit complex, framework that distinguishes their practice from both extreme iconoclasm and iconodulism.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, moderate_iconoclast_clergy, beneficiary,
    organized, biographical, constrained, regional).

% Bear the costs of compliance with strict regulations on religious imagery. They face limitations on artistic expression, bureaucratic hurdles for approval, and the risk of censure if their work is deemed to violate the prohibition. Their livelihood depends on producing permissible art.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, artists_and_artisans, payer,
    powerless, immediate, constrained, local).

% Desire visual aids for worship but must navigate the complex rules for two-dimensional images, often feeling a tension between their devotional needs and the strictures. They bear the cost of reduced access to desired forms of religious expression and the psychological burden of potential transgression.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, devotional_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Would argue for a complete ban on all religious imagery, viewing even two-dimensional images as a slippery slope to idolatry. Their more extreme interpretation is sidelined by this moderate reading, which they see as a compromise that still permits sin.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_faction, excluded,
    organized, generational, constrained, national).

% Would argue for the full permissibility of both two- and three-dimensional images, emphasizing their role as conduits to the divine. They are excluded from the interpretive framework of this reading, which they view as unnecessarily restrictive and denying a valid form of worship.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_faction, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, regulated visual language for religious expression, preventing uncontrolled proliferation of images that could lead to idolatry while allowing some material mediation of the divine.
% TRANSFER_FUNCTION: Transfers interpretive authority and gatekeeping power over religious visual culture to the regulatory authority, extracting compliance costs and limiting expressive freedom from artists and practitioners.
% ABSENT_VOICES: Both the strict iconoclast and iconodule factions are excluded. The iconoclasts would object to any images, while the iconodules would object to the restrictions on three-dimensional art and the detailed regulation of two-dimensional forms. Their absence allows the moderate position to maintain its authority.
% DISAPPEARANCE_RATIONALE: If the prohibition and its regulations vanished, there would be an immediate proliferation of diverse religious imagery, including three-dimensional forms. Artists would be freed from restrictions, and devotional practices would evolve to incorporate previously forbidden visual aids. The religious authority would lose a significant source of its control over public and private worship.
% FOUNDING_PROBLEM: The historical problem of idolatry, where material representations of the divine became objects of worship themselves, leading to spiritual corruption and deviation from monotheistic principles.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and historical texts from various traditions corroborate the historical and ongoing risk of idolatry. While the specific forms of idolatry may change, the underlying human tendency to reify the divine in material objects is widely acknowledged outside the immediate beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).

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
 *   The extractiveness (0.65) is driven by the compliance costs imposed on artists and practitioners, who must navigate complex rules and face limitations on their creative and devotional expression. Suppression (0.75) is high due to the active enforcement by the religious authority, which monitors and censures violations, effectively suppressing alternative forms of visual religious practice. The theater ratio (0.20) is relatively low, as the regulatory activity is genuinely aimed at preventing perceived idolatry, though some of it serves to reinforce the authority's gatekeeping role. Accessibility collapse is moderate (0.40) because two-dimensional images are permitted, but under such strictures that many alternatives are effectively foreclosed. Resistance (0.50) is moderate, as both iconoclast and iconodule factions push back against the compromise, and artists chafe under the restrictions.
 *
 * PERSPECTIVAL GAP:
 *   The religious regulatory authority and moderate iconoclast clergy experience this as a necessary and beneficial framework for maintaining spiritual purity and order. For artists and devotional practitioners, it is a burdensome system that limits their expression and practice, extracting compliance and psychological costs. The engine will compute these divergent experiences based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious regulatory authority and moderate iconoclast clergy are beneficiaries, as they gain interpretive authority, legitimacy, and control over religious visual culture. Artists, artisans, and devotional practitioners are victims, bearing the costs of compliance, limited expression, and potential censure. Their identity-locked exit options amplify the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing idolatry) is still live, but its implementation has shifted to a bureaucratic snare. The detailed regulation of two-dimensional images, while ostensibly serving the original mandate, also functions to maintain the authority's power and control over religious expression. This prevents mislabeling it as a pure Rope, as the extraction is significant and asymmetric, but also prevents mislabeling it as a pure Snare if the coordination function were entirely absent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    idolatry_risk_assessment,
    'Is the perceived ''higher idolatry risk'' of three-dimensional statuary empirically justified, or is it a theological interpretation that serves to differentiate this reading?',
    'Cross-cultural studies of religious practice and cognitive science of material engagement: do 3D objects inherently carry a higher risk of reification than 2D images across diverse contexts?',
    'If the risk is not empirically justified, the prohibition on 3D statuary becomes a purely conventional or deontological rule, increasing the perceived extractiveness of this reading. If justified, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(idolatry_risk_assessment, empirical, 'Empirical basis for differential idolatry risk between 2D and 3D images.').

omega_variable(
    regulatory_burden_vs_benefit,
    'Does the extensive regulatory overhead for two-dimensional images genuinely prevent abuse, or does it primarily serve to maintain the authority''s gatekeeping power?',
    'Comparative analysis of communities with less stringent regulation: do they experience higher rates of ''abuse'' (as defined by the authority)? Cost-benefit analysis of the regulatory apparatus.',
    'If the burden is disproportionate to the benefit, the extractiveness and suppression metrics for this reading would be higher, pushing it closer to a pure Snare. If proportionate, it reinforces the Tangled Rope aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_burden_vs_benefit, empirical, 'Efficiency and necessity of regulatory burden for 2D images.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative visual practices structural (regulatory barriers) or internalized (practitioners'' self-censorship due to identity fusion with the religious authority)?',
    'Post-relaxation trajectory: if regulatory barriers were removed, would practitioners immediately adopt previously forbidden forms, or would internalized norms persist?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — practitioners carry the suppression with them after formal barrier removal. This would amplify the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for visual religious practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(deca_tr_t25, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(deca_tr_t75, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 75, 0.19).
narrative_ontology:measurement(deca_tr_t100, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(deca_be_t25, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(deca_be_t75, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 75, 0.63).
narrative_ontology:measurement(deca_be_t100, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(deca_su_t25, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(deca_su_t75, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 75, 0.73).
narrative_ontology:measurement(deca_su_t100, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
