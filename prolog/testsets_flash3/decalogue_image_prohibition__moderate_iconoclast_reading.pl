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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Decalogue Image Prohibition (Moderate Iconoclast Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint represents the 'moderate iconoclast' reading of the
 *   Decalogue's prohibition on images, which forbids three-dimensional
 *   statuary due to higher idolatry risk but permits two-dimensional images
 *   under strict regulation to prevent abuse. It is one reading of the
 *   'decalogue_image_prohibition' kernel, alongside the 'iconoclast_reading'
 *   (total prohibition) and 'iconodule_reading' (permits honor through
 *   images). This reading attempts to navigate between extremes but results
 *   in a Snare-type constraint due to the high compliance costs and
 *   regulatory overhead it imposes on permissible practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.68).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.75).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Decalogue Image Prohibition (Moderate Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, 'dedf61ba-1a4e-4cdc-a98c-193c6332c3ee').
narrative_ontology:cs_kernel_codification('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', fixed_text).
narrative_ontology:cs_authority_grounding('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', lineage).
narrative_ontology:cs_interpretation_layer_present('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee').
narrative_ontology:cs_reading_relation('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_axiom('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', foundational, three_dimensional_images_inherently_idolatrous).
narrative_ontology:cs_axiom_status(three_dimensional_images_inherently_idolatrous, holdable).
narrative_ontology:cs_axiom_grounding('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', three_dimensional_images_inherently_idolatrous, theological).
narrative_ontology:cs_axiom('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', foundational, two_dimensional_images_permissible_with_regulation).
narrative_ontology:cs_axiom_status(two_dimensional_images_permissible_with_regulation, holdable).
narrative_ontology:cs_axiom_grounding('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', two_dimensional_images_permissible_with_regulation, conventional).
narrative_ontology:cs_reference_frame('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', post_decalogue_interpretive_tradition).
narrative_ontology:cs_drift_state('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', contemporary_visual_culture_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('dedf61ba-1a4e-4cdc-a98c-193c6332c3ee', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, religious_authorities).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, theological_scholars).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, religious_artists).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, congregants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the prohibition, issuing detailed regulations for permissible two-dimensional images and monitoring compliance. They benefit from maintaining their interpretive authority and the bureaucratic overhead of regulation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Provide the intellectual framework and justifications for the moderate iconoclast position, contributing to the detailed regulations. Their academic and interpretive authority is reinforced by the constraint.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, theological_scholars, beneficiary,
    organized, generational, constrained, regional).

% Are restricted to two-dimensional forms and must adhere to strict stylistic and thematic guidelines, incurring creative and economic costs. Their work is subject to review and potential rejection by religious authorities.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_artists, payer,
    moderate, biographical, constrained, local).

% Experience a limited visual culture in their worship spaces, potentially feeling a spiritual cost from the absence of certain forms of religious art. Their identity is often deeply tied to the religious tradition, making exit unthinkable.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, congregants, payer,
    powerless, biographical, identity_locked, local).

% Would argue for the complete prohibition of all religious imagery, viewing even two-dimensional images as a slippery slope to idolatry. Their more extreme position is suppressed by the moderate consensus.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, radical_iconoclasts, excluded,
    moderate, biographical, constrained, local).

% Would argue for the spiritual benefit and theological legitimacy of three-dimensional statuary and a broader range of visual expression. Their position is explicitly rejected by this reading.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodules, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate religious practice by providing a clear, albeit restrictive, framework for visual art, aiming to prevent idolatry while allowing some material mediation of the divine.
% TRANSFER_FUNCTION: Transfers interpretive and regulatory power to religious authorities and theological scholars, while imposing creative and spiritual costs on artists and congregants.
% ABSENT_VOICES: Radical iconoclasts would demand total prohibition, and iconodules would advocate for broader use of imagery, including statuary. Both are excluded from the interpretive consensus that defines this constraint.
% DISAPPEARANCE_RATIONALE: If this specific prohibition vanished, religious artists would immediately explore three-dimensional forms, congregants would demand a richer visual culture, and the authority of the religious institutions would be challenged as the interpretive framework collapsed.
% FOUNDING_PROBLEM: The problem of idolatry and the potential for material representations to distract from or replace the worship of God, as articulated in the Decalogue.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities and theological scholars attest that the risk of idolatry remains live, requiring ongoing vigilance. However, artists and some congregants argue that the current regulations are overly restrictive and that the original problem can be addressed with less severe constraints; independent art historians and cultural critics also attest to the stifling effect on religious art.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Snare because it imposes significant costs (creative, spiritual, bureaucratic) on those it governs, while primarily benefiting the religious authorities and scholars who maintain its interpretive and enforcement power. Extractiveness is high (0.68) due to the ongoing costs of compliance and the stifling of artistic expression. Suppression (0.75) is also high, as the religious authorities actively enforce the rules and suppress alternative interpretations. The theater ratio (0.40) reflects that while preventing idolatry is a genuine concern, a substantial portion of the regulatory activity serves to maintain the authority's gatekeeping role rather than directly addressing the core spiritual risk. The time series shows a gradual increase in extractiveness and suppression as the regulatory framework hardens over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious authorities, this constraint is a necessary Rope, balancing spiritual purity with the need for some visual aids. From the perspective of artists and congregants, it is a Snare, imposing arbitrary restrictions and costs under the guise of spiritual protection. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities and theological scholars are beneficiaries (d near 0.0) as they gain interpretive power and control over religious expression. Religious artists and congregants are targets (d near 1.0) as they bear the costs of restricted creative freedom and spiritual expression. Radical iconoclasts and iconodules are excluded, their alternative positions actively suppressed by the dominant moderate interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    idolatry_risk_quantification,
    'Is the perceived ''higher idolatry risk'' of three-dimensional statuary empirically verifiable, or is it a theological construct used to justify the prohibition?',
    'Sociological and anthropological studies of religious practice across traditions, examining the actual incidence of idolatry in cultures with varying visual art forms. Theological re-evaluation of the nature of idolatry.',
    'If the risk is not empirically verifiable, the prohibition on statuary is a purely conventional or theological constraint, and its extractiveness is less justified by a ''coordination'' function. If verifiable, the constraint''s coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(idolatry_risk_quantification, empirical, 'Empirical basis for the differential idolatry risk of 2D vs. 3D images.').

omega_variable(
    interpretive_authority_legitimacy,
    'Is the interpretive authority of the religious institutions grounded in genuine spiritual insight and tradition, or is it maintained through the bureaucratic control of religious expression?',
    'Internal theological reform movements challenging the basis of interpretive authority, or external sociological analysis of power dynamics within the religious hierarchy.',
    'If authority is primarily bureaucratic, the constraint''s extractiveness is more clearly a function of power maintenance. If genuinely spiritual, the extraction might be re-read as a cost of maintaining a sacred tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'The true grounding of the interpretive authority enforcing the constraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional rules, monitoring) or internalized (congregants'' self-censorship, artists'' fear of censure)?',
    'Post-exit suppression trajectory: if creative self-censorship persists after the institutional enforcement is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — artists and congregants carry the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
