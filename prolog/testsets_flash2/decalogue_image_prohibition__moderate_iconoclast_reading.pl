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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Moderate Iconoclast Reading of Decalogue Image Prohibition
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint represents the 'moderate iconoclast' reading of the
 *   Decalogue's prohibition on images, which forbids three-dimensional
 *   statuary due to higher idolatry risk but permits two-dimensional images
 *   under strict regulation to prevent abuse. It is a Snare-type constraint
 *   because it extracts compliance costs through detailed regulation and
 *   monitoring, imposing bureaucratic overhead on permissible practices while
 *   benefiting the regulatory authority. The claim/metric gap is deliberate:
 *   the constraint is CLAIMED as a Snare (the structural reality of this
 *   reading) while the authored metrics describe its substantially
 *   extractive, actively enforced operation. The engine measures that
 *   divergence; do not reconcile the claim to the metrics.
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
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Moderate Iconoclast Reading of Decalogue Image Prohibition").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '1e03cf9c-ae3c-49a6-a051-925bbb6d18a0').
narrative_ontology:cs_kernel_codification('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', fixed_text).
narrative_ontology:cs_authority_grounding('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', lineage).
narrative_ontology:cs_interpretation_layer_present('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0').
narrative_ontology:cs_reading_relation('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', foundational, differential_idolatry_risk_by_dimension).
narrative_ontology:cs_axiom_status(differential_idolatry_risk_by_dimension, holdable).
narrative_ontology:cs_axiom_grounding('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', differential_idolatry_risk_by_dimension, theological).
narrative_ontology:cs_axiom('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', secondary, regulation_prevents_abuse_of_permissible_images).
narrative_ontology:cs_axiom_status(regulation_prevents_abuse_of_permissible_images, holdable).
narrative_ontology:cs_axiom_grounding('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', regulation_prevents_abuse_of_permissible_images, instrumental).
narrative_ontology:cs_reference_frame('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', balanced_idolatry_prevention_framework).
narrative_ontology:cs_drift_state('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', contemporary_visual_culture_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('1e03cf9c-ae3c-49a6-a051-925bbb6d18a0', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, religious_authorities).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, moderate_iconoclast_scholars).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, artists_and_craftsmen).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, congregants_seeking_visual_aids).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the prohibition, issuing detailed regulations for permissible two-dimensional images. They benefit from maintaining gatekeeping power over religious art and visual culture, ensuring compliance with their specific theological interpretation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Their academic and theological careers are built on elaborating and defending this nuanced interpretation. They gain status and influence by providing the intellectual framework for the regulatory regime, reinforcing their identity within the tradition.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, moderate_iconoclast_scholars, beneficiary,
    organized, generational, identity_locked, regional).

% Bear the costs of strict regulation, needing to navigate complex rules for materials, style, and subject matter. They are prevented from creating three-dimensional religious art and face constant scrutiny for two-dimensional works, limiting their creative expression and market.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, artists_and_craftsmen, payer,
    powerless, biographical, constrained, local).

% Desire visual representations to aid worship and understanding, but are restricted to two-dimensional forms under strictures. Their spiritual practices are shaped by the constraint, and they bear the cost of limited access to diverse forms of religious expression, often internalizing the prohibition as a virtue.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, congregants_seeking_visual_aids, payer,
    moderate, biographical, identity_locked, local).

% Would argue that even two-dimensional images carry idolatry risk and should be forbidden. They are marginalized by this moderate reading, which they see as a compromise that dilutes the true spirit of the prohibition.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_purists, excluded,
    organized, generational, constrained, regional).

% Would argue for a broader acceptance of religious imagery, including statuary, based on theological principles of material mediation. They are excluded from the interpretive framework of this reading, which views their practices as dangerously close to idolatry.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_advocates, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate religious practice around a specific interpretation of divine law, balancing the perceived risk of idolatry with the desire for some visual representation in worship, thereby maintaining a unified theological stance.
% TRANSFER_FUNCTION: Transfers interpretive authority and control over religious visual culture to the religious authorities and scholars who define and enforce the strict regulations, from artists and congregants who bear the costs of compliance and restricted expression.
% ABSENT_VOICES: Both stricter iconoclast purists and more permissive iconodule advocates are excluded from the conversation, as their positions are deemed either too extreme or too dangerous by the moderate iconoclast framework. They would argue for their respective interpretations, challenging the legitimacy of the compromise.
% DISAPPEARANCE_RATIONALE: If this specific prohibition and its enforcement vanished, religious art would diversify rapidly, including three-dimensional forms. The authority of the religious scholars and institutions built around this interpretation would diminish, and congregational practices would evolve to incorporate broader visual elements, leading to a significant rearrangement of religious visual culture and authority structures.
% FOUNDING_PROBLEM: The original problem was the perceived risk of idolatry associated with material representations of the divine, particularly in cultures where idol worship was prevalent.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities and moderate iconoclast scholars attest that the risk of idolatry remains live, requiring ongoing vigilance. However, artists and some congregants might argue that the problem is overblown in contemporary contexts, and the regulations are more about control than genuine risk mitigation. No external, non-beneficiary corroboration exists for the 'live' status beyond the interpretive community itself.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) due to the significant compliance costs and restrictions placed on artists and congregants, disproportionate to the actual risk of idolatry in modern contexts. Suppression is also high (0.75) as the constraint relies on active enforcement and constant monitoring to prevent 'abuse' of two-dimensional images and to exclude three-dimensional forms. The theater ratio (0.40) reflects that a substantial portion of the regulatory activity is about maintaining interpretive authority and control, rather than solely preventing genuine idolatry. Accessibility collapse is moderate (0.60) as two-dimensional images are permitted, but under such strictures that alternatives for visual expression are severely limited. Resistance is moderate (0.55) as artists and some congregants express discontent, but direct challenge is difficult due to the institutional power of religious authorities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious authorities and scholars, this is a necessary and balanced interpretation that safeguards against idolatry while allowing for some visual expression. From the perspective of artists and congregants, it is an overly restrictive and burdensome system that stifles creativity and spiritual practice. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities and moderate iconoclast scholars are beneficiaries (d near 0.0) as they gain interpretive authority, control, and academic standing. Artists and craftsmen, along with congregants seeking visual aids, are targets (d near 1.0) as they bear the costs of restricted expression and compliance. Iconoclast purists and iconodule advocates are excluded, their positions structurally foreclosed or marginalized by this moderate reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    idolatry_risk_assessment,
    'What is the actual, empirically verifiable risk of idolatry associated with two-dimensional images versus three-dimensional statuary in contemporary contexts?',
    'Sociological and psychological studies on religious practice and image veneration in diverse cultural settings, independent of theological interpretation.',
    'If the empirical risk is negligible for both, the constraint''s justification for suppression and extraction weakens significantly, potentially reclassifying it as a Piton or even a Snare with a weaker coordination cover. If a differential risk is confirmed, the moderate iconoclast position gains empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(idolatry_risk_assessment, empirical, 'Empirical basis for differential idolatry risk between 2D and 3D images.').

omega_variable(
    interpretive_authority_legitimacy,
    'Is the interpretive authority of the religious institutions grounded in genuine theological consensus or in the institutional power to enforce a specific reading?',
    'Historical analysis of interpretive shifts, comparative theology across different traditions, and internal dissent within the tradition regarding the legitimacy of the moderate iconoclast position.',
    'If grounded primarily in institutional power, the constraint''s extractiveness is more clearly revealed as rent-seeking for interpretive control. If genuine consensus, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'Grounding of interpretive authority: consensus vs. institutional power.').

omega_variable(
    identity_lock_strength_for_congregants,
    'How deeply is the identity of congregants fused with the moderate iconoclast reading, such that exit or resistance is unthinkable?',
    'Qualitative sociological research on congregant experiences, narratives of those who have left the tradition, and analysis of internal discourse regarding visual culture.',
    'If identity-lock is very strong, the effective suppression and extractiveness for congregants are higher than structural measures suggest, as they carry the constraint internally. If weaker, resistance potential is higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_strength_for_congregants, empirical, 'Degree of identity fusion for congregants with the moderate iconoclast reading.').


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
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Decalogue's image prohibition kernel. It represents the moderate iconoclast position, which permits two-dimensional images under strict regulation while forbidding three-dimensional statuary. It influences both the stricter iconoclast and more permissive iconodule readings by defining a contested middle ground.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
