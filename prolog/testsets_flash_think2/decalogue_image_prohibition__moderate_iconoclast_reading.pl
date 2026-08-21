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
 *   human_readable: Decalogue Image Prohibition (Moderate Iconoclast Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint represents the 'moderate iconoclast' reading of the
 *   Decalogue's prohibition on images, which forbids three-dimensional
 *   statuary due to higher idolatry risk but permits two-dimensional images
 *   under strict regulation to prevent abuse. This reading attempts to
 *   navigate between radical iconoclasm and iconodulism, but in doing so, it
 *   creates a Snare-type constraint that extracts compliance costs through
 *   detailed regulation and monitoring, benefiting the regulatory authority
 *   by maintaining gatekeeping power. This story is one reading of the
 *   'decalogue_image_prohibition' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.75).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.8).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Decalogue Image Prohibition (Moderate Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, 'bafa33f5-4903-44d8-9c3b-76db257bdaf9').
narrative_ontology:cs_kernel_codification('bafa33f5-4903-44d8-9c3b-76db257bdaf9', fixed_text).
narrative_ontology:cs_authority_grounding('bafa33f5-4903-44d8-9c3b-76db257bdaf9', lineage).
narrative_ontology:cs_interpretation_layer_present('bafa33f5-4903-44d8-9c3b-76db257bdaf9').
narrative_ontology:cs_reading_relation('bafa33f5-4903-44d8-9c3b-76db257bdaf9', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('bafa33f5-4903-44d8-9c3b-76db257bdaf9', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('bafa33f5-4903-44d8-9c3b-76db257bdaf9', foundational, idolatry_risk_mitigation).
narrative_ontology:cs_axiom_status(idolatry_risk_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('bafa33f5-4903-44d8-9c3b-76db257bdaf9', idolatry_risk_mitigation, deontological).
narrative_ontology:cs_axiom('bafa33f5-4903-44d8-9c3b-76db257bdaf9', foundational, dimensional_idolatry_gradient).
narrative_ontology:cs_axiom_status(dimensional_idolatry_gradient, holdable).
narrative_ontology:cs_axiom_grounding('bafa33f5-4903-44d8-9c3b-76db257bdaf9', dimensional_idolatry_gradient, conventional).
narrative_ontology:cs_reference_frame('bafa33f5-4903-44d8-9c3b-76db257bdaf9', controlled_material_mediation).
narrative_ontology:cs_drift_state('bafa33f5-4903-44d8-9c3b-76db257bdaf9', contemporary_globalized_culture, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('bafa33f5-4903-44d8-9c3b-76db257bdaf9', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, religious_authorities).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, theological_interpreters).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, artists_and_artisans).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, devotees_seeking_visual_expression).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, congregations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, interpret, and enforce the prohibition, maintaining theological purity and their own gatekeeping authority over religious art and expression. They benefit from the compliance and the power to mediate divine representation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Their intellectual work is essential for defining the nuanced boundaries of permissible imagery, reinforcing their role and influence within the religious structure. They benefit from the complexity and the need for their expertise.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, theological_interpreters, beneficiary,
    organized, generational, constrained, global).

% Must conform their creative practice to strict theological and regulatory guidelines, limiting their artistic freedom and potentially their livelihood. They bear the direct cost of compliance.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, artists_and_artisans, payer,
    moderate, biographical, constrained, local).

% Desire to express their faith through visual art and engage with sacred imagery, but are limited to approved two-dimensional forms, bearing the cost of restricted spiritual and aesthetic experience. Their identity is often tied to their religious community, making exit difficult.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, devotees_seeking_visual_expression, payer,
    powerless, biographical, identity_locked, local).

% Bear the financial and cultural costs of commissioning and maintaining approved imagery, and are denied the spiritual or aesthetic experiences that 3D statuary might offer.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, congregations, payer,
    organized, generational, constrained, local).

% Adhere to a stricter interpretation forbidding all religious imagery, viewing even regulated 2D images as a compromise with idolatry. Their perspective is marginalized by this moderate reading.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, radical_iconoclasts, excluded,
    organized, generational, constrained, global).

% Advocate for the veneration of images, including 3D statuary, as legitimate conduits to the divine, often citing theological arguments about the Incarnation. Their views are explicitly rejected by this reading.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for visual religious expression that attempts to balance the risk of idolatry with the desire for material mediation of the divine, preventing theological anarchy while allowing some artistic representation.
% TRANSFER_FUNCTION: Transfers interpretive and regulatory power over religious art from individual artists and communities to religious authorities and theological interpreters, along with the associated compliance costs and restrictions on creative freedom.
% ABSENT_VOICES: Radical iconoclasts, who would forbid all images, and iconodule advocates, who would permit and venerate 3D statuary, are both excluded from this moderate framing, which attempts to find a middle ground that satisfies neither extreme.
% DISAPPEARANCE_RATIONALE: If the prohibition and its regulations vanished, there would be a rapid proliferation of diverse religious imagery, including 3D statuary, leading to significant theological debate and shifts in religious practice and authority structures as communities explore new forms of expression.
% FOUNDING_PROBLEM: The historical problem of idolatry, where material representations of the divine became objects of worship themselves, diverting devotion from God, and the need to provide guidance on permissible forms of religious art.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and historical accounts from various theological traditions corroborate the historical problem of idolatry. The ongoing need for theological guidance on religious art is attested by religious scholars and community leaders, not just the enforcing authorities, though the specific interpretation remains contested.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.75) due to the significant compliance costs, bureaucratic overhead, and restrictions on artistic and spiritual expression imposed by the strict regulations. Suppression is also high (0.80) because the prohibition on 3D statuary is absolute, and the permission for 2D images is heavily controlled, requiring active enforcement and theological pressure to maintain. The theater ratio is moderate (0.40): while there is a genuine theological concern about idolatry, a substantial portion of the regulatory activity serves to reinforce the authority and gatekeeping power of the interpreters. The measurement series show a gradual increase in extractiveness and suppression over time, reflecting the entrenchment of regulatory power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious authorities and theological interpreters, this constraint is a necessary coordination mechanism to safeguard against idolatry and maintain theological purity. However, from the perspective of artists, devotees, and congregations, it operates as an extractive mechanism, imposing significant costs and limiting spiritual expression, while reinforcing the power of the agenda-setters.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities and theological interpreters are the primary beneficiaries, as they define and enforce the rules, gaining interpretive power and maintaining control over religious expression. Artists, devotees, and congregations are the victims, bearing the costs of compliance, restricted creative freedom, and limited spiritual engagement with imagery. Radical iconoclasts and iconodule advocates are excluded, as their positions fall outside the boundaries of this moderate interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint attempts to resolve the historical tension surrounding religious imagery by splitting the difference, but in doing so, it creates a new set of problems. The original mandate to prevent idolatry is still live, but the specific regulatory framework has accumulated extractive elements, imposing bureaucratic overhead and compliance costs that may not be directly proportional to the actual risk of idolatry, indicating a potential for mandatrophy where the means (regulation) become an end (power maintenance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    idolatry_risk_assessment,
    'Is the risk of idolatry from three-dimensional statuary genuinely higher than from two-dimensional images, or is this a theological distinction used to justify control and gatekeeping?',
    'Comparative theological studies across traditions with different image policies, and sociological analysis of how different forms of imagery are actually engaged with by adherents.',
    'If the dimensional distinction is found to be arbitrary or primarily a tool for control, the constraint''s justification weakens, and its extractiveness would be reclassified as less legitimate. If the distinction is robust, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(idolatry_risk_assessment, conceptual, 'Whether the dimensional distinction in idolatry risk is theological or instrumental.').

omega_variable(
    regulatory_burden_efficacy,
    'Is the strict regulation of two-dimensional images genuinely preventing abuse and maintaining theological purity, or is it primarily a mechanism for maintaining authority and extracting compliance?',
    'Empirical study of the actual incidence of ''abuse'' (e.g., idolatrous practices) in contexts with varying levels of regulation, compared to the administrative burden on artists and communities.',
    'If the regulatory burden is disproportionate to the actual prevention of abuse, the constraint''s extractiveness is confirmed as excessive, and its classification as a Snare is reinforced. If the regulation is highly effective, its coordination function is more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_burden_efficacy, empirical, 'Efficacy of regulation vs. its burden and gatekeeping function.').

omega_variable(
    spiritual_expression_cost,
    'What is the spiritual and cultural cost to devotees and communities of being denied certain forms of visual expression, particularly three-dimensional statuary?',
    'Qualitative sociological and theological research exploring the experiences of adherents in traditions with and without such prohibitions, focusing on spiritual fulfillment, communal identity, and artistic flourishing.',
    'If the spiritual cost is found to be significant, it highlights a hidden extraction from the victims, reinforcing the Snare classification by revealing a deeper impact beyond financial compliance costs. If the cost is negligible, the constraint''s impact on victims is less severe.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spiritual_expression_cost, preference, 'The unmeasured spiritual cost of restricted visual expression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(deca_tr_t80, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 80, 0.39).
narrative_ontology:measurement(deca_tr_t100, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(deca_be_t80, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 80, 0.74).
narrative_ontology:measurement(deca_be_t100, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 100, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(deca_su_t60, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 60, 0.77).
narrative_ontology:measurement(deca_su_t80, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 80, 0.79).
narrative_ontology:measurement(deca_su_t100, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'decalogue_image_prohibition' kernel, each representing a distinct theological interpretation with different structural properties and classifications. This reading attempts a middle ground between radical iconoclasm and iconodulism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
