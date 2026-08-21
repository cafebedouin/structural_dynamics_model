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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Decalogue Image Prohibition (Moderate Iconoclast Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint represents the 'moderate iconoclast' reading of the
 *   Decalogue's image prohibition, a specific interpretation within the
 *   broader 'decalogue_image_prohibition' kernel. This reading forbids
 *   three-dimensional statuary due to a perceived higher idolatry risk but
 *   permits two-dimensional images under strict regulation and monitoring to
 *   prevent abuse. It attempts to strike a balance between radical iconoclasm
 *   (forbidding all images) and iconodulism (permitting extensive image use,
 *   including veneration). The constraint is claimed as a Rope by its
 *   proponents, but its operational metrics, as authored here, suggest it
 *   functions as a Snare, extracting compliance costs through detailed
 *   regulation and monitoring while maintaining gatekeeping power for
 *   religious authorities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.7).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.8).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Decalogue Image Prohibition (Moderate Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, 'e9d9f279-3e66-45f4-9460-c4b6725e12b9').
narrative_ontology:cs_kernel_codification('e9d9f279-3e66-45f4-9460-c4b6725e12b9', fixed_text).
narrative_ontology:cs_authority_grounding('e9d9f279-3e66-45f4-9460-c4b6725e12b9', lineage).
narrative_ontology:cs_interpretation_layer_present('e9d9f279-3e66-45f4-9460-c4b6725e12b9').
narrative_ontology:cs_reading_relation('e9d9f279-3e66-45f4-9460-c4b6725e12b9', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9d9f279-3e66-45f4-9460-c4b6725e12b9', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('e9d9f279-3e66-45f4-9460-c4b6725e12b9', foundational, divine_transcendence_requires_limited_representation).
narrative_ontology:cs_axiom_status(divine_transcendence_requires_limited_representation, holdable).
narrative_ontology:cs_axiom_grounding('e9d9f279-3e66-45f4-9460-c4b6725e12b9', divine_transcendence_requires_limited_representation, theological).
narrative_ontology:cs_reference_frame('e9d9f279-3e66-45f4-9460-c4b6725e12b9', balanced_idolatry_prevention_and_pedagogy).
narrative_ontology:cs_drift_state('e9d9f279-3e66-45f4-9460-c4b6725e12b9', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e9d9f279-3e66-45f4-9460-c4b6725e12b9', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, religious_authorities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, religious_artists).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, congregants_seeking_visual_aids).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, divine_transcendence).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, anti_idolatry_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the prohibition, setting strict regulations for two-dimensional images and forbidding three-dimensional statuary. They gain legitimacy and control over religious expression by mediating between the divine command and congregational practice, ensuring theological coherence as they define it.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Must adhere to strict guidelines for two-dimensional images, limiting their creative expression and incurring compliance costs for approval. They are forbidden from creating three-dimensional religious art, which might otherwise be a significant form of expression and livelihood, forcing them to adapt or leave the religious art domain.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_artists, payer,
    moderate, biographical, constrained, local).

% Benefit from approved two-dimensional images for devotional and educational purposes, but are denied the potentially more immersive experience of three-dimensional statuary. They bear the indirect cost of limited artistic expression and potentially higher prices for regulated art, and their devotional practices are shaped by the available forms.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, congregants_seeking_visual_aids, payer,
    powerless, biographical, constrained, local).

% Believe all religious imagery, including two-dimensional, is idolatrous and forbidden. They are excluded from the interpretive framework of this moderate reading and view its permissions as a dangerous compromise that undermines the divine command, often forming dissenting groups.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, radical_iconoclasts, excluded,
    organized, biographical, identity_locked, regional).

% Believe that material images, including three-dimensional statuary, can be venerated as conduits to the divine, especially in light of theological doctrines like the Incarnation. They are excluded from this reading's strictures and view its prohibitions as limiting legitimate religious expression and devotional practice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodules, excluded,
    organized, biographical, identity_locked, regional).

% Analyze the historical, theological, and practical implications of the image prohibition and its various interpretations. They provide academic commentary and critique but do not directly enforce or benefit from the constraint's operation, maintaining an external analytical stance.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, theological_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for visual religious expression that mitigates the risk of idolatry while allowing for pedagogical and devotional aids, thereby maintaining communal theological coherence and preventing schism over image use.
% TRANSFER_FUNCTION: Transfers creative freedom and potential devotional depth from artists and congregants to the religious authorities, who gain enhanced control over religious practice and a solidified interpretive mandate. It also transfers compliance costs (time, resources for regulation and monitoring) to the community, which are then absorbed by the authorities as part of their gatekeeping function.
% ABSENT_VOICES: Both radical iconoclasts (who would forbid all images as idolatrous) and iconodules (who would permit more extensive use of images, including three-dimensional statuary, for veneration) are structurally excluded from the framing of this specific moderate compromise. Their perspectives represent the extremes this reading attempts to navigate but ultimately suppresses in its pursuit of a middle ground.
% DISAPPEARANCE_RATIONALE: If the prohibition and its regulations vanished, the religious community would immediately face a vacuum in how to approach visual culture. It would likely lead to a rapid proliferation of diverse imagery, including three-dimensional forms, and a re-evaluation of theological principles, potentially causing schisms or a rapid shift towards either more radical iconoclasm or full iconodulism, fundamentally reorganizing religious practice.
% FOUNDING_PROBLEM: The historical and theological problem of idolatry, specifically the risk that material representations of the divine could become objects of worship themselves, diverting devotion from God and corrupting true worship.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities and many congregants attest that the risk of idolatry remains live, citing historical precedents and ongoing human tendencies towards material worship. Theological scholars, while acknowledging the historical problem, often contest the specific application and efficacy of the current regulatory framework, suggesting the problem's *status* is contested in its contemporary manifestation rather than universally accepted as 'live' in the same way.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.7) stems from the significant compliance costs imposed on artists and the limitation of devotional expression for congregants, without a clear proportional benefit from the regulatory overhead. Suppression (0.8) is high due to the active enforcement mechanisms (monitoring, approval processes) and the structural exclusion of alternative forms of expression (3D statuary, unregulated 2D images). The theater ratio (0.4) reflects a genuine concern for preventing idolatry, but also a substantial component of performative regulation that primarily reinforces the authority's control rather than addressing a live, widespread threat of abuse from 2D images. Accessibility collapse (0.6) is moderate because 2D images are permitted, but 3D forms are completely inaccessible. Resistance (0.5) is present from artists pushing boundaries and from excluded factions, but not strong enough to overturn the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious authorities (agenda_setter), this constraint is a necessary and balanced Rope, coordinating appropriate religious expression and preventing idolatry. From the perspective of religious artists and congregants (payers), it is a Snare that imposes significant costs and limits creative/devotional freedom under the guise of protection. The engine's computation of per-seat classifications will highlight this divergence, showing the authorities as beneficiaries and artists/congregants as targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities are the primary beneficiaries (low d) as they gain legitimacy, control over religious expression, and a solidified interpretive mandate. Religious artists and congregants are the primary targets (high d) as they bear the direct and indirect costs of compliance, limited creative freedom, and restricted devotional practices. Radical iconoclasts and iconodules are excluded, meaning the constraint's enforcement actively suppresses their alternative views, placing them at the extreme target end of the spectrum for this specific reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine moderate iconoclast reading of the Decalogue image prohibition, or does it lean more towards a stricter iconoclast position in practice?',
    'Detailed ethnographic study of actual religious practice and enforcement patterns, comparing stated policy with lived experience and the frequency of exceptions or challenges.',
    'If it leans stricter, its effective suppression and extractiveness are higher, potentially reclassifying it closer to a pure Snare. If it is more permissive in practice, it might shift towards a Tangled Rope or even a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Ambiguity in the practical application of the moderate iconoclast reading.').

omega_variable(
    idolatry_risk_assessment,
    'Is three-dimensional statuary inherently a higher idolatry risk than two-dimensional images, or is this distinction a conventional theological construct?',
    'Comparative theological analysis across traditions with different image policies, and psychological studies on the human perception of sacred objects and their potential for idolatry.',
    'If the distinction is primarily conventional, the prohibition on 3D statuary is a more arbitrary extraction, increasing the constraint''s Snare-like qualities. If the risk is empirically demonstrable, it strengthens the coordination function of the prohibition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(idolatry_risk_assessment, conceptual, 'Whether the distinction between 2D and 3D images for idolatry risk is structural or conventional.').

omega_variable(
    regulatory_burden_efficacy,
    'Is the strict regulation of two-dimensional images genuinely effective in preventing abuse and idolatry, or does it primarily serve as bureaucratic overhead that extracts compliance costs?',
    'Analysis of historical cases of abuse/idolatry with 2D images under different regulatory regimes, and a cost-benefit analysis of the current monitoring and approval processes.',
    'If the regulation is largely ineffective bureaucratic overhead, the theater_ratio and extractiveness are higher than currently estimated, reinforcing the Snare classification. If it is highly effective, it strengthens the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_burden_efficacy, empirical, 'Efficacy of strict regulation on 2D images in preventing abuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'decalogue_image_prohibition' kernel. Each reading has a unique structural profile and ε value, reflecting different theological interpretations and their practical consequences. This 'moderate_iconoclast_reading' attempts a compromise, leading to a Snare-like structure of regulated permission.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
