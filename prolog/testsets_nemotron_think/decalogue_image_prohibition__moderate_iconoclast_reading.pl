% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Moderate Iconoclast Image Prohibition (3D forbidden, 2D regulated)
 *   domain: theological/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint story captures the moderate iconoclast reading of the
 *   Decalogue's image prohibition: three-dimensional statuary is forbidden as
 *   carrying higher idolatry risk, while two-dimensional images are permitted
 *   but subjected to strict regulation (approval processes, veneration
 *   protocols, placement rules). The regulatory authority uses this
 *   compromise to maintain gatekeeping power over visual culture, extracting
 *   compliance costs from lay practitioners and local clergy. The constraint
 *   is actively enforced through inspections, licensing, and penalties. The
 *   claimed type is snare: the coordination story (preventing idolatry) is
 *   cover for a structure that primarily serves the authority's extractive
 *   interest.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.72).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.78).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Moderate Iconoclast Image Prohibition (3D forbidden, 2D regulated)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theological/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '7f19ca25-b092-43f6-8386-ca0b70f14ed0').
narrative_ontology:cs_kernel_codification('7f19ca25-b092-43f6-8386-ca0b70f14ed0', fixed_text).
narrative_ontology:cs_authority_grounding('7f19ca25-b092-43f6-8386-ca0b70f14ed0', extraction).
narrative_ontology:cs_interpretation_layer_present('7f19ca25-b092-43f6-8386-ca0b70f14ed0').
narrative_ontology:cs_reading_relation('7f19ca25-b092-43f6-8386-ca0b70f14ed0', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f19ca25-b092-43f6-8386-ca0b70f14ed0', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('7f19ca25-b092-43f6-8386-ca0b70f14ed0', foundational, three_dimensional_statuary_prohibited).
narrative_ontology:cs_axiom_status(three_dimensional_statuary_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('7f19ca25-b092-43f6-8386-ca0b70f14ed0', three_dimensional_statuary_prohibited, theological).
narrative_ontology:cs_axiom('7f19ca25-b092-43f6-8386-ca0b70f14ed0', foundational, two_dimensional_images_permitted_under_regulation).
narrative_ontology:cs_axiom_status(two_dimensional_images_permitted_under_regulation, holdable).
narrative_ontology:cs_axiom_grounding('7f19ca25-b092-43f6-8386-ca0b70f14ed0', two_dimensional_images_permitted_under_regulation, conventional).
narrative_ontology:cs_reference_frame('7f19ca25-b092-43f6-8386-ca0b70f14ed0', moderate_iconoclast_settlement).
narrative_ontology:cs_drift_state('7f19ca25-b092-43f6-8386-ca0b70f14ed0', contemporary_secular_literacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7f19ca25-b092-43f6-8386-ca0b70f14ed0', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, lay_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, local_clergy).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, second_commandment_interpretation).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, idolatry_risk_gradation_by_dimensionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues detailed regulations on permissible two-dimensional images (style, placement, veneration protocols), conducts inspections, and grants or revokes licenses for image production. Collects fees for approval processes and retains the power to define what constitutes 'abuse'. The authority's institutional position depends on maintaining this gatekeeping role.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, universal).

% Must navigate complex rules for permissible images in homes and parishes; face penalties for non-compliance (fines, exclusion from sacraments). Cannot easily change denominations due to social, familial, and geographic ties. Bear the cost of compliance (approved images, permitted practices) and the risk of accidental violation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, lay_practitioners, payer,
    powerless, biographical, trapped, local).

% Responsible for enforcing regulations at parish level; must vet all visual materials, monitor lay practices, and report to higher authority. Their ordinal standing depends on compliance. Some privately favor more images (iconodule leanings) or fewer (iconoclast leanings) but cannot act without risking censure. Exit means leaving the clergy — high vocational identity cost.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, local_clergy, payer,
    moderate, biographical, constrained, regional).

% Advocate for total prohibition of all religious images. Their position is structurally excluded by the moderate reading's compromise. They exist as a pressure group but have no seat at the regulatory table; their dissent is treated as schismatic.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_hardliners, excluded,
    organized, biographical, trapped, universal).

% Argue that the Incarnation sanctifies matter and that veneration of images (both 2D and 3D) is legitimate dulia. Their theology is officially condemned by the moderate reading's framework. They operate in parallel structures (e.g., Eastern Orthodox jurisdictions) but within this constraint's jurisdiction they are excluded.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_proponents, excluded,
    organized, biographical, trapped, universal).

% Analyze the historical development, textual basis, and comparative theology of the three readings. They do not bear compliance costs nor collect regulatory rents. Their work informs but does not determine the constraint's operation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, theological_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulates the use of religious images to prevent idolatry while allowing limited visual mediation for instruction and devotion in a historically illiterate populace.
% TRANSFER_FUNCTION: Moves compliance costs (time, resources, autonomy) from practitioners and local clergy to the central religious regulatory authority, which gains gatekeeping power over permissible visual culture.
% ABSENT_VOICES: Iconoclast hardliners who want total prohibition and iconodule proponents who want full veneration rights are excluded from the regulatory compromise; they would object to the middle ground but are not represented in the enforcement structure.
% DISAPPEARANCE_RATIONALE: Without the prohibition and regulation, either full iconoclasm or full iconodulia would likely prevail, restructuring religious visual culture and the authority that governs it.
% FOUNDING_PROBLEM: The need to navigate the Second Commandment's prohibition on graven images while acknowledging the pedagogical and devotional value of images in a largely illiterate populace.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from the Iconoclast Controversy (8th-9th centuries) and Reformation-era negotiations corroborate the founding problem; however, the regulatory authority's own narrative is the primary source for the compromise's necessity, with limited external corroboration.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is high (0.72) because the regulatory apparatus imposes substantial compliance costs (fees, time, restricted practice) while the authority retains discretionary power over what counts as 'abuse'. Suppression is high (0.78) because alternatives (total iconoclasm or full iconodulia) are actively suppressed — dissenting clergy are disciplined, unauthorized images are destroyed. Theater ratio (0.42) reflects that the pedagogical justification is real but increasingly performative; the regulatory detail exceeds what idolatry prevention requires. Accessibility collapse (0.62) is moderate: the constraint makes unsanctioned visual practice difficult but not impossible (private devotion persists). Resistance (0.55) is moderate: periodic reform movements and schisms occur but are contained.
 *
 * PERSPECTIVAL GAP:
 *   From the authority's seat, the constraint appears as necessary coordination (rope-like) — it solves the idolatry problem while preserving images. From the payer seats (lay, clergy), the same structure operates as enforced extraction (snare) — the regulatory detail serves control, not prevention. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious_regulatory_authority is the structural beneficiary (collects fees, controls the visual economy, d near 0). Lay_practitioners are full targets (trapped, powerless, bear compliance costs, d near 1). Local_clergy are constrained targets (moderate power but identity-locked vocation, d ~0.7). Iconoclast_hardliners and iconodule_proponents are excluded — their exclusion is the enforcement object. Theological_scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (illiteracy + idolatry risk) is historically contingent. Literacy and theological development have reduced the pedagogical necessity, yet the regulatory structure persists and expands. This is mandatrophy: the mandate (prevent idolatry via regulated images) has outlived its function, but the constraint remains because the authority benefits from the gatekeeping. The classification as snare prevents mislabeling this as genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the classification of this constraint change if the kernel (Second Commandment) is read as iconoclast or iconodule instead of moderate?',
    'Generate separate constraint stories for each reading and compare their ε, beneficiary/victim structures, and computed types. The kernel_id decalogue_image_prohibition links them.',
    'If the iconoclast reading computes as mountain (natural law) and iconodule as rope (coordination), the moderate reading''s snare classification reveals it as a constructed compromise that extracts rents. If all three compute as snare, the kernel itself may be extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing under-determination: which reading is the ''true'' constraint?').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (canonical penalties, episcopal oversight) or internalized (lay conscience formation that makes unauthorized images feel sinful)?',
    'Compare suppression levels in jurisdictions where the regulatory authority has weakened (e.g., post-Reformation regions) versus where it remains strong. If suppression persists after structural removal, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in religious visual culture').

omega_variable(
    dimensionality_idolatry_risk,
    'Is there a genuine theological or psychological basis for the claim that 3D statuary carries higher idolatry risk than 2D images, or is the distinction a post-hoc rationalization for regulatory line-drawing?',
    'Comparative study of idolatry incidents across traditions with different dimensionality rules; cognitive science of representation and worship.',
    'If the distinction is baseless, the constraint''s coordination function is a cover story, strengthening snare classification. If genuine, part of the extraction may be the price of a real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dimensionality_idolatry_risk, conceptual, 'Naturalness of the 2D/3D distinction in idolatry risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(deca_tr_t80, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(deca_tr_t100, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(deca_be_t80, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(deca_be_t100, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(deca_su_t60, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(deca_su_t80, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 80, 0.77).
narrative_ontology:measurement(deca_su_t100, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__moderate_iconoclast_reading, 0.1).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the decalogue_image_prohibition kernel. The iconoclast_reading prohibits all religious imagery; the iconodule_reading permits veneration of images via latria/dulia distinction. This moderate reading splits the difference by dimensionality but imposes regulatory overhead on permissible practices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
