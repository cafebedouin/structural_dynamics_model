% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Distinction: Latria vs. Dulia (Image Veneration Coordination)
 *   domain: theological/religious/visual_culture
 *
 * SUMMARY:
 *   The iconodule reading interprets the Decalogue's image prohibition
 *   through the lens of incarnational theology: the Incarnation sanctified
 *   matter as a valid conduit to the divine, permitting visual honor (dulia)
 *   directed toward prototypes (God, saints) while preserving the prohibition
 *   on worship (latria) of the images themselves. This is ONE reading of a
 *   contested kernel (the Decalogue + its theological relationship to
 *   incarnationalism). The iconoclast reading rejects this distinction as a
 *   rationalization of idolatry. The moderate reading permits imagery under
 *   spatial or regulatory restriction. This story instantiates only the
 *   iconodule reading and treats its structural properties as a Rope-type
 *   constraint: it solves a genuine theological coordination problem (How to
 *   reconcile incarnationalism with the commandment?) and enables
 *   participation through icons, while requiring active enforcement to
 *   suppress competing interpretations and maintain the latria/dulia
 *   boundary.
 *
 * KEY AGENTS:
 *   - orthodox_clergy: institutional authority, sets and enforces the latria/dulia distinction, identity-locked in the interpretive role
 *   - icon_artisans: organized beneficiary, economic and social status from sacred art legitimacy, constrained exit
 *   - liturgical_practitioners: beneficiary and payer, participate in icon veneration under clergy definition, identity-locked in practice
 *   - laity_seeking_visual_mediation: beneficiary, access to visual pathway to the divine, constrained by clergy boundaries
 *   - iconoclast_reformers: structurally excluded, hold position that all religious imagery violates the commandment, would eradicate icons if in power
 *   - moderate_image_regulators: structurally excluded, propose alternative (dimensional or regulatory) restrictions, lack institutional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.38).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.72).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Distinction: Latria vs. Dulia (Image Veneration Coordination)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theological/religious/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '7aadd9ee-6cea-4233-aa46-1baec01f1fe2').
narrative_ontology:cs_kernel_codification('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', fixed_text).
narrative_ontology:cs_authority_grounding('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', lineage).
narrative_ontology:cs_interpretation_layer_present('7aadd9ee-6cea-4233-aa46-1baec01f1fe2').
narrative_ontology:cs_reading_relation('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', incarnation_sanctifies_matter, deontological).
narrative_ontology:cs_axiom('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', foundational, dulia_distinction_logically_coherent).
narrative_ontology:cs_axiom_status(dulia_distinction_logically_coherent, holdable).
narrative_ontology:cs_axiom_grounding('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', dulia_distinction_logically_coherent, deontological).
narrative_ontology:cs_reference_frame('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', incarnational_theology_permits_material_mediation).
narrative_ontology:cs_drift_state('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', contemporary_pluralistic_christendom, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7aadd9ee-6cea-4233-aa46-1baec01f1fe2', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, orthodox_clergy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_artisans).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, liturgical_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38 at interval end) because the constraint's primary function is genuine coordination—solving the incarnational/commandment tension—rather than pure extraction. The clergy benefit from interpretive authority, and artisans benefit from legitimacy, but these are incidental to the coordination function, not its raison d'être. Suppression is substantially higher (0.72) because the constraint persists by actively suppressing competing interpretations (iconoclasm, moderate restrictions) and by identity-locking participants (clergy and practitioners) to prevent exit. Theater is low-moderate (0.28): the theological distinction is intellectually substantive, not merely performative, but enforcement involves some rhetorical work to maintain boundaries against historical challenge. The measurement series show suppression rising over the interval (as enforcement infrastructure hardened during periods of ecumenical contestation) while extraction stays relatively flat (the coordination function stabilizes early and does not intensify). Accessibility collapse is moderate (0.65): the distinction itself is not empirically falsifiable—it rests on theological premises (incarnationalism is true, matter is sanctifiable) that alternatives reject—so once understood, the latria/dulia framework becomes difficult to exit without abandoning the entire theological edifice. Resistance is substantial (0.58): iconoclasts and moderate regulators mount real theological and historical objections; the constraint's persistence depends on active enforcement and institutional authority, not on universal conviction.
 *
 * PERSPECTIVAL GAP:
 *   The clergy seat experiences the constraint as genuine coordination—the theological solution to a real problem. The artisan and practitioner seats experience it as legitimate framework enabling their practice. The iconoclast excluded seat experiences it as rationalization of idolatry. The theological analyst seat sees the structure clearly: the distinction solves a coordination problem but also benefits the clergy's interpretive monopoly, which suppresses competing frameworks. The engine should compute the clergy seat as lower-extraction (closer to rope) and the excluded seats as higher extraction (from their perspective, this is snare—suppressed alternatives). The claim/metric independence rule applies: the story claims Rope (coordination-primary) while authoring metrics describing active enforcement against competing readings; the gap is exactly what the measurement should register.
 *
 * DIRECTIONALITY LOGIC:
 *   Orthodox clergy are the structural agenda-setters: they set the doctrinal boundary, teach it, enforce it through sacramental authority and theological instruction. They benefit from the interpretive monopoly. Icon artisans are beneficiaries: their craft is legitimized as theologically necessary. Liturgical practitioners are beneficiaries (they can venerate) and pay (they must submit to clergy definition and risk censure if boundaries blur). Laity are beneficiaries (visual pathway to divine) and constrained. Iconoclasts and moderates are excluded—structurally locked out by the distinction itself, which marks their positions as heretical. The directionality of the clergy seat should be low-beneficiary (d near 0.0–0.2): they interpret and enforce, collecting institutional authority. The excluded seats should be high-target (d near 0.8–1.0): they are suppressed by the constraint and cannot practice their reading. Practitioners sit symmetric (d near 0.5): genuine coordination benefit, but submission to authority. No directionality override is required; the derivation from beneficiary/victim + exit (identity_locked for clergy and practitioners) should produce the right d vector.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling commandment + incarnationalism) remains contested, not dead. The iconodule solution—the latria/dulia distinction—is live doctrine in orthodox traditions but rejected by iconoclasts and questioned by moderates. This is not a Piton (atrophied function, mostly performance). The constraint persists because the founding problem is still contested and the iconodule solution is institutionally backed. If the founding problem became universally settled (all Christendom accepted iconoclasm OR all accepted unrestricted veneration), the constraint might atrophy into theater. But the historical record shows the contest is genuinely persistent across centuries: the constraint is not dead function wrapped in performance, but contested function wrapped in enforcement. The measuring line is whether suppression_requirement tracks or exceeds extraction—if suppression is substantially higher, active enforcement is carrying a function that participants no longer find coordinate. Here, suppression (0.72) exceeds extraction (0.38) by a margin (0.34), which signals that the constraint is maintained against countervailing force, not passively affirmed. This is consistent with a Rope under real contestation, not a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_boundary_empirical,
    'Is the latria/dulia distinction empirically distinguishable in actual practice, or does it remain a theoretical boundary that collapses under enforcement scrutiny?',
    'Historical and ethnographic study of icon veneration: examine actual practice (posture, speech, offerings, emotional engagement) to determine whether practitioners reliably and consistently distinguish worship from honor, or whether the boundary is porous and maintained only through continued instruction and enforcement.',
    'If the boundary is reliably practiced, the constraint is genuine coordination with some suppression overhead. If the boundary is porous and requires continuous enforcement to maintain, the constraint is more extractive than the coordination story suggests—theater_ratio should rise and extractiveness might increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_boundary_empirical, empirical, 'Whether the latria/dulia distinction maps to distinguishable practice or is maintained only through doctrine.').

omega_variable(
    incarnational_necessity_alternative_forms,
    'Does the coordination problem (How to make incarnationalism compatible with the commandment?) require the latria/dulia distinction specifically, or can other theological frameworks (e.g., a pneumatological rather than incarnational emphasis, or a purely abstract meditation practice) solve the same problem without imaging?',
    'Comparative theological analysis and historical study of non-iconodule traditions (e.g., Protestants, Reformed theologians) that accept incarnationalism but restrict or prohibit images. Do they face the same founding problem? How do they resolve it? If they achieve the same theological goals without the distinction, the distinction''s necessity is questioned.',
    'If the problem has alternative solutions, the latria/dulia reading is ONE option, not the ONLY option, and its persistence becomes more obviously a matter of institutional authority than logical necessity. This would classify the constraint as more extractive (extracting institutional authority) rather than pure coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incarnational_necessity_alternative_forms, conceptual, 'Whether the latria/dulia distinction is structurally necessary or one of multiple possible solutions to the founding problem.').

omega_variable(
    iconoclasm_suppression_mechanism_structural_vs_internalized,
    'Is the suppression of iconoclast and moderate readings maintained by structural barriers (institutional authority, legal prohibition, access control) or by internalized commitment (training, identity fusion) or both?',
    'Historical study of periods of iconoclasm and restoration: measure enforcement intensity (how violently the constraint is maintained), exit rates when enforcement weakens, and whether suppression persists after the structural enforcement mechanism is removed. If practitioners continue to adopt the iconodule reading after enforcement disappears, suppression is partly internalized.',
    'If suppression is purely structural, removing the institutional enforcement (e.g., disestablishing the Orthodox Church) would likely collapse the constraint. If suppression is internalized (clergy and practitioners have fused their identity with the doctrine), the constraint persists even after external enforcement weakens. This distinguishes temporary suppression (Scaffold) from durable suppression (Rope or Snare). High internalization would support the Rope classification; absence of internalization would suggest the constraint is maintained artificially and might be Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iconoclasm_suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of competing readings is structural or internalized (identity-fused).').

omega_variable(
    kernel_reading_committer_uncertainty,
    'Is this interpretation (iconodule_reading) the reading the historical record endorses as authoritative, or is it one competing reading among others? Did the Seventh Ecumenical Council (787 CE) settle the matter or merely articulate the iconodule position in an ongoing contest?',
    'Historical theology: examine the actual text of the Council of Nicaea II, the contestation that followed, and whether ''settlement'' means universal acceptance or institutional authority. Did iconoclasm persist as a live threat? Did it return? Is there surviving intellectual tradition outside the council''s ruling that rejected the distinction?',
    'If the council settled the matter universally (all Christian traditions accepted the distinction), the founding problem is solved and the constraint is pure coordination. If the council settled it institutionally (established Orthodoxy adopted it, but other traditions rejected it), the constraint carries extractive suppression. The latter is more historically accurate, which would raise the classification''s extractiveness relative to the coordination story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_uncertainty, empirical, 'Whether the iconodule reading is a settled doctrine or an institutionally-enforced reading in ongoing contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(deca_tr_t5, decalogue_image_prohibition__iconodule_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__iconodule_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconodule_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__iconodule_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconodule_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(deca_be_t5, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(deca_su_t5, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconodule_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% The Decalogue's image prohibition is instantiated as three separate constraints, each a distinct reading of the contested kernel. The iconodule reading (this story) treats images as permissible under the latria/dulia distinction and claims Rope (genuine coordination enabling visual mediation of the divine). The iconoclast reading treats all religious images as idolatrous and claims Snare (suppressed alternatives, no genuine coordination). The moderate reading treats 3D as higher risk and 2D as regulated and claims Rope (coordination with tighter boundaries). These are NOT the same constraint viewed differently; they have different ε values (iconodule ~0.38, iconoclast lower, moderate higher), different victim/beneficiary sets, different types. Each instantiates a different functional structure from the same kernel. All three are linked via affects_constraints; reading-level contestation is routed through omega variables and cs_structure fields per the Kernels and Readings rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
