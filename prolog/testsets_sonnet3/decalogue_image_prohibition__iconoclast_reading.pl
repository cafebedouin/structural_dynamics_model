% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconoclast_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Iconoclast Reading: Categorical Prohibition on Religious Imagery
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This story instantiates the iconoclast reading of the Decalogue image
 *   prohibition kernel: the commandment against graven images is read as a
 *   categorical, wall-type bar on all material representation used in
 *   worship, with no exception carved out for veneration-versus-worship
 *   distinctions. Enforcement required imperial councils, confiscation of
 *   icons, closure of workshops, and coercion of monastic resistance. The
 *   centralizing beneficiary is the imperial authority and its aligned clergy
 *   hierarchy, who gain a unified doctrinal instrument and monopolize the
 *   interpretation of licit worship. This is NOT the same constraint as the
 *   iconodule reading (which reads the same commandment as permitting
 *   honor-through-images) or the moderate iconoclast reading (which permits
 *   two-dimensional images) — each is a structurally distinct constraint with
 *   its own epsilon, beneficiary/victim set, and enforcement profile, linked
 *   only by shared kernel ancestry.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.68).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.87).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, tangled_rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Iconoclast Reading: Categorical Prohibition on Religious Imagery").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '049e687a-d325-4ede-b495-79743fa1c498').
narrative_ontology:cs_kernel_codification('049e687a-d325-4ede-b495-79743fa1c498', fixed_text).
narrative_ontology:cs_authority_grounding('049e687a-d325-4ede-b495-79743fa1c498', extraction).
narrative_ontology:cs_interpretation_layer_present('049e687a-d325-4ede-b495-79743fa1c498').
narrative_ontology:cs_reading_relation('049e687a-d325-4ede-b495-79743fa1c498', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('049e687a-d325-4ede-b495-79743fa1c498', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('049e687a-d325-4ede-b495-79743fa1c498', foundational, material_mediation_of_holy_categorically_impermissible).
narrative_ontology:cs_axiom_status(material_mediation_of_holy_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('049e687a-d325-4ede-b495-79743fa1c498', material_mediation_of_holy_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('049e687a-d325-4ede-b495-79743fa1c498', secondary, veneration_and_worship_are_indistinguishable_in_practice).
narrative_ontology:cs_axiom_status(veneration_and_worship_are_indistinguishable_in_practice, overridden).
narrative_ontology:cs_axiom_grounding('049e687a-d325-4ede-b495-79743fa1c498', veneration_and_worship_are_indistinguishable_in_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('049e687a-d325-4ede-b495-79743fa1c498', aniconic_mosaic_covenant_purity).
narrative_ontology:cs_drift_state('049e687a-d325-4ede-b495-79743fa1c498', post_seventh_ecumenical_council, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('049e687a-d325-4ede-b495-79743fa1c498', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, imperial_iconoclast_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_hierarchy).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, lay_devotional_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates the categorical prohibition as imperial and ecclesiastical policy, convenes councils to ratify it, and directs confiscation and destruction of images. Consolidates religious authority under the throne by asserting a single, centrally-adjudicated reading of the commandment, displacing monastic and local interpretive authority over sacred practice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, imperial_iconoclast_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Bishops and theologians aligned with the imperial position gain office, patronage, and doctrinal authority by supplying the theological justification for the prohibition. Their standing is contingent on the prohibition's continued enforcement; loyalty is rewarded with sees and synodal influence.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_hierarchy, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_hierarchy, agenda_setter).

% Painters, mosaicists, and craftsmen whose livelihood depended on commissioned devotional imagery lose their trade outright; workshops are shut down, materials confiscated, and practicing the craft for worship purposes becomes a punishable offense. Exit means abandoning a trained vocation entirely.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    powerless, biographical, trapped, regional).

% Monasteries that maintained icon veneration as central to communal devotional life face confiscation of icons, imprisonment or exile of resistant monks, and forced compliance under threat of dissolution. Some communities flee to peripheral or foreign territories beyond imperial reach; most cannot relocate without losing their institutional base.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    moderate, generational, constrained, regional).

% Ordinary worshippers whose household and parish devotional practice centered on venerating images lose access to a familiar spiritual vocabulary overnight; icons in homes and churches are removed or destroyed, and continued private veneration risks accusations of idolatry from informers and officials.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, lay_devotional_practitioners, payer,
    powerless, biographical, trapped, local).

% Theologians who would argue the Incarnation sanctifies matter as a conduit to the divine, and that veneration of images is honor passed to their prototype rather than idolatry, are excluded from imperial councils, exiled, or silenced. Their doctrinal counter-argument exists but is not permitted a hearing within the constraint's own adjudicating bodies.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    moderate, generational, constrained, continental).

% Later scholars and church bodies examine the councils, canons, and enforcement record to assess whether the prohibition reflected a stable theological consensus or a contested, politically-instrumentalized reading later reversed by conciliar authority.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unambiguous rule — no material representation in worship — that in principle resolves any dispute about whether a given devotional object crosses into idolatry, removing the need for case-by-case theological adjudication of individual images.
% TRANSFER_FUNCTION: Moves religious authority and interpretive control from monastic and local devotional communities to the imperial center and its aligned clergy; moves material wealth (workshops, commissioned art, trained labor) away from icon producers; moves devotional practice away from lay practitioners toward centrally-approved forms of worship.
% ABSENT_VOICES: Iconodule theologians and monastic defenders of image veneration are the most direct doctrinal objectors and are excluded from the councils that ratify the prohibition — exiled, imprisoned, or simply not seated. Lay practitioners whose household devotional life is disrupted have no formal voice in the councils at all.
% DISAPPEARANCE_RATIONALE: If the prohibition were lifted, icon production would resume as a licit trade, monastic communities would restore public veneration practices, confiscated and destroyed images would need replacement, and the centralized doctrinal authority claimed through enforcement of the ban would lose its primary instrument — the empire's religious-political settlement would need to be renegotiated.
% FOUNDING_PROBLEM: A perceived drift toward idol-worship in popular devotional practice, understood by the prohibition's proponents as a violation of the commandment against graven images and a corruption of proper worship of the divine.
% FOUNDING_PROBLEM_CORROBORATION: The imperial and iconoclast clergy attest the problem is live and worsening. Iconodule theologians and later ecumenical councils (attesting from outside the iconoclast beneficiary set, and eventually prevailing doctrinally) hold that the prohibition mischaracterizes veneration as worship and that no genuine idolatry problem justified the categorical ban — their corroboration undercuts the founding-problem narrative rather than confirming it.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the prohibition transfers devotional practice, trade livelihood, and interpretive authority to the imperial center under coercive enforcement, not through voluntary theological consensus. Suppression is high and rising (0.55 to 0.87) reflecting escalating conciliar ratification, confiscation campaigns, and punishment of resistant monastics over the interval — enforcement infrastructure hardened as resistance from monastic communities and iconodule theologians persisted. Theater ratio rises moderately (0.2 to 0.42) as later phases of enforcement increasingly emphasize performative destruction and public renunciation ceremonies alongside genuine doctrinal conviction.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial authority and aligned clergy are structural beneficiaries: they gain centralized doctrinal control and political consolidation, sit at institutional power with arbitrage-grade exit (they set the rule and are never subject to its costs). Icon producers, monastic communities, and lay practitioners are targets: they are powerless or moderate, largely trapped or constrained in exit, and bear the material and vocational cost of compliance. Iconodule theologians are excluded rather than coordinated — their doctrinal objection exists but has no seat at the adjudicating councils.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (perceived idol-worship corrupting true devotion) may have been genuinely live at the outset, but the R5 corroboration shows that the strongest counter-attestation comes from within the same religious tradition after the fact (later ecumenical restoration of images) — suggesting the categorical reading outlived, or never matched, any genuine doctrinal consensus, and instead functioned as an instrument of imperial centralization that required continuous escalating enforcement to sustain, which is the tangled-rope signature: a real coordination story (doctrinal clarity) yoked to asymmetric extraction (vocational and devotional loss for the powerless) sustained only through active coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_qualified_prohibition_scope,
    'Does the commandment''s original scope actually extend to all material religious representation, or only to representations intended for direct worship/latria as the iconodule and moderate readings hold?',
    'Textual-critical and patristic reception-history analysis of the commandment''s original context and earliest interpretive tradition, cross-referenced against how each reading''s council record justified its scope.',
    'If the categorical scope is a later imperial extension rather than the commandment''s original sense, the iconoclast reading''s coordination claim (resolving genuine doctrinal ambiguity) weakens substantially and the extraction reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_qualified_prohibition_scope, conceptual, 'Whether the categorical reading reflects the commandment''s genuine original scope or an imperially-motivated extension.').

omega_variable(
    imperial_motive_vs_genuine_theological_conviction,
    'Was the iconoclast prohibition driven primarily by genuine theological conviction about idolatry risk, or by imperial political interest in centralizing religious authority and confiscating monastic wealth?',
    'Comparative analysis of enforcement patterns — did confiscation and punishment track doctrinal severity of the alleged idolatry, or track monastic wealth and political independence?',
    'If enforcement tracked wealth/independence rather than doctrinal severity, this strengthens the tangled_rope classification (coordination as cover for extraction); if it tracked doctrinal severity consistently, the coordination function is more genuinely load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_motive_vs_genuine_theological_conviction, empirical, 'Whether enforcement patterns reveal political-extractive motive or consistent theological conviction.').

omega_variable(
    kernel_framing_text_vs_interpretive_authority,
    'Is the kernel here best framed as the fixed text of the commandment itself, or as the interpretive tradition/conciliar authority layered above it that determines which reading is enforced?',
    'Compare classification under a text-as-kernel framing (authority_grounding: lineage, interpretation absorbs drift) versus an authority-as-kernel framing (authority_grounding: extraction, the councils themselves are the contested object).',
    'Under the text-as-kernel framing, the iconoclast reading is one interpretation among several competing for legitimate transmission; under the authority-as-kernel framing, the councils'' capacity to declare and enforce a reading IS the extractive object, and the classification shifts more decisively toward pure extraction with theological coordination as thin cover. This story adopts the authority-as-kernel framing (authority_grounding: extraction) because enforcement, not textual interpretation alone, is what sustains this reading''s dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_text_vs_interpretive_authority, conceptual, 'Alternative framings of what the kernel actually is — the text or the interpretive authority above it — and how that choice shifts classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 50, 0.86).
narrative_ontology:measurement(deca_su_t60, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 60, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconoclast_reading, 0.08).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'Decalogue image prohibition' kernel into structurally distinct constraints per the epsilon-invariance principle: iconoclast_reading (categorical ban, this file), iconodule_reading (latria/dulia distinction, images licit), and moderate_iconoclast_reading (statuary banned, two-dimensional images regulated but permitted). Each carries its own epsilon, beneficiary/victim structure, and enforcement profile; none should be treated as a measurement-basis variant of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
