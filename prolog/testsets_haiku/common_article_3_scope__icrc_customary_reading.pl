% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: Common Article 3 Scope via Customary International Law Evolution
 *   domain: legal/humanitarian
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions sets minimum humanitarian
 *   standards for armed conflict, but its scope—what counts as 'armed
 *   conflict' triggering its application—has been contested since
 *   ratification. This constraint represents ONE reading: the ICRC's
 *   customary international law approach, which determines CA3 scope through
 *   documented state practice and evolving opinio juris rather than fixed
 *   treaty text. This reading treats CA3 as a procedurally open commitment
 *   that expands as states adopt and recognize new humanitarian norms. The
 *   other sibling readings—state-centric (formal thresholds only) and
 *   expansive human-rights (any organized violence)—represent competing
 *   framings of the same contested kernel (what determines CA3 scope). This
 *   story instantiates ONLY the customary-law reading, not the others; each
 *   has its own ε, beneficiary/victim structure, and type classification.
 *
 * KEY AGENTS:
 *   - ICRC: institutional setter of the customary law procedure; synthesizes state practice into binding interpretation
 *   - State parties to Geneva Conventions: both beneficiaries (avoid formal renegotiation) and payers (bound by evolving customary norms)
 *   - Armed non-state actors: excluded from the practice-documentation process but subject to CA3 scope as customary law expands
 *   - Low-intensity conflict parties: retroactively bound by standards that crystallize after their conduct is recorded as practice
 *   - Humanitarian NGOs and legal scholars: beneficiaries of expansive scope, advocates for customary expansion
 *   - Treaty-text positivists: marginalized by a procedure that privileges practice over text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.31).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.19).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.19).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope via Customary International Law Evolution").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "legal/humanitarian").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'e518c27d-5555-44c3-839b-0d9ed677fe4d').
narrative_ontology:cs_kernel_codification('e518c27d-5555-44c3-839b-0d9ed677fe4d', fixed_text).
narrative_ontology:cs_authority_grounding('e518c27d-5555-44c3-839b-0d9ed677fe4d', lineage).
narrative_ontology:cs_interpretation_layer_present('e518c27d-5555-44c3-839b-0d9ed677fe4d').
narrative_ontology:cs_reading_relation('e518c27d-5555-44c3-839b-0d9ed677fe4d', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('e518c27d-5555-44c3-839b-0d9ed677fe4d', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_axiom('e518c27d-5555-44c3-839b-0d9ed677fe4d', foundational, customary_law_as_binding_source).
narrative_ontology:cs_axiom_status(customary_law_as_binding_source, holdable).
narrative_ontology:cs_axiom_grounding('e518c27d-5555-44c3-839b-0d9ed677fe4d', customary_law_as_binding_source, conventional).
narrative_ontology:cs_axiom('e518c27d-5555-44c3-839b-0d9ed677fe4d', foundational, state_practice_crystallizes_obligation).
narrative_ontology:cs_axiom_status(state_practice_crystallizes_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e518c27d-5555-44c3-839b-0d9ed677fe4d', state_practice_crystallizes_obligation, conventional).
narrative_ontology:cs_reference_frame('e518c27d-5555-44c3-839b-0d9ed677fe4d', geneva_convention_fixed_text_with_evolving_interpretation).
narrative_ontology:cs_drift_state('e518c27d-5555-44c3-839b-0d9ed677fe4d', post_1977_non_international_armed_conflicts_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e518c27d-5555-44c3-839b-0d9ed677fe4d', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_as_interpreter).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, humanitarian_coordination_infrastructure).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.31 at interval end) because the customary law procedure primarily COORDINATES humanitarian protection, not EXTRACTS from parties. The ICRC is the beneficiary (gains authority as arbiter of practice), but the mechanism solves a genuine coordination problem: how to expand humanitarian norms without renegotiating treaties. The 49-year measurement series shows extractiveness RISING gradually (0.08 → 0.31), tracking the procedural accumulation of customary norms around CA3 scope—each major armed conflict adds documented practice; the ICRC synthesizes it; scope expands. Theater is very low (0.12), indicating the customary law procedure is genuinely functional, not theatrical. Suppression is low (0.19) because no party actively opposes documentation of state practice (all states report conduct); opposition comes at the stage of interpretation (whether conduct counts as opinio juris), not at the stage of recording. Accessibility of alternatives is moderate (0.42 collapse): states CAN exit by formal treaty amendment or reinterpreting their own conduct differently, but the customary procedure makes exit costly (requires sustained counternarrative). Resistance is substantial (0.58) from military establishments and states that fear expanding CA3 scope into law-enforcement contexts; they resist the ICRC's interpretations, though openly rather than through coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC and humanitarian NGO seats, the customary law procedure is genuine coordination—it solves the treaty-amendment bottleneck and allows norms to evolve responsively. From the state parties' seats (especially military and security establishments), the procedure appears as mission creep: the ICRC's interpretive authority expands incrementally with each documented case, and states find themselves bound by norms they did not formally consent to. From low-intensity conflict parties' seats, the procedure is retroactive: standards crystallize AFTER their conduct is recorded, imposing obligations they did not anticipate. The engine will compute each seat's directionality from the structural data (the ICRC gains authority, states are constrained, low-intensity parties are targets); these divergences are how the corpus detects whether the customary law reading is a true coordination mechanism or camouflage for an interpretive power grab.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC holds institutional power and gains authority as the synthesizer of customary law—low directionality (0.15–0.25, beneficiary end). State parties have institutional power but constrained exit: they cannot easily opt out of customary international law applicability; their directionality is moderate (0.40–0.55, symmetric: they benefit from avoiding renegotiation but pay through binding interpretations they do not control). Low-intensity conflict parties have only moderate power and trapped exit; they become targets retroactively as their conduct is interpreted as state practice or opinio juris; directionality is high (0.70–0.85, target end). Armed non-state actors are excluded from the procedure entirely; they are identity-locked into the role of subjects of CA3 with no voice in its scope expansion; directionality is near the target end (0.75–0.90). No directionality overrides are required; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows NO mandatrophy: the founding problem (treaty-amendment bottleneck) remains live, and the customary law procedure continues to address it. The rising extractiveness trajectory (0.08 → 0.31) is not evidence of mandatrophy but of the procedure's effectiveness—as more cases accumulate, the synthesized customary norms become more precise and binding, which feels extractive to parties that resist expansive scope. The distinction is important: extractiveness RISING because the mechanism is WORKING BETTER (more state practice accumulates, scope clarifies) is not the same as extractiveness rising because the mechanism's original function has atrophied. Mandatrophy would manifest as theater_ratio RISING while extractiveness FALLS or PLATEAUS—the procedure would become performative theater (states go through motions of reporting practice) without actually expanding scope. The actual pattern (low theater, rising extraction) indicates functional evolution, not degradation. However, a contestable question remains: is the rise in extractiveness driven by genuine humanitarian coordination (expanding standards address real protection gaps), or by institutional scope creep (the ICRC's authority expands beyond its mandate to interpret treaties)? This is routed to omegas rather than asserted as mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_scope_creep_vs_coordination,
    'Is the rising extractiveness of the customary law procedure evidence of genuine humanitarian coordination (norms expanding to address real protection gaps), or institutional scope creep (the ICRC''s interpretive authority expanding beyond its mandate to synthesize treaties)?',
    'Track the ICRC''s documented scope expansions against independent measures of actual humanitarian protection gaps in armed conflicts. If scope expansions consistently address demonstrated protection needs, coordination is genuine; if expansions primarily extend the ICRC''s institutional reach without proportionate humanitarian gain, scope creep is indicated.',
    'If scope creep is found, reclassification may move from ROPE to TANGLED_ROPE (coordination as cover for institutional expansion). If genuine coordination is found, the constraint remains ROPE despite rising extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_scope_creep_vs_coordination, empirical, 'Whether the procedure''s rising extractiveness reflects functional coordination or institutional mission expansion.').

omega_variable(
    customary_law_vs_state_consent,
    'Does binding customary international law—especially scope expansions of CA3 that states did not formally ratify—constitute legitimate legal obligation or illegitimate imposition of norms without state consent?',
    'Examine state resistance to customary law interpretations over time. Consistent, sustained formal protests from a significant state minority would suggest norms lack adequate consent base; quiet acceptance suggests de facto consent has crystallized. Track ratification patterns and state practice in adopting ICRC-synthesized norms.',
    'If lack-of-consent is found, the constraint may reclassify as SNARE from certain state seats (extractive imposition of norms). If consent is found, it remains ROPE (legitimate coordination by practice accumulation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_vs_state_consent, preference, 'Whether customary law expansion without formal consent is legitimate or extractive imposition.').

omega_variable(
    low_intensity_conflict_retroactivity,
    'Is applying expanding CA3 norms retroactively to parties in low-intensity conflicts (whose conduct is recorded as state practice) fair, given they could not have anticipated the scope expansion when engaging in conflict?',
    'Examine cases where CA3 scope expanded after low-intensity conflict conduct occurred, then assess whether criminal/compliance liability was imposed retroactively. Track whether parties were given notice of emerging norms before being held to them.',
    'If significant retroactivity is found, low-intensity conflict seats should reclassify as SNARE victims (retroactive binding norms imposed without consent or notice). If retroactivity is minimal, classification remains within ROPE/TANGLED_ROPE framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(low_intensity_conflict_retroactivity, empirical, 'Whether the procedure creates retroactive legal obligations on parties to low-intensity conflicts.').

omega_variable(
    reading_coexistence_in_courts,
    'Can the three CA3 scope readings coexist within the same court or legal system, or does adoption of one reading foreclose the others?',
    'Examine international court and tribunal decisions applying CA3. If courts cite customary law synthesis alongside treaty text alongside human-rights principles without contradiction, readings coexist. If courts explicitly choose one reading and reject others as incoherent with their chosen framework, readings foreclose one another.',
    'Affects the reading_relations declaration: if courts show coexistence-in-practice, relation is ''coexists_with''; if courts show incompatibility, relation is ''forecloses''. This impacts the ε-invariance principle: if readings foreclose one another, they instantiate genuinely different constraints (different ε); if they coexist, they are perspectivals of the same constraint (same ε, different seats).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_in_courts, empirical, 'Whether the three readings of CA3 scope can coexist in the same legal framework or are mutually exclusive.').

omega_variable(
    documentation_completeness_as_bias,
    'Does the ICRC''s documentation of state practice have systematic biases—e.g., overweighting practices of large, visible states and underweighting practices of smaller or marginalized states—that shape which practices crystallize into customary norms?',
    'Audit the ICRC''s customary law studies for representation of state practice sources. Compare documented practice distribution with actual conflict distribution globally. Test for correlation between state power/visibility and whether practice is cited.',
    'If significant documentation bias is found, the customary law procedure is not neutral synthesis but biased aggregation, which would support reclassification toward TANGLED_ROPE (coordination as cover for asymmetric power). If documentation is representative, classification remains ROPE.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_completeness_as_bias, empirical, 'Whether the ICRC''s documentation of state practice is systematically biased toward certain states or perspectives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1977, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__icrc_customary_reading, theater_ratio, 1977, 0.02).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__icrc_customary_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(comm_tr_t2001, common_article_3_scope__icrc_customary_reading, theater_ratio, 2001, 0.07).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__icrc_customary_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comm_tr_t2018, common_article_3_scope__icrc_customary_reading, theater_ratio, 2018, 0.11).
narrative_ontology:measurement(comm_tr_t2026, common_article_3_scope__icrc_customary_reading, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1977, 0.08).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1990, 0.14).
narrative_ontology:measurement(comm_be_t2001, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2001, 0.22).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(comm_be_t2018, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2018, 0.29).
narrative_ontology:measurement(comm_be_t2026, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2026, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(common_article_3_scope__icrc_customary_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__icrc_customary_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% Common Article 3 scope is contested across three readings. This story (icrc_customary_reading) models scope-by-practice, a procedural/institutional mechanism. The state-centric reading models scope-by-text/thresholds, treating CA3 as a fixed commitment. The expansive reading models scope-by-principle, treating CA3 as a binding humanitarian baseline. All three instantiate the same kernel (the Geneva Convention text) but arrive at different ε values and stakeholder structures by emphasizing different sources of interpretive authority (practice synthesis, treaty text, humanitarian principle). Network links show how each reading creates downstream pressure on the others: the customary reading undermines the state-centric reading by showing that state practice has moved beyond the text; the expansive reading undermines both by arguing the principle is more binding than either text or practice; the state-centric reading constrains the other two by insisting that scope requires formal state consent. These are not contradictions to resolve—they are the structure of a living contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
