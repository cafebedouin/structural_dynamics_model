% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at Divine Mandate: Pharaoh as Cosmic Order Source
 *   domain: political/religious
 *
 * SUMMARY:
 *   This constraint instantiates the divine-mandate reading of the Ma'at
 *   principle in ancient Egyptian political theology. Ma'at — cosmic order,
 *   justice, truth, and balance — is presented as flowing from divine
 *   creation through the Pharaoh as its sole earthly embodiment to society
 *   below. In this reading, the Pharaoh cannot violate Ma'at by definition
 *   because the Pharaoh's identity IS Ma'at's earthly form. This stands in
 *   contrast to the reciprocity reading (Ma'at imposes mutual obligations on
 *   Pharaoh and subjects) and the distributed-maintenance reading (all actors
 *   sustain Ma'at through proper conduct in their stations). The
 *   divine-mandate reading justifies unaccountable extraction as cosmic
 *   necessity and suppresses alternative readings through theological
 *   claim-making rather than physical coercion.
 *
 * KEY AGENTS:
 *   - Pharaoh: institutional agenda-setter, identity-locked to cosmic role; embodiment premise means the Pharaoh cannot be constrained by the system the Pharaoh stands outside of.
 *   - Priesthood rank-and-file: organized payer-beneficiary; maintain ritual validation of the Pharaoh's cosmic mediation while bearing suppression costs for defending the reading against alternatives.
 *   - Subject populations: powerless trapped payers; must obey Pharaonic decrees as cosmic necessity; have no appeal mechanism because the Pharaoh is the sole source of justice-definition.
 *   - Provincial governors: powerful constrained payers; derive legitimacy from Pharaonic alignment but cannot appeal to Ma'at as accountability standard because the reading locates accountability authority in the Pharaoh alone.
 *   - Scribal record-keepers and alternative theologians: excluded voices; suppressed from arguing reciprocity or distributed-maintenance readings of the same Ma'at tradition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.78).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.87).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at Divine Mandate: Pharaoh as Cosmic Order Source").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "political/religious").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '502aed31-c09f-4061-8957-77139d6625a0').
narrative_ontology:cs_kernel_codification('502aed31-c09f-4061-8957-77139d6625a0', fixed_text).
narrative_ontology:cs_authority_grounding('502aed31-c09f-4061-8957-77139d6625a0', extraction).
narrative_ontology:cs_interpretation_layer_present('502aed31-c09f-4061-8957-77139d6625a0').
narrative_ontology:cs_reading_relation('502aed31-c09f-4061-8957-77139d6625a0', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('502aed31-c09f-4061-8957-77139d6625a0', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('502aed31-c09f-4061-8957-77139d6625a0', foundational, pharaoh_identity_coextensive_with_ma_at).
narrative_ontology:cs_axiom_status(pharaoh_identity_coextensive_with_ma_at, holdable).
narrative_ontology:cs_axiom_grounding('502aed31-c09f-4061-8957-77139d6625a0', pharaoh_identity_coextensive_with_ma_at, theological).
narrative_ontology:cs_axiom('502aed31-c09f-4061-8957-77139d6625a0', foundational, cosmic_order_requires_unambiguous_singular_source).
narrative_ontology:cs_axiom_status(cosmic_order_requires_unambiguous_singular_source, holdable).
narrative_ontology:cs_axiom_grounding('502aed31-c09f-4061-8957-77139d6625a0', cosmic_order_requires_unambiguous_singular_source, deontological).
narrative_ontology:cs_reference_frame('502aed31-c09f-4061-8957-77139d6625a0', pharaoh_cosmic_embodiment).
narrative_ontology:cs_drift_state('502aed31-c09f-4061-8957-77139d6625a0', late_dynastic_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('502aed31-c09f-4061-8957-77139d6625a0', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaoh_institution).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, subject_populations).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, provincial_governors).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, priesthood_rank_and_file).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers resources, authority, and legitimacy upward to the Pharaoh with no reciprocal obligation structure and no exit options for subjects. The Pharaoh's unaccountability is the extraction mechanism itself. Suppression is very high (0.87) because the constraint must actively suppress alternative readings of Ma'at that coexist in the same textual tradition — the divine-mandate reading is not the only coherent reading, so its persistence depends on enforcement against competing interpretations, not on universal belief. Theater ratio is elevated (0.62) because as dynastic records show actual Pharaonic failure (military defeat, crop failure, civil unrest), the divine-mandate reading requires increasingly elaborate ritual activity and narrative management to sustain the claim that the Pharaoh embodies perfected Ma'at. The measurement series shows extraction stabilizing at high levels while theater rises slightly — the reading requires increasing performance to maintain believability as reality diverges from the claim. Suppression remains high and stable because alternative readings never disappear; they must be continuously suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The Pharaoh's seat perceives this constraint as genuine cosmic principle — unquestionable, binding, and legitimating. The priesthood's seat perceives it as a coordination solution that prevents chaos and distributes authority cleanly, while suppressing their own doubts about whether the Pharaoh truly embodies perfected Ma'at. Subject populations experience it as enforced extraction justified by unfalsifiable theological claim. Provincial governors experience it as structural trap — they gain legitimacy from alignment but cannot appeal to justice standards when demands conflict with their own understanding of Ma'at. The engine will compute different types from different seats because the structural relationship differs: from the Pharaoh's position, the constraint is natural (cosmic order itself); from the payer seats, it is extractive and suppressed. This divergence is exactly the engine's job to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh sits at d ≈ 0.0 (full beneficiary — collects legitimacy, resources, unaccountability). Subject populations sit at d ≈ 1.0 (full target — pay labor, tribute, vulnerability without exit or appeal). Provincial governors sit at d ≈ 0.7–0.8 (mostly target — gain some status alignment, but locked into absolute obedience and cannot use Ma'at as constraint on Pharaonic demands). Priesthood sits at d ≈ 0.4–0.5 (hybrid — benefit from institutional resources and authority, but bear suppression costs and identity-lock into defending a reading that increasingly diverges from observable reality). The beneficiary/victim declarations direct this derivation: Pharaoh is beneficiary, subject populations and governors are victims, priesthood is split. Exit options sharpen the targets: subjects are trapped (no exit), governors are constrained (exit costs career and legitimacy), priesthood are constrained (exit means heresy). The Pharaoh's exit is identity_locked in a different sense — exit would dissolve the role itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows one form of mandatrophy in the theological register. The founding problem — how to consolidate rule over a vast territory without standing enforcement — was genuinely urgent in early New Kingdom consolidation. The divine-mandate reading offered a solution: make the Pharaoh immune to accountability by definition. But as dynasties stabilized and the Pharaoh's practical power exceeded their theoretical need for cosmic legitimacy, the constraint persisted by institutional inertia and theological maintenance rather than by necessity. By the time explicit criticism of Pharaohs for Ma'at violation appears in the record (late-dynastic and Ptolemaic), the divine-mandate reading had become a contestable theological position, not a settled fact. The theater ratio's rise reflects this drift: increasing ritual elaboration and narrative management were required to maintain the reading against evidence that the Pharaoh could and did violate Ma'at. No single party benefited enough to modernize or dismantle the constraint (the priesthood depended on it; the Pharaoh had other legitimacy sources but kept the reading as useful); no single party was hurt enough to fix it (even subjects had no coordinated alternative framework). The constraint persisted as performance — the reading lived on because it was useful to some and inertially stable, not because it was believed as cosmic fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmic_embodiment_empirical_status,
    'Is the claim that the Pharaoh embodies cosmic Ma''at a metaphysical fact, a theological doctrine, or a legitimacy narrative?',
    'Examine whether the claim is treated as revisable within the tradition (if evidence of Pharaonic failure can prompt reinterpretation, it is theology/narrative; if failure is treated as logically impossible, it asserts metaphysical status). Historical record shows the claim shifting between registers — treated as absolute in some texts, qualified in others, explicitly contested in late-dynastic sources.',
    'If metaphysical, the divine-mandate reading forecloses alternative readings logically (no framework could hold both a metaphysical fact and its negation). If theological/narrative, alternative readings coexist. The structural difference determines whether this reading forecloses or merely influences the siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmic_embodiment_empirical_status, conceptual, 'The ontological status of the Pharaoh''s cosmic embodiment claim.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression of alternative readings structural (theological gatekeeping, institutional exclusion) or internalized (priests and scribes genuinely believed the divine-mandate reading was the only coherent interpretation)?',
    'Examine scribal records, private letters, and tomb inscriptions for evidence of cognitive friction — priests expressing doubts about the reading, scribes noting contradictions, alternative readings maintained as ''esoteric'' knowledge. The late-dynastic evidence suggests substantial internalization (alternative readings are not just silenced but treated as self-evidently wrong by believers), while political-crisis periods show structural suppression becoming visible.',
    'If internalized, the constraint carries suppression beyond its formal elimination — priests would defend the reading even after losing institutional power. If structural, the constraint would destabilize more quickly if institutional enforcement were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative readings is externally enforced or internalized as genuine cognitive conviction.').

omega_variable(
    founding_problem_obsolescence,
    'Did the founding problem (how to maintain control without standing enforcement) actually persist into the middle and late dynastic periods, or was the problem solved such that the divine-mandate reading became mandatrophic (persisting through inertia rather than necessity)?',
    'Examine administrative records, military organization, and succession histories. Middle Kingdom records show increasingly developed standing enforcement apparatus and bureaucratic coordination mechanisms that reduce dependence on cosmic legitimacy claims. Late Kingdom records show explicit appeal to Ma''at as accountability standard for Pharaohs, indicating the founding problem had been reframed or solved.',
    'If the problem persisted, the divine-mandate reading maintained genuine necessity and coordination function. If the problem was solved, the reading became mandatrophic — persisted through priesthood investment and Pharaonic advantage, not through necessity. The theater ratio''s rise supports the mandatrophy hypothesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem remained urgent across dynastic history or became solved, making the constraint mandatrophic.').

omega_variable(
    committer_frame_alternative_readings,
    'In the same textual tradition (Old Kingdom Pyramid Texts, New Kingdom theology, Ptolemaic period sources), the reciprocity and distributed-maintenance readings are also attested. What determines which reading is treated as ''the'' Ma''at principle at different moments?',
    'Analyze the correlation between (1) Pharaonic political stability, (2) priesthood institutional power, (3) explicit evidence of Pharaonic failure, and (4) which Ma''at reading is emphasized in official ideology. The data suggest: divine-mandate emphasized when Pharaoh is strongest and priesthood is aligned; reciprocity and distributed-maintenance emphasized when Pharaoh is contested or Pharaonic failure is undeniable.',
    'If readings track political conditions rather than theological truth, the divine-mandate reading is a constructed political theology (a snare with ideological cover) rather than a natural cosmic principle. The engine''s structural analysis should reveal this through high suppression + high theater + high extraction despite claims of natural cosmic order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_alternative_readings, empirical, 'Whether the choice of Ma''at reading is determined by political/institutional conditions or by independent theological argument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(maat_tr_t5, maat_order_principle__divine_mandate_reading, theater_ratio, 5, 0.51).
narrative_ontology:measurement(maat_tr_t10, maat_order_principle__divine_mandate_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(maat_tr_t15, maat_order_principle__divine_mandate_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__divine_mandate_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(maat_tr_t25, maat_order_principle__divine_mandate_reading, theater_ratio, 25, 0.61).
narrative_ontology:measurement(maat_tr_t30, maat_order_principle__divine_mandate_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__divine_mandate_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(maat_be_t5, maat_order_principle__divine_mandate_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(maat_be_t10, maat_order_principle__divine_mandate_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(maat_be_t15, maat_order_principle__divine_mandate_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__divine_mandate_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(maat_be_t25, maat_order_principle__divine_mandate_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(maat_be_t30, maat_order_principle__divine_mandate_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__divine_mandate_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(maat_su_t5, maat_order_principle__divine_mandate_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement(maat_su_t10, maat_order_principle__divine_mandate_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(maat_su_t15, maat_order_principle__divine_mandate_reading, suppression_requirement, 15, 0.84).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__divine_mandate_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(maat_su_t25, maat_order_principle__divine_mandate_reading, suppression_requirement, 25, 0.86).
narrative_ontology:measurement(maat_su_t30, maat_order_principle__divine_mandate_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__divine_mandate_reading, suppression_requirement, 40, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(maat_order_principle__divine_mandate_reading, 0.12).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Ma'at order principle kernel. The divine-mandate reading treats the Pharaoh as the sole authorized voice of cosmic order, with high extraction justified as cosmic necessity and high suppression of alternative readings enforced through theological claim-making. Sibling readings (reciprocity and distributed-maintenance) present different beneficiary/victim structures and different ε-values rooted in the same theological tradition. All three stories share the same constraint family identifier (maat_order_principle) and link to each other via network.affects_constraints. The engine will compute different types from each reading; divergence is diagnostic of reading-level specification rather than measurement error.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__divine_mandate_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
