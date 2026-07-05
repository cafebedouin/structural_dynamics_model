% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of the Anthropological Record (Evolution/Migration via Scientific Method)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This story instantiates the NATURALIST reading of the contested
 *   anthropological-record kernel: human origins as revealed by evolutionary
 *   biology and migration genetics, knowable through scientific method, with
 *   supernatural causation methodologically excluded. The coordination
 *   function is real and historically significant — a falsifiable,
 *   cross-checkable evidentiary discipline that replaced authority-based and
 *   untested-tradition-based origin claims. But the same method has been
 *   institutionally extended into credentialing gatekeeping that determines
 *   legal standing (repatriation), curatorial control (museums), and
 *   pedagogical monopoly (curricula) — domains the method's falsifiability
 *   commitment does not itself require monopolizing. The sibling readings
 *   (creationist_reading: divine creation compatible with scriptural
 *   timeline; indigenous_epistemology_reading: relational continuity via oral
 *   tradition) are NOT part of this constraint — they are separate constraint
 *   files linked via network.affects_constraints, each with its own epsilon
 *   and stakeholder structure, per the kernel-committer discipline.
 *
 * KEY AGENTS:
 *   - credentialed_paleoanthropologists: agenda_setter/beneficiary (institutional/arbitrage) — controls interpretive apparatus and collects institutional prestige/funding
 *   - non_credentialed_indigenous_interpreters: payer (powerless/trapped) — bears exclusion from evidentiary standing over their own ancestry
 *   - communities_denied_repatriation_standing: payer (powerless/trapped) — bears direct legal cost of the naturalist evidentiary monopoly
 *   - science_policy_and_museum_boards: observer (institutional/analytical) — adjudicates practical force of competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.58).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.62).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of the Anthropological Record (Evolution/Migration via Scientific Method)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '78a19510-1690-49a7-9656-c6342c285899').
narrative_ontology:cs_kernel_codification('78a19510-1690-49a7-9656-c6342c285899', formalized).
narrative_ontology:cs_authority_grounding('78a19510-1690-49a7-9656-c6342c285899', expertise).
narrative_ontology:cs_interpretation_layer_present('78a19510-1690-49a7-9656-c6342c285899').
narrative_ontology:cs_reading_relation('78a19510-1690-49a7-9656-c6342c285899', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('78a19510-1690-49a7-9656-c6342c285899', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('78a19510-1690-49a7-9656-c6342c285899', foundational, supernatural_causation_methodologically_excluded).
narrative_ontology:cs_axiom_status(supernatural_causation_methodologically_excluded, holdable).
narrative_ontology:cs_axiom_grounding('78a19510-1690-49a7-9656-c6342c285899', supernatural_causation_methodologically_excluded, conventional).
narrative_ontology:cs_axiom('78a19510-1690-49a7-9656-c6342c285899', foundational, physical_evidentiary_record_sufficient_for_origins_knowledge).
narrative_ontology:cs_axiom_status(physical_evidentiary_record_sufficient_for_origins_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('78a19510-1690-49a7-9656-c6342c285899', physical_evidentiary_record_sufficient_for_origins_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('78a19510-1690-49a7-9656-c6342c285899', methodological_naturalism_as_evidentiary_standard).
narrative_ontology:cs_drift_state('78a19510-1690-49a7-9656-c6342c285899', contemporary_repatriation_and_heritage_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('78a19510-1690-49a7-9656-c6342c285899', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_paleoanthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, research_universities).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, peer_reviewed_journals).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_indigenous_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, amateur_researchers_and_avocational_archaeologists).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, communities_denied_repatriation_standing).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, common_descent_of_humans).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, out_of_africa_migration_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control excavation permits, peer review, journal gatekeeping, and university hiring in the field. Their interpretive authority over fossil and genetic evidence is institutionally exclusive: findings that pass through credentialed channels count as knowledge; findings from outside them are treated as folklore or contamination risk regardless of empirical content. They also collect grant funding, museum curation contracts, and academic prestige tied to naturalist framing.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_paleoanthropologists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, credentialed_paleoanthropologists, beneficiary).

% Hold degree-granting monopoly over who becomes a recognized interpreter of the record. Extract tuition, grant overhead, and prestige from training the credentialed class; have no incentive to certify alternative interpretive traditions as equally valid pathways to knowledge production.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, research_universities, beneficiary,
    institutional, generational, arbitrage, global).

% Serve as the chokepoint through which naturalist findings become citable, fundable, and legally admissible as scientific fact. Reject submissions grounded in non-naturalist causal frameworks as a matter of editorial policy, which is defensible as method but also forecloses competing knowledge claims from ever reaching the same institutional standing.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, peer_reviewed_journals, beneficiary,
    institutional, generational, arbitrage, global).

% Hold sustained oral-tradition accounts of ancestry and place that predate and sometimes contradict naturalist migration timelines. Their testimony is treated as ethnographic data about belief rather than as evidence bearing on origins questions; they cannot obtain standing to shape repatriation or heritage-management decisions without translation through credentialed intermediaries.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_indigenous_interpreters, payer,
    powerless, generational, trapped, regional).

% Conduct fieldwork, collect specimens, and publish independently, but find their work systematically excluded from the naturalist record's authoritative channels unless a credentialed collaborator adopts and re-frames it. Their exit is nominally open (self-publish, popular press) but carries no epistemic standing.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, amateur_researchers_and_avocational_archaeologists, payer,
    moderate, biographical, constrained, national).

% Under statutes like NAGPRA-analog frameworks, must demonstrate 'cultural affiliation' using naturalist evidentiary standards (genetic distance, dating) to reclaim ancestral remains and objects, even where their own epistemic tradition already establishes continuity. The naturalist reading's gatekeeping directly determines whether their claims on their own ancestors are legally recognized.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, communities_denied_repatriation_standing, payer,
    powerless, generational, trapped, regional).

% Reject the naturalist framing's exclusion of designed-origin and scriptural-timeline explanations as a foundational premise, not a data dispute. Excluded from the credentialing and publication apparatus entirely; their objection is present in public discourse and school-board politics but absent from the scientific record itself.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, creationist_reading_advocates, excluded,
    organized, civilizational, constrained, national).

% Adjudicate funding priorities, exhibit content, and repatriation compliance. Take testimony from credentialed researchers primarily, occasionally from indigenous communities under legal mandate, and set the practical boundary of whose reading of the record gets institutional force.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, science_policy_and_museum_boards, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, falsifiable, cross-checkable method (stratigraphy, radiometric dating, comparative genomics) for reconstructing human origins that does not depend on any single observer's testimony or tradition, enabling cumulative, revisable knowledge across generations of researchers.
% TRANSFER_FUNCTION: Moves interpretive authority, funding, legal standing, and cultural narrative control from non-credentialed knowledge-holders (indigenous oral tradition keepers, amateur researchers, religious communities) to credentialed institutional actors, on the premise that only naturalist-method findings count as knowledge about origins.
% ABSENT_VOICES: Indigenous epistemic authorities whose oral traditions encode long-standing origin and migration knowledge are structurally absent from the evidentiary standard itself — their accounts are collected as ethnographic data ABOUT the naturalist record rather than admitted AS evidence within it. Creationist advocates are absent by the field's foundational methodological exclusion of supernatural causation, not by oversight.
% DISAPPEARANCE_RATIONALE: If naturalist credentialing gatekeeping vanished overnight, the coordination function (shared falsifiable method) would likely persist informally among practitioners who value it, but the institutional apparatus determining whose origin claims receive legal, curatorial, and pedagogical standing would have to be rebuilt from scratch — repatriation law, museum practice, and school curricula would face immediate legitimacy crises. Credentialed researchers would say the world of reliable knowledge collapses; excluded parties would say the world of forced narrative monopoly ends.
% FOUNDING_PROBLEM: Prior to systematic scientific method, claims about human origins were adjudicated by religious authority, political power, or untested tradition, with no shared cross-checkable procedure for resolving disputes about the physical evidentiary record (fossils, strata, genetic material).
% FOUNDING_PROBLEM_CORROBORATION: Historians and philosophers of science outside the paleoanthropological credentialing apparatus (e.g., science studies scholars) corroborate that the founding problem — the need for a falsifiable, evidence-disciplined method — remains substantively live and was genuinely solved by naturalist method. The same outside literature also documents, independently of both credentialed anthropologists and indigenous advocates, that the credentialing apparatus has since expanded well past what falsifiability requires into gatekeeping over cultural and legal standing questions the method itself does not adjudicate.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, contested).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58) because the naturalist reading's core coordination function (falsifiable method) is genuinely valuable and not itself the extractive component — the extraction rides on the credentialing and legal-standing apparatus built atop the method, which excludes non-credentialed interpreters from evidentiary and legal standing regardless of the empirical merit of their claims. Suppression (0.62) is higher than extraction because active enforcement — peer review gatekeeping, legal evidentiary standards for repatriation, curricular exclusivity — is required to maintain the credentialing monopoly; without enforcement, competing interpretive communities would simply operate in parallel. Theater ratio is comparatively low (0.28) because the underlying scientific method continues to do real evidentiary work; the theatrical component is concentrated in gatekeeping practices (credential requirements untethered from actual falsifiability content) rather than in the method's core operation. Accessibility collapse is high (0.71): once naturalist method is understood as the dominant institutional standard, alternative evidentiary standing genuinely becomes very hard to obtain. Resistance (0.55) reflects sustained, organized pushback from indigenous rights movements, creationist advocacy, and amateur-research communities.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialed-researcher seat, the arrangement is Rope — genuine, hard-won epistemic coordination protecting against unfalsifiable claims. From the seat of communities denied repatriation standing, the identical structure is Tangled Rope shading toward Snare — a coordination story used to justify continued denial of legal and cultural standing over their own ancestors, requiring active enforcement (evidentiary statutes, peer-review exclusion) to hold. The engine computes both from the same structural data; the divergence IS the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed paleoanthropologists, universities, and journals sit near the beneficiary end: they set the rules, collect the institutional rents (funding, prestige, legal deference), and have arbitrage-grade exit (they can move between institutions without losing standing). Non-credentialed indigenous interpreters and repatriation-seeking communities sit near the full-target end: trapped exit (their own ancestral claims are adjudicated by a system that structurally distrusts their evidentiary form), generational time horizon (this affects multi-generational legal and cultural standing), and no coalition leverage against institutional gatekeeping absent legislative intervention. Amateur researchers occupy a middle position: real work product, constrained but not fully trapped exit (self-publication exists but carries no institutional weight).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (needing a falsifiable, cross-checkable method for adjudicating physical evidentiary claims) remains genuinely live — this is not a pure zombie mandate. But the apparatus has drifted from 'method that adjudicates evidentiary claims' toward 'institution that adjudicates legal and cultural standing,' a mandate creep the founding problem does not itself justify. This is precisely the tangled-rope signature: real coordination function persists (Mountain-adjacent at the level of falsifiability itself) while an extraction layer (credentialing monopoly over legal/cultural standing) has been welded onto it and requires active enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    method_vs_gatekeeping_boundary,
    'Is the credentialing apparatus (degree requirements, peer review exclusivity, legal evidentiary standards) a necessary component of the falsifiable method itself, or a separable institutional layer that could be reformed without abandoning naturalist epistemology?',
    'Comparative analysis of jurisdictions or institutions that have opened repatriation and heritage-management standing to non-credentialed oral-tradition evidence alongside naturalist evidence, tracking whether falsifiability and evidentiary rigor degrade as a result.',
    'If separable, the extraction component is a removable institutional accretion and the constraint''s tangled-rope classification could resolve toward rope with reformed governance; if inseparable, the extraction is intrinsic to maintaining any shared evidentiary standard at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_vs_gatekeeping_boundary, conceptual, 'Whether credentialing gatekeeping is necessary to naturalist method or a separable extraction layer.').

omega_variable(
    kernel_reading_relationship_naturalist_creationist,
    'This constraint instantiates the naturalist reading of the anthropological_record kernel. The creationist_reading (divine creation compatible with scriptural timeline) holds that supernatural causation is a legitimate explanatory category for the same physical record. What structural relationship holds between these readings?',
    'This is recorded structurally in cs_structure.reading_relations as ''forecloses'' — a framework committed to methodological naturalism as constitutive of what counts as scientific evidence cannot simultaneously hold that supernatural causation is an admissible explanation within that same evidentiary framework. This is a logical incompatibility at the level of what counts as evidence, not merely a disagreement about which evidence is more persuasive.',
    'If a party attempted to hold both readings within one institutional framework (e.g., a public school science curriculum board), the framework would face an internal contradiction the engine''s foreclosure signature is designed to detect — not a mere policy disagreement but a categorical incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship_naturalist_creationist, conceptual, 'Naturalist and creationist readings foreclose each other within any single evidentiary framework.').

omega_variable(
    kernel_reading_relationship_naturalist_indigenous,
    'What structural relationship holds between the naturalist reading and the indigenous_epistemology_reading (relational continuity via oral tradition)?',
    'This is recorded as ''influences'' rather than ''forecloses'' or ''coexists_with'' in isolation — the naturalist reading''s institutional dominance changes the legitimacy conditions and resource availability for oral-tradition-based knowledge claims (e.g., what counts as admissible evidence in repatriation law) without logically ruling out the truth of relational-continuity claims themselves. Many practitioners hold both as compatible at the level of personal belief while only one has institutional force.',
    'If the relationship were ''forecloses,'' no dual-recognition legal framework (e.g., statutes that weight both genetic and oral-tradition evidence for cultural affiliation) could coherently exist. Since some jurisdictions successfully operate such dual-recognition frameworks, ''influences'' rather than ''forecloses'' is the structurally accurate relation, though the influence is presently asymmetric and extractive in the naturalist reading''s favor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship_naturalist_indigenous, conceptual, 'Naturalist reading structurally influences but does not logically foreclose the indigenous epistemology reading.').

omega_variable(
    credentialing_extraction_magnitude,
    'How much of the measured extraction (0.58) is attributable to genuine coordination overhead (training costs for method competence) versus rent-seeking (credential requirements that exceed what method competence requires)?',
    'Audit of credentialing requirements against a minimal competence standard for method application, compared across peer disciplines with less institutionally entrenched gatekeeping.',
    'A finding of substantial rent-seeking would support reclassification pressure toward snare at the institutional-administration layer even while the underlying method remains mountain-adjacent; a finding of mostly genuine overhead would support rope at more layers than currently classified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialing_extraction_magnitude, empirical, 'Decomposing measured extraction into coordination overhead versus rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__naturalist_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__naturalist_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(anth_tr_t60, anthropological_record__naturalist_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(anth_tr_t80, anthropological_record__naturalist_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(anth_tr_t100, anthropological_record__naturalist_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(anth_be_t20, anthropological_record__naturalist_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(anth_be_t40, anthropological_record__naturalist_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(anth_be_t60, anthropological_record__naturalist_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(anth_be_t80, anthropological_record__naturalist_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(anth_be_t100, anthropological_record__naturalist_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(anth_su_t20, anthropological_record__naturalist_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(anth_su_t40, anthropological_record__naturalist_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(anth_su_t60, anthropological_record__naturalist_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(anth_su_t80, anthropological_record__naturalist_reading, suppression_requirement, 80, 0.59).
narrative_ontology:measurement(anth_su_t100, anthropological_record__naturalist_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__naturalist_reading, 0.1).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the anthropological_record kernel, each authored as an independent constraint story per the epsilon-invariance principle (a single natural-language label — 'what does the human origins record reveal' — conceals three structurally distinct claims with different epsilon values, different beneficiary/victim structures, and different classifications). naturalist_reading (this file) measures extraction from credentialing gatekeeping atop a genuine falsifiable-method coordination function (tangled_rope). creationist_reading measures extraction from institutional/political enforcement of a scriptural-compatibility standard against contrary physical evidence. indigenous_epistemology_reading measures extraction from the systematic non-recognition of oral-tradition evidentiary standing in legal and academic institutions. The three are linked bidirectionally via affects_constraints; none averages or subsumes the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
