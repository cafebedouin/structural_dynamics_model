% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Domain-Partition Reading of Kami-Buddha Ontology (Shinbutsu-shugo)
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This story instantiates the domain-partition reading of the kami-buddha
 *   kernel within shinbutsu-shugo (the pre-modern Japanese amalgamation of
 *   kami worship and Buddhism). On this reading, kami and buddhas are
 *   ontologically distinct kinds of entity, each sovereign over a separate
 *   functional domain of human life: kami govern life, purity, birth, and
 *   agricultural fertility; buddhas and bodhisattvas govern death, impurity,
 *   and the fate of the deceased. The two systems coordinate practically (a
 *   household uses shrine ritual for a wedding and temple ritual for a
 *   funeral) without requiring theoretical unification — there is no
 *   hierarchy in which one pantheon derives from or is subordinate to the
 *   other. This is DISTINCT from the honji_suijaku_monism reading (which
 *   holds kami are phenomenal manifestations of an underlying buddha-ground,
 *   a monist identity claim) and from the incoherent_bundle reading (which
 *   denies shinbutsu-shugo is a coherent kernel at all, describing it instead
 *   as an institutionally sustained mix of contradictory commitments). Each
 *   reading is its own constraint with its own epsilon; this file authors
 *   only the domain-partition claim.
 *
 * KEY AGENTS:
 *   - shinto_shrine_priests: ritual authority over life/purity domain, moderate power, regional scope
 *   - buddhist_temple_clergy: ritual authority over death/impurity domain, moderate power, regional scope
 *   - lay_households_managing_ritual_life: practical beneficiaries of the division of ritual labor
 *   - honji_suijaku_theorists: excluded systematizers whose correspondence doctrine is denied ontological standing here
 *   - meiji_era_shinbutsu_bunri_officials: later state actors excluded as anachronistic to the pre-modern practitioners' self-understanding
 *   - comparative_religion_scholars: analytical observers assessing which reading the historical record best supports
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.28).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.22).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.28).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Domain-Partition Reading of Kami-Buddha Ontology (Shinbutsu-shugo)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/philosophy_of_religion/japanese_cultural_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, 'aeb56730-8fb4-401f-b908-812ba2632df8').
narrative_ontology:cs_kernel_codification('aeb56730-8fb4-401f-b908-812ba2632df8', distributed).
narrative_ontology:cs_authority_grounding('aeb56730-8fb4-401f-b908-812ba2632df8', practice).
narrative_ontology:cs_interpretation_layer_present('aeb56730-8fb4-401f-b908-812ba2632df8').
narrative_ontology:cs_reading_relation('aeb56730-8fb4-401f-b908-812ba2632df8', kami_buddha_ontology__honji_suijaku_monism, forecloses).
narrative_ontology:cs_reading_relation('aeb56730-8fb4-401f-b908-812ba2632df8', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('aeb56730-8fb4-401f-b908-812ba2632df8', foundational, kami_and_buddhas_are_ontologically_distinct_kinds).
narrative_ontology:cs_axiom_status(kami_and_buddhas_are_ontologically_distinct_kinds, holdable).
narrative_ontology:cs_axiom_grounding('aeb56730-8fb4-401f-b908-812ba2632df8', kami_and_buddhas_are_ontologically_distinct_kinds, conventional).
narrative_ontology:cs_axiom('aeb56730-8fb4-401f-b908-812ba2632df8', foundational, no_ontological_hierarchy_between_domains).
narrative_ontology:cs_axiom_status(no_ontological_hierarchy_between_domains, holdable).
narrative_ontology:cs_axiom_grounding('aeb56730-8fb4-401f-b908-812ba2632df8', no_ontological_hierarchy_between_domains, conventional).
narrative_ontology:cs_reference_frame('aeb56730-8fb4-401f-b908-812ba2632df8', pre_honji_suijaku_dual_system_practice).
narrative_ontology:cs_drift_state('aeb56730-8fb4-401f-b908-812ba2632df8', post_meiji_shinbutsu_bunri, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aeb56730-8fb4-401f-b908-812ba2632df8', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_shrine_priests).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_temple_clergy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, lay_households_managing_ritual_life).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, functional_complementarity_without_fusion).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, no_ontological_hierarchy_between_kami_and_buddhas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer purity rites, birth observances, and agricultural/life-cycle festivals under the kami's jurisdiction. The domain-partition reading gives them an intact, undiluted sphere of ritual authority over the living and the pure, and protects their institutional relevance from absorption into a buddha-centered cosmology.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_shrine_priests, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, shinto_shrine_priests, agenda_setter).

% Administer funerary rites, memorial services, and afterlife liturgy under buddha/bodhisattva jurisdiction. The domain-partition reading secures an exclusive functional territory (death and impurity) that does not have to be justified as subordinate to or derivative of kami worship.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_temple_clergy, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, buddhist_temple_clergy, agenda_setter).

% Use shrine ritual for births, weddings, and harvest, and temple ritual for funerals and ancestor veneration, without needing either institution to explain its relationship to the other. The partition reading matches lived practice: households move between the two systems as occasions require, coordinating action without needing a unified theology.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, lay_households_managing_ritual_life, beneficiary,
    powerless, biographical, constrained, local).

% Medieval doctrinal specialists who built elaborate identification schemes mapping specific kami to specific buddhas as manifestation-of-ground-truth. Their systematizing project is sidelined by a partition reading that denies any single ontological order links the two pantheons; they would object that partition ignores centuries of doctrinal correspondence literature.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, honji_suijaku_theorists, excluded,
    organized, generational, mobile, national).

% 19th-century state officials who forcibly separated Shinto and Buddhist institutions (shinbutsu bunri), destroying combinatory shrine-temple complexes. They are historically downstream of a partition-style reading pressed into state ideology, but are not party to the pre-modern practitioners' own understanding and are excluded from this constraint's contemporaneous stakeholder set.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, meiji_era_shinbutsu_bunri_officials, excluded,
    institutional, generational, analytical, national).

% Study shinbutsu-shugo as a case of religious syncretism and debate whether pre-modern practitioners held a coherent partition ontology, a monist honji-suijaku framework, or an unsystematized bundle of practices retrofitted with theory after the fact.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows two distinct ritual specialist institutions (shrine and temple) to divide labor across the human life-cycle without either having to subordinate its cosmology to the other: kami handle birth, growth, and purity; buddhas handle death, impurity, and the afterlife.
% TRANSFER_FUNCTION: Moves ritual labor and lay patronage (fees, land grants, offerings) to whichever institution's domain the life-event falls under — shrines receive life-affirming ritual business, temples receive death-and-memorial ritual business — without either side extracting from the other's domain.
% ABSENT_VOICES: Honji-suijaku systematizers and the medieval combinatory-shrine-temple complexes (jingu-ji) they built are excluded from this reading's account — their elaborate identification schemes treating kami as manifestations of buddhas have no place in a strict domain-partition ontology and would object that the historical record shows far more theoretical entanglement than partition allows.
% DISAPPEARANCE_RATIONALE: If the domain-partition understanding vanished, shrine and temple ritual practice would likely continue functioning much as before at the lay level (households would still go to shrines for births and temples for funerals) because the partition largely describes an existing division of ritual labor rather than creating it. But the theoretical self-understanding of each institution as ontologically autonomous would be lost, potentially reopening old hierarchical claims (honji-suijaku subordination in either direction) — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Pre-modern Japanese communities needed both life-affirming ritual (birth, growth, agricultural fertility, purity maintenance) and death-related ritual (funerals, pollution removal, ancestor memorial) but inherited two separate religious systems, one indigenous (kami worship) and one imported (Buddhism), with no textual mandate to merge them.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars attest that the domain division (kami=life/purity, buddhas=death/impurity) is well-documented in shrine and temple practice across centuries and persists in modified form in contemporary Japan (shrine births/weddings, temple funerals). However, doctrinal historians point to extensive honji-suijaku correspondence literature and jingu-ji institutional fusion as evidence the practitioners themselves often held richer, non-partitioned theories — corroboration for pure partition, rather than partition alongside fusion, is contested even among scholars outside either institution's own self-presentation.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, contested).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).
:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the partition reading describes complementary institutional territories rather than one institution capturing value from the other's domain — shrines do not tax temple funerals and temples do not tax shrine births. Suppression is low-moderate (0.22) because nothing prevents a household from seeking cross-domain ritual service if it wished (there is no doctrinal or coercive bar, only convention). Theater ratio is low (0.2) because the division tracks genuine functional specialization (purity ritual technique differs substantially from funerary liturgy) rather than performing a distinction that does no real work. Accessibility collapse is moderate (0.35): once a household accepts the domain-partition framework, alternative framings (monist identification, syncretic fusion rituals) become harder to access as live options, but this is a soft collapse, not an enforced one. Resistance is moderate-low (0.3), reflecting ongoing scholarly and doctrinal contestation over whether partition is the right reading at all — the resistance is mostly in the interpretive record (honji-suijaku theorists, jingu-ji institutional fusion), not in lay practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine priests and temple clergy are both structural beneficiaries: the partition reading secures each institution an intact, non-subordinated domain of ritual authority and lay patronage. Lay households are also beneficiaries in a diffuse sense — they get a coherent practical map for which institution to approach for which life event, without needing to resolve deep theological questions. There are no victims in this reading: the coordination is genuinely low-conflict, which is why the claimed type is rope rather than tangled_rope. Honji-suijaku theorists and shinbutsu-bunri officials are excluded rather than victimized — their voices matter for adjudicating BETWEEN readings (that adjudication happens in the sibling stories, not here) but the domain-partition constraint itself does not extract from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dividing ritual labor between an indigenous life-affirming system and an imported death-related system) remains partially live: households still route different life events to different institutions in contemporary Japan (shrine weddings, temple funerals persist as a strong cultural pattern). This is not a mandatrophied arrangement — the coordination function it names continues to do real work, distinguishing it from Piton candidates elsewhere in the kernel space.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_monism_kernel_indeterminacy,
    'Did pre-modern Japanese religious practitioners actually hold a domain-partition ontology (two distinct sovereign systems) or a honji-suijaku monist ontology (kami as manifestations of an underlying buddha-ground), or did different communities/periods/sects hold genuinely different views such that no single answer is correct across shinbutsu-shugo as a whole?',
    'Systematic textual and institutional-practice survey across period (Nara/Heian/Kamakura/Muromachi) and region, distinguishing doctrinal literature (which skews toward honji-suijaku systematization) from popular/lay practice (which may better fit partition) and from institutional records of jingu-ji fusion complexes.',
    'If the record shows honji-suijaku dominance in doctrinal sources but partition-consistent behavior in lay practice, both readings would be partially vindicated for different strata of the same historical phenomenon, suggesting the kernel itself may not have a single correct reading — supporting the incoherent_bundle sibling''s meta-claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_monism_kernel_indeterminacy, conceptual, 'Whether the domain-partition and honji-suijaku readings describe different historical strata or genuinely competing accounts of the same phenomenon.').

omega_variable(
    coherent_kernel_vs_bundle_ambiguity,
    'Is shinbutsu-shugo a single coherent kernel admitting of rival readings (as this file and the honji_suijaku_monism file both assume), or is the very premise that practitioners held ANY single coherent ontology mistaken — with the incoherent_bundle reading correct that it was always an unsystematized, self-contradictory institutional bundle?',
    'Assess whether contradictions attributed to ''the tradition'' (simultaneous strict separation in some contexts and full identification in others, e.g. at the same shrine-temple complex at different points) are resolvable by period/sect/context distinctions (supporting coherent-kernel-with-rival-readings) or persist even within a single community''s simultaneous practice (supporting incoherent_bundle).',
    'If contradictions are irreducible even at the level of a single community''s simultaneous practice, the entire kernel-with-readings framing (including this domain_partition file) may be a scholarly overlay imposing more theoretical coherence on the historical record than practitioners themselves maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coherent_kernel_vs_bundle_ambiguity, conceptual, 'Whether shinbutsu-shugo supports the kernel-with-rival-readings model at all, or whether the incoherent_bundle reading dissolves the premise of a single contested kernel.').

omega_variable(
    post_meiji_retrospective_construction,
    'To what extent is the clean domain-partition reading itself a retrospective construction shaped by the Meiji-era forced separation (shinbutsu bunri), which imposed institutional and legal separateness on Shinto and Buddhism in the 1868-1872 period — i.e., is this reading describing pre-modern practice, or projecting a post-separation categorical scheme backward onto it?',
    'Compare pre-Meiji primary sources (temple-shrine complex records, medieval ritual calendars) against post-Meiji historiography and popular accounts for evidence of anachronistic backward-projection of the partition framework.',
    'If the partition reading is substantially a post-hoc construction, its accessibility_collapse and low suppression scores may understate how much the modern institutional separation (itself coercively imposed) now constrains which reading of the historical record is culturally legible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_meiji_retrospective_construction, empirical, 'Whether the domain-partition reading pre-dates or post-dates the Meiji forced separation of Shinto and Buddhism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.12).
narrative_ontology:measurement(kami_tr_t20, kami_buddha_ontology__domain_partition, theater_ratio, 20, 0.14).
narrative_ontology:measurement(kami_tr_t40, kami_buddha_ontology__domain_partition, theater_ratio, 40, 0.16).
narrative_ontology:measurement(kami_tr_t60, kami_buddha_ontology__domain_partition, theater_ratio, 60, 0.18).
narrative_ontology:measurement(kami_tr_t80, kami_buddha_ontology__domain_partition, theater_ratio, 80, 0.19).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__domain_partition, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(kami_be_t20, kami_buddha_ontology__domain_partition, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(kami_be_t40, kami_buddha_ontology__domain_partition, base_extractiveness, 40, 0.26).
narrative_ontology:measurement(kami_be_t60, kami_buddha_ontology__domain_partition, base_extractiveness, 60, 0.27).
narrative_ontology:measurement(kami_be_t80, kami_buddha_ontology__domain_partition, base_extractiveness, 80, 0.28).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__domain_partition, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kami_buddha_ontology__domain_partition, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kami_buddha_ontology kernel. domain_partition (this file) claims two ontologically distinct, functionally complementary systems with no hierarchy. honji_suijaku_monism claims ontological identity (kami as suijaku/traces of a buddha honji/ground). incoherent_bundle denies either reading captures a coherent single kernel, treating the tradition as an institutionally sustained bundle of contradictory commitments. Each reading is authored as its own constraint with its own epsilon per the ε-invariance principle; they are linked here via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
