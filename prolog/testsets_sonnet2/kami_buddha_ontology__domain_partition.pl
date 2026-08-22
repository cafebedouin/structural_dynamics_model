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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Domain-Partition Reading of Kami-Buddha Ontological Separation (Shinbutsu-shugo)
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This story instantiates the domain-partition reading of the
 *   shinbutsu-shugo kernel: the claim that kami and buddhas are ontologically
 *   distinct entities governing non-overlapping functional domains — Shinto
 *   handling life, purity, and the living; Buddhism handling death, impurity,
 *   and the deceased. Under this reading the coordination between the two
 *   traditions is practical rather than theoretical: institutions divide
 *   ritual labor along a life/death axis without needing a unifying
 *   metaphysics. This is deliberately NOT the honji-suijaku reading (which
 *   asserts kami are traces of an underlying buddha-ground, collapsing the
 *   two into one ontology with an implicit hierarchy) and NOT the
 *   incoherent-bundle reading (which denies shinbutsu-shugo is a coherent
 *   kernel at all, treating it as institutionally sustained contradiction).
 *   The domain-partition reading is the most benign of the three: it authors
 *   low extraction and low suppression because, on its own terms, it
 *   describes complementary specialization rather than either doctrinal
 *   subordination or incoherence management. Extractiveness here is not zero
 *   because the reading still allocates real jurisdictional value (ritual fee
 *   income, doctrinal authority) along institutional lines that both
 *   benefiting institutions have an interest in preserving — but nothing in
 *   this reading requires coercion to hold, and it faces genuine descriptive
 *   challenges from the historical record of jingu-ji shrine-temple complexes
 *   and the Meiji state's forcible separation, both of which suggest more
 *   entanglement than a clean partition allows.
 *
 * KEY AGENTS:
 *   - shinto_shrine_priests: beneficiary/agenda_setter (organized/constrained) — hold exclusive jurisdiction over life-domain ritual under this reading
 *   - buddhist_temple_clergy: beneficiary/agenda_setter (organized/constrained) — hold exclusive jurisdiction over death-domain ritual under this reading
 *   - lay_households_managing_death_ritual: beneficiary (moderate/constrained) — get a clear practical division of ritual labor
 *   - honji_suijaku_theorists: excluded (moderate/identity_locked) — the sibling reading's proponents, structurally absent from this account
 *   - meiji_state_shinbutsu_bunri_officials: excluded (institutional/trapped) — their forcible-separation intervention presupposes prior fusion, awkward for this reading
 *   - comparative_religion_scholars: observer (analytical/analytical) — assess the historical record against all three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.28).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.22).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.28).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Domain-Partition Reading of Kami-Buddha Ontological Separation (Shinbutsu-shugo)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/philosophy_of_religion/japanese_cultural_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '8be42a95-a83b-42fa-8c5d-052ad88bf622').
narrative_ontology:cs_kernel_codification('8be42a95-a83b-42fa-8c5d-052ad88bf622', distributed).
narrative_ontology:cs_authority_grounding('8be42a95-a83b-42fa-8c5d-052ad88bf622', practice).
narrative_ontology:cs_interpretation_layer_present('8be42a95-a83b-42fa-8c5d-052ad88bf622').
narrative_ontology:cs_reading_relation('8be42a95-a83b-42fa-8c5d-052ad88bf622', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('8be42a95-a83b-42fa-8c5d-052ad88bf622', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('8be42a95-a83b-42fa-8c5d-052ad88bf622', foundational, ontological_non_identity_of_kami_and_buddhas).
narrative_ontology:cs_axiom_status(ontological_non_identity_of_kami_and_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('8be42a95-a83b-42fa-8c5d-052ad88bf622', ontological_non_identity_of_kami_and_buddhas, conventional).
narrative_ontology:cs_axiom('8be42a95-a83b-42fa-8c5d-052ad88bf622', foundational, functional_complementarity_without_hierarchy).
narrative_ontology:cs_axiom_status(functional_complementarity_without_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('8be42a95-a83b-42fa-8c5d-052ad88bf622', functional_complementarity_without_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('8be42a95-a83b-42fa-8c5d-052ad88bf622', pre_honji_suijaku_dual_tradition_coexistence).
narrative_ontology:cs_drift_state('8be42a95-a83b-42fa-8c5d-052ad88bf622', post_medieval_doctrinal_elaboration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8be42a95-a83b-42fa-8c5d-052ad88bf622', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_shrine_priests).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_temple_clergy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, lay_households_managing_death_ritual).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, functional_domain_separation_doctrine).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, purity_impurity_dual_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami worship, purification rites, and life-affirming ceremonies (birth, marriage, harvest, community protection). The domain-partition reading secures their exclusive jurisdiction over purity and the living, keeping death and its pollution outside shrine precincts and therefore outside their liability. They benefit from a clean division of ritual labor that protects shrine purity requirements without needing to negotiate theological subordination to Buddhist cosmology.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_shrine_priests, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, shinto_shrine_priests, agenda_setter).

% Administer funerary rites, memorial services, and ancestral veneration under a doctrinal apparatus equipped to handle death and impurity without cosmological contamination risk. The partition reading grants temples an uncontested monopoly over death-domain ritual economy (funeral fees, memorial tablets, grave maintenance) without requiring their doctrine to absorb or subordinate kami into a Buddhist hierarchy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_temple_clergy, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, buddhist_temple_clergy, agenda_setter).

% Households navigate both institutions across a lifecycle: shrine visits for birth and community events, temple services for funerals and ancestral rites. The domain partition gives them a clear, practically workable division of labor — they know which institution to approach for which need without having to adjudicate a theological claim about ultimate unity or hierarchy between kami and buddhas.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, lay_households_managing_death_ritual, beneficiary,
    moderate, biographical, constrained, local).

% Medieval doctrinal specialists who developed the honji-suijaku framework asserting kami are traces of an underlying buddha-ground. From the partition reading's vantage they are simply not part of this account: the two-ontology reading does not engage their monist claim, it structurally excludes it by asserting the parties never needed unification to coordinate. Their doctrinal apparatus, where cited at all under this reading, is treated as theological elaboration external to the working coordination the partition describes.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, honji_suijaku_theorists, excluded,
    moderate, generational, identity_locked, national).

% Nineteenth-century state officials who forcibly separated Shinto and Buddhist institutions (shinbutsu bunri, 1868) presupposing that the pre-Meiji arrangement was an entangled fusion requiring violent disentanglement. Their historical action is difficult to explain under a pure domain-partition reading, since forcible separation implies there was something fused to separate — this reading treats their intervention as a political imposition rather than as evidence bearing on the pre-existing ontological structure, and thereby brackets a major historical actor.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, meiji_state_shinbutsu_bunri_officials, excluded,
    institutional, generational, trapped, national).

% Analyze shinbutsu-shugo as a case study in religious syncretism, evaluating whether the historical record supports parallel-domain coexistence, hierarchical subordination, or incoherent bundling. They assess primary sources (temple-shrine complex records, ritual calendars, doctrinal treatises) without an institutional stake in which reading prevails.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the total space of ritual need across the human lifecycle (birth, growth, marriage, community protection versus death, mourning, ancestral memorial) to two separate institutional systems, each competent and doctrinally equipped for its half, avoiding the need for either tradition to develop internal machinery for the domain it does not handle.
% TRANSFER_FUNCTION: Moves ritual labor, fee income, and doctrinal authority along a life/death axis: life-domain ritual practice and revenue flow to shrines, death-domain ritual practice and revenue flow to temples. No transfer crosses between the two systems because the reading asserts no ontological bridge exists for value or authority to travel across.
% ABSENT_VOICES: Honji-suijaku theorists and syncretist practitioners who spent centuries producing combinatory doctrine (ryobu Shinto, Sanno Shinto) are not accommodated by this reading — their elaborate identification schemes become external to the account. Meiji shinbutsu-bunri officials, whose forcible separation implies a pre-existing fusion, are also structurally absent since their intervention presupposes exactly what this reading denies needed doing.
% DISAPPEARANCE_RATIONALE: If the domain-partition understanding vanished, shrine and temple institutions would still exist and lay households would probably continue the same practical behavior (shrine for the living, temple for the dead) out of habit and convenience, so at the level of ritual practice the world may be largely unchanged. But at the level of institutional self-justification and doctrinal claim-making, its disappearance would remove the framework shrines and temples currently use to explain why they do not need to resolve their historical entanglement — that would likely reopen dormant disputes over jurisdiction, especially at shrine-temple complexes (jingu-ji) where the historical record shows genuine functional overlap.
% FOUNDING_PROBLEM: Historically, kami worship and Buddhist practice arrived in Japan through different routes and needed some working modus vivendi that let both traditions operate without either delegitimizing the other or requiring lay practitioners to choose exclusively between them.
% FOUNDING_PROBLEM_CORROBORATION: Shrine priests and temple clergy institutionally attest that the division is functional and long-standing (self-interested, since it grounds their separate jurisdictions). Independent corroboration is thin: historians of the shinbutsu-shugo period (e.g. scholarship on jingu-ji complexes and combinatory kami-buddha shrines) document extensive doctrinal fusion and shared institutional space that complicates a clean partition account, and the Meiji state's need to forcibly disentangle the two traditions in 1868 is itself outside-party evidence that the pre-Meiji arrangement was not simply two parallel non-interacting systems. No fully disinterested corroboration for the domain-partition reading specifically (as opposed to the honji-suijaku or bundle readings) has been identified outside the institutions it benefits.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, contested).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low-moderate (0.28) because the domain-partition reading, on its own terms, describes a genuinely complementary division of institutional labor rather than an extractive hierarchy — no party is described as subordinated to another's cosmology. It is not zero because real economic and jurisdictional value (funeral fees, shrine offerings, doctrinal authority over specific life-events) is allocated by the partition and both institutions have a stake in the partition holding. Suppression is low (0.22) because nothing in the domain-partition account requires coercive enforcement against dissent — the reading is a description of coexistence, not a policed boundary, though some residual suppression is warranted because clergy on both sides have an interest in not raising the syncretic complications that the historical record (jingu-ji complexes) actually presents. Theater ratio is low (0.15): this is a description of a working arrangement, not a maintained performance, though it rises slightly toward the interval's end as shinbutsu-shugo practice grew more elaborate and self-justifying. Accessibility collapse (0.35) and resistance (0.3) are moderate-low: alternative framings (monism, incoherence) remain live and contestable throughout the period — the partition reading never achieves mountain-like closure over the alternatives, which is itself evidence that this is a rope (voluntary coordination reading) rather than anything approaching mountain or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine priests and temple clergy are both beneficiaries and, jointly, the agenda-setters who maintain the practical division — but neither dominates the other under this reading, which is the structural signature of a rope rather than a tangled rope: there is no asymmetric extraction, no single party paying while another collects. Lay households are net beneficiaries of a workable, low-friction system for navigating life-cycle ritual needs. There are no true victims in this reading (the victims array is deliberately empty) — the domain-partition account does not describe anyone being extracted from, which is exactly what distinguishes it from the honji-suijaku reading (where kami-associated communities might read hierarchical subordination as extractive) and the incoherent-bundle reading (where the persistence of institutional contradiction itself could be read as extracting coherence-costs from practitioners). Honji-suijaku theorists and Meiji officials are marked excluded rather than victim because their objection is to the reading's descriptive adequacy, not to being extracted from by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating two traditions with different origins so neither needs to fully subordinate or absorb the other — is genuinely contested as to whether it is live, dead, or was ever quite what this reading says it was. If the founding problem is 'dead' (i.e., modern secular households no longer feel any tension requiring resolution, they simply have habits), the domain-partition reading persists mostly as post-hoc institutional rationale for a jurisdictional line that is now maintained by economic interest (temple funeral-fee income, shrine ceremonial income) rather than by live theological need. The engine's computed type is left to diverge from the claimed rope if the metrics point elsewhere; the story does not tune claim to metric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jingu_ji_complex_evidence,
    'Do the historical jingu-ji (shrine-temple complexes), where kami worship and Buddhist practice were physically co-located and administratively intertwined, refute the domain-partition reading''s claim of clean functional separation, or are they compatible with partition as complementary co-location without ontological fusion?',
    'Close historical-institutional analysis of jingu-ji administrative records: were kami and buddha ritual functions kept doctrinally and administratively separate within the same physical complex, or were they treated as expressions of a single underlying system (supporting the honji-suijaku reading instead)?',
    'If jingu-ji records show doctrinal fusion rather than mere co-location, the domain-partition reading is descriptively weaker than the honji-suijaku reading for a substantial swath of the historical record, and this constraint''s claimed_type should be read with greater skepticism relative to its sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jingu_ji_complex_evidence, empirical, 'Whether shrine-temple complex evidence supports partition or fusion.').

omega_variable(
    meiji_separation_as_evidence_of_prior_fusion,
    'Does the Meiji state''s need to forcibly separate Shinto and Buddhist institutions in 1868 (shinbutsu bunri) constitute evidence that the pre-Meiji arrangement was NOT simply two parallel, non-interacting ontologies — i.e., that something requiring violent disentanglement must have been entangled?',
    'Analysis of Meiji government edicts and the resistance/compliance patterns of shrine and temple institutions during separation: if separation was easy and institutions were already functionally distinct, that supports domain-partition; if separation was disruptive and contested precisely because functions were intertwined, that supports honji-suijaku or incoherent-bundle readings.',
    'A finding of high disruption during Meiji separation would suggest the domain-partition reading retrofits a clean story onto an entangled historical reality, strengthening the case for reading this kernel instead as honji_suijaku_monism or incoherent_bundle for the pre-Meiji period specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_evidence_of_prior_fusion, empirical, 'Whether forcible Meiji-era separation implies prior ontological fusion.').

omega_variable(
    framing_choice_institution_vs_legitimacy_narrative,
    'Is the correct kernel-reading unit the institutional division of ritual labor (shrines vs temples, life vs death) or the legitimacy narrative each tradition tells about why it does not need to theologically absorb the other? These two framings could support different readings: the institutional-division framing supports domain_partition cleanly, while the legitimacy-narrative framing may reveal that both traditions'' self-justifications actually borrow honji-suijaku-style identification claims when convenient (e.g., a shrine invoking a bodhisattva identity for its kami to gain temple-network legitimacy) and drop them when inconvenient — which would look more like the incoherent_bundle reading.',
    'Compare institutional operating records (who performs what ritual, who collects what fee) against doctrinal/legitimacy texts (what each institution says about its relationship to the other) across several time periods.',
    'If the legitimacy-narrative framing is adopted as primary, this story''s claimed_type and low extractiveness score would likely need revision toward acknowledging more inconsistency, moving the story''s descriptive profile closer to incoherent_bundle. The domain_partition reading is authored here under the institutional-division framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_institution_vs_legitimacy_narrative, conceptual, 'Alternative framing (institutional division vs. legitimacy narrative) could shift which reading the evidence best supports.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(kami_tr_t0, projected).
narrative_ontology:measurement(kami_tr_t400, kami_buddha_ontology__domain_partition, theater_ratio, 400, 0.11).
narrative_ontology:measurement_basis(kami_tr_t400, projected).
narrative_ontology:measurement(kami_tr_t800, kami_buddha_ontology__domain_partition, theater_ratio, 800, 0.12).
narrative_ontology:measurement_basis(kami_tr_t800, projected).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__domain_partition, theater_ratio, 1200, 0.13).
narrative_ontology:measurement_basis(kami_tr_t1200, projected).
narrative_ontology:measurement(kami_tr_t1600, kami_buddha_ontology__domain_partition, theater_ratio, 1600, 0.14).
narrative_ontology:measurement_basis(kami_tr_t1600, projected).
narrative_ontology:measurement(kami_tr_t1868, kami_buddha_ontology__domain_partition, theater_ratio, 1868, 0.15).
narrative_ontology:measurement_basis(kami_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(kami_be_t0, projected).
narrative_ontology:measurement(kami_be_t400, kami_buddha_ontology__domain_partition, base_extractiveness, 400, 0.22).
narrative_ontology:measurement_basis(kami_be_t400, projected).
narrative_ontology:measurement(kami_be_t800, kami_buddha_ontology__domain_partition, base_extractiveness, 800, 0.25).
narrative_ontology:measurement_basis(kami_be_t800, projected).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__domain_partition, base_extractiveness, 1200, 0.26).
narrative_ontology:measurement_basis(kami_be_t1200, projected).
narrative_ontology:measurement(kami_be_t1600, kami_buddha_ontology__domain_partition, base_extractiveness, 1600, 0.27).
narrative_ontology:measurement_basis(kami_be_t1600, projected).
narrative_ontology:measurement(kami_be_t1868, kami_buddha_ontology__domain_partition, base_extractiveness, 1868, 0.28).
narrative_ontology:measurement_basis(kami_be_t1868, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kami_buddha_ontology__domain_partition, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__domain_partition, 0.1).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kami_buddha_ontology kernel (shinbutsu-shugo). domain_partition (this story) authors low extraction/suppression consistent with genuine complementary coordination. honji_suijaku_monism authors a different structural claim (identity/hierarchy: kami as traces of a buddha-ground) with its own ε and likely higher suppression given the implicit subordination. incoherent_bundle authors the claim that no coherent kernel exists at all, treating the apparent coordination as institutionally sustained contradiction-management, which would carry a distinctly different (likely higher) theater_ratio and extractiveness profile reflecting the cost of maintaining an incoherent bundle. All three share the same underlying historical material but are NOT the same constraint — each is evaluated by its own reading's lights per the ε-invariance principle, and none should be treated as more 'true' than the others within this framework; they are structurally distinct claims requiring separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
