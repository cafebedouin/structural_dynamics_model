% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion Reading
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   The honji-suijaku (original ground / manifest traces) theory is the
 *   classical Japanese Buddhist doctrine asserting that kami (Shinto deities)
 *   are local manifestations of universal buddhas and bodhisattvas. This
 *   reading claims ontological identity: kami and buddhas are the same beings
 *   viewed through different cultural lenses. The theory provided the
 *   metaphysical infrastructure for over a millennium of Japanese religious
 *   syncretism (shinbutsu-shūgō), allowing simultaneous veneration at
 *   shrine-temple complexes. However, the framework was asymmetrical:
 *   Buddhist institutions claimed the exclusive authority to identify which
 *   buddha was the 'original ground' of each kami, thereby subordinating
 *   indigenous traditions to Buddhist interpretive monopoly while extracting
 *   ritual authority, economic resources, and political legitimacy. The
 *   constraint persisted through active enforcement — doctrinal polemics,
 *   ritual incorporation, and institutional pressure — until the Meiji state
 *   forcibly dismantled it in 1868.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.75).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.7).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Reading").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2').
narrative_ontology:cs_kernel_codification('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', fixed_text).
narrative_ontology:cs_authority_grounding('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', lineage).
narrative_ontology:cs_interpretation_layer_present('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2').
narrative_ontology:cs_reading_relation('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', foundational, kami_buddha_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', kami_buddha_ontological_identity, theological).
narrative_ontology:cs_axiom('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', secondary, buddhist_interpretive_monopoly).
narrative_ontology:cs_axiom_status(buddhist_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', buddhist_interpretive_monopoly, conventional).
narrative_ontology:cs_reference_frame('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', heian_syncretic_equilibrium).
narrative_ontology:cs_drift_state('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', meiji_separation_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('c33d5e2f-35d6-4bc6-8f01-6185bc24a1c2', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, buddha_nature_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the authoritative interpretation of honji-suijaku theory through monastic lineages, temple networks, and doctrinal texts. Claims interpretive monopoly over the kami-buddha relationship, using the framework to incorporate local kami cults into Buddhist institutional structures while extracting ritual authority, land holdings, and lay patronage. Exit is arbitrage-grade: they can reformulate the doctrine or emphasize other teachings without losing institutional position.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, beneficiary).

% Local shrine priesthoods and kami-centered ritual communities whose autonomous traditions are reinterpreted as manifestations of Buddhist deities. Their distinct cosmologies, ritual calendars, and authority structures are subordinated to the honji-suijaku framework. Exit is constrained: they can resist through esoteric preservation or syncretic adaptation, but the Buddhist interpretive monopoly controls the legitimate discourse.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_tradition, payer,
    organized, generational, constrained, national).

% Ordinary worshippers who gain a coherent metaphysical framework allowing simultaneous veneration of kami and buddhas without cognitive dissonance or social penalty. They receive ritual convenience and spiritual economy: one visit serves both traditions. Exit is mobile: they can shift devotional focus between shrines and temples fluidly, though the framework shapes what options appear legitimate.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, lay_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% State-builders who later dismantled the honji-suijaku system through the 1868 Shinbutsu Bunri (Separation of Kami and Buddhas) edicts. They were structurally excluded from the honji-suijaku discourse — their project required destroying the constraint, not participating in it. Their exclusion was the enforcement mechanism's blind spot: the constraint could not anticipate its own political obsolescence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, meiji_reformers, excluded,
    powerful, biographical, trapped, national).

% Academic observers who analyze the honji-suijaku framework as a historical syncretic mechanism. They see the full structural asymmetry: Buddhist institutional benefit, kami tradition subordination, lay convenience, and the constraint's eventual collapse under modern state pressure. Their analytical exit is complete — they bear no devotional cost and face no institutional penalty for their reading.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified metaphysical framework allowing simultaneous veneration of kami and buddhas without contradiction, resolving the tension between indigenous Japanese religiosity and imported Buddhist doctrine by declaring them ontologically identical.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual control, land endowments, and lay patronage from autonomous kami traditions to the Buddhist institutional hierarchy, which claims the exclusive right to define the 'true nature' (honji) of each kami.
% ABSENT_VOICES: Indigenous kami priesthoods and local shrine traditions that were subordinated into the honji-suijaku framework; they would object to the ontological subsumption of their deities as mere 'manifest traces' (suijaku) of Buddhist originals, but were incorporated before they could articulate a separate institutional voice.
% DISAPPEARANCE_RATIONALE: The honji-suijaku framework structured over a millennium of Japanese religious practice, temple-shrine complexes, pilgrimage routes, and devotional economies. Its removal via Meiji-era Shinbutsu Bunri triggered violent reorganization: shrine-temple complexes were forcibly separated, Buddhist artifacts destroyed, kami priesthoods reconstituted under state Shinto, and the entire ritual landscape restructured.
% FOUNDING_PROBLEM: How to reconcile indigenous Japanese kami veneration with imported Buddhist doctrine without forcing conversion, cultural erasure, or perennial sectarian conflict — a live problem from the 6th through 19th centuries as Buddhism sought legitimacy in Japan.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from both Buddhist monastic chronicles and Shinto shrine documents document the syncretic development as a deliberate institutional strategy. Modern scholars outside both traditions (e.g., Kuroda Toshio, Teeuwen & Rambelli) confirm the political utility of the framework for Buddhist institutional expansion and state formation. The problem is dead because the Meiji separation resolved it by state fiat, though the reading persists in some Buddhist traditions.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the Buddhist hierarchy claims interpretive monopoly over the entire kami pantheon, converting autonomous traditions into subsidiaries. Suppression is substantial (0.7) because the framework's persistence depended on actively marginalizing kami-centered cosmologies and controlling the discourse of legitimacy. Theater ratio is moderate (0.4): the coordination function (enabling simultaneous veneration without cognitive dissonance) is genuine, but a growing share of doctrinal elaboration served institutional extraction rather than lay convenience. The measurement series track the constraint's intensification from early syncretic accommodation (low extraction, low theater) to mature institutional dominance (high extraction, rising theater), with suppression requirements escalating as kami traditions developed counter-narratives.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist hierarchy's seat, the constraint is a rope: genuine coordination solving the Buddhism-in-Japan problem. From the kami tradition's seat, it is a snare: ontological erasure enforced by interpretive monopoly. From the lay practitioner's seat, it is a rope with extractive drift: convenient coordination that gradually accumulated institutional rent. The engine computes this divergence from the structural data; the claimed type (tangled_rope) acknowledges the coordination function while the metrics capture the extraction asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy sits at the beneficiary end (d near 0.0): they collect interpretive rents, control the framework's application, and face arbitrage-grade exit (can reformulate doctrine). Indigenous kami traditions sit at the target end (d near 1.0): they bear the cost of ontological subordination, face constrained exit (resistance means marginalization), and lack independent interpretive authority. Lay practitioners sit near symmetric (d ~ 0.5): genuine coordination benefit (unified devotional field) balanced against diffuse indirect cost (the framework shapes what appears legitimate). Meiji reformers were excluded (d undefined — they were the constraint's external negation). Modern scholars are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling Buddhism with indigenous religiosity) was live for centuries but died with the Meiji separation. The constraint persisted past its founding problem's death — classic mandatrophy — because the Buddhist hierarchy extracted sufficient benefit to maintain it, and the kami traditions lacked exit power to dismantle it. The Meiji state provided the external shock that resolved the mandatrophy by force. The constraint is now historically inert but persists as a doctrinal claim in some Buddhist traditions (a piton residue).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural classification change if the sibling readings are considered as alternative framings of the same historical phenomenon?',
    'Cross-reading comparison: compute χ for each reading''s constraint story using their respective beneficiary/victim declarations and metrics; the kernel-level pattern emerges from the distribution of computed types across readings.',
    'If domain_partition_reading computes as mountain/rope (low extraction) and pragmatic_incoherence_reading computes as snare (high extraction, no coordination), the ontological_fusion_reading''s tangled_rope classification reveals it as the specific historical mechanism that fused coordination with extraction — the ''how it actually worked'' reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer frame: this constraint is one reading of the simultaneous_veneration kernel; sibling readings are separate constraints with different ε and structural data.').

omega_variable(
    ontological_claim_vs_institutional_strategy,
    'Is the ontological identity claim (honji-suijaku) a genuine metaphysical conviction or an institutional strategy retrospectively theologized?',
    'Internal textual evidence: track whether monastic authors treat the identity as a discovered truth (citing meditative realization, scriptural authority) versus a pragmatic accommodation (citing political expediency, lay convenience). Cross-reference with institutional records of land grants, priestly appointments, and ritual incorporations.',
    'If genuine conviction, the coordination function is primary and extraction is secondary drift; if institutional strategy, extraction is primary and coordination is the cover story. Changes the constraint''s moral valence and the weight of the victim claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_claim_vs_institutional_strategy, conceptual, 'Whether the metaphysical claim is the cause or the rationalization of the institutional arrangement.').

omega_variable(
    kami_agency_in_syncretism,
    'Did kami traditions exercise agency in adopting the honji-suijaku framework, or was it imposed unilaterally by Buddhist institutions?',
    'Comparative analysis of shrine documents (engi, ruiki) versus temple records: look for kami-side authorship of honji-suijaku identifications, negotiated equivalences, and instances where kami traditions leveraged Buddhist patronage for their own ends.',
    'If kami traditions actively negotiated the equivalences, the victim claim weakens — the arrangement becomes a bargained coordination with asymmetric power, not pure subordination. If imposed unilaterally, the snare/tangled_rope extraction profile strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kami_agency_in_syncretism, empirical, 'Degree of kami-side agency in the construction of the honji-suijaku mapping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(simu_tr_t200, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 200, 0.2).
narrative_ontology:measurement(simu_tr_t400, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 600, 0.3).
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 800, 0.35).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1000, 0.38).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1200, 0.4).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(simu_be_t200, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(simu_be_t400, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 400, 0.65).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 600, 0.7).
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 800, 0.72).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1000, 0.74).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1200, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(simu_su_t200, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 200, 0.45).
narrative_ontology:measurement(simu_su_t400, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 400, 0.55).
narrative_ontology:measurement(simu_su_t600, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 600, 0.6).
narrative_ontology:measurement(simu_su_t800, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1000, 0.68).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1200, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__ontological_fusion_reading, 0.08).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single label 'simultaneous veneration of kami and buddhas' into three structurally distinct constraints with different ε values, beneficiary/victim structures, and classifications. The ontological_fusion_reading is the historically dominant mechanism (tangled_rope); domain_partition_reading is the Shinto counter-theology (rope/mountain); pragmatic_incoherence_reading is the modern critical reading (snare). All three share the kernel_id 'simultaneous_veneration'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__ontological_fusion_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
