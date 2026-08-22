% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Honji-Suijaku Ontological Fusion (Buddhist Reading)
 *   domain: religious/philosophical
 *
 * SUMMARY:
 *   From the 9th century onward, Japanese Buddhism absorbed Shinto kami into
 *   its doctrinal framework through honji-suijaku theory: kami are held to be
 *   manifestations (suijaku) of underlying Buddhist deities (honji). This
 *   reading — the ontological_fusion_reading — asserts that the honji-suijaku
 *   doctrine captures genuine metaphysical truth: kami and buddhas are
 *   ontologically identical beings accessed through different cultural
 *   lenses. Under this reading, simultaneous veneration is not compromise or
 *   incoherence but correct perception of reality. The constraint's
 *   structure, however, reveals asymmetry: Buddhist institutions monopolize
 *   interpretive authority, indigenous kami-only veneration becomes
 *   incoherent or heretical under the fusion frame, and kami are subordinated
 *   to Buddhist ontology. The reading is one of three contestable
 *   interpretations of the simultaneous veneration kernel; the sibling
 *   readings (domain_partition and pragmatic_incoherence) assert different
 *   structural truths about what honji-suijaku is and how it should be
 *   classified.
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy: interprets honji-suijaku doctrine, maintains the fusion frame, benefits from monopoly over kami meaning-making
 *   - indigenous kami practitioners/shrines: traditionally maintained autonomous kami veneration; under fusion reading, their autonomy is recast as misunderstanding or incomplete perception
 *   - scholar-monks and doctrinal authorities: author and defend the fusion thesis as metaphysical truth, not mere accommodation
 *   - lay practitioners: navigate simultaneous veneration under the fusion frame, may internalize the ontological hierarchy
 *   - Meiji-era reformers: later forcibly separate the constraint when state ideology requires kami as purely Japanese/Shinto
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.78).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.71).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion (Buddhist Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious/philosophical").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'd3cccd6b-0d2d-49e5-8529-5adef49140dd').
narrative_ontology:cs_kernel_codification('d3cccd6b-0d2d-49e5-8529-5adef49140dd', formalized).
narrative_ontology:cs_authority_grounding('d3cccd6b-0d2d-49e5-8529-5adef49140dd', lineage).
narrative_ontology:cs_interpretation_layer_present('d3cccd6b-0d2d-49e5-8529-5adef49140dd').
narrative_ontology:cs_reading_relation('d3cccd6b-0d2d-49e5-8529-5adef49140dd', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('d3cccd6b-0d2d-49e5-8529-5adef49140dd', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('d3cccd6b-0d2d-49e5-8529-5adef49140dd', foundational, kami_buddhist_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddhist_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('d3cccd6b-0d2d-49e5-8529-5adef49140dd', kami_buddhist_ontological_identity, deontological).
narrative_ontology:cs_axiom('d3cccd6b-0d2d-49e5-8529-5adef49140dd', foundational, honji_suijaku_metaphysical_truth).
narrative_ontology:cs_axiom_status(honji_suijaku_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('d3cccd6b-0d2d-49e5-8529-5adef49140dd', honji_suijaku_metaphysical_truth, conventional).
narrative_ontology:cs_reference_frame('d3cccd6b-0d2d-49e5-8529-5adef49140dd', kami_buddha_ontological_unity).
narrative_ontology:cs_drift_state('d3cccd6b-0d2d-49e5-8529-5adef49140dd', meiji_separation_pressure, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('d3cccd6b-0d2d-49e5-8529-5adef49140dd', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, lay_simultaneous_practitioners).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, doctrinal_scholar_monks).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, kami_shrine_priests).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, lay_simultaneous_practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, kami_shrine_priests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the honji-suijaku doctrine as Buddhist orthodoxy. Trains priests in the fusion frame, educates lay practitioners through temples and doctrinal texts, and adjudicates disputes about correct understanding. Benefits from the fusion by consolidating religious authority and incorporating kami veneration into Buddhist institutional control. Can reframe, reformulate, or reinterpret doctrine with minimal institutional cost; exit means changing the doctrine, not leaving the system.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Traditionally maintained autonomous kami veneration within shrines and local religious practice. Under the ontological_fusion reading, their kami are recast as manifestations of underlying Buddhist deities; kami autonomy becomes heretical or theologically naive. They must either accept the fusion frame and subordinate their tradition, or resist and be marginalized from mainstream religious authority. Exit from the constraint means abandoning their religious identity or migrating to rival kami-only traditions (limited availability outside mainstream Shinto revival).
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_practitioners, payer,
    moderate, generational, identity_locked, regional).

% Engage in simultaneous veneration of kami and buddhas without formal doctrinal training. Benefit from having the fusion frame available to make sense of their practice—it reduces cognitive dissonance and provides institutional legitimacy. Also bear the cost of the ontological hierarchy: they learn that kami are subordinate and their autonomous kami practice is incomplete. Their exit is constrained by cultural embeddedness; leaving simultaneous veneration means either specializing in one tradition or practicing incoherently.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, lay_simultaneous_practitioners, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, lay_simultaneous_practitioners, payer).

% Author and defend the honji-suijaku doctrine as metaphysical truth, not mere institutional accommodation. They benefit from the fusion by having a coherent and sophisticated theological framework to defend, and by gaining prestige as interpreters of this synthesis. They can, if needed, argue for an alternative reading or reinterpretation of the doctrine, giving them mobility within the system.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, doctrinal_scholar_monks, beneficiary,
    powerful, generational, mobile, national).

% Manage kami shrines and perform rituals. Under the fusion frame, their kami are subordinate to Buddhist deities; shrine autonomy is compromised by the institutional demand to accept the hierarchical ontology. They benefit from having mainstream religious legitimacy through acceptance of the fusion doctrine. They are constrained by institutional pressure and regional dependence on Buddhist temple networks; exit means abandoning shrine priesthood or mounting effective resistance (historically difficult).
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, kami_shrine_priests, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, kami_shrine_priests, beneficiary).

% Later (outside this interval's endpoint) forcibly separate Shinto from Buddhism to align with modernization ideology. They would object to the fusion frame if present in the conversation, arguing instead for Shinto as pure indigenous religion. Their exclusion during the pre-Meiji period means the fusion constraint persists without facing this institutional challenge; the separation removes the constraint entirely when the state's power intervenes.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, meiji_state_reformers, excluded,
    institutional, biographical, analytical, national).

% Examines the constraint from outside any participating tradition. Documents the structure, measures extractiveness, and tracks how different seats experience the fusion frame differently. Neither benefits nor bears direct cost; seat exists to enable structural analysis.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(simultaneous_veneration__ontological_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent theological framework for simultaneous kami and buddha veneration: the honji-suijaku fusion explains how both can coexist without contradiction by asserting they are ontologically identical beings viewed through different cultural perspectives. Solves the coordination problem of maintaining a unified religious system that honors both traditions without requiring practitioners to hold incommensurable beliefs.
% TRANSFER_FUNCTION: Transfers interpretive monopoly from indigenous kami practitioners to the Buddhist institutional hierarchy. Moves authority over kami meaning from shrines and local practice to Buddhist temples and doctrinal authorities. Moves legitimacy from kami-autonomous veneration to fusion-mediated veneration. Practitioners transfer assent to the honji-suijaku frame in exchange for having their simultaneous practice explained and legitimized.
% ABSENT_VOICES: Kami-only practitioners who would argue for kami autonomy and reject the subordination implied by the fusion. Meiji-era reformers and Shinto revivalists who would later argue for kami as purely indigenous and non-Buddhist. Practitioners who find the fusion incoherent and would prefer to hold contradictory beliefs explicitly rather than accepting a false synthesis. Alternative Buddhist readings that would partition domains rather than fuse them.
% DISAPPEARANCE_RATIONALE: From the Buddhist institutional perspective, if the honji-suijaku fusion disappeared, Buddhist authority over kami meaning would collapse and the institutional integration of kami veneration into Buddhism would dissolve—kami would revert to autonomous religious actors. From the indigenous kami practitioners' perspective, if the fusion disappeared, their autonomy would be restored and simultaneous veneration would require no longer accepting the ontological hierarchy. The Meiji separation (outside the interval) actually removes the constraint by state power, causing the world to reorganize: Shinto becomes separate from Buddhism, kami revert to Shinto autonomy, and the forced fusion ends. The verdict is contested because the Buddhist hierarchy argues the fusion is metaphysically necessary (world would be disordered without it), while critics argue it is institutional capture (world reorganizes successfully once removed).
% FOUNDING_PROBLEM: From the 9th century onward, Japanese Buddhism encountered indigenous kami veneration as a religious practice that could not be absorbed into Buddhist cosmology without theological revision. The problem: how can kami, pre-Buddhist indigenous deities, coexist with Buddhist deities in a single coherent system? Initial solution: reframe kami as manifestations (suijaku) of underlying Buddhist deities (honji), restoring coherence while maintaining both practices. The honji-suijaku theory emerged to solve this coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist doctrinal authorities and institutional historians attest the founding problem is still live—simultaneous veneration persists and requires the fusion frame to remain coherent. Kami practitioners and later Shinto revivalists attest the problem was never real—kami autonomy was always viable and the fusion was institutional capture, not necessary synthesis. Contemporary comparative religion scholars document both the coherence benefits of the fusion frame and the power asymmetry it encoded. The Meiji separation (1868+) provides external corroboration that the founding problem could be resolved WITHOUT the fusion—by simply separating the traditions—suggesting the problem was institutional choice, not metaphysical necessity.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, contested).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint enforces a specific metaphysical reading against alternatives that would preserve kami autonomy. The Buddhist hierarchy benefits from exclusive interpretive authority (beneficiary status); indigenous kami practitioners lose the ability to claim kami as ontologically independent beings (victim status). Suppression (0.71) is substantial because maintaining the fusion frame requires suppressing—through institutional authority, doctrinal enforcement, and eventually internalization—the alternative readings and the concept of kami autonomy. Theater ratio (0.42, rising) reflects that enforcement increasingly depends on doctrinal performance (maintaining the coherence of the fusion, training priests in the correct reading) rather than pure institutional force. Accessibility_collapse (0.68) is moderate-high because the fusion frame constrains the alternative interpretations available to practitioners once they are embedded in the Buddhist institutional system, but pre-fusion kami veneration remains historically available as a contrast. Resistance (0.55) is moderate because throughout the period, rival kami practitioners and later Meiji reformers mount real opposition to the fusion, though enforcement maintains it until the Meiji separation (1868+) removes the constraint entirely.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist institutional seat, the honji-suijaku fusion is a genuine metaphysical discovery that correctly explains the nature of kami and buddhas; it is a coordination mechanism that permits both traditions to operate within a single coherent framework. From the indigenous kami practitioners' seat, the same structure is extractive: their autonomous kami are reinterpreted as subordinate manifestations, their interpretive authority is erased, and their only option is to accept the Buddhist frame or be marginalized. The engine will compute these as sharply divergent type classifications per seat; the authored claim (tangled_rope) reflects the fusion reading's self-description (genuine coordination + asymmetric subordination), not a prediction of what the engine will compute.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy (institutional power, high exit via arbitrage—they can always reinterpret or reformulate doctrine, and they benefit from the framework) is the structured beneficiary: they gain monopoly over kami meaning and consolidate religious authority. Directionality: d ≈ 0.2 (beneficiary end). Indigenous kami practitioners (ranging from moderate institutional shrine power to powerless independent devotees, trapped or identity-locked exit—their religious identity is constituted through kami veneration and leaving means spiritual displacement) are the targets: they must either accept the fusion frame or be labeled heretical. Directionality for powerless kami devotees: d ≈ 0.85 (target end). Directionality for organized shrine priests: d ≈ 0.65 (still targets, but with more institutional resources and room for negotiation within the frame). Lay practitioners sit between: they benefit from having a coherent framework for simultaneous veneration, but they also bear the cognitive cost of the ontological hierarchy and the implicit delegitimization of kami autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophic at the endpoint of its interval (time 10), because the founding problem (how to coherently accommodate kami veneration within Buddhist institutions) is still live and the honji-suijaku frame is still the official Buddhist teaching. However, the Meiji separation (outside this interval) makes the constraint's mandate obsolete: the state's decision to separate kami (Shinto) from Buddhism (religious status) removes the institutional pressure to maintain the fusion, and the constraint's persistence becomes theatrical—doctrinal recitation without institutional necessity. The measurement series shows rising theater_ratio (0.25 → 0.42), suggesting the constraint's functional extraction is increasingly sustained by performative doctrine-maintenance rather than by active institutional enforcement. This is a precursor signal to mandatrophy, not mandatrophy itself, within the interval; the actual transition occurs at the Meiji separation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_pragmatic_unity,
    'Is the honji-suijaku fusion a genuine metaphysical claim about the nature of kami and buddhas, or a pragmatic accommodation that conceals incommensurable beliefs held simultaneously?',
    'Examination of pre-Meiji doctrinal texts, practitioner testimony, and institutional enforcement records: if the fusion was defended as logically necessary, it is ontological; if it was defended as culturally workable without logical resolution, it is pragmatic.',
    'If pragmatic (not genuinely ontological), the constraint reclassifies from tangled_rope (enforced coordination + asymmetric extraction of kami autonomy) to snare (pure extraction using incoherence as cover). If genuinely ontological within Buddhist epistemology, the extraction persists but the reading''s legitimacy claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_pragmatic_unity, conceptual, 'Whether honji-suijaku is a true metaphysical theory or pragmatic incoherence under institutional enforcement.').

omega_variable(
    beneficiary_capture_mechanism,
    'Did the Buddhist institutional hierarchy deliberately construct the honji-suijaku fusion to monopolize kami veneration and extract institutional authority from indigenous religion, or did the fusion emerge from genuine theological synthesis?',
    'Historical analysis of doctrinal development, political context of institutional expansion, and timing of formal codification relative to institutional power consolidation.',
    'If deliberately constructed extraction (capture), the reading''s epistemic warrant erodes and the constraint becomes a case study in false natural law via doctrinal capture. If genuine theological synthesis, the extraction asymmetry persists but the fusion is not falsified by its institutional utility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_mechanism, empirical, 'Whether the fusion doctrine reflects genuine theological synthesis or institutional capture of indigenous autonomy.').

omega_variable(
    reading_incommensurability,
    'Can the ontological_fusion_reading, domain_partition_reading, and pragmatic_incoherence_reading coexist as three different but compatible readings of the same kernel, or does adopting one logically foreclose the others?',
    'Formal analysis of the axioms: if each reading''s foundational claim contradicts the others'' (e.g., ''kami are identical to buddhas'' forecloses ''kami and buddhas are ontologically distinct''), they foreclose; if each describes a different coherent position on what honji-suijaku is, they coexist.',
    'If they foreclose, the kernel decision is binary or ternary (one reading wins); if they coexist, institutional drift permits all three to circulate, and the constraint is less stable than a single-reading story implies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether the three kernel readings are logically incompatible or able to circulate as different institutional positions.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.71) structural (external enforcement preventing kami-only veneration) or internalized (practitioners have absorbed the honji-suijaku frame so thoroughly they no longer perceive kami autonomy as an option)?',
    'Post-Meiji separation and contemporary Shinto revival movements: if suppression persists after the external enforcement apparatus dissolves, the mechanism is partially internalized.',
    'If internalized, the effective suppression exceeds the structural measure; the constraint carries its own maintenance through ideology, not just institutional force. Exit becomes cognitively harder than the structural barriers alone would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of kami autonomy is structural or internalized in practitioner epistemology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(simu_tr_t2, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(simu_tr_t4, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(simu_tr_t6, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(simu_tr_t8, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 8, 0.39).
narrative_ontology:measurement(simu_tr_t10, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(simu_be_t2, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 2, 0.68).
narrative_ontology:measurement(simu_be_t4, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 4, 0.71).
narrative_ontology:measurement(simu_be_t6, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 6, 0.74).
narrative_ontology:measurement(simu_be_t8, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 8, 0.76).
narrative_ontology:measurement(simu_be_t10, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(simu_su_t2, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 2, 0.61).
narrative_ontology:measurement(simu_su_t4, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(simu_su_t6, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 6, 0.67).
narrative_ontology:measurement(simu_su_t8, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 8, 0.69).
narrative_ontology:measurement(simu_su_t10, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 10, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__ontological_fusion_reading, 0.12).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the simultaneous_veneration kernel. The ontological_fusion reading asserts that honji-suijaku doctrine captures genuine metaphysical truth. Sibling readings frame the same historical phenomenon differently: domain_partition reframes it as functional specialization (no ontological claim); pragmatic_incoherence reframes it as incommensurable beliefs held simultaneously without logical resolution. The three readings share the same kernel (the historical practice of simultaneous veneration) but instantiate different constraints (different ε, different beneficiary/victim structures, different classifications). All three must be authored to fully model the kernel's contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__ontological_fusion_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
