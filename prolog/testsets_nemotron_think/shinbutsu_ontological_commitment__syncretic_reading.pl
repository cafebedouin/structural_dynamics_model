% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Unified Cosmological Order
 *   domain: religious/historical/ontological
 *
 * SUMMARY:
 *   Honji-suijaku (original ground, manifest trace) is the medieval Japanese
 *   Buddhist doctrine asserting that kami are local manifestations of
 *   universal buddhas and bodhisattvas. Formulated primarily by Tendai and
 *   Shingon institutions from the 9th century onward, it provided the
 *   theological architecture for shinbutsu-shūgō (kami-buddha amalgamation) —
 *   the temple-shrine complexes that dominated Japanese religious life until
 *   the 1868 Meiji separation edicts. The constraint is claimed as
 *   tangled_rope: it genuinely solved a coordination problem (integrating two
 *   heterogeneous religious systems into a stable social order) while
 *   asymmetrically extracting institutional authority and resources from
 *   Shinto practitioners for the benefit of the Buddhist hierarchy. The
 *   metrics describe a constraint whose extractiveness and suppression grew
 *   over a millennium as Buddhist institutional power consolidated, while
 *   theater ratio remained relatively low — the coordination function was
 *   real but increasingly overlaid with rent-seeking.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.62).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.71).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Unified Cosmological Order").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious/historical/ontological").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '9b26c3fc-595f-4f4e-a9ec-e60afdf4c541').
narrative_ontology:cs_kernel_codification('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', formalized).
narrative_ontology:cs_authority_grounding('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', lineage).
narrative_ontology:cs_interpretation_layer_present('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541').
narrative_ontology:cs_reading_relation('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', shinbutsu_ontological_commitment__incoherence_reading, influences).
narrative_ontology:cs_axiom('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', foundational, honji_suijaku_unified_cosmology).
narrative_ontology:cs_axiom_status(honji_suijaku_unified_cosmology, holdable).
narrative_ontology:cs_axiom_grounding('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', honji_suijaku_unified_cosmology, theological).
narrative_ontology:cs_axiom('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', foundational, buddhist_hierarchy_legitimate_authority).
narrative_ontology:cs_axiom_status(buddhist_hierarchy_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', buddhist_hierarchy_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', honji_suijaku_framework).
narrative_ontology:cs_drift_state('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', meiji_separation_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('9b26c3fc-595f-4f4e-a9ec-e60afdf4c541', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, lay_population).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, lay_population).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, competing_buddhist_sects).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, buddha_nature_universality).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, skillful_means_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, institutional_integration_through_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Esoteric Buddhist institutions (Tendai, Shingon) formulate and enforce honji-suijaku doctrine, positioning buddhas as original ground (honji) and kami as manifest traces (suijaku). They control ritual authorization, temple networks, and doctrinal interpretation, extracting patronage, land rights, and spiritual authority. Exit is arbitrage-grade: they can shift between sectarian lineages or retreat to pure Buddhist practice without losing institutional capital.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Shrine priests and local kami communities see their deities reinterpreted as manifestations of Buddhist figures, their rituals incorporated into Buddhist liturgical calendars, and their institutional autonomy subordinated to temple-shrine complexes (jingūji). They bear the cost of doctrinal subordination — loss of independent theological voice, ritual sovereignty, and patronage streams. Exit is constrained: abandoning kami practice means abandoning communal identity and ancestral obligations; resistance invites Buddhist institutional suppression.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_practitioners, payer,
    organized, biographical, constrained, regional).

% Ordinary people gain a unified cosmological framework that makes sense of both kami worship and Buddhist practice without contradiction — life-cycle rituals (kami) and afterlife salvation (buddhas) integrate coherently. They also bear diffuse costs: mandatory support for temple-shrine complexes, restricted access to 'pure' kami rites, and taxation for Buddhist institutional maintenance. Exit is constrained by social embeddedness and lack of alternative meaning-systems.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, lay_population, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, lay_population, payer).

% Non-esoteric sects (Pure Land, Zen, Nichiren) that reject or reinterpret honji-suijaku face marginalization in the institutional hierarchy dominated by Tendai/Shingon. They pay through exclusion from court patronage, temple registration systems, and legitimating discourse. Exit is mobile: they can develop independent lay followings, emphasize alternative doctrines, or relocate to regional strongholds.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, competing_buddhist_sects, payer,
    organized, generational, mobile, national).

% The court patronizes both Buddhist and Shinto institutions, using honji-suijaku as a stabilizing ideology that integrates diverse religious power centers under imperial legitimacy. It does not directly extract from the constraint but benefits from the social order it produces. Its analytical seat observes the constraint's operation without bearing its costs or collecting its rents.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent cosmological framework that integrates kami worship (this-worldly benefits, communal identity, life-cycle rituals) with Buddhist soteriology (afterlife salvation, universal liberation, monastic discipline), enabling institutional cooperation between shrines and temples and giving the population a unified meaning-system.
% TRANSFER_FUNCTION: Moves institutional authority, patronage resources, ritual control, and theological sovereignty from Shinto practitioners to the Buddhist hierarchy (especially Tendai and Shingon), while transferring cosmological coherence and ritual accessibility to the lay population.
% ABSENT_VOICES: Local kami communities without institutional representation, women's ritual specialists (miko) displaced by male Buddhist clergy, and emergent Shinto theological voices (later Yoshida/Ise Shinto) that would articulate kami autonomy — all structurally excluded from the honji-suijaku formulation process.
% DISAPPEARANCE_RATIONALE: If honji-suijaku vanished overnight, the medieval temple-shrine complex system would collapse: shrines would reclaim ritual independence, Buddhist institutions would lose their theological warrant for governing kami rites, the population would lose its unified cosmology, and the court would lose a key ideological integrator. The religious landscape would reorganize around separated or competing frameworks — as it did historically during Meiji shinbutsu bunri.
% FOUNDING_PROBLEM: How to integrate the indigenous kami cults — deeply embedded in local community life, agriculture, and imperial legitimacy — with the imported Buddhist soteriology that claimed universal truth, without triggering destructive conflict or requiring populations to abandon either system.
% FOUNDING_PROBLEM_CORROBORATION: Medieval Tendai/Shingon texts (e.g., honjaku-suijaku treatises) attest the integration project as deliberate doctrinal engineering. Modern scholarship outside the Buddhist tradition (e.g., Kuroda Toshio, Fabio Rambelli) corroborates that the founding problem — integration of two heterogeneous religious systems — was substantially solved by honji-suijaku, but that the solution became a structure of extraction. The Meiji state's 1868 separation policy explicitly treated the integration as a solved problem whose institutional form had become obsolete.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the Buddhist hierarchy's capture of shrine patronage, land, and ritual control beyond what the coordination function required. Suppression (0.71) is high because the doctrine's persistence depended on active institutional enforcement: temple control of shrine rites, doctrinal policing of kami interpretations, and state-backed temple registration systems that made honji-suijaku the only legitimate framework. Theater ratio (0.28) is moderate — the coordination function (unified cosmology, ritual integration) remained genuinely functional throughout, but a growing share of institutional activity served extraction rather than integration. Accessibility collapse (0.58) is mid-range: alternative frameworks (Yoshida Shinto, Ise Shinto, nativist kokugaku) emerged but were marginalized until the Meiji rupture. Resistance (0.45) reflects periodic Shinto institutional pushback and competing Buddhist sect resistance, but never enough to destabilize the constraint until state intervention.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist hierarchy's seat, honji-suijaku is a compassionate skillful means (upaya) that brings kami worshippers to the Buddha's teaching — genuine coordination. From Shinto practitioners' seat, it is a theological colonization that subordinates their deities and institutions — enforced extraction. From the lay population's seat, it is a convenient unity that makes religious life coherent — net benefit with diffuse costs. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist hierarchy (agenda_setter, institutional power, arbitrage exit) sits at the beneficiary pole (d ~ 0.15): it sets the doctrinal terms, collects the rents, and can exit into pure Buddhist practice. Shinto practitioners (payer, organized power, constrained exit) sit at the target pole (d ~ 0.85): they bear subordination costs, have limited exit (communal identity binds them), and face suppression for resistance. Lay population (beneficiary/payer, moderate power, constrained exit) sits near symmetric (d ~ 0.5): genuine coordination benefit offset by diffuse extraction. Competing Buddhist sects (payer, organized power, mobile exit) sit at d ~ 0.65: they pay exclusion costs but can develop alternative followings. The imperial court (observer, institutional power, analytical exit) sits at d ~ 0.0: it observes without bearing costs or collecting rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (integrating kami and Buddhist systems) was live in the 9th-10th centuries but dead by the 12th century — the integration was achieved. Yet the constraint persisted for six more centuries, extracting increasing rents from Shinto autonomy while its coordination function became routine maintenance. This is classic mandatrophy: the arrangement outlived its founding problem, and the Buddhist hierarchy's interest in maintaining it shifted from solving the integration problem to preserving the extraction stream. The Meiji separation (1868) was the exogenous shock that finally resolved the mandatrophy by forcibly dissolving the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'Is this constraint one reading of a contested kernel (shinbutsu_ontological_commitment) rather than a standalone constraint?',
    'Cross-reading comparison: if partition_reading and incoherence_reading produce structurally distinct constraints with different ε, beneficiaries, victims, and types, the kernel framing is validated. The committer structure is irreducible to a single constraint''s metrics.',
    'If validated, the three readings form a constraint family linked by network.affects_constraints. The syncretic_reading''s ε=0.62 (tangled_rope) would contrast with partition_reading''s likely lower ε (rope or mountain) and incoherence_reading''s near-zero ε (mountain or piton). Classification divergence across readings would be the signal, not noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the syncretic reading is one element of a kernel family rather than an independent constraint.').

omega_variable(
    partition_reading_relation,
    'Does the syncretic reading foreclose the partition reading, or do they coexist as live positions?',
    'Historical analysis: honji-suijaku was an elite/esoteric doctrine; partition (separate domains) was the popular/lay framework. They operated at different social levels simultaneously. If a single party could hold both (esoteric honji-suijaku for initiates, partition for laity), they coexist. If honji-suijaku''s universal claim logically eliminates partition''s domain separation, it forecloses.',
    'If forecloses: reading_relations = forecloses; the kernel has a logical hierarchy. If coexists_with: reading_relations = coexists_with; the kernel supports stable pluralism. The engine''s cross-reading contamination analysis depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_reading_relation, conceptual, 'Structural relationship between syncretic and partition readings of the same kernel.').

omega_variable(
    incoherence_reading_relation,
    'Does the syncretic reading foreclose the incoherence reading, or does it influence it?',
    'Historiographical analysis: incoherence_reading is a modern scholarly claim (Kuroda, Rambelli) that honji-suijaku was post-hoc rationalization, not a lived ontology. Syncretic_reading is the historical actors'' claim. They operate at different epistemic levels (emic vs etic). A medieval actor could not hold both; a modern scholar can analyze both. The relation is likely influences: the syncretic reading''s historical dominance creates the object the incoherence reading critiques.',
    'If influences: reading_relations = influences; the syncretic reading''s institutional success creates the evidence base the incoherence reading reinterprets. If forecloses: the syncretic reading''s claim to stable ontology would logically eliminate the incoherence reading''s claim — but they are different types of claims (historical vs historiographical).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incoherence_reading_relation, conceptual, 'Structural relationship between the historical actors'' reading (syncretic) and the modern scholarly reading (incoherence).').

omega_variable(
    extraction_coordination_boundary,
    'Where does the genuine coordination function end and the extraction begin in honji-suijaku''s historical operation?',
    'Comparative institutional analysis: measure shrine autonomy, patronage flows, and ritual control before/after honji-suijaku adoption at specific temple-shrine complexes. If shrines retained substantial autonomy and the doctrine only governed cosmological interpretation, coordination dominates. If shrine resources and authority were systematically transferred to temples, extraction dominates.',
    'If coordination dominates at early period (ε ~ 0.35) and extraction accumulates later (ε ~ 0.62), the constraint underwent mandatropy — supporting the tangled_rope classification with temporal drift. If extraction was high from inception, the coordination claim is cover story — snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, empirical, 'Whether honji-suijaku''s coordination function was genuine or a cover for extraction from the start.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_tr_t800, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t950, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 950, 0.15).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1100, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1100, 0.2).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1250, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1250, 0.22).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1400, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1400, 0.25).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1550, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1550, 0.27).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1700, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1700, 0.28).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1868, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1868, 0.28).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_be_t800, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 800, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_be_t950, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 950, 0.45).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1100, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1100, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1250, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1250, 0.58).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1400, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1400, 0.6).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1550, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1550, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1700, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1700, 0.61).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1868, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1868, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_su_t800, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 800, 0.4).
narrative_ontology:measurement(shinbutsu_syncretic_su_t950, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 950, 0.5).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1100, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1100, 0.6).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1250, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1250, 0.65).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1400, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1400, 0.68).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1550, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1550, 0.7).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1700, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1700, 0.71).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1868, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1868, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__syncretic_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint (syncretic_reading) decomposes the shinbutsu_ontological_commitment kernel with partition_reading and incoherence_reading. The syncretic reading has ε=0.62 (tangled_rope) with high institutional integration; partition_reading likely has lower ε (rope/mountain) with domain separation; incoherence_reading likely has near-zero ε (mountain/piton) as a historiographical claim. All three share the kernel but instantiate different constraints with different ε — per ε-invariance principle, they are separate stories linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_commitment__syncretic_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
