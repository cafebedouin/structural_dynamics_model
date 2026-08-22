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
 *   human_readable: Honji-Suijaku Ontological Fusion Theory
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   Honji-suijaku (original ground / trace manifestation) theory asserts that
 *   kami are local Japanese manifestations of universal buddhas —
 *   ontologically identical beings viewed through different cultural lenses.
 *   This reading, dominant from Heian through Edo, subordinates indigenous
 *   kami autonomy to Buddhist interpretive monopoly. The constraint
 *   coordinates a unified religious field while extracting institutional
 *   primacy, land, and hermeneutic authority from kami cults. Active
 *   enforcement is required: rival interpretations (kami independence,
 *   dual-practice without subordination) are suppressed through doctrinal
 *   policing, temple-shrine administrative control, and the institutional
 *   weight of the syncretic system. The claimed type is tangled_rope: genuine
 *   coordination (unified devotional field) fused with asymmetric extraction
 *   (Buddhist hierarchy over kami autonomy).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.78).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.72).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Theory").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'fe697ae5-bd12-4dd8-b0c8-5920e6e42374').
narrative_ontology:cs_kernel_codification('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', fixed_text).
narrative_ontology:cs_authority_grounding('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', lineage).
narrative_ontology:cs_interpretation_layer_present('fe697ae5-bd12-4dd8-b0c8-5920e6e42374').
narrative_ontology:cs_reading_relation('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', simultaneous_veneration__pragmatic_incoherence_reading, influences).
narrative_ontology:cs_axiom('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', foundational, kami_are_suijaku_of_honji_buddhas).
narrative_ontology:cs_axiom_status(kami_are_suijaku_of_honji_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', kami_are_suijaku_of_honji_buddhas, theological).
narrative_ontology:cs_axiom('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', foundational, buddhist_interpretive_authority_over_kami).
narrative_ontology:cs_axiom_status(buddhist_interpretive_authority_over_kami, holdable).
narrative_ontology:cs_axiom_grounding('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', buddhist_interpretive_authority_over_kami, conventional).
narrative_ontology:cs_reference_frame('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', honji_suijaku_orthodoxy).
narrative_ontology:cs_drift_state('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', meiji_restoration, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('fe697ae5-bd12-4dd8-b0c8-5920e6e42374', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, lay_practitioners).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, imperial_court).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, buddhist_interpretive_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the doctrinal interpretation of kami as local manifestations (suijaku) of universal buddhas (honji). Monopolizes ritual authority, temple-shrine administration, and the hermeneutic framework that subordinates indigenous worship to Buddhist soteriology. Extracts legitimacy, land patronage, and institutional primacy from the fusion claim.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Bears the cost of ontological subordination: kami lose independent agency, mythic particularity, and ritual sovereignty. Their cults are reorganized under Buddhist liturgical forms, their priesthoods displaced or subordinated, their narratives rewritten as skillful means. Exit is constrained by the institutional weight of the syncretic system and the social capital tied to participation.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy, payer,
    moderate, biographical, constrained, local).

% Gain a unified religious field where this-worldly benefits (kami) and afterlife salvation (buddhas) are accessible in one devotional act. Pay through ritual conformity, material offerings, and the cognitive labor of maintaining contradictory ontological commitments. Can exit locally by shifting devotional focus, but the structural fusion is inescapable at the communal level.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, lay_practitioners, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, lay_practitioners, payer).

% Practitioners of autonomous kami worship who are structurally excluded from doctrinal authority. Their insistence on kami independence is treated as doctrinal error or incomplete understanding. Would object to the fusion claim but lack institutional voice; their resistance manifests as ritual preservation in marginal spaces.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shinto_ritualists, excluded,
    powerless, biographical, trapped, local).

% Uses the fused system to legitimize sovereign authority through both kami descent and Buddhist merit. Benefits from the ideological coherence the fusion provides while retaining the option to patronize either side independently. Observes the constraint's operation from a position of structural privilege.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, imperial_court, observer,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, imperial_court, beneficiary).

% Analyzes the fusion as a historical power arrangement rather than metaphysical truth. Sees the full structure of extraction, coordination, and suppression without being subject to its enforcement. Provides the external corroboration for the founding problem genealogy.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, modern_religious_studies, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single integrated religious field where Japanese people can access both this-worldly efficacy (kami) and transcendent liberation (buddhas) without cognitive dissonance or institutional fragmentation. Solves the coordination problem of plural devotional demands in a pre-modern polity.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual sovereignty, land patronage, and doctrinal primacy from autonomous kami cults to the Buddhist institutional hierarchy. The hierarchy collects the gains of unified religious administration; kami autonomy pays the cost of subordination.
% ABSENT_VOICES: Autonomous kami priesthoods and pre-syncretic ritual lineages were structurally excluded from the doctrinal conversation. Their voices survive only in fragmented ritual texts and oral traditions that the fusion framework treats as preliminary or erroneous.
% DISAPPEARANCE_RATIONALE: If the ontological fusion claim vanished overnight, the Buddhist institutional hierarchy would lose its interpretive monopoly over kami, shrine-temple complexes would face immediate jurisdictional contests, kami cults would reclaim ritual autonomy, and the unified religious field would fracture into competing domains — exactly the Meiji separation outcome.
% FOUNDING_PROBLEM: Pre-Heian Japanese religion faced a coordination crisis: proliferating kami cults with no unified soteriology, no shared metaphysical framework, and no institutional mechanism to integrate local worship with continental Buddhist doctrine. The fusion theory was built to solve this by subordinating the particular to the universal.
% FOUNDING_PROBLEM_CORROBORATION: Modern religious studies (non-beneficiary observers) attest the founding coordination crisis was resolved by the Kamakura period; the fusion persisted for 500+ years after its coordinating function was spent, sustained by institutional extraction. Buddhist institutional apologists alone claim the problem remains live.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the fusion claim transfers interpretive sovereignty, ritual control, and material patronage from kami institutions to Buddhist ones — the coordination benefit is real but the extraction is structural. Suppression (0.72) is substantial because maintaining the fusion requires active doctrinal enforcement: autonomous kami narratives are rewritten, independent priesthoods displaced, and the very category of 'kami' is redefined as 'suijaku.' Theater ratio (0.42) rises over time as the coordination function atrophies (by Kamakura the unified field exists) while the extraction machinery (temple-shrine administration, doctrinal policing) elaborates. Accessibility collapse (0.65) reflects that once the fusion framework is internalized, alternatives (autonomous kami worship) become cognitively and institutionally difficult. Resistance (0.58) is moderate: marginalized kami priesthoods preserve counter-narratives, but organized challenge only emerges in the medieval period (Yoshida Shinto) and decisively at Meiji.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist hierarchy's seat, the constraint is experienced as rope (genuine coordination they built and maintain). From the kami autonomy seat, it is experienced as snare (ontological subordination enforced by institutional power). From lay practitioners, it is tangled_rope (coordination benefit + diffuse extraction). The engine computes this divergence from the structural data — the authored claim (tangled_rope) reflects the analytical observer's synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional hierarchy is the structural beneficiary (agenda_setter, d ~ 0.15): it sets the hermeneutic frame, collects the rents, and has arbitrage-grade exit (can redefine the fusion at will). Indigenous kami autonomy is the target (payer, d ~ 0.85): it bears the ontological subordination, has constrained exit (embedded in local communities, dependent on the same patronage system), and is identity-locked to the very cults being subordinated. Lay practitioners sit near symmetric (d ~ 0.5): genuine coordination benefit, diffuse indirect cost, mobile exit at individual level but constrained at communal level. Shinto ritualists are excluded (trapped, d ~ 0.9): they would reject the fusion but lack voice. Imperial court and modern observers are analytical/beneficiary seats with arbitrage exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination crisis (fragmented kami cults without unified soteriology) was resolved by the Kamakura period — the unified religious field existed, the coordination problem was solved. Yet the fusion constraint persisted for 500+ years with rising extractiveness and theater, sustained by Buddhist institutional extraction, not coordinating need. This is classic mandatrophy: the mandate (coordination) died, the constraint (fusion) lived on as extraction. The founding_problem_status = dead with corroboration from non-beneficiary observers (modern religious studies) triggers the mandatrophy flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fusion_vs_coordination_necessity,
    'Was ontological fusion structurally necessary for the coordination function, or could domain-appropriate specialization (domain_partition_reading) have achieved the same unified devotional field without extraction?',
    'Counterfactual analysis of pre-Meiji religious organization: did regions with stronger kami autonomy (e.g., Ise, Izumo) fail to provide integrated this-worldly/transcendent access? Comparative study of shrine-temple complexes with varying degrees of Buddhist control.',
    'If fusion was unnecessary for coordination, the measured extraction is pure rent-seeking riding on a pretext — strengthening the snare component. If necessary, part of the extraction is the price of coordination itself — supporting tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fusion_vs_coordination_necessity, conceptual, 'Whether the ontological fusion claim was structurally necessary for religious coordination or an extractive overlay').

omega_variable(
    practitioner_belief_vs_institutional_doctrine,
    'Did lay practitioners genuinely believe the ontological identity claim, or did they pragmatically participate in both cults while the fusion doctrine served institutional interests? (The pragmatic_incoherence_reading''s core claim.)',
    'Analysis of vernacular devotional texts, votive offerings, and folk practice records vs. elite doctrinal texts. If popular practice shows domain-appropriate specialization without ontological commitment, the fusion doctrine is institutional theater.',
    'If practitioners did not believe the fusion, theater_ratio is underestimated — the coordination function is a cover story. If they did believe, the fusion has genuine cognitive coordination value, supporting the tangled_rope coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioner_belief_vs_institutional_doctrine, empirical, 'Gap between institutional doctrine and popular belief in honji-suijaku theory').

omega_variable(
    committer_frame_ambiguity,
    'This constraint is one reading (ontological_fusion_reading) of the simultaneous_veneration kernel. The sibling readings (domain_partition_reading, pragmatic_incoherence_reading) would author fundamentally different ε values, beneficiary/victim structures, and constraint types. Where exactly is the structural disagreement located?',
    'Map each reading''s ε referent: the fusion reading assesses the standing fused arrangement; the domain_partition reading assesses the counterfactual partitioned arrangement; the pragmatic_incoherence reading assesses the contingent practice without doctrinal superstructure. The disagreement is located in what counts as ''the constraint'' — a doctrinal claim, a domain partition, or a practice pattern.',
    'Resolves whether these are three constraints (per ε-invariance) or one constraint with measurement ambiguity. The ε-invariance principle demands three stories — which this corpus structure already implements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Kernel-reading decomposition: structural location of disagreement across simultaneous_veneration readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 750, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t750, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 750, 0.15).
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 900, 0.22).
narrative_ontology:measurement(simu_tr_t1050, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1050, 0.3).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1200, 0.35).
narrative_ontology:measurement(simu_tr_t1350, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1350, 0.38).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1500, 0.4).
narrative_ontology:measurement(simu_tr_t1650, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1650, 0.41).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1868, 0.42).

% Extraction over time
narrative_ontology:measurement(simu_be_t750, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 750, 0.35).
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 900, 0.52).
narrative_ontology:measurement(simu_be_t1050, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1050, 0.65).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1200, 0.71).
narrative_ontology:measurement(simu_be_t1350, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1350, 0.75).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1500, 0.77).
narrative_ontology:measurement(simu_be_t1650, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1650, 0.78).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1868, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t750, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 750, 0.3).
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 900, 0.45).
narrative_ontology:measurement(simu_su_t1050, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1050, 0.58).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1200, 0.65).
narrative_ontology:measurement(simu_su_t1350, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1350, 0.68).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1500, 0.7).
narrative_ontology:measurement(simu_su_t1650, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1650, 0.71).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1868, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__ontological_fusion_reading, 0.08).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__pragmatic_incoherence_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, meiji_shinbutsu_bunri).

% DUAL FORMULATION NOTE:
% Part of the simultaneous_veneration constraint family (kernel_id: simultaneous_veneration). This reading (ontological_fusion) asserts a single high-extraction tangled_rope constraint. The domain_partition_reading would assert a lower-extraction rope with symmetric coordination. The pragmatic_incoherence_reading would assert no coherent constraint, only contingent practice. All three share the kernel but instantiate different structural claims per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
