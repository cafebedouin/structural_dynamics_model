% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Kami-Buddha Ontological Substrate (Syncretic Fusion Reading)
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   This constraint embodies ONE READING of the contested shinbutsu
 *   (kami-buddha) relationship in Japanese religious history. The syncretic
 *   fusion reading claims that kami and buddhas are ontologically unified —
 *   that honji suijaku (original essence, manifest traces) describes
 *   metaphysical reality, not merely a convenient institutional arrangement.
 *   This reading treats syncretism as a discovery or elaboration of deep
 *   truth rather than as pragmatic institutional imposition. It competes
 *   against domain partition (kami and buddhas govern distinct cosmological
 *   domains) and incoherent bundle (no unified kernel exists, only
 *   accumulated institutional drift). The constraint story focuses on the
 *   syncretic reading as a commitment-system kernel and its operation through
 *   time.
 *
 * KEY AGENTS:
 *   - Syncretist interpreters: elaborate and defend the honji suijaku framework; institutional beneficiaries of its dominance
 *   - Integrated practitioners: temples and shrines organized around unified kami-buddha ritual; face fragmentation if syncretism dissolves
 *   - State authority: historical sponsor/enforcer of syncretism as unified field; benefits from simplified regulation
 *   - Confessional reformists: hold alternative readings (domain partition, separatism); marginalized within the dominant frame
 *   - Lay practitioners: inherit and perform syncretism; bear cognitive burden of unified commitment
 *   - Excluded purist traditions: doctrinal lineages treated as heterodox; structurally prevented from teaching alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.42).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.31).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Kami-Buddha Ontological Substrate (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious/philosophical/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, '42af5228-cbee-4203-b15c-81fb206b2994').
narrative_ontology:cs_kernel_codification('42af5228-cbee-4203-b15c-81fb206b2994', fixed_text).
narrative_ontology:cs_authority_grounding('42af5228-cbee-4203-b15c-81fb206b2994', lineage).
narrative_ontology:cs_interpretation_layer_present('42af5228-cbee-4203-b15c-81fb206b2994').
narrative_ontology:cs_reading_relation('42af5228-cbee-4203-b15c-81fb206b2994', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('42af5228-cbee-4203-b15c-81fb206b2994', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('42af5228-cbee-4203-b15c-81fb206b2994', foundational, kami_buddha_ontological_unity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('42af5228-cbee-4203-b15c-81fb206b2994', kami_buddha_ontological_unity, deontological).
narrative_ontology:cs_axiom('42af5228-cbee-4203-b15c-81fb206b2994', foundational, honji_suijaku_metaphysical_necessity).
narrative_ontology:cs_axiom_status(honji_suijaku_metaphysical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('42af5228-cbee-4203-b15c-81fb206b2994', honji_suijaku_metaphysical_necessity, deontological).
narrative_ontology:cs_reference_frame('42af5228-cbee-4203-b15c-81fb206b2994', unified_kami_buddha_cosmology).
narrative_ontology:cs_drift_state('42af5228-cbee-4203-b15c-81fb206b2994', meiji_restoration_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('42af5228-cbee-4203-b15c-81fb206b2994', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretist_interpreters).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, integrated_institutional_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, confessional_reformists).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_metaphysical_thesis).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, ontological_unity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars, priests, and doctrinal authorities whose interpretive authority derives from defending and elaborating the syncretic fusion thesis. They have invested intellectual and institutional capital in the honji suijaku framework as metaphysically true, not merely institutionally convenient. Their position depends on the unified reading remaining the dominant interpretive frame.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretist_interpreters, beneficiary,
    institutional, generational, constrained, national).

% Buddhist temples, Shinto shrines, and hybrid ritual practitioners who maintain syncretist observance as their core function. They conduct joint rites, host pilgrims seeking both kami and buddha favor, and organize institutional life around the unified assumption. Separation would fragment their operational coherence and require institutional redesign.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, integrated_institutional_practitioners, beneficiary,
    organized, generational, identity_locked, regional).

% Buddhist and Shinto clerics and scholars who hold that kami and buddhas are distinct entities or serve functionally separate domains. They must operate within an interpretive consensus that treats syncretism as metaphysically grounded; their objections are read as doctrinal confusion or heterodoxy rather than alternative coherence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, confessional_reformists, payer,
    moderate, biographical, constrained, national).

% The institutional apparatus (court, shogunate, later Meiji state) that historically sponsored or enforced syncretism as a unified frame. They benefit from a single integrated religious field easier to regulate than competing autonomously-organized confessions. They set the terms under which alternative readings are permitted or suppressed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, state_authority_administrators, agenda_setter,
    institutional, generational, mobile, national).

% Individual believers and community members who inherit syncretist practice as their lived religious context. They benefit from unified ritual access and coherent cosmology. They also bear the cost of not being able to distinguish or prioritize kami and buddha devotion separately without cognitive dissonance or social friction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners, payer).

% Academic observers studying Japanese religious history and syncretism. They document the constraint's operation and contest its structural nature without bearing its costs or collecting its benefits directly.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, comparative_scholars, observer,
    analytical, generational, analytical, global).

% Doctrinal lineages (certain esoteric Buddhist schools, reconstructionist Shinto movements) that reject the syncretic fusion thesis as incoherent or false. They are structurally marginalized within the dominant framework and denied institutional standing to teach their alternative reading as legitimate doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, excluded_purist_traditions, excluded,
    moderate, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretist_interpreters).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the Japanese religious field under a single coherent metaphysical structure, enabling shared sanctuary, joint ritual, and integrated cosmology without requiring adherents to choose between kami and buddha devotion as incompatible commitments.
% TRANSFER_FUNCTION: Transfers interpretive authority to syncretist specialists and integrated institutions; transfers cognitive burden onto lay practitioners who must hold unified commitment rather than maintaining distinct kami and buddha domains; transfers regulatory simplicity to state authorities who manage one religious field instead of sectarian competition.
% ABSENT_VOICES: Confessional purists (Buddhist separatists, Shinto particularists) would contest the thesis as metaphysically false and institutionally imposed; indigenous religious specialists from pre-syncretist kami traditions and pre-Mahayana Buddhist lineages have no voice in the contemporary reading; rational-skeptic and secular practitioners are systematized out of the framework entirely.
% DISAPPEARANCE_RATIONALE: If the syncretic fusion reading dissolved overnight — if the dominant reading shifted to domain partition or acknowledged incoherence — integrated temples would face institutional fragmentation, ritual practitioners would need to reorganize around chosen identity, lay practitioners would experience cosmological rupture, and state regulatory apparatus would need to manage plural confessions. Institutional, cognitive, and administrative infrastructure depends on the unified reading persisting.
% FOUNDING_PROBLEM: Early medieval Japan faced a choice: treat kami and buddhas as irreconcilable entities requiring doctrinal competition and geographical/ritual separation, or elaborate a unified metaphysical framework permitting both to operate within one coherent cosmology. The syncretic reading solves this by positing ontological unity — kami are manifestations of buddha-nature or bodhisattvas in kami form; buddhas access kami domains; the two are expressions of a deeper metaphysical substrate.
% FOUNDING_PROBLEM_CORROBORATION: Syncretist interpreters and integrated practitioners attest the founding problem as live — the risk of confessional fragmentation and cosmological incoherence persists if syncretism dissolves. Confessional reformists and comparative scholars attest the founding problem was solved through institutional imposition rather than metaphysical truth — the problem was political (managing competition), not ontological. Post-Meiji Shinto purists explicitly rejected syncretism and asserted the founding problem was an imposed constraint, not a discovered truth. Meiji documentary record shows explicit state policy to suppress syncretism as 'contamination' of pure Shinto — external evidence that the founding problem had been solved and the constraint had become institutional inertia.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42 at interval end) is moderate because the syncretic reading does deliver genuine coordination: it unifies the religious field, permits shared ritual, and offers coherent cosmology to practitioners. But it also extracts by concentrating interpretive authority in syncretist specialists, by imposing unified commitment on lay practitioners who might prefer domain separation, and by marginalizing alternative readings. Suppression (0.31) is lower than extractiveness because the reading's persistence depends more on interpretive authority and institutional investment than on active coercion — confessional reformists are marginalized rather than actively suppressed, though Meiji-era moves toward Shinto puritanism did involve active suppression of syncretism. Theater (0.18) is low because the coordination function is real and central; theatrical performance is not the reading's primary maintenance mechanism. The temporal series shows extractiveness and suppression rising from 800–1700 as syncretism became institutionalized, then stabilizing after 1850 as the reading became hegemonic and required less active maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Syncretist interpreters perceive the constraint as rope (genuine coordination elaborated over centuries); confessional reformists perceive it as snare (metaphysically false doctrine imposed to serve institutional consolidation). State authorities perceive it as rope (regulatory simplification); lay practitioners experience it as partly binding (unified framework) and partly extractive (cognitive burden, loss of kami-specific or buddha-specific identity). The engine computes per-seat classification from the structural data; the authored metrics reflect the syncretic reading's own internal logic (modest extraction, real coordination) rather than averaging across the competing readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretist interpreters and integrated practitioners are beneficiaries (d low, near 0.15–0.25): they benefit from interpretive authority, institutional centrality, and coherent cosmology. Confessional reformists are partly targets (d ~0.60–0.70): they must operate within a frame they reject, cannot teach alternatives, face marginalization. State authorities are mobile (d ~0.50): they benefit from regulatory simplification but can shift their support if political conditions change. Lay practitioners are complex (d ~0.45): they benefit from unified ritual and cosmology but pay the cost of cognitive constraint and loss of identity autonomy. Excluded purist traditions are fully targeted (d ~0.85): they cannot participate, teach, or gain institutional standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The syncretic fusion reading's founding problem (avoiding confessional fragmentation, elaborating unified cosmology) was substantially solved by Edo period (1600s onward). By Meiji (1868+), the founding problem had shifted: Meiji ideologues explicitly rejected syncretism and pursued Shinto purism, treating the syncretic reading as an imposed constraint to be dissolved. Post-1950, syncretism persists but is no longer defended as metaphysically true by state authority — it operates as institutional inertia and lay practice, not as dominant doctrine. The constraint shows mandatrophy characteristics: the founding problem is dead (confessional separation is no longer the threat it was in medieval Japan; institutional multiplicity is now normalized), yet the reading persists through integrated practitioner institutions, lay cognitive habitus, and scholarly tradition. This is the classic piton signature: extracted rents (interpretive authority, institutional centrality) persist beyond the functional need they once served.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_truth_vs_institutional_convenience,
    'Is the syncretic fusion reading''s claim to ontological truth genuine discovery/elaboration, or is it post-hoc rationalization of an institutional convenience that served political consolidation?',
    'Genealogical analysis of doctrinal texts: did honji suijaku emerge as independent philosophical elaboration, or as explicit justification for institutional unification? Comparative study of pre-syncretism kami and buddha theology to establish whether the reading represents conceptual innovation or repackaging.',
    'If institutional convenience: reclassify as snare (extraction disguised by philosophical framing). If genuine elaboration: classification holds as rope with real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_truth_vs_institutional_convenience, conceptual, 'Whether the constraint represents metaphysical truth or post-hoc institutional justification.').

omega_variable(
    confessional_reformist_agency,
    'Do confessional reformists genuinely hold alternative readings (domain partition), or are they primarily resisting institutional imposition without coherent alternative theology?',
    'Close textual analysis of reformist doctrinal writings; comparison of their cosmological claims and internal coherence to the syncretic framework. Do they offer logically integrated alternative systems, or primarily negative critique?',
    'If coherent alternatives exist: the suppression measure understates the actual marginalization. If reformists lack coherent alternatives: the constraint''s dominance is less extractive (reformism is incoherent, not suppressed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confessional_reformist_agency, empirical, 'Whether confessional reformism represents coherent alternative theology or primarily reactive critique.').

omega_variable(
    lay_practitioner_identity_lock_internalization,
    'Is lay practitioners'' adherence to syncretism structurally enforced (exit options genuinely trapped/constrained), or internalized such that exit feels impossible even absent external barriers?',
    'Historical and ethnographic study of lay practitioners in post-1950 Japan where active state enforcement has ceased. Do practitioners maintain syncretism through institutional participation, or through cognitive identity fusion? Post-exit trajectory: do practitioners who exit syncretism report persistence of internal constraint even after leaving integrated institutions?',
    'If internalized: effective suppression exceeds the authored metric (the constraint is carried by practitioners into contexts where external enforcement absent). If structural: suppression metric accurately reflects institutional barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_practitioner_identity_lock_internalization, empirical, 'Whether lay identity-lock to syncretism is internalized or structurally maintained.').

omega_variable(
    reading_stability_under_modernization,
    'Is the syncretic fusion reading''s persistence in contemporary Japan evidence of robust metaphysical commitment, or artifact of institutional inertia and lay habitus in post-Meiji environment where active state enforcement ceased?',
    'Comparative study of syncretism vitality across different institutional contexts: integrated temples (high vitality), Meiji-reformed Shinto shrines (low vitality), urbanized lay practitioners (variable vitality). Generational studies: are younger practitioners as committed to honji suijaku as older generations, or is syncretism decaying at population level?',
    'High post-Meiji vitality supports rope/coordination reading. Decay suggests piton/inertia reading. Differential vitality across contexts suggests extraction concentrates in institutional rather than lay commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability_under_modernization, empirical, 'Whether syncretic commitment is robust or inertial in contemporary practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 800, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t800, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 800, 0.08).
narrative_ontology:measurement_basis(shin_tr_t800, projected).
narrative_ontology:measurement(shin_tr_t1100, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement_basis(shin_tr_t1100, observed).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement_basis(shin_tr_t1400, observed).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1700, 0.18).
narrative_ontology:measurement_basis(shin_tr_t1700, observed).
narrative_ontology:measurement(shin_tr_t1850, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1850, 0.19).
narrative_ontology:measurement_basis(shin_tr_t1850, observed).
narrative_ontology:measurement(shin_tr_t1950, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement_basis(shin_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t800, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 800, 0.15).
narrative_ontology:measurement_basis(shin_be_t800, projected).
narrative_ontology:measurement(shin_be_t1100, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1100, 0.28).
narrative_ontology:measurement_basis(shin_be_t1100, observed).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1400, 0.38).
narrative_ontology:measurement_basis(shin_be_t1400, observed).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1700, 0.45).
narrative_ontology:measurement_basis(shin_be_t1700, observed).
narrative_ontology:measurement(shin_be_t1850, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1850, 0.42).
narrative_ontology:measurement_basis(shin_be_t1850, observed).
narrative_ontology:measurement(shin_be_t1950, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement_basis(shin_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t800, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 800, 0.1).
narrative_ontology:measurement_basis(shin_su_t800, projected).
narrative_ontology:measurement(shin_su_t1100, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1100, 0.18).
narrative_ontology:measurement_basis(shin_su_t1100, observed).
narrative_ontology:measurement(shin_su_t1400, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1400, 0.24).
narrative_ontology:measurement_basis(shin_su_t1400, observed).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1700, 0.29).
narrative_ontology:measurement_basis(shin_su_t1700, observed).
narrative_ontology:measurement(shin_su_t1850, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1850, 0.32).
narrative_ontology:measurement_basis(shin_su_t1850, observed).
narrative_ontology:measurement(shin_su_t1950, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1950, 0.31).
narrative_ontology:measurement_basis(shin_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, meiji_shintoism_purification).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the shinbutsu kernel (kami-buddha relationship). The syncretic fusion reading claims ontological unity via honji suijaku. Domain partition reading claims functional separation (this-world vs. afterlife domains). Incoherent bundle reading denies a coherent kernel exists. All three are constraints on the same persisting question, linked by network.affects_constraints. Each has different ε, beneficiary/victim structure, and historical trajectory. Generated separately per ε-invariance principle (OQ-26); linked here to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
