% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Syncretic Honji-Suijaku Ontological Integration
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   Under honji-suijaku metaphysics, kami are understood as manifestations
 *   (suijaku) of Buddhist deities (honji), integrating Shinto and Buddhism
 *   into a unified cosmological order. This constraint instantiates the
 *   syncretic reading of the contested kernel
 *   shinbutsu_ontological_commitment. The syncretic reading asserts that a
 *   genuine, stable metaphysical integration was achieved and
 *   institutionalized. This reading benefits Buddhist institutional hierarchy
 *   (which controls the interpretive framework) while suppressing Shinto
 *   autonomy (local kami practice must be reinterpreted within Buddhist
 *   ontology). The constraint operates through doctrinal authority,
 *   institutional hierarchy, and the gradual reframing of kami within
 *   Buddhist cosmological premises. By the early-modern period, the syncretic
 *   framework is deeply embedded in institutional practice, though its
 *   metaphysical coherence remains contested by alternative readings
 *   (partition reading: Shinto and Buddhism occupy separate domains;
 *   incoherence reading: no stable ontological commitment was achieved, only
 *   institutional tolerance of contradiction).
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy: sets and enforces the syncretic framework; benefits from unified authority
 *   - Autonomous Shinto practice: subordinated within the framework; loses institutional autonomy
 *   - Local kami devotion: incorporated without explicit consent; trapped by proximity to shrines and community ritual
 *   - Honji-suijaku doctrinal authority: gains institutional weight from the syncretic premise
 *   - Ritual practitioners at shrine level: gain legitimacy but lose autonomy
 *   - Secular rulers: benefit from unified metaphysical order that simplifies governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.67).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Syncretic Honji-Suijaku Ontological Integration").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '29d5e64f-53e4-483f-b7b3-d687e08acca2').
narrative_ontology:cs_kernel_codification('29d5e64f-53e4-483f-b7b3-d687e08acca2', formalized).
narrative_ontology:cs_authority_grounding('29d5e64f-53e4-483f-b7b3-d687e08acca2', lineage).
narrative_ontology:cs_interpretation_layer_present('29d5e64f-53e4-483f-b7b3-d687e08acca2').
narrative_ontology:cs_reading_relation('29d5e64f-53e4-483f-b7b3-d687e08acca2', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('29d5e64f-53e4-483f-b7b3-d687e08acca2', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('29d5e64f-53e4-483f-b7b3-d687e08acca2', foundational, kami_are_bodhisattva_manifestations).
narrative_ontology:cs_axiom_status(kami_are_bodhisattva_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('29d5e64f-53e4-483f-b7b3-d687e08acca2', kami_are_bodhisattva_manifestations, deontological).
narrative_ontology:cs_axiom('29d5e64f-53e4-483f-b7b3-d687e08acca2', foundational, unified_cosmological_order_through_honji_suijaku).
narrative_ontology:cs_axiom_status(unified_cosmological_order_through_honji_suijaku, holdable).
narrative_ontology:cs_axiom_grounding('29d5e64f-53e4-483f-b7b3-d687e08acca2', unified_cosmological_order_through_honji_suijaku, conventional).
narrative_ontology:cs_reference_frame('29d5e64f-53e4-483f-b7b3-d687e08acca2', integrated_shinto_buddhist_hierarchy).
narrative_ontology:cs_drift_state('29d5e64f-53e4-483f-b7b3-d687e08acca2', meiji_restoration_1868, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('29d5e64f-53e4-483f-b7b3-d687e08acca2', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, syncretic_doctrinal_authority).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, autonomous_shinto_practice).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, local_kami_devotion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, honji_suijaku_doctrinal_authority).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, ritual_practitioners_at_shrine_level).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, secular_authorities_and_rulers).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, ritual_practitioners_at_shrine_level).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, metaphysical_unity_of_kami_and_buddhas).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, honji_suijaku_cosmological_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples, monasteries, and the ecclesiastical establishment operate and interpret the honji-suijaku framework. They maintain the doctrinal coherence, adjudicate what counts as legitimate kami interpretation, and benefit from institutional authority over the entire religious landscape. Buddhist authorities set the interpretive standard and enforce it through teaching, ritual adjudication, and institutional hierarchy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, constrained, national).

% Shinto shrines, priests, and practitioners operate within the syncretic framework but with reduced autonomy. Their kami are required to be understood as manifestations of Buddhist deities (honji-suijaku). Institutional standing depends on doctrinal coherence with Buddhist interpretations. Local shrine autonomy in defining kami nature is constrained by the requirement to align with Buddhist cosmology.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, autonomous_shinto_practice, payer,
    moderate, biographical, identity_locked, local).

% Village-level and family-level kami worship—ancestor veneration, seasonal festivals, local sacred geography—is incorporated into the syncretic framework without explicit negotiation or consent. Practitioners cannot easily exit because kami devotion is woven into community ritual, seasonal cycles, and kinship practice. The constraint operates through the same institutional structures (shrines, priests, festivals) that practitioners depend on for other purposes.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, local_kami_devotion, payer,
    powerless, immediate, trapped, local).

% The honji-suijaku doctrine and its interpretive tradition gain institutional prestige and authority from the syncretic framework. Buddhist scholarship consolidates around syncretic coherence, creating a self-reinforcing doctrinal edifice. The doctrine's legitimacy compounds as it becomes institutionally embedded, and alternative readings (partition, incoherence) are marginalized as theologically naive or institutionally marginal.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, honji_suijaku_doctrinal_authority, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, honji_suijaku_doctrinal_authority, agenda_setter).

% Shinto autonomy advocates and those who would defend a partition logic (Shinto handles life, community, and society; Buddhism handles death, salvation, and the afterlife) are excluded from the framework's authority structure. Their positions are treated as theologically inferior or institutionally naive rather than as coherent alternatives. They lack institutional power to adjudicate the meaning of kami within the syncretic system.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, alternative_partition_advocates, excluded,
    moderate, biographical, constrained, national).

% Modern scholars and analysts who argue that shinbutsu-shugo was pragmatically tolerated incoherence rather than achieved metaphysical unity are excluded from the constraint's legitimacy structure. Their readings treat the syncretic framework as institutional convenience masking logical contradiction rather than genuine integration. They operate from outside the religious authority system and their conclusions are treated as reductionist or anachronistic.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, empiricist_incoherence_critics, excluded,
    analytical, generational, analytical, global).

% Shrine priests, ritual specialists, and community leaders gain institutional legitimacy and doctrinal clarity from the syncretic framework. Their work is validated as participating in a unified cosmological order. But they also lose autonomy: their kami interpretations must align with Buddhist doctrinal standards, their innovations must be justified within honji-suijaku logic, and their institutional standing depends on maintaining coherence with Buddhist hierarchy. Identity fusion with priestly role makes exit from the framework very costly.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, ritual_practitioners_at_shrine_level, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, ritual_practitioners_at_shrine_level, payer).

% Political rulers and state authorities benefit from a unified metaphysical order that integrates local kami cults with national Buddhist institutions. One coherent religious framework simplifies governance, reduces competing religious authorities, and allows rulers to position themselves within both systems simultaneously. Rulers can invoke kami for local legitimacy and Buddhism for civilizational/continental legitimacy without religious contradiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, secular_authorities_and_rulers, beneficiary,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates disparate local kami cults and national Buddhist institutional authority into a single metaphysical framework. Solves the problem of how to maintain Buddhist institutional dominance while incorporating indigenous Shinto practice into a coherent order. Provides doctrinal coherence, unified interpretive authority, and a cosmological hierarchy in which both kami and buddhas have a place.
% TRANSFER_FUNCTION: Transfers interpretive authority, institutional prestige, and cosmological primacy from local kami practice to Buddhist institutional hierarchy. Kami reinterpreted as manifestations (suijaku) of Buddhist deities (honji), subordinating Shinto to Buddhist doctrinal authority. Benefits flow to Buddhist institutions (authority, prestige, unified governance) while Shinto loses ontological autonomy and institutional standing.
% ABSENT_VOICES: Those who would defend Shinto as an autonomous tradition with its own metaphysical integrity independent of Buddhist cosmology; scholars and practitioners who read the arrangement as institutional tolerance of incoherence rather than achieved coherence; local communities whose kami practice operates on different ontological premises; Shinto reformers who would partition the domains (kami for life/community, Buddhism for death/salvation) without ontological integration.
% DISAPPEARANCE_RATIONALE: If honji-suijaku were abandoned and the syncretic framework dissolved, institutional authority would splinter—Buddhist claims to hierarchical authority over kami cults would lose metaphysical grounding; local shrine practice would reorient toward autonomous kami interpretation; political rulers would lose the convenience of a unified religious order; and the religious landscape would reorganize around decoupled or differently-integrated traditions. The Meiji Restoration (1868) demonstrates this: when the syncretic framework is officially abandoned, both the institutional structure and the cosmological order rearrange significantly.
% FOUNDING_PROBLEM: Early medieval Japan faced the institutional challenge of maintaining Buddhist dominance and coherence while incorporating widespread indigenous kami worship that had its own cosmological status and institutional bases. Buddhism could not simply suppress kami worship (too entrenched, too locally important), but it needed to subordinate it doctrinally. Honji-suijaku metaphysics solved this by reinterpreting kami as emanations of Buddhist deities, creating a cosmological hierarchy in which Buddhism remained supreme.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional histories, doctrinal texts, and contemporary scholars within the Buddhist tradition attest that the founding problem was real and honji-suijaku solved it. Modern scholars outside the benefiting Buddhist institutions (Grapard, Josephson, Miyake, Kuroda) attest that the problem framing itself is contestable: some argue the framework functioned despite internal logical incoherence; others argue Shinto and Buddhism never achieved true ontological integration, only institutional proximity. No corroboration exists from sources independent of either Buddhist or Shinto institutional interests; the problem itself is religiously loaded.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderately high (0.67) because the constraint redistributes interpretive authority from local kami practice to Buddhist hierarchy; kami are reinterpreted as derivative manifestations rather than autonomous beings. Suppression is substantial (0.58) because the constraint requires Shinto practitioners to frame their practice within Buddhist ontology—alternative readings of kami autonomy are marginalized. Theater is moderate (0.42): the honji-suijaku doctrine is genuinely believed by many practitioners and is not mere performance, but the institutional dynamics of the constraint depend partly on maintaining the fiction of perfect coherence despite ongoing local and doctrinal tensions. Accessibility collapse is moderately high (0.71) because once the syncretic framework is internalized, the kami worldview can be reinterpreted retrospectively as always having been manifestations of Buddha-nature—the alternative (autonomous kami) requires active cognitive work to recover. Resistance is moderate (0.54) because the constraint operates through existing religious institutions and rituals, many practitioners benefit from it, and organized opposition is dispersed rather than coordinated (though some scholarly and sectarian voices resist the integration). The measurement series runs on one time grid (750-1868 at six-point intervals) and shows extractiveness and suppression rising steeply through the Heian-Kamakura period (750-1200) as the framework consolidates, then stabilizing at a high plateau through early-modernity—the theater ratio follows the same trajectory, suggesting performative maintenance increases as the constraint becomes institutionally entrenched.
 *
 * PERSPECTIVAL GAP:
 *   The Buddhist institutional seat and the autonomous-Shinto-practice seat should compute sharply different types: from the institutional perspective the constraint is genuine coordination achieving metaphysical coherence; from the subordinated-practice perspective it is enforced reinterpretation of kami nature. The gap arises from the asymmetric benefit distribution (institutional prestige vs. interpretive subordination) and exit options (constrained for Buddhists but only constrained-or-trapped for Shinto practitioners). The engine computes this from the structural data: beneficiary/victim asymmetry + power difference + exit divergence produces the per-seat classification divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional hierarchy occupies the beneficiary seat: they set the framework, define what counts as coherent, and gain institutional prestige and authority from a unified order. Extractiveness toward this seat is low (d near 0.0). Autonomous Shinto practice and local kami devotion occupy the target seats: their interpretive autonomy is constrained, their kami reinterpreted, their institutional standing subordinated. Extractiveness toward these seats is high (d near 1.0). Ritual practitioners at shrine level are genuinely dual-positioned: they gain legitimacy and doctrinal status from the syncretic framework but lose autonomy in exchange—they occupy both beneficiary and payer roles, with d near 0.5. Secular rulers benefit from the unified order without bearing direct costs of maintaining it, placing them near the beneficiary end. The engine derives these directionalities from the declared beneficiary/victim structure plus exit options: identity-locked exit for autonomous Shinto practice and local kami devotion amplifies their target positioning (d increases toward 1.0); constrained or mobile exit for institutional actors keeps them from full target status.
 *
 * MANDATROPHY ANALYSIS:
 *   The syncretic reading does not reduce to Mandatrophy (a decoupled founding problem). The founding problem—integrating kami worship into Buddhist institutional authority while maintaining both traditions' spiritual legitimacy—remains live through early-modernity. Shinto practitioners do not uniformly abandon kami worship as the constraint took hold; instead, kami reinterpretation becomes gradual and deeply embedded in practice. The constraint persists because it solves a real coordination problem (how to maintain religious authority unity in a multiconfessional landscape) even though its metaphysical premise (true ontological unity) is contested by the incoherence reading. The theater ratio rise (0.15 to 0.42 over the interval) suggests an increasing gap between the functional coordination (institutional unity works) and the metaphysical claim (kami are really Buddhist manifestations)—this is consistent with a tangled_rope that accretes performance without abandoning the coordination core. Mandatrophy would manifest as the founding problem becoming entirely dead (kami worship abandoned, Buddhism institutionally unified without Shinto) while the constraint persists as institutional inertia; the historical record shows the opposite: kami worship remains live, the coordination problem remains live, and the syncretic framework adapts rather than merely persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_coherence_vs_institutional_function,
    'Did honji-suijaku metaphysics constitute a genuine, stable ontological commitment to the unity of kami and buddhas, or was it a pragmatic institutional framework that functioned despite underlying incoherence?',
    'Hermeneutic analysis of doctrinal texts and practitioner testimony: do accounts treat the integration as ontologically necessary or institutionally convenient? Comparative analysis with other syncretic systems (e.g., Confucianism + kami worship) that did NOT require honji-suijaku integration. Post-Meiji separation: did practitioners abandon the metaphysical claim or the institutional framework?',
    'If genuinely coherent ontological commitment: the syncretic reading stands; the constraint is tangled_rope coordinating real integration with institutional extraction. If institutional function without metaphysical coherence: the incoherence reading applies; the constraint is snare using ontological language to mask extraction. If partition logic (Shinto and Buddhism never integrated ontologically): the partition reading applies; the constraint is theater maintaining institutional proximity without metaphysical claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_coherence_vs_institutional_function, conceptual, 'Whether the syncretic framework achieved genuine metaphysical integration or masked institutional exploitation or represented genuine partition under unified language.').

omega_variable(
    autonomy_suppression_mechanism,
    'Is the measured suppression (0.58) structural (external enforcement by Buddhist hierarchy, legal/institutional barriers to autonomous Shinto practice) or internalized (Shinto practitioners have internalized the honji-suijaku premise and experience their own kami interpretation as naturally derivative)?',
    'Post-constraint removal analysis: the Meiji Restoration (1868) officially desacralizes Buddhism and attempts to restore Shinto autonomy. Measurement of suppression persistence after institutional enforcement is removed—do local kami practices immediately recover autonomy and ontological independence, or does the honji-suijaku frame persist in practitioners'' self-understanding?',
    'If suppression is primarily structural: the constraint''s effective suppression diminishes rapidly after institutional enforcement is withdrawn, and Shinto autonomy recovers quickly. If suppression is primarily internalized: the honji-suijaku frame persists in practitioners'' understanding even after institutional incentives to maintain it vanish, and the constraint''s binding force outlasts its institutional machinery. Internalization would indicate the constraint runs deeper than institutional power alone—it has modified how Shinto practitioners understand kami.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_suppression_mechanism, empirical, 'Whether suppression of Shinto autonomy is enforced externally or internalized by practitioners, indicated by post-Meiji recovery dynamics.').

omega_variable(
    institutional_capture_vs_genuine_coordination,
    'Does Buddhist institutional hierarchy benefit from the syncretic framework primarily through capturing and controlling interpretive authority (snare-type extraction), or through enabling genuine multi-tradition coordination that would not be possible with Shinto autonomous (tangled_rope-type coordination with asymmetric distribution)?',
    'Counterfactual analysis: absent honji-suijaku, could Buddhist institutions have maintained institutional authority while tolerating autonomous Shinto practice? Comparative institutional analysis: do other jurisdictions or time periods achieve similar religious integration without syncretic metaphysics (suggesting the metaphysical claim is decorative)? Cost analysis: what would it cost Buddhist institutions to abandon honji-suijaku if the coordination function dissolved?',
    'If honji-suijaku is primarily a capture mechanism: the constraint is snare; benefits accrue to Buddhist hierarchy through suppression of Shinto autonomy, and the coordination function is post-hoc rationalization. If honji-suijaku enables genuine coordination: the constraint is tangled_rope; both traditions benefit from institutional integration even though the benefits are asymmetrically distributed. The boundary between these is whether Shinto practitioners would choose institutional integration with Buddhist hierarchy absent the honji-suijaku frame (or absent suppression): if yes, it''s coordination; if no, it''s capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_genuine_coordination, empirical, 'Whether the syncretic framework enables genuine multi-tradition coordination or primarily facilitates Buddhist capture of interpretive authority.').

omega_variable(
    kernel_reading_under_determination,
    'Which of the three sibling readings (syncretic, partition, incoherence) most accurately captures the ontological and institutional reality of shinbutsu-shugo?',
    'This is inherently under-determined by empirical evidence. All three readings coexist in the contemporary scholarly literature and in historical sources. The determination cannot be made through historical discovery because the readings are interpretive framings of a contested commitment, not claims about empirical facts that could be resolved by additional data.',
    'If partition reading is correct: Shinto and Buddhism never achieved ontological integration; honji-suijaku language describes institutional proximity, not metaphysical unity. If incoherence reading is correct: shinbutsu-shugo tolerated logical contradiction; institutional utility, not metaphysical coherence, held the arrangement together. If syncretic reading is correct: genuine metaphysical integration was achieved and remain the legitimate understanding of kami-buddha relationship. Each reading carries different implications for contemporary Shinto-Buddhist relations and for interpreting the Meiji Restoration as rupture vs. suppression vs. decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Kernel reading indeterminacy: syncretic, partition, and incoherence readings all coexist; determination requires committer choice, not empirical resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 750, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t750, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 750, 0.15).
narrative_ontology:measurement_basis(shin_tr_t750, projected).
narrative_ontology:measurement(shin_tr_t950, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 950, 0.24).
narrative_ontology:measurement_basis(shin_tr_t950, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1200, 0.35).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1500, 0.42).
narrative_ontology:measurement_basis(shin_tr_t1500, observed).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1700, 0.43).
narrative_ontology:measurement_basis(shin_tr_t1700, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1868, 0.42).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t750, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 750, 0.35).
narrative_ontology:measurement_basis(shin_be_t750, projected).
narrative_ontology:measurement(shin_be_t950, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 950, 0.48).
narrative_ontology:measurement_basis(shin_be_t950, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement_basis(shin_be_t1200, observed).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1500, 0.67).
narrative_ontology:measurement_basis(shin_be_t1500, observed).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1700, 0.66).
narrative_ontology:measurement_basis(shin_be_t1700, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1868, 0.67).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t750, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 750, 0.25).
narrative_ontology:measurement_basis(shin_su_t750, projected).
narrative_ontology:measurement(shin_su_t950, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 950, 0.38).
narrative_ontology:measurement_basis(shin_su_t950, observed).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1200, 0.51).
narrative_ontology:measurement_basis(shin_su_t1200, observed).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1500, 0.58).
narrative_ontology:measurement_basis(shin_su_t1500, observed).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1700, 0.59).
narrative_ontology:measurement_basis(shin_su_t1700, observed).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1868, 0.58).
narrative_ontology:measurement_basis(shin_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__syncretic_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, meiji_shinto_restoration).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, modern_shinto_buddhism_separation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel shinbutsu_ontological_commitment. The syncretic reading asserts genuine ontological integration via honji-suijaku metaphysics. The partition reading asserts functional separation without ontological integration (Shinto for life/community, Buddhism for death/salvation). The incoherence reading asserts that no stable ontological commitment was achieved—institutional tolerance of contradiction held the arrangement together. All three readings cite identical historical phenomena but interpret their meaning differently. They form a constraint family linked by network.affects_constraints: each reading influences the others (the coherence of one reading depends partly on the incoherence of alternatives), but none forecloses another (they are held by different institutional and scholarly seats). The Meiji Restoration (1868) marks an institutional break where the syncretic framework is officially abandoned in favor of Shinto restoration, making this a historical endpoint for the constraint's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
