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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-suijaku syncretic ontology (unified cosmological order reading)
 *   domain: religious_studies/ontology
 *
 * SUMMARY:
 *   Under honji-suijaku metaphysics, kami (Shinto deities) are understood as
 *   manifestations (suijaku) of buddhas or bodhisattvas, who are the
 *   'original essence' (honji). This reading licenses Buddhist institutional
 *   authority to interpret and oversee kami veneration as part of a unified
 *   cosmological order. Buddhist monasteries and shrine-temple complexes
 *   (jingūji) administered by Buddhist priests became the institutional locus
 *   for interpreting both Buddhist doctrine and kami theology. The reading
 *   subordinates autonomous Shinto priesthoods and local kami traditions
 *   within a Buddhist epistemological framework. The claim/metric gap is
 *   intentional: the syncretic reading claims to articulate genuine
 *   cosmological coherence (rope-like coordination function), while the
 *   authored metrics describe substantial extraction (0.68 extractiveness,
 *   0.72 suppression) and active enforcement of institutional hierarchy — the
 *   engine measures this divergence as a tangled-rope structure where
 *   coordination function and extraction are both present but asymmetric.
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy (sets and enforces the syncretic reading; institutionally powerful; claims interpretive authority)
 *   - Autonomous Shinto priesthood (bears cost of subordination; identity-locked to kami tradition; moderate power but constrained by Buddhist institutional dominance)
 *   - Syncretic temple-shrine complexes (benefit from unified administration and integrated ritual authority; powerful at regional scale)
 *   - Local kami traditions (trapped; reinterpreted and subordinated; powerless to assert autonomous cosmology)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.72).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-suijaku syncretic ontology (unified cosmological order reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious_studies/ontology").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '7309f0c1-9042-4913-847e-14873ffbae5b').
narrative_ontology:cs_kernel_codification('7309f0c1-9042-4913-847e-14873ffbae5b', formalized).
narrative_ontology:cs_authority_grounding('7309f0c1-9042-4913-847e-14873ffbae5b', lineage).
narrative_ontology:cs_interpretation_layer_present('7309f0c1-9042-4913-847e-14873ffbae5b').
narrative_ontology:cs_reading_relation('7309f0c1-9042-4913-847e-14873ffbae5b', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('7309f0c1-9042-4913-847e-14873ffbae5b', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('7309f0c1-9042-4913-847e-14873ffbae5b', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, overridden).
narrative_ontology:cs_axiom_grounding('7309f0c1-9042-4913-847e-14873ffbae5b', kami_are_buddha_manifestations, deontological).
narrative_ontology:cs_axiom('7309f0c1-9042-4913-847e-14873ffbae5b', foundational, unified_cosmology_grounded_in_honji_suijaku).
narrative_ontology:cs_axiom_status(unified_cosmology_grounded_in_honji_suijaku, holdable).
narrative_ontology:cs_axiom_grounding('7309f0c1-9042-4913-847e-14873ffbae5b', unified_cosmology_grounded_in_honji_suijaku, conventional).
narrative_ontology:cs_reference_frame('7309f0c1-9042-4913-847e-14873ffbae5b', unified_cosmological_order).
narrative_ontology:cs_drift_state('7309f0c1-9042-4913-847e-14873ffbae5b', edo_meiji_transition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7309f0c1-9042-4913-847e-14873ffbae5b', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, syncretic_temple_complexes).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, autonomous_shinto_priesthood).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, local_kami_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, state_patronage_apparatus).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, state_patronage_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the syncretic reading through scriptural interpretation, theological education, and institutional authority. Claims doctrinal competence to define kami ontology as manifestations of buddha-nature. Administers jingūji (shrine-temple complexes) under Buddhist authority. Benefits from interpretive monopoly and institutional integration.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Bears the cost of accepting kami as subordinate manifestations rather than autonomous beings. Professional identity is fused with kami veneration, making exit from the syncretic framework structurally impossible without dissolving priesthood status. Loses interpretive authority and control over kami theology. Constrained by Buddhist institutional dominance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, autonomous_shinto_priesthood, payer,
    moderate, biographical, identity_locked, local).

% Benefit from unified administration under the syncretic reading. Draw on both Buddhist and Shinto ritual repertoires while remaining under single institutional authority. Serve local populations seeking integrated spiritual services. Collect patronage from devotees and state support.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, syncretic_temple_complexes, beneficiary,
    powerful, generational, constrained, regional).

% Community-based kami veneration practices are reinterpreted and subordinated within Buddhist cosmological frameworks. Local practitioners continue kami worship but under frameworks articulated by Buddhist authorities. No voice in determining kami ontology. Cannot organize alternative cosmologies without institutional resistance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, local_kami_traditions, payer,
    powerless, generational, trapped, local).

% Benefits from a unified cosmological framework that allows state patronage to flow through integrated Buddhist-Shinto institutions under single authority structure. Simplifies religious administration. Also bears cost of suppressing Shinto reformers who would organize rival priesthoods.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, state_patronage_apparatus, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, state_patronage_apparatus, payer).

% Systematically excluded from institutional authority. Argue kami are autonomous beings worthy of independent theology. Suppressed through institutional control of temples, scriptural education, and state favor. Their alternative cosmology (autonomous Shinto) is barred from institutional expression during the syncretic period.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, partisan_shinto_reformers, excluded,
    organized, generational, constrained, national).

% Analyze the coherence and historical status of the syncretic reading. Examine whether honji-suijaku metaphysics functioned as genuine cosmological commitment or as institutional convenience. Study textual, institutional, and ethnographic evidence. Produce competing historical narratives about whether the reading was ever genuinely unified or merely institutionally enforced.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, doctrinal_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the practical problem of how Buddhist and Shinto institutions, serving overlapping populations in the same sacred geography, can coexist without institutional conflict. The syncretic reading provides a single cosmological framework (honji-suijaku metaphysics) that licenses unified administration of shrine-temple complexes and justifies Buddhist institutional authority over both Buddhist doctrine and kami veneration.
% TRANSFER_FUNCTION: Transfers interpretive authority over kami from autonomous Shinto priesthoods to the Buddhist institutional hierarchy. Transfers administrative control of local kami traditions from autonomous shrines to integrated complexes administered under Buddhist authority. Redistributes patronage and institutional prestige toward Buddhist centers and away from autonomous Shinto priesthoods.
% ABSENT_VOICES: Autonomous Shinto priesthoods are excluded from determining doctrinal authority. Partisan Shinto reformers arguing for autonomous kami theology are systematically excluded from institutional platforms. Local kami-tradition practitioners, while continuing their practice, are not invited to articulate kami ontology — that task is claimed exclusively by Buddhist scholars.
% DISAPPEARANCE_RATIONALE: If the syncretic reading disappeared, shrine-temple complexes would bifurcate; Shinto priesthoods would reassert interpretive autonomy; Buddhist-Shinto institutional competition would resurface; local kami traditions would recover authority to articulate their own ontology; the religious landscape would reorganize around competing theologies and independent priesthoods.
% FOUNDING_PROBLEM: Early medieval Japan had both Buddhist institutions (claiming universal salvific truth) and indigenous Shinto priesthoods (serving local kami and community needs) occupying the same space and serving overlapping populations. Without a framework allowing coexistence, institutional conflict was likely.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional sources attest the problem was real and coexistence required a unified cosmological framework. Modern historians (Kuroda Toshio on 'kenmon taisei'; Grapard on sacred geography) attest the problem existed and the syncretic solution was historically deployed. Shinto reformers and scholars outside Buddhist institutions attest the problem was less about metaphysical coherence than about institutional competition, and that alternative solutions (domain partition, institutional autonomy without ontological integration) could have worked without requiring Buddhist subordination of Shinto.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).

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
 *   Extractiveness rises from 0.42 (early 900s, projected) to 0.68 (by 1550+, stable) over 650 years. This trajectory tracks the consolidation of the syncretic framework as institutional doctrine and the suppression of competing Shinto theologies. Theater ratio climbs from 0.2 to 0.41 over the same period: as the framework becomes institutionalized, more enforcement activity shifts from articulating the coherence of the unified cosmology (functional activity) to maintaining institutional subordination (performative assertion of Buddhist authority). Suppression requirement rises from 0.48 to 0.72 as competing kami theologies must be actively suppressed — this is not a constraint that persists through general agreement but through institutional power. The measurements track a constraint that begins with some genuine coordination benefit (Buddhist and Shinto institutions need to coexist) but increasingly functions as a mechanism for extracting interpretive authority and institutional control from the Shinto priesthood.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist institutional seat: the syncretic reading articulates genuine cosmological truth and provides a coherent framework for unified spiritual practice. The constraint appears as rope (coordination with minimal asymmetry). From the autonomous Shinto priesthood seat: the same structure appears as enforced subordination disguised in metaphysical language. The constraint appears as snare (pure extraction, presented as coherence). The engine computes per-seat classifications from the structural data — beneficiary/victim declarations, exit options, enforced hierarchy — and produces divergent type classifications that reflect this perspectival gap. The authored claim (tangled_rope) sits between these poles, acknowledging both the coordination function (that Buddhist-Shinto coexistence is a real problem requiring solution) and the extraction (that the solution installed Buddhist institutional authority as the sole interpreter of both traditions).
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy is the structural beneficiary (d near 0.1–0.2): it claims interpretive authority, administers integrated institutions, collects patronage as a unified authority structure. Autonomous Shinto priesthoods are the primary targets (d near 0.8–0.9): they bear the cost of subordination, lose interpretive autonomy, are identity-locked to kami tradition and cannot exit without dissolving their professional/spiritual identity. Local kami traditions are trapped (d = 0.95+): they have no exit options, no voice in cosmological interpretation, and no ability to assert kami as self-subsisting beings. The state apparatus sits near symmetric (d = 0.5): it benefits from unified religious administration but also bears the cost of suppressing rival priesthoods and managing the potential conflict between the two traditions.
 *
 * MANDATROPHY ANALYSIS:
 *   The syncretic reading's founding problem (how to let Buddhist and Shinto institutions coexist without direct conflict) is contested as to whether it remains 'live' or has become 'dead.' The Buddhist institutional reading maintains the problem is perpetually live because continuous theological articulation of the unified cosmology is necessary. Shinto reformers and modern scholars argue the problem shifted by the Edo period (1603+) from a genuine institutional coordination problem to a matter of maintaining Buddhist authority over Shinto — the founding problem was solved (coexistence is stable) but the constraint persists for extractive reasons. This is a classic mandatrophy signature: the founding problem's status (live vs. dead) diverges sharply between institutional seats, and the divergence tracks whether the constraint persists because it solves a coordination problem or because institutional actors benefit from maintaining the subordination. The theater ratio rising to 0.41 corroborates this: as extractive enforcement replaces genuine cosmological articulation, more of the constraint's operation becomes performative assertion of authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_coherence_vs_institutional_convenience,
    'Did the syncretic reading (kami as manifestations of buddhas) function as a genuine metaphysical commitment held by practitioners and scholars, or as an institutional fiction that allowed coexistence without requiring actual cosmological coherence?',
    'Textual analysis of honji-suijaku theology in Buddhist philosophical texts versus administrative documents; comparative study of doctrinal sophistication across regions and time periods; ethnographic/historical reconstruction of whether local practitioners internalized the syncretic cosmology or treated it as imposed doctrine.',
    'If genuine metaphysical commitment: the constraint is primarily a rope with coordination function dominating extraction. If institutional convenience: the constraint is primarily tangled-rope with suppression of autonomous Shinto theology as the dominant extractive function. If mixed: measurement of the ratio would locate the constraint within the rope/tangled-rope spectrum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_coherence_vs_institutional_convenience, empirical, 'Whether the syncretic ontology was authentically held versus instrumentally deployed.').

omega_variable(
    alternative_coordination_possibility,
    'Could the coordination problem (Buddhist and Shinto institutions coexisting in the same sacred space) have been solved by a non-hierarchical partition (separate domains of authority) rather than by syncretic subordination?',
    'Comparative historical analysis of regions or periods where Buddhist-Shinto coexistence was negotiated via domain separation (Buddhist temples handling afterlife/salvation; Shinto shrines handling life-cycle/local welfare) without subordination claims. Analysis of whether partition frameworks were actively rejected or never seriously entertained.',
    'If partition was a viable alternative that was rejected: the subordination of Shinto within the syncretic framework is exposed as extractive hierarchy rather than necessary coordination. If partition was genuinely unworkable or rejected for compelling reasons: more of the measured extraction is justifiable coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_possibility, conceptual, 'Whether hierarchical syncretic integration was the only possible coordination solution.').

omega_variable(
    autonomous_kami_theology_suppression_mechanism,
    'Is the suppression of autonomous Shinto theology (kami as self-subsisting beings) structural (enforced through institutional control and doctrinal authority) or internalized (Shinto practitioners came to believe kami were manifestations)?',
    'Analysis of Shinto reformist texts and movements (Edo-period National Learning, Meiji-era Shinto restoration) to identify what alternative kami theology was suppressed and how; examination of whether suppression required ongoing institutional enforcement or whether it became naturalized in practice.',
    'If structural: suppression is an active extractive cost; if internalized: the constraint''s effective suppression is higher than the scalar measure suggests, and the constraint would persist even after institutional enforcement weakened. If mixed: ratio analysis would clarify the balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomous_kami_theology_suppression_mechanism, empirical, 'Mechanism of suppression of autonomous Shinto kami theology.').

omega_variable(
    kernel_reading_contest_location,
    'This syncretic reading positions kami and buddhas within a unified ontological order grounded in honji-suijaku metaphysics. The sibling readings (partition_reading and incoherence_reading) locate the kernel differently. Where does the core disagreement lie — in the metaphysics itself, in the institutional structures that instantiate it, or in the historical question of whether it was ever genuinely unified?',
    'Explicit engagement with sibling readings: does partition_reading accept the metaphysics but reject institutional integration? Does incoherence_reading accept neither? Analysis of what factual or conceptual claims each reading depends on.',
    'Clarifies whether the three readings are incompatible on metaphysical grounds (forecloses) or simply emphasize different aspects of a complex institutional history (coexists_with). Routes the committer contest to the appropriate omega-resolution surface.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Structural location of disagreement among kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 900, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.2).
narrative_ontology:measurement_basis(shin_tr_t900, projected).
narrative_ontology:measurement(shin_tr_t1050, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1050, 0.28).
narrative_ontology:measurement_basis(shin_tr_t1050, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1200, 0.35).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1400, 0.4).
narrative_ontology:measurement_basis(shin_tr_t1400, observed).
narrative_ontology:measurement(shin_tr_t1550, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1550, 0.41).
narrative_ontology:measurement_basis(shin_tr_t1550, observed).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1600, 0.41).
narrative_ontology:measurement_basis(shin_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.42).
narrative_ontology:measurement_basis(shin_be_t900, projected).
narrative_ontology:measurement(shin_be_t1050, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1050, 0.54).
narrative_ontology:measurement_basis(shin_be_t1050, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement_basis(shin_be_t1200, observed).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1400, 0.67).
narrative_ontology:measurement_basis(shin_be_t1400, observed).
narrative_ontology:measurement(shin_be_t1550, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1550, 0.68).
narrative_ontology:measurement_basis(shin_be_t1550, observed).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1600, 0.68).
narrative_ontology:measurement_basis(shin_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.48).
narrative_ontology:measurement_basis(shin_su_t900, projected).
narrative_ontology:measurement(shin_su_t1050, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1050, 0.58).
narrative_ontology:measurement_basis(shin_su_t1050, observed).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1200, 0.66).
narrative_ontology:measurement_basis(shin_su_t1200, observed).
narrative_ontology:measurement(shin_su_t1400, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1400, 0.71).
narrative_ontology:measurement_basis(shin_su_t1400, observed).
narrative_ontology:measurement(shin_su_t1550, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1550, 0.72).
narrative_ontology:measurement_basis(shin_su_t1550, observed).
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1600, 0.72).
narrative_ontology:measurement_basis(shin_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__syncretic_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, jinguj_administrative_integration).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinto_priesthood_authority_suppression).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'shinbutsu_ontological_commitment'. The syncretic_reading presents kami-buddha unification as stable metaphysical coherence. Sibling readings (partition_reading: autonomous domains; incoherence_reading: institutionalized contradiction) instantiate different constraints with different ε values and beneficiary structures. All three readings are linked via network.affects_constraints to enable cross-reading analysis. The syncretic reading has the highest extractiveness (0.68) because it requires the most active suppression of alternative theologies. Partition and incoherence readings have lower extractiveness because they do not claim to resolve the kami-buddha relationship — they either separate it or accept its contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_commitment__syncretic_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
