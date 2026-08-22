% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism â Diffuse Democratic Contestation of Constitutional Meaning
 *   domain: constitutional law / political theory / institutional design
 *
 * SUMMARY:
 *   This constraint instantiates the popular_constitutionalism_reading of the
 *   basic_law_interpretive_authority kernel. Under this reading,
 *   constitutional meaning is generated through ongoing democratic
 *   contestation rather than through the final pronouncements of courts or
 *   legislatures. The constraint denies any institution terminal interpretive
 *   authority, distributing both interpretive voice and gridlock costs across
 *   the democratic public and institutional actors. It is claimed as a
 *   democratizing rope-like arrangement, but the metrics independently report
 *   moderate extractiveness borne by institutional actors who lose finality.
 *
 * KEY AGENTS:
 *   - popular_movements: Primary beneficiary (organized/mobile) â gain interpretive standing without institutional capture
 *   - democratic_citizenry: Diffuse beneficiary (moderate/constrained) â gain democratic control of meaning at the cost of uncertainty
 *   - judicial_institutions: Primary payer (institutional/constrained) â bear the loss of final constitutional authority
 *   - legislative_assemblies: Primary payer (institutional/constrained) â lose the ability to claim final democratic settlement
 *   - administrative_agencies: Secondary payer (institutional/constrained) â bear gridlock costs from contested legal boundaries
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â documents the structural trade-offs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.38).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism â Diffuse Democratic Contestation of Constitutional Meaning").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional law / political theory / institutional design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, '3ba30444-8452-4191-a862-687aea12d9b3').
narrative_ontology:cs_kernel_codification('3ba30444-8452-4191-a862-687aea12d9b3', distributed).
narrative_ontology:cs_authority_grounding('3ba30444-8452-4191-a862-687aea12d9b3', distributed).
narrative_ontology:cs_reading_relation('3ba30444-8452-4191-a862-687aea12d9b3', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('3ba30444-8452-4191-a862-687aea12d9b3', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('3ba30444-8452-4191-a862-687aea12d9b3', foundational, no_terminal_interpreter).
narrative_ontology:cs_axiom_status(no_terminal_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('3ba30444-8452-4191-a862-687aea12d9b3', no_terminal_interpreter, deontological).
narrative_ontology:cs_axiom('3ba30444-8452-4191-a862-687aea12d9b3', foundational, democratic_contestation_generates_meaning).
narrative_ontology:cs_axiom_status(democratic_contestation_generates_meaning, holdable).
narrative_ontology:cs_axiom_grounding('3ba30444-8452-4191-a862-687aea12d9b3', democratic_contestation_generates_meaning, conventional).
narrative_ontology:cs_reference_frame('3ba30444-8452-4191-a862-687aea12d9b3', ongoing_democratic_contestation).
narrative_ontology:cs_drift_state('3ba30444-8452-4191-a862-687aea12d9b3', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ba30444-8452-4191-a862-687aea12d9b3', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_citizenry).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_institutions).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, legislative_assemblies).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, administrative_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain direct interpretive standing in constitutional politics without needing to capture courts or legislatures. Shift constitutional meaning through mobilization, protest, and cultural argument. Can exit to other political arenas but lose constitutional voice when doing so.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, generational, mobile, national).

% Hold diffuse interpretive authority as members of the democratic public. Benefit from constitutional meaning that tracks popular will rather than elite institutional settlement. Bear diffuse costs of legal uncertainty and slower resolution of fundamental disputes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_citizenry, beneficiary,
    moderate, generational, constrained, national).

% Lose the claim to final constitutional authority. Decisions remain perpetually open to democratic contestation and potential override through popular mobilization. Must operate under conditions of eroded finality and ongoing legitimacy challenges without being able to exit the interpretive role.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_institutions, payer,
    institutional, generational, constrained, national).

% Cannot claim final democratic mandate to settle constitutional meaning. Legislation remains subject to reinterpretation through extra-electoral contestation. Bear procedural and political costs of operating without stable constitutional boundaries or terminal authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legislative_assemblies, payer,
    institutional, biographical, constrained, national).

% Face legal uncertainty when constitutional boundaries are perpetually contested, complicating rule-making and enforcement. Cannot rely on settled judicial or legislative interpretation to guide regulatory action, increasing compliance and litigation costs.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, administrative_agencies, payer,
    institutional, biographical, constrained, national).

% Analyze competing claims to interpretive authority and document the institutional costs and democratic benefits of diffuse constitutional contestation. Observe the divergence between institutional practice and popular constitutionalist theory.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single institution from monopolizing constitutional meaning, preserving space for democratic participation and ongoing popular engagement with foundational law.
% TRANSFER_FUNCTION: Moves interpretive authority from professional legal and legislative institutions to diffuse democratic publics and social movements; moves gridlock costs and legal uncertainty to institutional actors who can no longer claim final settlement.
% ABSENT_VOICES: Minority groups vulnerable to majoritarian reinterpretation of rights protections; institutional legal professionals whose expertise is devalued by the democratization of interpretation; future generations who inherit the instability of perpetually contestable meaning.
% DISAPPEARANCE_RATIONALE: If this constraint vanished and terminal institutional authority were re-established, constitutional politics would shift from diffuse popular contestation to centralized adjudication or legislation. Social movements would lose their claim to direct interpretive standing, and courts or legislatures would regain the ability to settle disputes with finality.
% FOUNDING_PROBLEM: How to legitimate constitutional interpretation in a democracy without allowing a single unelected or even elected institution to dominate the people's fundamental law.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional theorists from outside the popular constitutionalism campâdefenders of judicial review and parliamentary sovereignty alikeâcorroborate that the countermajoritarian difficulty is the central problem, though they propose different solutions. Political scientists studying democratic backsliding also attest to the live tension between institutional finality and popular sovereignty.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).
:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.42 reflects the moderate but real extraction of institutional finality from courts and legislatures, distributed as gridlock costs and legal uncertainty. Suppression at 0.38 captures the normative suppression of judicial and legislative claims to final authority, enforced through democratic mobilization and political pressure rather than legal coercion. Theater ratio at 0.20 indicates mostly genuine contestation with modest performative dimensions. Accessibility collapse at 0.35 reflects that alternatives (judicial supremacy, parliamentary sovereignty) remain visible and institutionally viable. Resistance at 0.60 captures strong institutional pushback from courts and legislatures against the erosion of their authority. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The popular_movements seat experiences the constraint as democratizing and empowering, with directionality near the beneficiary end. The judicial_institutions and legislative_assemblies seats experience it as extractive and erosive, with directionality near the target end. The democratic_citizenry seat sits nearer symmetric, gaining interpretive voice while bearing diffuse costs of instability. The engine computes this divergence from the structural role and exit data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (popular_movements, democratic_citizenry) gain interpretive standing and democratic legitimacy; their directionality is low. Victims/payers (judicial_institutions, legislative_assemblies, administrative_agencies) lose terminal authority and bear gridlock costs; their directionality is high. The extraction is diffuseâno single seat captures the transferred authority, which dissipates into ongoing contestationâso gain_flow is 'diffuse'.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe countermajoritarian difficultyâremains live, so this is not a piton case. The constraint coordinates genuine democratic participation while simultaneously extracting institutional finality. Classifying it as tangled_rope prevents mislabeling it as pure coordination (which would ignore the real costs to institutional governance) or pure extraction (which would ignore the genuine democratic function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    basic_law_kernel_reading_location,
    'This constraint instantiates the popular_constitutionalism_reading of kernel basic_law_interpretive_authority. Sibling readings (judicial_supremacy_reading, parliamentary_sovereignty_reading) assign terminal authority to courts or legislature respectively. The structural disagreement is located at the finality node: does constitutional interpretation require a terminal institutional adjudicator?',
    'Cross-jurisdictional comparison of constitutional regimes to identify whether any framework successfully operates without an implicit final authority, even if displaced onto popular mobilization.',
    'If all operational constitutional orders implicitly assign final authority somewhere, popular constitutionalism functions as a normative aspiration rather than an operative constraint, altering its extractiveness and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(basic_law_kernel_reading_location, conceptual, 'Whether popular constitutionalism can operate as an actual authority structure or only as a normative critique.').

omega_variable(
    diffuse_cost_asymmetry,
    'Gridlock costs under popular constitutionalism are distributed across multiple institutional sites. Is this diffusion itself the extraction mechanism, or does the absence of a concentrated beneficiary make the constraint symmetric coordination despite the costs?',
    'Tracking whether any agent or class systematically captures the benefits of institutional paralysis (e.g., executive aggrandizement during gridlock).',
    'If costs are purely diffuse with no capturer, the constraint may compute as rope or piton rather than tangled rope; if some actor benefits from gridlock, tangled rope or snare classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_cost_asymmetry, empirical, 'Whether distributed gridlock costs hide concentrated extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the basic_law_interpretive_authority kernel, which decomposes into three structurally distinct constraints based on where final interpretive authority is located: courts (judicial_supremacy_reading), legislature (parliamentary_sovereignty_reading), or diffuse democratic contestation (this reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
