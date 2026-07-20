% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation Reading of the Imposition Mechanism Kernel
 *   domain: historical sociology / state formation / cultural authority
 *
 * SUMMARY:
 *   This constraint story models the hybrid legitimation reading of the
 *   imposition mechanism kernel: the historical pattern by which new norms
 *   achieve legitimacy in large-scale polities through a combination of
 *   symbolic authority transfer (the emperor's personal example) and
 *   institutional incentives (examinations, patronage, tax privileges). The
 *   mechanism operates neither as pure bottom-up cultural diffusion
 *   (endogenous climb) nor as pure coercive imposition (exogenous override),
 *   but as a stratified adoption cascade in which elites are captured first
 *   through incentives and then serve as local enforcement nodes, while the
 *   imperial charisma lowers the normative cost of compliance. This is a
 *   contested kernel reading; the JSON instantiates only the hybrid reading,
 *   with sibling readings documented in the cs_structure and commentary.
 *
 * KEY AGENTS:
 *   - central_state: Agenda-setter that designs the norm package and incentive structure
 *   - imperial_personage: Charismatic beneficiary whose example provides the symbolic center
 *   - regional_elites: Dual-positioned beneficiaries and payers who receive incentives and enforce locally
 *   - peasant_communities: Payers who absorb downstream compliance costs without receiving incentives
 *   - local_customary_institutions: Excluded actors displaced by imperial norm penetration
 *   - comparative_historical_sociologists: Analytical observers who test the mechanism across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.62).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.55).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation Reading of the Imposition Mechanism Kernel").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical sociology / state formation / cultural authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e558846f-e0f3-43b8-8f16-4ef4b9616a43').
narrative_ontology:cs_kernel_codification('e558846f-e0f3-43b8-8f16-4ef4b9616a43', distributed).
narrative_ontology:cs_authority_grounding('e558846f-e0f3-43b8-8f16-4ef4b9616a43', lineage).
narrative_ontology:cs_interpretation_layer_present('e558846f-e0f3-43b8-8f16-4ef4b9616a43').
narrative_ontology:cs_reading_relation('e558846f-e0f3-43b8-8f16-4ef4b9616a43', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('e558846f-e0f3-43b8-8f16-4ef4b9616a43', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('e558846f-e0f3-43b8-8f16-4ef4b9616a43', foundational, charismatic_exemplarism_as_legitimation_basis).
narrative_ontology:cs_axiom_status(charismatic_exemplarism_as_legitimation_basis, holdable).
narrative_ontology:cs_axiom_grounding('e558846f-e0f3-43b8-8f16-4ef4b9616a43', charismatic_exemplarism_as_legitimation_basis, empirically_contingent).
narrative_ontology:cs_axiom('e558846f-e0f3-43b8-8f16-4ef4b9616a43', foundational, institutional_incentives_sufficient_for_elite_capture).
narrative_ontology:cs_axiom_status(institutional_incentives_sufficient_for_elite_capture, holdable).
narrative_ontology:cs_axiom_grounding('e558846f-e0f3-43b8-8f16-4ef4b9616a43', institutional_incentives_sufficient_for_elite_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('e558846f-e0f3-43b8-8f16-4ef4b9616a43', imperial_charisma_and_patronage_synthesis).
narrative_ontology:cs_drift_state('e558846f-e0f3-43b8-8f16-4ef4b9616a43', bureaucratic_rationalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e558846f-e0f3-43b8-8f16-4ef4b9616a43', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, central_state).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_personage).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, regional_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_communities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, local_customary_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, regional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and promulgates new norms under the symbolic aegis of the imperial personage, administers institutional incentives such as examinations, patronage, and tax remissions to secure elite compliance, and maintains the ideological apparatus that presents imperial example as the normative standard. Its own legitimacy is recursively bound to the success of this mechanism.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, central_state, agenda_setter,
    institutional, generational, constrained, continental).

% Provides the personal example and charismatic center that makes the symbolic authority transfer legible. The personage gains enhanced prestige and historical stature from the norm cascade, but is constrained by the same ritual and ideological apparatus that elevates the example â deviation risks delegitimation.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_personage, beneficiary,
    powerful, biographical, constrained, continental).

% Receive institutional incentives â examination access, patronage, tax privileges, status markers â in exchange for adopting and locally enforcing imperial norms. They bear the transaction costs of cultural retooling and the political risk of enforcing unpopular norms on local populations. Their social position depends on continued participation in the imperial system.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, regional_elites, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, regional_elites, payer).

% Absorb the downstream costs of norm compliance â altered agricultural calendars, new tax obligations encoded in norms, loss of local ritual autonomy, and submission to elite enforcers who are themselves incentivized by the center. They are the last stratum to adopt and receive no institutional incentives.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_communities, payer,
    powerless, biographical, trapped, local).

% Village temples, lineage elders, local deities, and customary legal forums that previously managed normative life. They are structurally excluded from the imperial bargain and are gradually displaced or subordinated as imperial norms penetrate local society. Their exclusion is necessary for the hybrid mechanism to appear successful.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, local_customary_institutions, excluded,
    powerless, generational, trapped, local).

% Analyze the mechanism across multiple imperial formations, comparing adoption timelines, elite compliance rates, and resistance patterns. They occupy an analytical seat outside the historical constraint, though their theoretical commitments may align with one reading of the kernel.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, comparative_historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the adoption of new norms across a stratified, territorially dispersed population by aligning elite incentives with central authority and using imperial charisma to lower resistance costs.
% TRANSFER_FUNCTION: Moves symbolic legitimacy and material or status rewards from the imperial center to regional elites, and compliance obligations from regional elites down to local communities, in exchange for norm adoption.
% ABSENT_VOICES: Local customary institutions and non-elite populations whose practices are displaced by imperial norms are present only as objects of adoption, not as parties to the legitimation bargain; their resistance appears in the record as banditry or superstition rather than as normative contestation.
% DISAPPEARANCE_RATIONALE: If the hybrid legitimation mechanism vanished, imperial norms would lose their rapid diffusion channel; regional elites would revert to local autonomy or require direct coercion, and the unified normative order of the polity would fragment into regional particularism.
% FOUNDING_PROBLEM: How to unify a large, culturally heterogeneous polity under new norms without relying solely on expensive permanent coercion or waiting for uncertain generational cultural drift.
% FOUNDING_PROBLEM_CORROBORATION: Comparative imperial historians corroborate that pre-modern states faced integration problems requiring such mechanisms; however, sociologists emphasizing endogenous cultural change and political scientists emphasizing pure coercion contest whether the hybrid mechanism was the operative solution, or merely the legitimating discourse of a primarily coercive process.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the mechanism systematically channels material and status rewards to early-adopting elites while extracting compliance from lower strata; the asymmetry is structural, not incidental. Suppression (0.55) is moderate because the hybrid mechanism deliberately substitutes incentives and symbolic authority for raw coercion, though enforcement capacity remains necessary to handle holdouts. Theater ratio (0.30) reflects the genuinely performative dimension of imperial example â the emperor's public rituals, edicts, and displays â but acknowledges that over the interval the performance gives way to routinized bureaucratic enforcement. Accessibility collapse (0.70) is high because once imperial norms are institutionalized through elite networks, local alternatives lose their institutional carriers and become socially invisible. Resistance (0.45) is moderate: localized and sporadic, dampened by the elite capture mechanism but never fully eliminated.
 *
 * PERSPECTIVAL GAP:
 *   The central state and imperial personage experience the constraint as a successful coordination device that generates legitimacy and order with lower coercion costs than pure override. Regional elites experience it as a calculable exchange of autonomy for status and material gain. Peasant communities and local customary institutions experience the same structure as externally imposed extraction, where the soft face of incentives does not reach them and the hard face of elite enforcement does. The engine computes these divergent seat classifications from the structural asymmetry in power, exit options, and beneficiary or victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (central_state, imperial_personage, regional_elites) derive low directionality values: they are structurally subsidized by the constraint. The regional_elites' dual position (beneficiary and payer) places them nearer symmetric than the center, reflecting their intermediate structural location. Victims (peasant_communities, local_customary_institutions) derive high directionality values: they are extraction targets with minimal exit. The exclusion of local customary institutions is structurally necessary for the mechanism â their presence would reveal that adoption is shallower than the imperial narrative claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mandatrophy mislabeling by preserving the genuine coordination function (large-scale norm integration without total reliance on coercion) while refusing to collapse the mechanism into benign coordination. Without the Tangled Rope classification, the mechanism could be misread as either a benign Rope (ignoring the stratified extraction and elite-mass asymmetry) or a pure Snare (ignoring that symbolic authority and incentives do partially substitute for coercion and that elites genuinely gain). The classification requires active enforcement because the mechanism only persists as long as the state maintains the incentive stream and the symbolic apparatus; if enforcement atrophies, the norm cascade stops and local alternatives resurface, distinguishing it from a self-sustaining Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_vs_pure_mechanism_boundary,
    'Is the hybrid legitimation reading a distinct mechanism, or an ex post facto rationalization that combines elements of pure coercion and pure endogenous adoption without predictive power?',
    'Comparative historical analysis across multiple imperial formations testing whether hybrid mechanisms produce measurably different adoption timelines and resistance patterns than pure coercion or pure bottom-up models.',
    'If the hybrid reading lacks predictive distinction, it collapses into descriptive convenience and the kernel should be reduced to two readings; if distinct, the three-reading family is structurally warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_vs_pure_mechanism_boundary, conceptual, 'Whether the hybrid reading is a distinct mechanism or a descriptive composite').

omega_variable(
    enforcement_cost_ambiguity,
    'Do the moderate enforcement costs represent genuine coordination overhead, or disguised coercion that the hybrid framing obscures?',
    'Archival analysis of state expenditure records and resistance documentation comparing claimed incentive costs versus actual coercive expenditure in norm imposition campaigns.',
    'If enforcement costs are primarily coercive, the constraint is closer to exogenous_override; if primarily incentive-based, the hybrid reading is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_ambiguity, empirical, 'Whether enforcement costs are coordination or disguised coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_legitimation_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(hybrid_legitimation_tr_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(hybrid_legitimation_tr_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(hybrid_legitimation_tr_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(hybrid_legitimation_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(hybrid_legitimation_tr_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(hybrid_legitimation_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hybrid_legitimation_be_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(hybrid_legitimation_be_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hybrid_legitimation_be_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(hybrid_legitimation_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(hybrid_legitimation_be_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_legitimation_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hybrid_legitimation_su_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(hybrid_legitimation_su_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(hybrid_legitimation_su_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(hybrid_legitimation_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(hybrid_legitimation_su_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The imposition_mechanism_kernel decomposes into three structurally distinct constraints because the natural-language label 'how new norms achieve legitimacy' conflates three mechanisms with different epsilon profiles: bottom-up endogenous adoption (low extraction, coordination-heavy), top-down coercive override (high extraction, suppression-heavy), and hybrid symbolic-incentive transfer (moderate extraction, stratified coordination). Each reading has a distinct beneficiary and victim structure and enforcement logic. They are linked as a constraint family because empirical cases often invoke all three, but they must be modeled separately to preserve epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
