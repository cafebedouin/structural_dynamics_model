% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Popular Constitutionalism Interpretive Authority
 *   domain: constitutional law / political theory / institutional design
 *
 * SUMMARY:
 *   This constraint story models the popular_constitutionalism_reading of the
 *   basic_law_interpretive_authority kernel. Under this reading,
 *   constitutional meaning is produced through ongoing democratic
 *   contestation rather than by terminal adjudication from courts or
 *   legislatures. The constraint binds institutional actors by denying them
 *   final interpretive authority, while empowering civil society mobilizers.
 *   It functions as coordination against institutional monopoly but
 *   simultaneously extracts settled authority from the judiciary,
 *   legislature, and executive, distributing gridlock costs across multiple
 *   sites. The claim is tangled_rope because the coordination (preventing
 *   tyranny of a single institution) and extraction (perpetual uncertainty,
 *   gridlock, eroded institutional capacity) are structurally inseparable and
 *   require active enforcement against institutional supremacy claims.
 *
 * KEY AGENTS:
 *   - civil_society_mobilizers: Primary beneficiary (organized/mobile) â gains interpretive standing from open contestation
 *   - judiciary: Primary target (institutional/constrained) â loses terminal authority
 *   - legislature: Primary target (institutional/constrained) â loses final interpretive authority
 *   - executive_branch: Secondary target (institutional/constrained) â bears gridlock implementation costs
 *   - political_minorities: Excluded voice (powerless/trapped) â needs settled protections against reopening
 *   - constitutional_theorists: Analytical observer (analytical/analytical) â maps the contest without controlling it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.64).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.59).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.59).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional law / political theory / institutional design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, '56660429-efb3-4fd0-932f-8a28d3dee506').
narrative_ontology:cs_kernel_codification('56660429-efb3-4fd0-932f-8a28d3dee506', distributed).
narrative_ontology:cs_authority_grounding('56660429-efb3-4fd0-932f-8a28d3dee506', distributed).
narrative_ontology:cs_reading_relation('56660429-efb3-4fd0-932f-8a28d3dee506', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('56660429-efb3-4fd0-932f-8a28d3dee506', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('56660429-efb3-4fd0-932f-8a28d3dee506', foundational, constitutional_meaning_popularly_contested).
narrative_ontology:cs_axiom_status(constitutional_meaning_popularly_contested, holdable).
narrative_ontology:cs_axiom_grounding('56660429-efb3-4fd0-932f-8a28d3dee506', constitutional_meaning_popularly_contested, deontological).
narrative_ontology:cs_axiom('56660429-efb3-4fd0-932f-8a28d3dee506', foundational, terminal_adjudication_illegitimate).
narrative_ontology:cs_axiom_status(terminal_adjudication_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('56660429-efb3-4fd0-932f-8a28d3dee506', terminal_adjudication_illegitimate, deontological).
narrative_ontology:cs_reference_frame('56660429-efb3-4fd0-932f-8a28d3dee506', ongoing_democratic_contestation).
narrative_ontology:cs_drift_state('56660429-efb3-4fd0-932f-8a28d3dee506', contemporary_constitutional_politics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56660429-efb3-4fd0-932f-8a28d3dee506', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, civil_society_mobilizers).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize campaigns and litigation strategies knowing that constitutional meaning remains open to political contestation; their mobilization gains direct constitutional significance rather than being filtered through institutional gatekeepers.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, civil_society_mobilizers, beneficiary,
    organized, biographical, mobile, national).

% Issues constitutional interpretations that are perpetually subject to democratic override and contestation; denied final authority, its decisions function as provisional inputs rather than settled law.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Enacts statutes under constant threat of constitutional reopening by popular movements; lacks final interpretive authority, which weakens its capacity to commit to long-term policy frameworks.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature, payer,
    institutional, generational, constrained, national).

% Implements policy across multiple institutional sites afflicted by gridlock; bears operational costs of perpetual constitutional uncertainty and contested mandates.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Would require stable, institutionally enforced constitutional protections but are structurally disadvantaged when majoritarian contestation can continually reopen settled rights; largely absent from the democratic contestation frame.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, political_minorities, excluded,
    powerless, generational, trapped, national).

% Analyze the competing claims of institutional and popular authority without exercising control over which reading prevails in practice.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single institution from monopolizing constitutional meaning by distributing interpretive authority across an ongoing democratic process.
% TRANSFER_FUNCTION: Transfers final interpretive authority from courts and legislatures to the arena of democratic contestation; transfers gridlock and uncertainty costs to institutional implementers.
% ABSENT_VOICES: Political minorities who require settled constitutional protections against majoritarian mobilization; institutional actors who would benefit from clear hierarchies of authority.
% DISAPPEARANCE_RATIONALE: If constitutional meaning were no longer understood as emerging from ongoing democratic contestation, courts or legislatures would reclaim terminal interpretive authority, civil society would lose its constitutional standing, and the gridlock costs of perpetual contestation would vanish â but so would the distributed legitimacy claims.
% FOUNDING_PROBLEM: How to legitimate constitutional meaning in a democracy without allowing a single institution, whether unelected courts or elected legislatures, to dominate the constitutional order.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional theorists outside the benefiting civil society sector attest that institutional supremacy creates democratic deficits; however, public administration scholars and some minority-rights advocates attest that the founding problem of institutional domination has been replaced by a new problem of perpetual uncertainty.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.64, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64) reflects the substantial cost of perpetual contestation and lost institutional finality. Suppression (0.59) captures the active normative and political pressure required to prevent courts and legislatures from asserting terminal authority. Theater (0.48) is elevated because much democratic contestation over constitutional meaning is performative rather than deliberative, substituting symbolic mobilization for authoritative resolution. Accessibility_collapse (0.45) is moderate: alternatives like judicial supremacy or parliamentary sovereignty remain intellectually accessible but are politically blocked by the popular constitutionalism norm. Resistance (0.72) is high because institutions actively resist the erosion of their authority.
 *
 * PERSPECTIVAL GAP:
 *   The civil society mobilizer seat experiences the constraint as enabling genuine democratic coordination â it opens space for popular voice. The institutional payer seats (judiciary, legislature, executive) experience the same structure as extractive: it strips them of capacities they previously held and imposes gridlock costs. The engine computes this divergence from the structural data â the same constraint reads as coordination from one seat and extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations flow to civil_society_mobilizers (low d, subsidized by the constraint's opening of interpretive space). Victim declarations flow to judiciary, legislature, and executive_branch (high d, targeted by the constraint's denial of terminal authority). Political_minorities are excluded rather than directly targeted, but their trapped exit and powerless position amplify their effective extraction. The analytical seat (constitutional_theorists) sits outside the directional flow.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would mislabel this constraint if its coordination function were ignored (rendering it a Snare) or if its extraction of institutional authority were ignored (rendering it a Rope). The Tangled Rope classification is warranted only because both functions are present: the constraint genuinely coordinates democratic participation against institutional monopoly, AND it asymmetrically extracts final decision-making capacity from institutions while distributing gridlock costs. If the coordination function atrophied leaving only the gridlock, it would degrade toward Piton; if institutional authority were fully crushed and popular movements captured absolute power, it would become Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_contestation_legitimacy,
    'Does ongoing democratic contestation over constitutional meaning produce inclusive democratic legitimacy, or does it disproportionately empower already-mobilized actors while diffusing accountability?',
    'Comparative analysis of policy stability, inclusion metrics, and mobilization concentration across jurisdictions with strong popular constitutionalism norms versus institutional supremacy.',
    'If concentrated among mobilized actors, the coordination function is cover for asymmetric extraction, pushing classification toward Snare; if broadly inclusive, extraction is lower and the constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_contestation_legitimacy, conceptual, 'Ambiguity over whether contestation is genuinely inclusive or captured by mobilized minorities').

omega_variable(
    gridlock_cost_bearing,
    'Which agents bear the gridlock costs when no institution can settle constitutional meaning, and are these costs a necessary coordination overhead or extractive surplus?',
    'Trace policy implementation failures and uncertainty costs across judicial, legislative, and executive sites; compare to coordination benefits.',
    'If costs fall diffusely on the public without concentrated capture, directionality is widely distributed; if concentrated on powerless groups, effective extraction is higher than base metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gridlock_cost_bearing, empirical, 'Uncertainty over who pays for perpetual contestation and whether costs are overhead or extraction').

omega_variable(
    kernel_reading_scope,
    'Is the gridlock produced by this reading an inherent feature of popular constitutionalism or a transitional artifact of its contest with institutional supremacy readings?',
    'Historical analysis of constitutional politics in jurisdictions where popular constitutionalism has been dominant versus those where it is contested.',
    'If transitional, the constraint may be a Scaffold rather than Tangled Rope; if inherent, the gridlock is structurally extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Whether gridlock is inherent to the reading or a byproduct of kernel contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(basi_tr_t45, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(basi_tr_t60, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(basi_be_t45, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 45, 0.6).
narrative_ontology:measurement(basi_be_t60, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 60, 0.64).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(basic_law_interpretive_authority__popular_constitutionalism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the basic_law_interpretive_authority kernel, decomposed per the epsilon-invariance principle from judicial_supremacy_reading and parliamentary_sovereignty_reading because each reading has a distinct beneficiary/victim structure and epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
