% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Ontology: Hybrid Pragmatic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The constraint typology itself—the framework that classifies constraints
 *   into six types—is a contested institution. This story instantiates the
 *   hybrid pragmatic reading: the typology has a fixed, observationally
 *   grounded core (mountains are physical/logical invariants; ropes are
 *   genuine coordination solutions), but the periphery (tangled_ropes and
 *   snares) requires normative judgment about what counts as 'legitimate
 *   beneficiary.' This reading claims both are discoverable but by different
 *   epistemological methods. The sibling readings contest this boundary: the
 *   immutable diagnostic reading claims the entire typology is
 *   observationally discoverable if measurement is careful enough; the
 *   rhetorical scaffold reading claims the entire typology is a normative
 *   vocabulary for policy critique, with no observational ground separate
 *   from declared interests.
 *
 * KEY AGENTS:
 *   - Analytic consensus maintainers: institutional gate-keepers who enforce the hybrid reading as canonical
 *   - Pragmatic interdisciplinary researchers: benefit from the shared vocabulary without full normative commitment
 *   - Empiricist purists: resist the admission of constructed measurement in peripheral classifications
 *   - Normative theorists: contest the claim that core and periphery are separable
 *   - Applied policy practitioners: use the framework's ambiguity to justify contested decisions
 *   - Alternative typological frameworks: excluded from canonical position by consensus enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.48).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.62).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Ontology: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '45083ff7-5159-4e7c-b492-1cceecc6aee0').
narrative_ontology:cs_kernel_codification('45083ff7-5159-4e7c-b492-1cceecc6aee0', distributed).
narrative_ontology:cs_authority_grounding('45083ff7-5159-4e7c-b492-1cceecc6aee0', extraction).
narrative_ontology:cs_interpretation_layer_present('45083ff7-5159-4e7c-b492-1cceecc6aee0').
narrative_ontology:cs_reading_relation('45083ff7-5159-4e7c-b492-1cceecc6aee0', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('45083ff7-5159-4e7c-b492-1cceecc6aee0', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('45083ff7-5159-4e7c-b492-1cceecc6aee0', foundational, core_periphery_measurability_split).
narrative_ontology:cs_axiom_status(core_periphery_measurability_split, holdable).
narrative_ontology:cs_axiom_grounding('45083ff7-5159-4e7c-b492-1cceecc6aee0', core_periphery_measurability_split, conventional).
narrative_ontology:cs_axiom('45083ff7-5159-4e7c-b492-1cceecc6aee0', foundational, normative_judgment_operationalizable_periphery).
narrative_ontology:cs_axiom_status(normative_judgment_operationalizable_periphery, holdable).
narrative_ontology:cs_axiom_grounding('45083ff7-5159-4e7c-b492-1cceecc6aee0', normative_judgment_operationalizable_periphery, instrumental).
narrative_ontology:cs_reference_frame('45083ff7-5159-4e7c-b492-1cceecc6aee0', pragmatic_epistemic_bridge).
narrative_ontology:cs_drift_state('45083ff7-5159-4e7c-b492-1cceecc6aee0', contemporary_institutional_consensus, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('45083ff7-5159-4e7c-b492-1cceecc6aee0', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, analytic_consensus_maintainers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, pragmatic_interdisciplinary_researchers).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, normative_theorists_excluded_from_classification_authority).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, empiricist_purists_resisting_constructivism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, applied_policy_practitioners).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_typology_bridges_is_ought).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, observational_grounding_coexists_with_normative_judgment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic institutions and research communities that enforce the hybrid reading as the canonical interpretation of deferential realism. They administer which classification decisions are accepted as 'correct' and which are relegated to contested periphery. They benefit from the typology's coordination power (bridges disciplines) without accepting full responsibility for its normative commitments. They set the terms of what counts as 'observational' vs. 'constructed' in peripheral classifications.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, analytic_consensus_maintainers, agenda_setter,
    institutional, generational, arbitrage, global).

% Researchers across epistemology, political economy, institutional design, and applied constraint analysis who use the hybrid reading to navigate between empirical measurement and normative critique. They benefit from having a shared vocabulary that acknowledges both measurement grounding and value contestation. Their continued citation and teaching of the framework sustains its authority.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, pragmatic_interdisciplinary_researchers, beneficiary,
    organized, biographical, constrained, global).

% Philosophers, critical theorists, and normative institutional designers who contest the hybrid reading's claim that empirical measurement can be cleanly separated from normative judgment even in the 'core.' They argue that what counts as a 'mountain' or 'rope' already encodes normative choices (boundary conditions, what counts as coordination, who is the reference agent). They pay by having their objections absorbed into 'peripheral contestation' rather than treated as structural alternatives to the typology itself.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, normative_theorists_excluded_from_classification_authority, payer,
    powerful, biographical, mobile, global).

% Measurement-focused researchers and natural scientists who reject the hybrid reading's acceptance of 'constructed' measurement on the periphery. They argue that admitting normative judgment into epsilon measurement (for snares/tangled_ropes) corrupts the typology's claimed objectivity. They bear the cost of being classified as 'refusing pragmatism' when they maintain empirical boundaries.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, empiricist_purists_resisting_constructivism, payer,
    powerful, biographical, mobile, global).

% Regulators, competition authorities, and institutional designers who use the constraint typology to diagnose market failures and design remedies. The hybrid reading allows them to treat core constraints as discoverable facts while admitting peripheral constraints as responsive to policy judgment. This gives them cover for decisions that are normatively contested.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, applied_policy_practitioners, beneficiary,
    moderate, biographical, constrained, national).

% Competing ontologies (pure empiricism, pure normativism, other constraint vocabularies) are structurally excluded from the canonical position by the enforcement of the hybrid reading as institutional consensus. Their advocates must engage on the typology's own terms or exit the discourse entirely.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, alternative_typological_frameworks, excluded,
    powerful, biographical, trapped, global).

% The apparatus itself—the engine, compiler, and measurement infrastructure that operationalizes the typology. This seat sees the structure from outside all parties' commitments and tracks where measurement contestation actually lives (core vs. periphery classifications, directionality divergence, per-seat type computation).
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, observational_epistemic_community, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__hybrid_pragmatic_reading, analytic_consensus_maintainers).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary and measurement framework for diagnosing constraint mechanisms across disciplines (physics, economics, politics, epistemology). Enables researchers to compare mountains (natural invariants) with ropes (genuine coordination) without collapsing the distinction or denying that the distinction itself encodes value choices.
% TRANSFER_FUNCTION: Transfers classification authority from pure empiricists (who claim measurement objectivity) and pure normativists (who claim full transparency about value) to a hybrid consensus that claims to hold both grounds simultaneously. Practitioners gain the ability to invoke 'observational grounding' when convenient and 'normative judgment' when necessary, without fully committing to either.
% ABSENT_VOICES: Pure empiricist natural philosophers who reject constructivism entirely; pure normativists who reject the claim that any measurement can escape value; alternative ontological frameworks (Latourian actor-network theory, capability approaches, other constraint vocabularies) are structurally excluded from the canonical interpretation. Their objections would dispute whether the core/periphery split is real or a concealment of the typology's own normativity.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished and were replaced by a pure empiricist reading, peripheral constraints (snares, tangled_ropes) would lose their measurement ground and researchers would fragment into incommensurable vocabularies. If replaced by a pure normativist reading, core constraints (mountains) would lose their claimed objectivity and the typology would become explicitly a policy tool rather than a diagnostic framework. Both outcomes are plausible; the hybrid's persistence depends on its utility for constituencies that benefit from ambiguity.
% FOUNDING_PROBLEM: Early constraint analysis exhibited two incommensurable research programs: empiricists measuring extraction mechanisms without acknowledging normative boundary-setting; normativists critiquing institutions without grounding their claims in measurable facts. The hybrid reading was developed to bridge these programs by accepting that core constraints are observationally grounded while peripheral constraints involve normative judgment—allowing both to operate within one framework.
% FOUNDING_PROBLEM_CORROBORATION: Pragmatic researchers and institutional designers attest the founding problem is live—incommensurable frameworks hamper coordination across disciplines. Empiricist critics attest the 'solution' is illusory—it merely postpones the measurement dispute to the periphery and creates false consensus. Normativists attest the founding problem dissolves if one accepts that ALL classification encodes value. No external corroboration exists; the problem's existence is itself contested along the lines of the reading disagreement.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the hybrid reading extracts classification authority from both pure empiricists and pure normativists, claiming to hold both grounds without fully committing to either. Suppression is moderately high (0.62) because the framework actively maintains the core/periphery boundary against challenges from both sides—empiricists who want to extend observational grounding to the periphery, and normativists who want to dissolve the boundary entirely. Theater ratio is near-neutral (0.51) because the framework genuinely performs classification work (not purely theater) but increasingly relies on rhetorical framing to manage the periphery's instability. The measurement trajectory shows extraction and suppression rising from t=0 to t=25 (as the hybrid reading becomes institutionalized and must defend against both challenges), then plateauing (reaching a stable enforcement level once consensus consolidates). Accessibility collapse (0.58) is moderate because alternatives remain live—researchers can still exit to pure empiricism or pure normativism, but the coordination benefits of staying in the hybrid framework create constrained exit. Resistance is high (0.72) because the periphery of the typology is genuinely contested; alternatives are actively mounted.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of analytic consensus maintainers, the hybrid reading is a genuine bridging mechanism that enables cross-disciplinary coordination. From the seat of empiricist purists, it is a corruption of measurement objectivity that admits normative judgment where empirical rigor is required. From the seat of normative theorists, it is a concealment of the typology's own normativity, which extends to the core as well as the periphery. The engine should compute different types for these seats: the agenda-setter seat may experience the typology as rope (coordination benefit); the payer seats (empiricists and normativists excluded from authority) experience it as tangled_rope (coordination appearance coupled with asymmetric extraction of classification authority). The observer seat (the apparatus itself) sees the structure as deliberately hybrid, engineering ambiguity to maintain consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Analytic consensus maintainers benefit from the hybrid reading and set its terms (d near 0.0, full beneficiary). Pragmatic researchers benefit through coordination without bearing full normativity responsibility (d near 0.2, partial beneficiary). Empiricist purists and normative theorists are targets: they bear the cost of having their objections classified as 'peripheral contestation' rather than structural alternatives, losing authority over core definition (d near 0.9, full targets). Applied policy practitioners occupy d near 0.5—they use the framework's ambiguity (symmetric benefit/cost), but the cost is the legitimacy questions the ambiguity creates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—incommensurable research programs (empiricism vs. normativity)—remains live but contested. The hybrid reading does not solve it; it coordinates around it by designating part of the framework as observationally grounded and part as normatively contested. This is mandatrophy-adjacent: the typology persists not because it solved the founding problem but because it is useful for constituencies that benefit from maintaining the appearance of both objectivity and policy-responsiveness. If the founding problem were actually solved (both research programs could talk to each other), the hybrid reading would become unnecessary. The framework's persistence depends on the problem remaining alive but manageable through institutional enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_periphery_boundary_reality,
    'Is the distinction between core (observationally grounded) and periphery (normatively contested) constraints a genuine structural feature of the typology, or is it a concealment of the fact that ALL constraint classification encodes normative judgment?',
    'Attempt to classify a constraint (e.g., information-theoretic limits, equilibrium concepts, institutional design principles) as ''mountain'' or ''rope'' using only observational criteria, with no normative assumptions about boundary conditions, reference agents, or what counts as ''coordination.'' Measure how much the classification shifts when different normative framings are introduced. If the core classification is stable across framings, the boundary is real; if it shifts, the distinction is rhetorical.',
    'If the boundary is real, the hybrid reading holds and peripheral constraints can be classified via normative judgment while core constraints remain observationally grounded. If the boundary is rhetorical, the immutable diagnostic reading fails and the rhetorical scaffold reading partially succeeds—the typology is fundamentally a normative vocabulary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(core_periphery_boundary_reality, conceptual, 'Whether the core/periphery distinction is structural or conceals universal normativity.').

omega_variable(
    consensus_maintenance_necessity,
    'Does the suppression we measure (enforcing the hybrid reading as canonical) arise from genuine empirical disagreement (different observers measuring the same constraint and getting different types), or from institutional politics (different constituencies preferring different readings and enforcing one as authority)?',
    'Conduct a measurement experiment with researchers from empiricist, normativist, and pragmatist traditions classifying the same constraint set independently, then compare classifications. If disagreement tracks disciplinary affiliation more than measurement uncertainty, suppression is institutional rather than empirical.',
    'If suppression is empirical, the hybrid reading is performing a genuine bridging function—it accepts measurement ambiguity and manages it pragmatically. If suppression is institutional, the reading is maintaining consensus through gate-keeping rather than resolving incommensurability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_maintenance_necessity, empirical, 'Whether measured suppression arises from genuine empirical disagreement or institutional politics.').

omega_variable(
    reading_foreclosure_possibility,
    'Can a researcher or research community cohere around the immutable diagnostic reading (full observational grounding) or the rhetorical scaffold reading (full normativity) without logical contradiction, or does the hybrid reading''s existence make those readings incoherent by requiring acceptance of the core/periphery distinction?',
    'Attempt to construct research programs that systematically instantiate immutable diagnostic reading or rhetorical scaffold reading premises without internally invoking hybrid logic. Measure whether such programs can sustain themselves or whether they inevitably import hybrid assumptions to maintain empirical or normative coverage.',
    'If pure programs are coherent and sustainable, the readings coexist genuinely and neither forecloses the other. If hybrid logic becomes necessary for internal consistency, the hybrid reading partially forecloses its siblings—they become available only by accepting hybrid premises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether the sibling readings are logically independent or foreclosed by hybrid premises.').

omega_variable(
    normative_judgment_operationalization,
    'When the hybrid reading admits normative judgment into peripheral constraint classification, what operationalizes that judgment? Who decides which beneficiary is ''legitimate''? By what criterion? Is this criterion itself subject to the typology (does it have a classification type?), and if so, does it recursively admit normative judgment?',
    'Trace three peripheral constraint classifications (e.g., snare, tangled_rope) that the hybrid reading would produce, documenting the normative judgment at each step: who decided the beneficiary structure, on what grounds, and whether the decision-making process itself exhibits constraint-theoretic properties.',
    'If normativity is fully operationalized (clear criteria, specified authority), the hybrid reading has theoretical rigor. If normativity remains tacit or circular (the typology''s normativity is itself unclassifiable), the reading conceals more than it clarifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_judgment_operationalization, conceptual, 'Whether normative judgment in peripheral classification is operationalized or tacit.').

omega_variable(
    measurement_indexicality_suppression,
    'The suppression we measure (0.62) includes suppression of alternative measurement frameworks—the exclusion of pure empiricism and pure normativism from the canonical position. Is this suppression the target of the hybrid reading (the reading itself is what suppresses alternatives), or is it a side effect of consensus enforcement?',
    'Compare the measured suppression of the hybrid reading to the measured suppression of the immutable diagnostic and rhetorical scaffold readings in scenarios where all three compete for canonical status. If the hybrid reading shows higher suppression than its siblings when all are equally resourced, the suppression is endogenous to the reading; if the suppression tracks institutional resource and political backing rather than measurement divergence, it is exogenous.',
    'If suppression is endogenous, the hybrid reading is designed to suppress alternatives (a feature of its pragmatic strategy). If exogenous, the suppression is institutional politics using any available reading as the vehicle. This affects whether classification of the hybrid reading itself should include suppression in its type computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_indexicality_suppression, empirical, 'Whether measured suppression is intrinsic to the hybrid reading or institutional contingency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dro_hybrid_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(dro_hybrid_tr_t0, observed).
narrative_ontology:measurement(dro_hybrid_tr_t5, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(dro_hybrid_tr_t5, observed).
narrative_ontology:measurement(dro_hybrid_tr_t10, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(dro_hybrid_tr_t10, observed).
narrative_ontology:measurement(dro_hybrid_tr_t15, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(dro_hybrid_tr_t15, observed).
narrative_ontology:measurement(dro_hybrid_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(dro_hybrid_tr_t20, observed).
narrative_ontology:measurement(dro_hybrid_tr_t25, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement_basis(dro_hybrid_tr_t25, observed).
narrative_ontology:measurement(dro_hybrid_tr_t30, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(dro_hybrid_tr_t30, observed).
narrative_ontology:measurement(dro_hybrid_tr_t40, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 40, 0.51).
narrative_ontology:measurement_basis(dro_hybrid_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(dro_hybrid_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(dro_hybrid_be_t0, observed).
narrative_ontology:measurement(dro_hybrid_be_t5, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement_basis(dro_hybrid_be_t5, observed).
narrative_ontology:measurement(dro_hybrid_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(dro_hybrid_be_t10, observed).
narrative_ontology:measurement(dro_hybrid_be_t15, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(dro_hybrid_be_t15, observed).
narrative_ontology:measurement(dro_hybrid_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(dro_hybrid_be_t20, observed).
narrative_ontology:measurement(dro_hybrid_be_t25, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 25, 0.49).
narrative_ontology:measurement_basis(dro_hybrid_be_t25, observed).
narrative_ontology:measurement(dro_hybrid_be_t30, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(dro_hybrid_be_t30, observed).
narrative_ontology:measurement(dro_hybrid_be_t40, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(dro_hybrid_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(dro_hybrid_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(dro_hybrid_su_t0, observed).
narrative_ontology:measurement(dro_hybrid_su_t5, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(dro_hybrid_su_t5, observed).
narrative_ontology:measurement(dro_hybrid_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(dro_hybrid_su_t10, observed).
narrative_ontology:measurement(dro_hybrid_su_t15, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement_basis(dro_hybrid_su_t15, observed).
narrative_ontology:measurement(dro_hybrid_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(dro_hybrid_su_t20, observed).
narrative_ontology:measurement(dro_hybrid_su_t25, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(dro_hybrid_su_t25, observed).
narrative_ontology:measurement(dro_hybrid_su_t30, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(dro_hybrid_su_t30, observed).
narrative_ontology:measurement(dro_hybrid_su_t40, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(dro_hybrid_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__hybrid_pragmatic_reading, 0.18).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the deferential_realism_ontology kernel. The immutable diagnostic reading treats the entire typology as observationally discoverable through careful measurement. The rhetorical scaffold reading treats it as a normative vocabulary whose value lies in persuasive power for policy critique. The hybrid pragmatic reading claims the core (mountains, ropes) is observationally grounded while the periphery (tangled_ropes, snares) requires normative judgment. Each reading has distinct epsilon (what counts as extraction), distinct suppression profiles (how alternative readings are managed), and distinct stakeholder structures (who benefits from each reading). Decomposition is justified because the three readings would produce different classifications of the typology itself—the readings are not different perspectives on the same constraint, but structurally distinct constraints grounded in incommensurable epistemic commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
