% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Software Source-Status Norm — Utilitarian/Hybrid Reading
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the utilitarian/hybrid reading of the
 *   software-source-status kernel: the claim that licensing choice should be
 *   evaluated case-by-case for aggregate welfare, with openness favored for
 *   infrastructure/coordination goods and proprietary models justified for
 *   specialized, high-fixed-cost development. This is a genuinely distinct
 *   constraint from the freedom-imperative reading (which treats proprietary
 *   software as categorically unjust), the pragmatic-development reading
 *   (which treats openness as instrumentally superior for quality regardless
 *   of ethics), and the property-rights reading (which treats creator control
 *   as the default entitlement). The hybrid reading's structural signature is
 *   the absence of a categorical victim class — it produces winners and
 *   losers only relative to which context a given piece of software falls
 *   into, and it explicitly declines to rule out either pole as illegitimate.
 *   This story's ε (0.32) reflects that: real extraction exists
 *   (uncompensated open-infrastructure labor subsidizing both open and closed
 *   downstream users) but it is diffuse and non-categorical rather than the
 *   concentrated extraction a pure property-rights or pure freedom-imperative
 *   reading would identify.
 *
 * KEY AGENTS:
 *   - infrastructure_dependent_developers: benefit from openness-favoring context logic
 *   - specialized_proprietary_vendors: benefit from legitimized closed licensing in narrow contexts
 *   - mixed_ecosystem_integrators: benefit from non-categorical legitimacy but pay integration/licensing friction
 *   - software_freedom_absolutists: bear the cost of having their categorical claim treated as one input among several
 *   - underfunded_maintainers_of_critical_open_infrastructure: bear uncompensated-labor cost with no correction mechanism
 *   - policy_and_procurement_bodies: analytical/observer seat applying the framework to procurement decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.32).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.22).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Software Source-Status Norm — Utilitarian/Hybrid Reading").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software_engineering/political_economy/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '44d9baf5-4337-4c01-887f-b92165754e9a').
narrative_ontology:cs_kernel_codification('44d9baf5-4337-4c01-887f-b92165754e9a', distributed).
narrative_ontology:cs_authority_grounding('44d9baf5-4337-4c01-887f-b92165754e9a', distributed).
narrative_ontology:cs_reading_relation('44d9baf5-4337-4c01-887f-b92165754e9a', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('44d9baf5-4337-4c01-887f-b92165754e9a', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('44d9baf5-4337-4c01-887f-b92165754e9a', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('44d9baf5-4337-4c01-887f-b92165754e9a', foundational, licensing_legitimacy_is_context_relative).
narrative_ontology:cs_axiom_status(licensing_legitimacy_is_context_relative, holdable).
narrative_ontology:cs_axiom_grounding('44d9baf5-4337-4c01-887f-b92165754e9a', licensing_legitimacy_is_context_relative, instrumental).
narrative_ontology:cs_axiom('44d9baf5-4337-4c01-887f-b92165754e9a', foundational, no_licensing_model_is_categorically_privileged).
narrative_ontology:cs_axiom_status(no_licensing_model_is_categorically_privileged, holdable).
narrative_ontology:cs_axiom_grounding('44d9baf5-4337-4c01-887f-b92165754e9a', no_licensing_model_is_categorically_privileged, instrumental).
narrative_ontology:cs_reference_frame('44d9baf5-4337-4c01-887f-b92165754e9a', context_dependent_welfare_optimization).
narrative_ontology:cs_drift_state('44d9baf5-4337-4c01-887f-b92165754e9a', post_platform_saturation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('44d9baf5-4337-4c01-887f-b92165754e9a', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, infrastructure_dependent_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, specialized_proprietary_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_integrators).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, software_freedom_absolutists).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, underfunded_maintainers_of_critical_open_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build on widely shared open infrastructure (compilers, OS kernels, protocol libraries) where openness lowers coordination cost and prevents lock-in. Under the hybrid reading, this is exactly the context where openness maximizes welfare, so they benefit from the norm treating infrastructure as presumptively open without having to fight an absolutist battle over every proprietary tool they also use.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, infrastructure_dependent_developers, beneficiary,
    moderate, biographical, constrained, global).

% Sell narrowly-scoped, high-investment tools (CAD systems, scientific instrumentation software, niche compilers) where the hybrid reading's context-dependent logic legitimizes closed licensing as welfare-maximizing when open development would undersupply the specialized R&D investment needed. They benefit from a framework that does not treat their business model as inherently unjust.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, specialized_proprietary_vendors, beneficiary,
    organized, biographical, mobile, global).

% Companies and teams that combine open infrastructure with proprietary specialized components. They benefit from a framework that legitimizes their hybrid stack without categorical guilt, but they also absorb the friction of managing licensing compliance across incompatible models and occasionally pay proprietary rents for components that could, in a different reading, have been open.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_integrators, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_integrators, payer).

% Individuals and organizations (FSF-aligned developers, copyleft advocates) whose ethical commitment treats proprietary software as an injustice regardless of context. The utilitarian hybrid reading structurally denies them a categorical victory: it declares their claim one input to an aggregate calculation rather than a trump. They experience this as a cost — their moral framework is treated as one preference among several, not as the authoritative account of legitimacy — and cannot exit the disagreement without abandoning the commitment that constitutes their position.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_freedom_absolutists, payer,
    organized, generational, identity_locked, global).

% Volunteer or thinly-funded maintainers of load-bearing open-source infrastructure (build tools, cryptographic libraries, parsers) that the hybrid reading praises as welfare-maximizing precisely because it is open and freely usable by proprietary vendors downstream. The aggregate-welfare framing that celebrates their infrastructure's openness provides no mechanism to compensate them; the same context-dependent optimization that licenses proprietary specialization elsewhere offers them no equivalent revenue model, and their labor subsidizes both open and closed downstream users without them having meaningful exit from maintaining code the ecosystem now depends on.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, underfunded_maintainers_of_critical_open_infrastructure, payer,
    powerless, biographical, trapped, global).

% Government agencies and large institutional procurers who must decide licensing policy for public software investment. They read the utilitarian hybrid framework as offering a workable decision procedure (case-by-case welfare assessment) rather than requiring commitment to either absolutist pole, and use it to justify mixed procurement portfolios.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, policy_and_procurement_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__utilitarian_hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(software_source_status__utilitarian_hybrid_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision procedure for choosing licensing models per-context (open for infrastructure/coordination goods, proprietary permissible for specialized/high-fixed-cost goods) instead of requiring a single categorical rule across all software, reducing the coordination cost of litigating the open-vs-proprietary question domain-wide for every project.
% TRANSFER_FUNCTION: Distributes legitimacy rather than money directly: it moves normative authority away from absolutist claims (either 'all software must be free' or 'creators own their code unconditionally') toward case-by-case welfare assessment, which in practice tends to ratify existing market allocations — open infrastructure stays open and uncompensated, proprietary specialization stays proprietary and monetized, with no redistribution mechanism connecting the two.
% ABSENT_VOICES: Underfunded open-infrastructure maintainers are structurally present as praised contributors but absent as parties whose compensation the welfare calculus would revisit; the hybrid framework has no seat at which their uncompensated labor is treated as a cost requiring correction rather than a positive externality to be celebrated. Software freedom absolutists are present in public debate but are treated as one interest group among several rather than as holders of a claim the calculus must satisfy.
% DISAPPEARANCE_RATIONALE: If the hybrid utilitarian framing vanished, procurement bodies and mixed-ecosystem firms would lose their ready justification for mixed portfolios and would face renewed pressure to pick a side (freedom imperative or property rights), likely triggering more litigation and ecosystem fragmentation — so for institutional actors the world rearranges. But for actual technical practice (which infrastructure gets built open, which specialized tools stay closed) the underlying market and coordination logic that produced today's mixed ecosystem would likely persist even without the framework's articulation, since the pattern predates the explicit utilitarian defense of it — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The open-source/proprietary licensing debate had polarized into two mutually exclusive ethical camps (software freedom as moral absolute vs. property rights as moral absolute) that provided no workable guidance for the empirical reality that different kinds of software have different optimal licensing regimes depending on network effects, fixed-cost recovery needs, and coordination requirements.
% FOUNDING_PROBLEM_CORROBORATION: Empirical software-economics researchers and large-scale procurement policy analysts (outside both the FSF tradition and proprietary-vendor trade associations) corroborate that mixed licensing outcomes correlate with the structural features the hybrid reading identifies (network effects favor open, high fixed-cost specialization favors closed). However, labor economists studying open-source maintainer sustainability corroborate a different, unresolved problem: the hybrid framework's celebration of open infrastructure as welfare-maximizing has not been matched by any corroborated mechanism that addresses maintainer compensation, suggesting the founding problem it claims to solve (categorical gridlock) is live-resolved while a problem it does not claim to solve (sustainability of the open commons) remains live and uncorroborated as solved by anyone, including the hybrid reading's own proponents.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, contested).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.32) and suppression is low (0.22) because the hybrid reading imposes no exclusive licensing regime and no enforcement machinery forcing any project into open or closed status — the coordination function is a decision heuristic, not a mandate. Theater ratio starts low (0.15) and rises modestly (0.28) reflecting a real drift: as the hybrid framing has become dominant in procurement and industry discourse, some of its invocation has shifted from genuine context-sensitive analysis toward a rhetorical device used to avoid engaging with either absolutist critique. Accessibility collapse (0.35) is moderate: alternatives (picking one licensing philosophy categorically) remain fully available to any individual project, but the hybrid framing has become sufficiently entrenched in institutional procurement policy that a project or agency wanting to make a categorical ethical stand faces real friction. Resistance (0.45) is meaningful because software freedom absolutists actively and vocally contest the framework's legitimacy on principled grounds.
 *
 * PERSPECTIVAL GAP:
 *   Infrastructure developers and specialized vendors experience this as a genuine, low-friction rope: it lets them build and sell without ideological gatekeeping. Software freedom absolutists experience the identical structure as a subtle extraction of moral standing — their categorical claim is structurally demoted to a preference to be weighed, which they regard as already conceding the argument. Underfunded maintainers experience it as praise without payment: the same welfare calculus that celebrates their infrastructure's openness provides no mechanism to route value back to them, so from their seat the 'coordination' function functions as an extraction channel with the language of coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (infrastructure-dependent developers, specialized vendors, mixed integrators) are declared beneficiaries because the hybrid framework directly legitimizes and lowers friction for their existing practices. Victims are declared for two structurally distinct reasons: software freedom absolutists pay a status cost (their totalizing claim is downgraded to one input), which is identity-locked because their exit would require abandoning the commitment that constitutes their position; underfunded maintainers pay a material cost (uncompensated labor with no correction mechanism), and are trapped because the ecosystem's dependency on their infrastructure exists whether or not they are compensated, and stepping away risks breaking the dependent stack without altering the incentive structure that produced the situation.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading is structurally the LEAST mandatrophy-prone of the four kernel readings by design — it has no fixed mandate to outlive, since its content IS 'reassess per context.' But this flexibility is also its vulnerability: because it never commits to a fixed rule, it can be invoked post-hoc to justify whatever the market has already produced (rising theater_ratio captures this drift — a framework that started as genuine context-sensitive analysis increasingly gets cited to rationalize existing allocations rather than to actually re-derive them). The classification as rope (not tangled_rope) reflects that no active enforcement compels adoption of this reading; institutions and developers choose it because it is analytically convenient, not because they are coerced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_calculus_neutrality,
    'Is the ''aggregate welfare'' calculus genuinely neutral across contexts, or does its selection of which welfare effects count (efficiency, innovation incentive) systematically favor incumbent market allocations over redistributive alternatives (e.g., funded public infrastructure commons)?',
    'Comparative institutional analysis: examine whether welfare calculations under this framework have ever recommended converting a currently-proprietary specialized tool to open, or only ever ratify existing open/closed splits along already-existing market lines.',
    'If the calculus never recommends departures from existing market allocation, the hybrid reading functions less as genuine optimization and more as post-hoc legitimation of whatever the market already produced — which would push its computed type toward a milder tangled_rope (coordination cover for status-quo protection) rather than a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_calculus_neutrality, conceptual, 'Whether the utilitarian calculus is a genuine optimization procedure or a legitimation device for existing market splits.').

omega_variable(
    maintainer_compensation_gap,
    'Does the absence of a compensation mechanism for critical open-infrastructure maintainers reflect a genuine gap in the hybrid framework''s welfare calculus (an oversight correctable within the framework) or a structural feature (the framework treats uncompensated open labor as a free input rather than a cost, by design)?',
    'Track whether emerging mechanisms (Open Source Security Foundation funding, GitHub Sponsors, corporate foundation grants) that attempt to address maintainer sustainability are framed as corrections within the utilitarian hybrid logic, or as external interventions that the dominant welfare framing has resisted or been indifferent to.',
    'If correctable within the framework, this constraint''s extraction score should fall as compensation mechanisms mature. If structural, the framework''s celebration of open infrastructure functions as a stable extraction channel and the extraction score is a floor, not a transient measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_compensation_gap, empirical, 'Whether maintainer non-compensation is a fixable gap or a structural feature of the hybrid welfare framing.').

omega_variable(
    kernel_reading_selection_neutrality,
    'Is the choice to adjudicate software licensing via THIS reading (utilitarian/hybrid) itself neutral, or does the fact that this reading is currently dominant in industry and policy discourse (as opposed to the freedom-imperative or property-rights readings) reflect the structural power of incumbent mixed-ecosystem firms who benefit from non-categorical legitimacy?',
    'Trace the institutional history of how the hybrid framing became dominant in procurement policy and industry discourse — was it adopted through open deliberation weighing all four kernel readings, or did it emerge because it was the reading most compatible with existing large-firm mixed-licensing business models?',
    'If the hybrid reading''s dominance reflects incumbent-firm structural power rather than open deliberative selection among readings, that would support classifying the SELECTION of this reading (as distinct from its internal content) as itself an instance of soft extraction — a separate, higher-order constraint about which reading gets to adjudicate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_neutrality, conceptual, 'Whether the hybrid reading''s real-world dominance among the four kernel readings is itself structurally neutral or favors incumbent mixed-ecosystem power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t5, software_source_status__utilitarian_hybrid_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(soft_tr_t15, software_source_status__utilitarian_hybrid_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(soft_tr_t25, software_source_status__utilitarian_hybrid_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(soft_be_t5, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(soft_be_t15, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(soft_be_t25, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 25, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_source_status__utilitarian_hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__utilitarian_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language 'software source status' debate per the ε-invariance principle: the label 'should software be open or proprietary' conflates at least four structurally distinct normative claims with different beneficiary/victim structures and different ε. This utilitarian_hybrid_reading is deliberately the lowest-ε, most diffuse-extraction sibling because it declines to name a categorical victim class; the freedom_imperative_reading would author much higher ε for proprietary licensing (naming proprietary vendors and users locked into closed formats as victims), while the property_rights_reading would author near-zero ε for the same arrangement (treating restriction as a legitimate entitlement, naming unauthorized copiers as the violators instead). Each reading is generated as its own file with its own claimed_type and metrics; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
