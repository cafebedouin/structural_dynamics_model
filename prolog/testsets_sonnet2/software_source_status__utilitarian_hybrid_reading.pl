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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Utilitarian-Hybrid Reading of Software Source-Status Norms
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the utilitarian-hybrid reading of the software
 *   source-status kernel: the claim that licensing choice should be evaluated
 *   instrumentally, by expected aggregate welfare, rather than by categorical
 *   commitment to either freedom or property. Under this reading there is no
 *   fixed victim class — infrastructure with high coordination value is
 *   expected to trend open, specialized high-investment tools may
 *   legitimately remain proprietary, and mixed ecosystems are the normal,
 *   welfare-optimal outcome rather than an unstable compromise. This is a
 *   genuinely different constraint from its siblings
 *   (freedom_imperative_reading, pragmatic_development_reading,
 *   property_rights_reading) — its ε is low because the reading's own
 *   optimization criterion structurally minimizes categorical harm by design,
 *   not because the underlying licensing practices it describes are less
 *   real.
 *
 * KEY AGENTS:
 *   - infrastructure_maintainer_communities: organized beneficiary of the open-infrastructure branch of the welfare calculus
 *   - specialized_proprietary_vendors: powerful beneficiary of the proprietary-legitimation branch
 *   - end_users_of_mixed_ecosystems: moderate-power beneficiary of the resulting mixed outcome
 *   - small_developers_without_capital: powerless, excluded from the calculus's distributional attention
 *   - software_policy_analysts: analytical observer applying the welfare framework to concrete policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.28).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.15).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian-Hybrid Reading of Software Source-Status Norms").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software_engineering/political_economy/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, 'f92ada71-e6ad-4644-bdb2-f16ea4cc0208').
narrative_ontology:cs_kernel_codification('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', distributed).
narrative_ontology:cs_authority_grounding('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', distributed).
narrative_ontology:cs_reading_relation('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', foundational, aggregate_welfare_is_the_licensing_criterion).
narrative_ontology:cs_axiom_status(aggregate_welfare_is_the_licensing_criterion, holdable).
narrative_ontology:cs_axiom_grounding('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', aggregate_welfare_is_the_licensing_criterion, instrumental).
narrative_ontology:cs_axiom('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', foundational, source_status_optimality_is_context_dependent_not_categorical).
narrative_ontology:cs_axiom_status(source_status_optimality_is_context_dependent_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', source_status_optimality_is_context_dependent_not_categorical, empirically_contingent).
narrative_ontology:cs_reference_frame('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', context_sensitive_welfare_optimization).
narrative_ontology:cs_drift_state('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', contemporary_platform_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f92ada71-e6ad-4644-bdb2-f16ea4cc0208', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, infrastructure_maintainer_communities).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, specialized_proprietary_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, end_users_of_mixed_ecosystems).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, welfare_maximization_as_licensing_criterion).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, context_dependent_optimality_of_source_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain widely-shared infrastructure (compilers, protocols, core libraries) under open licenses because the utilitarian calculus favors network-effect goods being open: broad interoperability and shared maintenance burden outweigh any single firm's appropriation gain. They benefit from the reading's endorsement of openness where coordination value is high, and face no obligation to open specialized or context-limited software.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, infrastructure_maintainer_communities, beneficiary,
    organized, generational, mobile, global).

% Build narrow, high-investment tools (e.g. specialized scientific instruments' control software, bespoke enterprise systems) where the utilitarian case for proprietary capture is that appropriability funds the R&D that would not otherwise occur. This reading grants them legitimacy without requiring a categorical property-rights claim — their proprietary status is justified case-by-case by expected welfare, not by an inherent right.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, specialized_proprietary_vendors, beneficiary,
    powerful, biographical, mobile, national).

% Use software stacks combining open infrastructure and proprietary specialized layers. They gain the interoperability benefits of open standards while still accessing high-investment proprietary tools that might not exist under a strict freedom mandate. Their exit is only partially free: switching away from a proprietary specialized tool can be costly, but the open infrastructure layer beneath it is never a lock-in point.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, end_users_of_mixed_ecosystems, beneficiary,
    moderate, biographical, constrained, global).

% Lack the capital to either build proprietary tools that could recoup R&D investment or to sustain long-term open-source maintenance work without external funding. The utilitarian calculus, run in aggregate, does not weight their structural disadvantage — welfare is measured in total surplus, not distribution, so their position is invisible to the reading's own optimization criterion even though they are neither named victims nor named beneficiaries.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, small_developers_without_capital, excluded,
    powerless, biographical, constrained, global).

% Evaluate licensing regimes, antitrust concerns, and public procurement rules against welfare outcomes rather than ideological commitments. They can shift policy toward whichever licensing model empirical analysis favors in a given domain, and are the primary audience this reading is built to persuade.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision procedure for choosing licensing models per-context — open where coordination/network value dominates, proprietary where appropriability funds otherwise-unviable investment — replacing a single categorical rule with case-by-case welfare comparison.
% TRANSFER_FUNCTION: Distributes legitimacy rather than money directly: it moves normative permission to license restrictively toward vendors whose proprietary model can be welfare-justified, and moves normative expectation of openness toward maintainers of broadly shared infrastructure. Aggregate surplus is the currency; distribution within that surplus is not tracked by the framework itself.
% ABSENT_VOICES: Small developers without capital to pursue either open-source community-funded maintenance or proprietary appropriation are structurally invisible to an aggregate-welfare calculus that measures total surplus rather than its distribution; they would object that the reading treats their disadvantage as noise rather than a structural cost.
% DISAPPEARANCE_RATIONALE: If this reading vanished as the operative policy framework, mixed ecosystems would not immediately collapse (many exist independent of the utilitarian justification), but public procurement policy, antitrust doctrine, and licensing-choice legitimation in courts and legislatures would lose their current context-sensitive justification and likely revert to a more categorical framework (either the freedom imperative or the property-rights reading), which parties on both sides would contest as either a loss of nuance or a return to principle.
% FOUNDING_PROBLEM: Neither a strict software-freedom mandate nor an unqualified property-rights claim could account for the empirical fact that some software ecosystems thrive under open licensing while others require proprietary appropriation to exist at all — the founding problem was reconciling two incompatible categorical claims with observed heterogeneous outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Economists studying open-source sustainability (e.g. public-goods analyses of infrastructure software) and antitrust regulators evaluating platform licensing independently corroborate that licensing outcomes vary by context in ways neither categorical reading predicts; this corroboration comes from academic and regulatory sources outside the vendor and maintainer communities that benefit from the reading's flexibility.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, contested).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.28) because this reading's structural claim is precisely that no party is systematically extracted from when licensing tracks context-appropriate welfare optimization — proprietary appropriation is only endorsed where it funds investment that would not otherwise occur, and openness is only mandated where coordination value dominates. Suppression is low (0.15) because the reading does not compel a single licensing model; it is a decision procedure, not an enforcement regime. Resistance (0.35) reflects genuine philosophical contest from both the freedom-imperative and property-rights camps, who each regard hybrid utilitarianism as either insufficiently principled or as smuggling in redistribution. Accessibility collapse is low (0.2) — the reading explicitly preserves multiple licensing pathways as legitimate, which is the opposite of a collapsing-alternatives structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared across the spectrum this reading is built to accommodate: infrastructure maintainers benefit from the reading's endorsement of open licensing where network effects dominate; proprietary vendors benefit from its endorsement of restriction where appropriability funds R&D; end users benefit from the resulting mixed ecosystem's blend of interoperability and specialized capability. No victim group is declared, consistent with the expected structural delta — this reading's core claim is precisely that it has no categorical victim set. Small developers without capital are named as excluded rather than as victims: they are not extracted from by this constraint, but the welfare-aggregation logic renders their structural disadvantage invisible to the framework's own success criterion, which is a distinct harm from extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling categorical freedom and property claims with heterogeneous empirical outcomes) remains live rather than resolved-and-persisting, which is why this reading does not present piton or scaffold dynamics — it is an ongoing, actively-used decision procedure, not a vestigial one. The absence of a victim class and the low suppression score together prevent this reading from being mischaracterized as a snare or tangled rope; the coordination function is genuine and no party's welfare is being sacrificed through the same structure that benefits another, by the reading's own construction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_aggregation_masks_distribution,
    'Does an aggregate-welfare criterion for licensing choice systematically under-weight harms to parties (like uncapitalized small developers) whose disadvantage does not register as a loss in total surplus?',
    'Distributional analysis of licensing-policy outcomes: compare total-surplus calculations against Gini-style or worst-off-party metrics across historical licensing disputes decided on welfare grounds.',
    'If aggregate welfare systematically favors already-resourced parties, this reading''s claim of having no categorical victim set would be undermined — the victim set would be diffuse and structurally invisible rather than absent, which would push classification toward a hybrid tangled-rope reading despite the low headline extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_aggregation_masks_distribution, conceptual, 'Whether utilitarian aggregation conceals distributional harm not visible in surplus totals.').

omega_variable(
    context_dependent_optimum_gaming,
    'Can vendors strategically frame genuinely appropriable, non-specialized software as ''specialized'' to claim welfare-justified proprietary status under this reading, effectively laundering property_rights_reading claims through utilitarian language?',
    'Track empirical divergence between vendors'' welfare-justification claims at time of licensing decision and independent ex-post analysis of whether R&D would plausibly have occurred without appropriability.',
    'If gaming is prevalent, the reading''s low extractiveness score is an artifact of self-reported justification rather than actual welfare outcomes, and the effective operative constraint converges toward the property_rights_reading despite different declared axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_dependent_optimum_gaming, empirical, 'Whether the context-dependent optimality standard is exploitable as cover for unjustified proprietary claims.').

omega_variable(
    kernel_framing_choice,
    'Is the software_source_status kernel best modeled as a four-way partition of mutually exclusive ethical/instrumental claims (as declared), or does the utilitarian_hybrid_reading actually function as a meta-framework that adjudicates BETWEEN the other three rather than sitting alongside them as a peer reading?',
    'Examine whether policy analysts and courts treat the hybrid reading as a first-order licensing philosophy or as a second-order arbitration procedure invoked specifically to resolve conflicts between freedom_imperative and property_rights claims.',
    'If the hybrid reading is actually a meta-framework, its cs_structure.reading_relations to the other three readings would be better modeled as ''influences'' across the board (since it structurally reshapes when each sibling''s claim is invoked) rather than the declared ''coexists_with'' relations, which would change how contamination propagates across the kernel family in network analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether this reading is a peer philosophical position or a meta-level arbitration procedure over its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t5, software_source_status__utilitarian_hybrid_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(soft_tr_t15, software_source_status__utilitarian_hybrid_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(soft_tr_t25, software_source_status__utilitarian_hybrid_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(soft_be_t5, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(soft_be_t15, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(soft_be_t25, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 25, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_source_status__utilitarian_hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'the software source-status debate' per the epsilon-invariance principle: freedom_imperative_reading (categorical ethical claim, high suppression of proprietary alternatives), pragmatic_development_reading (instrumental methodology claim, moderate extractiveness centered on development-quality outcomes), property_rights_reading (categorical property claim, victim set includes those denied access/modification), and this utilitarian_hybrid_reading (context-dependent optimization claim, no categorical victim set, lowest authored extractiveness of the four because its own success criterion is designed to minimize categorical harm). Each reading has its own epsilon and stakeholder structure; they are linked here rather than merged because measuring 'software licensing' by the freedom-imperative lens versus the utilitarian lens yields incompatible epsilon values — the ε-invariance test requires decomposition, not averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
