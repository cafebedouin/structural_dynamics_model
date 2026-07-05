% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__commons_coordination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive Open Source License Text (Commons Coordination Reading)
 *   domain: software licensing / intellectual property / technology governance
 *
 * SUMMARY:
 *   Permissive open-source licenses (MIT, BSD, Apache 2.0) grant nearly
 *   unrestricted rights to use, modify, and redistribute source code,
 *   including in proprietary derivative products, in exchange only for
 *   attribution. Under this reading, the license text functions as a
 *   low-overhead coordination mechanism: it eliminates the legal friction
 *   that would otherwise gate code reuse behind negotiation, and it does so
 *   for every conceivable class of implementer — hobbyist, academic, startup,
 *   or trillion-dollar incumbent — symmetrically. This is one of three
 *   readings of the same kernel license text. The
 *   copyleft_counterfactual_reading and corporate_moat_reading (separate
 *   constraint stories) read the identical text as either dangerously
 *   permissive relative to a reciprocity norm, or as a structural extraction
 *   vector for uncompensated corporate appropriation, respectively. Each
 *   reading has a materially different epsilon: this reading's epsilon is low
 *   because it evaluates the arrangement against the counterfactual of no
 *   reuse rights at all (strict copyright), not against the counterfactual of
 *   copyleft reciprocity or against the specific asymmetry between an unpaid
 *   individual author and a well-resourced corporate adopter. Per the
 *   epsilon-invariance principle, these are genuinely different constraints
 *   sharing a kernel text, not one constraint viewed three ways.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.08).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.03).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive Open Source License Text (Commons Coordination Reading)").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "software licensing / intellectual property / technology governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, 'a561889f-41dd-4871-ab14-be8e3ec355a4').
narrative_ontology:cs_kernel_codification('a561889f-41dd-4871-ab14-be8e3ec355a4', fixed_text).
narrative_ontology:cs_authority_grounding('a561889f-41dd-4871-ab14-be8e3ec355a4', distributed).
narrative_ontology:cs_reading_relation('a561889f-41dd-4871-ab14-be8e3ec355a4', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a561889f-41dd-4871-ab14-be8e3ec355a4', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('a561889f-41dd-4871-ab14-be8e3ec355a4', foundational, strict_copyright_is_the_relevant_counterfactual).
narrative_ontology:cs_axiom_status(strict_copyright_is_the_relevant_counterfactual, holdable).
narrative_ontology:cs_axiom_grounding('a561889f-41dd-4871-ab14-be8e3ec355a4', strict_copyright_is_the_relevant_counterfactual, conventional).
narrative_ontology:cs_axiom('a561889f-41dd-4871-ab14-be8e3ec355a4', foundational, attribution_only_reciprocity_is_sufficient_return_flow).
narrative_ontology:cs_axiom_status(attribution_only_reciprocity_is_sufficient_return_flow, holdable).
narrative_ontology:cs_axiom_grounding('a561889f-41dd-4871-ab14-be8e3ec355a4', attribution_only_reciprocity_is_sufficient_return_flow, instrumental).
narrative_ontology:cs_reference_frame('a561889f-41dd-4871-ab14-be8e3ec355a4', strict_default_copyright_baseline).
narrative_ontology:cs_drift_state('a561889f-41dd-4871-ab14-be8e3ec355a4', contemporary_platform_ecosystem_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a561889f-41dd-4871-ab14-be8e3ec355a4', '').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, universal_implementer_pool).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, original_authors).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, downstream_commercial_adopters).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, academic_and_hobbyist_developers).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, minimal_friction_maximizes_adoption).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, attribution_only_reciprocity_suffices_for_commons_maintenance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chooses to release source code under a permissive license (MIT/BSD/Apache-style) rather than proprietary terms or copyleft. Retains copyright but grants broad rights to use, modify, and redistribute, including in proprietary derivatives, with only an attribution requirement. Could relicense future versions, dual-license, or add restrictions at will since no reciprocal obligation binds them.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, original_authors, agenda_setter,
    moderate, biographical, mobile, global).

% Any developer, company, hobbyist, or institution anywhere can take the code and use it in any project — open, closed, commercial, or personal — without negotiating terms, paying fees, or disclosing their own source. Legal review cost for adopting the software approaches zero because the license is short, standard, and pre-vetted by widespread industry use.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, universal_implementer_pool, beneficiary,
    moderate, civilizational, arbitrage, global).

% Firms building products on the permissively-licensed code incorporate it into proprietary offerings without triggering disclosure obligations. They benefit from mature, community-tested infrastructure at zero licensing cost and face no legal exposure from license incompatibility with their own closed codebases.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, downstream_commercial_adopters, beneficiary,
    powerful, generational, mobile, global).

% Individual developers and researchers with no legal department can safely incorporate the code into coursework, research tools, or side projects without needing to interpret complex reciprocal-licensing obligations. The simplicity of the permissive grant is itself the accessibility mechanism.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, academic_and_hobbyist_developers, beneficiary,
    powerless, biographical, arbitrage, global).

% Foundations (OSI, Apache Software Foundation, etc.) certify and steward the standard license texts, monitor adoption patterns, and study whether permissive terms sustain healthy contributor ecosystems over time. They do not extract from the arrangement; they document and legitimize it.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, open_source_governance_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of enabling code reuse across an unbounded and unpredictable set of future implementers — commercial, academic, individual, open, or closed — without requiring case-by-case negotiation, legal review, or reciprocal obligation that would otherwise price most potential adopters out of using the software at all.
% TRANSFER_FUNCTION: Moves nothing extractive: the license transfers usage rights from the original author to the universal implementer pool at zero price, in exchange only for attribution. No payment, royalty, or disclosure flows back to the author under this reading; any value the author later derives (reputation, ecosystem placement, downstream contribution) is incidental to the grant, not a rent collected through it.
% ABSENT_VOICES: Under this reading there is no structurally excluded party: the license imposes no obligation on anyone that they did not choose to be bound by, and no implementer is turned away. The sibling readings (copyleft_counterfactual, corporate_moat) locate a victim class here — unpaid original contributors relative to proprietary appropriators — but this reading's structural claim is that the coordination benefit accruing to the universal implementer pool, including the original author as one member of that pool, is not asymmetric extraction.
% DISAPPEARANCE_RATIONALE: If permissive licensing disappeared and all software reverted to default 'all rights reserved' copyright, the vast majority of code reuse, dependency ecosystems (npm, PyPI, Maven Central, etc.), and derivative innovation built on freely reusable components would become illegal overnight, forcing either mass renegotiation, abandonment of derivative works, or a shift to costlier reciprocal-licensing regimes. The current scale of software infrastructure depends on this specific low-friction grant existing.
% FOUNDING_PROBLEM: Early software distribution under strict default copyright made even trivial reuse — a utility function, a parser, a driver — legally hazardous or require case-by-case negotiation, which throttled the pace of software development and locked reusable logic inside single organizations.
% FOUNDING_PROBLEM_CORROBORATION: Independent software supply-chain research (e.g. academic studies of open-source dependency graphs, Linux Foundation ecosystem surveys) attests that permissive licensing continues to solve an active coordination problem — the sheer volume of cross-organizational code reuse observed today would be legally impossible without it. This corroboration comes from researchers and standards bodies who are not themselves license authors or commercial beneficiaries capturing rents from the arrangement.
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__commons_coordination_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__commons_coordination_reading_tests).
:- end_tests(permissive_license_text__commons_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is scored very low (0.08) because, under this reading's own counterfactual (strict default copyright), the arrangement strictly increases the option set for every party without imposing new costs — no one is worse off than the pre-license baseline, and most parties are substantially better off. Suppression is near-zero because no party is coerced into using the license or coerced into contributing back; adoption is fully voluntary in both directions. Theater ratio is negligible because the license text does exactly what it says — grant broad rights with an attribution condition — with no gap between stated and actual function. Accessibility collapse is scored moderate-low (0.15) rather than near-zero because once an ecosystem standardizes on a specific permissive license as the default expectation, switching an established project to a different license later becomes practically difficult (a real but modest alternative-narrowing effect, not a coercive one).
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, no party sits meaningfully toward the extraction end of the directionality spectrum. Original authors retain full ownership and choose the terms; they are simultaneously the agenda-setter and a beneficiary. The universal implementer pool, including large commercial adopters, holds full arbitrage-grade exit (they can walk away, fork, or write their own equivalent) which pulls their directionality toward the beneficiary end regardless of their power level. Powerless individual developers benefit from the same zero-cost access as powerful firms — this reading treats that symmetry, not the size differential in ultimate capture, as the structurally decisive fact. The sibling corporate_moat_reading disputes exactly this point by treating the size differential as the decisive fact instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal friction blocking beneficial reuse) remains fully live: the volume of cross-organizational software reuse enabled by permissive terms has only grown, and no alternative mechanism has emerged that solves the same coordination problem at comparable cost. This reading resists mandatrophy because the coordination function has not atrophied into pure performance — the license text is still doing exactly the friction-reduction work it was designed to do, verified independently by supply-chain researchers outside the pool of commercial beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_baseline_selection,
    'Is the correct baseline for evaluating this license''s extractiveness ''no reuse rights at all'' (strict copyright) or ''reuse rights conditioned on reciprocity'' (copyleft)? The three sibling readings of this kernel select different baselines and arrive at different epsilon values from the identical license text.',
    'This is not empirically resolvable — it is a framing choice about which counterfactual is the relevant one for measuring extraction. Documenting all three readings as separate constraints, linked via network edges, is the framework''s answer rather than forcing a single resolution.',
    'If the copyleft baseline is adopted instead, this reading''s near-zero epsilon would need re-evaluation against a reciprocity norm, likely raising measured extraction substantially — this is precisely why the two readings are authored as separate constraint stories rather than reconciled into one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_baseline_selection, conceptual, 'Kernel-level baseline selection ambiguity between the three sibling readings of permissive_license_text.').

omega_variable(
    asymmetric_capture_visibility,
    'Does this reading''s finding of ''no victim set'' hold up when specific instances are examined where a well-resourced firm builds a proprietary product worth billions on top of an individual maintainer''s unpaid permissively-licensed work, with zero compensation flowing back?',
    'Case-level analysis of specific high-profile permissive-license dependencies (e.g. critical infrastructure libraries) maintained by individuals or small teams versus captured by large commercial products — compare maintainer compensation/support trajectories against commercial value captured downstream.',
    'If systematic patterns of maintainer burnout, unpaid critical-infrastructure labor, and asymmetric commercial capture are found, the corporate_moat_reading''s victim-set claim gains empirical support and this reading''s ''no victim set'' conclusion would need to be understood as valid only in aggregate/systemic terms, not at the level of individual maintainer relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_capture_visibility, empirical, 'Whether aggregate coordination benefit masks individual-level asymmetric capture that the corporate_moat sibling reading would flag as a victim class.').

omega_variable(
    attribution_sufficiency,
    'Is attribution alone (without reciprocal source disclosure or compensation) a sufficient return-flow to the original author to characterize this as balanced coordination rather than a one-directional grant?',
    'Survey original authors of widely-adopted permissively-licensed projects on whether they perceive the arrangement as sufficient, and compare against career/reputational outcomes attributable to attribution versus outcomes under copyleft-licensed comparable projects.',
    'If authors broadly report attribution as inadequate compensation given commercial capture by adopters, the ''beneficiary'' classification of original_authors weakens and the directionality derivation would need review.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attribution_sufficiency, preference, 'Whether attribution-only reciprocity is normatively sufficient, which is a values question the sibling readings answer differently.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__commons_coordination_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__commons_coordination_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__commons_coordination_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__commons_coordination_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__commons_coordination_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__commons_coordination_reading, theater_ratio, 25, 0.05).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__commons_coordination_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__commons_coordination_reading, base_extractiveness, 5, 0.06).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__commons_coordination_reading, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__commons_coordination_reading, base_extractiveness, 15, 0.07).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__commons_coordination_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__commons_coordination_reading, base_extractiveness, 25, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__commons_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(permissive_license_text__commons_coordination_reading, 0.02).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__copyleft_counterfactual_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__corporate_moat_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the permissive_license_text kernel. commons_coordination_reading (this file) evaluates the license against a strict-copyright baseline and finds low epsilon coordination with a universal beneficiary pool and no victim set. copyleft_counterfactual_reading evaluates the same text against a reciprocity-obligation baseline and finds the absence of a reciprocity requirement structurally exploitation-enabling. corporate_moat_reading evaluates the same text against a fairness-of-capture baseline and finds uncompensated extraction favoring resourced commercial adopters at the expense of unpaid individual authors. Per the epsilon-invariance principle, these are three separate constraints sharing one kernel text, not one constraint with three epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
