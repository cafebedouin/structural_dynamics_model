% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary â Enclosure Reading
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint instantiates the enclosure reading of the derivative-work
 *   statutory boundary: the claim that any use of copyrighted expression in a
 *   new work constitutes preparation of a derivative work requiring
 *   authorization. It is a maximalist interpretation of 17 U.S.C. Â§ 101/106
 *   that expands the copyright holder's veto over downstream creation,
 *   enforced through statutory damages, platform liability, and automated
 *   filtering.
 *
 * KEY AGENTS:
 *   - incumbent_copyright_holders: Primary beneficiary/agenda_setter (institutional/arbitrage) â collects licensing rents and enforces broad derivative rights through litigation
 *   - independent_remixers: Primary target (powerless/constrained) â bears licensing costs and creative suppression
 *   - technology_platforms: Secondary target (powerful/constrained) â bears compliance and over-removal costs
 *   - fair_use_advocates: Excluded voice (organized/analytical) â would argue for narrower boundaries but is absent from negotiations
 *   - federal_judiciary: Analytical observer (analytical/analytical) â interprets the statutory boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.85).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.9).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary â Enclosure Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '686a6857-9137-4428-9bf0-0dd69aa918f9').
narrative_ontology:cs_kernel_codification('686a6857-9137-4428-9bf0-0dd69aa918f9', formalized).
narrative_ontology:cs_authority_grounding('686a6857-9137-4428-9bf0-0dd69aa918f9', lineage).
narrative_ontology:cs_interpretation_layer_present('686a6857-9137-4428-9bf0-0dd69aa918f9').
narrative_ontology:cs_reading_relation('686a6857-9137-4428-9bf0-0dd69aa918f9', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('686a6857-9137-4428-9bf0-0dd69aa918f9', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('686a6857-9137-4428-9bf0-0dd69aa918f9', foundational, exclusive_preparation_authorization).
narrative_ontology:cs_axiom_status(exclusive_preparation_authorization, holdable).
narrative_ontology:cs_axiom_grounding('686a6857-9137-4428-9bf0-0dd69aa918f9', exclusive_preparation_authorization, conventional).
narrative_ontology:cs_axiom('686a6857-9137-4428-9bf0-0dd69aa918f9', foundational, no_transformative_exemption).
narrative_ontology:cs_axiom_status(no_transformative_exemption, holdable).
narrative_ontology:cs_axiom_grounding('686a6857-9137-4428-9bf0-0dd69aa918f9', no_transformative_exemption, conventional).
narrative_ontology:cs_reference_frame('686a6857-9137-4428-9bf0-0dd69aa918f9', maximal_statutory_enclosure).
narrative_ontology:cs_drift_state('686a6857-9137-4428-9bf0-0dd69aa918f9', post_transformative_use_doctrine_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('686a6857-9137-4428-9bf0-0dd69aa918f9', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_remixers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, technology_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control large catalogs of music, film, and text. They generate revenue by requiring licenses for any new work that incorporates their copyrighted expression, and they file lawsuits to stop unlicensed uses. Their market position depends on being able to treat sampling, remixing, and intermediate copying as infringing derivative works.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders, beneficiary).

% Musicians, video artists, and software developers who build new expression by referencing, sampling, or remixing existing copyrighted material. Under the prevailing interpretation, each use requires a separate license that is often unavailable or unaffordable, forcing many to abandon projects or distribute underground.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_remixers, payer,
    powerless, biographical, constrained, national).

% Operate content-hosting services and must implement automated filtering systems to detect and block material that may constitute a derivative work. They face legal exposure for user uploads and therefore over-remove content, absorbing significant engineering and legal compliance costs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, technology_platforms, payer,
    powerful, biographical, constrained, global).

% Public-interest lawyers, academics, and digital-rights groups who argue that transformative and non-commercial uses should fall outside the derivative-work boundary. They file amicus briefs and advocate for statutory reform but are rarely seated at the legislative bargaining table where copyright expansion is negotiated.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, fair_use_advocates, excluded,
    organized, generational, analytical, national).

% Interprets the Copyright Act and applies the derivative-work definition in infringement cases. While some courts have narrowed the boundary through fair-use analysis, the statutory text and industry litigation pressure sustain the broad enclosure reading.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, federal_judiciary, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for original authors to control adaptations of their work, theoretically preserving the integrity of the original and its market by requiring authorization for any reworkings that incorporate its expression.
% TRANSFER_FUNCTION: Moves licensing revenue, creative control, and compliance burdens from downstream creators and distribution platforms to incumbent copyright holders, backed by the threat of statutory damages and injunctive relief.
% ABSENT_VOICES: Remix artists, open-source developers, and public-domain advocates are structurally excluded from copyright policy negotiations; they would argue for a narrower boundary and broader exemptions but are not in the legislative or standard-setting rooms where the enclosure reading is maintained.
% DISAPPEARANCE_RATIONALE: If the maximal derivative-work boundary disappeared overnight, downstream creators would no longer need licenses for transformative or intermediate uses, platform filtering obligations would shrink, and incumbent licensing revenue would fall sharply; the creative economy would reorganize around more open appropriation and reuse.
% FOUNDING_PROBLEM: Unauthorized adaptations risked displacing the market for original works and reducing the economic incentive to produce expressive content.
% FOUNDING_PROBLEM_CORROBORATION: Independent economists and technology-policy researchers attest that the incentive rationale is overstated and that broad derivative rights now function primarily as rent-preservation for incumbents. Incumbent industry associations attest the problem remains live. Corroboration from outside the benefiting parties includes empirical studies on copyright's marginal incentive effects and amicus briefs from library and technology associations.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the enclosure reading treats virtually any use of prior expression as a derivative work requiring a license, decoupled from actual market harm. Suppression is even higher (0.90) because the constraint depends on statutory damages, automated filtering mandates, and the threat of injunction to pre-empt creation. Theater is moderate (0.40): much enforcement is functional (real legal judgments and platform liability), but a significant share is performative deterrence. Accessibility collapse is high (0.80) because fair use and public domain are technically available but practically inaccessible due to risk aversion and licensing friction. Resistance is moderate (0.55) because public-interest groups and some technology firms actively contest the boundary, though they have not overcome incumbent institutional power.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent copyright holder seat and the downstream creator seat compute to divergent types: from the incumbent position the arrangement protects legitimate property and investment incentives; from the constrained creator position the same structure operates as a licensing snare that extracts before creation can occur. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent copyright holders are declared beneficiaries with arbitrage-grade exit (global licensing markets), placing their directionality near the beneficiary pole. Independent remixers and technology platforms are declared victims with constrained exit, placing their directionality near the target pole. The effective extraction is therefore amplified for creators and damped (or inverted into subsidy) for incumbents.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both declared victims and active enforcement for the snare type. The constraint claims a coordination function (protecting original authorship incentives), but the authored metrics show high extraction and suppression without symmetric benefit to the coordinated parties. The divergence between the claimed coordination story and the actual victim structure is what the engine registers as a snare rather than a rope or tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_dependent_classification,
    'Is the derivative work boundary a single constraint with observable-dependent epsilon, or do the enclosure, coordination, and hybrid readings instantiate structurally distinct constraints?',
    'Apply the epsilon-invariance test: if changing the reading changes epsilon, they are distinct constraints. Compare the three sibling stories'' metrics and computed types.',
    'If they are distinct, each reading warrants its own constraint story and the kernel is a family; if they are one constraint, the variation is observer-indexical and the epsilon variance indicates measurement error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_dependent_classification, conceptual, 'Whether kernel readings are distinct constraints').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Is the suppression of unlicensed derivative creation achieved primarily through direct legal coercion or through internalized platform self-censorship and automated filtering?',
    'Quantify the ratio of DMCA takedowns and statutory damage awards to voluntary over-filtering by platforms; measure removal rates relative to judicial determinations of infringement.',
    'If suppression is largely internalized and platform-mediated, the effective extraction exceeds what legal incidence alone suggests, because targets carry the constraint with them after any formal legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    incentive_rhetoric_naturalness,
    'Is the broad derivative work right a natural extension of authorship or a constructed enclosure that benefits identifiable incumbents?',
    'Historical analysis of copyright scope expansion; comparative law examining jurisdictions with narrower derivative rights and different innovation rates.',
    'If purely constructed and beneficiary-captured, the constraint''s legitimacy as natural law collapses and it reclassifies as a false-summit candidate; if grounded in a natural-right theory, the extraction may be reframed as legitimate rent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incentive_rhetoric_naturalness, conceptual, 'Natural law vs constructed enclosure ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(deri_tr_t32, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(deri_be_t32, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 32, 0.83).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(deri_su_t8, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(deri_su_t16, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(deri_su_t32, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 32, 0.88).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 40, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This constraint is the enclosure reading of the derivative_work_statutory_boundary kernel, decomposed per the epsilon-invariance principle from the coordination and hybrid carveout readings. The siblings share the same statutory kernel but instantiate structurally distinct claims with different epsilon values and victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
