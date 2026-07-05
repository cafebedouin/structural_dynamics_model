% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Utilitarian Hybrid Reading of Software Source Status (Context-Optimized Licensing)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the utilitarian hybrid reading of the
 *   software_source_status kernel: the claim that software licensing should
 *   be evaluated case-by-case for its effect on aggregate welfare rather than
 *   governed by a single categorical rule. Under this reading, foundational
 *   infrastructure tends toward openness (network effects,
 *   security-by-scrutiny, avoidance of lock-in externalities favor open
 *   licensing at that layer) while narrow, high-fixed-cost specialized tools
 *   can legitimately be proprietary (the welfare loss from restricted access
 *   is outweighed by the welfare gain from investment the proprietary model
 *   funds). This is a distinct constraint from its sibling readings, not a
 *   synthesis or an average of them — it has its own ε, its own beneficiary
 *   structure (diffuse, context-dependent, with no categorical victim set),
 *   and its own classification path. The freedom_imperative_reading (software
 *   freedom as unconditional ethical requirement), the
 *   pragmatic_development_reading (openness as superior methodology, freedom
 *   as instrumental), and the property_rights_reading (software as property,
 *   restriction as a legitimate right) are each separate constraint stories
 *   with their own metrics; they are not represented inside this file except
 *   as named siblings in cs_structure and as excluded-voice stakeholders
 *   whose categorical claims this reading declines to adopt as trumps.
 *
 * KEY AGENTS:
 *   - infrastructure_dependent_developers: primary beneficiary (moderate/mobile) — gains from open-licensed foundational layers
 *   - specialized_tool_vendors: beneficiary (organized/mobile) — gains legitimacy for proprietary models in narrow markets
 *   - mixed_ecosystem_integrators: beneficiary and payer (moderate/constrained) — gains flexibility, bears compliance complexity
 *   - software_consuming_public: diffuse beneficiary (powerless/constrained) — benefits indirectly from correctly-routed licensing
 *   - free_software_movement_advocates: excluded voice (organized/civilizational) — categorical claim demoted to one input
 *   - proprietary_rights_absolutists: excluded voice (organized/civilizational) — categorical claim made contingent on welfare test
 *   - welfare_economists_and_policy_analysts: analytical observer — supplies the empirical evidence the standard depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.28).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.18).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Reading of Software Source Status (Context-Optimized Licensing)").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, 'c14a1472-0d20-4c99-9053-c329f48e5470').
narrative_ontology:cs_kernel_codification('c14a1472-0d20-4c99-9053-c329f48e5470', distributed).
narrative_ontology:cs_authority_grounding('c14a1472-0d20-4c99-9053-c329f48e5470', distributed).
narrative_ontology:cs_reading_relation('c14a1472-0d20-4c99-9053-c329f48e5470', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('c14a1472-0d20-4c99-9053-c329f48e5470', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('c14a1472-0d20-4c99-9053-c329f48e5470', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('c14a1472-0d20-4c99-9053-c329f48e5470', foundational, licensing_legitimacy_is_context_dependent).
narrative_ontology:cs_axiom_status(licensing_legitimacy_is_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('c14a1472-0d20-4c99-9053-c329f48e5470', licensing_legitimacy_is_context_dependent, instrumental).
narrative_ontology:cs_axiom('c14a1472-0d20-4c99-9053-c329f48e5470', foundational, no_categorical_source_status_default).
narrative_ontology:cs_axiom_status(no_categorical_source_status_default, holdable).
narrative_ontology:cs_axiom_grounding('c14a1472-0d20-4c99-9053-c329f48e5470', no_categorical_source_status_default, instrumental).
narrative_ontology:cs_reference_frame('c14a1472-0d20-4c99-9053-c329f48e5470', context_dependent_welfare_optimization).
narrative_ontology:cs_drift_state('c14a1472-0d20-4c99-9053-c329f48e5470', contemporary_platform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c14a1472-0d20-4c99-9053-c329f48e5470', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, infrastructure_dependent_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_consuming_public).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, specialized_tool_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_integrators).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, aggregate_welfare_maximization_standard).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, context_dependent_licensing_optimality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build on shared infrastructure — compilers, operating system kernels, networking stacks, cryptographic libraries — where open licensing lets them inspect, fork, and harden the tools their work depends on. Under this reading they benefit from a norm that pushes foundational layers toward openness while leaving room to build proprietary products above that layer.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, infrastructure_dependent_developers, beneficiary,
    moderate, generational, mobile, global).

% Develop narrow, high-investment tools — CAD systems, scientific instrumentation software, niche vertical applications — where the fixed cost of development is large relative to the addressable market. This reading's welfare calculus explicitly validates proprietary licensing here, on the argument that closed models fund development that would not otherwise occur, so vendors are not treated as illegitimate rent-seekers by default.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, specialized_tool_vendors, beneficiary,
    organized, biographical, mobile, global).

% Assemble products that combine open infrastructure with proprietary components — enterprise software vendors, cloud platform builders, embedded systems integrators. They benefit from the reading's permission to mix models but also bear the ongoing cost of navigating license compatibility, dual-licensing terms, and the absence of a single bright-line rule telling them what is permitted.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_integrators, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_integrators, payer).

% Uses the resulting mixed ecosystem of software without much say in how any individual license is set. Benefits when the welfare calculus correctly routes commodity infrastructure toward openness (lower prices, more scrutiny, more interoperability) but has no direct voice in whether any specific vendor's proprietary claim is actually welfare-maximizing or just self-serving.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_consuming_public, beneficiary,
    powerless, generational, constrained, global).

% Hold that software freedom is a non-negotiable ethical baseline, not one input to a welfare calculation to be traded off against vendor investment incentives. This reading treats their categorical claim as one consideration among several rather than as a side constraint, which the advocates experience as their core commitment being quietly demoted rather than engaged.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, free_software_movement_advocates, excluded,
    organized, civilizational, constrained, global).

% Hold that creators have an unconditional right to restrict access to their work regardless of aggregate welfare effects. This reading subordinates that right to a welfare test, which the absolutists experience as their property claim being made contingent on a calculation courts, users, or economists could someday declare against them.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_rights_absolutists, excluded,
    organized, civilizational, constrained, global).

% Attempt to measure whether specific licensing arrangements actually maximize aggregate welfare — comparing consumer surplus, innovation rates, security outcomes, and market concentration across open and proprietary regimes. Their empirical findings are the load-bearing evidence this reading depends on but cannot itself generate.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, welfare_economists_and_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision procedure for allocating source-status (open vs. proprietary) across the software ecosystem by asking, per artifact, which regime produces greater aggregate welfare given that artifact's development-cost structure, network effects, and substitutability — rather than applying one licensing rule universally.
% TRANSFER_FUNCTION: Does not transfer resources through a single fixed channel the way a tangled rope or snare would; instead it allocates legitimacy — it moves the burden of justification onto whichever regime (open or closed) is claimed for a given piece of software, requiring a welfare argument rather than granting either regime default legitimacy.
% ABSENT_VOICES: Free software movement advocates and property-rights absolutists are both structurally present as objectors but functionally sidelined: the welfare calculus treats their categorical claims as one input among several, not as trumps, so neither position's core premise gets to actually determine outcomes even though both are loudly represented in public debate.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the two categorical readings (freedom_imperative and property_rights) would fill the vacuum and each would claim their principle as the correct default, restarting a legitimacy contest that the utilitarian hybrid reading currently manages by refusing to grant either side automatic priority. Vendors and developers who currently rely on 'it depends on context' as a working norm would lose that flexibility and face pressure to justify their licensing choice against a single categorical standard instead of a case-by-case welfare argument — a real practical change for mixed-ecosystem integrators, but not a change to the underlying technical capabilities of any software itself.
% FOUNDING_PROBLEM: Neither pure free-software absolutism nor pure property-rights absolutism could account for observed cases where the 'wrong' regime by their own lights produced better outcomes — infrastructure that thrived once opened, specialized tools that would never have been built without proprietary funding — so a framework was needed that could evaluate licensing choices by their actual welfare effects rather than by which side's foundational premise the software happened to satisfy.
% FOUNDING_PROBLEM_CORROBORATION: Attested partly from outside any single benefiting party: empirical software-economics literature (e.g. studies of open-source adoption curves, security-outcome comparisons, and venture-funded proprietary tool markets) documents genuine welfare variance across licensing regimes that neither categorical camp fully predicts. However, the standard's practical application in any given dispute is still adjudicated largely by parties with a stake in the outcome — courts, industry consortia, and funding bodies whose welfare assessments are not independently audited — so corroboration of the standard's abstract validity is stronger than corroboration of its case-by-case application.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, contested).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.28) and drifts only slightly upward over the interval: this reading does not concentrate extraction on an identifiable victim class the way a snare or tangled rope would — its costs are diffuse (compliance friction for integrators, unresolved contest for the excluded categorical camps) rather than a rent flowing to a concentrated beneficiary. Suppression is low (0.18) because no party is coercively barred from advocating their preferred licensing model; the categorical camps remain free to argue their position, they simply do not get automatic priority. Accessibility collapse is moderate-low (0.25): alternatives to case-by-case welfare reasoning (the categorical readings) remain fully articulable and are in fact live constraints elsewhere in the kernel family, so nothing has collapsed — the reading coexists with rather than eliminates its alternatives. Resistance is moderate (0.35), reflecting genuine, sustained objection from both categorical camps who experience the welfare calculus as illegitimately relativizing what they hold as absolute principles.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure-dependent developer and specialized-vendor seats, this reading looks like straightforward coordination — a sensible rule that lets different needs get different treatment. From the excluded categorical seats (free software absolutists, property rights absolutists) the same reading looks like an erosion of their principle into a mere factor to be weighed, which they experience as a loss even though no resource has been extracted from them in the conventional sense — the loss is normative standing, not material extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholder here is a structural victim in the classical extraction sense — this reading's expected structural delta explicitly specifies no categorical victim set, and the beneficiary declarations reflect that: infrastructure developers, tool vendors, integrators, and the general public all derive some welfare gain from context-appropriate licensing, with costs (mainly compliance complexity for integrators, and standing-loss for the excluded categorical camps) diffused rather than concentrated. This differs sharply from the property_rights_reading, where restricted-access users would be structural targets, and from the freedom_imperative_reading, where proprietary vendors would be structural targets — this reading's whole point is to avoid manufacturing a categorical victim class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (neither absolutist premise survives contact with the empirical record of what actually maximizes welfare) remains live: the underlying tension between infrastructure economics and specialized-tool economics has not resolved, so this is not a mandate that has outlived its function. But because the standard's application in any concrete dispute is often adjudicated by interested parties rather than the analytical seat, there is real risk of mandatrophy at the point of use — a vendor could invoke 'welfare maximization' as cover for what is actually simple rent extraction, without triggering a formal reclassification, because the standard's abstract legitimacy is real even where its case-by-case invocation is not audited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_measurement_underdetermination,
    'Is ''aggregate welfare'' in this reading a well-defined, measurable quantity, or does its apparent precision hide the fact that welfare comparisons across open and proprietary regimes depend on contested weightings (consumer surplus vs. producer surplus, short-run access vs. long-run innovation incentive) that are themselves value judgments?',
    'Cross-check independent welfare-economics studies of comparable licensing decisions for convergent vs. divergent conclusions; divergence driven by weighting choices rather than data would indicate the standard smuggles in unstated value premises under an empirical-sounding label.',
    'If welfare measurement is substantially underdetermined, this reading risks functioning as a laundering mechanism — any licensing choice can be retrofitted with a welfare justification — which would push its practical operation closer to a tangled rope (coordination language covering discretionary extraction) than the rope classification the low measured extractiveness currently supports.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_measurement_underdetermination, conceptual, 'Whether aggregate welfare is a measurable standard or a contested-weighting construct dressed as empirical.').

omega_variable(
    committer_framing_alternative_readings,
    'Is the utilitarian hybrid reading genuinely a fourth distinct position, or is it better understood as a meta-level arbitration procedure that sits above the other three readings and adjudicates between them, rather than as a sibling occupying the same level?',
    'Examine whether any real-world licensing dispute has been resolved by explicit appeal to welfare-maximization reasoning as opposed to appeal to freedom, property rights, or pragmatic development-quality arguments — if disputes are consistently resolved by translating into one of the three other readings'' vocabulary, the hybrid reading may be descriptively empty at the point of application even though it is coherent in principle.',
    'If the hybrid reading is actually a meta-level arbitration function rather than a peer reading, its cs_structure relations to the three siblings would need re-examination — it might more accurately ''influence'' all three rather than merely ''coexist'' with them, since an arbitration procedure structurally constrains how the object-level readings can be invoked in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative_readings, conceptual, 'Whether this reading is a peer position or a meta-level procedure standing above its siblings.').

omega_variable(
    excluded_voice_correction_risk,
    'Does treating the free-software and property-rights absolutist positions as inputs to a welfare calculation rather than as trumps risk systematically under-weighting non-consequentialist harms (e.g., dignitary harms from being denied the right to inspect code that governs one''s own devices) that a welfare framework is not well-suited to capture?',
    'Solicit direct testimony from the excluded stakeholder groups on cases where they believe welfare-framed reasoning produced an outcome they experienced as a rights violation rather than a mere cost, and assess whether the welfare framework has a mechanism for incorporating that testimony as more than one more input.',
    'If such harms are structurally invisible to the welfare calculus, the ''no categorical victim set'' claim in this reading''s expected structural delta would need qualification — there may be a diffuse but real victim class (users and developers whose rights-based objections get systematically discounted) that current metrics do not capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_voice_correction_risk, preference, 'Whether welfare-maximization reasoning structurally discounts rights-based objections that resist consequentialist translation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t5, software_source_status__utilitarian_hybrid_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(soft_tr_t15, software_source_status__utilitarian_hybrid_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(soft_tr_t25, software_source_status__utilitarian_hybrid_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(soft_be_t5, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(soft_be_t15, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(soft_be_t25, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 25, 0.28).

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
% This story is one of four sibling constraints decomposing the natural-language concept 'the software freedom debate' / 'the open source vs proprietary question' per the ε-invariance principle. Each sibling reading of the software_source_status kernel is authored as its own file with its own ε, beneficiary/victim structure, and classification: freedom_imperative_reading (categorical ethical claim, proprietary software as injustice), pragmatic_development_reading (instrumental methodological claim), property_rights_reading (categorical property claim), and this file, utilitarian_hybrid_reading (context-dependent welfare-optimization claim, the only reading with no categorical victim set). The readings are linked bidirectionally via affects_constraints because each reading's practical uptake shifts the legitimacy conditions and resource availability for the others — e.g. growth in this reading's institutional acceptance (welfare-based licensing review in courts or standards bodies) reduces the property_rights_reading's ability to claim unconditional restriction rights, and increases pressure on the freedom_imperative_reading to justify its categorical claim in welfare terms it originally rejected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
