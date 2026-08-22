% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__freedom_imperative_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Proprietary Software Source Restriction (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint story models the standing arrangement of proprietary
 *   software licensing â wherein source code is withheld from users and
 *   modification or redistribution are legally restricted â as read through
 *   the freedom_imperative lens of the software_source_status kernel. From
 *   this reading, the arrangement is a fundamental ethical violation: all
 *   users of proprietary software are structurally victimized by being denied
 *   inalienable rights to study, modify, and share the code that runs their
 *   computing. The reading treats licensing restrictions as categorically
 *   illegitimate constraints that persist through legal coercion and
 *   network-effect lock-in, not through genuine consent. This story
 *   instantiates one committed reading of a four-way contested kernel,
 *   routing sibling-structure to omega variables and the CS commitment-system
 *   layer.
 *
 * KEY AGENTS:
 *   - Proprietary software vendors: agenda-setter and beneficiary â they define licensing terms, enforce restrictions, and capture revenue.
 *   - Software users: primary payer â they surrender freedoms and fees under constrained exit.
 *   - Independent developers: secondary payer â blocked from source access for dependencies and platforms.
 *   - Free software advocates: observer â they resist the arrangement and maintain alternative licensing infrastructure.
 *   - National copyright regimes: enforcement infrastructure â they codify the legal basis for source restriction.
 *   - Global South developers: excluded â criminalized as pirates and absent from policy forums.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.88).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.82).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software Source Restriction (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'bc580e64-ab72-43d6-a133-8083a6920eeb').
narrative_ontology:cs_kernel_codification('bc580e64-ab72-43d6-a133-8083a6920eeb', formalized).
narrative_ontology:cs_authority_grounding('bc580e64-ab72-43d6-a133-8083a6920eeb', lineage).
narrative_ontology:cs_interpretation_layer_present('bc580e64-ab72-43d6-a133-8083a6920eeb').
narrative_ontology:cs_reading_relation('bc580e64-ab72-43d6-a133-8083a6920eeb', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc580e64-ab72-43d6-a133-8083a6920eeb', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('bc580e64-ab72-43d6-a133-8083a6920eeb', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('bc580e64-ab72-43d6-a133-8083a6920eeb', foundational, users_inherent_source_access_rights).
narrative_ontology:cs_axiom_status(users_inherent_source_access_rights, holdable).
narrative_ontology:cs_axiom_grounding('bc580e64-ab72-43d6-a133-8083a6920eeb', users_inherent_source_access_rights, deontological).
narrative_ontology:cs_axiom('bc580e64-ab72-43d6-a133-8083a6920eeb', foundational, proprietary_restriction_categorically_illegitimate).
narrative_ontology:cs_axiom_status(proprietary_restriction_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('bc580e64-ab72-43d6-a133-8083a6920eeb', proprietary_restriction_categorically_illegitimate, deontological).
narrative_ontology:cs_reference_frame('bc580e64-ab72-43d6-a133-8083a6920eeb', libre_software_ethic).
narrative_ontology:cs_drift_state('bc580e64-ab72-43d6-a133-8083a6920eeb', contemporary_digital_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bc580e64-ab72-43d6-a133-8083a6920eeb', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, independent_developers).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, intellectual_property_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, software_as_private_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define licensing terms, deploy technical restrictions, and enforce copyright through legal systems to capture revenue from software distribution. They administer the legal-technical infrastructure that withholds source code and modification rights from users.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Use proprietary software under licenses that forbid modification, redistribution, and often inspection of source code. They pay license fees and surrender control over their computing environment, with limited practical alternatives for many professional and social functions due to network effects and format lock-in.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_users, payer,
    organized, biographical, constrained, global).

% Build software atop proprietary platforms and tools, denied access to source code for underlying systems. They bear compliance costs, licensing fees for tools, and creative restrictions from closed ecosystems that limit interoperability and reuse.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Maintain the ethical critique of proprietary software, develop copyleft licenses, and advocate for policy changes. They observe and resist the licensing arrangement but do not operate within its revenue structure.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_advocates, observer,
    organized, generational, analytical, global).

% Codify and enforce the legal scaffolding that makes source-code restriction actionable through statutory copyright terms, anti-circumvention provisions, and international treaty obligations. They set the formal rules that vendors operationalize.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, national_copyright_regimes, agenda_setter,
    institutional, civilizational, constrained, global).

% Software practitioners and users in low-income regions who face criminalization under international copyright enforcement for unlicensed use or distribution. They are structurally excluded from the policy forums where licensing frameworks are negotiated and from the capital required to purchase legal licenses.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, global_south_developers, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Proprietary licensing coordinates the funding of software development by creating legally enforceable excludability over digital goods, addressing the collective-action problem of financing production when copies have near-zero marginal cost.
% TRANSFER_FUNCTION: Moves monetary rents and control over computing from software users and independent developers to copyright holders, in exchange for access to compiled executables. Simultaneously transfers the freedoms to study, modify, and redistribute from users to vendors.
% ABSENT_VOICES: Individual non-technical users who lack vocabulary to demand source-code rights are effectively muted in policy discourse. Developers in the Global South are criminalized as pirates rather than admitted as stakeholders with legitimate alternative needs.
% DISAPPEARANCE_RATIONALE: If proprietary source restrictions vanished overnight, software funding models would reorganize around services, support, SaaS, patronage, and commons-based peer production. Vendor business models dependent on copy scarcity would collapse, and the global software stack would shift toward transparent, modifiable infrastructure.
% FOUNDING_PROBLEM: Non-excludable digital goods create free-rider problems in funding software development, making voluntary payment unreliable for sustaining large-scale production.
% FOUNDING_PROBLEM_CORROBORATION: Free software historians and economists cite successful commons-based development as evidence the problem is solvable without proprietary exclusion. Vendors and incumbent copyright scholars attest that capital-intensive software requires excludability. Academic observers outside the vendor beneficiary set corroborate both positions, leaving the status genuinely contested.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint denies users fundamental operational freedoms while extracting monopoly rents; suppression is similarly high (0.82) because persistence depends on international copyright enforcement, anti-circumvention law, and the suppression of unauthorized distribution. Theater is moderate (0.35): rhetoric about innovation and security is performative, but the core enforcement machinery is functionally directed at maintaining excludability. Accessibility collapse (0.75) is high because network effects, file-format lock-in, and hardware compatibility make practical exit difficult even where free alternatives exist. Resistance (0.55) reflects sustained but institutionally outgunned opposition from the free software movement. The measurement series track a single shared time grid from 1980 to 2025.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat experiences the constraint as legitimate coordination: without excludability, software production would face underproduction. The user and independent-developer seats experience the same structure as denial of fundamental freedoms. The engine computes this divergence from the structural data â beneficiary/victim declarations and exit asymmetries â without requiring the author to reconcile the frames. The global_south_developer seat experiences the constraint as criminalization of poverty, a further asymmetry not captured by the vendor-user binary.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary_software_vendors sit at low directionality (d near 0.0): they are the structural beneficiaries who collect rents and control the legal-technical boundary. Software_users and independent_developers sit at high directionality (d near 1.0): the constraint extracts both money and freedoms from them, and their exit is blocked by network effects, format lock-in, and legal prohibitions. National_copyright_regimes sit near symmetric (d ~0.5): they enforce but do not personally collect. Free_software_advocates are analytical observers with no stake in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists mislabeling by separating the coordination function (funding development through excludability) from the extraction function (denying source freedoms and monopoly pricing). A pure-snare reading would ignore the genuine resource-allocation coordination that proprietary licensing performs; a pure-rope reading would ignore the asymmetric extraction of user freedoms and the coercive enforcement required to maintain the arrangement. Tangled_rope captures that both are structurally present, even as the freedom-imperative reading denies the coordination function's moral legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the freedom_imperative reading correctly identify the proprietary constraint as purely extractive, or does it obscure a genuine coordination function that the reading''s deontological frame cannot register?',
    'Comparative institutional analysis of software production funding across proprietary, copyleft, and commons-based regimes, measuring output quantity and maintenance sustainability.',
    'If proprietary licensing carries irreplaceable coordination function, the constraint is tangled_rope; if the coordination is replicable without extraction, the freedom reading''s snare classification gains structural support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the freedom reading''s deontological lens captures or misses the coordination function.').

omega_variable(
    ethical_naturalness_ambiguity,
    'Is the claim that software freedom is a fundamental ethical requirement a discovered moral fact or a constructed political commitment?',
    'Meta-ethical analysis of whether digital-rights claims derive from empirical features of computation or from conventionally established social contracts.',
    'If a discovered moral fact, the constraint''s normative force approaches mountain-like universality; if constructed, it remains a contestable political position within the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_naturalness_ambiguity, conceptual, 'Meta-ethical status of the freedom-imperative claim.').

omega_variable(
    suppression_efficiency_ambiguity,
    'Does the persistence of proprietary software rest primarily on legal suppression of alternatives or on economic efficiency in resource allocation?',
    'Natural experiment from jurisdictions with weakened software patent and copyright enforcement; if proprietary share collapses, suppression is primary; if it persists, coordination efficiency is primary.',
    'Legal-dominant persistence would raise suppression and support snare classification; efficiency-dominant persistence would support tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_efficiency_ambiguity, empirical, 'Structural vs efficiency mechanism in software licensing persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t9, software_source_status__freedom_imperative_reading, theater_ratio, 9, 0.12).
narrative_ontology:measurement(soft_tr_t18, software_source_status__freedom_imperative_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(soft_tr_t27, software_source_status__freedom_imperative_reading, theater_ratio, 27, 0.24).
narrative_ontology:measurement(soft_tr_t36, software_source_status__freedom_imperative_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement(soft_tr_t45, software_source_status__freedom_imperative_reading, theater_ratio, 45, 0.35).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(soft_be_t9, software_source_status__freedom_imperative_reading, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(soft_be_t18, software_source_status__freedom_imperative_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(soft_be_t27, software_source_status__freedom_imperative_reading, base_extractiveness, 27, 0.78).
narrative_ontology:measurement(soft_be_t36, software_source_status__freedom_imperative_reading, base_extractiveness, 36, 0.85).
narrative_ontology:measurement(soft_be_t45, software_source_status__freedom_imperative_reading, base_extractiveness, 45, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(soft_su_t9, software_source_status__freedom_imperative_reading, suppression_requirement, 9, 0.55).
narrative_ontology:measurement(soft_su_t18, software_source_status__freedom_imperative_reading, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(soft_su_t27, software_source_status__freedom_imperative_reading, suppression_requirement, 27, 0.72).
narrative_ontology:measurement(soft_su_t36, software_source_status__freedom_imperative_reading, suppression_requirement, 36, 0.78).
narrative_ontology:measurement(soft_su_t45, software_source_status__freedom_imperative_reading, suppression_requirement, 45, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the freedom_imperative_reading of the software_source_status kernel, which decomposes the colloquial label 'software licensing' into four structurally distinct readings with different epsilon values and stakeholder configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
