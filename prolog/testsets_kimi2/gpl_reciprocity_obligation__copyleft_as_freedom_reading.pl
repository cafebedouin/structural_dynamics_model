% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation â Copyleft as Freedom Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the copyleft_as_freedom_reading of the
 *   gpl_reciprocity_obligation kernel. The kernel is the GNU General Public
 *   License's requirement that derivative works be licensed under the same
 *   terms. This reading interprets the viral reciprocity obligation as a
 *   freedom-preserving mechanism that prevents proprietary capture of
 *   communal software development. It is contested by the
 *   copyleft_as_restriction_reading, which frames the obligation as a harmful
 *   constraint on legitimate business models, and the
 *   copyleft_as_commons_reading, which frames it as commons-preserving
 *   institutional technology. The structural asymmetry is between downstream
 *   users who receive source-code assurance and proprietary integrators who
 *   lose the ability to close derivative works.
 *
 * KEY AGENTS:
 *   - Downstream users (powerless/mobile): structural beneficiaries who gain source-code assurance
 *   - GPL copyright holders (organized/mobile): agenda_setters who enforce the reciprocity obligation
 *   - Proprietary integrators (powerful/constrained): payers who lose proprietary closure options
 *   - Permissive advocates (organized/mobile): excluded voices arguing for non-reciprocal licensing
 *   - Software policy researchers (analytical/analytical): observers studying licensing effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.65).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.75).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation â Copyleft as Freedom Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'b3647400-a9e7-49e7-b88e-8bbbbcee586c').
narrative_ontology:cs_kernel_codification('b3647400-a9e7-49e7-b88e-8bbbbcee586c', fixed_text).
narrative_ontology:cs_authority_grounding('b3647400-a9e7-49e7-b88e-8bbbbcee586c', distributed).
narrative_ontology:cs_reading_relation('b3647400-a9e7-49e7-b88e-8bbbbcee586c', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3647400-a9e7-49e7-b88e-8bbbbcee586c', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('b3647400-a9e7-49e7-b88e-8bbbbcee586c', foundational, user_autonomy_requires_source_reciprocity).
narrative_ontology:cs_axiom_status(user_autonomy_requires_source_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('b3647400-a9e7-49e7-b88e-8bbbbcee586c', user_autonomy_requires_source_reciprocity, deontological).
narrative_ontology:cs_axiom('b3647400-a9e7-49e7-b88e-8bbbbcee586c', foundational, proprietary_enclosure_violates_user_rights).
narrative_ontology:cs_axiom_status(proprietary_enclosure_violates_user_rights, holdable).
narrative_ontology:cs_axiom_grounding('b3647400-a9e7-49e7-b88e-8bbbbcee586c', proprietary_enclosure_violates_user_rights, deontological).
narrative_ontology:cs_reference_frame('b3647400-a9e7-49e7-b88e-8bbbbcee586c', recursive_user_freedom).
narrative_ontology:cs_drift_state('b3647400-a9e7-49e7-b88e-8bbbbcee586c', contemporary_cloud_computing_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b3647400-a9e7-49e7-b88e-8bbbbcee586c', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_community).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive source code and a legal assurance that derivative works of the software they use will remain modifiable and redistributable. They benefit structurally from the barrier to proprietary enclosure but do not participate in enforcement. Exit means switching to proprietary or permissively-licensed alternatives that lack the reciprocity guarantee.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    powerless, biographical, mobile, global).

% Hold copyright in GPL-licensed works and enforce the reciprocity obligation through compliance notices, negotiations, and selective litigation. They choose the license terms and trigger enforcement, shaping what counts as acceptable use. Exit means relicensing their own works under permissive or proprietary terms.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_copyright_holders, agenda_setter,
    organized, generational, mobile, global).

% Seek to incorporate existing software into proprietary products and services. The reciprocity obligation forces them to release source code for derivative works or to refrain from distribution altogether. They bear compliance costs, legal exposure, and the loss of proprietary control over their own modifications. Exit means avoiding all GPL code, fully complying with source release, or replacing components with permissively-licensed alternatives.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Advocate for permissive licenses that permit proprietary integration without reciprocity requirements. They are structurally excluded from the freedom reading's normative framework because their preferred licensing model is precisely the alternative that the constraint suppresses, though they remain active in broader open-source discourse.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, permissive_advocates, excluded,
    organized, generational, mobile, global).

% Study the empirical effects of copyleft licensing on innovation, enclosure, and developer behavior. They analyze enforcement patterns, compliance costs, and comparative outcomes across licensing regimes without enforcing or bearing the constraint directly.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of communal software development by ensuring that downstream distributors cannot free-ride on a shared codebase and then strip users of the ability to study, modify, or share improved versions.
% TRANSFER_FUNCTION: Moves the obligation to distribute source code from original authors to anyone who distributes derivative works, and moves usable source code and legal assurance from distributors to end users.
% ABSENT_VOICES: Proprietary software vendors and permissive-license advocates argue that reciprocity is an unnecessary restriction on code combination and business-model choice; they are present in broader discourse but excluded from the freedom reading's normative beneficiary framework.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight, proprietary firms could incorporate GPL code without releasing source, the assurance that downstream users can modify their software would collapse, enclosure of the software commons would accelerate, and the institutional foundation of the largest collaborative software projects would reorganize around permissive or proprietary licensing.
% FOUNDING_PROBLEM: Early software sharing was being enclosed by proprietary capture: companies took communal code, improved it privately, and distributed only binaries, stripping recipients of the source code and freedoms to study, modify, or redistribute.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and GPL authors attest the enclosure dynamic remains live today. Proprietary integrators and permissive-license proponents attest that permissive licensing has demonstrated viable alternatives without reciprocity mandates. Independent software historians corroborate the original enclosure dynamic but dispute its current severity across all domains.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.65 because proprietary integrators face a genuine forced transfer: either release their own source code or forgo use of the entire GPL codebase. Suppression is 0.75 because the constraint depends on copyright-enforcement machinery to actively suppress proprietary licensing of derivatives. Theater_ratio is 0.35 because while some enforcement is performative (public compliance shaming), the underlying legal mechanism produces real structural outcomes. Accessibility_collapse is 0.80 because once GPL code is incorporated, the licensing path is effectively locked without complete rewrite. Resistance is 0.60 because proprietary actors actively develop workarounds (clean-room reimplementation, SaaS distribution models, lobbying for weaker copyleft).
 *
 * PERSPECTIVAL GAP:
 *   From the downstream user seat, the constraint is protective: it structurally subsidizes their access to modifiable source and prevents enclosure. From the proprietary integrator seat, the same constraint is extractive: it strips their ability to combine communal improvements with proprietary layers and keep the result closed. From the copyright-holder seat, the constraint is ideological enforcementâa tool to preserve a specific software ecosystem. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream_users are beneficiaries with mobile exit, placing d near the beneficiary end (low effective extraction, subsidized access). Proprietary_integrators are declared victims with constrained exit, placing d near the full-target end (high effective extraction). Gpl_copyright_holders are agenda_setters with mobile exit; they do not bear the constraint's costs and their directionality is near the beneficiary end, though their benefit is ideological rather than monetary.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a mandatrophy candidate. The founding problemâproprietary enclosure of communal softwareâremains live, as evidenced by continued SaaS circumvention, embedded-device tivoization, and attempts to proprietize Linux derivatives. The reciprocity mechanism still performs the function it was built for, even if partially circumvented by network-distribution models. The Tangled Rope classification captures both the genuine coordination function (preserving user-modifiable software) and the asymmetric extraction from proprietary integrators, preventing misclassification as either pure Rope or pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_ambiguity,
    'Is the derivative-work boundary (static linking, dynamic linking, plugin architectures, network interaction) a natural technical fact or a legally constructed ambiguity that amplifies extraction?',
    'Comparative jurisdictional analysis of court rulings on what constitutes a derivative work under GPL, combined with technical audit of common integration patterns.',
    'If the boundary is largely constructed, the constraint''s extractiveness is inflated by legal uncertainty rather than technical necessity, pushing the effective extraction higher for proprietary integrators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, conceptual, 'Whether the derivative-work boundary is natural or constructed').

omega_variable(
    enforcement_motive_uncertainty,
    'Does contemporary GPL enforcement primarily serve user-freedom preservation, or has it partly shifted toward settlement extraction and institutional rent-seeking?',
    'Systematic review of enforcement outcomes: proportion of cases resulting in source release versus monetary settlement, and the distribution of settlement proceeds.',
    'If enforcement has shifted toward rent extraction, the theater_ratio understates the performative component and the constraint''s coordination function is weaker than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_motive_uncertainty, empirical, 'Whether enforcement serves freedom or rent extraction').

omega_variable(
    saas_circumvention_drift,
    'Does the network-distribution loophole (SaaS, cloud hosting without distribution) represent a fatal drift in the freedom-preserving function, or a legitimate scope boundary?',
    'Comparative analysis of GPL versus AGPL adoption rates, and empirical measurement of source-code availability for networked GPL derivatives.',
    'If the loophole has substantially hollowed out the reciprocity obligation, the constraint''s current extraction is lower than historical levels and its coordination function is partially obsolete outside distributed-software contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(saas_circumvention_drift, empirical, 'Whether SaaS circumvention has hollowed out GPL reciprocity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_copyleft_freedom_tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gpl_copyleft_freedom_tr_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(gpl_copyleft_freedom_tr_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(gpl_copyleft_freedom_tr_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(gpl_copyleft_freedom_tr_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 35, 0.35).

% Extraction over time
narrative_ontology:measurement(gpl_copyleft_freedom_be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gpl_copyleft_freedom_be_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(gpl_copyleft_freedom_be_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(gpl_copyleft_freedom_be_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(gpl_copyleft_freedom_be_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 35, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl_copyleft_freedom_su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gpl_copyleft_freedom_su_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(gpl_copyleft_freedom_su_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(gpl_copyleft_freedom_su_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(gpl_copyleft_freedom_su_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 35, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
