% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Software Source Status: Freedom Imperative Reading (Proprietary Software as Injustice)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom imperative' reading of
 *   the 'software_source_status' kernel. From this perspective, proprietary
 *   software is fundamentally an injustice, as it denies users the four
 *   essential freedoms (run, study, redistribute, modify). The constraint is
 *   the system of legal and technical restrictions that enforce proprietary
 *   software's closed nature. It is classified as a Snare because its primary
 *   function is extraction (of freedom and control) and it relies on active
 *   suppression of alternatives (open source development, user modification).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.9).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.75).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Software Source Status: Freedom Imperative Reading (Proprietary Software as Injustice)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, '92913eb8-504f-4d81-890c-a8abcbea299a').
narrative_ontology:cs_kernel_codification('92913eb8-504f-4d81-890c-a8abcbea299a', implicit).
narrative_ontology:cs_authority_grounding('92913eb8-504f-4d81-890c-a8abcbea299a', diffuse_epistemic).
narrative_ontology:cs_reading_relation('92913eb8-504f-4d81-890c-a8abcbea299a', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('92913eb8-504f-4d81-890c-a8abcbea299a', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('92913eb8-504f-4d81-890c-a8abcbea299a', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('92913eb8-504f-4d81-890c-a8abcbea299a', foundational, software_freedom_is_fundamental_right).
narrative_ontology:cs_axiom_status(software_freedom_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('92913eb8-504f-4d81-890c-a8abcbea299a', software_freedom_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('92913eb8-504f-4d81-890c-a8abcbea299a', foundational, proprietary_software_is_unjust).
narrative_ontology:cs_axiom_status(proprietary_software_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('92913eb8-504f-4d81-890c-a8abcbea299a', proprietary_software_is_unjust, deontological).
narrative_ontology:cs_reference_frame('92913eb8-504f-4d81-890c-a8abcbea299a', universal_software_freedom).
narrative_ontology:cs_drift_state('92913eb8-504f-4d81-890c-a8abcbea299a', contemporary_digital_economy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('92913eb8-504f-4d81-890c-a8abcbea299a', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, independent_developers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, public_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users of proprietary software are denied fundamental freedoms: the freedom to run the program for any purpose, to study how it works, to redistribute copies, and to distribute modified versions. They are locked into systems that control their computing experience and often their data.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_users, payer,
    powerless, biographical, identity_locked, global).

% Developers who wish to build upon or modify proprietary software are legally and technically restricted, hindering innovation and collaboration. They are forced to either work within restrictive ecosystems or develop entirely new, often incompatible, solutions.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Governments, schools, and public services that rely on proprietary software compromise their autonomy and public trust. They are unable to audit the software for backdoors, adapt it to public needs, or ensure long-term access and control of critical infrastructure.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, public_institutions, payer,
    organized, generational, constrained, national).

% These advocates articulate and defend the ethical imperative of software freedom, working to expose the injustices of proprietary software and promote free alternatives. They seek to change legal and social norms to align with the principles of user liberty.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_advocates, agenda_setter,
    organized, civilizational, analytical, global).

% From this reading's perspective, proprietary software vendors are the agents of injustice, creating and enforcing constraints that deny users their fundamental rights. Their business model is seen as inherently unethical, and their arguments for property rights are rejected as illegitimate.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, excluded,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading asserts that true coordination in software development and use can only occur when all participants have the freedom to share, study, and modify software. Proprietary models are seen as inherently anti-coordinative, creating silos and power imbalances.
% TRANSFER_FUNCTION: Proprietary software transfers control, knowledge, and potential for modification from users and the public to the software's owner. It also transfers economic value from users to vendors without providing commensurate ethical value.
% ABSENT_VOICES: The voices of proprietary software vendors are absent from this ethical framework, as their claims to intellectual property rights are deemed illegitimate and their business practices are considered unjust. Their perspective is fundamentally incompatible with the core axioms of software freedom.
% DISAPPEARANCE_RATIONALE: If proprietary software and its associated legal/technical restrictions vanished overnight, the entire software industry would undergo a radical transformation. Users would gain unprecedented control, innovation would accelerate through open collaboration, and new economic models based on services and support (rather than restricted access) would emerge. The digital world would be fundamentally reshaped.
% FOUNDING_PROBLEM: The problem this reading addresses is the historical emergence of proprietary software, which created a system where users were denied control over their computing tools, leading to a loss of freedom and autonomy in the digital realm.
% FOUNDING_PROBLEM_CORROBORATION: Free software advocates and numerous academic ethicists corroborate that the problem of proprietary software's injustice remains live and pervasive, citing ongoing examples of user lock-in, surveillance, and lack of control. This is attested by independent legal analysis and philosophical arguments from outside the proprietary software industry.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because proprietary software fundamentally extracts user freedom and control, which this reading considers an inalienable right. Suppression is also high, as proprietary software relies on copyright law, EULAs, and technical measures (DRM, obfuscation) to prevent users from exercising their freedoms. Theater ratio is low because the constraint's function is direct and effective in its stated goal of restricting access and modification; there is little performative maintenance. Accessibility collapse is moderate because while proprietary software is pervasive, free software alternatives do exist, though often with significant switching costs or feature gaps. Resistance is high due to the ongoing advocacy and development efforts within the free software movement.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those who view software freedom as an ethical imperative and those who view software as property. This reading asserts that the 'property rights' perspective is a cover for an extractive system, while the 'pragmatic development' and 'utilitarian hybrid' readings are seen as compromising fundamental ethical principles for instrumental or aggregate welfare gains.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software users, independent developers, and public institutions are victims, as they are denied fundamental freedoms and control. Free software advocates act as agenda-setters, working to dismantle this unjust system. Proprietary software vendors are structurally excluded from this ethical framework, as their very existence is seen as a violation of the freedom imperative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (to deny freedom) is seen as inherently unjust from its inception. The classification as a Snare directly reflects this, preventing mislabeling it as a coordination mechanism or a degraded form of support. The problem it 'solves' (enabling proprietary business models) is itself the injustice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''freedom imperative'' reading of the ''software_source_status'' kernel, or is it conflating with other readings?',
    'Comparison with canonical texts of the free software movement (e.g., GNU Manifesto, works by Richard Stallman) to ensure fidelity to core principles and victim identification.',
    'If conflated, the extractiveness and suppression metrics might be diluted by considerations from other readings (e.g., pragmatic benefits of open source), leading to an inaccurate classification of the ethical stance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring the constraint strictly adheres to the ''freedom imperative'' interpretation.').

omega_variable(
    identity_locked_vs_constrained_exit,
    'For proprietary software users, is their exit truly ''identity_locked'' (e.g., professional identity tied to specific proprietary tools) or merely ''constrained'' (high switching costs, but no identity fusion)?',
    'Qualitative sociological studies of user communities and professional groups, examining the psychological and social costs of switching from proprietary to free software alternatives.',
    'If exit is merely constrained, the effective extraction might be slightly lower, as the agent retains more agency. If truly identity-locked, the suppression is deeper and more insidious, amplifying effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_constrained_exit, empirical, 'Distinguishing between identity-based and purely economic/technical lock-in for users.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, technical DRM) or internalized (lack of awareness of alternatives, perceived difficulty of free software)?',
    'Post-exit suppression trajectory: if suppression persists after legal/technical barriers are removed (e.g., in regions with weaker IP enforcement), reclassify as partially internalized. Surveys on user awareness and perceived competence with free software.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — users carry the suppression with them even if external barriers weaken. This would amplify the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in proprietary software use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 1983, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1983, software_source_status__freedom_imperative_reading, theater_ratio, 1983, 0.05).
narrative_ontology:measurement(soft_tr_t1995, software_source_status__freedom_imperative_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(soft_tr_t2005, software_source_status__freedom_imperative_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(soft_tr_t2015, software_source_status__freedom_imperative_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__freedom_imperative_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1983, software_source_status__freedom_imperative_reading, base_extractiveness, 1983, 0.7).
narrative_ontology:measurement(soft_be_t1995, software_source_status__freedom_imperative_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(soft_be_t2005, software_source_status__freedom_imperative_reading, base_extractiveness, 2005, 0.85).
narrative_ontology:measurement(soft_be_t2015, software_source_status__freedom_imperative_reading, base_extractiveness, 2015, 0.88).
narrative_ontology:measurement(soft_be_t2024, software_source_status__freedom_imperative_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1983, software_source_status__freedom_imperative_reading, suppression_requirement, 1983, 0.5).
narrative_ontology:measurement(soft_su_t1995, software_source_status__freedom_imperative_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(soft_su_t2005, software_source_status__freedom_imperative_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(soft_su_t2015, software_source_status__freedom_imperative_reading, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(soft_su_t2024, software_source_status__freedom_imperative_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, identity_coordination).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_source_status' kernel. This 'freedom imperative' reading views proprietary software as an ethical injustice, distinct from pragmatic, property-rights, or utilitarian perspectives. All readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
