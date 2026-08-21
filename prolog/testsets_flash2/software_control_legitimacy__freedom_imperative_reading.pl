% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software as a Denial of User Freedom (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'freedom imperative' reading of software
 *   control legitimacy, asserting that proprietary software is ethically
 *   illegitimate due to its denial of fundamental user freedoms. From this
 *   perspective, any software that restricts users' ability to run, study,
 *   modify, and distribute it is inherently extractive. The high
 *   extractiveness (0.9) reflects the categorical rejection of proprietary
 *   models, viewing them as a fundamental violation of rights. Suppression
 *   (0.7) is present in the pervasive nature of proprietary software and the
 *   difficulty users face in avoiding it, often due to network effects or
 *   hardware dependencies. Resistance (0.8) is high, driven by the active
 *   free software movement. The claimed type is 'snare' because the
 *   coordination story (commercial sustainability, ease of use) is seen as a
 *   cover for a system that traps users and extracts their freedom.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.9).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.7).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software as a Denial of User Freedom (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, 'f0a530c6-6c75-4def-98cf-0426a3263ea9').
narrative_ontology:cs_kernel_codification('f0a530c6-6c75-4def-98cf-0426a3263ea9', implicit).
narrative_ontology:cs_authority_grounding('f0a530c6-6c75-4def-98cf-0426a3263ea9', lineage).
narrative_ontology:cs_interpretation_layer_present('f0a530c6-6c75-4def-98cf-0426a3263ea9').
narrative_ontology:cs_reading_relation('f0a530c6-6c75-4def-98cf-0426a3263ea9', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0a530c6-6c75-4def-98cf-0426a3263ea9', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('f0a530c6-6c75-4def-98cf-0426a3263ea9', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('f0a530c6-6c75-4def-98cf-0426a3263ea9', foundational, user_control_is_fundamental_freedom).
narrative_ontology:cs_axiom_status(user_control_is_fundamental_freedom, holdable).
narrative_ontology:cs_axiom_grounding('f0a530c6-6c75-4def-98cf-0426a3263ea9', user_control_is_fundamental_freedom, deontological).
narrative_ontology:cs_axiom('f0a530c6-6c75-4def-98cf-0426a3263ea9', foundational, proprietary_software_denies_freedom).
narrative_ontology:cs_axiom_status(proprietary_software_denies_freedom, holdable).
narrative_ontology:cs_axiom_grounding('f0a530c6-6c75-4def-98cf-0426a3263ea9', proprietary_software_denies_freedom, empirically_contingent).
narrative_ontology:cs_reference_frame('f0a530c6-6c75-4def-98cf-0426a3263ea9', universal_user_freedom).
narrative_ontology:cs_drift_state('f0a530c6-6c75-4def-98cf-0426a3263ea9', contemporary_digital_economy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f0a530c6-6c75-4def-98cf-0426a3263ea9', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users of proprietary software are denied fundamental control over their computing, forced to accept terms that restrict their freedom to inspect, modify, and share the software they rely on. Their 'choice' to use such software is often constrained by network effects or lack of viable free alternatives, making exit difficult.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users, payer,
    powerless, biographical, identity_locked, global).

% Developers who create proprietary software are seen as perpetuating a system that denies user freedom. While they benefit financially, their work contributes to the 'snare' from this reading's perspective, making them both agents of and participants in the extractive system. They are 'victims' in the sense that their creative output is framed as ethically illegitimate.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_developers, payer,
    moderate, biographical, constrained, global).

% This group represents the ideal state of users who assert their fundamental right to control their computing. They benefit from the ethical imperative to reject proprietary software, as it aligns with their core values of freedom and autonomy. They are the conceptual beneficiaries of a world where this constraint is resolved.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders, beneficiary,
    organized, generational, analytical, universal).

% These advocates actively promote the ethical imperative of user freedom and the illegitimacy of proprietary software. They set the agenda for the movement, define the terms of the debate, and work to educate users and developers about the importance of free software. They are the primary enforcers of this ethical constraint.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, agenda_setter,
    organized, generational, mobile, global).

% Those who believe software control is primarily a property right are fundamentally at odds with this reading. Their arguments for intellectual property protection are dismissed as justifications for denying fundamental user freedoms. They are excluded from the ethical framework of this reading.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, property_rights_proponents, excluded,
    institutional, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a moral stance and a political movement around the principle of user freedom in computing, fostering a community dedicated to developing and using free software.
% TRANSFER_FUNCTION: Transfers moral legitimacy and control from proprietary software developers and vendors to users, reframing proprietary software as an ethical violation rather than a legitimate commercial offering.
% ABSENT_VOICES: Proponents of intellectual property rights in software are absent from this ethical framework; their arguments for creator control and commercial viability are considered irrelevant or harmful to the fundamental imperative of user freedom.
% DISAPPEARANCE_RATIONALE: If the ethical imperative of user freedom vanished, the moral argument against proprietary software would collapse. The free software movement would lose its foundational ethical grounding, and the debate would shift entirely to pragmatic or economic terms, fundamentally altering the landscape of software development and consumption.
% FOUNDING_PROBLEM: The rise of proprietary software created a power imbalance where users lost control over their computing, leading to a denial of fundamental freedoms and a dependency on vendors.
% FOUNDING_PROBLEM_CORROBORATION: Free software advocates attest that the problem is acutely live, citing ongoing restrictions, DRM, and vendor lock-in. Proprietary software users, through their experiences of limited control, corroborate the existence of the problem, even if they don't always frame it in terms of ethical illegitimacy. Independent legal scholars and digital rights organizations also provide corroboration from outside the immediate beneficiary group.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness stems from the ethical framing: any restriction on user freedom is a maximal extraction. Suppression is significant because proprietary software is the default in many contexts, and alternatives require conscious effort and often technical skill. Theater ratio is low (0.1) because the ethical stance is direct and uncompromising; there's little performative justification beyond the core claim of user freedom. Accessibility collapse is moderate (0.3) because while free alternatives exist, they are not always readily accessible or equivalent in functionality, and switching costs can be high. Resistance is high (0.8) due to the organized and vocal free software movement actively challenging the status quo.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between this reading's ethical imperative and the pragmatic or property-rights-based views. From the perspective of proprietary software developers, they are providing a valuable service and exercising their property rights, not denying freedom. The engine's classification will highlight this divergence, showing a 'snare' from the freedom imperative perspective versus potentially a 'rope' or 'tangled_rope' from other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software users are the primary victims, as their freedom is denied (high d). Proprietary software developers are also victims in the sense that their work is deemed ethically illegitimate, even if they benefit financially (moderate d, as they are also agents of the system). Users as rights-holders are the conceptual beneficiaries (low d), representing the ideal state this reading aims for. Free software advocates are the agenda-setters, actively working to enforce this ethical constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_freedom_denial,
    'To what extent does proprietary software truly deny ''fundamental'' user freedom, versus merely restricting certain functionalities or choices?',
    'Empirical studies on user agency and control in various software environments, combined with philosophical analysis of ''fundamental'' freedoms in digital contexts.',
    'If the denial of freedom is less fundamental than claimed, the extractiveness score might be lower, potentially shifting the classification towards a Tangled Rope or even a Rope if genuine coordination benefits are found to outweigh the restrictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_freedom_denial, conceptual, 'Ambiguity in the definition and scope of ''fundamental user freedom'' in software.').

omega_variable(
    practical_alternatives_availability,
    'Are there genuinely viable and accessible free software alternatives for all critical proprietary applications, or are users often practically ''trapped'' by a lack of functional substitutes?',
    'Market analysis of free vs. proprietary software ecosystems, user surveys on switching costs and perceived quality of alternatives, and technical assessments of feature parity.',
    'If viable alternatives are scarce, the suppression score would be higher, reinforcing the ''snare'' classification by demonstrating a lack of genuine exit options. If alternatives are abundant, suppression would be lower, weakening the ''snare'' argument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practical_alternatives_availability, empirical, 'The actual availability and accessibility of free software alternatives for users.').

omega_variable(
    ethical_vs_pragmatic_framing,
    'Is the primary motivation for advocating free software truly an ethical imperative of freedom, or is it often a pragmatic argument for better software quality, security, or community collaboration?',
    'Content analysis of free software advocacy literature, interviews with movement leaders, and examination of policy proposals to discern underlying justifications.',
    'If pragmatic arguments dominate, this reading''s high extractiveness (based on ethical violation) might be seen as overstating the case, potentially shifting the classification towards a ''pragmatic_openness_reading'' which would likely be a Rope or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_vs_pragmatic_framing, conceptual, 'Distinguishing between ethical and pragmatic motivations for free software advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 1983, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1983, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1983, 0.05).
narrative_ontology:measurement(soft_tr_t1995, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(soft_tr_t2005, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(soft_tr_t2015, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1983, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1983, 0.8).
narrative_ontology:measurement(soft_be_t1995, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1995, 0.85).
narrative_ontology:measurement(soft_be_t2005, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2005, 0.88).
narrative_ontology:measurement(soft_be_t2015, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2015, 0.9).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1983, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1983, 0.6).
narrative_ontology:measurement(soft_su_t1995, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(soft_su_t2005, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(soft_su_t2015, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel. Each reading presents a distinct structural claim about software control, leading to different classifications and stakeholder dynamics. This 'freedom imperative' reading focuses on ethical illegitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
