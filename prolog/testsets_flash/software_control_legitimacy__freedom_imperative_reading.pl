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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Software Control as Fundamental User Freedom (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'freedom imperative' reading of software
 *   control, asserting that proprietary software is ethically illegitimate
 *   due to its denial of user freedom. It frames proprietary software as a
 *   fundamental violation of rights, leading to a high perceived
 *   extractiveness from users and developers of proprietary systems. The
 *   constraint is actively enforced through advocacy and ideological
 *   pressure, rather than legal means, and meets significant resistance from
 *   the proprietary software industry. This is one reading of the
 *   'software_control_legitimacy' kernel.
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
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Software Control as Fundamental User Freedom (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660').
narrative_ontology:cs_kernel_codification('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', implicit).
narrative_ontology:cs_authority_grounding('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', distributed).
narrative_ontology:cs_reading_relation('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', software_control_legitimacy__pragmatic_openness_reading, forecloses).
narrative_ontology:cs_reading_relation('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', foundational, user_freedom_is_absolute).
narrative_ontology:cs_axiom_status(user_freedom_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', user_freedom_is_absolute, deontological).
narrative_ontology:cs_axiom('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', foundational, proprietary_software_is_unethical).
narrative_ontology:cs_axiom_status(proprietary_software_is_unethical, holdable).
narrative_ontology:cs_axiom_grounding('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', proprietary_software_is_unethical, deontological).
narrative_ontology:cs_reference_frame('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', absolute_user_autonomy).
narrative_ontology:cs_drift_state('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', contemporary_digital_ecosystem, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11a40ac8-3cbc-4c6d-bbc3-9f5df2b81660', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, free_software_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These users believe that software freedom is a fundamental right, and they benefit from the ideological clarity and moral imperative of this reading. They are identity-locked into this perspective, seeing any proprietary software as a violation of their autonomy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders, beneficiary,
    organized, generational, identity_locked, global).

% These are the proponents and enforcers of the 'freedom imperative' reading. They actively campaign against proprietary software, develop free alternatives, and educate users on the ethical implications of software control. Their agenda is to shift the entire software ecosystem towards freedom.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, agenda_setter,
    organized, generational, constrained, global).

% Users who rely on proprietary software for work, education, or personal use. From the perspective of the freedom imperative, they are 'victims' of a system that denies them control, even if they perceive benefits from the software itself. Their 'payment' is the loss of freedom and autonomy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users, payer,
    powerless, biographical, constrained, global).

% Developers who create and distribute proprietary software. This reading labels their work as ethically illegitimate, imposing a moral cost and ideological pressure. Their 'payment' is the constant challenge to their business model and ethical standing.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_developers, payer,
    powerful, biographical, constrained, global).

% Developers who value open source for its technical benefits but do not subscribe to the categorical ethical rejection of proprietary software. They are excluded from the 'freedom imperative' discourse due to their more nuanced stance.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, pragmatic_open_source_developers, excluded,
    moderate, biographical, mobile, global).

% Legal professionals who analyze software licensing and intellectual property rights. They observe the ideological conflict from a legal framework, noting the tension between property rights and claims of fundamental user freedom.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, intellectual_property_lawyers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community of users and developers around the shared ethical principle of software freedom, fostering collaboration on free software projects and collective resistance against proprietary models.
% TRANSFER_FUNCTION: Transfers moral authority and legitimacy from proprietary software models to free software models, effectively 'extracting' ethical standing from closed-source developers and 'subsidizing' free software advocates.
% ABSENT_VOICES: Developers and users who see proprietary software as a legitimate choice, either for business reasons (property_rights_reading) or for practical benefits (pragmatic_openness_reading), are largely absent from the core discourse of the freedom imperative, their perspectives dismissed as ethically compromised.
% DISAPPEARANCE_RATIONALE: If the 'freedom imperative' reading vanished, the ethical landscape of software would fundamentally shift. Proprietary software would lose a major source of ideological challenge, potentially reducing pressure for open alternatives. The free software movement would lose its core moral grounding, forcing a re-evaluation of its mission and tactics.
% FOUNDING_PROBLEM: The perceived loss of user control and autonomy due to the rise of proprietary software, leading to a system where users are subservient to software vendors.
% FOUNDING_PROBLEM_CORROBORATION: Free software advocates and a segment of privacy-conscious users attest that the problem is very much alive, citing ongoing issues with digital rights management, vendor lock-in, and surveillance. Proprietary software developers and many mainstream users, however, contest this, arguing that the benefits of proprietary software outweigh perceived control issues. No external, neutral party fully corroborates the 'live' status from a non-ideological stance, as the core claim is ethical.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).

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
 *   The extractiveness is high (0.9) because this reading categorically rejects proprietary software as inherently unethical, imposing a severe moral cost on its existence. Suppression (0.7) reflects the ideological pressure and social shaming directed at proprietary software and its users, aiming to suppress its legitimacy. Resistance (0.95) is high due to the ongoing, fundamental conflict with the entrenched proprietary software industry. Accessibility collapse (0.8) is high because, from this perspective, 'true' freedom is largely inaccessible within the proprietary ecosystem. Theater ratio is low (0.1) as the movement is driven by genuine conviction, not performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of free software advocates, this is a necessary ethical stance (a 'mountain' of moral truth), while from the perspective of proprietary software developers, it is a 'snare' that delegitimizes their work and seeks to extract their market share. The engine's classification as 'snare' reflects the coercive and extractive nature of this ideological enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Users as rights-holders and free software advocates are the primary beneficiaries, as this reading validates their ethical stance and empowers their movement. Proprietary software users and developers are the victims, as their choices and livelihoods are deemed ethically illegitimate. The directionality for proprietary software users is high (near 1.0) because they are seen as 'trapped' by systems that deny them freedom, even if they don't perceive themselves as victims. Proprietary software developers also face high directionality due to the direct challenge to their ethical standing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_vs_practical_legitimacy,
    'Is the ethical illegitimacy claimed by the freedom imperative reading universally applicable, or does it conflict with practical benefits and property rights recognized by other readings?',
    'Analysis of user behavior in jurisdictions with strong free software advocacy vs. those with dominant proprietary markets: if users consistently choose proprietary software despite ethical arguments, it suggests a strong practical counter-legitimacy.',
    'If practical legitimacy consistently overrides ethical claims for a majority of users, the effective extractiveness of this reading (its ability to ''extract'' moral standing from proprietary software) would be lower, potentially shifting its classification towards a ''piton'' of ideological inertia rather than an active ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_vs_practical_legitimacy, conceptual, 'The tension between ethical claims of freedom and the practical realities of software use and development.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., lack of viable free alternatives for critical tasks) or internalized (e.g., users internalizing the ethical imperative and feeling guilt for using proprietary software)?',
    'Post-exit suppression trajectory: if users continue to feel ethical pressure after being presented with viable free alternatives, it suggests internalized suppression. If they readily switch, it''s more structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. This would reinforce the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ethical claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 1985, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1985, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(soft_tr_t1995, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(soft_tr_t2005, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(soft_tr_t2015, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1985, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(soft_be_t1995, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(soft_be_t2005, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(soft_be_t2015, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2015, 0.85).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1985, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(soft_su_t1995, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(soft_su_t2005, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(soft_su_t2015, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel, each representing a distinct structural claim about software control. This 'freedom_imperative_reading' focuses on user freedom as the primary ethical determinant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
