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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software as Freedom Denial (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'freedom imperative' reading of the
 *   'software_control_legitimacy' kernel. From this perspective, proprietary
 *   software is ethically illegitimate because it fundamentally denies users
 *   control over their computing, thereby infringing on a core freedom. The
 *   constraint describes the structural denial of freedom inherent in
 *   proprietary software models, enforced through legal and technical means.
 *   This reading views proprietary software as a snare, trapping users in
 *   systems they cannot control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.9).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.88).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software as Freedom Denial (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '3fbd1504-c1c9-4153-8833-59027900055a').
narrative_ontology:cs_kernel_codification('3fbd1504-c1c9-4153-8833-59027900055a', formalized).
narrative_ontology:cs_authority_grounding('3fbd1504-c1c9-4153-8833-59027900055a', expertise).
narrative_ontology:cs_reading_relation('3fbd1504-c1c9-4153-8833-59027900055a', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fbd1504-c1c9-4153-8833-59027900055a', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('3fbd1504-c1c9-4153-8833-59027900055a', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('3fbd1504-c1c9-4153-8833-59027900055a', foundational, user_control_is_fundamental_freedom).
narrative_ontology:cs_axiom_status(user_control_is_fundamental_freedom, holdable).
narrative_ontology:cs_axiom_grounding('3fbd1504-c1c9-4153-8833-59027900055a', user_control_is_fundamental_freedom, deontological).
narrative_ontology:cs_axiom('3fbd1504-c1c9-4153-8833-59027900055a', secondary, proprietary_software_is_digital_subjugation).
narrative_ontology:cs_axiom_status(proprietary_software_is_digital_subjugation, holdable).
narrative_ontology:cs_axiom_grounding('3fbd1504-c1c9-4153-8833-59027900055a', proprietary_software_is_digital_subjugation, deontological).
narrative_ontology:cs_reference_frame('3fbd1504-c1c9-4153-8833-59027900055a', user_sovereignty_over_computing).
narrative_ontology:cs_drift_state('3fbd1504-c1c9-4153-8833-59027900055a', contemporary_digital_economy, gap(stable, minor, false)).
narrative_ontology:cs_created_at('3fbd1504-c1c9-4153-8833-59027900055a', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, independent_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, free_software_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users are denied fundamental control over their computing devices and software, forced to accept proprietary terms that restrict their ability to study, modify, or share the software they use. Their digital lives are increasingly dependent on systems they cannot control.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, software_users, payer,
    powerless, biographical, identity_locked, global).

% These entities design, distribute, and license software under terms that deny users control, thereby securing revenue streams and market dominance. They enforce these restrictions through legal means (EULAs, DMCA) and technical means (DRM, obfuscation).
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Actively resist proprietary software, developing and promoting free and open-source alternatives. They bear the cost of advocacy and development in a system dominated by proprietary models, but maintain an analytical exit from the proprietary paradigm.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, observer,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, payer).

% Often find their ability to innovate and distribute software constrained by proprietary platforms and ecosystems. They are forced to comply with vendor-imposed rules, limiting their freedom and often extracting a share of their revenue.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Represent proprietary software vendors in enforcing their intellectual property rights, which are seen by this reading as mechanisms for denying user freedom. They are instrumental in maintaining the legal framework that underpins proprietary control.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, intellectual_property_lawyers, agenda_setter,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, proprietary software primarily coordinates control and revenue for vendors, rather than genuinely solving a user coordination problem. Any perceived coordination (e.g., ease of use) is secondary to the denial of freedom.
% TRANSFER_FUNCTION: Transfers control over computing, the right to study, modify, and share software, and often personal data, from software users to proprietary software vendors.
% ABSENT_VOICES: Users who are unaware of the ethical implications of proprietary software, or those who have internalized the idea that convenience or functionality must come at the cost of freedom. Their voices are absent from the ethical debate, often due to marketing and default choices.
% DISAPPEARANCE_RATIONALE: If proprietary software and its underlying legal/technical enforcement vanished overnight, the entire digital economy would undergo a fundamental restructuring. Users would gain unprecedented control, leading to a massive shift in software development, distribution, and business models, prioritizing freedom and collaboration.
% FOUNDING_PROBLEM: The problem of users losing control over their computing to vendors, leading to digital subjugation and a fundamental erosion of freedom in the digital realm.
% FOUNDING_PROBLEM_CORROBORATION: Free software foundations, digital rights organizations, and academic critiques of digital monopolies consistently attest that the problem of user control and freedom remains live and is, in fact, intensifying with the rise of cloud computing and 'software as a service' models. This corroboration comes from outside the proprietary software industry itself.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.90) is high because proprietary software, by its very definition, extracts fundamental freedoms (to run, study, modify, and share software) from users. Suppression (0.88) is also high, as proprietary licenses, DRM, and legal frameworks actively prevent users from exercising these freedoms, effectively collapsing alternatives. The accessibility collapse (0.92) reflects the pervasive nature of proprietary software, making it nearly impossible for users to avoid systems that deny them control. Resistance (0.75) is significant, driven by the free software movement and digital rights advocates. Theater ratio (0.10) is low because the ethical claim is direct and the denial of freedom is a core, functional aspect of proprietary software, not a performance.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between this reading, which sees proprietary software as a fundamental denial of freedom, and readings like 'property_rights_reading' which view vendor control as a legitimate exercise of intellectual property. The engine will compute vastly different classifications for these perspectives based on their differing beneficiary/victim declarations and metric assessments.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software vendors are the clear beneficiaries, as they gain control, revenue, and market power by restricting user freedoms. Software users and independent developers are the primary victims, as they are denied control and constrained by proprietary ecosystems. Free software advocates, while resisting, bear costs in their efforts to provide alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, from the freedom imperative reading, is not subject to mandatrophy in the traditional sense, as its 'mandate' (the denial of freedom) is seen as inherently illegitimate and persistent. The analysis here focuses on exposing the underlying extractive structure rather than detecting a decay of function. The 'founding problem' of user subjugation is considered 'live' because the ethical imperative for freedom remains unfulfilled by proprietary models.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_illegitimacy_vs_mitigable_harm,
    'Is proprietary software inherently ethically illegitimate due to its denial of user freedom, or can its harms be mitigated through alternative licensing, business models, or regulatory oversight?',
    'Analysis of hybrid models (e.g., open core, source-available with restrictions) and their impact on user control, or empirical study of regulatory interventions designed to restore user freedoms within proprietary contexts.',
    'If harms are mitigable without abandoning proprietary models entirely, the extractiveness of proprietary software might be re-evaluated as lower, potentially shifting the classification from a pure snare to a tangled rope (if a coordination function is identified). If inherent, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_illegitimacy_vs_mitigable_harm, conceptual, 'Whether the ethical illegitimacy of proprietary software is absolute or conditional.').

omega_variable(
    user_perception_of_freedom_denial,
    'To what extent do average software users perceive their lack of control over proprietary software as a denial of fundamental freedom, versus a reasonable trade-off for convenience, functionality, or security?',
    'Large-scale user surveys, ethnographic studies of digital literacy and autonomy, and analysis of public discourse regarding software ownership and control.',
    'If a significant portion of users do not perceive this as a denial of freedom, the ''victim'' status might be re-evaluated, potentially lowering the effective extractiveness (χ) for those seats, even if the base extractiveness (ε) remains high from the ethical perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(user_perception_of_freedom_denial, empirical, 'The gap between the ethical claim of freedom denial and user''s lived experience/perception.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one specific reading of the ''software_control_legitimacy'' kernel. What would be the structural implications if a different reading (e.g., ''property_rights_reading'') were adopted as the dominant framework?',
    'Comparative analysis of legal systems and software ecosystems where different readings are dominant, examining their impact on software licensing, development, and user rights.',
    'Adopting the ''property_rights_reading'' would likely reclassify proprietary software as a rope or even a mountain (from the vendor''s perspective), with users as beneficiaries of innovation, and significantly lower extractiveness and suppression from that seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identification of this constraint as the ''freedom_imperative_reading'' within the ''software_control_legitimacy'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t6, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(soft_tr_t12, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(soft_tr_t18, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(soft_be_t6, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 6, 0.8).
narrative_ontology:measurement(soft_be_t12, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 12, 0.85).
narrative_ontology:measurement(soft_be_t18, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 18, 0.88).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 24, 0.89).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 30, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(soft_su_t6, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 6, 0.78).
narrative_ontology:measurement(soft_su_t12, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 12, 0.83).
narrative_ontology:measurement(soft_su_t18, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 18, 0.86).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 24, 0.87).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 30, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, digital_rights_legislation).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, open_source_licensing_regimes).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'freedom_imperative_reading' of the 'software_control_legitimacy' kernel. Its ε value reflects the ethical illegitimacy of proprietary software from the perspective of user freedom, which differs significantly from other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
