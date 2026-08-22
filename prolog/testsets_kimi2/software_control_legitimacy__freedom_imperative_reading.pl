% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software Control Regime â Freedom Imperative Reading
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_imperative_reading of the
 *   software_control_legitimacy kernel. The standing arrangement under
 *   contest is the proprietary software control regime: the global system of
 *   copyright, end-user licensing, digital rights management, and platform
 *   gatekeeping that restricts users from running, inspecting, modifying, and
 *   sharing software. From this reading's perspective, the regime is
 *   ethically illegitimate because it denies users fundamental control over
 *   their computing. The constraint extracts autonomy and surplus from users
 *   and developers, concentrating control in vendors and platform
 *   gatekeepers. The beneficiary/victim structure reflects the regime's
 *   actual operation, not the reading's normative vision (in which users
 *   would be rights-holders).
 *
 * KEY AGENTS:
 *   - proprietary_vendors: Primary agenda-setter (institutional/arbitrage) â writes licenses and enforces restrictions
 *   - platform_gatekeepers: Secondary agenda-setter/beneficiary (institutional/arbitrage) â controls distribution and collects rents
 *   - software_users: Primary payer (organized/constrained) â denied four freedoms, bears cost of restrictions
 *   - independent_developers: Secondary payer (moderate/constrained) â restricted by proprietary toolchains and platform terms
 *   - free_software_advocates: Analytical observer (organized/analytical) â contests legitimacy from outside the regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.92).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.85).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software Control Regime â Freedom Imperative Reading").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '73c31238-5b93-41ee-9b2f-17f20762098f').
narrative_ontology:cs_kernel_codification('73c31238-5b93-41ee-9b2f-17f20762098f', fixed_text).
narrative_ontology:cs_authority_grounding('73c31238-5b93-41ee-9b2f-17f20762098f', lineage).
narrative_ontology:cs_interpretation_layer_present('73c31238-5b93-41ee-9b2f-17f20762098f').
narrative_ontology:cs_reading_relation('73c31238-5b93-41ee-9b2f-17f20762098f', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('73c31238-5b93-41ee-9b2f-17f20762098f', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('73c31238-5b93-41ee-9b2f-17f20762098f', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('73c31238-5b93-41ee-9b2f-17f20762098f', foundational, software_freedom_as_fundamental_right).
narrative_ontology:cs_axiom_status(software_freedom_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('73c31238-5b93-41ee-9b2f-17f20762098f', software_freedom_as_fundamental_right, deontological).
narrative_ontology:cs_axiom('73c31238-5b93-41ee-9b2f-17f20762098f', foundational, proprietary_control_as_systematic_harm).
narrative_ontology:cs_axiom_status(proprietary_control_as_systematic_harm, holdable).
narrative_ontology:cs_axiom_grounding('73c31238-5b93-41ee-9b2f-17f20762098f', proprietary_control_as_systematic_harm, deontological).
narrative_ontology:cs_reference_frame('73c31238-5b93-41ee-9b2f-17f20762098f', unrestricted_user_sovereignty).
narrative_ontology:cs_drift_state('73c31238-5b93-41ee-9b2f-17f20762098f', contemporary_proprietary_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('73c31238-5b93-41ee-9b2f-17f20762098f', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, proprietary_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, platform_gatekeepers).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, independent_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control software source code and distribution rights through copyright, end-user license agreements, and technical restrictions. They set licensing terms that prohibit modification, redistribution, and often inspection. Collect revenue from access restrictions and maintain market position through network effects, format lock-in, and proprietary standards.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate operating systems and app stores that enforce proprietary software distribution rules. They extract commissions from developers, control signing and notarization, and use hardware-enforced boot restrictions to prevent alternative software stacks. Benefit from the proprietary control regime through developer surplus extraction and user data capture.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, platform_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, platform_gatekeepers, beneficiary).

% Use software under terms that deny them the freedom to run, inspect, modify, or share programs. Subject to surveillance, feature removal, forced updates, and termination of service. Exit to free software exists but incurs practical penalties: incompatible file formats, hardware driver gaps, workflow disruption, and social coordination costs.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, software_users, payer,
    organized, biographical, constrained, global).

% Build applications and services on proprietary platforms and APIs under terms that restrict redistribution of their own work. Cannot fix bugs in underlying proprietary libraries. Face platform risk from unilateral vendor policy changes. Many depend on proprietary toolchains for income even while advocating for open alternatives.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Maintain the free software definition and copyleft licenses. Analyze and publicly contest the legitimacy of proprietary control. Do not collect from either regime directly; their position is analytical and normative, grounded in a commitment to user autonomy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, proprietary_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates artificial scarcity in information goods to enable price discrimination and investment recovery for software development, addressing the zero-marginal-cost reproduction problem through legal restriction.
% TRANSFER_FUNCTION: Transfers control over computing from users and independent developers to proprietary vendors and platform gatekeepers; transfers monetary surplus from users and developers to vendors through licensing fees, subscription rents, and platform commissions.
% ABSENT_VOICES: Users in the global south with limited bandwidth and hardware to self-host free alternatives; developers inside proprietary firms who would prefer to release source but are contractually bound; small business users lacking legal resources to challenge EULA terms; hardware owners blocked by proprietary firmware from installing alternative operating systems.
% DISAPPEARANCE_RATIONALE: If proprietary control vanished overnight, software distribution would shift immediately to source-available and copyleft models, vendor revenue models would reorganize around services and support rather than scarcity, hardware business models would fragment, and users would regain the four freedoms. The global software economy would rearrange around non-scarce distribution and user sovereignty.
% FOUNDING_PROBLEM: How to sustain investment in software creation when digital information can be copied at near-zero marginal cost; how to prevent free-riding on development costs in information goods.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary vendors and industry associations attest the problem remains live, citing development costs and cybersecurity risks. Free software advocates, empirical studies of open-source sustainability (e.g., Nadia Eghbal, Linux Foundation reports), and economists such as Boldrin and Levine attest that alternative funding and coordination mechanisms exist and are viable. No consensus from outside the beneficiary set.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.92, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.92) is near-maximum because the regime categorically denies users the four freedoms (use, study, share, modify) across almost all commercial software. Suppression (0.85) is high because the constraint depends on active legal enforcement (copyright, DMCA anti-circumvention, contract law), technical enforcement (DRM, signed bootloaders, SaaS lock-in), and market suppression (network effects, format incompatibility). Theater ratio (0.52 at interval end) has crossed above the Goodhart threshold: a growing share of enforcement activity defends revenue extraction rather than user safety or software quality, evidenced by EULAs that are unread and unenforceable in practice, license audits targeting compliance theater, and security justifications for control that primarily serve vendor interests. Accessibility collapse (0.75) is high because once a user or organization adopts proprietary ecosystems, the cost of switching to free alternatives rises steeply due to file formats, workflows, and social coordination. Resistance (0.60) reflects the sustained free software movement, widespread piracy, jailbreaking, and recent regulatory challenges, but these remain insufficient to dislodge the regime.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat experiences the constraint as legitimate property rights and necessary investment protection (likely computing as rope or tangled_rope from that seat). The user seat experiences it as structural extraction (snare). The freedom-imperative reading authors the metrics from the user/developer perspective, creating the claim/metric divergence that the engine is designed to detect. The agenda-setter/beneficiary seats have arbitrage-grade exit (they can adopt open models if profitable) while payer seats are constrained by network effects and practical lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors and platform gatekeepers are beneficiaries (d near the beneficiary end â they collect the extracted autonomy and surplus). Software users are the primary victims (d near the full-target end â the constraint is constructed specifically to deny them control). Independent developers are also victims but slightly less targeted than end users. Free software advocates are analytical observers with no directionality stake in the constraint's operation. No override is needed: the structural derivation from beneficiary/victim declarations plus exit options correctly maps the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the proprietary regime as rope by requiring victim identification for snare classification and by testing whether the coordination story (funding software production) is cover for extraction. The freedom imperative reading treats the funding story as a cover: empirical evidence shows substantial software production under free terms, and the regime's persistence exceeds what genuine coordination would require. Mandatrophy would occur if the funding justification became obsolete but the control persisted; the contested status of the founding problem and rising theater ratio suggest partial mandatrophy is already present, though full classification remains snare because extraction is still actively captured by beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_alternative_viability,
    'Can large-scale software development be sustainably funded without proprietary control mechanisms?',
    'Longitudinal study of firms operating under open-source business models and public-software funding initiatives (e.g., Mozilla, Red Hat, EU public code, GitHub Sponsors).',
    'If yes, the proprietary regime''s coordination justification collapses, strengthening snare classification; if no, the extraction reading must acknowledge a genuine coordination problem that its endorsed alternative may not solve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_alternative_viability, empirical, 'Whether alternative funding exists for software without proprietary restriction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal and technical barriers) or internalized (users believe proprietary control is natural, deserved, or necessary for quality)?',
    'Post-exit suppression trajectory: assess whether users who migrate to free software ecosystems continue to feel moral obligation to obey proprietary licenses or anxiety about using unauthorized software.',
    'If internalized, effective suppression exceeds the structural measure â users carry the constraint with them after formal exit, amplifying extraction beyond what legal enforcement alone would achieve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    commons_reading_convergence,
    'Does the freedom imperative reading''s categorical rejection of proprietary software foreclose convergence with the commons reading''s negotiated governance, or can copyleft function as a commons mechanism?',
    'Comparative analysis of whether copyleft licenses operate as absolute freedom enforcement or as bounded collective management tools that accommodate some coordination constraints.',
    'If convergent, the freedom reading is less absolutist than pure snare classification suggests; if divergent, the categorical stance hardens the boundary and supports the high-extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_reading_convergence, conceptual, 'Whether freedom imperative and commons readings are structurally compatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 30, 0.88).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 40, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is the freedom_imperative_reading of the software_control_legitimacy kernel. The kernel decomposes into four structurally distinct constraints because each reading assigns a different epsilon, beneficiary/victim structure, and normative foundation to the standing arrangement of software control. This reading assesses the proprietary regime as high-extraction snare; siblings assess it as property-rights coordination (rope/tangled_rope), pragmatic methodology choice (rope), or commons governance (scaffold/commons).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
