% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Free Software Freedom Imperative (Ethical Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the freedom_imperative_reading of the
 *   software_source_status kernel: the claim that software freedom is a
 *   fundamental ethical requirement and that proprietary software is a
 *   categorical injustice. Under this reading, the constraint is the active
 *   social, legal, and technical arrangement that enforces source code
 *   availability and delegitimizes proprietary restrictions. The kernel is
 *   contested: sibling readings include the pragmatic_development_reading
 *   (open source as methodology), the property_rights_reading (software as
 *   legitimate IP), and the utilitarian_hybrid_reading (context-dependent
 *   licensing). This reading is structurally distinct because it treats
 *   proprietary restrictions not as inefficient or contextually suboptimal
 *   but as categorically illegitimate, creating a victim set of proprietary
 *   vendors and a beneficiary set of users and the free software commons.
 *
 * KEY AGENTS:
 *   - free_software_community: Agenda-setter (institutional/generational) â creates copyleft licenses and adjudicates the Free Software Definition
 *   - software_users: Beneficiary (powerless/biographical) â receive rights to study, modify, and share source code
 *   - proprietary_vendors: Victim (powerful/biographical) â bear the costs of denied restriction rights and threatened business models
 *   - hardware_oems: Payer (powerful/biographical) â compelled to release driver and firmware source
 *   - software_historians: Observer (analytical/generational) â document the licensing paradigm contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.62).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.58).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Free Software Freedom Imperative (Ethical Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'eb70890e-ae81-4db0-b89f-872a39bd3a41').
narrative_ontology:cs_kernel_codification('eb70890e-ae81-4db0-b89f-872a39bd3a41', formalized).
narrative_ontology:cs_authority_grounding('eb70890e-ae81-4db0-b89f-872a39bd3a41', lineage).
narrative_ontology:cs_interpretation_layer_present('eb70890e-ae81-4db0-b89f-872a39bd3a41').
narrative_ontology:cs_reading_relation('eb70890e-ae81-4db0-b89f-872a39bd3a41', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb70890e-ae81-4db0-b89f-872a39bd3a41', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('eb70890e-ae81-4db0-b89f-872a39bd3a41', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('eb70890e-ae81-4db0-b89f-872a39bd3a41', foundational, software_freedom_fundamental_ethical_requirement).
narrative_ontology:cs_axiom_status(software_freedom_fundamental_ethical_requirement, holdable).
narrative_ontology:cs_axiom_grounding('eb70890e-ae81-4db0-b89f-872a39bd3a41', software_freedom_fundamental_ethical_requirement, deontological).
narrative_ontology:cs_axiom('eb70890e-ae81-4db0-b89f-872a39bd3a41', foundational, proprietary_software_categorical_injustice).
narrative_ontology:cs_axiom_status(proprietary_software_categorical_injustice, holdable).
narrative_ontology:cs_axiom_grounding('eb70890e-ae81-4db0-b89f-872a39bd3a41', proprietary_software_categorical_injustice, deontological).
narrative_ontology:cs_reference_frame('eb70890e-ae81-4db0-b89f-872a39bd3a41', free_software_definition_framework).
narrative_ontology:cs_drift_state('eb70890e-ae81-4db0-b89f-872a39bd3a41', contemporary_cloud_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eb70890e-ae81-4db0-b89f-872a39bd3a41', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, free_software_community).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, hardware_oems).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, copyleft_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, software_freedom_ethic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the Free Software Definition and copyleft licensing framework. Authors and enforces licenses such as the GPL and AGPL that legally compel source code availability. Adjudicates disputes about what counts as free software, campaigns against proprietary restrictions, and stewards the ethical tradition that proprietary software is a social harm.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_community, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive legally protected rights to run, study, modify, and redistribute software. Depend on the free software community for maintained alternatives to proprietary tools. While they benefit from source access, their practical choice set is constrained by the dominance of proprietary ecosystems outside the free software constraint and by the community's ethical framing of proprietary use as morally compromised.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_users, beneficiary,
    powerless, biographical, constrained, global).

% Develop and distribute software under licenses that restrict source access and modification. The freedom imperative categorizes these restrictions as illegitimate, denying them legal and social permission to enforce intellectual property exclusions. They face legal compulsion under copyleft, social stigmatization, and market pressure to abandon restriction-based business models.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_vendors, payer,
    powerful, biographical, constrained, global).

% Manufacture hardware requiring driver and firmware support. Under pressure from the free software community and copyleft-adjacent norms to release source code for drivers and firmware. Bear the engineering costs and intellectual property exposure of opening previously closed hardware interfaces, with limited ability to exit without losing access to the free software ecosystem.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, hardware_oems, payer,
    powerful, biographical, constrained, global).

% Study the evolution of software licensing paradigms and the socio-technical contest between proprietary and free development models. Document the institutionalization of the freedom imperative, the enclosure of the software commons in the 1970s and 1980s, and the contemporary divergence between ethical and pragmatic open-source movements.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables large-scale collaborative software development by guaranteeing all participants can access, study, modify, and redistribute source code. Prevents enclosure of the software commons and ensures user autonomy over computing infrastructure by solving the trust and continuity problems that arise when source is hidden.
% TRANSFER_FUNCTION: Moves control over software from proprietary authors to users and the public commons. Moves the legal obligation to share source code from creators to downstream distributors. Transfers the cost of software restriction-enforcement from vendors to the compliance-monitoring community and the legal risk of non-compliance to proprietary distributors.
% ABSENT_VOICES: Proprietary software vendors and intellectual property attorneys are structurally excluded from free software governance bodies such as the FSF. End-users who prefer supported proprietary solutions without source access are not represented in license drafting. Corporate managers seeking hybrid licensing models are marginalized in movement discourse as ethically compromised.
% DISAPPEARANCE_RATIONALE: If the freedom imperative vanished overnight, proprietary licensing would expand rapidly, copyleft enforcement would collapse, collaborative development norms would weaken as enclosure became permissible, and user control over computing infrastructure would decline. The software economy would reorganize around intellectual property exclusion and SaaS enclosure.
% FOUNDING_PROBLEM: The enclosure of software source code in the 1970s and 1980s eliminated users' ability to study, modify, and share programs. Early computing relied on shared source; the rise of proprietary licensing transformed software into a restricted commodity and shifted control from users to vendors.
% FOUNDING_PROBLEM_CORROBORATION: Free software advocates attest to the enclosure problem from within the movement. Independent software historians and science-and-technology studies scholars corroborate the historical shift from academic and hobbyist sharing to proprietary business models. Proprietary vendors and intellectual property economists dispute the framing, arguing that proprietary incentives drove software investment, quality, and diversity.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the constraint actively denies proprietary vendors their preferred business model and compels source release through copyleft. Suppression (0.58) reflects the legal enforcement of copyleft licenses and the social stigmatization of proprietary restrictions, though alternatives persist in adjacent markets. Theater ratio (0.45) captures the performative dimension of moral condemnation alongside genuine coordination function. Accessibility collapse (0.70) is high because accepting the ethical framework renders proprietary alternatives cognitively illegitimate. Resistance (0.75) is high due to sustained opposition from the proprietary software industry and IP regimes. The claimed type is tangled_rope: the constraint genuinely coordinates collaborative development and protects user autonomy, but it asymmetrically extracts from proprietary vendors by categorically denying their right to restrict.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (free software community) experiences the constraint as legitimate ethical coordination that restores user rights. The victim seat (proprietary vendors) experiences the same structure as coercive extraction that destroys their business model. The beneficiary seat (users) experiences expanded autonomy but also dependency on community-maintained infrastructure. The engine computes this divergence from the structural data: identical constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   The free software community sits near the beneficiary end (low d) because they control the licensing framework and their identity is fused with the constraint's persistence. Software users sit at moderate-low d: they receive the coordination benefit without bearing enforcement costs. Proprietary vendors sit near the full-target end (high d): the constraint is designed to extract compliance from them and deny their restriction rights. Hardware OEMs sit at moderate-high d: they are not the primary target but bear compliance costs for driver openness.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the freedom imperative as pure extraction (snare) â there is a genuine coordination function in collaborative development and user autonomy. It also prevents mislabeling it as pure coordination (rope) â the categorical denial of proprietary rights creates identifiable victims who bear real costs. The mandate has not atrophied: the founding problem (software enclosure) is contested but the constraint remains actively justified by its proponents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_imperative_naturalness,
    'Is software freedom a discovered natural moral law or a constructed social norm arising from specific twentieth-century hacker cultures?',
    'Cross-cultural historical analysis of whether concepts equivalent to software freedom emerge independently across computing traditions, or are tethered to the specific institutional and cultural context of the MIT AI Lab and early GNU project.',
    'If a natural moral law, the constraint approaches mountain-like status with near-universal accessibility collapse; if a constructed norm, it remains a contested coordination mechanism whose legitimacy is culturally bounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(freedom_imperative_naturalness, conceptual, 'Whether the freedom imperative is a natural moral fact or a constructed norm').

omega_variable(
    copyleft_enforcement_efficacy,
    'How effectively does copyleft enforcement actually compel source release versus merely shifting proprietary behavior to SaaS migration or clean-room circumvention?',
    'Empirical study of GPL violation lawsuit outcomes, compliance-engineering industry growth, and industry-wide licensing shift patterns from on-premise to cloud-delivered software.',
    'If enforcement is weak, the constraint''s extractiveness is lower than claimed and its suppression is partially theatrical; if strong, the extraction from proprietary vendors is structurally real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_enforcement_efficacy, empirical, 'Actual efficacy of copyleft as an enforcement mechanism').

omega_variable(
    categorical_vs_contextual_ethics,
    'Does the freedom imperative''s categorical rejection of proprietary software foreclose all hybrid or utilitarian approaches, or can it coexist with instrumental openness in practice?',
    'Examination of whether the free software community''s licensing practice (GPL, LGPL, AGPL) admits any context-dependent proprietary use, or whether the categorical claim is absolute in implementation.',
    'If absolute and non-porous, the constraint functions as a stronger extractive mechanism against proprietary vendors; if the community permits hybrid or contextual exceptions, the constraint is less extractive than its rhetoric claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_contextual_ethics, conceptual, 'Whether the categorical ethical claim is absolute or porous in practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t9, software_source_status__freedom_imperative_reading, theater_ratio, 9, 0.25).
narrative_ontology:measurement(soft_tr_t18, software_source_status__freedom_imperative_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(soft_tr_t27, software_source_status__freedom_imperative_reading, theater_ratio, 27, 0.38).
narrative_ontology:measurement(soft_tr_t36, software_source_status__freedom_imperative_reading, theater_ratio, 36, 0.42).
narrative_ontology:measurement(soft_tr_t45, software_source_status__freedom_imperative_reading, theater_ratio, 45, 0.45).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(soft_be_t9, software_source_status__freedom_imperative_reading, base_extractiveness, 9, 0.48).
narrative_ontology:measurement(soft_be_t18, software_source_status__freedom_imperative_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(soft_be_t27, software_source_status__freedom_imperative_reading, base_extractiveness, 27, 0.57).
narrative_ontology:measurement(soft_be_t36, software_source_status__freedom_imperative_reading, base_extractiveness, 36, 0.6).
narrative_ontology:measurement(soft_be_t45, software_source_status__freedom_imperative_reading, base_extractiveness, 45, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(soft_su_t9, software_source_status__freedom_imperative_reading, suppression_requirement, 9, 0.42).
narrative_ontology:measurement(soft_su_t18, software_source_status__freedom_imperative_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(soft_su_t27, software_source_status__freedom_imperative_reading, suppression_requirement, 27, 0.52).
narrative_ontology:measurement(soft_su_t36, software_source_status__freedom_imperative_reading, suppression_requirement, 36, 0.56).
narrative_ontology:measurement(soft_su_t45, software_source_status__freedom_imperative_reading, suppression_requirement, 45, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
