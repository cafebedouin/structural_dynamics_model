% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Software Source Code as Intellectual Property (Property Rights Reading)
 *   domain: software engineering/political economy/intellectual property
 *
 * SUMMARY:
 *   This constraint instantiates the property-rights reading of the
 *   software_source_status kernel: the claim that software source code is a
 *   proprietary asset and that licensing restrictions on access and
 *   modification are legitimate exercises of ownership. The constraint
 *   operates through copyright statutes, contract law (EULAs), and technical
 *   protection measures (DRM) to enforce exclusion. It functions as a
 *   market-coordination mechanism (incentivizing production by enabling rent
 *   capture) while simultaneously extracting from users and secondary
 *   developers through monopoly pricing and rights forfeiture. The engine
 *   will compute per-seat classifications; the structural data here should
 *   produce divergent types across the vendor seat
 *   (beneficiary/agenda-setter) and the user/repair/developer seats
 *   (targets).
 *
 * KEY AGENTS:
 *   - proprietary_vendors: Primary agenda-setter and beneficiary (institutional/global/arbitrage) â defines licensing terms and captures monopoly rents
 *   - software_users: Primary target (organized/global/constrained) â pay monopoly prices and surrender modification rights
 *   - commercial_licensors: Secondary beneficiary (powerful/global/mobile) â extract fees via patent and copyright portfolios
 *   - independent_repair_providers: Target (moderate/regional/trapped) â legally prohibited from modifying firmware
 *   - interoperability_developers: Target (moderate/national/constrained) â blocked by anti-circumvention law
 *   - free_software_advocates: Excluded voice (organized/global/analytical) â marginalized in IP policy forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.68).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.72).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software Source Code as Intellectual Property (Property Rights Reading)").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "software engineering/political economy/intellectual property").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, 'f48783ff-df12-40fc-9a4d-efb41139cb90').
narrative_ontology:cs_kernel_codification('f48783ff-df12-40fc-9a4d-efb41139cb90', formalized).
narrative_ontology:cs_authority_grounding('f48783ff-df12-40fc-9a4d-efb41139cb90', lineage).
narrative_ontology:cs_interpretation_layer_present('f48783ff-df12-40fc-9a4d-efb41139cb90').
narrative_ontology:cs_reading_relation('f48783ff-df12-40fc-9a4d-efb41139cb90', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('f48783ff-df12-40fc-9a4d-efb41139cb90', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('f48783ff-df12-40fc-9a4d-efb41139cb90', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('f48783ff-df12-40fc-9a4d-efb41139cb90', foundational, software_source_as_property).
narrative_ontology:cs_axiom_status(software_source_as_property, holdable).
narrative_ontology:cs_axiom_grounding('f48783ff-df12-40fc-9a4d-efb41139cb90', software_source_as_property, deontological).
narrative_ontology:cs_axiom('f48783ff-df12-40fc-9a4d-efb41139cb90', secondary, restrictive_license_as_owner_prerogative).
narrative_ontology:cs_axiom_status(restrictive_license_as_owner_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('f48783ff-df12-40fc-9a4d-efb41139cb90', restrictive_license_as_owner_prerogative, conventional).
narrative_ontology:cs_reference_frame('f48783ff-df12-40fc-9a4d-efb41139cb90', proprietary_property_framework).
narrative_ontology:cs_drift_state('f48783ff-df12-40fc-9a4d-efb41139cb90', contemporary_open_source_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f48783ff-df12-40fc-9a4d-efb41139cb90', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, commercial_licensors).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_repair_providers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, interoperability_developers).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, intellectual_property_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, creative_labor_deserves_exclusive_reward).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and distribute proprietary software under restrictive licenses; lobby for stronger copyright and anti-circumvention laws; collect monopoly rents from controlled distribution. They define the terms of access and modification restriction and can pivot business models across jurisdictions.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, proprietary_vendors, beneficiary).

% Purchase or subscribe to software under licenses that prohibit modification, redistribution, and often reverse engineering. They bear monopoly pricing and lose autonomy over devices they own. Exit to open alternatives is possible for some categories but blocked by interoperability lock-in and market dominance in others.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_users, payer,
    organized, biographical, constrained, global).

% Acquire and enforce software patents and copyrights, extracting licensing fees without producing software. They benefit directly from the legal enforceability of source code as property and can move capital across licensing portfolios.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, commercial_licensors, beneficiary,
    powerful, biographical, mobile, global).

% Provide hardware and software repair services. Prohibited by DRM and licensing restrictions from modifying firmware or circumventing access controls to restore functionality. Legal risk from anti-circumvention laws traps them in a shrinking legitimate service market.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_repair_providers, payer,
    moderate, immediate, trapped, regional).

% Develop compatibility layers, reverse-engineer protocols, or port software to new platforms. Face legal threats and technical barriers from license terms and anti-circumvention statutes that prohibit needed research and implementation.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, interoperability_developers, payer,
    moderate, biographical, constrained, national).

% Advocate for software freedom and user rights to study, modify, and share code. Marginalized or excluded from intellectual property policy forums where proprietary frameworks are codified; their alternative model is structurally disadvantaged by the legal enforcement of source-code restriction.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, free_software_advocates, excluded,
    organized, generational, analytical, global).

% Develop and distribute non-proprietary alternatives. Disadvantaged by procurement preferences for proprietary software, patent thickets, and interoperability secrecy enforced through IP law. They would compete on equal terms if source restriction were not enforced.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, open_source_competitors, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_vendors).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Incentivize software creation by enabling creators to capture value through exclusive control over distribution and modification; organize a market for digital goods where development costs can be recouped through licensing.
% TRANSFER_FUNCTION: Moves monopoly rents and control rights from software users and secondary developers to proprietary vendors and rights-holders through licensing restrictions and legal enforcement.
% ABSENT_VOICES: Free software advocates and interoperability developers are structurally underrepresented in IP policy bodies; independent repair professionals are excluded from device-design conversations where access controls are embedded.
% DISAPPEARANCE_RATIONALE: If proprietary source-code restrictions vanished overnight, business models for a significant software sector would collapse, open-source alternatives would rapidly replace many proprietary stacks, users would gain modification and repair rights, and the legal apparatus around software IP would become obsolete. The digital economy would reorganize around unrestricted distribution.
% FOUNDING_PROBLEM: Early software creation faced a free-rider problem: digital goods are trivially copyable, making it difficult for creators to recoup development investment without exclusion mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary vendors and industry associations attest the problem is still live, citing R&D costs. Free software advocates and empirical economists from outside the beneficiary set attest that alternative models (services, support, open-source development) have proven viable and the founding problem is substantially solved for many categories; no independent consensus confirms that restriction is the only viable solution.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers monopoly rents and control rights from users to vendors, decoupled from marginal service cost. Suppression (0.72) is higher because the constraint's persistence depends on active legal enforcement (copyright, DMCA, contract law) and technical barriers (DRM) to suppress copying and modification. Theater ratio (0.25) is moderate-low: most enforcement is functional for rights-holders, though some is performative (e.g., DRM that is quickly broken). Accessibility collapse (0.60) reflects that once a user commits to a proprietary stack, alternatives collapse due to interoperability lock-in and data formats. Resistance (0.55) is significant due to the free software movement, piracy, and right-to-repair campaigns. Measurements track the maturation of enforcement infrastructure from 1970 to 2020 on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat experiences the constraint as a necessary and legitimate coordination mechanism that enables investment and market organization. The user, repair, and interoperability seats experience the same structure as enforced extraction that strips autonomy and imposes artificial scarcity. The engine should compute this divergence from the structural data: agenda-setter with arbitrage exit versus payers with constrained or trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (proprietary_vendors, commercial_licensors) receive low directionality (d near the beneficiary end), translating their structural position into subsidy and control. Victims (software_users, independent_repair_providers, interoperability_developers) receive high directionality (d near the target end), amplifying effective extraction. Free software advocates, though excluded, have analytical exit and thus do not sit at the full-target end despite their opposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â incentivizing software creation in the face of trivial copyability â may have been live in the 1970s. Today it is contested: open-source production, SaaS, and alternative funding models demonstrate that restriction is not the only viable coordination mechanism. The constraint persists beyond its original justification, suggesting potential mandatrophy. However, because the coordination function (market organization) remains real and a concentrated beneficiary class actively maintains it, the constraint is classified as tangled rope rather than piton. If the coordination function fully atrophied while enforcement persisted, it would degrade toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_status_kernel_contest,
    'Is software source code more accurately modeled as a proprietary asset, a shared public good, an instrumental development input, or a welfare-maximizing hybrid?',
    'Comparative institutional analysis across jurisdictions and development models; assessment of which reading''s predictions hold empirically.',
    'Resolving toward one reading would dissolve the current constraint into a different classification (rope for pragmatic, mountain-like norm for freedom-imperative, etc.).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_status_kernel_contest, conceptual, 'Structural ambiguity of the software source status kernel').

omega_variable(
    marginal_incentive_effect,
    'What is the marginal effect of source-code restriction on software production relative to unrestricted models?',
    'Natural experiments from industries with weak software IP enforcement; comparison of open-source and proprietary output quality and quantity.',
    'If negligible, the coordination story is cover for extraction (snare); if substantial, the tangled rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginal_incentive_effect, empirical, 'Empirical test of the coordination justification').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Does the suppression of copying and modification rely primarily on technical measures, legal statutes, or internalized norms of consumer passivity?',
    'Cross-jurisdictional comparison of piracy rates, DRM circumvention prevalence, and user autonomy movements.',
    'If suppression is mostly internalized, effective extraction exceeds the structural measure; if mostly legal/technical, suppression tracks enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(soft_tr_t10, software_source_status__property_rights_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(soft_tr_t20, software_source_status__property_rights_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(soft_tr_t30, software_source_status__property_rights_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(soft_tr_t40, software_source_status__property_rights_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(soft_tr_t50, software_source_status__property_rights_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(soft_be_t10, software_source_status__property_rights_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(soft_be_t20, software_source_status__property_rights_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(soft_be_t30, software_source_status__property_rights_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(soft_be_t40, software_source_status__property_rights_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(soft_be_t50, software_source_status__property_rights_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(soft_su_t10, software_source_status__property_rights_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(soft_su_t20, software_source_status__property_rights_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(soft_su_t30, software_source_status__property_rights_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(soft_su_t40, software_source_status__property_rights_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(soft_su_t50, software_source_status__property_rights_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(software_source_status__property_rights_reading, freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the property-rights reading of the software_source_status kernel. It decomposes the colloquial 'software IP' label into structurally distinct claims: this reading treats source restriction as a fundamental property right; siblings treat it as ethical imperative, instrumental methodology, or welfare optimization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
