% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Expansive Beta Designation Liability Shield
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint instantiates the expansive_shield_reading of the
 *   contested kernel beta_designation_doctrine. Under this reading, affixing
 *   a 'beta' label to software operates as a comprehensive waiver of
 *   liability for defects, data loss, and security failures; the waiver is
 *   permissible for indefinite duration and applies categorically across all
 *   software contexts. The sibling readingsânarrow_warning_reading and
 *   severity_carve_out_readingâoffer competing structural interpretations.
 *   Here, the beta designation is not a genuine testing disclosure but a
 *   contractual extraction mechanism that externalizes all downstream costs
 *   to users.
 *
 * KEY AGENTS:
 *   - Software vendors: primary beneficiary (powerful/mobile) â externalize liability costs while monetizing products
 *   - Commercial platforms: secondary beneficiary (institutional/arbitrage) â scale the waiver across ecosystems
 *   - Consumer end users: primary target (powerless/trapped) â bear defect costs with no practical exit or remedy
 *   - Enterprise customers: secondary target (moderate/constrained) â absorb business-critical failure costs under adhesive terms
 *   - Judiciary and legislature: agenda setter (institutional/analytical) â enforces and codifies the waiver doctrine
 *   - Consumer protection groups: excluded voice (moderate/constrained) â structurally sidelined in contract reform debates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.87).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.82).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Expansive Beta Designation Liability Shield").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, 'da0964c0-d4c8-4995-ad72-3d1f7b51dd3e').
narrative_ontology:cs_kernel_codification('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', formalized).
narrative_ontology:cs_authority_grounding('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', lineage).
narrative_ontology:cs_interpretation_layer_present('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e').
narrative_ontology:cs_reading_relation('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_reading_relation('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', beta_designation_doctrine__severity_carve_out_reading, forecloses).
narrative_ontology:cs_axiom('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', foundational, beta_label_perpetual_immunity).
narrative_ontology:cs_axiom_status(beta_label_perpetual_immunity, holdable).
narrative_ontology:cs_axiom_grounding('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', beta_label_perpetual_immunity, conventional).
narrative_ontology:cs_axiom('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', foundational, user_assumption_of_all_digital_risk).
narrative_ontology:cs_axiom_status(user_assumption_of_all_digital_risk, holdable).
narrative_ontology:cs_axiom_grounding('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', user_assumption_of_all_digital_risk, conventional).
narrative_ontology:cs_reference_frame('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', vendor_immunity_default).
narrative_ontology:cs_drift_state('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', contemporary_commercial_software_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('da0964c0-d4c8-4995-ad72-3d1f7b51dd3e', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_vendors).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, commercial_platforms).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, consumer_end_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, enterprise_customers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__expansive_shield_reading, user_assumes_all_risk_doctrine).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__expansive_shield_reading, unrestricted_beta_deployability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy software under perpetual beta designations to avoid liability for defects, crashes, and data loss while collecting revenue from fully commercialized products. Exit involves forum shopping or corporate restructuring to maintain shield advantages.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Operate marketplaces and SaaS ecosystems where the beta designation is standard contractual boilerplate, insulating the platform and its vendors from warranty and tort claims across all software categories.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, commercial_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Rely on software for daily tasks, personal data management, and communication. Accept terms of service that permanently label products as beta, surrendering recourse for defects, breaches, or harms with no practical alternative services offering liability protection.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_end_users, payer,
    powerless, immediate, trapped, national).

% License business-critical software that carries beta disclaimers indefinitely. Face vendor lock-in and contractual adhesion that prevents negotiating liability terms, while bearing costs of downtime, data corruption, and security failures.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, enterprise_customers, payer,
    moderate, biographical, constrained, national).

% Interpret and codify the legal doctrine that beta labels function as comprehensive liability waivers. Enforce forum-selection and arbitration clauses that prevent users from challenging the designation's validity.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, judiciary_and_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for restoration of product liability in digital goods. Are systematically sidelined in contract-law reform debates by industry lobbying and judicial deference to freedom-of-contract frameworks.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_protection_groups, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, software_vendors).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Purports to solve the coordination problem of encouraging innovation by shielding early-stage software developers from crushing liability during genuine testing phases, allowing rapid iteration and public feedback.
% TRANSFER_FUNCTION: Moves all liability and defect costs from software vendors and platforms to end users and enterprise customers, regardless of actual product maturity or revenue status.
% ABSENT_VOICES: Consumer protection advocates and injured users are largely excluded from the doctrinal conversation, which occurs between vendor lobbies, contract drafters, and courts within a freedom-of-contract frame.
% DISAPPEARANCE_RATIONALE: If the expansive shield vanished, users would gain standing to sue for defects under standard product liability and warranty law, software vendors would face liability exposure forcing investment in quality and insurance, and contract drafting practices would shift away from perpetual beta boilerplate.
% FOUNDING_PROBLEM: Genuine uncertainty in early software development about bugs and usability flaws, where exposing untested products to full liability might chill innovation and prevent crowdsourced feedback.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection scholars and plaintiff-side tort attorneys attest the founding problem is solved for any commercialized product; industry lobbyists assert it remains live. Independent legal historians note the doctrine's expansion beyond its original testing-phase justification.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.87, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.87) because the constraint transfers virtually all defect liability from vendors to users, regardless of product maturity or revenue status. Suppression is high (0.82) because the legal doctrine actively suppresses tort, warranty, and consumer-protection remedies that would otherwise provide exit. Theater ratio is high (0.72) because the 'beta' label performs a testing narrative while the underlying products are fully commercialized, widely distributed, and revenue-generating. Accessibility collapse is high (0.80) because users cannot realistically access software that does not carry beta terms. Resistance is moderate (0.55) because consumer advocates and some jurisdictions push back, but they are structurally excluded from the doctrinal conversation.
 *
 * PERSPECTIVAL GAP:
 *   Software vendors experience the beta designation as a necessary legal shield enabling innovation and risk-taking; consumer end users experience the same label as a contractual trap that externalizes all downside risk. Enterprise customers sit between, with enough resources to negotiate minor contractual terms but not to escape the doctrine's sweep. The engine computes this divergence from the structural data: vendors are beneficiaries with mobile exit options, while users are payers with trapped or constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors and commercial platforms are structural beneficiaries: the constraint subsidizes them by eliminating liability exposure, pushing their directionality toward the beneficiary end. Consumer end users and enterprise customers are structural targets: they absorb the costs the waiver removes, pushing their directionality toward the full-target end. The judiciary sits near symmetric because it enforces but does not collect the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprotecting genuine software testing phases from liabilityâhas been dead for decades. The constraint persists because it now functions as a rent-extraction mechanism for vendors, not because it coordinates any live testing need. Recognizing this prevents misclassifying the arrangement as a scaffold (transitional support) or rope (genuine coordination). It is a snare because the coordination story is cover for pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_boundary_ambiguity,
    'Do courts actually enforce indefinite beta waivers without any temporal limitation, or do they retain a hidden de facto testing-phase boundary?',
    'Longitudinal case-outcome analysis tracking judicial rejection of beta waivers on duration grounds across federal and state dockets.',
    'If duration limits persist informally, the extraction ceiling is lower than authored and the constraint may shift toward tangled_rope; if indefinite enforcement is consistent, the expansive reading is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_boundary_ambiguity, empirical, 'Whether the doctrine retains an implicit temporal boundary despite the expansive reading').

omega_variable(
    severity_exclusion_ambiguity,
    'Do courts apply the expansive shield to life-safety, financial, or critical infrastructure software, or do they create implicit severity carve-outs even under the expansive reading?',
    'Case law analysis in medical device, automotive, and fintech software liability contexts.',
    'If severity exclusions exist informally, the victim set and effective extraction are narrower than authored; if the shield is applied universally, extraction is as high as modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_exclusion_ambiguity, empirical, 'Whether critical-system exclusions operate de facto under the expansive reading').

omega_variable(
    doctrinal_reading_instability,
    'Does the expansive shield reading represent the actual settled doctrine, or is it a maximalist vendor-framing that courts apply selectively?',
    'Comparative jurisdictional analysis tracking adoption rates of expansive versus narrow precedent, combined with appellate reversal rates of broad beta waivers.',
    'If the doctrine is less settled than claimed, extraction and suppression are lower in practice; if expansive precedent is consolidating, the kernel effectively resolves toward the snare classification instantiated here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_reading_instability, conceptual, 'Uncertainty about whether this reading is settled law or a contested vendor-framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 16, 0.62).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.68).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 24, 0.72).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 24, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 24, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% The beta_designation_doctrine kernel decomposes into three structurally distinct readings: expansive_shield (snare, high extraction), narrow_warning (rope/scaffold, limited coordination), and severity_carve_out (mountain-like boundary on critical systems). Each reading carries a distinct epsilon and victim/beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
