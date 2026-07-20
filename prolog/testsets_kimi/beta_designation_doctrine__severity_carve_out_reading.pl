% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__severity_carve_out_reading, []).

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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Severity Carve-Out for Critical Systems
 *   domain: legal/technology/consumer_protection
 *
 * SUMMARY:
 *   This constraint instantiates the severity_carve_out_reading of the
 *   contested beta_designation_doctrine kernel. It holds that beta
 *   designation is categorically unavailable as a liability-limitation
 *   mechanism for life-safety, financial, and other critical systems,
 *   regardless of testing status or disclosure completeness. The doctrine
 *   represents a judicial and regulatory assertion that physical harm
 *   severity and safety requirements override contractual allocation of risk
 *   in high-stakes domains.
 *
 * KEY AGENTS:
 *   - critical_system_public: Primary beneficiary (powerless/trapped) â gains protection from unilateral beta disclaimers
 *   - high_stakes_vendors: Primary target (powerful/constrained) â bears full liability exposure without beta shield
 *   - liability_judiciary: Agenda setter (institutional/analytical) â enforces safety-priority interpretation
 *   - commercial_insurers: Analytical observer (organized/analytical) â monitors risk pool effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.72).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.65).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Severity Carve-Out for Critical Systems").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "legal/technology/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '858f4b90-cbce-41ab-b524-b93866581dd4').
narrative_ontology:cs_kernel_codification('858f4b90-cbce-41ab-b524-b93866581dd4', formalized).
narrative_ontology:cs_authority_grounding('858f4b90-cbce-41ab-b524-b93866581dd4', lineage).
narrative_ontology:cs_interpretation_layer_present('858f4b90-cbce-41ab-b524-b93866581dd4').
narrative_ontology:cs_reading_relation('858f4b90-cbce-41ab-b524-b93866581dd4', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('858f4b90-cbce-41ab-b524-b93866581dd4', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_axiom('858f4b90-cbce-41ab-b524-b93866581dd4', foundational, harm_severity_overrides_contract).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contract, holdable).
narrative_ontology:cs_axiom_grounding('858f4b90-cbce-41ab-b524-b93866581dd4', harm_severity_overrides_contract, empirically_contingent).
narrative_ontology:cs_axiom('858f4b90-cbce-41ab-b524-b93866581dd4', foundational, beta_unavailability_in_critical_domains).
narrative_ontology:cs_axiom_status(beta_unavailability_in_critical_domains, holdable).
narrative_ontology:cs_axiom_grounding('858f4b90-cbce-41ab-b524-b93866581dd4', beta_unavailability_in_critical_domains, conventional).
narrative_ontology:cs_reference_frame('858f4b90-cbce-41ab-b524-b93866581dd4', safety_priority_liability_framework).
narrative_ontology:cs_drift_state('858f4b90-cbce-41ab-b524-b93866581dd4', contemporary_software_liability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('858f4b90-cbce-41ab-b524-b93866581dd4', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_system_public).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, high_stakes_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and distribute software for life-safety, financial, and critical infrastructure domains; cannot invoke beta designation to limit liability regardless of product maturity or disclosure quality; bear full products liability exposure without the contractual shield available in non-critical domains.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, high_stakes_vendors, payer,
    powerful, biographical, constrained, national).

% Relies on software embedded in medical devices, financial systems, transportation control, and other critical infrastructure; protected from vendors unilaterally disclaiming liability through beta labels regardless of testing status or disclosure completeness.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_public, beneficiary,
    powerless, immediate, trapped, national).

% Interprets products liability and consumer protection precedent to enforce that harm severity and physical safety requirements override contractual beta disclaimers in critical domains; establishes precedent removing beta availability as a liability allocation mechanism for high-stakes software.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, liability_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Underwrite products liability and errors-and-omissions coverage for software vendors; face altered risk pools and pricing models when beta shields are categorically removed in critical domains, observing the constraint's effects without setting its terms.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, commercial_insurers, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents software vendors in life-safety, financial, and critical domains from using beta labels to contractually disclaim liability for physical and economic harm, coordinating risk allocation so that safety severity overrides contractual bargaining power.
% TRANSFER_FUNCTION: Moves liability exposure from consumers and the general public to software vendors in critical domains, and transfers legal certainty from vendor-controlled contractual terms to judicially enforced safety-priority standards.
% ABSENT_VOICES: Software vendors from non-critical domains concerned the carve-out will expand; international standards bodies seeking harmonized software liability regimes; beta-testing communities and early adopters who argue for access to unfinished but innovative critical tools.
% DISAPPEARANCE_RATIONALE: Critical system vendors would immediately deploy beta disclaimers to shed liability exposure; consumer recourse for defective medical, financial, and infrastructure software would fragment; the liability landscape would reorganize around contractual rather than safety-based allocation.
% FOUNDING_PROBLEM: Software vendors in critical domains used perpetual, disguised, or aggressively disclosed beta labels to avoid products liability for defects in life-safety and financial systems, producing a persistent gap between contractual disclaimers and physical harm.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection agencies and medical device safety regulators attest that vendors continue to attempt beta-label disclaimers for critical systems; plaintiffs' bar and independent tort scholars corroborate that contractual disclaimers do not prevent physical harm in high-stakes domains.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the carve-out categorically removes a liability shield that vendors in other domains retain, concentrating exposure on critical-domain software producers. Suppression is substantial (0.65) because the constraint suppresses the contractual alternative (beta disclaimer) through active judicial enforcement. Theater ratio is low-moderate (0.25): most enforcement activity is functional safety protection rather than performative ritual, though some litigation posture exists. Accessibility collapse is high (0.80) because once the doctrine is understood, beta disclaimers in critical domains offer no legal refuge. Resistance is high (0.70) due to sustained vendor lobbying and doctrinal opposition. The measurement series share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat experiences the constraint as asymmetric extraction â a liability exposure imposed without a corresponding coordination benefit to them. The public seat experiences the same structure as genuine coordination â a safety guarantee that overrides vendor bargaining power. The judicial seat experiences it as norm enforcement. The engine computes this divergence from the structural data rather than the narrative claim.
 *
 * DIRECTIONALITY LOGIC:
 *   high_stakes_vendors are declared victims with constrained exit (cannot easily abandon critical domains if that is their core business), placing their directionality near the full-target end. critical_system_public are declared beneficiaries with trapped exit (patients and infrastructure operators cannot opt out of the software they depend on), placing their directionality near the full-beneficiary end. liability_judiciary sits at the agenda_setter position with analytical exit; it does not collect extraction and its directionality is neutral-to-beneficiary. commercial_insurers observe without bearing or receiving the core extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â vendors using beta labels to escape liability for critical system defects â remains live, as corroborated by regulators and plaintiffs outside the benefiting parties. Were the problem solved (e.g., universal vendor insurance or formal safety certification making beta disclaimers irrelevant), persistence of the carve-out would indicate mandatrophy and a slide toward piton. For now, the constraint retains genuine coordination function alongside its extraction, supporting tangled_rope classification and guarding against mislabeling it as pure snare or degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beta_doctrine_reading_contest,
    'Is the beta designation doctrine properly read as a comprehensive liability shield (expansive), a time-bounded testing disclosure (narrow), or a categorically unavailable mechanism in critical domains (severity carve-out)?',
    'Corpus-level analysis of the three sibling constraint stories; tracking judicial precedent and legislative codification of software liability standards across jurisdictions.',
    'Adoption of the expansive shield reading would eliminate victims and convert the constraint into a rope for vendors; adoption of the narrow warning reading would reduce extractiveness and limit the victim set to post-testing-phase harms; the severity carve-out reading maintains high extractiveness concentrated on critical-domain vendors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beta_doctrine_reading_contest, conceptual, 'The beta designation doctrine kernel admits multiple structurally distinct readings.').

omega_variable(
    critical_domain_boundary,
    'What constitutes a ''critical system'' such that beta designation is unavailable â is the boundary defined by potential harm severity, regulatory classification, or sectoral labels?',
    'Comparative analysis of judicial and regulatory classifications across jurisdictions; tracking which software domains are treated as critical under the doctrine.',
    'If the boundary expands, the constraint becomes more extractive for more vendors; if it contracts, the carve-out shrinks toward the narrow_warning reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_domain_boundary, conceptual, 'Uncertainty in the boundary definition of critical systems.').

omega_variable(
    safety_investment_invariance,
    'Do contractual beta disclaimers in critical domains actually reduce vendor safety investment, or do vendors maintain high standards regardless of liability exposure?',
    'Empirical studies comparing safety outcomes and security investment levels in jurisdictions with and without beta shield carve-outs.',
    'If disclaimers reduce safety investment, the coordination function is genuine; if investment is invariant, the carve-out may be pure extraction without protective benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_investment_invariance, empirical, 'Whether the constraint''s coordination function produces measurable safety benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_sev_carve_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(beta_sev_carve_tr_t5, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(beta_sev_carve_tr_t10, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(beta_sev_carve_tr_t15, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(beta_sev_carve_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(beta_sev_carve_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(beta_sev_carve_be_t5, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(beta_sev_carve_be_t10, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(beta_sev_carve_be_t15, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(beta_sev_carve_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(beta_sev_carve_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(beta_sev_carve_su_t5, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(beta_sev_carve_su_t10, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(beta_sev_carve_su_t15, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(beta_sev_carve_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, narrow_warning_reading).

% DUAL FORMULATION NOTE:
% This constraint is the severity_carve_out_reading of the beta_designation_doctrine kernel. The kernel decomposes into three structurally distinct claims: an expansive shield reading (pure vendor protection with negligible extraction), a narrow warning reading (time-bounded testing disclosure with moderate extraction), and this severity carve-out reading (categorical unavailability in critical domains with high extraction concentrated on critical-domain vendors). Each reading has a different epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
