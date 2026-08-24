% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Beta Designation as Comprehensive Liability Waiver (Expansive Shield Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   The expansive shield reading of the beta designation doctrine holds that
 *   labeling software 'beta' constitutes a comprehensive, indefinite,
 *   context-universal liability waiver. This reading emerged from early
 *   software contract law (ProCD v. Zeidenberg, 1996; Specht v. Netscape,
 *   2002) and has been extended by courts to cover production systems labeled
 *   'beta' for decades (Gmail carried beta for 5 years; countless SaaS
 *   products remain permanently beta). The structural delta is high
 *   extraction: developers externalize all defect costs to users; users enter
 *   the victim set with no meaningful exit; no temporal or severity
 *   boundaries limit the waiver. The constraint is enforced through
 *   click-wrap agreements, arbitration clauses, and judicial precedent that
 *   treats software licenses as freely negotiated contracts.
 *
 * KEY AGENTS:
 *   - software_developers: Primary agenda_setter/beneficiary (organized/mobile) — drafts and enforces beta terms, externalizes all defect costs
 *   - platform_operators: Primary beneficiary (institutional/arbitrage) — requires beta labeling, benefits from ecosystem velocity and reduced platform liability
 *   - venture_backed_startups: Secondary beneficiary (moderate/constrained) — relies on perpetual beta to meet investor velocity demands
 *   - individual_software_users: Primary payer (powerless/trapped) — bears all defect costs with no meaningful consent or exit
 *   - enterprise_customers: Secondary payer (moderate/constrained) — absorbs vendor defect costs through redundancy and insurance
 *   - critical_infrastructure_operators: Tertiary payer (organized/constrained) — bears physical-world consequences of vendor defects in life-safety systems
 *   - healthcare_institutions: Quaternary payer (organized/constrained) — bears patient harm costs from beta-designated medical software
 *   - financial_institutions: Quinary payer (organized/constrained) — bears systemic risk from beta-designated financial infrastructure
 *   - courts_regulators: Observer (institutional/analytical) — enforces doctrine, sees full structure but lacks statutory override
 *   - consumer_advocates: Excluded (organized/trapped) — argues for limits but excluded from doctrinal interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.88).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.82).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive Liability Waiver (Expansive Shield Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, 'ad9d5511-a36b-4a68-8f88-97a38b6500d4').
narrative_ontology:cs_kernel_codification('ad9d5511-a36b-4a68-8f88-97a38b6500d4', fixed_text).
narrative_ontology:cs_authority_grounding('ad9d5511-a36b-4a68-8f88-97a38b6500d4', lineage).
narrative_ontology:cs_interpretation_layer_present('ad9d5511-a36b-4a68-8f88-97a38b6500d4').
narrative_ontology:cs_reading_relation('ad9d5511-a36b-4a68-8f88-97a38b6500d4', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('ad9d5511-a36b-4a68-8f88-97a38b6500d4', beta_designation_doctrine__severity_carve_out_reading, forecloses).
narrative_ontology:cs_axiom('ad9d5511-a36b-4a68-8f88-97a38b6500d4', foundational, beta_designation_waives_all_liability).
narrative_ontology:cs_axiom_status(beta_designation_waives_all_liability, holdable).
narrative_ontology:cs_axiom_grounding('ad9d5511-a36b-4a68-8f88-97a38b6500d4', beta_designation_waives_all_liability, conventional).
narrative_ontology:cs_axiom('ad9d5511-a36b-4a68-8f88-97a38b6500d4', foundational, beta_duration_indefinite_permissible).
narrative_ontology:cs_axiom_status(beta_duration_indefinite_permissible, holdable).
narrative_ontology:cs_axiom_grounding('ad9d5511-a36b-4a68-8f88-97a38b6500d4', beta_duration_indefinite_permissible, conventional).
narrative_ontology:cs_axiom('ad9d5511-a36b-4a68-8f88-97a38b6500d4', foundational, beta_applies_to_all_contexts_including_critical).
narrative_ontology:cs_axiom_status(beta_applies_to_all_contexts_including_critical, holdable).
narrative_ontology:cs_axiom_grounding('ad9d5511-a36b-4a68-8f88-97a38b6500d4', beta_applies_to_all_contexts_including_critical, conventional).
narrative_ontology:cs_reference_frame('ad9d5511-a36b-4a68-8f88-97a38b6500d4', early_software_liability_shield).
narrative_ontology:cs_drift_state('ad9d5511-a36b-4a68-8f88-97a38b6500d4', critical_infrastructure_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ad9d5511-a36b-4a68-8f88-97a38b6500d4', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, platform_operators).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, venture_backed_startups).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, individual_software_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, enterprise_customers).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, critical_infrastructure_operators).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, healthcare_institutions).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, financial_institutions).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__expansive_shield_reading, beta_testing_immunity_doctrine).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__expansive_shield_reading, contractual_liability_waiver_enforceability).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__expansive_shield_reading, software_disclaimer_permissibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce beta designations through click-wrap agreements, terms of service, and license terms. They control the labeling decision and the legal language. Benefit by externalizing all defect liability costs — security flaws, data loss, downtime, physical harm — to users. Can move between platforms and jurisdictions to find favorable enforcement regimes.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_developers, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, software_developers, beneficiary).

% App stores, cloud marketplaces, and distribution platforms require or incentivize beta labeling as a condition of distribution. They benefit from reduced liability exposure for the platform itself and from the ecosystem velocity that unlimited beta shields enable. Their market power lets them set the terms developers must accept.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Rely on perpetual beta to ship fast and iterate under capital pressure. The expansive shield lets them treat production systems as permanent beta, deferring all quality investment. Their exit is constrained by investor expectations and runway — they cannot afford the insurance or testing rigor a real liability regime would demand.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, venture_backed_startups, beneficiary,
    moderate, immediate, constrained, global).

% Click through beta disclaimers to access essential services — banking, communication, health portals, government services. Have no meaningful choice: refusing beta terms means exclusion from digital life. Bear all costs of defects — data breaches, financial loss, privacy violations, time spent on workarounds — with no recourse. Class actions are blocked by arbitration clauses that ride alongside beta designations.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, individual_software_users, payer,
    powerless, biographical, trapped, global).

% Negotiate enterprise licenses but still face beta carve-outs in SLAs and addenda. Cannot fully escape the doctrine because critical dependencies (cloud infra, identity providers, dev tools) are beta-designated upstream. Absorb costs of vendor defects through redundant engineering, insurance, and incident response. Some leverage to negotiate but structurally trapped by ecosystem dependencies.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, enterprise_customers, payer,
    moderate, biographical, constrained, global).

% Power grids, water systems, transportation, telecom — run on software that vendors label beta to avoid liability. Regulators require reliability but vendors disclaim it. The operators bear physical-world consequences (outages, safety incidents) while vendors bear none. Exit is constrained by decades-long procurement cycles and lack of non-beta alternatives for specialized industrial software.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, critical_infrastructure_operators, payer,
    organized, generational, constrained, national).

% Hospital systems, medical devices, EHR platforms — all carry beta disclaimers despite life-safety context. FDA regulatory framework does not preempt contractual beta waivers. Patients harmed by software defects in medical contexts have no vendor recourse. Institutions absorb liability through malpractice insurance and internal safety engineering.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, healthcare_institutions, payer,
    organized, generational, constrained, national).

% Trading platforms, settlement systems, core banking — run on beta-designated software. Regulatory capital requirements treat software risk as operational risk borne by the institution, not the vendor. Systemic risk from vendor defects is externalized to the financial system. Exit constrained by regulatory approval processes for vendor changes.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, financial_institutions, payer,
    organized, generational, constrained, global).

% Courts enforce beta waivers under contract law precedent (ProCD, Brower, Specht). Regulators (FTC, CFPB, sectoral) issue guidance but lack statutory authority to override contractual liability waivers. Some state AGs challenge unconscionable terms but precedent favors enforceability. The analytical seat sees the full structure: a doctrine that has migrated from nascent-industry protection to mature-industry extraction.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, courts_regulators, observer,
    institutional, generational, analytical, national).

% Argue for liability floors, beta duration limits, and critical-system carve-outs. Excluded from the doctrinal conversation because contract law treats software licenses as negotiated agreements between equals. Their legislative proposals (software liability bills) stall against industry lobbying. They would object to every element of the expansive reading but are not in the room when courts interpret 'beta'.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_advocates, excluded,
    organized, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: enabled rapid iteration in nascent software industry by limiting developer liability during genuine pre-release testing phases, allowing real-user feedback without existential lawsuit risk.
% TRANSFER_FUNCTION: Transfers all defect risk — security vulnerabilities, data corruption, availability failures, physical harm — from developers and platforms to users, indefinitely, without severity caps, and across all deployment contexts including life-safety and financial systems.
% ABSENT_VOICES: Users of critical systems (medical devices, financial infrastructure, industrial control) who cannot meaningfully consent to unlimited liability waivers; future users who inherit beta-designated systems that never exit beta; regulators with safety mandates but no liability authority; insurance markets that cannot price vendor risk because it is contractually extinguished.
% DISAPPEARANCE_RATIONALE: If the expansive shield vanished overnight: vendors would need liability insurance and would price it into software; beta periods would become genuine time-bounded testing phases with defined exit criteria; critical-system software would require higher assurance levels; a software liability insurance market would emerge; development velocity would slow but defect costs would internalize to the party best positioned to prevent them.
% FOUNDING_PROBLEM: Early software industry (1970s-1990s) needed space to iterate rapidly without existential liability risk killing innovation; software was experimental, low-stakes, and developers had minimal capital.
% FOUNDING_PROBLEM_CORROBORATION: Software historians (Ceruzzi, Campbell-Kelly) and early industry participants (Gates, Andreessen retrospective interviews) attest the liability shield was for a nascent industry with experimental products. Consumer protection advocates (EPIC, Public Citizen), critical infrastructure operators (NERC, financial regulators), and insurance industry analyses (Marsh, Aon cyber reports) attest the founding problem is dead — software is now critical infrastructure — but the shield persists and has expanded.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is very high (0.88) because the waiver is comprehensive — all defect types, all severities, all contexts, indefinite duration. The coordination function (genuine beta testing) has been almost entirely displaced by extraction; theater_ratio (0.38) reflects that some actual beta testing still occurs but the doctrine's persistence is driven by the shield value. Suppression (0.82) is high because alternatives are structurally collapsed: users cannot access essential services without accepting beta terms; enterprise and critical infrastructure operators face ecosystem lock-in; arbitration clauses block collective redress. Accessibility_collapse (0.79) is high because once a user encounters a beta-designated essential service, alternatives (self-hosting, competing non-beta products, regulatory protection) are practically unavailable. Resistance (0.45) is moderate — some state AG actions, legislative proposals, and academic critique exist but have not shifted the doctrinal baseline.
 *
 * PERSPECTIVAL GAP:
 *   From the developer/platform seat, the arrangement looks like a rope: it coordinates rapid iteration and ecosystem velocity, and they genuinely built the software. From the user seats (especially critical infrastructure and healthcare), it is a snare: the coordination story is cover for total liability externalization, alternatives are suppressed, and exit is impossible. The engine computes this per-seat divergence from the structural data — the claimed_type (snare) reflects the victim-seat reality, while the theater_ratio and coordination_function capture the residual coordination claim that persists in the beneficiary narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and platforms are structural beneficiaries (d near 0.0): they collect the value of externalized liability, control the labeling decision, and have mobile/arbitrage exit. Venture-backed startups are partial beneficiaries (d ~0.2): they benefit but are constrained by capital dependencies. All user categories are structural targets (d near 1.0): they bear the full cost of defects with trapped or constrained exit. Courts/regulators sit at analytical (d=0.5): they enforce the structure but do not collect from it. Consumer advocates are excluded (no directionality — they are not in the constraint's operational frame). The derivation chain: beneficiary/victim declarations + power + exit_options → d values. No overrides needed — the structural data captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (nascent industry protection) is dead — software is now the substrate of critical infrastructure. The arrangement persists as a snare because the cost to fix (vendor liability internalization) is prohibitive for the industry, while the cost to maintain (lobbying, precedent defense) is cheap for beneficiaries. No concentrated beneficiary captures enough to maintain it alone — it persists by institutional inertia and distributed industry lobbying. This is mandatrophy: the mandate (protect nascent industry) has atrophied but the constraint (comprehensive liability waiver) persists and has expanded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the expansive shield reading foreclose the sibling readings within a single legal framework, or do they coexist as competing interpretations applied by different courts/jurisdictions?',
    'Survey of appellate decisions: if any jurisdiction applies narrow_warning or severity_carve_out while another applies expansive_shield to materially similar facts, the readings coexist across frameworks. If a single court must choose one and rejecting the others, they foreclose.',
    'If forecloses: this reading''s axioms are structurally incompatible with siblings — adopting one logically excludes the others. If coexists_with: the kernel is genuinely under-specified (distributed codification) and the engine should model cross-reading coupling. If influences: this reading''s dominance creates downstream pressure (precedent, industry practice) that makes sibling readings harder to sustain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship between this kernel reading and its siblings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression structural (arbitration clauses, click-wrap ubiquity, ecosystem lock-in) or internalized (users believe they have no rights, developers believe beta is legitimate shield)?',
    'Post-reform suppression trajectory: if a jurisdiction enacts a software liability floor with beta carve-outs, measure whether user behavior (seeking redress, switching vendors) changes immediately (structural) or only after cultural shift (internalized).',
    'If internalized, effective suppression is higher than structural measure — users carry the suppression with them even after legal barriers are removed. This would increase the constraint''s extractive stability beyond what structural metrics capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in beta waiver enforcement').

omega_variable(
    coordination_extraction_separability,
    'Is genuine beta testing coordination separable from the expansive liability shield, or has the coordination function been entirely subsumed by extraction?',
    'Natural experiment: jurisdictions or platforms that impose beta duration limits or severity carve-outs — measure whether genuine beta testing (user feedback, bug discovery, iteration velocity) continues at comparable levels.',
    'If separable, the expansive shield is pure extraction riding on a separable coordination function (tangled_rope structure). If inseparable, some measured extraction is the price of coordination itself — but the current doctrine goes far beyond any plausible coordination need.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether beta testing coordination requires comprehensive indefinite liability waiver').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_shield_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(beta_shield_tr_t8, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(beta_shield_tr_t16, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(beta_shield_tr_t24, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(beta_shield_tr_t32, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(beta_shield_tr_t40, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(beta_shield_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(beta_shield_be_t8, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(beta_shield_be_t16, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(beta_shield_be_t24, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(beta_shield_be_t32, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 32, 0.82).
narrative_ontology:measurement(beta_shield_be_t40, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 40, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(beta_shield_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(beta_shield_su_t8, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(beta_shield_su_t16, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(beta_shield_su_t24, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(beta_shield_su_t32, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(beta_shield_su_t40, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__expansive_shield_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, software_liability_insurance_market).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, critical_infrastructure_software_assurance).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, consumer_arbitration_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is the expansive_shield_reading of the beta_designation_doctrine kernel. It forecloses the narrow_warning_reading (time-bounded testing disclosure) and severity_carve_out_reading (critical-system exclusion) because its foundational axioms — total liability waiver, indefinite duration, universal applicability — are logically incompatible with temporal bounds, severity limits, or contextual carve-outs within any single legal framework. The narrow and carve-out readings survive as live positions in other jurisdictions and legislative proposals, creating a distributed kernel (cs_structure.kernel_codification: distributed at the kernel level, fixed_text at this reading's level).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__expansive_shield_reading, organized, 0.15).
constraint_indexing:directionality_override(beta_designation_doctrine__expansive_shield_reading, moderate, 0.85).
constraint_indexing:directionality_override(beta_designation_doctrine__expansive_shield_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
