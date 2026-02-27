% ============================================================================
% CONSTRAINT STORY: meta_nda
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_nda, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: meta_nda
 *   human_readable: Meta's Non-Disclosure Agreements for Undercover Testers
 *   domain: economic/platform_governance/labor
 *
 * SUMMARY:
 *   Meta's Non-Disclosure Agreements (NDAs) with undercover testers in
 *   simulated environments ('Simulated Terrorist Attack,' 'Simulated School
 *   Shooting') represent a structural extraction mechanism that prevents
 *   disclosure of platform vulnerabilities, manipulative design patterns, and
 *   testing methodologies. The constraint operates through legal
 *   enforceability and employment power asymmetry to silence
 *   testifier-victims while preserving Meta's informational monopoly.
 *   Undercover testers are recruited to discover how bad actors exploit
 *   Meta's systems, how content moderation fails, and how users can be
 *   manipulated — but the NDA prevents them from sharing findings with
 *   regulators, researchers, journalists, or the public. This creates a dual
 *   extraction: Meta captures the benefit of systematic vulnerability
 *   discovery while externalizing the cost of hidden platform flaws onto the
 *   public (which cannot exit the platform and cannot access information
 *   needed to assess its risks). The constraint exhibits classical snare
 *   properties: high suppression (legal liability, employment dependence),
 *   asymmetric extraction (Meta benefits, testers and public pay), and
 *   minimal coordination benefit (the testing function could operate without
 *   information lockdown).
 *
 * KEY AGENTS:
 *   - Undercover Testers: Primary victims (powerless/trapped) — recruited specifically for vulnerability discovery but legally prohibited from disclosing findings; trapped by employment and litigation risk
 *   - Meta Internal Testing Operations: Primary beneficiary (institutional/arbitrage) — captures vulnerability information while preventing competitors and regulators from accessing it; uses tester labor while externalizing risk
 *   - Public Epistemic Commons: Secondary victim (powerless/trapped) — cannot access information about platform design flaws or manipulative tactics; trapped in informational asymmetry
 *   - Regulatory Bodies / Lawmakers: Powerful observer (powerful/mobile) — can theoretically mandate disclosure or subpoena, but rely on public complaints and external research to identify problems because internal testing remains hidden
 *   - Researcher / Journalist Community: Organized observer (organized/constrained) — can conduct independent research but face legal barriers to accessing Meta's internal testing data; constrained by IP protections and legal threats
 *   - Legal Department: Institutional enforcer (institutional/arbitrage) — maintains NDA enforcement apparatus; benefits from institutional inertia and threat of litigation even if actual enforcement is rare
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_nda, 0.62).
domain_priors:suppression_score(meta_nda, 0.68).
domain_priors:theater_ratio(meta_nda, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_nda, extractiveness, 0.62).
narrative_ontology:constraint_metric(meta_nda, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(meta_nda, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_nda, snare).
narrative_ontology:human_readable(meta_nda, "Meta's Non-Disclosure Agreements for Undercover Testers").
narrative_ontology:topic_domain(meta_nda, "economic/platform_governance/labor").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_nda, meta_internal_testing_operations).
narrative_ontology:constraint_victim(meta_nda, undercover_testers).
narrative_ontology:constraint_victim(meta_nda, public_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERCOVER TESTER (SNARE) — Trapped by employment contract and legal liability. Cannot disclose platform vulnerabilities, manipulative design patterns, or testing methodologies without legal exposure. Exit requires abandoning income and facing potential litigation. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.61.
constraint_indexing:constraint_classification(meta_nda, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC EPISTEMIC COMMONS (SNARE) — Cannot access information about platform design flaws, content moderation failures, or vulnerability in manipulative tactics. Trapped in informational asymmetry. No exit mechanism exists; public bears cost of hidden platform risks without knowledge or recourse. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.67.
constraint_indexing:constraint_classification(meta_nda, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: META INTERNAL TESTING OPERATIONS (ROPE) — Experiences the NDA as a coordination mechanism: ensures controlled testing environment, prevents competitive leakage, enables systematic vulnerability discovery. Benefits from testers' compliance and information asymmetry. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(meta_nda, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY BODIES / LAWMAKERS (TANGLED ROPE) — Powerful actors with exit options (can subpoena, conduct independent audits, mandate disclosure). See mixed function: NDA enables structured testing (coordination benefit) but also enables information hiding from regulators (extraction). d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.33.
constraint_indexing:constraint_classification(meta_nda, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RESEARCHER / JOURNALIST COMMUNITY (TANGLED ROPE) — Organized but constrained by legal barriers to information access. The NDA enables structured testing (which researchers could theoretically access through collaboration) but prevents independent verification and disclosure. d≈0.68, f(d)≈0.98, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(meta_nda, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGAL DEPARTMENT ENFORCEMENT THEATER (PITON) — Maintains performative enforcement: NDA compliance is monitored, but the primary function (preventing collective disclosure) is theatrical — an individual tester has minimal leverage to expose Meta even without the NDA. The constraint persists through institutional inertia and legal ritualism rather than genuine enforcement capacity. theater_ratio=0.55 reflects modest theatrical component; many enforcement activities are substantive (legal letters, litigation threats), but the constraint's core extraction function relies on perceived rather than demonstrated legal risk.
constraint_indexing:constraint_classification(meta_nda, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_nda_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_nda, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_nda, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_nda, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meta_nda, TR),
    TR >= 0.70.

:- end_tests(meta_nda_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. Meta extracts substantial value from the NDA: monopoly control over vulnerability information, reduced reputational exposure (hidden flaws prevent public pressure), competitive advantage (vulnerability data unavailable to competitors), and regulatory opacity (hidden testing prevents informed policy response). The value increases over time as Meta accumulates vulnerability data and the public's information deficit deepens. Suppression (0.68): High. Testers face multiple barriers to disclosure: legal liability (actual litigation risk is moderate but widely publicized), employment dependence (loss of income and professional reputation if violate NDA), informational asymmetry (most testers lack resources to fight Meta litigation), and collective action barriers (dispersed individual testers cannot coordinate). Public also faces suppression: no direct access to testing data, dependent on leaked information, high barriers to independent verification. Theater ratio (0.55): Moderate. Enforcement is partly theatrical (actual litigation is rare; threat of litigation carries more weight) and partly substantive (legal threats are credible and have chilling effect). The enforcement apparatus is maintained through institutional inertia — legal departments perceive NDAs as standard practice — but the actual deterrence comes from perceived rather than demonstrated legal risk.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Meta's institutional view and the tester's powerless view is maximal. Meta sees the NDA as a coordination mechanism ensuring controlled testing (Rope) — it enables systematic vulnerability discovery by preventing testers from discussing methodology with external parties, thus preserving research validity. Testers see the NDA as pure extraction (Snare) — it prevents them from exiting an exploitative arrangement and silences them even when they discover serious platform failures. Regulatory bodies see mixed function (Tangled Rope) — the NDA enables structured testing (coordination benefit) but blocks access to findings that regulators need to assess platform risk. Journalists and researchers see constrained extraction (Tangled Rope) — the NDA enables Meta to maintain a monopoly on vulnerability information, preventing independent verification. The public sees the constraint as a complete information barrier (Snare) — unable to access information about platform risks, dependent on leaks or independent research, bearing cost of hidden flaws without recourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Undercover Testers: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction. Employment dependence + legal liability + lack of exit options produce high directionality. Public Epistemic Commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximal extraction. Abstract collective cannot exit, cannot organize, cannot even recognize itself as a victim (no mechanism to perceive the platform risks being hidden). Meta Internal Testing: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can disclose findings selectively, can use vulnerability data to improve platform (competitive advantage), can manage regulatory exposure by controlling information flow. Regulatory Bodies: Powerful + mobile → d≈0.45, f(d)≈0.45. Can mandate disclosure or conduct independent audits, but choose not to (or lack capacity). Mixed directionality reflects that they could exit but don't exercise that power. Researcher/Journalist Community: Organized + constrained → d≈0.68, f(d)≈0.98. Significant extraction because their ability to access and verify information is blocked by NDA. They cannot walk away from the constraint (it prevents their core function of independent verification) without accepting ignorance.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the tester NDA is structurally a Snare, not a Rope. The superficial coordination narrative (NDAs enable controlled testing) masks an asymmetric extraction mechanism. A true Rope would require: (1) genuine coordination benefit that requires information lockdown, or (2) symmetric cost-sharing (both Meta and testers restricted). Instead: Meta retains full disclosure authority (can leak findings selectively to regulators, use internally for competitive advantage), while testers bear full restriction. The testing function could operate under Rope-level disclosure rules (e.g., findings disclosed to independent auditors, regulatory bodies, or peer-reviewed researchers) without compromising research validity. The actual function of the NDA is to maximize Meta's monopoly on vulnerability information and prevent external accountability — pure extraction masked as coordination. The false positive (Rope framing) is dangerous because it legitimizes information lockdown as a technical necessity rather than recognizing it as a power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_vs_collective_disclosure,
    'Is the NDA extraction mechanism primarily against individual disclosure or collective whistleblowing?',
    'Historical analysis of disclosed Meta internal testing details; comparison of legal enforcement rates against individual leakers vs coordinated group disclosures',
    'If primarily individual: constraint is less severe (coordinated resistance possible). If primarily collective: extraction is more complete (prevents organized counter-narrative). Classification shifts from Snare (χ≈0.62) toward Snare (χ≈0.75).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_vs_collective_disclosure, empirical, 'Whether NDA targets individual or collective disclosure').

omega_variable(
    tester_alternative_employment,
    'Do alternative testing employment paths exist for undercover testers that do not require equivalent NDAs?',
    'Labor market analysis of testing contractor roles; survey of NDA clauses across competing platforms and independent testing firms',
    'If alternatives exist without NDAs: exit options upgrade to ''constrained'' or ''mobile'' (χ decreases). If trapped in NDA-requiring labor market: exit remains trapped (χ≈0.62 confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tester_alternative_employment, empirical, 'Availability of alternative employment without restrictive NDAs').

omega_variable(
    vulnerability_discovery_rate,
    'Does the NDA-enabled controlled testing environment discover more platform vulnerabilities than would be discovered through public bug bounties or academic research?',
    'Comparison of vulnerability discovery rates: Meta internal testing vs public bug bounties vs academic security research in similar domains',
    'If internal testing discovers significantly more: NDA enables coordination benefit (χ downward pressure). If discovery rates similar: NDA is primarily extraction with minimal coordination value (χ upward pressure to 0.70+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_discovery_rate, empirical, 'Whether NDA-enabled testing improves vulnerability discovery vs alternatives').

omega_variable(
    legal_enforceability_probability,
    'What is the actual probability that Meta would pursue legal action against a whistleblowing tester?',
    'Historical record of Meta litigation against former testers; cost-benefit analysis of litigation vs reputational damage',
    'If enforcement probability is low (<30%): constraint is primarily theatrical (theater_ratio approaches 0.85, piton classification). If high (>70%): constraint is substantive extraction (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_enforceability_probability, empirical, 'Actual enforcement likelihood of NDA legal terms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_nda, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_nda_tr_t0, meta_nda, theater_ratio, 0, 0.42).
narrative_ontology:measurement(meta_nda_tr_t3, meta_nda, theater_ratio, 3, 0.48).
narrative_ontology:measurement(meta_nda_tr_t6, meta_nda, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(meta_nda_be_t0, meta_nda, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(meta_nda_be_t3, meta_nda, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(meta_nda_be_t6, meta_nda, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_nda, enforcement_mechanism).
narrative_ontology:affects_constraint(meta_nda, platform_content_moderation_opacity).
narrative_ontology:affects_constraint(meta_nda, meta_algorithmic_opacity_regulation).
narrative_ontology:affects_constraint(meta_nda, labor_power_asymmetry_tech_contractors).

% DUAL FORMULATION NOTE:
% The tester NDA is a specific instantiation of broader platform information asymmetry constraints. It downstream affects regulatory response capacity (which depends on access to internal platform data) and labor protections (which depend on testers' ability to report conditions). The NDA is upstream of public epistemic deficit regarding platform risks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meta_nda, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
