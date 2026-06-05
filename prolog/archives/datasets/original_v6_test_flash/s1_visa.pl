% ============================================================================
% CONSTRAINT STORY: s1_visa
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_s1_visa, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: s1_visa
 *   human_readable: SEC S-1 Registration Framework for Initial Public Offerings
 *   domain: economic/political
 *
 * SUMMARY:
 *   The SEC S-1 registration framework governs the public offering of
 *   securities in the US. It mandates the filing of Form S-1 under the
 *   Securities Act of 1933. This framework aims to protect investors by
 *   requiring companies to disclose detailed information about their
 *   business, financial condition, and management. However, the framework
 *   also imposes significant costs on issuing companies and creates
 *   opportunities for rent-seeking by intermediaries. The S-1 process can be
 *   viewed as a tangled rope, with both coordination and extraction aspects.
 *
 * KEY AGENTS:
 *   - Issuing Companies: Primary target (powerless/trapped) — bears the costs of compliance.
 *   - Securities and Exchange Commission (SEC): Primary beneficiary (institutional/arbitrage) — benefits from increased regulatory oversight.
 *   - Investment Banks: Intermediary (powerful/arbitrage) — benefits from underwriting IPOs and advising companies on S-1 filings.
 *   - Investing Public (late access): Secondary target (moderate/constrained) - bears the costs of higher-than-necessary compliance and first mover advantage of insiders.
 *   - Investing Public (early access): Secondary beneficiary (powerful/arbitrage) - benefits from the first mover advantage over retail.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(s1_visa, 0.45).
domain_priors:suppression_score(s1_visa, 0.25).
domain_priors:theater_ratio(s1_visa, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(s1_visa, extractiveness, 0.45).
narrative_ontology:constraint_metric(s1_visa, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(s1_visa, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(s1_visa, tangled_rope).
narrative_ontology:human_readable(s1_visa, "SEC S-1 Registration Framework for Initial Public Offerings").
narrative_ontology:topic_domain(s1_visa, "economic/political").

domain_priors:requires_active_enforcement(s1_visa).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(s1_visa, sec).
narrative_ontology:constraint_beneficiary(s1_visa, investment_banks).
narrative_ontology:constraint_beneficiary(s1_visa, investing_public_early_access).
narrative_ontology:constraint_victim(s1_visa, issuing_companies).
narrative_ontology:constraint_victim(s1_visa, investing_public_late_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The issuing company is trapped by the regulatory requirements and bears the costs of compliance (legal, accounting, administrative).
constraint_indexing:constraint_classification(s1_visa, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% The SEC benefits from the S-1 process through increased regulatory oversight and enforcement capabilities.
constraint_indexing:constraint_classification(s1_visa, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the S-1 registration as a system of mixed coordination and extraction. It coordinates the public offering of securities but also extracts rents and creates barriers to entry.
constraint_indexing:constraint_classification(s1_visa, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Investment banks benefit from underwriting IPOs and advising companies on S-1 filings.
constraint_indexing:constraint_classification(s1_visa, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% The investing public is extracted from by the higher-than-necessary costs for compliance as well as by the first-mover advantage of insiders.
constraint_indexing:constraint_classification(s1_visa, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(s1_visa_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(s1_visa, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(s1_visa, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(s1_visa_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.45 - The S-1 process does extract value in the form of compliance costs, legal fees, accounting fees and potentially slower process. However, the intent of the framework is to benefit all parties via the regulation of fraud and the promotion of transparency. Suppression: 0.25 - Low suppression - there are many ways for issuers to avoid the S-1 framework, including private placements, Regulation A+ offerings, and foreign listings. Theater Ratio: 0.30 - The S-1 framework aims to promote transparency and accountability. However, there is a theatrical aspect to compliance, as companies may prioritize meeting legal requirements over genuinely informative disclosures. An increasing prevalence of boilerplate disclosures and legal jargon is not necessarily indicative of underlying value to the public.
 *
 * PERSPECTIVAL GAP:
 *   The issuing company views the S-1 process as a snare, as it is trapped by the regulatory requirements and bears the costs of compliance. The SEC views the S-1 process as a rope, as it benefits from increased regulatory oversight and enforcement capabilities. An analytical observer sees the S-1 registration as a system of mixed coordination and extraction. It coordinates the public offering of securities but also extracts rents and creates barriers to entry. Investment banks view it as a rope as they make rents from advising and underwriting. The investing public (late access) see the process as a snare as they are extracted from via greater compliance costs as well as by first movers.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic reflects the structural relationships between the agents. The issuing company is the primary target and has a high directionality value. The SEC is the primary beneficiary and has a low directionality value. Investment banks and the early-access investing public are also beneficiaries, but to a lesser extent. The investing public has a higher directionality value as the ultimate victim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_cost_threshold,
    'What is the threshold for compliance cost at which the benefit is no longer net positive?',
    'Cost benefit analysis of compliance vs. benefits to the issuer, the SEC and the public. Comparison with alternative mechanisms such as direct listings.',
    'If cost is determined to exceed benefit, then reclassification to snare for all participants other than SEC, implying a lower net benefit of the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_threshold, empirical, 'Compliance cost threshold at which benefits are no longer net positive').

omega_variable(
    information_asymmetry_reduction,
    'To what degree is information asymmetry truly mitigated by S-1 disclosures, considering the complexity of financial statements and the ability of sophisticated actors to exploit loopholes?',
    'Empirical analysis of post-IPO performance relative to S-1 disclosures, accounting for various control variables. Examination of enforcement actions and litigation related to misleading disclosures.',
    'If information asymmetry is substantially reduced: S-1 framework is primarily a coordination mechanism (Rope). If asymmetry persists: S-1 framework is more of an extraction mechanism (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_reduction, empirical, 'The true impact of the S-1 on the investing public').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(s1_visa, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(s1_v_tr_t0, s1_visa, theater_ratio, 0, 0.2).
narrative_ontology:measurement(s1_v_tr_t5, s1_visa, theater_ratio, 5, 0.3).
narrative_ontology:measurement(s1_v_tr_t10, s1_visa, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(s1_v_be_t0, s1_visa, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(s1_v_be_t5, s1_visa, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(s1_v_be_t10, s1_visa, base_extractiveness, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(s1_visa, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
