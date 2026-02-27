% ============================================================================
% CONSTRAINT STORY: notary_ink_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notary_ink_dependency, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: notary_ink_dependency
 *   human_readable: The Notary/Wet-Ink Persistence
 *   domain: legal/institutional
 *
 * SUMMARY:
 *   The persistence of notary and wet-ink requirements for legal documents
 *   reflects a balance between ensuring document integrity and creating
 *   barriers to efficient transactions. While intended to provide security
 *   and authentication, these requirements can impose significant costs on
 *   individuals and businesses, particularly those in remote locations or
 *   lacking easy access to notarial services. The constraint can be viewed as
 *   a tangled rope, where coordination (security) is intertwined with
 *   extraction (rent-seeking and inefficiency).
 *
 * KEY AGENTS:
 *   - Remote Transactors: Primary target (powerless/trapped) — bears the cost of inconvenience and delays.
 *   - Notary Profession: Primary beneficiary (institutional/arbitrage) — benefits from the continued demand for their services.
 *   - Document Custodians: Secondary beneficiary (powerful/constrained) — benefits from existing procedures, constrained by legal requirements.
 *   - Efficiency of Commerce: Indirect victim (powerless/trapped) — overall economic activity is hampered by the friction introduced by the requirement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notary_ink_dependency, 0.55).
domain_priors:suppression_score(notary_ink_dependency, 0.7).
domain_priors:theater_ratio(notary_ink_dependency, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notary_ink_dependency, extractiveness, 0.55).
narrative_ontology:constraint_metric(notary_ink_dependency, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(notary_ink_dependency, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notary_ink_dependency, tangled_rope).
narrative_ontology:human_readable(notary_ink_dependency, "The Notary/Wet-Ink Persistence").
narrative_ontology:topic_domain(notary_ink_dependency, "legal/institutional").

domain_priors:requires_active_enforcement(notary_ink_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notary_ink_dependency, notary_profession).
narrative_ontology:constraint_beneficiary(notary_ink_dependency, document_custodians).
narrative_ontology:constraint_victim(notary_ink_dependency, remote_transactors).
narrative_ontology:constraint_victim(notary_ink_dependency, efficiency_of_commerce).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual who is unable to easily access a notary and is forced to comply with the in-person signature requirement to complete a transaction. Trapped due to lack of alternatives.
constraint_indexing:constraint_classification(notary_ink_dependency, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Notaries benefit from the continued requirement for their services, experiencing this as a coordination mechanism that maintains their professional relevance and income.
constraint_indexing:constraint_classification(notary_ink_dependency, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Large institutions that are legally required to maintain document custody, they benefit from the persistence of paper processes even if they could technically adopt digital workflows. They are constrained by compliance concerns.
constraint_indexing:constraint_classification(notary_ink_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer recognizes the mix of coordination (security and authentication) and extraction (rent-seeking and inefficiency) inherent in the wet-ink dependency.
constraint_indexing:constraint_classification(notary_ink_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notary_ink_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(notary_ink_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(notary_ink_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(notary_ink_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notary_ink_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: Moderate. The requirement extracts time and money from remote transactors but also provides security to the legal system. Suppression: High. Alternatives like digital signatures are suppressed by legal and institutional inertia. Theater Ratio: Moderate-High. While there is some functional benefit to the wet-ink requirement (verification of identity), the requirement is also performative (gives the impression of security without necessarily increasing it).
 *
 * PERSPECTIVAL GAP:
 *   The remote transactor sees this as a snare because they bear the costs. The notary profession sees this as a rope, as it is a coordination mechanism to provide their service. The document custodians sees this as a tangled rope, because they benefit from the stability, but it limits their efficiency.
 *
 * DIRECTIONALITY LOGIC:
 *   Remote transactors are the primary victims, bearing the cost of the constraint in terms of time, money, and convenience. The notary profession benefits directly from the requirement, as it guarantees demand for their services. Document custodians benefit from the persistence of paper processes even if they could technically adopt digital workflows. The directionality values are derived from these structural relationships.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_signature_trust,
    'How reliable and resistant to fraud are digital signature technologies, and what level of public trust do they command?',
    'Ongoing audits of digital signature platforms, analysis of fraud rates compared to wet-ink signatures, and surveys of public confidence in digital signatures.',
    'If high reliability and trust: Pressure increases to relax wet-ink dependency. If low: The status quo persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_signature_trust, empirical, 'Reliability and trust in digital signature technology').

omega_variable(
    regulatory_capture_risk,
    'To what extent is the persistence of wet-ink requirements driven by regulatory capture or lobbying by the notary profession?',
    'Analysis of campaign finance and lobbying records, studies of regulatory decision-making processes, and public interest litigation challenging restrictive regulations.',
    'If high regulatory capture: Wet-ink dependency may persist despite technological advancements. If low: Regulations may adapt more readily to new technologies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Degree to which wet-ink persistence is driven by regulatory capture').

omega_variable(
    security_vs_convenience,
    'What is the acceptable trade-off between security and convenience in legal document signing?',
    'Legal and philosophical debates, policy discussions, cost-benefit analyses of different authentication methods.',
    'A strong preference for security will lead to the continuation of stringent requirements. A higher value placed on convenience will push towards more flexible alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_convenience, preference, 'Balance between security and convenience in legal documents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notary_ink_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notary_ink_dependency, theater_ratio, 0, 0.4).
narrative_ontology:measurement(nota_tr_t10, notary_ink_dependency, theater_ratio, 10, 0.5).
narrative_ontology:measurement(nota_tr_t20, notary_ink_dependency, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notary_ink_dependency, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nota_be_t10, notary_ink_dependency, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(nota_be_t20, notary_ink_dependency, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notary_ink_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(notary_ink_dependency, digital_signature_adoption).
narrative_ontology:affects_constraint(notary_ink_dependency, legal_document_security).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
