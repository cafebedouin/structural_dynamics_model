% ============================================================================
% CONSTRAINT STORY: hoa_covenants
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenants, []).

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
 *   constraint_id: hoa_covenants
 *   human_readable: HOA Architectural Review and Restrictive Covenants
 *   domain: economic/social/legal
 *
 * SUMMARY:
 *   Homeowners' association (HOA) covenants are legally binding agreements
 *   that regulate the use and appearance of properties within a community.
 *   These covenants, enforced by an elected board or a property management
 *   company, aim to maintain property values and aesthetic standards.
 *   However, they also impose restrictions on individual homeowners,
 *   potentially leading to conflicts and perceptions of unfairness. The
 *   structure embodies coordination and extraction, making it a tangled rope.
 *
 * KEY AGENTS:
 *   - Individual Homeowners: Primary target (powerless/trapped) - bears the brunt of restrictive covenants.
 *   - HOA Board: Primary beneficiary (institutional/arbitrage) - maintains control and property values.
 *   - Property Management Companies: Secondary beneficiary (powerful/arbitrage) - profits from enforcement.
 *   - Conforming Homeowner: Experiences a mix of benefit and constraint.
 *   - Minority Homeowners: Homeowners who may face discrimination (powerless/trapped) - subject to biased enforcement.
 *   - Legal Scholar: Analytical observer (analytical/analytical) - analyzes structure and impact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenants, 0.55).
domain_priors:suppression_score(hoa_covenants, 0.65).
domain_priors:theater_ratio(hoa_covenants, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenants, extractiveness, 0.55).
narrative_ontology:constraint_metric(hoa_covenants, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hoa_covenants, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenants, tangled_rope).
narrative_ontology:human_readable(hoa_covenants, "HOA Architectural Review and Restrictive Covenants").
narrative_ontology:topic_domain(hoa_covenants, "economic/social/legal").

domain_priors:requires_active_enforcement(hoa_covenants).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenants, hoa_board).
narrative_ontology:constraint_beneficiary(hoa_covenants, property_management_companies).
narrative_ontology:constraint_victim(hoa_covenants, individual_homeowners).
narrative_ontology:constraint_victim(hoa_covenants, minority_homeowners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual homeowners often feel trapped by the covenants, facing significant costs and limited exit options due to property value impacts. They bear the brunt of the extraction with limited recourse.
constraint_indexing:constraint_classification(hoa_covenants, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Homeowners who generally conform to HOA covenants experience a mix of benefits and constraints. They benefit from maintained property values and aesthetic consistency but are constrained in their personal expression and property use.
constraint_indexing:constraint_classification(hoa_covenants, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% HOA boards benefit from the covenants by maintaining control and property values, but also face the burden of enforcement and potential legal challenges. They can arbitrage power effectively.
constraint_indexing:constraint_classification(hoa_covenants, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Property management companies benefit from the fees associated with enforcing the covenants, experiencing the constraint as a source of revenue and business opportunities. They can arbitrage within the HOA legal structure.
constraint_indexing:constraint_classification(hoa_covenants, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Legal scholars analyze the covenants as a tangled rope, acknowledging both the coordination benefits (property value maintenance, aesthetic consistency) and extraction costs (restrictions on individual liberty, potential for discriminatory enforcement).
constraint_indexing:constraint_classification(hoa_covenants, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenants_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hoa_covenants, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hoa_covenants, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenants, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenants_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. Restrictions on property use and modifications can significantly limit homeowners' autonomy and investment choices. Enforcement costs can be substantial, including fines and legal fees. Suppression (0.65): High. Homeowners have limited ability to exit the HOA or modify the covenants, particularly in established communities. The legal framework generally favors enforcement of covenants. Theater Ratio (0.40): Moderate. While there are stated goals of maintaining property values, the enforcement often comes down to aesthetic preferences and can feel performative, or worse, capricious.
 *
 * PERSPECTIVAL GAP:
 *   The individual homeowner sees a snare (extraction), while the HOA board and property management company view the arrangement as a rope (coordination). The legal scholar sees the hybrid nature of the arrangement, a tangled rope. Conforming Homeowners sees a mix of both.
 *
 * DIRECTIONALITY LOGIC:
 *   The HOA board benefits by maintaining property values and community standards (low d), while the individual homeowner bears the cost of restrictions (high d). Property management companies directly benefit through fees and contracts (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   The claim of 'maintaining property value' is often a justification for extractive practices. The tension between the stated coordination benefits and the clear limitations on individual homeowners requires a tangled rope classification. This avoids falsely labeling as rope, which would deny extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_fairness,
    'Is enforcement of covenants applied fairly and consistently across all homeowners, or does it disproportionately target certain groups?',
    'Statistical analysis of enforcement actions, surveys of homeowner experiences, legal audits of HOA practices',
    'If enforcement is fair: Covenant is a functional rope (property value maintenance). If enforcement is discriminatory: Covenant is a pure snare (extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_fairness, empirical, 'Fairness and consistency of covenant enforcement.').

omega_variable(
    community_preference_alignment,
    'Do the covenants reflect the genuine preferences of the majority of homeowners, or are they imposed by a small group or outdated regulations?',
    'Regular homeowner surveys, community meetings, amendment processes, and analyses of homeowner participation rates.',
    'If aligned with community preferences: Covenants serve a legitimate coordination function (rope). If misaligned: Covenants are an extraction mechanism (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_preference_alignment, empirical, 'Alignment of covenants with community preferences.').

omega_variable(
    property_value_impact,
    'Do the covenants genuinely contribute to maintaining or increasing property values, or do they stifle innovation and individual expression, ultimately decreasing property values?',
    'Comparative analysis of property values in HOA-governed communities versus non-HOA communities, surveys of potential homebuyers.',
    'If covenants increase property values: HOA provides net coordination benefit (rope or scaffold). If covenants decrease property values: HOA enacts net extraction (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_value_impact, empirical, 'Impact of covenants on property values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenants, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenants, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenants, theater_ratio, 10, 0.4).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenants, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenants, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hoa__be_t10, hoa_covenants, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(hoa__be_t20, hoa_covenants, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenants, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
