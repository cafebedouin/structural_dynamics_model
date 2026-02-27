% ============================================================================
% CONSTRAINT STORY: acip_hep_b_infant_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acip_hep_b_infant_mandate, []).

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
 *   constraint_id: acip_hep_b_infant_mandate
 *   human_readable: ACIP Universal Hepatitis B Vaccination Mandate for Infants (1991-2025)
 *   domain: social/medical
 *
 * SUMMARY:
 *   The ACIP's universal hepatitis B vaccination mandate for infants,
 *   introduced in 1991, aimed to eradicate Hep B transmission. While it has
 *   significantly reduced infection rates, it also created tensions between
 *   public health goals, pharmaceutical company profits, and parental
 *   autonomy. The mandate exemplifies a complex social constraint with both
 *   positive and negative aspects, leading to varying perspectives.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Companies: Primary beneficiary (institutional/arbitrage) - benefit from increased vaccine sales.
 *   - Healthcare Providers: Intermediary (moderate/constrained) - required to administer vaccine; face liability risks and patient care responsibilities.
 *   - Parents Opposed to Vaccination: Primary target (powerless/trapped) - face social pressure, potential legal repercussions, and concerns about adverse reactions.
 *   - Infant Population Health: Beneficiary (abstract/universal) - overall health of infants improved through reduced Hep B infections.
 *   - Infants with Adverse Reactions: Victim (powerless/trapped) - experience negative health consequences from the vaccine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acip_hep_b_infant_mandate, 0.35).
domain_priors:suppression_score(acip_hep_b_infant_mandate, 0.4).
domain_priors:theater_ratio(acip_hep_b_infant_mandate, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, extractiveness, 0.35).
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acip_hep_b_infant_mandate, tangled_rope).
narrative_ontology:human_readable(acip_hep_b_infant_mandate, "ACIP Universal Hepatitis B Vaccination Mandate for Infants (1991-2025)").
narrative_ontology:topic_domain(acip_hep_b_infant_mandate, "social/medical").

domain_priors:requires_active_enforcement(acip_hep_b_infant_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, healthcare_providers).
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, infant_population_health).
narrative_ontology:constraint_victim(acip_hep_b_infant_mandate, parents_opposed_vaccination).
narrative_ontology:constraint_victim(acip_hep_b_infant_mandate, infants_with_adverse_reactions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Parents who strongly oppose vaccination feel trapped by the mandate, facing social pressure and potential legal repercussions if they refuse. They perceive the mandate as a pure extraction of their autonomy. d=0.95, f(d)=1.42, chi=0.35*1.42*1.0=0.497
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Pharmaceutical companies benefit directly from increased vaccine sales due to the mandate. They experience it as coordination, facilitating market expansion. d=0.05, f(d)=-0.12, chi=0.35*(-0.12)*1.0=-0.042
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Healthcare providers are required to administer the vaccine, facing potential liability for adverse reactions but also benefiting from increased patient volume and reduced Hep B cases. d=0.50, f(d)=0.65, chi=0.35*0.65*1.0=0.2275
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From a broad perspective, the mandate represents a complex interplay of public health benefits, economic incentives, and individual autonomy. It is a tangled rope due to the combination of coordination (public health improvement) and asymmetric extraction (restrictions on parental choice). d=0.72, f(d)=1.15, chi=0.35*1.15*1.0=0.4025
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acip_hep_b_infant_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(acip_hep_b_infant_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) due to the overall public health benefits, but suppression is present (0.40) because of limited parental choice and potential social pressure. The theater ratio is low (0.20), as the primary focus is on genuine health outcomes rather than performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing positions of those affected. Pharmaceutical companies see a beneficial market expansion. Healthcare providers navigate a complex situation with benefits and risks. Parents opposed to vaccination feel coerced and see their autonomy diminished. The analytical observer recognizes the inherent trade-offs between individual liberty and collective well-being.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the beneficiary/victim status and exit options. Pharmaceutical companies (beneficiary, arbitrage) have a low d value. Parents opposed to vaccination (victim, trapped) have a high d value. Healthcare providers and the analytical observer occupy intermediate positions reflecting their mixed experiences.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vaccine_efficacy_vs_adverse_effects,
    'What is the precise ratio of vaccine efficacy in preventing Hep B versus the incidence of adverse reactions in infants?',
    'Longitudinal studies tracking Hep B incidence and adverse reaction rates in vaccinated vs. unvaccinated populations.',
    'If efficacy is significantly higher than adverse effects, the mandate is justified. If adverse effects are more prevalent, the mandate should be re-evaluated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccine_efficacy_vs_adverse_effects, empirical, 'Balancing vaccine efficacy with adverse effects').

omega_variable(
    alternative_vaccination_schedules,
    'Are there alternative vaccination schedules that could mitigate adverse reactions while maintaining sufficient protection against Hep B?',
    'Clinical trials comparing different vaccination schedules and their impact on Hep B incidence and adverse reaction rates.',
    'If alternative schedules are effective, the mandate could be modified to offer more flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_vaccination_schedules, empirical, 'Exploring alternative vaccination schedules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acip_hep_b_infant_mandate, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acip_tr_t0, acip_hep_b_infant_mandate, theater_ratio, 0, 0.1).
narrative_ontology:measurement(acip_tr_t12, acip_hep_b_infant_mandate, theater_ratio, 12, 0.15).
narrative_ontology:measurement(acip_tr_t24, acip_hep_b_infant_mandate, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(acip_be_t0, acip_hep_b_infant_mandate, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(acip_be_t12, acip_hep_b_infant_mandate, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(acip_be_t24, acip_hep_b_infant_mandate, base_extractiveness, 24, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acip_hep_b_infant_mandate, resource_allocation).
narrative_ontology:affects_constraint(acip_hep_b_infant_mandate, childhood_vaccination_schedule).
narrative_ontology:affects_constraint(acip_hep_b_infant_mandate, vaccine_hesitancy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
