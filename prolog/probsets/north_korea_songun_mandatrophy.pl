% ============================================================================
% CONSTRAINT STORY: north_korea_songun_mandatrophy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_north_korea_songun_mandatrophy, []).

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
 *   constraint_id: north_korea_songun_mandatrophy
 *   human_readable: North Korean "Songun" (Military-First) Policy
 *   domain: political/economic
 *
 * SUMMARY:
 *   The "Songun" (Military-First) policy in North Korea mandates the absolute
 *   prioritization of the Korean People's Army in all affairs of state and
 *   resource allocation. This policy has led to severe economic hardship for
 *   the civilian population, while solidifying the power of the ruling Kim
 *   family and the military elite. The policy's emphasis on military strength
 *   as a means of national survival has created a closed and oppressive
 *   society, with limited freedoms and widespread human rights abuses. While
 *   the regime justifies the policy as a necessary response to external
 *   threats, critics argue that it serves primarily to maintain the Kim
 *   family's grip on power.
 *
 * KEY AGENTS:
 *   - North Korean Civilian Population: Primary victim (powerless/trapped) - Bears the brunt of resource extraction for the military.
 *   - Korean People's Army: Primary beneficiary (institutional/arbitrage) - Receives prioritized resource allocation and maintains power.
 *   - Ruling Kim Family: Organized actors (organized/constrained) - benefits most but must coordinate and enforce to maintain power.
 *   - National Economy: Constrained and distorted economic actor (institutional/constrained) - resource extraction creates long-term damage.
 *   - Analytical Observer: Global observer (analytical/analytical) - assesses the regime's true goals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(north_korea_songun_mandatrophy, 0.85).
domain_priors:suppression_score(north_korea_songun_mandatrophy, 0.9).
domain_priors:theater_ratio(north_korea_songun_mandatrophy, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, extractiveness, 0.85).
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(north_korea_songun_mandatrophy, snare).
narrative_ontology:human_readable(north_korea_songun_mandatrophy, "North Korean \"Songun\" (Military-First) Policy").
narrative_ontology:topic_domain(north_korea_songun_mandatrophy, "political/economic").

domain_priors:requires_active_enforcement(north_korea_songun_mandatrophy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(north_korea_songun_mandatrophy, korean_peoples_army).
narrative_ontology:constraint_beneficiary(north_korea_songun_mandatrophy, ruling_kim_family).
narrative_ontology:constraint_victim(north_korea_songun_mandatrophy, north_korean_civilian_population).
narrative_ontology:constraint_victim(north_korea_songun_mandatrophy, national_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: North Korean Civilian Population (Snare). Powerless and trapped within the system, they bear the brunt of resource extraction for the military, facing severe economic hardship and limited freedoms. No real exit options.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Korean People's Army (Rope). Benefits from the policy through resource prioritization, maintaining power and privilege. They experience the policy as coordination, ensuring their continued dominance.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 3: National Economy (Piton). The economy is constrained and distorted by the policy, with resources diverted from productive sectors. While the policy was initially intended to strengthen national defense, it has become a drag on economic development, maintained through inertia and lack of alternatives. The theater ratio is still high, indicating that the original function of the policy has atrophied, leaving behind only performative elements and the structures to continue to enforce them.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4: Ruling Kim Family (Tangled Rope). The Kim family benefits greatly from the Songun policy, solidifying their power and control. While they rely on the KPA for protection, they are also constrained by the need to maintain the army's loyalty and prevent internal challenges. They have power over the system (extraction from population), but must also actively maintain and coordinate it (coordinate with KPA) in order to maintain power (active enforcement).
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: Analytical Observer (Snare). An objective observer sees the Songun policy as a system of coercion and control, extracting resources from the civilian population to benefit the military elite. The policy is unsustainable in the long term, leading to economic stagnation and social unrest.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_korea_songun_mandatrophy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(north_korea_songun_mandatrophy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(north_korea_songun_mandatrophy, TR),
    TR >= 0.70.

:- end_tests(north_korea_songun_mandatrophy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85): Very high. The policy leads to severe resource deprivation for the civilian population, with a large portion of the national budget allocated to the military. Suppression (0.90): Extremely high. The regime maintains a tight grip on power through propaganda, surveillance, and repression, severely limiting freedoms and suppressing dissent. Theater ratio (0.75): High. While the policy has some genuine military function, much of it is performative, serving to project an image of strength and deter potential adversaries. The Kim family's constant appearances on television and newspapers visiting military installations and personnel serves mostly a propaganda and political purpose.
 *
 * PERSPECTIVAL GAP:
 *   The North Korean civilian population views this as pure extraction (snare). The military views this as coordination to maintain power (rope). The Kim family benefits from extraction and coordinates with the KPA to actively maintain power (tangled rope). An analytical observer sees a destructive regime (snare) extracting maximum possible value to consolidate their power at the expense of their citizen population.
 *
 * DIRECTIONALITY LOGIC:
 *   The civilian population's directionality value is high because they are powerless and trapped. The military's directionality value is low because they are the beneficiaries with power and privilege within the system. The Kim family has moderate directionality because they coordinate to maintain power, but must extract from their citizen population to do so.
 *
 * MANDATROPHY ANALYSIS:
 *   This resolves a potential mandatrophy by differentiating the genuine benefits for the ruling elite and the military from the detrimental impacts on the broader population. The policy is not merely a mechanism for national defense, but also a tool for internal control and resource extraction, hence its classification as a snare for the civilian population and a mixed tangle for the Kim family. It is only a rope from the narrow perspective of the KPA, since they are the main benefactors of the policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internal_stability_threshold,
    'At what level of economic hardship does the civilian population become a significant threat to internal stability?',
    'Analysis of historical data on social unrest and economic conditions in North Korea, coupled with intelligence gathering on current levels of dissatisfaction.',
    'If threshold is high: The policy can be sustained for longer, resulting in continued economic stagnation. If threshold is low: The policy may lead to internal unrest and potential regime change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_stability_threshold, empirical, 'Threshold of economic hardship for internal stability').

omega_variable(
    external_threat_validity,
    'To what extent is the perceived external threat driving the Songun policy a genuine security concern versus a tool for internal control?',
    'Geopolitical analysis of the Korean peninsula, examining the military capabilities and intentions of neighboring countries, as well as the regime''s propaganda and rhetoric.',
    'If genuine threat: The policy may be seen as a necessary evil for national defense. If a tool for control: The policy is primarily a means of maintaining power and suppressing dissent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_threat_validity, conceptual, 'Validity of external threat driving Songun policy').

omega_variable(
    military_loyalty_limits,
    'What is the limit of the Korean People''s Army''s loyalty in the face of growing economic hardship and potential social unrest?',
    'Intelligence gathering on the KPA''s internal dynamics, morale, and potential for factions to emerge that may challenge the regime.',
    'If loyalty is absolute: The regime can rely on the KPA to suppress dissent and maintain control. If loyalty is conditional: The KPA may become a source of instability, potentially leading to a coup or civil war.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_loyalty_limits, empirical, 'Limits of military loyalty to the regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(north_korea_songun_mandatrophy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nort_tr_t0, north_korea_songun_mandatrophy, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nort_tr_t10, north_korea_songun_mandatrophy, theater_ratio, 10, 0.5).
narrative_ontology:measurement(nort_tr_t20, north_korea_songun_mandatrophy, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(nort_be_t0, north_korea_songun_mandatrophy, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(nort_be_t10, north_korea_songun_mandatrophy, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(nort_be_t20, north_korea_songun_mandatrophy, base_extractiveness, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(north_korea_songun_mandatrophy, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
