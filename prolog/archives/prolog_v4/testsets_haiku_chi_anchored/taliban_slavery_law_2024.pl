% ============================================================================
% CONSTRAINT STORY: taliban_slavery_law_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taliban_slavery_law_2024, []).

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
 *   constraint_id: taliban_slavery_law_2024
 *   human_readable: Taliban Criminal Code Re-legalizing Slavery
 *   domain: political/legal/human_rights
 *
 * SUMMARY:
 *   The Taliban's 2024 criminal code re-legalizes slavery and slave-like
 *   practices following their return to power in Afghanistan in August 2021.
 *   The constraint represents a systematic, state-enforced extraction of
 *   labor from populations classified as legitimate slaves under Taliban
 *   interpretation of Islamic law and Pashtun customary practice. Primary
 *   victims include Hazara minorities (historically enslaved under Taliban
 *   rule), Tajik and other ethnic minorities, women and girls trafficked into
 *   forced marriage and domestic servitude, and children forced into labor
 *   and military service. The constraint exhibits extremely high
 *   extractiveness (0.78) because the extraction mechanism is (a) legal and
 *   state-enforced with no alternative remedy, (b) sustained by near-total
 *   suppression (0.85) through Taliban monopoly on violence and information
 *   control, and (c) low in theater (0.35) because the regime makes no
 *   pretense that this is voluntary or temporary — the slavery is openly
 *   legal and permanent. The theater ratio is low because the Taliban frames
 *   slavery as authentic Islamic law and social order, not as an emergency
 *   measure or coordination mechanism requiring narrative justification. This
 *   distinguishes the constraint from snares that maintain extractive
 *   mechanisms through performative rhetoric (e.g., predatory lending systems
 *   using 'financial inclusion' language, or labor trafficking using
 *   'employment' framing). The Taliban slavery law is a clean snare: brutal
 *   extraction with minimal narrative camouflage.
 *
 * KEY AGENTS:
 *   - Taliban State Apparatus: Primary beneficiary (institutional/arbitrage) — extracts labor, property, and control; frames constraint as restoration of law and order
 *   - Enslaved Populations: Primary victims (powerless/trapped) — Hazara minorities, trafficked women, child laborers; no legal exit or recourse
 *   - Wealthy Landowners and Military Commanders: Secondary beneficiary (powerful/mobile) — benefit from Taliban-sanctioned enslavement of labor force; have mobility to flee if Taliban falls
 *   - Afghan Civilian Population: Secondary victim (moderate/constrained) — subject to arbitrary enforcement; potential targets for enslavement expansion; cannot organize collective resistance
 *   - International Community: Powerful observer (powerful/mobile) — can measure and condemn but lacks enforcement mechanism within Afghanistan; trapped by geopolitical constraints
 *   - Analytical Observer: Universal legal/moral framework (analytical/analytical) — sees snare from UN conventions, international humanitarian law, universal human rights standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taliban_slavery_law_2024, 0.78).
domain_priors:suppression_score(taliban_slavery_law_2024, 0.85).
domain_priors:theater_ratio(taliban_slavery_law_2024, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taliban_slavery_law_2024, extractiveness, 0.78).
narrative_ontology:constraint_metric(taliban_slavery_law_2024, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(taliban_slavery_law_2024, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taliban_slavery_law_2024, snare).
narrative_ontology:human_readable(taliban_slavery_law_2024, "Taliban Criminal Code Re-legalizing Slavery").
narrative_ontology:topic_domain(taliban_slavery_law_2024, "political/legal/human_rights").

domain_priors:requires_active_enforcement(taliban_slavery_law_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taliban_slavery_law_2024, taliban_state_apparatus).
narrative_ontology:constraint_beneficiary(taliban_slavery_law_2024, wealthy_landowners).
narrative_ontology:constraint_beneficiary(taliban_slavery_law_2024, military_commanders).
narrative_ontology:constraint_victim(taliban_slavery_law_2024, enslaved_populations).
narrative_ontology:constraint_victim(taliban_slavery_law_2024, hazara_minorities).
narrative_ontology:constraint_victim(taliban_slavery_law_2024, tajik_minorities).
narrative_ontology:constraint_victim(taliban_slavery_law_2024, women_trafficked_populations).
narrative_ontology:constraint_victim(taliban_slavery_law_2024, child_laborers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED POPULATIONS (SNARE) — No legal recourse, no exit mechanism, complete vulnerability to coercive labor extraction. Hazara minorities, trafficked women, child laborers face systematic dehumanization encoded in law. d≈0.98, f(d)≈1.45, σ=1.0 → χ≈1.13.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVILIAN POPULATION (SNARE) — Constrained by Taliban total control; cannot organize collective resistance; subject to arbitrary enforcement. Potential victims of slavery expansion; face coercive confiscation of property and labor. d≈0.88, f(d)≈1.28, σ=1.0 → χ≈0.99.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TALIBAN STATE (ROPE) — Frames slavery legalization as restoring religious law and social order; extracts labor and resources through legal mechanism; experiences the constraint as coordination of territorial control and resource mobilization. d≈0.02, f(d)≈-0.18, σ=1.0 → χ≈-0.14. Net beneficiary; effective extraction is negative from their perspective because they see only the coordination function.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL OBSERVERS (SNARE) — Powerful but effectively trapped by geopolitical constraints and lack of enforcement mechanism. Can see and measure the extraction clearly but cannot exit the system without escalation. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SNARE) — From universal legal and moral frameworks (UN conventions, international humanitarian law), this is unambiguously a snare: systematic, legal, state-enforced extraction of labor without compensation, with no legal exit. ε and structure are invariant across observation methodologies. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.08.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taliban_slavery_law_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taliban_slavery_law_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taliban_slavery_law_2024, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taliban_slavery_law_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(taliban_slavery_law_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Extremely high. The Taliban slavery law is designed to extract maximum labor and resources from enslaved populations with zero compensation and zero legal recourse. The extraction is not hidden or softened by pretense — it is explicitly legal and permanent. Extractiveness has increased from 0.52 at initial legalization to 0.78 at full enforcement because the Taliban has moved from code on paper to systematic implementation across controlled territories. Suppression (0.85): Near-total. The Taliban monopolizes violence and information; enslaved populations have no legal appeals, no collective organizing capacity, no access to external help, and face extreme penalties for resistance or flight. Suppression is limited only by Taliban administrative reach in remote areas and by underground resistance networks. Theater ratio (0.35): Low. Unlike many extractive systems that maintain legitimacy through performative narrative (financial regulation using 'consumer protection' language, labor systems using 'development' rhetoric, taxation systems using 'public goods' framing), the Taliban slavery law is openly brutal. The regime claims Islamic law authenticity and social restoration, but makes no pretense that slavery is temporary, voluntary, or reciprocal. Low theater reflects that the regime's legitimacy claim is based on religious authority and force, not on narrative justification of the extraction mechanism itself.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximal and irreducible. The Taliban state sees the slavery law as a coordination mechanism (Rope from their perspective) — they are organizing labor, territorial control, and social order according to their interpretation of Islamic law. From the Taliban's power position (institutional/arbitrage), the constraint appears functional: it solves the 'problem' of how to mobilize labor and resources for state consolidation. From the enslaved population's position (powerless/trapped), the same structure is a snare with no exit and no coordination benefit — extraction is pure and one-directional. The international observer sees snare clearly because universal legal frameworks define the structure as systematic labor extraction, but lacks the enforcement mechanism to create an exit pathway for victims. This is one of the clearest examples of how indexical classification reveals structural asymmetry: the two sides experience the same law fundamentally differently because their power positions, exit options, and time horizons are incommensurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Enslaved populations: Victim + trapped → d≈0.98, f(d)≈1.45. Maximum extraction target. No exit, no choice, complete vulnerability. Taliban state: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.18. Net beneficiary from their own perspective; can exit (flee) if system destabilizes. Afghan civilians: Victim + constrained → d≈0.88, f(d)≈1.28. High extraction target but with some residual constraint options (migration, black market, underground networks). International community: Powerful but trapped → d≈0.45, f(d)≈0.48. Powerful agents (states, NGOs) can measure and document the constraint but face geopolitical costs for intervention; effective exit is constrained despite nominal power. Analytical observer: d≈0.72, f(d)≈1.15. Observer sees the structure clearly through universal legal frameworks; classification is snare from all methodologies.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_scope_slavery,
    'How does the Taliban code define ''slavery'' vs. ''debt bondage'' vs. ''forced labor''? Does the legal text explicitly legalize chattel slavery or use de facto mechanisms?',
    'Detailed textual analysis of the Taliban criminal code; comparison with Ottoman-era and pre-2001 Taliban codes; case documentation of enforcement patterns',
    'If explicit chattel slavery: unambiguous snare classification sustained. If de facto through debt bondage and forced labor provisions: still snare but with lower ε (≈0.65) and different suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_scope_slavery, empirical, 'Legal definition of slavery vs. debt bondage in Taliban criminal code').

omega_variable(
    enforcement_capacity_scale,
    'What fraction of Afghanistan''s population is actually subject to active enslavement under this law? Is the law''s extractive power limited by Taliban enforcement capacity or by population size?',
    'Population surveys of enslaved/trafficked populations; documentation of active enforcement; comparison with Taliban administrative reach in different regions',
    'If enforcement is <20% of population: suppression may be lower (Taliban cannot reach all territories). If enforcement is >50% where Taliban controls: suppression is maximal and extractiveness approaches 0.85 baseline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_scale, empirical, 'Proportion of population under active enslavement').

omega_variable(
    coalition_resistance_formation,
    'Can enslaved populations or international actors form sufficient coordination to create alternative exit pathways (underground networks, refugee flows, insurgent coalitions)?',
    'Tracking of refugee flows, underground resistance networks, Taliban defection rates; documentation of autonomous region attempts',
    'If strong coalitions emerge: exit_options upgrade from trapped to constrained; powerless becomes organized; snare may degrade to tangled rope with lower χ. If coalitions are suppressed: snare classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_resistance_formation, empirical, 'Possibility of organized resistance coalition formation').

omega_variable(
    international_intervention_threshold,
    'At what scale of enforcement or severity does the international community escalate from sanctions to direct intervention, creating an actual exit mechanism for victims?',
    'Precedent analysis (Rwanda, Syria, Darfur); stated intervention thresholds from UN, NATO, regional powers; monitoring of escalation signals',
    'If intervention becomes likely: international perspective''s exit_options upgrade from mobile to arbitrage; powerful actors'' d drops; global scope χ decreases. If intervention remains unlikely: snare classification hardens globally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_intervention_threshold, preference, 'Likelihood of international military intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taliban_slavery_law_2024, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_tr_t0, taliban_slavery_law_2024, theater_ratio, 0, 0.28).
narrative_ontology:measurement(tsl_tr_t2, taliban_slavery_law_2024, theater_ratio, 2, 0.31).
narrative_ontology:measurement(tsl_tr_t4, taliban_slavery_law_2024, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(tsl_be_t0, taliban_slavery_law_2024, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(tsl_be_t2, taliban_slavery_law_2024, base_extractiveness, 2, 0.65).
narrative_ontology:measurement(tsl_be_t4, taliban_slavery_law_2024, base_extractiveness, 4, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taliban_slavery_law_2024, enforcement_mechanism).
narrative_ontology:affects_constraint(taliban_slavery_law_2024, afghanistan_minority_persecution).
narrative_ontology:affects_constraint(taliban_slavery_law_2024, trafficking_networks_central_asia).
narrative_ontology:affects_constraint(taliban_slavery_law_2024, forced_recruitment_taliban_military).

% DUAL FORMULATION NOTE:
% The Taliban slavery law is a primary constraint affecting downstream problems of minority persecution, trafficking networks, and forced recruitment. The upstream context is Taliban territorial consolidation and legitimacy-seeking through religious law implementation. Decomposition: this story (slavery law as mechanism) should be linked to stories about specific slave populations (Hazara enslavement, women trafficking, child soldier recruitment) which have their own ε values reflecting the particular demographic and institutional circumstances of each population.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taliban_slavery_law_2024, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
