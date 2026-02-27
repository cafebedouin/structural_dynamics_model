% ============================================================================
% CONSTRAINT STORY: ancient_grudge_verona
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_ancient_grudge_verona, []).

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
 *   constraint_id: ancient_grudge_verona
 *   human_readable: The Montague-Capulet Feud
 *   domain: social/political
 *
 * SUMMARY:
 *   The Montague-Capulet feud represents an inherited, transgenerational
 *   conflict that mandates spontaneous violence between two noble houses in
 *   Renaissance Verona. The 'ancient grudge' creates a context where acts of
 *   aggression are expected and peace is actively suppressed by social norms
 *   and expectations. This constraint extracts social and economic value from
 *   the city of Verona, benefiting only opportunistic criminals and those who
 *   thrive on chaos. It functions as a deadly snare for the families
 *   themselves and an impediment to the city's overall well-being. The
 *   concept of 'honor' is leveraged theatrically to sustain the conflict.
 *
 * KEY AGENTS:
 *   - Montague Family: Primary victim (powerless/trapped) - locked in the feud.
 *   - Capulet Family: Primary victim (powerless/trapped) - locked in the feud.
 *   - Verona Peace: Abstract victim (powerless/trapped) - the overall peace and stability of Verona suffers.
 *   - Opportunistic Criminals: Beneficiary (moderate/mobile) - thrive on the chaos and disruption.
 *   - Mercutio/Tybalt Archetypes: Mixed beneficiary/victim (powerful/mobile) - derive social standing but risk death.
 *   - The Church: Potential peacekeeper (organized/constrained) - attempts to mediate but is limited by the feud's intransigence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancient_grudge_verona, 0.75).
domain_priors:suppression_score(ancient_grudge_verona, 0.9).
domain_priors:theater_ratio(ancient_grudge_verona, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancient_grudge_verona, extractiveness, 0.75).
narrative_ontology:constraint_metric(ancient_grudge_verona, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(ancient_grudge_verona, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancient_grudge_verona, tangled_rope).
narrative_ontology:human_readable(ancient_grudge_verona, "The Montague-Capulet Feud").
narrative_ontology:topic_domain(ancient_grudge_verona, "social/political").

domain_priors:requires_active_enforcement(ancient_grudge_verona).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ancient_grudge_verona, opportunistic_criminals).
narrative_ontology:constraint_beneficiary(ancient_grudge_verona, mercutio_tybalt_archetypes).
narrative_ontology:constraint_victim(ancient_grudge_verona, montague_family).
narrative_ontology:constraint_victim(ancient_grudge_verona, capulet_family).
narrative_ontology:constraint_victim(ancient_grudge_verona, verona_peace).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The families, locked in a generational feud with no easy escape, see the conflict as a deadly snare. Violence is expected, and peace is actively suppressed by societal norms and expectations. They cannot exit without dishonor.
constraint_indexing:constraint_classification(ancient_grudge_verona, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Peacekeepers, such as the Prince, find their efforts constantly thwarted by the feud. They are constrained in their ability to enforce peace and face the constant threat of renewed violence. Suppressing the violence requires constant effort.
constraint_indexing:constraint_classification(ancient_grudge_verona, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% Perspective 3: Certain individuals within the families (e.g., Mercutio, Tybalt) derive social standing and opportunities for advancement through displays of aggression and loyalty to the feud. It's a tangled rope because they benefit from the conflict even as it endangers them. Mobile in that they can seek opportunities elsewhere (were it not for loyalty), but powerful within the existing structure.
constraint_indexing:constraint_classification(ancient_grudge_verona, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Perspective 4: The Church, as an organized institution, could potentially mediate or resolve the conflict. However, they are constrained by their own power dynamics and the deeply ingrained nature of the feud. The Church does derive some benefit (power/influence) from serving as an arbitrator, but also incurs costs when their interventions fail. Constrained in that they can't directly interfere without risking their authority.
constraint_indexing:constraint_classification(ancient_grudge_verona, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% Perspective 5: The concept of honor, as it applies to this specific feud, has largely become performative. It is invoked to justify violence, but the original reasons for the feud have been lost or distorted over time. The institutional memory sustains the conflict beyond its functional relevance. Analytical in that it represents an observer position that can assess the entire lifecycle.
constraint_indexing:constraint_classification(ancient_grudge_verona, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 6: The abstract 'peace of Verona' is trapped by the feud. It is the ultimate victim, constantly suppressed and violated by the actions of the families. An analytical perspective here highlights the structural problems preventing lasting peace.
constraint_indexing:constraint_classification(ancient_grudge_verona, snare,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancient_grudge_verona_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ancient_grudge_verona, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_grudge_verona, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ancient_grudge_verona, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ancient_grudge_verona, TR),
    TR >= 0.70.

:- end_tests(ancient_grudge_verona_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.75. The feud extracts a high social and economic cost from Verona. It diverts resources into violence, disrupts trade, and creates a climate of fear. Suppression: 0.90. Peace is actively suppressed by social norms, expectations of violence, and lack of credible exit options for family members. Theater Ratio: 0.75. 'Honor' is used as a performative justification for violence, masking the underlying causes and perpetuating the cycle of conflict. The theater ratio has increased over time as the original reasons for the feud have become less relevant, and the performative aspects have become more prominent.
 *
 * PERSPECTIVAL GAP:
 *   The families themselves see the conflict as an inescapable snare. Peacekeepers find their efforts thwarted. Certain individuals derive social standing from the conflict, making it a tangled rope for them. The Church sees the possibility of mediation, but is constrained by the feud's deep roots. The concept of 'honor' has become a performative justification, indicating a degraded piton. The abstract concept of Verona's peace is trapped and constantly violated.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims (Montagues, Capulets, Verona Peace) experience high extraction because they are trapped in the conflict with no easy escape. Beneficiaries (opportunistic criminals, certain individuals within the families) experience low to negative extraction because they profit from the chaos. The Church, as a potential peacekeeper, has a more neutral directionality, as its interventions are sometimes successful and sometimes not. The performative concept of honor is a piton because its original function has been lost, and it is now primarily used to justify violence.
 *
 * MANDATROPHY ANALYSIS:
 *   The Montague-Capulet feud presents a mandatrophy challenge because it could be framed as either a pure extraction mechanism (snare) or a mixed coordination-extraction system (tangled rope). The key to resolving this is to recognize that the feud, while harmful, also serves a coordination function by defining group identity and providing a framework for social interaction. The 'honor' system, while often performative, also provides a code of conduct and a means of resolving disputes (albeit violently). Therefore, the tangled rope classification is more accurate because it acknowledges both the extractive and coordinative aspects of the feud. The high extractiveness score reflects the severe consequences of the conflict, while the theater ratio indicates the performative nature of the 'honor' system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_cause_authenticity,
    'Is the original cause of the feud remembered accurately, or has it been distorted over time?',
    'Historical research into original documents and oral traditions.',
    'If the original cause is accurate, reconciliation might be possible. If distorted, the feud is self-perpetuating.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_cause_authenticity, empirical, 'Whether the original cause of the feud is accurately remembered.').

omega_variable(
    community_intervention_threshold,
    'At what point could external intervention (e.g., by the Prince, the Church, or a rival city-state) successfully resolve the conflict?',
    'Game-theoretic modeling of power dynamics and potential intervention strategies.',
    'Determines whether the feud is ultimately resolvable, or a permanent feature of Verona''s political landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_intervention_threshold, conceptual, 'The point at which external intervention could resolve the feud.').

omega_variable(
    opportunity_cost_perception,
    'To what extent do the families perceive the opportunity costs (e.g., economic, social) of maintaining the feud?',
    'Economic analysis of the feud''s impact on Verona''s economy, combined with surveys of family members'' attitudes.',
    'If the opportunity costs are high and perceived, the families might be more willing to compromise. If low, the feud will likely continue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_perception, empirical, 'The extent to which families perceive the opportunity costs of the feud.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancient_grudge_verona, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anci_tr_t0, ancient_grudge_verona, theater_ratio, 0, 0.3).
narrative_ontology:measurement(anci_tr_t10, ancient_grudge_verona, theater_ratio, 10, 0.65).
narrative_ontology:measurement(anci_tr_t20, ancient_grudge_verona, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(anci_be_t0, ancient_grudge_verona, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(anci_be_t10, ancient_grudge_verona, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(anci_be_t20, ancient_grudge_verona, base_extractiveness, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ancient_grudge_verona, enforcement_mechanism).
narrative_ontology:affects_constraint(ancient_grudge_verona, verona_city_watch_corruption).
narrative_ontology:affects_constraint(ancient_grudge_verona, arranged_marriage_alliance_failure).

% DUAL FORMULATION NOTE:
% The Montague-Capulet feud is a specific instance of a broader class of inherited conflicts and honor-based violence. It is distinct from related constraints, such as corruption within the city watch or the failure of arranged marriages to foster peace, but it influences and is influenced by them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
