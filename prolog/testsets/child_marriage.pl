% ============================================================================
% CONSTRAINT STORY: child_marriage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_child_marriage, []).

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
 *   constraint_id: child_marriage
 *   human_readable: Child Marriage as a Social Constraint
 *   domain: social/religious
 *
 * SUMMARY:
 *   Child marriage is a social constraint primarily affecting girls in
 *   impoverished and traditional societies. It functions as an economic and
 *   social mechanism for families and communities, but at the cost of the
 *   child's autonomy, health, education, and life opportunities. The practice
 *   is enforced through a combination of intense social pressure, economic
 *   necessity, and traditional or religious norms, creating a
 *   high-suppression environment for its primary victims.
 *
 * KEY AGENTS:
 *   - The Child Bride: Primary victim (powerless/trapped) — bears the full extractive cost.
 *   - The Child's Family: Beneficiary and enforcer, but also victim of systemic poverty (moderate/constrained).
 *   - Community Elders: Institutional beneficiaries (institutional/arbitrage) — uphold the system and derive social power from it.
 *   - International NGOs: Organized opposition (organized/mobile) — seek to dismantle the constraint through external intervention.
 *   - Analytical Observer: System-level view (analytical/analytical) — classifies the constraint based on its objective properties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(child_marriage, 0.85).
domain_priors:suppression_score(child_marriage, 0.9).
domain_priors:theater_ratio(child_marriage, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(child_marriage, extractiveness, 0.85).
narrative_ontology:constraint_metric(child_marriage, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(child_marriage, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(child_marriage, snare).
narrative_ontology:human_readable(child_marriage, "Child Marriage as a Social Constraint").
narrative_ontology:topic_domain(child_marriage, "social/religious").

domain_priors:requires_active_enforcement(child_marriage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(child_marriage, family_of_the_child).
narrative_ontology:constraint_beneficiary(child_marriage, groom_and_his_family).
narrative_ontology:constraint_beneficiary(child_marriage, community_elders).
narrative_ontology:constraint_victim(child_marriage, the_child_bride).
narrative_ontology:constraint_victim(child_marriage, societal_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CHILD BRIDE (SNARE) — The primary victim. Experiences total loss of autonomy, education, and future prospects. With no legal or social power and no exit, the constraint is pure, coercive extraction. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.97.
constraint_indexing:constraint_classification(child_marriage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CHILD'S FAMILY (TANGLED ROPE) — Experiences the constraint as a tragic necessity for economic survival and social cohesion. They are beneficiaries (bride price, reduced costs) but also victims of a larger system of poverty and tradition that constrains their choices. They see both the coordination function and the extraction. d≈0.70, f(d)≈1.05, σ=0.8 → χ≈0.71.
constraint_indexing:constraint_classification(child_marriage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE COMMUNITY ELDERS (ROPE) — As enforcers of tradition, they view the practice as a pure coordination mechanism for maintaining social order, family alliances, and religious purity. From their position of power, the extractive costs are invisible or justified. d≈0.15, f(d)≈-0.01, σ=0.9 → χ≈-0.01. Negative effective extraction signifies a net beneficiary.
constraint_indexing:constraint_classification(child_marriage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE INTERNATIONAL NGO (SCAFFOLD) — Views child marriage as a temporary problem to be dismantled through legal reform, education, and economic empowerment. Their actions are a scaffold intended to create a society where the practice is no longer necessary or tolerated, implying a sunset clause on the constraint's legitimacy. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.77.
constraint_indexing:constraint_classification(child_marriage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (SNARE) — The system's default classification. The extremely high base extractiveness (ε=0.85) and suppression (0.90) overwhelmingly define the constraint's structure as a snare, regardless of the justifications offered by its beneficiaries. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.17.
constraint_indexing:constraint_classification(child_marriage, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(child_marriage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(child_marriage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(child_marriage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(child_marriage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(child_marriage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.85) is extremely high, representing the near-total appropriation of the child's life-path and potential. Suppression (0.90) is also extremely high due to the combination of economic dependency, family coercion, and powerful social norms, leaving the victim with virtually no alternatives. The theater ratio (0.40) is moderate; while the practice is wrapped in the theater of tradition and ceremony, it serves a very real and brutal economic and social function for the beneficiaries. It is not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the child, it is an inescapable Snare. For the family, it is a Tangled Rope—a terrible choice that provides a genuine (if tragic) coordination benefit for family survival while extracting horribly from one member. For community elders, it is a Rope—a tool for social order whose extractive nature is rendered invisible by their ideology and structural position. For outside activists, it is a Scaffold—a temporary social ill to be dismantled. The analytical view confirms the victim's perspective: the objective metrics classify this as a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the clear victim/beneficiary structure. The child bride is a trapped victim (d≈0.95), experiencing maximum extraction. The community elders are beneficiaries with arbitrage power (d≈0.15), experiencing the system as a net subsidy. The family is a constrained victim/beneficiary (d≈0.70), experiencing high but not maximal extraction, reflecting their dual role. This differentiation in 'd' values is what drives the different classifications from the same base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case where mandatrophy occurs. Beneficiaries (community elders) frame the Snare as a Rope ('our sacred tradition,' 'a mechanism for social stability'). This mislabeling serves to naturalize extreme extraction and suppress dissent. The Deferential Realism framework resolves this by holding both perspectives simultaneously: it validates the elders' classification *as their perspective* (Rope) while using the objective metrics and the victim's perspective to establish the analytical ground truth of the constraint as a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_cultural_driver,
    'Is child marriage primarily a response to economic desperation (a ''poverty trap'') or a deeply embedded cultural/religious practice independent of economic status?',
    'Correlational studies between rates of child marriage and changes in local GDP, female education levels, and access to economic opportunities.',
    'If primarily economic, the constraint is more of a Tangled Rope that can be unwound with development aid (Scaffold). If primarily cultural, it is a more resilient Snare requiring normative intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_vs_cultural_driver, empirical, 'Distinguishing between economic and cultural drivers of the practice.').

omega_variable(
    parental_agency,
    'To what degree do parents, particularly mothers, act as willing enforcers versus coerced participants in the marriage of their children?',
    'Qualitative sociological research, interviews with families, and analysis of social power dynamics within households.',
    'If parents have low agency, they are better modeled as co-victims, shifting the beneficiary/victim balance. If they have high agency, they are primary beneficiaries, reinforcing the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parental_agency, conceptual, 'Assessing the level of agency and coercion experienced by parents.').

omega_variable(
    legal_prohibition_effectiveness,
    'Do top-down legal bans on child marriage reduce its prevalence, or merely drive the practice underground and remove what little protection exists?',
    'Comparative analysis of regions with and without legal bans, controlling for other socioeconomic factors. Tracking changes in reported vs. estimated incidence post-legislation.',
    'If effective, legal bans are a valid Scaffold. If ineffective or counterproductive, they represent a Piton (performative but non-functional) and the underlying Snare remains untouched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_prohibition_effectiveness, empirical, 'Evaluating the real-world impact of legal prohibitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(child_marriage, 1980, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chil_tr_t1980, child_marriage, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(chil_tr_t2005, child_marriage, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(chil_tr_t2030, child_marriage, theater_ratio, 2030, 0.4).

% Extraction over time
narrative_ontology:measurement(chil_be_t1980, child_marriage, base_extractiveness, 1980, 0.95).
narrative_ontology:measurement(chil_be_t2005, child_marriage, base_extractiveness, 2005, 0.9).
narrative_ontology:measurement(chil_be_t2030, child_marriage, base_extractiveness, 2030, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(child_marriage, enforcement_mechanism).
narrative_ontology:affects_constraint(child_marriage, poverty_cycle).
narrative_ontology:affects_constraint(child_marriage, lack_of_female_education).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
