% ============================================================================
% CONSTRAINT STORY: iran_hijab_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_hijab_law, []).

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
 *   constraint_id: iran_hijab_law
 *   human_readable: Mandatory Hijab Law in Iran
 *   domain: political/social/religious
 *
 * SUMMARY:
 *   The mandatory hijab law in the Islamic Republic of Iran, enforced by the
 *   state's "morality police" (Gasht-e Ershad), is a complex constraint with
 *   varying impacts on different groups. It represents a significant
 *   restriction on women's freedom and autonomy, while also serving as a
 *   symbol of the theocratic regime's authority. The theater_ratio has
 *   increased over time as resistance has grown, leading to more performative
 *   enforcement.
 *
 * KEY AGENTS:
 *   - Iranian Women: Primary victim (powerless/trapped) - faces legal and social repercussions for non-compliance.
 *   - Theocratic Establishment: Primary beneficiary (institutional/constrained) - maintains ideological control but faces increasing resistance.
 *   - Iranian Diaspora: Organized agent (organized/mobile) - advocates for change internationally.
 *   - Male Guardians: Beneficiary (moderate/constrained) - enforce the law but are constrained by societal expectations.
 *   - Cultural Expression: Victim (powerless/trapped) - stifled due to restrictions on personal appearance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_hijab_law, 0.75).
domain_priors:suppression_score(iran_hijab_law, 0.85).
domain_priors:theater_ratio(iran_hijab_law, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_hijab_law, extractiveness, 0.75).
narrative_ontology:constraint_metric(iran_hijab_law, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(iran_hijab_law, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_hijab_law, snare).
narrative_ontology:human_readable(iran_hijab_law, "Mandatory Hijab Law in Iran").
narrative_ontology:topic_domain(iran_hijab_law, "political/social/religious").

domain_priors:requires_active_enforcement(iran_hijab_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_hijab_law, theocratic_establishment).
narrative_ontology:constraint_beneficiary(iran_hijab_law, male_guardians).
narrative_ontology:constraint_victim(iran_hijab_law, iranian_women).
narrative_ontology:constraint_victim(iran_hijab_law, cultural_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Iranian women face significant restrictions and penalties for non-compliance, limiting their freedom and autonomy. Trapped within the legal system and social pressures.
constraint_indexing:constraint_classification(iran_hijab_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The theocratic establishment, while nominally benefiting from the law, finds that its legitimacy erodes over time as resistance grows and enforcement becomes increasingly theatrical and performative. Constrained exit as abandoning the law would threaten the regime's ideology.
constraint_indexing:constraint_classification(iran_hijab_law, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The Iranian diaspora benefits from advocating for change but are also affected by the law's impact on their homeland and family members. They have the mobility to voice dissent and organize internationally. Tangled Rope classification reflects the dual nature of their influence - advocacy benefits and emotional distress due to homeland repression.
constraint_indexing:constraint_classification(iran_hijab_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Benefits from enforcing control over female relatives but are constrained by societal expectations and legal obligations. However, increased freedom for women could also benefit men by freeing them from this restrictive obligation.
constraint_indexing:constraint_classification(iran_hijab_law, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, the law is a complex interplay of cultural values, religious beliefs, and political control, exhibiting both coordination (social cohesion) and extraction (suppression of individual freedom), thus fitting the tangled rope.
constraint_indexing:constraint_classification(iran_hijab_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_hijab_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_hijab_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_hijab_law, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_hijab_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_hijab_law, TR),
    TR >= 0.70.

:- end_tests(iran_hijab_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The law significantly restricts women's freedom, autonomy, and self-expression. Suppression (0.85): High. The law is strictly enforced with penalties ranging from fines to imprisonment. Theater Ratio (0.75): High. While there is genuine enforcement, the increasing resistance to the law leads to a performative aspect in its implementation. The morality police must be seen to be enforcing the law, even if it means increased public resentment.
 *
 * PERSPECTIVAL GAP:
 *   Iranian women experience the law as a snare, limiting their freedom and autonomy. The theocratic establishment sees it as a means of maintaining control, although its legitimacy erodes over time. The Iranian diaspora views it as a human rights violation and advocates for change. Male guardians benefit from enforcing control but are constrained by societal expectations. The analytical observer sees it as a tangled rope due to the interplay of cultural, religious, and political factors.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the agent's structural position relative to the constraint. Iranian women, as the primary victims, experience the highest level of extraction. The theocratic establishment benefits from the law but is also constrained by its consequences. The diaspora benefits from advocating for change but is emotionally distressed by the situation. Male guardians experience a mix of benefit and constraint. The analytical observer considers the broader implications of the law.
 *
 * MANDATROPHY ANALYSIS:
 *   The analysis resolves the mandatrophy by highlighting that the same law can be perceived differently based on the observer's perspective and structural position. What may appear as a form of social cohesion from one perspective is a form of oppression from another. The tangled rope classification, combined with the nuanced perspectives, provides a more complete understanding of the law's impact. The high extractiveness is justified due to the severe restrictions on women's lives, and the mandatrophy is resolved by acknowledging the varying perspectives and the increasing performative nature of enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_genuine_belief,
    'To what extent is the mandatory hijab law genuinely supported by a significant portion of the Iranian population, beyond the ruling elite?',
    'Sociological surveys, analysis of public discourse, and historical trends in social attitudes.',
    'If widespread support, the law may be considered a rope from some perspectives. If limited support, it reinforces the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_genuine_belief, empirical, 'The degree of popular support for the mandatory hijab law.').

omega_variable(
    enforcement_severity_threshold,
    'What level of enforcement severity (e.g., fines, imprisonment) triggers widespread social unrest and undermines the law''s legitimacy?',
    'Historical analysis of protest movements, government responses, and social media trends.',
    'Identifying this threshold could indicate points of instability or potential for reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_severity_threshold, empirical, 'The point at which law enforcement triggers social backlash').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_hijab_law, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_tr_t0, iran_hijab_law, theater_ratio, 0, 0.6).
narrative_ontology:measurement(iran_tr_t10, iran_hijab_law, theater_ratio, 10, 0.7).
narrative_ontology:measurement(iran_tr_t20, iran_hijab_law, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(iran_be_t0, iran_hijab_law, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(iran_be_t10, iran_hijab_law, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(iran_be_t20, iran_hijab_law, base_extractiveness, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_hijab_law, enforcement_mechanism).
narrative_ontology:affects_constraint(iran_hijab_law, gender_equality).
narrative_ontology:affects_constraint(iran_hijab_law, freedom_of_expression).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
