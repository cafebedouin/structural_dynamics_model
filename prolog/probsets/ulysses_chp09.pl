% ============================================================================
% CONSTRAINT STORY: ulysses_chp09
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp09, []).

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
 *   constraint_id: ulysses_chp09
 *   human_readable: The Hamlet Algebra (National Library)
 *   domain: social/religious/philosophical
 *
 * SUMMARY:
 *   Stephen Dedalus's elaborate "Hamlet" theory in the National Library is a
 *   constraint that reflects the tensions between rigorous intellectual
 *   analysis (Scylla) and speculative interpretation (Charybdis). While the
 *   theory can stimulate intellectual discourse, it also carries the risk of
 *   misleading those who lack the critical tools to evaluate its claims. The
 *   library functions as a container for this exchange, while the analytical
 *   observer recognizes the broader implications for knowledge and its
 *   accessibility.
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Elaborator of the "Hamlet" theory
 *   - The National Library: The physical and institutional space where the theory is presented.
 *   - Naive Listeners: Those who may be persuaded without a clear understanding of its claims.
 *   - Other Intellectuals: Constrained to respond and participate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp09, 0.45).
domain_priors:suppression_score(ulysses_chp09, 0.35).
domain_priors:theater_ratio(ulysses_chp09, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp09, extractiveness, 0.45).
narrative_ontology:constraint_metric(ulysses_chp09, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ulysses_chp09, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp09, tangled_rope).
narrative_ontology:human_readable(ulysses_chp09, "The Hamlet Algebra (National Library)").
narrative_ontology:topic_domain(ulysses_chp09, "social/religious/philosophical").

domain_priors:requires_active_enforcement(ulysses_chp09).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp09, intellectual_discourse).
narrative_ontology:constraint_victim(ulysses_chp09, naive_listeners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The naive listener, lacking the critical tools to analyze Stephen's theory, may be trapped by its apparent complexity and persuasiveness. Unable to easily exit the discourse, they are susceptible to accepting the theory without proper scrutiny.
constraint_indexing:constraint_classification(ulysses_chp09, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The National Library benefits from hosting intellectual discourse, regardless of the specific content. It's a space for ideas to be exchanged, and it can easily arbitrate different viewpoints and discussions, extracting from all positions equally without preference.
constraint_indexing:constraint_classification(ulysses_chp09, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical observer can appreciate both the potential value of Stephen's insights and the potential pitfalls of overly complex, ungrounded theorizing. From a civilizational perspective, the tension between rigorous analysis (Scylla) and creative interpretation (Charybdis) is fundamental to intellectual progress.
constraint_indexing:constraint_classification(ulysses_chp09, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Other intellectuals may take temporary refuge within this space, constrained to follow and respond to the rhetoric employed, benefiting in the short-term from participating, before then moving on once the session concludes.
constraint_indexing:constraint_classification(ulysses_chp09, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp09_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp09, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp09, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(ulysses_chp09_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the potential for Stephen's theory to capture the attention and intellectual energy of those present, while the suppression (0.35) captures the limited freedom of those who are forced to respond and respond with scrutiny. The theater ratio is limited to .20, reflecting more substance than pure posturing.
 *
 * PERSPECTIVAL GAP:
 *   The naive listener sees a Snare because they lack agency, whereas The analytical observer may see the event as a generative source of knowledge. The Library functions as an easily arbitrated space for intellectual debate. While other intellectuals are required to respond, constrained to the discourse but still able to leave when they choose.
 *
 * DIRECTIONALITY LOGIC:
 *   Stephen as an intellectual is more likely to benefit from the discourse (0.05) as a person who has organized and created the theory. The National Library benefits from the intellectual energy brought to the Library, but also extracts from it, as this activity occurs within its building (0.5). Finally, the naive listener receives little benefit as they are exposed to complex discourse and may be misled.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjectivity_vs_objectivity,
    'To what extent is Stephen''s theory a product of his own subjectivity, and to what extent does it reflect objective truths about Shakespeare and Hamlet?',
    'Compare Stephen''s theory with other Shakespearean interpretations; analyze the historical and biographical evidence for and against his claims.',
    'If largely subjective, the theory is primarily valuable as a window into Stephen''s mind. If objectively grounded, it could represent a significant contribution to Shakespearean scholarship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subjectivity_vs_objectivity, conceptual, 'The balance between subjective interpretation and objective analysis in Stephen''s theory').

omega_variable(
    accessibility_of_knowledge,
    'Is the pursuit of esoteric knowledge inherently elitist, or can it be made accessible to a broader audience?',
    'Assess the degree to which Stephen''s theory requires specialized knowledge to understand; explore alternative ways of communicating complex ideas.',
    'If elitist, it reinforces existing power structures. If accessible, it promotes intellectual democratization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_of_knowledge, preference, 'The question of whether knowledge should be exclusive or freely accessible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp09, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp09, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ulys_tr_t5, ulysses_chp09, theater_ratio, 5, 0.2).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp09, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp09, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ulys_be_t5, ulysses_chp09, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp09, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp09, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
