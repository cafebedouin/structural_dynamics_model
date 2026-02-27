% ============================================================================
% CONSTRAINT STORY: grete_samsa_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_grete_samsa_transition, []).

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
 *   constraint_id: grete_samsa_transition
 *   human_readable: Grete's Burden and Ascendance in The Metamorphosis
 *   domain: social/economic
 *
 * SUMMARY:
 *   The Metamorphosis presents a constraint that emerges from the asymmetry
 *   between Gregor's complete physical and economic dependence following his
 *   transformation and Grete's duty to sustain him. This constraint is not
 *   imposed externally by a state or institution but generated internally by
 *   family structure, filial obligation, and the absence of alternatives.
 *   Grete's initial role as her brother's caretaker evolves into a position
 *   of household authority, yet the underlying extraction mechanism persists:
 *   she bears the labor, foregoes her own development, and faces suppression
 *   both through external family demands and through her internalized sense
 *   of duty. The constraint exhibits tangled rope characteristics because it
 *   solves a genuine problem (family survival, Gregor's care) while
 *   simultaneously extracting from Grete in an asymmetric and coercive
 *   manner. Her transition from pure victim to family authority does not
 *   resolve the underlying extraction — it transforms it into a more subtle
 *   form where she exercises agency within a deeply constrained field.
 *
 * KEY AGENTS:
 *   - Grete Samsa: Primary victim (powerless/trapped) — bears the full burden of Gregor's care; her individual development, education, courtship prospects are suppressed indefinitely with no exit option
 *   - Gregor Samsa (post-transformation): Secondary victim and dependent (powerless/trapped) — entirely dependent on Grete's labor; his condition creates the extraction mechanism that targets Grete
 *   - The Samsa Family Collective: Primary beneficiary (moderate/constrained) — benefits from Grete's caretaking; enforces her obligation through family duty and implicit guilt; has constrained exit from the arrangement (could hire help but chooses not to)
 *   - The Samsa Parents: Institutional actors (institutional/arbitrage) — delegate caretaking to Grete while maintaining authority; have the option to seek external care but do not; benefit from Grete's labor while bearing minimal burden themselves
 *   - The Bourgeois Family Institution: Institutional beneficiary (institutional/arbitrage) — the 19th-century family structure as an organizing principle benefits from the naturalization of Grete's role; has arbitrage options (external care, institutional placement) but does not exercise them
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a hybrid coordination-extraction mechanism whose theater increases over time as genuine necessity declines but cultural obligation persists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(grete_samsa_transition, 0.58).
domain_priors:suppression_score(grete_samsa_transition, 0.68).
domain_priors:theater_ratio(grete_samsa_transition, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(grete_samsa_transition, extractiveness, 0.58).
narrative_ontology:constraint_metric(grete_samsa_transition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(grete_samsa_transition, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(grete_samsa_transition, tangled_rope).
narrative_ontology:human_readable(grete_samsa_transition, "Grete's Burden and Ascendance in The Metamorphosis").
narrative_ontology:topic_domain(grete_samsa_transition, "social/economic").

domain_priors:requires_active_enforcement(grete_samsa_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(grete_samsa_transition, samsa_family_survival).
narrative_ontology:constraint_victim(grete_samsa_transition, grete_individual_development).
narrative_ontology:constraint_victim(grete_samsa_transition, gregor_dependent_status).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Grete's individual aspirations and trajectory are suppressed by her obligation to care for the transformed Gregor. She cannot exit her filial duty; her education, courtship prospects, and personal growth are deferred indefinitely. The constraint extracts her labor, time, and potential without compensation or escape route. Maximum experienced extraction for this agent.
constraint_indexing:constraint_classification(grete_samsa_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Gregor becomes utterly dependent on Grete's care without reciprocal obligation. He cannot consent to or refuse the caretaking arrangement; he is immobilized and mute. His needs directly extract Grete's labor, creating a secondary victim status — Gregor is trapped not just by his physical form but by the dependency structure that his condition imposes on Grete.
constraint_indexing:constraint_classification(grete_samsa_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The family as a unit benefits from Grete's caretaking — she sustains the household's survival and moral coherence. The family also extracts her labor coercively: she has no alternative (constrained exit), and the family enforces her duty through guilt, obligation, and implicit threat of abandonment. This is both coordination (family survival) and asymmetric extraction (her burden).
constraint_indexing:constraint_classification(grete_samsa_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% From the perspective of 19th-century family structure as an institution, Grete's role is a coordinating mechanism: filial duty binds family members together and allocates caretaking responsibilities. The constraint is experienced as natural family order, not as extraction. The institution has exit options (hiring external care, institutionalization) but does not use them, treating Grete's obligation as the proper solution.
constraint_indexing:constraint_classification(grete_samsa_transition, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% By the story's end, Grete begins to exercise agency within her constraint — she becomes the locus of decision-making about the household and Gregor's fate. Her emergence as the family's decision-maker (evident in her silencing of her parents' objections to Gregor's eviction) suggests a sunset clause: her caretaking burden is transformed into a form of authority. The theater of filial duty begins to decline as genuine organizational responsibility emerges. The constraint has an endpoint — though ambiguous, Grete's ascendance implies the extraction mechanism is dissolving.
constraint_indexing:constraint_classification(grete_samsa_transition, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% From the broadest perspective, Grete's obligation embodies a social convention (female filial duty, women as caregivers) that has degraded into pure theater by Kafka's era. The convention persists through performative invocation (the family appeals to duty, morality, propriety) but no longer serves a genuine coordination function — 1910s Prague has alternatives to family-based care. The theater ratio is high because the language of duty is extensive while the actual functional necessity is limited. Piton classification reflects institutional inertia.
constraint_indexing:constraint_classification(grete_samsa_transition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% The analytical observer sees the constraint as genuinely hybrid: it performs a coordination function (family survival, moral order) while simultaneously extracting from Grete in an asymmetric and coercive manner. Her suppression is high (she cannot refuse), her extraction is significant (she bears the full burden), yet the family genuinely depends on her labor. The constraint persists because it solves a real problem (who cares for Gregor?) while hiding the asymmetry (by naturalizing female duty). This perspective classifies as tangled rope for all three canonical reasons: coordination + extraction + active enforcement.
constraint_indexing:constraint_classification(grete_samsa_transition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(grete_samsa_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(grete_samsa_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(grete_samsa_transition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(grete_samsa_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(grete_samsa_transition, TR),
    TR >= 0.70.

:- end_tests(grete_samsa_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Grete's labor is extracted without compensation, her personal development is deferred, her exit options are eliminated by filial obligation. However, the extraction is not maximal (snare-level ≥0.66) because the family's need for her care is genuine — she is not pure slave labor but is solving a real problem. The constraint occupies the tangled_rope range because it combines genuine coordination necessity with clear asymmetric extraction. Suppression (0.68): Moderate-high. Grete faces multiple suppressions: she cannot refuse caretaking without violating filial duty; she has no institutional support; she has no economic means to hire help; she faces implicit familial sanctions (guilt, abandonment threat) for non-compliance. However, suppression is not absolute because she retains some agency (she chooses to care, at least partly through her own values), and the text shows her exercising voice and decision-making authority by the story's end. Theater ratio (0.61): Moderate-high. The language and justification of Grete's obligation is extensive (filial duty, family morality, propriety) but the actual functional necessity of her caretaking is lower than the rhetoric suggests. By 1910s Prague, the family has economic options (hiring help) that they do not exercise, instead invoking the theater of natural duty. The theater increases over the interval as Grete's caretaking becomes more routine and less obviously necessary, yet the rhetoric of duty persists.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a profound perspectival gap between Grete's lived experience and the family's institutional framing. From Grete's perspective (powerless/trapped), the constraint is a Snare — she experiences pure extraction, her labor flows outward without reciprocal benefit, and she has no exit. From the family's perspective (moderate/constrained), the constraint is a Tangled Rope — it solves a genuine problem (Gregor's care) while extracting from Grete, and they experience themselves as both dependent on her and entitled to demand her labor. From the institutional perspective (institutional/arbitrage), the constraint is a Rope — it is a natural and proper allocation of caretaking duty; the institution sees no extraction because it naturalizes the role. From the analytical civilizational view, the constraint is Tangled Rope with piton features — it genuinely coordinates family survival, but the theater of duty has increased as actual economic necessity has declined. The scaffold perspective (Grete's emerging agency and ascendance) is the most contested — it is unclear whether her final authority represents genuine transformation of the constraint or a more subtle form of the same extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Grete's experienced directionality (d) is derived from her status as both a structural victim (she bears costs) and a member of a constrained collective (she has modest agency within the family). The engine computes d from her victim status + trapped exit options, placing her at d ≈ 0.85-0.95, yielding high f(d) and thus high experienced extraction chi. Her power atom (powerless) reflects her inability to unilaterally exit the constraint — she is economically dependent, socially vulnerable, and bound by internalized duty. The family's directionality as a moderate agent with constrained exit is lower (d ≈ 0.50-0.60) because they benefit from her caretaking while facing some costs (resource expenditure, moral responsibility). By the story's end, Grete's agency increases (she makes binding decisions), which should lower her d slightly, but she remains trapped in the structural role — her ascendance is authority within constraint, not escape from it. The suppression value (0.68) reflects that her options for exit are heavily constrained by economic dependence, social norms, and filial obligation, all of which limit her ability to negotiate terms.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVING MANDATROPHY — Family vs Extraction: The core mandatrophy is whether Grete's role is a coordination mechanism (natural family allocation of labor) or a pure extraction mechanism (coercive assignment of burden without consent or reciprocal benefit). The tangled_rope classification resolves this by showing that BOTH are true: the constraint genuinely coordinates family survival (beneficiaries = samsa_family_survival), yet it simultaneously extracts from Grete (victims = grete_individual_development, gregor_dependent_status) through suppression and asymmetry. The cannon classifications require active enforcement (true — the family enforces duty through guilt and social obligation), beneficiaries (true — the family survives), and victims (true — Grete's development is deferred, Gregor's dependency becomes an extractive mechanism). NATURAL LAW FALLACY: The analytical observer risks reading the constraint as a natural law (mountain) — 'families have always allocated caretaking this way; it is inherent to human social structure.' But the structural data contradicts this: the constraint is contingent on specific economic conditions (the family's narrow margin, the absence of institutional alternatives), on social conventions (female filial duty) that have changed historically, and on the family's choice not to seek external care. The mountain classification fails on the accessibility_collapse metric — the family could cease the constraint by hiring external caregivers; it is not physically or logically impossible. THEATER PARADOX: The theater_ratio (0.61) reveals that the constraint persists partly through performative invocation of duty rather than pure necessity. The gap between rhetoric (duty is binding, the family cannot afford alternatives, Grete's caretaking is the only solution) and structural reality (the family has some economic flexibility, external care exists as an option, the obligation is enforced through guilt rather than force) indicates piton features. However, the constraint is not purely piton because genuine need underlies it — unlike a pure piton, the constraint solves a real problem. GRETE'S ASCENDANCE: The final twist is Grete's emergence as the household decision-maker and her apparent liberation from pure victim status. The scaffold classification captures this as a sunset — her caretaking burden is transformed into authority, suggesting the constraint has an endpoint. But this outcome is structurally ambiguous: does her authority represent genuine escape from the constraint (sunset), or does it represent her internalization and naturalization of it (deeper entrapment)? The omega variables address this uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    family_survival_alternative,
    'Could the Samsa family have survived without Grete''s caretaking? What were the actual alternatives (external hired care, institutional placement, community support)?',
    'Historical analysis of 1910s Prague working-class household economics; comparison of Samsa family income to actual cost of hired caregiving or institutional placement; examination of what alternatives the text presents or forecloses',
    'If alternatives existed: the constraint is pure extraction disguised as necessity (Snare from Grete''s perspective strengthens). If no alternatives existed: the constraint is genuine coordination necessity (Rope perspective legitimacy increases). Classification could shift from tangled_rope to pure snare depending on necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_survival_alternative, empirical, 'Whether family survival required Grete''s caretaking or alternatives existed').

omega_variable(
    grete_ascendance_agency_threshold,
    'Does Grete''s final ascendance (her decision-making role, her parents'' deference to her judgment about Gregor''s eviction) represent genuine agency or a performative illusion that masks deepened obligation?',
    'Textual analysis of Grete''s decision-making authority pre- and post-Gregor''s death; examination of whether her authority is genuine delegation or implicit coercion (her parents no longer contest her because she has already decided unilaterally); analysis of her emotional state and expressed desires at the story''s conclusion',
    'If genuine agency: the scaffold classification is accurate — her caretaking burden is transformed into a form of power with real agency. Sunset clause is structural. If performative: her ascendance is a theater of empowerment masking continued extraction; the constraint transitions to piton rather than scaffold, and the sunset clause is illusory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grete_ascendance_agency_threshold, conceptual, 'Whether Grete''s ascendance represents genuine agency or performative illusion').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is Grete''s suppression enforced by external coercion (family demands, social expectation) versus internalized guilt and filial duty (her own values)?',
    'Textual analysis of Grete''s internal monologue and emotional responses; identification of moments where she experiences external pressure versus moments where she volunteers her effort; examination of whether she ever questions her duty or fantasizes about escape',
    'If predominantly external coercion: suppression score (0.68) is appropriate, and the constraint is more extractive than she has internalized. If predominantly internalized: her suppression may be self-imposed, and the classification should shift — she experiences the constraint as duty rather than extraction, potentially moving toward rope or scaffold. The directionality (her experienced d value) would shift downward if her consent is genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, conceptual, 'Whether Grete''s suppression is externally coerced or self-imposed through internalized duty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(grete_samsa_transition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grete_tr_t0, grete_samsa_transition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(grete_tr_t25, grete_samsa_transition, theater_ratio, 25, 0.58).
narrative_ontology:measurement(grete_tr_t50, grete_samsa_transition, theater_ratio, 50, 0.61).

% Extraction over time
narrative_ontology:measurement(grete_be_t0, grete_samsa_transition, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(grete_be_t25, grete_samsa_transition, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(grete_be_t50, grete_samsa_transition, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(grete_samsa_transition, resource_allocation).
narrative_ontology:affects_constraint(grete_samsa_transition, gregor_transformation_metamorphosis).

% DUAL FORMULATION NOTE:
% Grete's constraint is downstream of Gregor's physical transformation (the triggering event), but it represents a distinct structural constraint on her agency and development. The upstream constraint (Gregor's metamorphosis and biological dependence) has its own ε reflecting the force and inevitability of his condition; Grete's constraint has its own ε reflecting the family's choice to assign caretaking to her without external alternatives. These are linked but distinct structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(grete_samsa_transition, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
