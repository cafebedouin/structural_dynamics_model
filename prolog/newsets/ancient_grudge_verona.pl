% ============================================================================
% CONSTRAINT STORY: ancient_grudge_verona
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
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
 *   The 'ancient grudge' between the houses of Montague and Capulet is a
 *   transgenerational social constraint that mandates conflict and suppresses
 *   affiliation in Renaissance Verona. Its origins are forgotten, but its
 *   enforcement is immediate and lethal, structuring social life around
 *   arbitrary enmity. The feud functions as a powerful, self-perpetuating
 *   cycle of violence that ultimately consumes its own children,
 *   demonstrating how a social construct can attain the force of a natural
 *   law for those trapped within it.
 *
 * KEY AGENTS:
 *   - Younger Generation (Romeo, Juliet, Tybalt): Primary victims and enforcers (powerless/moderate, trapped/constrained)
 *   - Family Patriarchs (Lords Montague & Capulet): Primary beneficiaries of the honor system (institutional/arbitrage)
 *   - The Prince of Verona: Institutional authority whose power is undermined by the feud (institutional/constrained)
 *   - Friar Laurence: External organized agent attempting to build a temporary resolution (organized/constrained)
 *   - Citizens of Verona: Collateral victims of the public violence (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancient_grudge_verona, 0.75).
domain_priors:suppression_score(ancient_grudge_verona, 0.8).
domain_priors:theater_ratio(ancient_grudge_verona, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancient_grudge_verona, extractiveness, 0.75).
narrative_ontology:constraint_metric(ancient_grudge_verona, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(ancient_grudge_verona, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancient_grudge_verona, tangled_rope).
narrative_ontology:human_readable(ancient_grudge_verona, "The Montague-Capulet Feud").
narrative_ontology:topic_domain(ancient_grudge_verona, "social/political").

domain_priors:requires_active_enforcement(ancient_grudge_verona).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ancient_grudge_verona, family_patriarchs).
narrative_ontology:constraint_beneficiary(ancient_grudge_verona, concept_of_familial_honor).
narrative_ontology:constraint_victim(ancient_grudge_verona, younger_generation_members).
narrative_ontology:constraint_victim(ancient_grudge_verona, citizens_of_verona).
narrative_ontology:constraint_victim(ancient_grudge_verona, the_princes_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LOVERS (SNARE) — For Romeo and Juliet, the feud is a pure extraction mechanism. They are trapped by their family identities, and any attempt to build an alternative life is suppressed, leading directly to their deaths. The constraint extracts their future, their love, and their lives. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.85.
constraint_indexing:constraint_classification(ancient_grudge_verona, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PATRIARCHS (ROPE) — From the perspective of the heads of house, the feud functions as a costly but effective coordination mechanism. It enforces in-group loyalty, solidifies family identity against an external other, and provides a framework for maintaining honor. They possess the agency to escalate or de-escalate (arbitrage), externalizing the costs onto the younger generation. d≈0.05, f(d)≈-0.12, σ=0.8 → χ≈-0.07. Negative extraction indicates a net perceived benefit.
constraint_indexing:constraint_classification(ancient_grudge_verona, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE LOYAL KINSMAN (TANGLED ROPE) — For an agent like Tybalt, the feud is both a source of identity (coordination) and a mandate for violence (extraction). He is constrained to participate to uphold his honor, but also actively enforces the feud's rules. He experiences both its cohesive and destructive functions simultaneously. d≈0.55, f(d)≈0.75, σ=0.8 → χ≈0.45.
constraint_indexing:constraint_classification(ancient_grudge_verona, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: THE PRINCE (PITON) — The Prince sees the feud as a degraded institution. Its original purpose is forgotten ('ancient grudge'), and it now functions primarily as performative, pointless violence that undermines his authority and civil peace. The high theater_ratio (0.75) and its persistence despite his decrees mark it as a Piton—a system maintained by institutional inertia within the families, not by any remaining legitimate function. His exit is constrained; he cannot abandon his city.
constraint_indexing:constraint_classification(ancient_grudge_verona, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE FRIAR (SCAFFOLD) — Friar Laurence attempts to construct a temporary solution to bypass the feud. His plan—the secret marriage, the sleeping potion—is a scaffold designed to support Romeo and Juliet until the feud can be resolved and the structure dismantled. It has a clear, if tragic, sunset clause. He acts as an organized agent trying to build a temporary bridge over a structural conflict.
constraint_indexing:constraint_classification(ancient_grudge_verona, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: THE TRAGIC THEORIST (MOUNTAIN) — A detached, civilizational observer might frame the feud as an immutable law of human tribalism or a tragic flaw inherent to social structures. This perspective naturalizes the conflict, viewing it as an unchangeable feature of the human condition. The engine will flag this as a false summit, as the feud is a contingent social construct, not a physical law, evidenced by its eventual resolution.
constraint_indexing:constraint_classification(ancient_grudge_verona, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancient_grudge_verona_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ancient_grudge_verona, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_grudge_verona, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.75) is severe, as the feud directly extracts lives, happiness, and the future of the next generation. Suppression (0.80) is extremely high; the social cost of defying the feud is total ostracization or death, leaving no viable alternatives for members of the houses. Theater Ratio (0.75) is high because the original grievance is lost to time, and the conflict persists as a series of performative honor-defenses and public brawls rather than a dispute over a tangible resource.
 *
 * PERSPECTIVAL GAP:
 *   The feud is a diagnostic exemplar. For the lovers, it is a pure Snare, trapping and killing them. For the patriarchs, it is a Rope of social coordination, enforcing loyalty. For Tybalt, it is a Tangled Rope, a source of both identity and fatal obligation. For the Prince, it is a Piton, a decayed and functionless ritual of violence he cannot stop. For Friar Laurence, it is a problem to be solved with a temporary Scaffold. For a distant observer, it can be misread as a Mountain of human nature. The vast gap between these perspectives, all derived from the same base metrics, reveals the indexical nature of the classification system.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the abstract concept of 'honor' and the patriarchs who are its stewards; they experience the feud as a tool for maintaining social order and identity, giving them a low directionality (d). The victims are the younger generation and the city's populace, who bear the full, lethal costs of the conflict, giving them a high directionality (d). This asymmetry is the core engine of the tragedy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that a single, highly extractive phenomenon can legitimately occupy all six classifications depending on the observer's structural position. The 'correct' classification is the full set of perspectives. Labeling the feud as only a 'Snare' would miss its function as a 'Rope' for the patriarchs, and labeling it only a 'Rope' would erase the lethal extraction experienced by the youth. The system correctly models this complex reality without collapsing it to a single, inadequate label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_of_grudge,
    'What was the original cause of the ''ancient grudge''?',
    'Historical/textual analysis to determine if the feud began from a material dispute (e.g., land, wealth) versus a symbolic one (e.g., honor, insult).',
    'A material origin would suggest a more rational, though prolonged, Tangled Rope. A purely symbolic and forgotten origin reinforces the Piton classification, where the function has completely atrophied into theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(origin_of_grudge, conceptual, 'Whether the feud''s origin was material or purely symbolic').

omega_variable(
    princes_power_limit,
    'Was the Prince''s inability to stop the feud a failure of political will or a structural lack of enforcement power against noble houses?',
    'Analysis of the political structure of Renaissance Verona to determine the actual balance of power between the Prince and the major families.',
    'If the Prince had the power but not the will, the feud is a Snare maintained by elite impunity. If he structurally lacked the power, the feud itself acts as a competing sovereign, closer to a Mountain of local politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(princes_power_limit, empirical, 'The structural limit of the Prince''s authority vs noble power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancient_grudge_verona, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anci_tr_t0, ancient_grudge_verona, theater_ratio, 0, 0.3).
narrative_ontology:measurement(anci_tr_t50, ancient_grudge_verona, theater_ratio, 50, 0.6).
narrative_ontology:measurement(anci_tr_t100, ancient_grudge_verona, theater_ratio, 100, 0.75).

% Extraction over time
narrative_ontology:measurement(anci_be_t0, ancient_grudge_verona, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(anci_be_t50, ancient_grudge_verona, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(anci_be_t100, ancient_grudge_verona, base_extractiveness, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ancient_grudge_verona, enforcement_mechanism).
narrative_ontology:affects_constraint(ancient_grudge_verona, veronese_civil_order).
narrative_ontology:affects_constraint(ancient_grudge_verona, patriarchal_marriage_contracts).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
