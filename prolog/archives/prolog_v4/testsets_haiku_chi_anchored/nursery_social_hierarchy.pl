% ============================================================================
% CONSTRAINT STORY: nursery_social_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nursery_social_hierarchy, []).

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
 *   constraint_id: nursery_social_hierarchy
 *   human_readable: Social Hierarchy of Playthings in the Nursery
 *   domain: social/economic
 *
 * SUMMARY:
 *   Within the modern nursery, a status-based framework has crystallized
 *   around mechanical complexity: toys that beep, flash, respond to input, or
 *   exhibit apparent autonomy occupy a legitimized social stratum, while
 *   simple toys—blocks, dolls, balls, ring-stackers—are devalued and
 *   excluded. This constraint extracts dignity and play-value from simple
 *   objects to fuel the social dominance of technical toys. The extraction
 *   mechanism operates through multiple channels: (1) marketing systems that
 *   frame complex toys as 'developmentally superior,' (2) parental
 *   conditioning that equates mechanical responsiveness with educational
 *   value, (3) peer comparison within nurseries where children observe adult
 *   status-signaling around toy sophistication, and (4)
 *   theater—pseudo-scientific claims about cognitive engagement that mask
 *   pure status allocation. The constraint exhibits high suppression (0.68):
 *   simple toys are actively gatekept through messaging (parents steering
 *   away from 'overstimulating play'), institutional messaging (daycare
 *   facilities acquiring expensive electronics), and the child's internalized
 *   hierarchy (the child learns that imagination is insufficient; the toy
 *   must be complex to be worthy). The theater ratio (0.64) reflects how the
 *   hierarchy persists through claims about 'developmental stages,' 'fine
 *   motor skills,' and 'STEM engagement' that are largely retrospective
 *   justifications rather than forward-looking developmental science.
 *   Extractiveness has increased over the 50-year measurement interval (0.28
 *   → 0.52) as manufacturing and marketing systems have intensified, while
 *   theater has risen even faster (0.35 → 0.64) as the pseudo-scientific
 *   framing has calcified. The constraint represents a structural extraction
 *   from the domain of human imagination: simple toys have intrinsic
 *   developmental value (open-ended play, narrative agency, creative agency),
 *   but this value is systematically suppressed and reallocated to technical
 *   toys that offer apparent responsiveness at the cost of imagination.
 *
 * KEY AGENTS:
 *   - Simple Toys (wooden blocks, cloth dolls, ring-stackers): Primary victims (powerless/trapped) — bear the extraction cost through devaluation and exclusion
 *   - Child's Imaginative Capacity: Primary victim (powerless/trapped) — suppressed by the constraint's logic that imagination is insufficient without mechanical responsiveness
 *   - Technical Toy Manufacturers: Primary beneficiary (institutional/arbitrage) — benefit from status elevation and premium pricing
 *   - Consumer Marketing Systems: Secondary beneficiary (institutional/arbitrage) — benefit from the hierarchy as a coordination mechanism for demand creation
 *   - Parent Consumer: Mixed actor (moderate/constrained) — enforcer of hierarchy on child; trapped in consumption patterns justified by hierarchy's pseudo-scientific framing
 *   - Developmental Psychology Establishment: Institutional actor (institutional/arbitrage) — maintains piton: uses degraded legitimacy through appeals to 'developmental science' that is largely theater
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — risks naturalizing contingent commercial arrangement as inherent human preference
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nursery_social_hierarchy, 0.52).
domain_priors:suppression_score(nursery_social_hierarchy, 0.68).
domain_priors:theater_ratio(nursery_social_hierarchy, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nursery_social_hierarchy, extractiveness, 0.52).
narrative_ontology:constraint_metric(nursery_social_hierarchy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nursery_social_hierarchy, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nursery_social_hierarchy, snare).
narrative_ontology:human_readable(nursery_social_hierarchy, "Social Hierarchy of Playthings in the Nursery").
narrative_ontology:topic_domain(nursery_social_hierarchy, "social/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nursery_social_hierarchy, technical_toy_manufacturers).
narrative_ontology:constraint_beneficiary(nursery_social_hierarchy, consumer_marketing_systems).
narrative_ontology:constraint_victim(nursery_social_hierarchy, simple_toys).
narrative_ontology:constraint_victim(nursery_social_hierarchy, child_imaginative_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SIMPLE TOY (SNARE) — Cannot exit the hierarchy; its 'failure' to be complex is permanent. The wooden block, the cloth doll, the simple ring-stacker bear the full extraction cost: devaluation, exclusion from play, and relegation to the nursery basement. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(nursery_social_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CHILD'S IMAGINATIVE AGENCY (SNARE) — The constraint suppresses the child's own capacity to project narrative and meaning onto simple objects. The child is trapped in the hierarchy's logic: if the toy does not beep, flash, or move mechanically, the child has internalized that imagination is insufficient. Theater ratio (0.64) reflects pseudo-educational claims about 'developmental engagement' that mask status extraction. d≈0.88, f(d)≈1.32, σ=0.8 → χ≈0.55.
constraint_indexing:constraint_classification(nursery_social_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: TECHNICAL TOY MANUFACTURER (ROPE) — Experiences the constraint as coordination. The hierarchy ensures that their products occupy the premium position; coordinating with marketing systems and consumer expectation-shaping creates durable demand. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(nursery_social_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PARENT CONSUMER (TANGLED ROPE) — Constrained by desire to provide 'the best' for their child (enforced by marketing and peer comparison) but also benefits from the status coordination within parenting communities. The parent is both enforcer and victim: they enforce the hierarchy on their child while being trapped in consumption patterns justified by the hierarchy's logic. d≈0.58, f(d)≈0.78, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(nursery_social_hierarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEVELOPMENTAL PSYCHOLOGY APPARATUS (PITON) — The constraint is maintained through appeals to 'developmental stages,' 'cognitive engagement,' and 'fine motor skills' — pseudo-scientific framing that was once functionally explanatory but is now largely theater. Theater ratio (0.64) reflects that the hierarchy persists through inertial legitimacy (it feels educationally justified) rather than through actual differential developmental impact. d≈0.15, f(d)≈0.08, σ=1.1 → χ≈0.04.
constraint_indexing:constraint_classification(nursery_social_hierarchy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational frame, the constraint risks being naturalized as inherent to human preference: 'Children naturally prefer complex, responsive objects' or 'Status hierarchies are evolutionarily hardwired.' This perspective incorrectly treats the nursery hierarchy as immutable. The structural data (ε=0.52, suppression=0.68) contradicts mountain classification — the engine detects false summit: the hierarchy is contingent on marketing systems, parental conditioning, and the visibility of mechanical action, not on natural developmental law.
constraint_indexing:constraint_classification(nursery_social_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nursery_social_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nursery_social_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nursery_social_hierarchy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nursery_social_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nursery_social_hierarchy, TR),
    TR >= 0.70.

:- end_tests(nursery_social_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant value from simple toys and the child's imaginative capacity, but not total — some simple toys persist, some children and parents resist the hierarchy, and the extractiveness is not as absolute as a pure predatory system. The value reflects that the extraction is mediated through consumer choice and parental agency, not through coercion. Suppression (0.68): High. Multiple suppression mechanisms: (1) marketing framing that positions simple toys as 'overstimulating' or 'developmentally inappropriate,' (2) institutional gatekeeping (daycare facilities with expensive equipment requirements), (3) peer comparison dynamics (children observing that complex toys receive more attention), (4) parental time allocation (modern parents under time pressure default to 'educational' toys). However, suppression is not total — homemade toys, cultural traditions, and some parents actively resist. Theater ratio (0.64): Moderate-high. The constraint persists largely through claims about developmental appropriateness and STEM engagement that are not strongly grounded in current developmental science. The theater has increased over the interval as manufacturers have learned to deploy pseudo-scientific messaging more effectively. Claimed type (Snare): The structural data (high extractiveness, high suppression, beneficiary/victim clarity) classifies as snare. The hierarchy offers minimal coordination benefit to the victims; it purely redistributes status and play-value from simple to complex.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival gap. The technical toy manufacturer sees coordination (Rope) — the hierarchy solves the collective action problem of establishing a standard for quality and value. The simple toy sees extraction (Snare) — its devaluation is permanent and non-negotiable. The parent sees mixed experience (Tangled Rope) — they enforce the hierarchy on their child while being trapped in it themselves through guilt and social comparison. The developmental psychology apparatus sees its own degraded legitimacy (Piton) — once functionally explanatory (developmental stages are real), the frame now persists through inertia as commercial framing has captured the narrative. The analytical observer risks seeing natural law (Mountain) — the false summit comes from treating commercial preferences as evolutionary inevitabilities rather than contingent institutional arrangements. The child's imaginative agency sees pure extraction (Snare) — the hierarchy suppresses its own generative capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Simple toy: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — simple toy has no alternative status within the hierarchy. Child's imaginative capacity: Victim + trapped → d≈0.88, f(d)≈1.32. High extraction — child internalizes the hierarchy's logic that imagination is insufficient. Technical toy manufacturer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Parent consumer: Victim + constrained → d≈0.58, f(d)≈0.78. Moderate extraction — parent has some capacity to resist (can choose simple toys) but is constrained by social pressure and guilt. Developmental psychology: Institutional + arbitrage → d≈0.15, f(d)≈0.08. Low effective extraction; piton classification comes from theater gate, not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit: naturalization of contingent arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is a genuine Snare (high extractiveness, high suppression, clear beneficiary/victim), not a natural law or pure coordination. The false summit (mountain perspective) arises from treating commercial preferences as evolutionary inevitabilities — 'children naturally prefer complex toys' naturalizes what is actually a conditioned preference shaped by marketing, parental valuation, and peer comparison. The piton perspective correctly identifies that the developmental psychology legitimacy has degraded into theater — the claims about developmental stages persist but are increasingly detached from actual developmental science. The snare classification is robust: the hierarchy extracts dignity and play-value from simple toys through suppression mechanisms (marketing, peer comparison, parental gatekeeping) and redistributes that value to technical toys, with minimal offsetting coordination benefit to the victims. The parent's tangled rope perspective is secondary: while parents benefit from some coordination (shared standards about 'quality' toys), they are primarily trapped in consumption patterns justified by the hierarchy's logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preference_or_conditioning,
    'Do children''s documented preferences for complex, responsive toys reflect genuine developmental attraction or learned conditioning through marketing and parental valuation?',
    'Cross-cultural longitudinal studies comparing toy preference in children raised with/without exposure to commercial marketing; intervention studies introducing simple toys with equal parental enthusiasm and status framing',
    'If genuine preference: hierarchy reflects natural developmental stages (more mountain-like). If conditioning: hierarchy is contingent enforcement mechanism (more snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_or_conditioning, empirical, 'Whether toy preferences are innate or conditioned').

omega_variable(
    imaginative_capacity_causality,
    'Does exposure to simple toys causally enhance imaginative agency in children, or does the observed correlation reflect selection bias (children with stronger imagination gravitate toward simple toys)?',
    'Randomized intervention: children given equal play time with simple vs complex toys; measurement of imaginative output (narrative creation, pretend scenarios, novel object repurposing); longitudinal tracking of creative capacity',
    'If causal enhancement: simple toys are genuine developmental goods being suppressed by hierarchy (extraction is real). If selection bias: hierarchy reflects preferences of already-imaginative children (extraction claim is weakened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imaginative_capacity_causality, empirical, 'Whether simple toys causally enhance imagination or reflect selection').

omega_variable(
    commercial_coordination_versus_extraction,
    'Is the technical toy hierarchy primarily a coordination mechanism (manufacturers solving collective action problems around standards and expectations) or an extraction mechanism (rent-seeking through status manipulation)?',
    'Analysis of manufacturer innovation rates, product differentiation, and price markups in simple vs complex toy categories; study of marketing spend allocation relative to developmental claims; historical comparison with pre-commercial toy preference data',
    'If coordination dominates: constraint is more Rope than Snare. If extraction dominates: constraint is pure Snare with marketing theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_coordination_versus_extraction, empirical, 'Whether hierarchy serves coordination or extraction function').

omega_variable(
    suppression_mechanism_durability,
    'Is the suppression of simple toys maintained through active enforcement (marketing, peer pressure, parental gatekeeping) or through institutional inertia (conventions that persist without active reinforcement)?',
    'Documentation of marketing spend, parental messaging, and peer-status consequences for simple toy selection; studies of natural preference emergence when commercial signals are removed (e.g., play in commercials-unexposed environments)',
    'If active enforcement: high suppression score justified; snare classification robust. If inertia: suppression overstated; constraint may degrade faster than expected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_durability, empirical, 'Whether suppression requires active enforcement or persists via inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nursery_social_hierarchy, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsh_tr_t0, nursery_social_hierarchy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nsh_tr_t25, nursery_social_hierarchy, theater_ratio, 25, 0.5).
narrative_ontology:measurement(nsh_tr_t50, nursery_social_hierarchy, theater_ratio, 50, 0.64).

% Extraction over time
narrative_ontology:measurement(nsh_be_t0, nursery_social_hierarchy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nsh_be_t25, nursery_social_hierarchy, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(nsh_be_t50, nursery_social_hierarchy, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nursery_social_hierarchy, information_standard).
narrative_ontology:affects_constraint(nursery_social_hierarchy, consumer_status_extraction_systems).
narrative_ontology:affects_constraint(nursery_social_hierarchy, childhood_developmental_commodification).

% DUAL FORMULATION NOTE:
% The nursery social hierarchy is downstream of broader consumer marketing systems that use status allocation as a demand-generation mechanism. The upstream constraint (consumer_status_extraction_systems) has its own ε reflecting the general commercialization of consumer choice; the nursery hierarchy applies this mechanism specifically to childhood play objects, with ε=0.52 reflecting the domain-specific intensity of extraction through parental guilt and child peer comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nursery_social_hierarchy, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
