% ============================================================================
% CONSTRAINT STORY: grete_samsa_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   In Kafka's The Metamorphosis, Gregor Samsa's transformation into an
 *   insect creates a structural constraint on his sister Grete. Initially
 *   bound by emergency necessity and filial obligation to care for her
 *   helpless brother, Grete is drawn into a role that extracts her childhood
 *   agency, restricts her mobility, and absorbs her identity into the family
 *   crisis. Yet the constraint also contains a coordination function: family
 *   survival depends on organizing roles and sharing burden. As the narrative
 *   progresses, Grete simultaneously becomes a skilled caregiver
 *   (demonstrating agency within the constraint) and a victim of it (her life
 *   becomes consumed by care). By the narrative's conclusion, she emerges as
 *   a young woman with economic independence, personal preferences, and the
 *   capacity to refuse continued care—revealing the constraint as temporal
 *   rather than immutable. The transformation of the constraint from acute
 *   necessity to performative obligation, and finally to its rupture, models
 *   how tangled_rope constraints degrade into pitons and then dissolve when
 *   the structural conditions that justified them change.
 *
 * KEY AGENTS:
 *   - Grete Samsa: Primary victim initially (powerless/trapped) → emerges as beneficiary (organized/mobile) — transitions from forced caregiver to agent of her own ascendance
 *   - Gregor Samsa: Primary beneficiary of care (trapped/dependent) → victim of the constraint's structure (his continued existence becomes burden) — receives care but at cost of his family's future
 *   - Samsa Family Collective: Secondary beneficiary (institutional/arbitrage) — coordinates survival response; benefits from Grete's labor and patriarchal organization
 *   - Patriarchal social structure: Institutional constraint (institutional/arbitrage) — enforces gendered role expectations; maintains extraction through authority and obligation
 *   - Analytical observer: Civilizational view (analytical/analytical) — sees risk of naturalizing contingent Victorian family economics as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(grete_samsa_transition, 0.52).
domain_priors:suppression_score(grete_samsa_transition, 0.68).
domain_priors:theater_ratio(grete_samsa_transition, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(grete_samsa_transition, extractiveness, 0.52).
narrative_ontology:constraint_metric(grete_samsa_transition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(grete_samsa_transition, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(grete_samsa_transition, tangled_rope).
narrative_ontology:human_readable(grete_samsa_transition, "Grete's Burden and Ascendance in The Metamorphosis").
narrative_ontology:topic_domain(grete_samsa_transition, "social/economic").

domain_priors:requires_active_enforcement(grete_samsa_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(grete_samsa_transition, grete_samsa).
narrative_ontology:constraint_beneficiary(grete_samsa_transition, samsa_family_survival).
narrative_ontology:constraint_victim(grete_samsa_transition, grete_childhood_agency).
narrative_ontology:constraint_victim(grete_samsa_transition, gregor_samsa).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRETE AS TRAPPED CAREGIVER (SNARE) — Grete is bound by filial obligation, economic necessity (family needs her labor), and social expectation. She cannot refuse the burden of Gregor's care without abandoning her family or accepting destitution. No mobility; all costs, few benefits until late in the narrative. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(grete_samsa_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GRETE AS DOMESTIC FUNCTIONARY (TANGLED ROPE) — Within the household, Grete's role coordinates family care and maintenance (coordination benefit) while being extracted through unpaid labor, restricted autonomy, and absorbed into family identity. She experiences both the necessity of her role and its exploitation. Constrained exit: she could theoretically leave, but the social and economic costs are severe. d≈0.68, f(d)≈1.02, σ=0.8 → χ≈0.34.
constraint_indexing:constraint_classification(grete_samsa_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE SAMSA FAMILY COLLECTIVE (ROPE) — The family unit benefits from the coordination of roles: Grete's labor sustains the household; parental authority organizes response to catastrophe; economic pooling enables survival when Gregor cannot contribute. Grete's domestic role solves a genuine coordination problem. Family sees this as mutual obligation rather than extraction. d≈0.15, f(d)≈0.01, σ=0.8 → χ≈0.004.
constraint_indexing:constraint_classification(grete_samsa_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: GRETE'S EMERGENT AGENCY (SCAFFOLD) — By the narrative's end, Grete moves toward autonomy: she finds employment, develops skills, asserts preferences (objecting to Gregor's continued presence), and eventually participates in planning the family's relocation. The constraint is revealed as temporary — a crisis measure that normalizes into household dysfunction, then dissolves as Grete gains economic and social agency. The family's initial response to Gregor's transformation (Grete as essential caregiver) had a sunset: it lasted only as long as the family believed Gregor might recover. d≈0.35, f(d)≈0.32, σ=0.8 → χ≈0.17.
constraint_indexing:constraint_classification(grete_samsa_transition, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: PATRIARCHAL FAMILY STRUCTURE (PITON) — The broader social constraint (female duty to family, gendered household labor division, restricted women's economic participation) persists through institutional inertia and theatrical reaffirmation even as its functional necessity declines. The father's authority performatively reasserts itself; the mother's passivity ritually confirms the constraint's centrality. Theater ratio (0.64) reflects that much of the family's behavior is performative maintenance of a structure that no longer functionally resolves their core problem (economic survival). By narrative's end, the theater of patriarchal obligation becomes visible as degraded — the father's violence, the mother's ineffectuality, and Grete's refusal all unmask the constraint as inertial. d≈0.05, f(d)≈-0.12, σ=0.8 → χ≈-0.04.
constraint_indexing:constraint_classification(grete_samsa_transition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: NECESSITY OF FAMILY SURVIVAL (MOUNTAIN - FALSE SUMMIT) — A civilizational view might see the constraint as a natural law: when one family member becomes economically useless, the others must absorb his care-burden to survive. This appears as an immutable fact of human interdependence and scarcity. However, the structural data (ε=0.52, suppression=0.68, theater=0.64) contradicts a mountain classification. The constraint is not natural law but historically contingent: it depends on the nuclear family structure, the absence of institutional care, the marriage market's restriction of women's options, and patriarchal authority. The false summit detector reveals this perspective as naturalizing what is actually a tangled_rope — a coordination necessity (family must organize response to crisis) layered over extractive relationships (gendered labor, patriarchal authority, restricted mobility).
constraint_indexing:constraint_classification(grete_samsa_transition, mountain,
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
 *   Extractiveness (0.52): Moderate-high, declining. Initially (t=0), the acute crisis creates maximum extraction—Grete must provide care or the family unit collapses. By t=6, extractiveness declines as Grete gains employment, the family adjusts, and Gregor's condition becomes chronic rather than emergent. The trajectory reflects a constraint that was justified by emergency necessity but persists through institutional inertia after the emergency passes. Suppression (0.68): Moderate-high and sustained. Significant barriers to Grete's exit include economic dependence, social expectation of female filial duty, lack of alternative care arrangements, marriage market restricted by her caregiving obligations, and patriarchal family authority. The suppression is real and substantial, though not absolute—she does eventually find employment and asserts agency. Theater ratio (0.64): Moderate-high, increasing. The family's response begins as pragmatic crisis management (lower theater at t=0). Over time, as the acute emergency fades and Gregor's condition stabilizes, the caregiver role becomes more performative: the family continues rituals of care and obligation even as the original necessity diminishes. By t=6, theater is high because the constraint persists through institutional inertia (the father's reasserted authority, the mother's passive suffering, Grete's 'duty') rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits striking perspectival divergence. Grete initially sees pure extraction (Snare): she is trapped in a role she did not choose, bearing all costs. The family sees coordination (Rope): Grete's role solves the crisis of Gregor's care. The patriarchal structure sees its own maintenance (Piton): the constraint is performative theater reinforcing gendered authority. Grete's emerging agency sees a temporary scaffold: her caregiving has a sunset—she is building skills, economic independence, and the capacity to refuse. The civilizational observer risks seeing necessity (Mountain): family must care for helpless members. Yet the base properties (extractiveness at 0.52, substantial suppression, significant theater) confirm the mountain view is a false summit. The constraint is not natural law but a tangled coordination necessity (family must organize care) embedded in extractive gendered relations (unpaid labor, restricted mobility, patriarchal authority). The perspectival gap widens as the narrative progresses: Grete's snare experience sharpens (increasing burden), while the family's rope experience becomes more theatrical (the problem becomes managed routine rather than acute crisis).
 *
 * DIRECTIONALITY LOGIC:
 *   Grete: Victim + trapped → d≈0.92, f(d)≈1.40. Maximal extraction. She bears the costs (labor, mobility restriction, absorbed identity) with minimal exit options in the constraint's early phase. Samsa family: Beneficiary + arbitrage → d≈0.15, f(d)≈0.01. The family experiences the constraint as low-extraction coordination—they see Grete's role as mutual obligation and crisis response, not as exploitation. Gregor: Victim + trapped (dependent) → d≈0.90, f(d)≈1.38. He is trapped by his own condition; the family's response both helps him (provision, care) and harms him (restricts movement, marks him as burden). Patriarchal authority: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. The constraint reinforces patriarchal structure and male authority; the father's reassertion of control is a positive reinforcement from the system's perspective. Grete's emerging agency: Mobile exit option → d drops to ≈0.35, f(d)≈0.32. As she gains economic independence and job skills, her directionality shifts—she becomes less trapped, more capable of mobile exit. The constraint's extraction on her decreases because she can threaten to leave and enforce better terms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caregiver_voluntariness_boundary,
    'At what point does Grete''s caregiving transition from existential necessity to a choice she actively makes and could refuse?',
    'Narrative analysis of explicit statements of agency; correlation with economic independence milestones (employment, savings); cross-reference with her interactions showing compliance vs. initiative',
    'If transition occurs early (weeks): Grete''s middle-period experience is more rope than snare; constraint is weaker. If late (months to narrative end): prolonged snare; extraction is deeper and only resolved by Gregor''s death.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caregiver_voluntariness_boundary, conceptual, 'Boundary between necessity and choice in Grete''s caregiving').

omega_variable(
    family_economic_viability_postcatastrophe,
    'Could the Samsa family have survived Gregor''s transformation without Grete''s domestic labor, given parental income and the possibility of institutional care or alternative arrangements?',
    'Historical analysis of 1910s Vienna working-class family economy; calculation of actual household budget; comparison with families who lost breadwinners to disability or death',
    'If viable without Grete: caregiver extraction is choice, not necessity — Grete is trapped by patriarchal obligation, not economic survival. Snare perspective confirmed. If not viable: care is genuine coordination necessity — tangled_rope is correct, and the constraint is more justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(family_economic_viability_postcatastrophe, empirical, 'Whether family survival required Grete''s specific labor').

omega_variable(
    gregor_as_victim_or_beneficiary,
    'From Gregor''s structural perspective, is the family''s response to his transformation a constraint that extracts from him, or does he benefit from continued family provision?',
    'Analysis of Gregor''s agency, suffering, and dependence; evaluation of whether family care improves his condition or merely prolongs his suffering; assessment of whether he would choose isolation or family provision if agency were possible',
    'If Gregor is victim: constraint extracts from both Grete and Gregor; it is purer snare. If Gregor is beneficiary: complex asymmetry where Grete bears costs and Gregor receives care; tangled_rope is confirmed with reversed roles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gregor_as_victim_or_beneficiary, conceptual, 'Gregor''s structural position relative to the family constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(grete_samsa_transition, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grete_tr_t0, grete_samsa_transition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(grete_tr_t3, grete_samsa_transition, theater_ratio, 3, 0.58).
narrative_ontology:measurement(grete_tr_t6, grete_samsa_transition, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(grete_be_t0, grete_samsa_transition, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(grete_be_t3, grete_samsa_transition, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(grete_be_t6, grete_samsa_transition, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(grete_samsa_transition, resource_allocation).
narrative_ontology:affects_constraint(grete_samsa_transition, victorian_household_economy).
narrative_ontology:affects_constraint(grete_samsa_transition, familial_obligation_norm).

% DUAL FORMULATION NOTE:
% Grete's constraint is downstream of the broader Victorian family structure and labor economy but represents a distinct structural junction. The upstream constraints (patriarchal authority, gendered labor division, restricted female economic participation) create the conditions for Grete's extraction; this story models how those constraints manifest at the household level during catastrophic disruption. The family survival constraint (coordination necessity) and the patriarchal gender constraint (extraction mechanism) are separable—the story shows their tangled interaction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(grete_samsa_transition, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
