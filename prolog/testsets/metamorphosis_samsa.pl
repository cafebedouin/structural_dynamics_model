% ============================================================================
% CONSTRAINT STORY: metamorphosis_samsa
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_metamorphosis_samsa, []).

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
 *   constraint_id: metamorphosis_samsa
 *   human_readable: The Samsa Family's Debt Bondage
 *   domain: economic/social/biological
 *
 * SUMMARY:
 *   Gregor Samsa's biological transformation into a 'horrible vermin' exposes
 *   and intensifies a pre-existing debt bondage that governs the family's
 *   entire economic structure. Before the transformation, the constraint
 *   operated as a standard labor-debt mechanism: Gregor's income service
 *   justified the debt claim and maintained the family's precarious
 *   respectability. The transformation eliminates Gregor's labor capacity
 *   while preserving the debt obligation, revealing the constraint's true
 *   extractive nature. The family now bears double extraction: the employer's
 *   continuing debt claim AND the biological fact of Gregor's permanent
 *   dependency. The constraint operates across multiple structural levels —
 *   biological (incapacity), economic (debt enforcement), familial
 *   (obligation to support), and social (rejection and disgust). The
 *   extractiveness increases over the measurement interval as the family
 *   exhausts savings, accepts Gregor's permanent incapacity, and realizes the
 *   debt will never be discharged through his future earnings.
 *
 * KEY AGENTS:
 *   - Gregor Samsa: Primary victim (powerless/trapped) — sole income earner rendered permanently incapable; bears full force of extraction through immobility
 *   - Samsa Family Unit: Secondary victim (moderate/constrained) — dependent on Gregor's income; now faces debt obligation plus permanent care burden with no exit
 *   - The Employer / Creditor: Beneficiary (institutional/arbitrage) — holds debt claim that persists after Gregor's incapacity; can maintain extraction indefinitely through family liability
 *   - Social Context (Urban Prague): Structural actor (analytical/analytical) — establishes norms of respectability, work obligation, and debt enforcement that frame the family's trap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metamorphosis_samsa, 0.68).
domain_priors:suppression_score(metamorphosis_samsa, 0.75).
domain_priors:theater_ratio(metamorphosis_samsa, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metamorphosis_samsa, extractiveness, 0.68).
narrative_ontology:constraint_metric(metamorphosis_samsa, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(metamorphosis_samsa, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metamorphosis_samsa, snare).
narrative_ontology:human_readable(metamorphosis_samsa, "The Samsa Family's Debt Bondage").
narrative_ontology:topic_domain(metamorphosis_samsa, "economic/social/biological").

domain_priors:requires_active_enforcement(metamorphosis_samsa).
% --- Structural relationships ---
narrative_ontology:constraint_victim(metamorphosis_samsa, gregor_samsa).
narrative_ontology:constraint_victim(metamorphosis_samsa, samsa_family_unit).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GREGOR SAMSA (SNARE) — Complete structural trap. The biological transformation eliminates his sole income source while the debt obligation persists. No exit options: he cannot work, cannot leave, cannot escape the family's dependence on his earnings. Maximum suppression — the constraint combines biological incapacity with economic coercion. His experienced extractiveness is maximal because the debt mechanism now feeds off his immobility itself.
constraint_indexing:constraint_classification(metamorphosis_samsa, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SAMSA FAMILY UNIT (SNARE) — Trapped by their dependence on Gregor's income. The family cannot exit because they have no alternative income source and are bound by social obligation to support Gregor (and by law in some jurisdictions). The transformation does not relieve the debt — it intensifies extraction by making Gregor a permanent drain on household resources rather than an asset. The family is now victim to both the employer's debt claim AND to the biological fact of Gregor's condition.
constraint_indexing:constraint_classification(metamorphosis_samsa, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: EMPLOYER / CREDITOR (TANGLED ROPE) — Institutional actor with genuine arbitrage options. The employer has a coordination function (organizing Gregor's labor for commercial purposes) but uses debt leverage to extract surplus labor time and compliance. The transformation creates a structural ambiguity: the debt remains enforceable even though Gregor cannot work. The employer can maintain the debt claim indefinitely, turning the constraint into pure extraction. However, there is also a vestigial coordination component — the original debt was incurred through a legitimate labor exchange. The employer's position is hybrid: benefiting from continued debt leverage while losing the coordination function that originally justified it.
constraint_indexing:constraint_classification(metamorphosis_samsa, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a naturalistic perspective, the biological transformation appears to be an immutable constraint: a human body cannot serve as a salesman if it has become physically incompatible with human society. The constraint appears to emerge naturally from biological fact. However, the mountain classification is false. The structural data reveals this as naturalization of a social-economic arrangement (debt obligations). The 'mountain' is actually the intersection of a biological event with a pre-existing snare (debt bondage). The engine's false summit detector will expose this.
constraint_indexing:constraint_classification(metamorphosis_samsa, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metamorphosis_samsa_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(metamorphosis_samsa, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metamorphosis_samsa, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(metamorphosis_samsa, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(metamorphosis_samsa_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. Initial base extractiveness (0.35) reflects standard labor-debt extraction — Gregor works, earns, services debt, family survives. This is extraction but with a coordination function: the employer benefits from organized labor. After transformation, extractiveness rises sharply (0.68) because the debt claim persists while labor becomes impossible. The employer now extracts pure rent: the family cannot escape the debt, Gregor cannot work off the debt, and the constraint feeds on immobility itself. Suppression (0.75): Very high. The constraint combines biological incapacity (Gregor cannot transform back), economic coercion (debt claim is enforceable), social exclusion (the family is increasingly isolated), and family obligation (they cannot abandon Gregor). Every exit route is blocked. Theater ratio (0.38): Moderate but increasing over time. Early in the transformation, there is still pretense that Gregor's condition is temporary and he might recover. The family maintains social rituals (keeping the room locked, feeding Gregor, managing appearances). By the end, the theater increases as the family accepts Gregor's permanence and the rituals become purely performative — feeding a creature that will never work again, maintaining a debt claim on an incapacitated debtor. The theater is not false review or performative compliance; it is the family's construction of meaning around a structural trap.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between Gregor (Snare) and the Employer (Tangled Rope) is maximal. Gregor sees pure extraction — the constraint yields him nothing but incapacity and dependence. The employer sees a hybrid: they originally offered legitimate coordination (employment, which the Samsa family needed), and the debt was incurred through this exchange. However, the employer's perspective becomes increasingly snare-like as they maintain debt enforcement against an incapacitated debtor. The gap reveals that the constraint has degraded from Tangled Rope (labor + debt as legitimate coordination-extraction mix) to pure Snare (debt without the possibility of labor). The family's perspective (Snare) aligns with Gregor's, but for different reasons: Gregor is trapped by incapacity, while the family is trapped by obligation and dependence on debt discharge that cannot occur. The analytical observer's Mountain classification is a false summit: the observer risks naturalizing the social-economic arrangement (debt enforcement) as a biological law.
 *
 * DIRECTIONALITY LOGIC:
 *   Gregor as powerless/trapped: d ≈ 0.95 → f(d) ≈ 1.42. He experiences maximum extraction because he has zero exit options (biological incapacity), zero power (dependent), and zero time horizon (immediate survival focus). The employer as institutional/arbitrage: d ≈ 0.08 → f(d) ≈ -0.12. The employer experiences negative effective extraction (benefits) because of arbitrage options and beneficiary status. The family as moderate/constrained: d ≈ 0.70 → f(d) ≈ 1.15. The family is partially trapped (debt obligation, family duty to Gregor) but retains moderate power (can negotiate with employer, can seek alternative support). Their experienced extraction is moderate-to-high because the constraint is real but not absolute.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint has TWO distinct components: (1) the biological transformation (which might appear as a Mountain — immutable biological fact), and (2) the debt mechanism (which is clearly a Snare — enforced extraction with suppression). The narrative conflates these into a single constraint, but the structural analysis reveals they are separable. The biological fact (Gregor's transformation) is not the constraint — it is a trigger that exposes the pre-existing constraint (debt bondage). A society with debt forgiveness for incapacity would have a different constraint (biological isolation without economic extraction). A society with universal income would have a different constraint (Gregor's condition is private tragedy, not family economic catastrophe). The Snare classification is correct because it identifies the debt mechanism as the extractive element, not the biological transformation itself. The mandatrophy is resolved by refusing to naturalize the social-economic arrangement and by recognizing that 'extraction through debt' and 'biological incapacity' are two separate phenomena forced into proximity by the story's social context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_determinism_vs_social_construction,
    'Is the constraint fundamentally biological (the transformation itself) or fundamentally social (the debt mechanism that persists after transformation)?',
    'Counterfactual analysis: if Gregor had been born with the same physical form, would the family debt still be enforceable? If yes, the constraint is social. If the transformation occurred in a society with debt forgiveness provisions for incapacity, would the family be trapped? If no, the constraint is institutional, not biological.',
    'If biological: Snare classification is correct but focuses on the wrong agent (biological fact rather than debt structure). If social: Snare classification is correct and the biological event is merely a trigger that exposes a pre-existing extractive arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_determinism_vs_social_construction, conceptual, 'Whether constraint is rooted in biology or social structure').

omega_variable(
    debt_enforceability_after_incapacity,
    'Under what legal and social regimes can debt be enforced against an incapacitated debtor? Is the Samsa family''s continued obligation a matter of law or of social pressure?',
    'Comparative institutional analysis: examination of debt enforcement norms in the text''s implied jurisdiction; analysis of whether creditors pursue debt claims against permanently disabled individuals in similar circumstances; assessment of whether family members can be held liable for the debtor''s incapacity.',
    'If debt is legally enforceable despite incapacity: institutional snare with clear legal backing. If debt enforcement relies on social pressure and family shame: social snare with weak but effective coercive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_enforceability_after_incapacity, empirical, 'Whether debt can be enforced against incapacitated debtors').

omega_variable(
    gregor_agency_in_biological_form,
    'What degree of agency or economic contribution is possible for Gregor in his transformed state? Can he work in alternative forms, generate income through other means, or is the transformation total immobilization?',
    'Textual analysis of Gregor''s actual capabilities after transformation; assessment of whether any economic role remains available to him; determination of whether the constraint is purely biological immobility or a combination of immobility plus social rejection.',
    'If total immobility: constraint is primarily biological (albeit expressed through debt mechanism). If partial agency remains: constraint is primarily social (institutional rejection of transformed Gregor as economically viable). This affects whether the snare classification emphasizes biological or institutional suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gregor_agency_in_biological_form, empirical, 'Whether Gregor retains any capacity for economic contribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metamorphosis_samsa, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_theater_t0, metamorphosis_samsa, theater_ratio, 0, 0.22).
narrative_ontology:measurement(meta_theater_t3, metamorphosis_samsa, theater_ratio, 3, 0.3).
narrative_ontology:measurement(meta_theater_t6, metamorphosis_samsa, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(meta_extractiveness_t0, metamorphosis_samsa, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(meta_extractiveness_t3, metamorphosis_samsa, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(meta_extractiveness_t6, metamorphosis_samsa, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(metamorphosis_samsa, resource_allocation).
narrative_ontology:affects_constraint(metamorphosis_samsa, debt_enforcement_against_incapacity).
narrative_ontology:affects_constraint(metamorphosis_samsa, family_obligation_vs_survival).

% DUAL FORMULATION NOTE:
% The Samsa constraint decomposes into two structural elements: (a) the biological transformation as a trigger event (ε ≈ 0.05, immutable), and (b) the debt bondage as the extractive mechanism (ε ≈ 0.68, social/institutional). These are linked but distinct constraints. The narrative conflates them, which is the source of the apparent Mountain classification at the analytical level. The proper decomposition treats the debt mechanism as the constraint and the biological transformation as an exogenous shock that changes the constraint's configuration from Tangled Rope to Snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(metamorphosis_samsa, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
