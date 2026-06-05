% ============================================================================
% CONSTRAINT STORY: metamorphosis_samsa
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   Gregor Samsa, a traveling salesman, discovers himself transformed into a
 *   large insect. This metamorphosis is not a metaphor but a structural event
 *   that destroys his capacity to work — the sole mechanism by which he
 *   serviced his family's debt obligation to his employer. The constraint
 *   operates at multiple levels: biological (an incapacitated organism cannot
 *   work), economic (debt obligation persists despite incapacity), social
 *   (the family cannot abandon Gregor nor can they emigrate), and legal (the
 *   creditor's claim remains enforceable). The metamorphosis transforms
 *   Gregor from a capable debtor into a trapped victim, and simultaneously
 *   transforms the family from beneficiaries of his labor into victims of a
 *   now-impossible obligation. This is a canonical snare: high extractiveness
 *   (ε=0.72), maximum suppression (0.88), and no exit options for any actor.
 *
 * KEY AGENTS:
 *   - Gregor Samsa: Primary victim (powerless/trapped) — loses labor capacity, remains liable for debt, physically confined to family lodging
 *   - The Samsa Family: Co-victim (powerless/trapped) — inherits debt obligation, loses income source, cannot emigrate or abandon Gregor
 *   - The Employer/Creditor: Beneficiary (moderate/constrained) — holds debt claim, can enforce against family assets and labor, but does not fully control the outcome (cannot force Gregor to work in transformed state)
 *   - The Working Class (Generational): Organized observers (organized/mobile) — see Gregor's plight as exemplifying systematic vulnerability of labor-dependent debtors
 *   - The Legal/Contractual System: Institutional actor (institutional/arbitrage) — provides both coordination function (credit) and extraction mechanism (debt enforcement)
 *   - The Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the social contingency (obligation persists despite incapacity) as an immutable biological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metamorphosis_samsa, 0.72).
domain_priors:suppression_score(metamorphosis_samsa, 0.88).
domain_priors:theater_ratio(metamorphosis_samsa, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metamorphosis_samsa, extractiveness, 0.72).
narrative_ontology:constraint_metric(metamorphosis_samsa, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(metamorphosis_samsa, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metamorphosis_samsa, snare).
narrative_ontology:human_readable(metamorphosis_samsa, "The Samsa Family's Debt Bondage").
narrative_ontology:topic_domain(metamorphosis_samsa, "economic/social/biological").

% --- Structural relationships ---
narrative_ontology:constraint_victim(metamorphosis_samsa, gregor_samsa).
narrative_ontology:constraint_victim(metamorphosis_samsa, samsa_family).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GREGOR SAMSA POST-METAMORPHOSIS (SNARE) — Complete loss of exit options. The transformation eliminates his sole income-generating capacity. Debt obligation continues despite impossibility of fulfillment. No alternative employment possible. Family remains dependent on his labor. d≈0.98, f(d)≈1.45, σ=0.8 → χ≈0.82. Maximum extraction from a trapped, powerless agent.
constraint_indexing:constraint_classification(metamorphosis_samsa, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SAMSA FAMILY (SNARE) — Trapped by inherited debt obligation. Gregor's transformation does not discharge the debt; instead, the family loses the only income source that serviced it. No exit option exists: abandoning Gregor is culturally unthinkable; emigrating requires capital they do not possess; default triggers social and legal sanctions. The mother's illness and sister's education are now irrelevant to debt servicing capacity. d≈0.96, f(d)≈1.42, σ=0.8 → χ≈0.80.
constraint_indexing:constraint_classification(metamorphosis_samsa, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE EMPLOYER/CREDITOR (SNARE) — Benefits from the debt structure but does not experience extraction. From the creditor's perspective, the constraint is pure enforcement: the debt is owed, the debtor cannot pay, and the only recourse is to seize assets or demand labor from family members. The creditor has constrained exit (can collect from family labor, seize lodgings, apply legal pressure) but perceives the constraint as a coordinate fact: the debt exists, the debtor is incapacitated, obligation persists. d≈0.35, f(d)≈0.35, σ=0.9 → χ≈0.23. From the creditor's view, this is not a snare but a coordination problem (debt repayment mechanism) that has failed due to force majeure. Included here as the institutional beneficiary perspective to show the asymmetry.
constraint_indexing:constraint_classification(metamorphosis_samsa, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE WORKING CLASS / GENERATIONAL VIEW (SNARE) — From a generational perspective, Gregor's transformation exemplifies a structural feature of debt-bondage: workers whose labor is the sole asset are completely vulnerable to any incapacity (illness, injury, aging, disability). The system offers no insurance, no redundancy, no dignity in incapacity. Organized labor movements see this constraint as evidence of systemic extraction that persists across generations. d≈0.82, f(d)≈1.18, σ=0.9 → χ≈0.75.
constraint_indexing:constraint_classification(metamorphosis_samsa, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE LEGAL/CONTRACTUAL SYSTEM (TANGLED ROPE) — From a civilizational view, debt contracts serve a coordination function: they enable credit, investment, and capital allocation. But the system also extracts from debtors through interest, foreclosure, and the threat of ruin. The legal system both enables commerce (coordination benefit) and enforces collection (extraction mechanism). The system has arbitrage exits available (bankruptcy law, debt forgiveness, jubilee provisions) but historically has not used them. d≈0.48, f(d)≈0.62, σ=1.0 → χ≈0.44. The legal system perceives itself as serving both functions: fairness (coordination) and certainty (enforcement).
constraint_indexing:constraint_classification(metamorphosis_samsa, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — The transformation into a vermin represents the ultimate expression of a natural limit: a human biological organism has inherent constraints on what labor it can perform. No social system can compel a creature to do work its body cannot do. The constraint is immutable in the sense that no amount of coercion, law, or social pressure can restore Gregor's capacity to work as a traveling salesman. However, the structural data (ε=0.72, suppression=0.88, theater=0.35) contradicts mountain classification. The 'natural limit' framing masks the genuine snare: the social system continues to demand payment from an incapacitated debtor, creating an impossible bind. This is a false summit — the naturalizing of a contingent institutional arrangement (debt obligation surviving incapacity) as an immutable law.
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
 *   Extractiveness (0.72): Very high. The debt obligation produces maximal extraction from Gregor post-metamorphosis because: (1) he cannot work, yet (2) obligation persists, (3) the family becomes liable for servicing it, and (4) no legal or social mechanism exists to discharge the debt. The measurement shows the sharp increase from t=0 (ε=0.15, before transformation) to t=1 (ε=0.72, immediately after transformation). At t=3 (ε=0.76), the extraction has slightly increased as the family realizes the permanence of the situation and creditor pressure intensifies. Suppression (0.88): Extreme. Multiple barriers prevent escape: biological incapacity (Gregor cannot become mobile), legal liability (family is bound to debt), cultural shame (abandonment is unthinkable), economic necessity (they have no capital to emigrate or start over), and the employer's enforcement capacity (property seizure, legal action). Theater ratio (0.35): Low. Unlike many snares that maintain themselves through performative compliance, the Samsa snare is brutally functional. There is no pretense that Gregor can work; no false hope of recovery; no ritual compliance masking extraction. The theater increases slightly over time (t=0→0.10, t=3→0.38) as the family develops coping narratives (Gregor might recover, the transformation might reverse, the employer might forgive) but these are genuine coping mechanisms, not performative theater in the classical sense.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival gap between victim and institutional views. Gregor and the family see a snare with no exit. The legal system sees a tangled rope — debt contracts serve coordination (enabling credit and commerce) but also enforce extraction (debt collection). The employer sees only enforcement of a rightful claim; the transformation is force majeure that does not discharge the obligation. Organized labor sees a systematic snare: workers have no redundancy, no insurance, no social safety net — any incapacity triggers ruin. The analytical observer risks naturalizing the constraint as a mountain (biological incapacity makes work impossible) when the actual snare is social (obligation persists despite incapacity). The key insight: the transformation is a natural event, but the obligation's persistence is a social choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Gregor Samsa: Victim + trapped → d≈0.98, f(d)≈1.45. Maximum extraction. He cannot exit, cannot work, cannot escape obligation. Samsa Family: Victim + trapped → d≈0.96, f(d)≈1.42. Nearly maximum extraction. They inherit liability, cannot abandon Gregor, cannot emigrate without capital. Employer/Creditor: Beneficiary + constrained → d≈0.35, f(d)≈0.35. The employer benefits from enforcement power but cannot extract full value (cannot compel work from an incapacitated debtor). Legal system: Beneficiary + arbitrage → d≈0.48, f(d)≈0.62. The legal system perceives itself as neutral (coordination function) but actually enables extraction (enforcement power). Organized labor: Victim + mobile → d≈0.82, f(d)≈1.18. Organized agents can theoretically exit (unionize, emigrate, organize politically) but recognize themselves in Gregor's plight — the snare is systematic, not individual.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED BY SNARE DOMINANCE: This constraint avoids mandatrophy because the snare classification is unambiguous from the perspectives of all victims and the analytical observer. The only alternative classification (tangled rope from the legal system perspective) is explicitly secondary — the legal system perceives coordination and extraction as joint functions, but the empirical reality is that Gregor and the family experience pure extraction with no coordination benefit. The mandatrophy is resolved by recognizing that the legal system's self-perception (fair coordination mechanism) is a false summit — the actual structure is a snare that uses legality as a enforcement mechanism. The transformation serves as a diagnostic moment: it strips away the pretense that the debt contract is a fair coordination mechanism and reveals it as an extraction mechanism targeting labor-dependent debtors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_inevitability_vs_social_contingency,
    'Is Gregor''s transformation a natural biological event that inevitably makes debt servicing impossible, or is it a structurally contingent moment that reveals the fragility of a social system built on individual labor as the sole asset?',
    'Historical analysis of how other societies/legal systems have handled debtor incapacity; comparison with disability accommodation, insurance, and social safety net approaches in different contexts',
    'If biological inevitability: constraint is mountain-like, systemic reform is futile. If social contingency: constraint is snare-like, systemic reform is possible and necessary. The classification depends entirely on whether we naturalize or denaturalize the relationship between physical incapacity and social obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_inevitability_vs_social_contingency, conceptual, 'Whether incapacity is natural or whether obligation-persistence is socially contingent').

omega_variable(
    inherited_vs_personal_obligation,
    'Does the debt obligation belong to the employer (who loaned capital) or to Gregor specifically (who promised labor)? If Gregor is incapacitated, can his family''s obligation to service the debt be enforced, or does it discharge?',
    'Historical legal analysis of how debt inheritance worked in early 20th century Vienna/Prague; comparison with contemporary debt law and family liability standards',
    'If obligation is personal and discharges with incapacity: the snare is less total (ε→0.45). If obligation transfers to family: the snare is complete (ε=0.72). The entire extractiveness value hinges on whether family labor can be conscripted to service inherited debt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherited_vs_personal_obligation, empirical, 'Whether debt obligation persists or discharges with debtor incapacity').

omega_variable(
    transformation_as_external_shock_vs_internal_collapse,
    'Is Gregor''s transformation an external shock (a force-majeure event) that any reasonable creditor would forgive, or is it interpreted as internal failure (shirking, self-harm, refusal) that justifies enforcement escalation?',
    'Analysis of the employer''s documented response; comparison with how contemporaneous legal systems classified insanity, disability, and magical transformation; literary evidence of creditor intent',
    'If external shock: constraint becomes rope with sunset (ε→0.25, has_sunset_clause). If interpreted as internal failure: constraint becomes snare with maximum suppression (ε=0.88, suppression→1.0). The classification depends on the creditor''s interpretation of causation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_as_external_shock_vs_internal_collapse, conceptual, 'Whether transformation is force-majeure or creditor-interpreted shirking').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metamorphosis_samsa, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_theater_t0, metamorphosis_samsa, theater_ratio, 0, 0.1).
narrative_ontology:measurement(meta_theater_t1, metamorphosis_samsa, theater_ratio, 1, 0.35).
narrative_ontology:measurement(meta_theater_t3, metamorphosis_samsa, theater_ratio, 3, 0.38).

% Extraction over time
narrative_ontology:measurement(meta_extract_t0, metamorphosis_samsa, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(meta_extract_t1, metamorphosis_samsa, base_extractiveness, 1, 0.72).
narrative_ontology:measurement(meta_extract_t3, metamorphosis_samsa, base_extractiveness, 3, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(metamorphosis_samsa, resource_allocation).
narrative_ontology:affects_constraint(metamorphosis_samsa, labor_market_vulnerability).
narrative_ontology:affects_constraint(metamorphosis_samsa, debt_trap_structural).
narrative_ontology:affects_constraint(metamorphosis_samsa, family_liability_law).

% DUAL FORMULATION NOTE:
% The metamorphosis is a triggering event, not the constraint itself. The underlying constraint is the debt-bondage structure: labor-dependent debtors have zero redundancy and are vulnerable to incapacity. Upstream constraints (labor market vulnerability, wage inadequacy) create the condition for bondage. Downstream constraints (family liability, property seizure law) enforce it. This story focuses on the moment of maximum visibility — the transformation — but the snare structure exists both before and after the metamorphosis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
