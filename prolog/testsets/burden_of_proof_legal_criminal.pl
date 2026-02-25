% ============================================================================
% CONSTRAINT STORY: burden_of_proof_legal_criminal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burden_of_proof_legal_criminal, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: burden_of_proof_legal_criminal
 *   human_readable: "Beyond a Reasonable Doubt" (Criminal Legal Burden)
 *   domain: political/social
 *
 * SUMMARY:
 *   In common law criminal systems, the burden of proof rests on the
 *   prosecution to prove guilt 'beyond a reasonable doubt.' This constraint
 *   is a foundational principle designed to protect individuals from the
 *   overwhelming power of the state by prioritizing the prevention of
 *   wrongful convictions (false positives) over the risk of wrongful
 *   acquittals (false negatives). While it serves a critical coordination
 *   function in legitimizing state power and protecting civil liberties, it
 *   simultaneously creates an extractive cost borne by the victims of crime
 *   and society at large when factually guilty individuals are not convicted.
 *
 * KEY AGENTS:
 *   - Accused Individuals: Primary beneficiary (powerless/trapped) — protected from state overreach.
 *   - Victims of Crime: Primary victim (moderate/constrained) — may be denied legal resolution and justice.
 *   - Society at Large: Secondary victim (organized/mobile) — bears the risk of unconvicted criminals.
 *   - The State (Prosecution & Judiciary): Institutional actor (institutional/constrained) — must adhere to the standard, gaining legitimacy but bearing a high operational burden.
 *   - Civil Liberties Advocates: Secondary beneficiary (organized/mobile) — see the constraint as a core pillar of a just society.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burden_of_proof_legal_criminal, 0.35).
domain_priors:suppression_score(burden_of_proof_legal_criminal, 0.8).
domain_priors:theater_ratio(burden_of_proof_legal_criminal, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, extractiveness, 0.35).
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burden_of_proof_legal_criminal, tangled_rope).
narrative_ontology:human_readable(burden_of_proof_legal_criminal, "\"Beyond a Reasonable Doubt\" (Criminal Legal Burden)").
narrative_ontology:topic_domain(burden_of_proof_legal_criminal, "political/social").

domain_priors:requires_active_enforcement(burden_of_proof_legal_criminal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(burden_of_proof_legal_criminal, accused_individuals).
narrative_ontology:constraint_beneficiary(burden_of_proof_legal_criminal, civil_liberties_advocates).
narrative_ontology:constraint_victim(burden_of_proof_legal_criminal, victims_of_crime).
narrative_ontology:constraint_victim(burden_of_proof_legal_criminal, society_at_large).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ACCUSED (ROPE) — For the individual facing the power of the state, the high burden of proof is a pure coordination mechanism for their protection. It functions as a shield, with no perceived extraction. As a beneficiary with trapped exit, their derived directionality 'd' is low, resulting in a low effective extraction (χ) that classifies as Rope.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE VICTIM (SNARE) — For the victim of a crime, an acquittal due to the high standard of proof represents a pure extraction of justice. The system, from this view, fails to provide resolution and suppresses their claim. As a victim with constrained exit, their derived 'd' is high, leading to a high χ. Combined with high structural suppression (0.80), this classifies as a Snare.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE STATE (TANGLED ROPE) — The state is constrained to operate within this rule. It benefits from the legitimacy the rule provides, but is also a 'victim' of the high operational burden it imposes. This dual role results in a moderate 'd', and the classification reflects the mixed nature of the constraint: a necessary coordination rule that also entails significant costs and limitations.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The analyst sees the full structure: a genuine coordination function (protecting individuals from state power) paired with a genuine extractive function (the cost to victims and society of false negatives). It requires active enforcement and has clear beneficiaries and victims, meeting all gates for a Tangled Rope classification. This is the system's claimed type.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: NATURAL LAW THEORIST (MOUNTAIN) — This perspective naturalizes the principle, viewing it as an immutable, universal law of justice ('better that ten guilty men go free...'). However, the engine will detect this as a false summit. The constraint's base properties (ε=0.35, suppression=0.80, requires_active_enforcement=true) are fundamentally inconsistent with the Mountain classification, revealing that what is framed as a natural law is in fact a contingent, enforced, and extractive social technology.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_legal_criminal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(burden_of_proof_legal_criminal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.35): Moderate. This value represents the societal cost of false negatives—acquittals of factually guilty parties that would not have occurred under a lower standard. It is not a direct financial extraction but an extraction of security and legal finality from victims and society. Suppression (0.80): High. The standard is non-negotiable within the criminal legal system. Neither the accused, the victim, nor the state can opt for an alternative standard of proof in a criminal trial. Theater Ratio (0.25): Low. While courtroom proceedings can be theatrical, the constraint itself is a core, functional rule of legal procedure, not a performative ritual.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the accused, it is a pure Rope, a protective shield. For the victim, it is a Snare, a mechanism that extracts justice and leaves them without recourse. For the state, it is a Tangled Rope, a legitimizing but burdensome rule. The analytical observer also sees a Tangled Rope, recognizing the inherent trade-off between its coordination and extraction functions. A philosophical observer might mistake it for a Mountain, but its high suppression and moderate extraction reveal its constructed, non-natural character.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality 'd' is derived from the structural relationships. The 'accused_individuals' are beneficiaries, leading to a low 'd' and a Rope classification. The 'victims_of_crime' are victims, leading to a high 'd', which, combined with high suppression, results in a Snare classification. The State and analytical observers occupy intermediate positions, correctly identifying the mixed Tangled Rope nature of the constraint. The system correctly models how the same legal principle functions as a shield for one party and a weapon against another.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic example of resolving mandatrophy. A naive analysis might label the standard as purely 'good' (Rope) or purely 'bad' (Snare). Deferential Realism shows both are valid, indexical truths. The system is not broken because the victim perceives a Snare; the system is *defined* by the fact that the accused's Rope *is* the victim's Snare. The Tangled Rope classification from the analytical view correctly captures this irreducible duality, preventing the mislabeling of a complex trade-off as a simple coordination problem or a simple case of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burden_of_proof_legal_criminal, 1790, 2040).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burden_of_proof_legal_criminal, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
