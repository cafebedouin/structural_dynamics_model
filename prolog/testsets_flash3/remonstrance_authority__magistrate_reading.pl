% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Right (Magistrate Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This constraint models the 'remonstrance right' of the French Parlements
 *   from the perspective of the magistracy, who viewed it as a fundamental
 *   constitutional mechanism to preserve ancient liberties against arbitrary
 *   royal innovation, particularly in fiscal matters. This reading emphasizes
 *   the Parlements' role as guardians of the law and the rights of subjects,
 *   even as it implicitly benefits their own corporate interests and tax
 *   exemptions. The constraint is framed as a Tangled Rope, acknowledging
 *   both its claimed coordination function (constitutional balance) and its
 *   asymmetric extraction (fiscal obstruction benefiting the magistracy).
 *
 * KEY AGENTS:
 *   - parlements_magistracy: Primary agenda-setter and beneficiary (institutional/identity_locked)
 *   - crown_fiscal_reforms: Primary target/payer (institutional/constrained)
 *   - taxable_population: Primary victim (powerless/trapped)
 *   - ancient_liberties_doctrine: Abstract beneficiary (analytical/analytical)
 *   - royal_ministers: Excluded actor (powerful/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.68).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.75).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Right (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '8aea0e77-23bf-40cc-8fdf-728d624d71f2').
narrative_ontology:cs_kernel_codification('8aea0e77-23bf-40cc-8fdf-728d624d71f2', formalized).
narrative_ontology:cs_authority_grounding('8aea0e77-23bf-40cc-8fdf-728d624d71f2', lineage).
narrative_ontology:cs_interpretation_layer_present('8aea0e77-23bf-40cc-8fdf-728d624d71f2').
narrative_ontology:cs_reading_relation('8aea0e77-23bf-40cc-8fdf-728d624d71f2', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('8aea0e77-23bf-40cc-8fdf-728d624d71f2', foundational, ancient_liberties_are_fundamental_law).
narrative_ontology:cs_axiom_status(ancient_liberties_are_fundamental_law, holdable).
narrative_ontology:cs_axiom_grounding('8aea0e77-23bf-40cc-8fdf-728d624d71f2', ancient_liberties_are_fundamental_law, deontological).
narrative_ontology:cs_axiom('8aea0e77-23bf-40cc-8fdf-728d624d71f2', foundational, parlements_are_guardians_of_fundamental_law).
narrative_ontology:cs_axiom_status(parlements_are_guardians_of_fundamental_law, holdable).
narrative_ontology:cs_axiom_grounding('8aea0e77-23bf-40cc-8fdf-728d624d71f2', parlements_are_guardians_of_fundamental_law, conventional).
narrative_ontology:cs_reference_frame('8aea0e77-23bf-40cc-8fdf-728d624d71f2', constitutional_balance_of_powers).
narrative_ontology:cs_drift_state('8aea0e77-23bf-40cc-8fdf-728d624d71f2', pre_french_revolution_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8aea0e77-23bf-40cc-8fdf-728d624d71f2', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlements_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, ancient_liberties_doctrine).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscal_reforms).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, taxable_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Parlements, composed of hereditary magistrates, claim the right to review and register royal edicts, particularly those concerning taxation. They see this as a constitutional duty to protect ancient liberties and fundamental laws, often blocking fiscal reforms that would affect their own tax exemptions or those of their social class. Their identity is fused with this role.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlements_magistracy, agenda_setter,
    institutional, generational, identity_locked, national).

% Royal edicts aimed at modernizing the tax system or raising revenue to address state debt are frequently blocked or delayed by the remonstrances. The Crown views these as necessary for national solvency, but they are often frustrated by the Parlements' resistance.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_fiscal_reforms, payer,
    institutional, immediate, constrained, national).

% The general population, particularly the commoners, bears the burden of an inefficient and inequitable tax system perpetuated by the Parlements' defense of traditional exemptions. They are victims of the fiscal stagnation caused by the remonstrances, even if some of the 'ancient liberties' nominally protect them.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, taxable_population, payer,
    powerless, biographical, trapped, national).

% The abstract concept of 'ancient liberties' and 'fundamental laws' is vindicated and reinforced by the Parlements' actions. This doctrine provides the ideological cover for the remonstrance right, even if its practical application benefits a narrow elite.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, ancient_liberties_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(remonstrance_authority__magistrate_reading, ancient_liberties_doctrine).

% The Crown's chief advisors and administrators, tasked with implementing royal policy and managing state finances. They are often frustrated by the Parlements' obstructionism, seeing it as a challenge to royal authority and an impediment to necessary reforms. They are excluded from the Parlements' internal deliberations.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, royal_ministers, excluded,
    powerful, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for judicial review of royal edicts, ensuring they conform to established laws and traditions, thereby theoretically coordinating royal power with constitutional principles and preventing arbitrary rule.
% TRANSFER_FUNCTION: Transfers the power to delay or block royal fiscal reforms from the Crown to the Parlements, effectively preserving the tax exemptions and privileges of the magistracy and other privileged groups, at the expense of the Crown's revenue and the broader taxable population.
% ABSENT_VOICES: The broader taxable population, who bear the costs of fiscal stagnation and inequity, are largely absent from the formal constitutional debate, their interests represented neither by the Crown (seeking revenue) nor the Parlements (defending privilege).
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished, the Crown would gain unchecked legislative power, particularly over fiscal matters. This would likely lead to rapid, potentially arbitrary, tax reforms, fundamentally altering the balance of power and the constitutional landscape of the ancien régime.
% FOUNDING_PROBLEM: To prevent arbitrary royal decrees and ensure that royal legislation respected the fundamental laws of the kingdom and the established rights and privileges of its subjects.
% FOUNDING_PROBLEM_CORROBORATION: The Parlements themselves and their supporters attest that the problem of arbitrary rule remains live. Historians and royal ministers, however, argue that by the 18th century, the right had largely devolved into a mechanism for the magistracy to protect its own corporate interests, rather than genuinely safeguarding the kingdom's fundamental laws, with the Crown's fiscal needs being the true casualty.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because the remonstrance right, in this reading, effectively blocks necessary fiscal reforms, perpetuating an inequitable tax system that benefits the magistracy at the expense of the state and the general population. Suppression (0.75) is also high, as the Crown's attempts to bypass or suppress the remonstrances (e.g., through lits de justice) were met with strong resistance, requiring active enforcement to overcome. The theater ratio (0.40) reflects that while the constitutional arguments were genuine, a significant portion of the Parlements' activity was performative resistance aimed at preserving their own privileges. The increasing trend in extractiveness and suppression over the interval reflects the escalating fiscal crisis and the hardening of positions between the Crown and the Parlements leading up to the French Revolution.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Parlements' magistracy, the remonstrance right is a legitimate constitutional check, a coordination mechanism to prevent tyranny. From the Crown's perspective (and the 'crown_reading' sibling), it is an illegitimate obstruction to necessary governance. The engine's classification will highlight this divergence, showing the magistracy as a beneficiary of a system that extracts from the Crown and the taxable population.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parlements' magistracy is a clear beneficiary (d near 0.0) as they use the right to protect their privileges and influence policy. The Crown's fiscal reforms are the primary target (d near 1.0) as they are directly obstructed. The taxable population are victims (d near 1.0) as they bear the costs of fiscal stagnation. The 'ancient_liberties_doctrine' is an abstract beneficiary, providing legitimacy without collecting rents. Royal ministers are excluded, unable to directly influence the Parlements' internal process.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope, rather than a pure Rope or Snare, prevents mislabeling. It acknowledges the historical claim of a genuine coordination function (constitutional balance) while simultaneously identifying the asymmetric extraction that developed over time (protection of corporate privilege). The 'contested' status of the founding problem further supports this hybrid classification, indicating a mandate that has arguably drifted from its original intent but is still defended on those grounds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_legitimacy_vs_self_interest,
    'To what extent did the Parlements'' exercise of the remonstrance right genuinely uphold ancient liberties for the common good, versus primarily serving the corporate self-interest and tax exemptions of the magistracy?',
    'Detailed historical analysis of specific remonstrances, correlating their content and timing with the fiscal interests of the magistracy and the broader impact on the population. Comparative studies with other European constitutional mechanisms of the era.',
    'If primarily self-serving, the extractiveness and suppression metrics would be re-evaluated as even higher, pushing the classification closer to a Snare. If genuinely for the common good, the coordination function would be emphasized, potentially lowering effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_legitimacy_vs_self_interest, empirical, 'Ambiguity between constitutional duty and corporate self-interest in the exercise of the remonstrance right.').

omega_variable(
    remonstrance_kernel_reading_ambiguity,
    'Is this constraint a genuine constitutional mechanism, or an illegitimate minoritarian veto?',
    'Resolution of the underlying kernel contest between the ''magistrate_reading'' and the ''crown_reading'' through historical consensus or a definitive legal-philosophical framework.',
    'If the ''crown_reading'' (illegitimate veto) were adopted, the constraint would be reclassified as a Snare, with the Parlements as the primary agenda-setters of extraction and the Crown as a victim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remonstrance_kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the ''remonstrance_authority'' kernel. The ''magistrate_reading'' views it as a fundamental constitutional mechanism, while the ''crown_reading'' views it as an illegitimate minoritarian veto. The disagreement is located in the fundamental nature and legitimacy of the right itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1650, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1650, remonstrance_authority__magistrate_reading, theater_ratio, 1650, 0.25).
narrative_ontology:measurement(remo_tr_t1680, remonstrance_authority__magistrate_reading, theater_ratio, 1680, 0.3).
narrative_ontology:measurement(remo_tr_t1710, remonstrance_authority__magistrate_reading, theater_ratio, 1710, 0.33).
narrative_ontology:measurement(remo_tr_t1740, remonstrance_authority__magistrate_reading, theater_ratio, 1740, 0.36).
narrative_ontology:measurement(remo_tr_t1770, remonstrance_authority__magistrate_reading, theater_ratio, 1770, 0.38).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__magistrate_reading, theater_ratio, 1789, 0.4).

% Extraction over time
narrative_ontology:measurement(remo_be_t1650, remonstrance_authority__magistrate_reading, base_extractiveness, 1650, 0.55).
narrative_ontology:measurement(remo_be_t1680, remonstrance_authority__magistrate_reading, base_extractiveness, 1680, 0.6).
narrative_ontology:measurement(remo_be_t1710, remonstrance_authority__magistrate_reading, base_extractiveness, 1710, 0.63).
narrative_ontology:measurement(remo_be_t1740, remonstrance_authority__magistrate_reading, base_extractiveness, 1740, 0.65).
narrative_ontology:measurement(remo_be_t1770, remonstrance_authority__magistrate_reading, base_extractiveness, 1770, 0.67).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__magistrate_reading, base_extractiveness, 1789, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1650, remonstrance_authority__magistrate_reading, suppression_requirement, 1650, 0.6).
narrative_ontology:measurement(remo_su_t1680, remonstrance_authority__magistrate_reading, suppression_requirement, 1680, 0.65).
narrative_ontology:measurement(remo_su_t1710, remonstrance_authority__magistrate_reading, suppression_requirement, 1710, 0.68).
narrative_ontology:measurement(remo_su_t1740, remonstrance_authority__magistrate_reading, suppression_requirement, 1740, 0.7).
narrative_ontology:measurement(remo_su_t1770, remonstrance_authority__magistrate_reading, suppression_requirement, 1770, 0.73).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__magistrate_reading, suppression_requirement, 1789, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('magistrate_reading') of the 'remonstrance_authority' kernel. The sibling reading is 'crown_reading'. Each reading instantiates a distinct constraint with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
