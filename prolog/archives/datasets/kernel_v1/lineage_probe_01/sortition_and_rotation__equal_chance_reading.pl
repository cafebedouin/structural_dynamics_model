% ============================================================================
% CONSTRAINT STORY: sortition_and_rotation__equal_chance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sortition_and_rotation__equal_chance_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sortition_and_rotation__equal_chance_reading
 *   human_readable: Sortition as Equal-Access Coordination (Equal Chance Reading)
 *   domain: political_theory/democratic_institutions
 *
 * SUMMARY:
 *   The equal-chance reading of sortition grounds the legitimacy of
 *   democratic office-holding in a single principle: every citizen has
 *   literally identical non-zero probability of selection, regardless of
 *   wealth, rhetorical skill, family connection, professional credential, or
 *   prior political experience. This reading operationalizes formal equality
 *   via mechanism — the lottery suppresses all selection gradients at a
 *   structural level. Unlike the anti_professional_reading (which emphasizes
 *   the prevention of political class formation) or the
 *   strategic_exception_reading (which carves out competence-based exceptions
 *   for certain offices), the equal-chance reading takes the flat
 *   distribution of access as both the mechanism and the value. The
 *   constraint instantiates this reading as a rope (pure coordination) from
 *   most perspectives because sortition solves a legitimate coordination
 *   problem: how to select representatives in a way that prevents accumulated
 *   power from stratifying into an elite. The ordinary citizen's access is
 *   genuinely equal; the democratic assembly operationalizes this value; the
 *   analytical observer sees low extraction because the mechanism distributes
 *   office-holding access flat. Only from the perspective of the skilled or
 *   wealthy agent does the constraint appear as tangled rope — coordination
 *   that prevents the agent's advantage from converting to political power.
 *
 * KEY AGENTS:
 *   - Statistically Ordinary Citizen: Primary beneficiary (powerless/mobile) — sortition grants equal baseline access to office regardless of advantage or disadvantage
 *   - Skilled/Wealthy Agent: Mixed beneficiary and victim (moderate/constrained) — benefits from democratic legitimacy and rotation but cannot convert advantage into political power
 *   - Democratic Assembly (Sortition Operator): Institutional actor (institutional/arbitrage) — operationalizes the equal-chance principle; experiences sortition as coordination mechanism for preventing power stratification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — evaluates whether the equal-chance reading coherently operationalizes equality or conceals hidden selection gradients or competence-based exceptions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sortition_and_rotation__equal_chance_reading, 0.15).
domain_priors:suppression_score(sortition_and_rotation__equal_chance_reading, 0.08).
domain_priors:theater_ratio(sortition_and_rotation__equal_chance_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sortition_and_rotation__equal_chance_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(sortition_and_rotation__equal_chance_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(sortition_and_rotation__equal_chance_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sortition_and_rotation__equal_chance_reading, rope).
narrative_ontology:human_readable(sortition_and_rotation__equal_chance_reading, "Sortition as Equal-Access Coordination (Equal Chance Reading)").
narrative_ontology:topic_domain(sortition_and_rotation__equal_chance_reading, "political_theory/democratic_institutions").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sortition_and_rotation__equal_chance_reading, '7b671e62-3bfb-41ed-b451-1c51708c7dac').
narrative_ontology:cs_kernel_codification('7b671e62-3bfb-41ed-b451-1c51708c7dac', formalized).
narrative_ontology:cs_authority_grounding('7b671e62-3bfb-41ed-b451-1c51708c7dac', practice).
narrative_ontology:cs_interpretation_layer_present('7b671e62-3bfb-41ed-b451-1c51708c7dac').
narrative_ontology:cs_reading_relation('7b671e62-3bfb-41ed-b451-1c51708c7dac', sortition_and_rotation__anti_professional_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b671e62-3bfb-41ed-b451-1c51708c7dac', sortition_and_rotation__strategic_exception_reading, coexists_with).
narrative_ontology:cs_axiom('7b671e62-3bfb-41ed-b451-1c51708c7dac', foundational, literal_equal_access_principle).
narrative_ontology:cs_axiom_status(literal_equal_access_principle, holdable).
narrative_ontology:cs_axiom_grounding('7b671e62-3bfb-41ed-b451-1c51708c7dac', literal_equal_access_principle, deontological).
narrative_ontology:cs_axiom('7b671e62-3bfb-41ed-b451-1c51708c7dac', foundational, selection_gradient_suppression_necessary).
narrative_ontology:cs_axiom_status(selection_gradient_suppression_necessary, holdable).
narrative_ontology:cs_axiom_grounding('7b671e62-3bfb-41ed-b451-1c51708c7dac', selection_gradient_suppression_necessary, instrumental).
narrative_ontology:cs_reference_frame('7b671e62-3bfb-41ed-b451-1c51708c7dac', athenian_democratic_lot).
narrative_ontology:cs_drift_state('7b671e62-3bfb-41ed-b451-1c51708c7dac', contemporary_sortition_revival, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('7b671e62-3bfb-41ed-b451-1c51708c7dac', '').
narrative_ontology:cs_kernel_id(sortition_and_rotation__equal_chance_reading, sortition_and_rotation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sortition_and_rotation__equal_chance_reading, statistically_ordinary_citizen).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ORDINARY CITIZEN (ROPE) — Sortition grants literal equal access to rule regardless of wealth, rhetorical skill, family connection, or prior political experience. No selection gradient suppresses this agent's candidacy. Exit is mobile — the citizen can decline office if selected, but the baseline access is genuinely equal. Perceived as pure coordination: the mechanism that ensures rotation and equal access. Low extraction, minimal theater.
constraint_indexing:constraint_classification(sortition_and_rotation__equal_chance_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: THE DEMOCRATIC ASSEMBLY (ROPE) — Sortition operator (the institutional body managing the lottery) experiences the constraint as pure coordination: solving the legitimacy problem of how to select representatives without enabling wealth or charisma to accumulate power. The assembly's authority is grounded in the equal-access principle. Exit is arbitrage — the assembly can switch mechanisms, but within the equal-chance reading, sortition is the instrumentally endorsed mechanism for operationalizing the equality value. No extraction perceived; the mechanism directly instantiates the assembly's core commitment.
constraint_indexing:constraint_classification(sortition_and_rotation__equal_chance_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE) — From a civilizational/global frame, sortition instantiates formal equality via mechanism: every citizen has the same non-zero probability of office. Theater is minimal — the lottery is the function, not a cover for hidden selection. Extraction is low because the mechanism distributes access flatly. The analytical perspective recognizes that effective sortition requires suppression of all selection gradients (wealth bias, rhetorical advantage, professional credential) — this suppression is structural, not coercive. The equal-chance reading takes this suppression as the core value: the mechanism works by eliminating bases of distinction.
constraint_indexing:constraint_classification(sortition_and_rotation__equal_chance_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SKILLED OR WEALTHY AGENT (TANGLED ROPE) — Agents with rhetorical skill, wealth, or professional expertise experience sortition as mixed coordination and extraction. Coordination: the lottery ensures legitimacy and rotation, preventing any one faction from monopolizing office. Extraction: the mechanism suppresses the agent's advantage — their skill or wealth cannot be converted to political power. Exit is constrained — the agent remains a citizen and can be selected, but their conversion pathways (persuasion, funding, credential) are closed within office. Moderate extraction because the agent still benefits from the democratic system and can exercise influence through other channels. Requires active enforcement: the suppression of selection gradients must be maintained against incentives to game the system.
constraint_indexing:constraint_classification(sortition_and_rotation__equal_chance_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sortition_and_rotation__equal_chance_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sortition_and_rotation__equal_chance_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sortition_and_rotation__equal_chance_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(sortition_and_rotation__equal_chance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The equal-chance reading minimizes extraction because it distributes access flatly — no agent captures disproportionate office-holding opportunity. The measurement trajectory (0.10 → 0.15 → 0.18) reflects a slight drift as systems accumulate exceptions or competence gates, but the core principle maintains low extractiveness. From the ordinary citizen's perspective, extractiveness approaches zero (the mechanism grants equal access). From the skilled agent's perspective, extractiveness is moderate (suppression of advantage), but the overall constraint's extractiveness averages to 0.15 because the suppression serves the coordination function of preventing power stratification. Suppression (0.08): Very low. The suppression of selection gradients is structural, not coercive — it is enforced by the mechanical design of sortition, not by active policing of deviance. No agent is prevented from *attempting* to influence an election or to accumulate political capital outside office; the suppression affects only the pathway from wealth/skill to office. The suppression is clean and mechanical rather than requiring enforcement machinery. Theater ratio (0.25): Low. Sortition is substantially functional — the lottery is the selection mechanism, not a cover for hidden selection. Theater rises slightly over time (0.20 → 0.30) as systems develop administrative and procedural complexity around the lottery, but the core function remains transparent. The equal-chance reading explicitly rejects the claim that sortition is theater (that would belong to the piton reading, which does not apply here).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is between the ordinary citizen (who sees pure coordination and equal access) and the skilled agent (who sees coordination that suppresses their advantage). Both perspectives classify as rope or tangled rope, but with different directionalities and different experienced extractiveness. The ordinary citizen experiences sortition as a mechanism that grants access they would not have through election or wealth. The skilled agent experiences sortition as a mechanism that suppresses a pathway (conversion of advantage to power) that would be open in alternative systems. The analytical observer recognizes both experiences as legitimate readings of the same structural phenomenon — the equal-chance reading operationalizes formal equality by suppressing selection gradients, which is both a benefit (access for the ordinary) and a constraint (advantage suppression for the skilled). This perspectival gap is not a defect in the reading; it is the core feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural relationship to the constraint. The ordinary citizen has d ≈ 0.2 (beneficiary with mobile exit: can decline office but gains equal access). The skilled agent has d ≈ 0.6 (mixed: benefits from democratic legitimacy but loses advantage conversion pathways). The assembly has d ≈ 0.1 (beneficiary with arbitrage: operates the mechanism and endorses it). The analytical observer has d ≈ 0.72 (sees full structure). These directionalities reflect that sortition distributes extractiveness unevenly across agents — the beneficiary (ordinary citizen) experiences low or negative χ; the constrained agent (skilled) experiences moderate χ; the analytical observer sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The equal-chance reading resolves the mandatrophy by taking a single principle (literal equal access) as the core value and deriving all downstream consequences from it. The mechanism (sortition) is transparent — it operationalizes the value without theater or hidden extraction. The suppression of selection gradients is not coercive but structural. The constraint classifies as rope from the perspectives of the ordinary citizen and the assembly (pure coordination) and as tangled rope from the skilled agent's perspective (mixed coordination and suppression of advantage). The analytical observer sees rope at the civilizational scale because the constraint solves a fundamental political problem: how to prevent accumulated power from stratifying into an elite. The mandatrophy is avoided by distinguishing this reading from the anti_professional_reading (which emphasizes prevention of political class formation as the core value, leading to different conclusions about professionalization and competence) and from the strategic_exception_reading (which carves out competence exceptions, leading to tangled_rope or snare classifications for the skilled agent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equal_access_versus_competence_tradeoff,
    'Does literalizing equal access — identical non-zero probability for every citizen — necessarily suppress all competence-based or expertise-based selection? Or can sortition be combined with minimal competence thresholds without violating the equal-chance reading?',
    'Historical examination of sortition systems (Athenian, Venetian, modern jury selection): what competence gates existed? Did competence gates preserve the equal-access principle or establish a hidden selection gradient? Analysis of Condorcet jury theorem: does the diversity of ordinary judgment preserve functionality without credential-based selection?',
    'If competence gates are incompatible with equal-chance reading: extractiveness remains 0.15 (pure coordination). If competence gates can coexist with formal equal access (e.g., literacy requirement, basic civic knowledge): a sibling reading (strategic_exception_reading) gains coherence, and the equal_chance_reading''s claim to operationalize equality requires qualification (extractiveness rises to 0.25-0.35).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equal_access_versus_competence_tradeoff, conceptual, 'Tradeoff between equal access and competence-based selection').

omega_variable(
    suppression_mechanism_vs_natural_equality,
    'Is the suppression of selection gradients (0.08) a feature of sortition that operationalizes pre-existing equality, or a mechanism that *creates* artificial equality by suppressing natural variation in competence and skill?',
    'Normative clarification: does the equal-chance reading endorse suppression as enforcement of a value (equality), or as elimination of a corruption (the conversion of power into influence)? Does the reading distinguish between suppressing illegitimate bases of distinction (wealth, rhetoric, family) and legitimate ones (knowledge, experience)? Empirical test: post-sortition performance data — do ordinary citizens perform worse at governing, or do credential-holders just assume they do? Does citizen-perceived competence differ from institutional-outcome measures?',
    'If suppression is enforcement of a value: the reading is coherent; extractiveness from the skilled agent''s perspective is accurate (0.25-0.35 in perspective 4). If suppression is artificial elimination of legitimate skill: the equal-chance reading collapses into the strategic_exception_reading (competence exceptions restore the framework), and a new omega emerges (is the equal-chance reading even defensible?). If performance is neutral or better with ordinary citizens: the reading is strengthened; skilled agents accept their disadvantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_vs_natural_equality, empirical, 'Whether suppression enforces equality or artificially eliminates skill').

omega_variable(
    rotation_durability_without_professionalism,
    'Does the equal-chance reading require that office-holders be non-professional (part-time, term-limited, amateur)? Or can sortition select professional administrators while preserving equal-access principle?',
    'Historical analysis: did Athenian sortition require amateur governance? Modern examples: jury systems select ordinary citizens but operate within professional legal structures. Can sortition coexist with permanent professional bureaucracy, or does professionalization necessarily enable selection gradients and political class formation?',
    'If professional roles are incompatible with equal-chance reading: extractiveness remains 0.15 (pure coordination of amateur rotation). If professionalization can coexist: the anti_professional_reading becomes a distinct reading rather than a logical consequence of equal-chance, and the strategic_exception_reading''s claim (competence exceptions for certain offices) becomes more plausible. The sibling reading relationships shift: coexistence becomes more coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rotation_durability_without_professionalism, conceptual, 'Compatibility of sortition with professional governance structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sortition_and_rotation__equal_chance_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sortition_eq_tr_t0, sortition_and_rotation__equal_chance_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sortition_eq_tr_t5, sortition_and_rotation__equal_chance_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(sortition_eq_tr_t10, sortition_and_rotation__equal_chance_reading, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(sortition_eq_be_t0, sortition_and_rotation__equal_chance_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sortition_eq_be_t5, sortition_and_rotation__equal_chance_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(sortition_eq_be_t10, sortition_and_rotation__equal_chance_reading, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sortition_and_rotation__equal_chance_reading, identity_coordination).
narrative_ontology:affects_constraint(sortition_and_rotation__equal_chance_reading, sortition_and_rotation__anti_professional_reading).
narrative_ontology:affects_constraint(sortition_and_rotation__equal_chance_reading, sortition_and_rotation__strategic_exception_reading).

% DUAL FORMULATION NOTE:
% The sortition_and_rotation kernel contains three structurally distinct readings: anti_professional (emphasizes political class prevention), equal_chance (emphasizes formal equality), and strategic_exception (emphasizes competence gradients in high-stakes offices). Each reading is a separate constraint with different ε values and different beneficiary/victim structures. The equal_chance_reading has ε ≈ 0.15 (pure coordination); the anti_professional_reading has ε ≈ 0.08 (coordination with stronger emphasis on preventing accumulation); the strategic_exception_reading has ε ≈ 0.45-0.55 (tangled rope or snare, depending on whether competence exceptions dominate). All three readings are linked via network.affects_constraints and documented in their respective omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
