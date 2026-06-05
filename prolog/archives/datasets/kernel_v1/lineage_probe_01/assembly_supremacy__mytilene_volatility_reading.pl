% ============================================================================
% CONSTRAINT STORY: assembly_supremacy__mytilene_volatility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_assembly_supremacy__mytilene_volatility_reading, []).

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
 *   constraint_id: assembly_supremacy__mytilene_volatility_reading
 *   human_readable: Assembly Supremacy: Mytilene Volatility Reading (Government by the Speed of Regret)
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   In 427 BCE, the Athenian assembly voted to massacre the entire male
 *   population of Mytilene and enslave the women and children in retaliation
 *   for revolt. The decree was binding. Ships carried the order to the
 *   general commanding in Mytilene. But the next day — or within hours,
 *   according to some accounts — speakers rose to reverse the vote. Diodotus
 *   argued for clemency; the assembly was swayed; a second vote was held; the
 *   first decree was rescinded; a second ship raced to catch the first and
 *   countermand the massacre. By a margin of time and rhetoric, Mytilene was
 *   spared. This event instantiates the Mytilene volatility reading of
 *   assembly supremacy: government by the speed of regret. The reading
 *   interprets this constraint as structural — not a defect to be fixed but a
 *   feature that reveals what radical assembly sovereignty actually costs.
 *   The constraint is extractive because it puts the lives of other peoples
 *   and the reversibility of violent acts contingent on whether a better
 *   speaker emerges within hours, whether that speaker's rhetoric performs
 *   well on the day, and whether the population that voted the first massacre
 *   can be moved to reverse it. The suppression is extreme because once the
 *   vote is cast and the decree goes out, the only exit from the consequences
 *   is immediate replacement by contrary speech — there is no institutional
 *   distance, no cooling-off period, no formal procedure for reconsideration.
 *   The beneficiary is the eloquent advocate who can reverse the outcome by
 *   superior oratory; the victims are the condemned, the minority voices
 *   suppressed in the first vote, and all future decisions that require a
 *   calmer hour than assembly rhetoric can provide.
 *
 * KEY AGENTS:
 *   - Mytilene Assembly & Population (victim/trapped) — Subject to both massacre decree and its potential reversal based on Athenian assembly rhetoric; no capacity to influence the speed or quality of reconsideration
 *   - Cleon (beneficiary/institutional) — Eloquent advocate whose speech carries the massacre vote; benefits from rhetorical supremacy at a particular moment
 *   - Diodotus (beneficiary/institutional) — Eloquent advocate whose speech reverses the massacre; benefits from rhetorical supremacy in the second vote; his eloquence becomes the escape hatch from the first vote
 *   - Athenian Assembly / Demos (powerful/mobile) — Holds absolute authority to vote and reverse; experiences constraint as self-rule without remainder but also as volatility dependency
 *   - The Courier Racing to Mytilene (powerless/trapped) — Structurally carries the consequences of assembly volatility; races against time to deliver the reversal; bears extraction cost of rhetorical speed
 *   - Analytical Observer / Constitutional Theorist (analytical/analytical) — Views the event as evidence of either assembly radicalness or assembly recklessness depending on interpretive frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(assembly_supremacy__mytilene_volatility_reading, 0.68).
domain_priors:suppression_score(assembly_supremacy__mytilene_volatility_reading, 0.72).
domain_priors:theater_ratio(assembly_supremacy__mytilene_volatility_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(assembly_supremacy__mytilene_volatility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(assembly_supremacy__mytilene_volatility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(assembly_supremacy__mytilene_volatility_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(assembly_supremacy__mytilene_volatility_reading, snare).
narrative_ontology:human_readable(assembly_supremacy__mytilene_volatility_reading, "Assembly Supremacy: Mytilene Volatility Reading (Government by the Speed of Regret)").
narrative_ontology:topic_domain(assembly_supremacy__mytilene_volatility_reading, "legal/political/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(assembly_supremacy__mytilene_volatility_reading, 'b6b56070-3b2c-4bfd-817c-6386700c10f1').
narrative_ontology:cs_kernel_codification('b6b56070-3b2c-4bfd-817c-6386700c10f1', fixed_text).
narrative_ontology:cs_authority_grounding('b6b56070-3b2c-4bfd-817c-6386700c10f1', distributed).
narrative_ontology:cs_reading_relation('b6b56070-3b2c-4bfd-817c-6386700c10f1', assembly_supremacy__nomothetai_maturation_reading, influences).
narrative_ontology:cs_reading_relation('b6b56070-3b2c-4bfd-817c-6386700c10f1', assembly_supremacy__radical_self_rule_reading, coexists_with).
narrative_ontology:cs_axiom('b6b56070-3b2c-4bfd-817c-6386700c10f1', foundational, immediate_assembly_voting_structurally_extractive).
narrative_ontology:cs_axiom_status(immediate_assembly_voting_structurally_extractive, holdable).
narrative_ontology:cs_axiom_grounding('b6b56070-3b2c-4bfd-817c-6386700c10f1', immediate_assembly_voting_structurally_extractive, empirically_contingent).
narrative_ontology:cs_axiom('b6b56070-3b2c-4bfd-817c-6386700c10f1', foundational, suppression_inherent_to_unfiltered_assembly).
narrative_ontology:cs_axiom_status(suppression_inherent_to_unfiltered_assembly, holdable).
narrative_ontology:cs_axiom_grounding('b6b56070-3b2c-4bfd-817c-6386700c10f1', suppression_inherent_to_unfiltered_assembly, empirically_contingent).
narrative_ontology:cs_reference_frame('b6b56070-3b2c-4bfd-817c-6386700c10f1', immediate_assembly_supremacy).
narrative_ontology:cs_drift_state('b6b56070-3b2c-4bfd-817c-6386700c10f1', fourth_century_nomothetai_reforms, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b6b56070-3b2c-4bfd-817c-6386700c10f1', '2026-02-26T14:22:00Z').
narrative_ontology:cs_kernel_id(assembly_supremacy__mytilene_volatility_reading, assembly_supremacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(assembly_supremacy__mytilene_volatility_reading, rhetorical_majority).
narrative_ontology:constraint_beneficiary(assembly_supremacy__mytilene_volatility_reading, last_speakers_of_the_day).
narrative_ontology:constraint_victim(assembly_supremacy__mytilene_volatility_reading, irrevocable_decisions).
narrative_ontology:constraint_victim(assembly_supremacy__mytilene_volatility_reading, minority_positions).
narrative_ontology:constraint_victim(assembly_supremacy__mytilene_volatility_reading, future_regret_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MYTILENE CONDEMNED (SNARE) — The voted-upon population has no exit from the decision. The vote happened. The decree went out. The condemned experience maximum extraction: lives gambled on the assembly's rhetorical environment at the moment of voting, with reversal dependent on whether a second speaker can out-perform the first. This is not coordination — it is pure extraction of irreversible consequences contingent on the quality of oratory on a particular day.
constraint_indexing:constraint_classification(assembly_supremacy__mytilene_volatility_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ATHENIAN CITIZEN (SNARE) — Any citizen might find themselves in the position of Mytilene: subject to an immediate, binding vote where reversal depends on whether a second speaker emerges and whether their rhetoric moves the same assembly hours later. Suppression is extreme because the only exit is participation in the same assembly that voted the original decree — you cannot opt out of being governed by the speed of regret. Extractiveness derives from the irreversibility of the first vote coupled with the absence of deliberative distance.
constraint_indexing:constraint_classification(assembly_supremacy__mytilene_volatility_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ELOQUENT ADVOCATE (ROPE) — The speaker who can reverse a decree through superior oratory benefits from this arrangement. They experience the constraint as coordination: the system enables them to perform the function of better judgment by allowing them to address the assembly again. For institutional actors positioned to benefit from rhetorical advantage in a second vote, this is a coordination mechanism — it solves the problem of how to give hearing to alternative views when the first vote has already occurred.
constraint_indexing:constraint_classification(assembly_supremacy__mytilene_volatility_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FOURTH-CENTURY REFORM COALITION (TANGLED ROPE) — Later Athenians (the nomothetai reading's position) view this arrangement as a genuine problem requiring institutional filter. They see coordination value in having assembly decisions subject to procedural constraints, but they also recognize that radical assembly sovereignty without deliberative distance is extractive. Their response — the nomothetai — introduces deliberative distance as a coordination mechanism (roads to second thoughts) while retaining assembly authority. From this perspective, Mytilene shows both why coordination is needed AND why the unfiltered system has real extraction costs.
constraint_indexing:constraint_classification(assembly_supremacy__mytilene_volatility_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CIVILIZATIONAL ANALYST (MOUNTAIN) — From the longest view, immediate assembly voting without deliberative filters is a structural feature of radical democracy — not a bug, but an inherent condition. The constraint appears immutable: whenever a populus governs itself directly without institutional mediation, decisions will be subject to rapid reversal based on changed rhetoric, changed attendance, or changed moods. This perspective naturalizes volatility as the price of unmediated self-rule. However, the engine's false-summit detector will flag this: Mytilene shows that the volatility is contingent on institutional design (lack of deliberative delay), not inherent to democracy itself.
constraint_indexing:constraint_classification(assembly_supremacy__mytilene_volatility_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(assembly_supremacy__mytilene_volatility_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(assembly_supremacy__mytilene_volatility_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(assembly_supremacy__mytilene_volatility_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(assembly_supremacy__mytilene_volatility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(assembly_supremacy__mytilene_volatility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint gambles irreversible acts (massacres, enslavements, death decrees) on the speed and quality of rhetoric in a single sitting. Extractiveness rises sharply (0.55 → 0.68 → 0.72) as the material consequences of the vote accumulate and the messenger races against reversal. Suppression (0.72): Very high. The assembled demos has no procedural way to reconsider its own decision except through a new speech by a new speaker. There are no deliberative buffers, no cooling-off periods, no formal appeals process. Exit from the consequences of the first vote is possible only through immediate rhetorical counter-action. The condemned have no exit at all. The suppression arises not from coercion imposed by an external authority but from the structure of immediate assembly voting itself — the demos suppresses its own exit from its own decisions. Theater ratio (0.38): Moderate-low. The speeches in Thucydides' account (Cleon and Diodotus) engage substantive arguments about justice, security, and long-term Athenian interest. The constraint is not primarily performative — it is substantive disagreement about policy with real life-and-death consequences. However, the theater rises slightly as the event unfolds because the second vote's reversal depends on rhetorical re-persuasion of the same assembly by a new speaker, which adds an element of debate-as-performance. The critical point: theater is low relative to extractiveness. The extractiveness does not derive from theatrical performance but from the absence of deliberative delay.
 *
 * PERSPECTIVAL GAP:
 *   The powerless agent (the condemned of Mytilene) experiences this as pure snare: their lives are extracted and gambled on assembly rhetoric, with no procedural exit. The eloquent beneficiaries (Cleon and Diodotus) experience coordination: their speeches solve a legitimate problem (how to reconsider a decision once made) and enable better judgment. The assembly itself experiences self-rule: it retains the power to reverse its own decisions, which proves sovereignty. The fourth-century reformer (the nomothetai perspective) sees this as evidence that some institutional delay is necessary for coordination to work — the constraint shows why filtering through deliberative procedure improves outcomes. The civilizational analyst risks seeing this as inherent to assembly government — that volatility is the price of radical democracy — but the structure reveals that volatility is a choice about institutional design (how much deliberative distance to build in), not an immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to extraction flow. The condemned (trapped, powerless) have d ≈ 0.95 (full target). The eloquent advocates (institutional, arbitrage) have d ≈ 0.10 (partial beneficiary — they benefit from rhetorical advantage but lack control over assembly attendance and mood). The assembly (powerful, mobile) has d ≈ 0.50 (symmetric). The analytical observer has d ≈ 0.72 (analytical default). The chi formula χ = ε × f(d) × σ(S) produces highest chi for the trapped agents at local scope (where assembly volatility is most extractive) and lowest chi for the beneficiaries at local scope (where rhetorical advantage provides exit). The scope modifier σ(local=0.8) dampens extraction at local level, but the core extractiveness is so high that chi remains severe even dampened.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED VIA READING SPECIFICATION: This constraint has extractiveness 0.68 > 0.70, which triggers the mandatrophy gate. The mandatrophy is resolved by recognizing that the constraint is ONE READING of a contested kernel, not an attempt to force all readings into a single classification. The Mytilene volatility reading specifies that assembly supremacy, when instantiated as immediate voting without deliberative delay, IS extractive (Snare). The alternative readings (nomothetai maturation, radical self-rule) would classify the same constraint differently under different institutional conditions or different normative frameworks. The mandatrophy dissolves when we recognize that 'assembly supremacy' is not a single, fixed constraint — it is a contested kernel with multiple structural instantiations. The volatility reading shows one instantiation (extractive); the others would show different instantiations. The constraint is not ambiguous; the kernel is. This story specifies one reading completely and coherently. The contradictions with the other readings are resolved in those stories, where different institutional designs, axioms, and structural conditions produce different classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rhetoric_versus_judgment_distinction,
    'Is the quality of oratory a proxy for better judgment, or does rhetorical advantage systematically diverge from epistemic or moral correctness?',
    'Historical analysis of assembly reversals in Athens: track which reversals corrected genuine errors vs. which merely substituted popular speakers for less popular ones; examine outcome data for reversed vs. upheld decisions',
    'If rhetoric correlates with better judgment: the constraint is coordination (second chance at error correction). If rhetoric diverges from judgment: the constraint is extraction (the louder voice wins, regardless of correctness). If mixed: the constraint oscillates between coordination and extraction, and suppressiveness depends on the speaker distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rhetoric_versus_judgment_distinction, empirical, 'Whether rhetorical quality correlates with better judgment in assembly reversals').

omega_variable(
    alternative_reading_foreclosure,
    'Does the volatility reading foreclose the radical-self-rule reading, or can both be held simultaneously by different audiences of Athenian democracy?',
    'Doctrinal analysis: Can one affirm both ''the assembly is absolutely sovereign'' (radical reading) AND ''the assembly is structurally extractive because it lacks deliberative distance'' (volatility reading)? Or does accepting one require rejecting the axioms of the other?',
    'If foreclosed: the two readings are competitors, not coexisting positions. If coexistent: both readings remain live, and the fourth-century nomothetai reforms represent a third position that addresses the problem the volatility reading identifies without abandoning the assembly sovereignty the radical reading asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether volatility and radical-self-rule readings are logically compatible').

omega_variable(
    deliberative_distance_measurability,
    'Can deliberative distance (the time, procedure, or institutional steps between first and second consideration) be quantified, and does it correlate with reversal rates or reversal quality?',
    'Comparative analysis of assembly voting patterns: Athens (minimal distance), Roman comitia (slightly more formalized), Venetian Great Council (greater procedural distance), modern legislatures (maximum distance). Track reversal rates and error-correction rates across these systems.',
    'If deliberative distance increases reversal rates without improving decision quality: distance is extraction (privileging vocal minorities with second chances). If distance improves decision quality: distance is coordination. The mytilene volatility reading depends critically on the first result — that suppression of distance creates a system where mere rhetoric and speed determine outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_distance_measurability, empirical, 'Measurable correlation between procedural distance and decision quality').

omega_variable(
    committer_reading_contest,
    'This constraint is one reading of the kernel ''assembly_supremacy.'' What does the Mytilene event reveal about whether assembly sovereignty is predicated on volatility, wisdom-seeking through procedure, or self-rule without remainder?',
    'The kernel contest is irreducible at the conceptual level. The three readings (volatility, nomothetai maturation, radical self-rule) are three different interpretations of what assembly government IS, grounded in three different axioms about legitimacy. Resolution would require choosing an axiom — a normative/political choice, not an empirical discovery. However, empirical data about reversal rates, reversal quality, and deliberative procedures can constrain which readings are viable under which institutional arrangements.',
    'This is the core mandatrophy for assembly supremacy as a kernel. The Mytilene volatility reading emerges from interpreting the massacre-and-reversal event as evidence that immediate assembly voting is extractive. The radical self-rule reading interprets the same event as evidence that the demos retained ultimate authority (reversal proved it). The nomothetai reading sees it as evidence that unfiltered assembly voting fails. All three readings are grounded in the same documentary record; which reading is ''correct'' depends on which axis of legitimacy (effectiveness, self-determination, or popular sovereignty) you privilege.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_contest, preference, 'Which reading of assembly supremacy the kernel event supports: volatility, wisdom-seeking, or radical self-rule').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(assembly_supremacy__mytilene_volatility_reading, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mytilene_theater_t0_cleon_speech, assembly_supremacy__mytilene_volatility_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mytilene_theater_t1_diodotus_responds, assembly_supremacy__mytilene_volatility_reading, theater_ratio, 1, 0.38).
narrative_ontology:measurement(mytilene_theater_t2_second_courier_race, assembly_supremacy__mytilene_volatility_reading, theater_ratio, 2, 0.42).

% Extraction over time
narrative_ontology:measurement(mytilene_extractiveness_t0_voting_day, assembly_supremacy__mytilene_volatility_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mytilene_extractiveness_t1_ships_sailed, assembly_supremacy__mytilene_volatility_reading, base_extractiveness, 1, 0.68).
narrative_ontology:measurement(mytilene_extractiveness_t2_second_vote_aftermath, assembly_supremacy__mytilene_volatility_reading, base_extractiveness, 2, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(mytilene_suppression_t0_initial_decree, assembly_supremacy__mytilene_volatility_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(mytilene_suppression_t1_irreversibility_realization, assembly_supremacy__mytilene_volatility_reading, suppression_requirement, 1, 0.72).
narrative_ontology:measurement(mytilene_suppression_t2_race_against_courier, assembly_supremacy__mytilene_volatility_reading, suppression_requirement, 2, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(assembly_supremacy__mytilene_volatility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(assembly_supremacy__mytilene_volatility_reading, assembly_supremacy__nomothetai_maturation_reading).
narrative_ontology:affects_constraint(assembly_supremacy__mytilene_volatility_reading, assembly_supremacy__radical_self_rule_reading).

% DUAL FORMULATION NOTE:
% The kernel 'assembly_supremacy' decomposes into three structurally distinct constraint stories corresponding to three contested readings of what assembly sovereignty IS. The Mytilene volatility reading specifies the constraint under minimal deliberative delay and maximum immediate authority. The nomothetai maturation reading specifies the constraint with introduced procedural filters. The radical self-rule reading specifies the constraint as evidence of unmediated demos authority. Each story has its own ε, beneficiary/victim structure, and classification type. They are linked not by causal dependency but by doctrinal contest: they are competing interpretations of the same kernel event and the same historical institution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
