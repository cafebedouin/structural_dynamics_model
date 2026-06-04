% ============================================================================
% CONSTRAINT STORY: graphe_paranomon__self_binding_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_graphe_paranomon__self_binding_mechanism_reading, []).

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
 *   constraint_id: graphe_paranomon__self_binding_mechanism_reading
 *   human_readable: Graphe Paranomon: Self-Binding Mechanism Reading (Democratic Brake on Decree-by-Momentum)
 *   domain: legal/doctrinal/athenian_constitutional_law
 *
 * SUMMARY:
 *   The graphe paranomon (action for illegality) was an Athenian legal
 *   mechanism allowing any citizen to challenge a decree passed by the
 *   assembly as unlawful, with the proposer personally answering for the
 *   challenged decree. This constraint is one reading of a contested kernel:
 *   the same institution can be understood as (1) a mechanism for democracy
 *   to bind itself — collective sovereignty given an internal deliberative
 *   brake; (2) a risk-pricing mechanism that made the rostrum accessible only
 *   to orators wealthy enough to defend their speech; or (3) a weapon in
 *   factional struggle, where litigation became the continuation of assembly
 *   politics. This constraint instantiates the self-binding reading: the
 *   mechanism's beneficiary is second-thought governance (the assembly's
 *   capacity to reconsider its own decrees), and its victim is the momentum
 *   of unreviewable decree-by-majority-shout. The extractiveness is moderate
 *   (0.28) because the mechanism genuinely serves a coordination function
 *   (enabling collective reconsideration) while imposing real costs on the
 *   individual proposer (litigation liability, reputation damage if
 *   challenged). The theater ratio (0.42) is moderate-low because the
 *   judicial review is not primarily performative — courts genuinely
 *   deliberate whether decrees violated standing law — but the mechanism's
 *   full function is realized only when the assembly itself has second
 *   thoughts, not when the judiciary acts alone.
 *
 * KEY AGENTS:
 *   - The Hasty Decree: The immediate victim (powerless/trapped) — a decree passed in assembly momentum becomes vulnerable to challenge and retraction; no escape from review once proposed
 *   - The Individual Proposer/Orator: Primary bearer of extraction (moderate/constrained) — faces personal prosecution and reputation cost for any challenged decree, but also provides genuine coordination to assembly deliberation
 *   - The Assembly Collectively: Primary beneficiary (institutional/arbitrage) — gains mechanism for collective reconsideration without paying extraction costs itself; benefits from capacity to correct mistakes
 *   - The Opposing Faction: Organized users of the mechanism (organized/constrained) — coordinates collective opposition and extracts from the proposer through litigation while claiming to serve the assembly's second thoughts
 *   - The Judicial System (Dikasteria): Institutional actor (institutional/arbitrage) — formally reviews decrees; largely performative from civilizational perspective, confirming assembly sentiment rather than independently scrutinizing
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent Athenian solution as an immutable feature of democratic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(graphe_paranomon__self_binding_mechanism_reading, 0.28).
domain_priors:suppression_score(graphe_paranomon__self_binding_mechanism_reading, 0.35).
domain_priors:theater_ratio(graphe_paranomon__self_binding_mechanism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(graphe_paranomon__self_binding_mechanism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(graphe_paranomon__self_binding_mechanism_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(graphe_paranomon__self_binding_mechanism_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(graphe_paranomon__self_binding_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(graphe_paranomon__self_binding_mechanism_reading, "Graphe Paranomon: Self-Binding Mechanism Reading (Democratic Brake on Decree-by-Momentum)").
narrative_ontology:topic_domain(graphe_paranomon__self_binding_mechanism_reading, "legal/doctrinal/athenian_constitutional_law").

domain_priors:requires_active_enforcement(graphe_paranomon__self_binding_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(graphe_paranomon__self_binding_mechanism_reading, '719a1b67-d630-49ea-8aef-dcfa9171a015').
narrative_ontology:cs_kernel_codification('719a1b67-d630-49ea-8aef-dcfa9171a015', fixed_text).
narrative_ontology:cs_authority_grounding('719a1b67-d630-49ea-8aef-dcfa9171a015', lineage).
narrative_ontology:cs_interpretation_layer_present('719a1b67-d630-49ea-8aef-dcfa9171a015').
narrative_ontology:cs_reading_relation('719a1b67-d630-49ea-8aef-dcfa9171a015', graphe_paranomon__orator_risk_economy_reading, coexists_with).
narrative_ontology:cs_reading_relation('719a1b67-d630-49ea-8aef-dcfa9171a015', graphe_paranomon__weapon_of_faction_reading, coexists_with).
narrative_ontology:cs_axiom('719a1b67-d630-49ea-8aef-dcfa9171a015', foundational, democracy_binds_itself_through_reviewability).
narrative_ontology:cs_axiom_status(democracy_binds_itself_through_reviewability, holdable).
narrative_ontology:cs_axiom_grounding('719a1b67-d630-49ea-8aef-dcfa9171a015', democracy_binds_itself_through_reviewability, deontological).
narrative_ontology:cs_axiom('719a1b67-d630-49ea-8aef-dcfa9171a015', secondary, proposer_liability_enables_second_thought).
narrative_ontology:cs_axiom_status(proposer_liability_enables_second_thought, holdable).
narrative_ontology:cs_axiom_grounding('719a1b67-d630-49ea-8aef-dcfa9171a015', proposer_liability_enables_second_thought, instrumental).
narrative_ontology:cs_reference_frame('719a1b67-d630-49ea-8aef-dcfa9171a015', deliberative_democracy_self_correcting).
narrative_ontology:cs_drift_state('719a1b67-d630-49ea-8aef-dcfa9171a015', late_classical_athenian, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('719a1b67-d630-49ea-8aef-dcfa9171a015', '').
narrative_ontology:cs_kernel_id(graphe_paranomon__self_binding_mechanism_reading, graphe_paranomon).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(graphe_paranomon__self_binding_mechanism_reading, second_thought_governance).
narrative_ontology:constraint_beneficiary(graphe_paranomon__self_binding_mechanism_reading, assembly_collective_wisdom).
narrative_ontology:constraint_victim(graphe_paranomon__self_binding_mechanism_reading, unreviewable_decree_momentum).
narrative_ontology:constraint_victim(graphe_paranomon__self_binding_mechanism_reading, immediate_action_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE HASTY DECREE (SNARE) — An assembly motion passed in momentum cannot escape challenge; the proposer is locked into liability for whatever court scrutiny ensues. The decree's author faces maximum extraction: personal prosecution regardless of whether the assembly itself regrets the motion. No exit from this accountability bind — the proposal itself becomes the trap.
constraint_indexing:constraint_classification(graphe_paranomon__self_binding_mechanism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INDIVIDUAL PROPOSER/ORATOR (TANGLED ROPE) — Constrained by personal liability but also genuinely coordinating the assembly's deliberation. The orator who brings a proposal experiences extraction (litigation risk, reputation cost) alongside coordination function (his voice shapes collective policy). Exit is expensive but possible: one can refrain from proposing. The moderate power and constrained exit reflect that speakers in the ekklesia were not powerless but faced real costs.
constraint_indexing:constraint_classification(graphe_paranomon__self_binding_mechanism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ASSEMBLY COLLECTIVELY (ROPE) — Experiences the constraint as pure coordination: the graphe paranomon gives the assembly a mechanism to correct itself without dissolving democratic authority. The collective sees binding itself as a tool for better deliberation. Net beneficiary through arbitrage (the assembly can use the mechanism to review decrees it regrets without paying extraction costs itself). Institutional power, long time horizon, and arbitrage exit reflect that the assembly as a whole can always revoke or amend prior decrees.
constraint_indexing:constraint_classification(graphe_paranomon__self_binding_mechanism_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: THE OPPOSING FACTION (TANGLED ROPE) — Organized agents (rival political factions) use the graphe paranomon as a coordination mechanism for collective opposition while extracting from the proposer through litigation cost and reputational damage. The faction benefits from the institutional framework while imposing costs on the orator. Constrained exit reflects that factions cannot ignore the mechanism without losing political voice.
constraint_indexing:constraint_classification(graphe_paranomon__self_binding_mechanism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: THE JUDICIAL REVIEW APPARATUS (PITON) — The dikasteria reviewing decrees under the graphe paranomon are largely performative from the civilizational view: the real deliberation happens in the assembly itself, and the court review is a formal ritual confirming or overturning decisions already politically settled. The judicial machinery persists as a check-box mechanism maintaining the appearance of deliberative rigor even when the assembly's political consensus dominates the outcome. Theater ratio reflects that much of the judicial process is confirmation of assembly sentiment rather than independent scrutiny.
constraint_indexing:constraint_classification(graphe_paranomon__self_binding_mechanism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL NECESSITY VIEW (MOUNTAIN) — From a civilizational scope, the constraint appears as an inherent property of democratic law itself: any sovereign body must have a mechanism to review and constrain its own hasty action, or it ceases to govern itself. The graphe paranomon is seen as an immutable structural feature of what it means to be a deliberative democracy. However, the base properties reveal this as a false summit: the constraint is a contingent institutional arrangement benefiting second-thought governance, not an immutable law of democracy.
constraint_indexing:constraint_classification(graphe_paranomon__self_binding_mechanism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(graphe_paranomon__self_binding_mechanism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(graphe_paranomon__self_binding_mechanism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(graphe_paranomon__self_binding_mechanism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(graphe_paranomon__self_binding_mechanism_reading, TR),
    TR >= 0.70.

:- end_tests(graphe_paranomon__self_binding_mechanism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate. The proposer genuinely pays a cost (litigation risk, reputation damage if challenged, opportunity cost of defense preparation). This is real extraction. However, the mechanism also genuinely enables assembly reconsideration, so it is not pure extraction machinery. The beneficiary (second-thought governance) is not a faction or individual but the assembly's collective capacity to deliberate correctly. This bifurcation — real proposer cost + genuine coordination function — places the constraint squarely in tangled_rope territory rather than pure snare. Suppression (0.35): Moderate. The threat of litigation and reputation cost deters hasty proposals, especially by orators without wealthy backers or political protection. This is a real suppressive force. However, it is not total — many proposals are made despite the risk, and speakers are not physically prevented from proposing. Suppression here means 'cost of speech,' not 'silence under penalty.' Theater ratio (0.42): Moderate-low. The judicial review is not merely performative in the strict sense — dikasteria genuinely hear arguments and deliberate. However, the real deliberation often already happened in the assembly itself, and the court review sometimes formalizes what political consensus already determined. The theater is lower than, say, ceremonial approval procedures (which would be piton-range 0.70+) but higher than purely independent scrutiny. The theater has increased slightly over the measurement interval (0.35 → 0.42) as the mechanism became more formalized and predictable.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces divergent classifications across indexical positions. The hasty decree itself (powerless/trapped view) sees pure snare — no exit from the accountability bind. The proposer (moderate/constrained) sees tangled rope — real coordination mixed with real extraction. The assembly collectively (institutional/arbitrage) sees pure rope — a coordination mechanism that costs it nothing. The opposing faction (organized/constrained) sees tangled rope from a different angle — using the mechanism to organize opposition while extracting from the proposer. The court system (institutional/arbitrage) sees itself as merely executing a check (piton — degraded from its aspirational pure review function). The civilizational observer risks seeing a mountain — an immutable feature of democracy — but the structural data reveals this as false summit: the mechanism is a contingent Athenian solution that benefits identifiable agents (assembly, organized opposition) and imposes costs on identifiable agents (hasty proposers, politically isolated speakers). The gap reveals that what appears 'necessary to democracy' from one perspective is actually a specific institutional arrangement benefiting some actors at the expense of others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status + exit options. Hasty decree: victim of the mechanism, no exit, trapped → high d (0.95). Proposer: victim of personal liability, constrained exit (can refrain from proposing but not from society) → high d (0.65-0.75). Assembly: beneficiary with perfect exit (can ignore mechanism or amend law) → low d (0.10). Opposing faction: mixed — uses mechanism to organize (coordination benefit) but pays litigation costs (extraction) → moderate d (0.55). Court: institutional beneficiary (given delegated authority) with arbitrage (can interpret law loosely or strictly) → low d (0.15). Analyst: observer, no structural position in extraction flow → d = 0.72 (canonical analytical). The engine's sigmoid f(d) converts these to experienced extractiveness multipliers: high d produces high f(d), amplifying chi; low d produces low or negative f(d), dampening chi. This is why the beneficiary perspectives (assembly, court) see rope/piton while the victim perspective (hasty decree) sees snare.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all tangled rope gates. (1) Requires active enforcement: yes — the dikasteria must be convened, arguments heard, vote taken. (2) Beneficiaries present: yes — second-thought governance and the assembly collectively. (3) Victims present: yes — unreviewable decree momentum and the individual proposer face extraction. (4) Coordination function genuine: yes — the mechanism enables assembly reconsideration, a real coordination task. (5) Asymmetric extraction: yes — proposers pay costs, assembly collects benefits. The mandatrophy is resolved by recognizing that tangled rope is the analytically correct classification precisely because both coordination and extraction are structurally present. The apparent paradox (why would a democracy bind itself with extraction?) is dissolved by understanding that the extraction is the cost of enabling second-thought — the mechanism trades proposer liability for assembly deliberative capacity. This is not mandatrophy; it is tangled-rope-by-design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proposer_liability_allocation,
    'Is proposer personal liability a genuine brake on hasty decree or a mechanism for factional suppression of minority proposals?',
    'Empirical analysis of graphe paranomon cases: correlation between proposal content (radical reform vs. conservative affirmation of norms) and prosecution rates; wealth/status of proposers successfully challenged vs. those whose decrees stood',
    'If liability is egalitarian (independent of proposal type): self-binding mechanism holds. If liability correlates with radical proposals or low-status proposers: constraint is weapon of faction, not democratic brake.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proposer_liability_allocation, empirical, 'Whether proposer liability reflects genuine deliberation brake or factional suppression').

omega_variable(
    assembly_genuine_second_thought,
    'Does the graphe paranomon measure genuine assembly reconsideration or merely formalize ex-post factional reversal of decisions already determined by shifting coalitions?',
    'Analysis of assembly voting patterns on the contested decrees themselves vs. court outcomes; measurement of whether dikasteria reversals correlate with changes in assembly political coalitions between decree and trial',
    'If second thought is genuine: assembly uses the mechanism to correct mistakes. If formalized reversal: the constraint is a tool for tracking power shifts, not improving deliberation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(assembly_genuine_second_thought, empirical, 'Whether graphe paranomon represents genuine deliberative reconsideration or factional power realignment').

omega_variable(
    democratic_self_binding_trade_off,
    'Does the cost of proposer liability (chilled speech, favoring cautious/wealthy speakers) outweigh the benefit of deliberative review (collective wisdom)?',
    'Comparative analysis of speech patterns before vs. after introduction of graphe paranomon; measurement of proposal diversity and originality; comparison with poleis lacking such mechanisms',
    'If costs outweigh benefits: self-binding is suboptimal, and the constraint is correctly modeled as moderate extraction (this reading). If benefits dominate: extractiveness should be lower, moving toward pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_self_binding_trade_off, preference, 'Cost-benefit analysis of self-binding mechanism on deliberative quality').

omega_variable(
    reading_vs_orator_risk_economy,
    'Is the graphe paranomon fundamentally a mechanism for collective self-review (self-binding reading) or a pricing mechanism for political speech (orator risk economy reading)?',
    'Conceptual: examine whether the institution''s primary function is to enable second-thought (coordinating future deliberation) or to allocate risk (determining who can afford the rostrum). Evidence: Do records show courts framing decisions as ''the assembly reconsiders'' or ''the proposer bears the cost of political speech''?',
    'This is a kernel-level ambiguity. Both readings are structurally coherent. If the mechanism is primarily about second-thought, this reading holds. If it is primarily a risk allocation mechanism, the orator_risk_economy_reading better captures the constraint structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_orator_risk_economy, conceptual, 'Ambiguity between democratic self-binding and orator risk economy framings of the same institution').

omega_variable(
    false_summit_institutional_necessity,
    'Is the graphe paranomon presented as a universal institutional necessity of democracy, or a specific contingent Athenian solution to the problem of decree-by-momentum?',
    'Comparative constitutional law: do other democracies (ancient and modern) without graphe paranomon equivalents still function as democracies? If yes: it is contingent, not necessary. If every democracy converges on similar mechanisms: it may be structurally necessary.',
    'If contingent: the mountain classification in the analytical perspective is a false summit. If necessary: the mountain may hold. Current historical consensus suggests contingency — the mechanism is distinctly Athenian.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_institutional_necessity, empirical, 'Whether self-binding mechanism is institutional necessity or contingent Athenian solution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(graphe_paranomon__self_binding_mechanism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(graphe_sb_tr_t0, graphe_paranomon__self_binding_mechanism_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(graphe_sb_tr_t25, graphe_paranomon__self_binding_mechanism_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(graphe_sb_tr_t50, graphe_paranomon__self_binding_mechanism_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(graphe_sb_be_t0, graphe_paranomon__self_binding_mechanism_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(graphe_sb_be_t25, graphe_paranomon__self_binding_mechanism_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(graphe_sb_be_t50, graphe_paranomon__self_binding_mechanism_reading, base_extractiveness, 50, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(graphe_paranomon__self_binding_mechanism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(graphe_paranomon__self_binding_mechanism_reading, graphe_paranomon__orator_risk_economy_reading).
narrative_ontology:affects_constraint(graphe_paranomon__self_binding_mechanism_reading, graphe_paranomon__weapon_of_faction_reading).

% DUAL FORMULATION NOTE:
% The graphe paranomon kernel decomposes into three structurally distinct constraints corresponding to three readings. This constraint (self_binding_mechanism_reading) emphasizes the mechanism's coordination function and moderate extractiveness (0.28). The sibling orator_risk_economy_reading would emphasize the mechanism's allocation function and higher extractiveness (~0.50+). The sibling weapon_of_faction_reading would emphasize the mechanism's contestation function and higher suppression (~0.65+). Each reading produces different ε values because each measures the constraint through a different dominant function. All three readings are structurally coherent and instantiated in Athenian history.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
