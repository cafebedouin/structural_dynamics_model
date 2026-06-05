% ============================================================================
% CONSTRAINT STORY: articles_of_confederation__unanimity_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_articles_unanimity_trap, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: articles_of_confederation__unanimity_trap_reading
 *   human_readable: Article XIII Unanimity Trap: The Amendment Veto that Killed the Confederation
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   Article XIII of the Articles of Confederation required unanimous consent
 *   of all thirteen states for any amendment to the Articles. This constraint
 *   reads the Articles' structural death as a direct consequence of this
 *   unanimity veto — a rule that made internal reform impossible and
 *   transformed revolution into the only viable amendment mechanism. The
 *   constraint is not about whether the state-sovereignty principle was sound
 *   (that is the state-sovereignty-design reading), nor about Congress's
 *   failure to levy taxes (that is the requisition-failure reading), but
 *   specifically about how the amendment procedure itself became a tool of
 *   immobility. When Rhode Island or North Carolina used the veto to block
 *   proposed amendments on commerce power, revenue collection, or interstate
 *   regulation, they were exercising the protection that Article XIII
 *   provided. But that same protection made the confederation unable to adapt
 *   to post-war challenges, war debt, and interstate economic conflict. By
 *   the mid-1780s, the system was locked: it could not reform itself, it
 *   could not generate revenue, and its inability to adapt had become visible
 *   to every state. The solution was not amendment of Article XIII but
 *   replacement of the entire instrument — the Constitutional Convention of
 *   1787 dissolved the Articles by the most radical amendment imaginable, one
 *   that required no unanimous consent.
 *
 * KEY AGENTS:
 *   - Holdout States (Rhode Island, North Carolina paradigm): Primary beneficiaries (institutional/arbitrage) — extract the ability to preserve local autonomy and block unwanted obligations through veto power
 *   - Confederation's Viability: Primary victim (powerless/trapped) — the system itself cannot adapt; locked by its own foundational rule
 *   - Reform-Seeking States (Virginia, Pennsylvania, New York leadership): Secondary victims (moderate/trapped) — recognize the pathology but cannot escape it without dissolving the union
 *   - Congress as Deliberative Body: Institutional observer (institutional/constrained) — persists in performative form while substantive power atrophies to states
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent amendment rule as inevitable confederation logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(articles_of_confederation__unanimity_trap_reading, 0.88).
domain_priors:suppression_score(articles_of_confederation__unanimity_trap_reading, 0.92).
domain_priors:theater_ratio(articles_of_confederation__unanimity_trap_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(articles_of_confederation__unanimity_trap_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(articles_of_confederation__unanimity_trap_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(articles_of_confederation__unanimity_trap_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(articles_of_confederation__unanimity_trap_reading, snare).
narrative_ontology:human_readable(articles_of_confederation__unanimity_trap_reading, "Article XIII Unanimity Trap: The Amendment Veto that Killed the Confederation").
narrative_ontology:topic_domain(articles_of_confederation__unanimity_trap_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(articles_of_confederation__unanimity_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(articles_of_confederation__unanimity_trap_reading, '18b275df-ff9c-4333-8d82-aa6be8c96a9e').
narrative_ontology:cs_kernel_codification('18b275df-ff9c-4333-8d82-aa6be8c96a9e', formalized).
narrative_ontology:cs_authority_grounding('18b275df-ff9c-4333-8d82-aa6be8c96a9e', extraction).
narrative_ontology:cs_interpretation_layer_present('18b275df-ff9c-4333-8d82-aa6be8c96a9e').
narrative_ontology:cs_reading_relation('18b275df-ff9c-4333-8d82-aa6be8c96a9e', articles_of_confederation__requisition_failure_reading, coexists_with).
narrative_ontology:cs_reading_relation('18b275df-ff9c-4333-8d82-aa6be8c96a9e', articles_of_confederation__state_sovereignty_design_reading, influences).
narrative_ontology:cs_axiom('18b275df-ff9c-4333-8d82-aa6be8c96a9e', foundational, unanimous_amendment_makes_system_unadaptable).
narrative_ontology:cs_axiom_status(unanimous_amendment_makes_system_unadaptable, holdable).
narrative_ontology:cs_axiom_grounding('18b275df-ff9c-4333-8d82-aa6be8c96a9e', unanimous_amendment_makes_system_unadaptable, empirically_contingent).
narrative_ontology:cs_axiom('18b275df-ff9c-4333-8d82-aa6be8c96a9e', foundational, veto_is_extraction_under_adaptation_pressure).
narrative_ontology:cs_axiom_status(veto_is_extraction_under_adaptation_pressure, holdable).
narrative_ontology:cs_axiom_grounding('18b275df-ff9c-4333-8d82-aa6be8c96a9e', veto_is_extraction_under_adaptation_pressure, instrumental).
narrative_ontology:cs_reference_frame('18b275df-ff9c-4333-8d82-aa6be8c96a9e', sovereign_confederation_with_veto_protection).
narrative_ontology:cs_drift_state('18b275df-ff9c-4333-8d82-aa6be8c96a9e', post_war_fiscal_crisis_1783_1789, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('18b275df-ff9c-4333-8d82-aa6be8c96a9e', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(articles_of_confederation__unanimity_trap_reading, articles_of_confederation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(articles_of_confederation__unanimity_trap_reading, holdout_states).
narrative_ontology:constraint_beneficiary(articles_of_confederation__unanimity_trap_reading, status_quo_preservers).
narrative_ontology:constraint_victim(articles_of_confederation__unanimity_trap_reading, confederation_viability).
narrative_ontology:constraint_victim(articles_of_confederation__unanimity_trap_reading, reform_seeking_states).
narrative_ontology:constraint_victim(articles_of_confederation__unanimity_trap_reading, national_solvency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONFEDERATION VIABILITY (SNARE) — The system itself has no exit and no power. The constitutional commitment cannot reform itself; it is locked by the very rule designed to protect state sovereignty. Amendment requires unanimous consent, making reform structurally impossible when even one state opposes. The confederation as a living political order bears the full cost of its own immobility. Suppression is absolute: there are no alternatives within the Articles' logic — revolution is the only exit.
constraint_indexing:constraint_classification(articles_of_confederation__unanimity_trap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-SEEKING STATES (SNARE) — States that recognize the confederation's structural pathology (unpaid war debt, no independent revenue, Congress powerless to regulate commerce or enforce requisitions) have no mechanism to fix it. The very rule meant to protect state sovereignty becomes a tool used by holdout states (Rhode Island, North Carolina) to prevent adaptation. Reform-seeking states are trapped: they cannot leave without dissolving the union, cannot reform within it, and cannot impose reform on unwilling states. Maximum extraction — the holdout states extract the benefit of stability-through-paralysis while the union dies slowly.
constraint_indexing:constraint_classification(articles_of_confederation__unanimity_trap_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HOLDOUT STATES / RHODE ISLAND PARADIGM (ROPE) — From the beneficiary's perspective, Article XIII is pure coordination: it protects state autonomy by requiring consensus for any change. Rhode Island and other smaller or locally-focused states experience the rule as a coordination benefit — it guarantees that no state can be forced into unwanted obligations without its explicit consent. The rule works perfectly for its stated function: preventing centralization and enforcing state sovereignty. This perspective experiences no extraction, only the legitimate defense of state independence. The veto is not coercion but coordination.
constraint_indexing:constraint_classification(articles_of_confederation__unanimity_trap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CONGRESS AS PERFORMATIVE INSTITUTION (PITON) — By the 1780s, the Continental Congress persists as a deliberative body requesting requisitions that states ignore, issuing currency that inflates without control, and holding meetings that ratify decisions already made by state governments. The theater of congressional procedure (debate, voting, formal resolutions) continues, but the functional power has atrophied — requisitions go unpaid, the treasury is empty, and Congress cannot enforce any decision. The institution maintains the performative form of amendment procedure (the Articles require unanimous amendment) while the substance of legislative power has already departed to state capitals. Theater ratio is low (0.15) because the constraint is not about performative ritual — it is about structural paralysis.
constraint_indexing:constraint_classification(articles_of_confederation__unanimity_trap_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, Article XIII instantiates a logical limit: any confederation that grants true state sovereignty must also protect each state's ability to exit or veto changes, and this guarantee makes the system mathematically incapable of adapting when interests diverge. The unanimity requirement is not a design choice but an inevitable consequence of the sovereignty principle itself. Under this reading, the Articles' collapse is not a contingent failure but a demonstration of a natural law: confederations cannot survive adaptation pressure when exit costs are symmetrical. However, the structural data contradicts this mountain classification — the unanimity rule is a choice encoded in Article XIII, not a logical necessity, and other confederal systems have used different mechanisms (supermajority thresholds, regional voting, opt-out clauses). The engine will flag this as a false summit: the 'natural confederation law' reading naturalizes what is actually a contingent doctrinal choice.
constraint_indexing:constraint_classification(articles_of_confederation__unanimity_trap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(articles_of_confederation__unanimity_trap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(articles_of_confederation__unanimity_trap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(articles_of_confederation__unanimity_trap_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(articles_of_confederation__unanimity_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(articles_of_confederation__unanimity_trap_reading, TR),
    TR >= 0.70.

:- end_tests(articles_of_confederation__unanimity_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): Very high. The unanimity rule creates a mechanism by which any single state can extract the benefit of veto power — the ability to block changes that would disadvantage it, even if those changes benefit the union as a whole. As time progresses (t=0 to t=9, representing 1781-1789), the extractiveness increases because the holdout veto becomes more valuable the more desperate the union becomes for reform. By 1787, states facing financial ruin or interstate conflict would accept almost any amendment to restore solvency and order, making the holdout veto an increasingly powerful extraction tool. Suppression (0.92): Extreme. The rule creates complete suppression of alternatives within the Articles' logic. States cannot reform; Congress cannot enforce; individual states cannot exit cleanly (exit costs include loss of defense alliance, trade disruption, unilateral war-debt liability). The only exit is revolutionary regime change. Suppression increases over the interval as the alternatives' costs become clearer and the system's pathology more evident. Theater ratio (0.15): Low. This constraint is not performative — it is a hard mechanism. The amendment procedure is the procedure; Congress's debates are real deliberations; the veto is not ritual but structural power. The low theater indicates that the extraction is direct and overt, not hidden behind ceremonial forms.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The holdout states see Article XIII as a coordination success (Rope): the rule works perfectly to protect state autonomy and prevent centralization — exactly what it was designed for. Reform-seeking states and the confederation as a whole see catastrophic extraction (Snare): the same rule that protects holdout states paralyzes the entire system. The piton perspective recognizes that by the 1780s, Congress has become performative — it debates and votes but cannot implement. The mountain perspective risks claiming the unanimity trap is inevitable confederation logic, when in fact it is a specific doctrinal choice that other confederations avoided. The analytical observer's job is to recognize the false summit: framing Article XIII as a natural law of confederations obscures that it is a contingent institutional choice with distributional consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective: Holdout States (beneficiary + arbitrage) → d ≈ 0.15, f(d) ≈ -0.01, experience low effective extraction and see the rule as beneficial coordination. Reform-Seeking States (victim + trapped) → d ≈ 0.95, f(d) ≈ 1.42, experience maximum extraction because they bear the cost of immobility while unable to exit cleanly. Confederation as abstract entity (victim + trapped) → d = 1.0, f(d) ≈ 1.42, experiences pure extraction — the rule extracts its viability. Congress (institutional observer with constrained exit) → d ≈ 0.65, experience moderate extraction as their functional power has been hollowed out while the institution persists. Analytical observer (analytical exit) → d ≈ 0.73, applies the civilizational universality lens that risks naturalizing the rule.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The high extractiveness (0.88) and suppression (0.92) generate a mandatrophy signal: Is Article XIII a pure snare (extraction mechanism) or is it a legitimate coordination device (rope) that has pathological side effects? The constraint resolves this by showing that it is both, from different positions. For holdout states, it is rope — genuine coordination that protects state autonomy. For the confederation and reform-seeking states, it is snare — pure extraction in that it prevents adaptation while beneficiaries (holdout states) profit from the paralysis. The mandatrophy resolves by accepting the indexical plurality: there is no single 'true' type, only perspectival types relative to the observer's position. This is a canonical case where mandatrophy indicates that the constraint's classification depends entirely on which agent's structural position you occupy. The resolution is not to declare one type 'correct' but to recognize that the constraint legitimately exhibits snare properties from some positions and rope properties from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_amendment_mechanisms,
    'Would a supermajority threshold (9 of 13 states, or 75%) have enabled reform without dissolving state sovereignty?',
    'Counterfactual analysis of historical reform proposals: How many of the disputed amendments (Vermont admission, Morris tax, national commerce power) would have passed under 9-of-13 rule vs. actual 13-of-13? Which states would have remained consistently opposed?',
    'If supermajority would have enabled 3+ major reforms: unanimity was an excessive suppression mechanism, not a necessary condition. If supermajority would still have blocked most reforms: the problem is deeper than the amendment threshold — it is the cost-benefit asymmetry for holdout states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_amendment_mechanisms, empirical, 'Whether supermajority amendment threshold would have enabled viable reform').

omega_variable(
    holdout_state_rational_exit,
    'Why did holdout states like Rhode Island maintain veto power rather than secede? What benefits did they extract from membership despite blocking adaptation?',
    'Historical analysis of state-level economic data (trade with union states, access to common defense, foreign relations leverage), state documents and legislative records on ratification debates, comparative analysis of state behavior before vs. after Constitution adoption.',
    'If extraction (trade benefits, defense cost-sharing, diplomatic weight) was substantial: holdout states were rational extractors exercising a tool that benefited them. If extraction was minimal: states may have been locked by exit costs (geographic position, war debt, inability to secure foreign credit alone) rather than positive incentive — reframes the snare from ''rational veto'' to ''structural entrapment.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holdout_state_rational_exit, empirical, 'Rational structure of state-level holdout incentives').

omega_variable(
    reading_forecast_closure,
    'Is the unanimity-trap reading logically foreclosed by the state-sovereignty-design reading, or do they coexist as live interpretations of the same doctrinal commitment?',
    'Jurisprudential analysis: Do framers'' statements on Article II (sovereignty) and Article XIII (unanimity) present them as complementary parts of a unified design, or as tensions within the framers'' own thinking? Can a single coherent reading of ''the Articles as a confederation of sovereign states'' accommodate both the unanimity mechanism AND the reform failures?',
    'If coexists: both readings are defensible from within the sovereignty framework — the Articles could be read either as a successful league (sovereignty reading) or a failed system (unanimity-trap reading), depending on success criteria. If forecloses: the unanimity trap reading reveals that state sovereignty and systemic viability are incompatible within the Articles'' specific architecture, disproving the coherence of the design claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_forecast_closure, conceptual, 'Logical relationship between unanimity-trap and state-sovereignty readings').

omega_variable(
    revolution_as_amendment_procedure,
    'Is revolution (replacing the Articles with the Constitution) properly analyzed as an ''alternative amendment procedure,'' or does characterizing it that way obscure the discontinuity between written constitutional change and extra-constitutional regime replacement?',
    'Jurisprudential and political theory analysis: How do subsequent constitutional traditions (European, Commonwealth, post-colonial) treat regime replacement? Does framing revolution as ''the only amendment procedure that worked'' import a false equivalence between Article XIII amendment (legitimate constitutional procedure) and constitutional replacement (regime change)?',
    'If revolution is procedurally continuous with Article XIII: the unanimity trap is a design feature that forces systemic regeneration. If revolution is discontinuous: the unanimity trap represents a doctrinal failure so absolute that it required extra-doctrinal response — this strengthens the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolution_as_amendment_procedure, conceptual, 'Status of revolution as constitutional amendment mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(articles_of_confederation__unanimity_trap_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(articles_unanim_extract_t0, articles_of_confederation__unanimity_trap_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(articles_unanim_extract_t3, articles_of_confederation__unanimity_trap_reading, base_extractiveness, 3, 0.81).
narrative_ontology:measurement(articles_unanim_extract_t6, articles_of_confederation__unanimity_trap_reading, base_extractiveness, 6, 0.88).
narrative_ontology:measurement(articles_unanim_extract_t9, articles_of_confederation__unanimity_trap_reading, base_extractiveness, 9, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(articles_unanim_suppress_t0, articles_of_confederation__unanimity_trap_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(articles_unanim_suppress_t3, articles_of_confederation__unanimity_trap_reading, suppression_requirement, 3, 0.85).
narrative_ontology:measurement(articles_unanim_suppress_t6, articles_of_confederation__unanimity_trap_reading, suppression_requirement, 6, 0.92).
narrative_ontology:measurement(articles_unanim_suppress_t9, articles_of_confederation__unanimity_trap_reading, suppression_requirement, 9, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(articles_of_confederation__unanimity_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(articles_of_confederation__unanimity_trap_reading, 0.12).
narrative_ontology:affects_constraint(articles_of_confederation__unanimity_trap_reading, articles_of_confederation__requisition_failure_reading).
narrative_ontology:affects_constraint(articles_of_confederation__unanimity_trap_reading, articles_of_confederation__state_sovereignty_design_reading).
narrative_ontology:affects_constraint(articles_of_confederation__unanimity_trap_reading, constitutional_convention_extra_doctrinal_legitimacy).

% DUAL FORMULATION NOTE:
% The Articles of Confederation kernel has three distinct constraint stories corresponding to three diagnostic readings. This story (unanimity_trap_reading) focuses on the amendment mechanism as the primary pathology. The requisition_failure_reading focuses on fiscal authority and interstate compliance. The state_sovereignty_design_reading focuses on Article II's sovereignty protections. All three stories share the same base historical facts but emphasize different causal structures and beneficiary/victim relationships. Each story produces a different ε value: the unanimity trap produces very high extractiveness (0.88) because the veto mechanism is an explicit extraction tool; the requisition failure produces moderate extractiveness around the fiscal authority gap; the sovereignty design produces low extractiveness if read as successful coordination. The three stories are linked via network.affects_constraints to show how different diagnostic framings reveal different constraint types within the same doctrinal system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
