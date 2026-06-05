% ============================================================================
% CONSTRAINT STORY: failure_and_succession__internal_contradiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_failure_and_succession__internal_contradiction_reading, []).

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
 *   constraint_id: failure_and_succession__internal_contradiction_reading
 *   human_readable: Constitutional Contradiction as Structural Collapse (1791 French Settlement)
 *   domain: political/historical
 *
 * SUMMARY:
 *   The French Constitution of 1791 attempted to reconcile two incompatible
 *   legitimacy principles: hereditary monarchy (with retained executive
 *   prerogative, veto power, military command) and popular sovereignty
 *   (through a representative assembly granting universal rights to
 *   citizens). The settlement did not balance these principles — it stated
 *   them both as simultaneously true and binding. This internal contradiction
 *   created an extraction mechanism: the non-propertied majority, granted
 *   universal rights in principle but denied voting power through a censitary
 *   (property-based) franchise, bore the cost of maintaining the incoherence.
 *   The monarchy, granted sovereignty in the constitution yet constrained by
 *   legislative pretensions, also bore costs. Only the propertied
 *   bourgeoisie, whose interests aligned with both the property-based
 *   franchise and the monarch's capacity to suppress lower-class
 *   mobilization, benefited from the contradiction's suppression. The
 *   settlement died not from external conquest or incompetent execution, but
 *   from the logical rupture of its own incoherence: by 1792-1793, the
 *   attempt to hold contradictory principles simultaneously became
 *   unsustainable, and the constraint collapsed as each principle sought
 *   exclusive legitimacy. This reading traces collapse to the internal
 *   structure of the constraint itself rather than to contingent external
 *   disruption (the war sibling reading) or to the constitutional text's role
 *   as a template for future settlements (the template sibling reading).
 *
 * KEY AGENTS:
 *   - Non-Propertied Third Estate: Primary victim (powerless/trapped) — granted universal rights, denied voting power; bears full cost of incoherence
 *   - Propertied Assembly Bourgeoisie: Primary beneficiary (organized/constrained) — benefits from censitary franchise concentrating power; also faces genuine coordination challenge reconciling hereditary authority with representation
 *   - Monarchy (Louis XVI and court): Secondary beneficiary and victim (powerful/constrained) — benefits from constitutional legitimacy and retained executive prerogative; constrained by legislative encroachment and incompatibility with true sovereignty
 *   - Popular Sovereignty Principle: Institutional winner (institutional/arbitrage) — emerges as the victorious principle after rupture; experienced as pure coordination in this reading
 *   - Monarchical Principle: Institutional loser (institutional/constrained) — experiences extraction through ideological defeat; cannot simultaneously claim both absolute authority and constitutional legitimacy
 *   - The Constitution as Document (1791 text): Institutional artifact (institutional/arbitrage) — maintains performance of coherence through increasing theater as contradiction becomes apparent
 *   - Analytical Observer: Removed vantage (analytical/analytical) — recognizes the scaffold structure: a temporary bridging mechanism with built-in sunset determined by logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(failure_and_succession__internal_contradiction_reading, 0.58).
domain_priors:suppression_score(failure_and_succession__internal_contradiction_reading, 0.68).
domain_priors:theater_ratio(failure_and_succession__internal_contradiction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(failure_and_succession__internal_contradiction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(failure_and_succession__internal_contradiction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(failure_and_succession__internal_contradiction_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(failure_and_succession__internal_contradiction_reading, tangled_rope).
narrative_ontology:human_readable(failure_and_succession__internal_contradiction_reading, "Constitutional Contradiction as Structural Collapse (1791 French Settlement)").
narrative_ontology:topic_domain(failure_and_succession__internal_contradiction_reading, "political/historical").

domain_priors:requires_active_enforcement(failure_and_succession__internal_contradiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(failure_and_succession__internal_contradiction_reading, '90f41c1b-d94c-4515-bc1b-65ab345ab996').
narrative_ontology:cs_kernel_codification('90f41c1b-d94c-4515-bc1b-65ab345ab996', formalized).
narrative_ontology:cs_authority_grounding('90f41c1b-d94c-4515-bc1b-65ab345ab996', extraction).
narrative_ontology:cs_interpretation_layer_present('90f41c1b-d94c-4515-bc1b-65ab345ab996').
narrative_ontology:cs_reading_relation('90f41c1b-d94c-4515-bc1b-65ab345ab996', failure_and_succession__template_for_successors_reading, coexists_with).
narrative_ontology:cs_reading_relation('90f41c1b-d94c-4515-bc1b-65ab345ab996', failure_and_succession__war_destroyed_it_reading, influences).
narrative_ontology:cs_axiom('90f41c1b-d94c-4515-bc1b-65ab345ab996', foundational, contradiction_logically_unsustainable).
narrative_ontology:cs_axiom_status(contradiction_logically_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('90f41c1b-d94c-4515-bc1b-65ab345ab996', contradiction_logically_unsustainable, deontological).
narrative_ontology:cs_axiom('90f41c1b-d94c-4515-bc1b-65ab345ab996', foundational, rupture_resolves_incoherence).
narrative_ontology:cs_axiom_status(rupture_resolves_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('90f41c1b-d94c-4515-bc1b-65ab345ab996', rupture_resolves_incoherence, deontological).
narrative_ontology:cs_reference_frame('90f41c1b-d94c-4515-bc1b-65ab345ab996', reconciled_dual_sovereignty).
narrative_ontology:cs_drift_state('90f41c1b-d94c-4515-bc1b-65ab345ab996', constitutional_maturity_1792_1793, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('90f41c1b-d94c-4515-bc1b-65ab345ab996', '').
narrative_ontology:cs_kernel_id(failure_and_succession__internal_contradiction_reading, failure_and_succession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(failure_and_succession__internal_contradiction_reading, popular_sovereignty_principle).
narrative_ontology:constraint_victim(failure_and_succession__internal_contradiction_reading, monarchical_principle).
narrative_ontology:constraint_victim(failure_and_succession__internal_contradiction_reading, both_and_compromise).
narrative_ontology:constraint_victim(failure_and_succession__internal_contradiction_reading, document_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-PROPERTIED THIRD ESTATE (SNARE) — Granted universal rights in principle (Declaration of Rights), stripped of voting power in practice (censitary franchise requiring property ownership). Trapped by the internal incoherence of the settlement itself. Cannot vote for representation, cannot exit to alternative legitimacy framework, cannot make the contradiction go away. Bears the full extraction cost of the settlement's attempt to preserve both popular sovereignty AND hereditary executive power.
constraint_indexing:constraint_classification(failure_and_succession__internal_contradiction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROPERTIED ASSEMBLY / MODERATE BOURGEOISIE (TANGLED ROPE) — Benefits from the censitary franchise that concentrates voting power among property holders. Also faces genuine coordination problem: how to reconcile hereditary monarchy with any form of representation? The extraction is real (power asymmetry favoring property), but the constraint also coordinates a difficult problem — assembling legitimacy from incompatible sources. Constrained by their own ideological commitment to both universal rights AND property-based voting.
constraint_indexing:constraint_classification(failure_and_succession__internal_contradiction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONARCHY / LOUIS XVI (TANGLED ROPE) — Benefits from preservation of executive prerogative (veto power, military control, appointment authority). Faces genuine coordination problem: how to maintain feudal authority structure within a representative framework? The constraint both coordinates (legitimizes monarchical power through a written constitution) and extracts (constrains executive autonomy through legislative claims). Constrained by ideological commitment to both absolute authority AND constitutional legitimacy — cannot simply reject the assembly without abandoning the civilizing narrative of enlightened monarchy.
constraint_indexing:constraint_classification(failure_and_succession__internal_contradiction_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POPULAR SOVEREIGNTY PRINCIPLE (ROPE) — Sees the settlement purely as coordination: the written constitution IS the instantiation of popular will. The censitary limitation and hereditary executive are details to be resolved by future legislatures. From this perspective, the constraint has zero extractiveness and maximum coordination function — it is the successful negotiation of sovereignty transfer from monarchy to nation. This perspective experiences the constraint as pure rope because it identifies with the winning principle.
constraint_indexing:constraint_classification(failure_and_succession__internal_contradiction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CONSTITUTION AS ARTIFACT (PITON) — The text performs its legitimating role (appearing to settle the question of sovereignty and rights) while the underlying contradiction persists unresolved. Theater is high because the constitution claims to have resolved what it has merely stated in contradictory form. The document itself is maintained through institutional inertia — deployed to justify both monarchy and popular representation simultaneously, a performance that becomes increasingly strained as external pressure (war, emigration, counter-revolutionary mobilization) intensifies. The theater ratio is elevated by the necessity of pretending the contradiction has been solved.
constraint_indexing:constraint_classification(failure_and_succession__internal_contradiction_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SUNSET PERSPECTIVE (SCAFFOLD) — From a removed vantage, the 1791 settlement was inherently temporary: a constitutional framework that codified contradictions could only persist as long as external pressure remained manageable. The constraint has a logical sunset — the contradictions must resolve through rupture once enforcement costs rise. This reading sees the framework as a temporary bridging mechanism (scaffold) that was always bound to fail once the founding generation's political will eroded or external shock (war, counter-revolutionary mobilization) raised the cost of maintaining coherence. The sunset is built into the logical structure, not dependent on specific events.
constraint_indexing:constraint_classification(failure_and_succession__internal_contradiction_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(failure_and_succession__internal_contradiction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(failure_and_succession__internal_contradiction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(failure_and_succession__internal_contradiction_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(failure_and_succession__internal_contradiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(failure_and_succession__internal_contradiction_reading, TR),
    TR >= 0.70.

:- end_tests(failure_and_succession__internal_contradiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising over the interval (0.42 → 0.72). The settlement begins with manageable extraction during the founding moment (1791), when revolutionary prestige and the novelty of written constitutionalism suppress awareness of the contradiction. As the incompatibility between hereditary monarchy and popular representation becomes operationally apparent (the king's failed flight to Varennes in June 1791, the assembly's legislative encroachment on executive prerogative, the growing radicalization of the non-propertied majority who lack voting power), the extraction cost rises. By 1793, the contradiction can no longer be suppressed without escalating coercive requirements. Suppression (0.68): Consistently high and rising (0.55 → 0.85). The contradiction's suppression requires active enforcement by both monarchy and assembly to prevent it from becoming politically salient. The monarchy must suppress its knowledge that the assembly claims sovereignty incompatible with monarchical authority. The assembly must suppress awareness that hereditary executives retain coercive power. The non-propertied majority must suppress the incoherence of being granted universal rights while denied voting power. As external pressure (war with Austria and Prussia, counter-revolutionary mobilization, food crisis, emigration) intensifies, the suppression machinery consumes more resources, making the constraint increasingly visible. Theater ratio (0.35 → 0.80): The performance of constitutional coherence escalates as the underlying incoherence becomes apparent. The constitution itself performs the act of resolving the contradiction (by declaring both principles equally binding) despite having no mechanism to resolve them. By 1792-1793, the theater becomes absurd: the same constitutional text is invoked to justify the king's authority and the assembly's sovereignty, with both sides citing it as evidence their principle is supreme.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's extractive structure across indexical positions. The non-propertied majority (powerless/trapped) perceive a snare: universal rights without voting power. The propertied bourgeoisie (organized/constrained) perceive tangled rope: genuine coordination of sovereignty transfer plus extraction through censitary restriction. The monarchy (powerful/constrained) perceive tangled rope: constitutional legitimacy plus legislative encroachment. Popular sovereignty (institutional/arbitrage) perceives pure rope: the constitution successfully instantiates the people's will. The document itself (institutional/arbitrage) performs coherence while the underlying contradiction persists (piton). The analytical observer (analytical/analytical) perceives a scaffold with predetermined sunset: a temporary bridge that must fail as soon as external pressure or internal radicalization raises the cost of suppressing the contradiction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) flows from their structural position relative to the contradiction. Non-propertied citizens are trapped (no exit option) and victimized (denied voting rights despite universal rights declaration) — they experience maximum d and maximum f(d), producing maximum chi. The propertied bourgeoisie are organized (can mobilize as a class) and benefit from the censitary franchise — they experience moderate d offset by beneficiary status. The monarchy is powerful but constrained (cannot openly repudiate the constitution without abandoning legitimacy narrative) — their d is determined by their victim status (legislative encroachment on prerogative) offset by residual power. The popular sovereignty principle experiences low d because it identifies with the victorious side of the contradiction. The monarchical principle experiences high d because it identifies with the defeated side.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the settlement's collapse is not a contingent disaster but a structural inevitability given the incoherent principles it attempted to reconcile. The mandatrophy question 'Is this coordination or extraction?' receives different answers from different perspectives: the propertied bourgeoisie see mostly coordination (sovereignty transfer) with extraction confined to the censitary limitation; the non-propertied majority see mostly extraction (universal rights denied); the monarchy see mixed extraction (legislative encroachment) and coordination (constitutional legitimacy); the analytical observer sees the entire structure as a temporary scaffold destined to fail. The settlement's death resolves the mandatrophy by eliminating the both-and compromise: post-1792, each principle (hereditary authority vs popular sovereignty) must justify itself independently rather than claim simultaneous truth. The rupture is the mandatrophy's resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_principle_won,
    'Which principle was the settlement''s genuine kernel — hereditary executive power or popular sovereignty through representation?',
    'Examine the substantive distribution of coercive and legislative power in the 1791 structure; analyze which actor could unilaterally override the other in genuine crisis; track which principle the subsequent constitutional settlements (1793, 1795, 1799, 1804) preserved or abandoned.',
    'If hereditary executive was the kernel: the settlement''s collapse reveals extractive capture of popular sovereignty language. If popular sovereignty was the kernel: the settlement''s collapse reveals the hereditary executive as a failed compatibility attempt. This determines whether the victimhood is borne by the monarchy or the non-propertied majority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(which_principle_won, empirical, 'Which principle constituted the settlement''s actual legitimacy kernel').

omega_variable(
    incoherence_enforceability,
    'Could the settlement''s contradiction have been enforced indefinitely through continued suppression, or was rupture logically inevitable?',
    'Comparative historical analysis: identify other political settlements that maintained contradictory legitimacy claims over longer periods (e.g., British constitutional monarchy balancing parliamentary and royal authority); determine what structural features enabled indefinite maintenance versus what triggered rupture in the French case.',
    'If enforceability was possible: the constraint''s failure was contingent (political incompetence, war, emigration, etc.), not structural. If enforceability was impossible: the constraint was a scaffold with a predetermined sunset. This changes whether extractiveness was the operative mechanism or logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_enforceability, conceptual, 'Whether incoherence was enforceably sustainable or logically destined to rupture').

omega_variable(
    suppression_mechanism_source,
    'What enforced the suppression of the contradiction — coercive state apparatus, ideological commitment, material interests, or the weight of revolutionary prestige?',
    'Granular analysis of enforcement agents: the National Guard, the Assembly''s majority, the king''s acquiescence, the bourgeoisie''s investment in property-backed voting rights. Track which enforcement mechanism collapsed first and what triggered its collapse.',
    'If coercive suppression was primary: state apparatus failure would be the collapse mechanism (and suppression should be measured differently). If ideological commitment was primary: loss of revolutionary consensus would be the trigger. If material interests held the system: economic crisis would destabilize it. This specifies which causal pathway ran from contradiction to rupture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Primary enforcement mechanism for contradiction suppression').

omega_variable(
    this_reading_vs_siblings_committer,
    'Is the 1791 settlement''s collapse best explained as logical rupture from internal incoherence (this reading), as a constitutional template establishing discontinuous succession (template sibling), or as external disruption by war (war sibling)?',
    'Chronological and causal analysis: does the timeline of contradictions (censitary franchise limitation, non-ratification of royal veto override, assembly''s legislative encroachment) precede and enable the April 1792 war declaration, or does war precipitate the contradictions'' surfacing? Were earlier French constitutions (post-1792) designed to learn from the 1791 incoherence, or were they discontinuous responses to warfare?',
    'If internal contradiction reading is true: the settlement''s collapse is deterministic given its logical structure. If template reading is true: the collapse is merely one episode in a series, with later constitutions explicitly designed to avoid the 1791 mistakes. If war reading is true: the collapse is contingent on external shock, not internal structure. These readings have different implications for whether the incoherence was the proximate cause or merely exposed by external pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_vs_siblings_committer, conceptual, 'Committer frame: which causal pathway explains collapse — internal contradiction, constitutional template learning, or external war').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(failure_and_succession__internal_contradiction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intcon_tr_t0, failure_and_succession__internal_contradiction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(intcon_tr_t6, failure_and_succession__internal_contradiction_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement(intcon_tr_t12, failure_and_succession__internal_contradiction_reading, theater_ratio, 12, 0.62).
narrative_ontology:measurement(intcon_tr_t24, failure_and_succession__internal_contradiction_reading, theater_ratio, 24, 0.8).

% Extraction over time
narrative_ontology:measurement(intcon_be_t0, failure_and_succession__internal_contradiction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(intcon_be_t6, failure_and_succession__internal_contradiction_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(intcon_be_t12, failure_and_succession__internal_contradiction_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(intcon_be_t24, failure_and_succession__internal_contradiction_reading, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(intcon_su_t0, failure_and_succession__internal_contradiction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(intcon_su_t24, failure_and_succession__internal_contradiction_reading, suppression_requirement, 24, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(failure_and_succession__internal_contradiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(failure_and_succession__internal_contradiction_reading, failure_and_succession__template_for_successors_reading).
narrative_ontology:affects_constraint(failure_and_succession__internal_contradiction_reading, failure_and_succession__war_destroyed_it_reading).

% DUAL FORMULATION NOTE:
% The kernel 'failure_and_succession' has three readings instantiated as separate constraint stories: (1) internal_contradiction_reading (this file) — collapse driven by logical incoherence; (2) template_for_successors_reading — 1791 establishes constitutional discontinuity as a French pattern; (3) war_destroyed_it_reading — external war made internal contradictions politically fatal. These are not alternative measurements of the same constraint; they are genuinely different constraints with different ε values, different beneficiary/victim structures, and different time horizons. The internal_contradiction reading produces high extractiveness (0.58) from the contradiction's suppression cost. The template reading produces lower extractiveness because it sees the settlement as a successful episode in learning. The war reading produces a different victim set (the security-compromised monarchy) and different beneficiary set (counter-revolutionary powers). All three are causally linked: internal contradiction enabled war to destroy the settlement, which demonstrated the need for constitutional template learning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
