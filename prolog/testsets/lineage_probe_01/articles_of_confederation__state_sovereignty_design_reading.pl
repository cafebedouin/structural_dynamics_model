% ============================================================================
% CONSTRAINT STORY: articles_of_confederation__state_sovereignty_design_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_articles_of_confederation__state_sovereignty_design_reading, []).

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
 *   constraint_id: articles_of_confederation__state_sovereignty_design_reading
 *   human_readable: Articles of Confederation as Deliberate State Sovereignty Design (Coordination Reading)
 *   domain: constitutional_law/founding_doctrine
 *
 * SUMMARY:
 *   The Articles of Confederation instantiate a contested kernel that
 *   produces three structurally distinct constraints. This constraint story
 *   generates ONLY the state_sovereignty_design_reading: the interpretation
 *   that Article II was a deliberate political choice to preserve state
 *   sovereignty through a league structure, not an accidental failure of
 *   national consolidation. Under this reading, the Articles are a successful
 *   coordination mechanism that solves the problem 'how do we form a union
 *   without creating a tyrannical central power?' The extractiveness (0.28)
 *   is low because the constraint functions to enable coordination without
 *   excessive centralization. Suppression (0.38) is moderate: states are
 *   prevented from being taxed nationally, but this is by design, not by
 *   coercion. The theater_ratio rises over the interval (0.25 → 0.45) because
 *   the gap between rhetorical commitment to state sovereignty and actual
 *   behavior (tariff wars, currency instability, trade barriers) widens after
 *   1785. The constraint remains a rope at the state beneficiary level
 *   throughout, but the performative dimension increases as the structural
 *   problems become visible while the formal principle persists. This reading
 *   coexists with two siblings: the requisition_failure_reading (which
 *   interprets the same Articles as a coordination problem that failed
 *   because states didn't pay requisitions) and the unanimity_trap_reading
 *   (which interprets the unanimity rule as the core structural problem, not
 *   the sovereignty principle itself). All three readings are live positions
 *   in the 1787 Convention debates and in subsequent constitutional
 *   historiography.
 *
 * KEY AGENTS:
 *   - State Legislatures: Primary beneficiary (powerful/mobile) — retain control of taxation and can exit the league in principle, though doing so is costly. Article II preserves state autonomy.
 *   - Continental Congress: Secondary actor (moderate/constrained) — coordinates military and diplomatic action but dependent on state requisitions. Experiences tangled rope: genuine coordination function mixed with structural weakness.
 *   - War Debt Creditors and Unpaid Soldiers: Primary victim (powerless/trapped) — financed the war on the assumption of national repayment; trapped by the structural design that prevents Congress from raising revenue. Experience snare: pure extraction masked by procedural form.
 *   - Nationalist Coalition (Hamilton, Madison, Washington): Organized agents (organized/constrained) — see Article II as temporary scaffolding requiring amendment; formed the 1787 Convention to replace it. Treat the constraint as having a sunset.
 *   - State-Sovereignty Defenders: Institutional actors (institutional/mobile) — affirm Article II as principle; resist centralization. Maintain piton-like commitment through rhetoric while violating the principle in practice (tariffs, currency).
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing Article II as an immutable feature of federal design rather than a deliberate political choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(articles_of_confederation__state_sovereignty_design_reading, 0.28).
domain_priors:suppression_score(articles_of_confederation__state_sovereignty_design_reading, 0.38).
domain_priors:theater_ratio(articles_of_confederation__state_sovereignty_design_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(articles_of_confederation__state_sovereignty_design_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(articles_of_confederation__state_sovereignty_design_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(articles_of_confederation__state_sovereignty_design_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(articles_of_confederation__state_sovereignty_design_reading, rope).
narrative_ontology:human_readable(articles_of_confederation__state_sovereignty_design_reading, "Articles of Confederation as Deliberate State Sovereignty Design (Coordination Reading)").
narrative_ontology:topic_domain(articles_of_confederation__state_sovereignty_design_reading, "constitutional_law/founding_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(articles_of_confederation__state_sovereignty_design_reading, 'd1add5d6-f5ae-4b08-877f-bc0ba15dbbea').
narrative_ontology:cs_kernel_codification('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', formalized).
narrative_ontology:cs_authority_grounding('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', lineage).
narrative_ontology:cs_interpretation_layer_present('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea').
narrative_ontology:cs_reading_relation('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', articles_of_confederation__requisition_failure_reading, coexists_with).
narrative_ontology:cs_reading_relation('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', articles_of_confederation__unanimity_trap_reading, coexists_with).
narrative_ontology:cs_axiom('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', foundational, state_sovereignty_is_preservable_through_league_structure).
narrative_ontology:cs_axiom_status(state_sovereignty_is_preservable_through_league_structure, holdable).
narrative_ontology:cs_axiom_grounding('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', state_sovereignty_is_preservable_through_league_structure, deontological).
narrative_ontology:cs_axiom('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', foundational, national_consolidation_is_threat_requiring_structural_prevention).
narrative_ontology:cs_axiom_status(national_consolidation_is_threat_requiring_structural_prevention, holdable).
narrative_ontology:cs_axiom_grounding('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', national_consolidation_is_threat_requiring_structural_prevention, instrumental).
narrative_ontology:cs_reference_frame('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', state_sovereignty_preservation_principle).
narrative_ontology:cs_drift_state('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', post_1785_violations_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d1add5d6-f5ae-4b08-877f-bc0ba15dbbea', '').
narrative_ontology:cs_kernel_id(articles_of_confederation__state_sovereignty_design_reading, articles_of_confederation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(articles_of_confederation__state_sovereignty_design_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(articles_of_confederation__state_sovereignty_design_reading, state_sovereignty_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE LEGISLATURES (ROPE) — From the state-centered reading, Article II is a coordination mechanism that solves a genuine problem: how do we form a league of equal sovereigns without one overarching power consuming the others? Requisitions are the coordination cost — states ask Congress for funds; Congress coordinates military and diplomatic action. The suppression of national taxation is not a defect but a feature: it preserves state control over their own revenue. This perspective sees the constraint as pure coordination with minimal extraction. States could in principle exit (form bilateral treaties, abandon the league) but gain sufficient benefit from mutual defense and trade coordination that they remain.
constraint_indexing:constraint_classification(articles_of_confederation__state_sovereignty_design_reading, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CONTINENTAL CONGRESS (TANGLED ROPE) — From Congress's perspective, the constraint is mixed: it coordinates interstate commerce and military action (coordination function) but is structurally constrained by its inability to tax or compel state compliance (extraction asymmetry against Congress). Congress benefits from legitimacy as a representative body but bears the cost of dependent status on state requisitions. States bear the cost of being unable to simply raise funds nationally. This is tangled rope: genuine coordination function (Congress genuinely does coordinate) alongside asymmetric extraction (states retain power-of-the-purse leverage). Congress's exit options are constrained — they cannot simply become a national government without amendment; they can only petition for requisitions and hope.
constraint_indexing:constraint_classification(articles_of_confederation__state_sovereignty_design_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WAR DEBT CREDITORS AND UNPAID SOLDIERS (SNARE) — From the creditor perspective, the constraint is pure extraction: they are trapped (the debt is owed; they cannot exit the obligation without losing their claim). They bear the cost of Article II's principle — there is no national revenue stream to pay the debt. The design suppresses their alternatives (they cannot petition individual states for their full share, because the debt is a collective national obligation, and state treasuries are empty). Congress has no mechanism to extract revenue from unwilling states. The creditors experience maximum extraction — they financed the war, and the structural design ensures they cannot recover. States benefit from the design precisely because it prevents their wealth from flowing to cover collective debts.
constraint_indexing:constraint_classification(articles_of_confederation__state_sovereignty_design_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / IMMUTABLE FEATURE VIEW (MOUNTAIN) — From a civilizational analytical perspective, this constraint might appear as an immutable feature of federal design: any league of sovereign states faces an inherent tension between coordination (which requires central power) and sovereignty preservation (which restricts central power). This view risks naturalizing Article II as a law of confederal architecture rather than a deliberate political choice. However, the structural data undermines this classification: the constraint has identifiable beneficiaries (state legislatures), identifiable victims (creditors and Congress), and measurable extractiveness. The mountain classification is vulnerable to false-summit detection.
constraint_indexing:constraint_classification(articles_of_confederation__state_sovereignty_design_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: NATIONALIST COALITION (SCAFFOLD) — Organized agents who believe national consolidation is necessary (Hamilton, Madison, Washington) see the Articles as temporary scaffolding: the constraint has a sunset clause in the form of Article XIII amendment requirement (though unanimity makes the sunset enforced rather than voluntary). This perspective treats Article II's state-sovereignty principle as a transitional mechanism — necessary to get the states into the league but requiring replacement through the formal amendment process. The constraint has low theater and clear function: it coordinates action while preserving state trust. The sunset is visible (the 1787 Convention, which occurred 13 years after ratification). From this view, the constraint is not stable indefinitely — it has a built-in limitation and a known replacement (the Constitution).
constraint_indexing:constraint_classification(articles_of_confederation__state_sovereignty_design_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LOYALIST OPPOSITION / RUMP INSTITUTIONS (PITON) — From the perspective of those committed to the Articles (some state legislatures, some conservative delegates), the constraint persists through institutional inertia and performative commitment. States verbally affirm Article II — state sovereignty is sacred — while simultaneously violating the constraint (the Southern states impose their own tariffs; New York blocks commerce; states issue their own currency). The principle is maintained theatrically (speeches, formal affirmations) while the coordination function atrophies. This is piton: the constraint survives not because it functions but because abandoning it would require formal admission that the design was wrong. Theater ratio reflects the gap between rhetorical commitment to state sovereignty and actual behavior (protective tariffs, tariff wars, currency instability).
constraint_indexing:constraint_classification(articles_of_confederation__state_sovereignty_design_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(articles_of_confederation__state_sovereignty_design_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(articles_of_confederation__state_sovereignty_design_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(articles_of_confederation__state_sovereignty_design_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(articles_of_confederation__state_sovereignty_design_reading, TR),
    TR >= 0.70.

:- end_tests(articles_of_confederation__state_sovereignty_design_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. Under the state_sovereignty_design_reading, the Articles extract relatively little because the state beneficiaries willingly accept the constraints in exchange for preservation of their own revenue authority and policy independence. The extractiveness is not zero because Congress's dependence on state requisitions creates a real constraint on national action and forces creditors to bear uncompensated losses. But it is lower than the requisition_failure_reading would assign (which would frame the same metrics as 0.55+) because this reading interprets the requisition system as legitimate coordination rather than as failed centralization. Suppression (0.38): Moderate. States are prevented from being directly taxed by Congress, but this is a feature of the design, not a violation imposed by force. The suppression reflects the deliberate choice to restrict central power. Creditors face suppression (no mechanism to force payment), but from the state beneficiary perspective, this suppression is justified as necessary to prevent consolidation. Theater (0.25 → 0.45): Initial theater is low because the coordination function genuinely works — states do gather in Congress, do coordinate on some issues, and the legal form matches the functional reality. Theater rises over the interval because the structural problems become visible: states impose tariffs despite Article VI (free trade principle), issue their own currency despite its prohibition, and maintain militias despite Article IX (Congress controls military). By 1787, states are violating the Articles in practice while maintaining rhetorical commitment to them. The risen theater reflects the gap between the principle and behavior.
 *
 * PERSPECTIVAL GAP:
 *   The state_sovereignty_design_reading produces large perspectival gaps. State legislatures see coordination (rope) — they gain security and commerce benefits. Congress sees tangled constraint — genuine coordination function mixed with structural dependence. Creditors see pure extraction (snare) — trapped, unpaid, with no mechanism for recovery. Nationalists see temporary scaffolding (scaffold) — necessary but requiring replacement. Conservative defenders see inertial commitment (piton) — affirmed in principle, violated in practice. The analytical observer risks seeing immutable law (mountain) but the structural data reveals deliberate design. These gaps are not measurement error — they reflect real differences in structural position. The constraint is objectively rope from the beneficiary view and objectively snare from the creditor view; both classifications are correct relative to their respective agents.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislatures (powerful/mobile) derive low d because they are beneficiaries with exit options: they could in principle form bilateral treaties or abandon the league, but gain enough security and coordination benefit that they remain. Institutional power + mobile exit + beneficiary status → low d → negative or low f(d) → low effective extraction on beneficiaries. Continental Congress (moderate/constrained) derives higher d because it is a victim of the design: Congress cannot exit (it is constituted by the Articles), cannot tax (Article I restricts it), and experiences the burden of dependence. Moderate power + constrained exit + victim status → medium-high d → medium f(d) → moderate experienced extraction. War creditors (powerless/trapped) derive maximum d: they are completely trapped (the debt is owed, they cannot exit their creditor status), powerless (they have no authority to demand anything), and victims (they bear all costs). Powerless power + trapped exit + victim status → high d → high f(d) → high experienced extraction. The beneficiary perspective's low extraction and the victim perspective's high extraction on the same structural constraint reflect real differences in d driven by power and exit options, not measurement ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not face mandatrophy because ε = 0.28 (below the 0.46 threshold for high-extraction constraints where mandatrophy arises). The constraint is classified as rope — pure coordination with minimal extraction — and this classification is stable across perspectives at the designated power level (state legislatures, institutional, mobile exit). The mandatrophy would arise if a different reading (requisition_failure_reading) were adopted, which would assign ε = 0.55+ and classify as tangled_rope or snare. But within this reading, the classification is clean.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    design_intent_vs_failure,
    'Was Article II a deliberately chosen design commitment to state sovereignty, or was it a compromise that everyone acknowledged would need correction?',
    'Historical documents: Federalist Papers (esp. Madison, Hamilton on the Articles'' defects vs. their structure); Anti-Federalist defenses of state sovereignty; delegates'' private correspondence; ratification debates comparing design intent to design constraint. Distinguish between ''we built this deliberately'' vs. ''this was the price of getting states to agree, knowing it would fail.''',
    'If design-intent: the constraint is rope (coordination mechanism preserving state autonomy). If acknowledged-failure: the constraint is tangled_rope or snare (extraction of state power with false pretense of successful coordination). The classification shifts based on the historical actors'' own understanding of what they were building.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_intent_vs_failure, empirical, 'Whether Article II was deliberate design or acknowledged compromise').

omega_variable(
    requisition_vs_taxation_structural_delta,
    'Is the difference between requisition-based and tax-based revenue a structural design choice or a functional difference with identical distributional outcomes?',
    'Comparative fiscal analysis: requisition enforcement rates vs. tax collection rates in other confederacies (Swiss cantons, Dutch Republic); modeling of extraction distribution under voluntary vs. compulsory revenue. Does the moral/legal difference (state legislature votes vs. central power extracts) produce measurable behavioral differences in willingness to pay?',
    'If structural: requisitions are a genuine coordination mechanism (rope). If functional-identity: requisitions are taxation by another name, and the constraint is pure suppression of revenue collection authority (snare or tangled_rope). Extractiveness value depends on this resolution: if requisitions shift costs to creditors while preserving state autonomy, ε = 0.28 (current); if requisitions shift costs AND fail to deliver, ε rises to 0.55.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(requisition_vs_taxation_structural_delta, empirical, 'Whether requisition system is structurally distinct from taxation').

omega_variable(
    state_mobility_vs_constraint,
    'Did states retain genuine exit options, or did Article II lock them in while claiming they were free?',
    'Historical analysis: what states actually did when Congress requested requisitions and they refused; what the consequences were; whether any state faced military or economic coercion for non-compliance. Did states have the option to abandon the league without military consequences? How many states seriously considered exit?',
    'If exit was mobile: states could truly leave, and the constraint is coordination (rope). If exit was theoretically possible but practically costly: the constraint is constrained exit (tangled_rope). If exit was blocked (military or economic coercion): the constraint is trapped exit (snare). This directly affects which exit option should apply in the beneficiary perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_mobility_vs_constraint, empirical, 'Whether states had genuine exit options or faced coercion').

omega_variable(
    reading_contest_kernel_structure,
    'This constraint is one reading of the contested Articles kernel. The sibling readings (requisition_failure_reading, unanimity_trap_reading) inhabit the same historical moment and the same legal text. How do they relate logically?',
    'This is routed to cs_structure.reading_relations and cs_structure.axioms per the committer frame. The three readings coexist: different parties (nationalists, state-sovereignty defenders, reform-minded federalists) hold all three simultaneously. See cs_structure section for the formal structure.',
    'Recognizing the reading contest prevents naturalizing any single reading as ''the truth about the Articles.'' The kernel (Article II, requisition system, unanimity rule) is ambiguous; different readings instantiate different constraints with different ε values, different beneficiary/victim sets, and different classifications. The Articles are not a unified constraint — they are a site of reading contest that produces multiple constraints depending on which reading the observer adopts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_structure, conceptual, 'The Articles as a contested kernel with multiple readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(articles_of_confederation__state_sovereignty_design_reading, 1781, 1794).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, articles_of_confederation__state_sovereignty_design_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t7, articles_of_confederation__state_sovereignty_design_reading, theater_ratio, 7, 0.35).
narrative_ontology:measurement(arti_tr_t13, articles_of_confederation__state_sovereignty_design_reading, theater_ratio, 13, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, articles_of_confederation__state_sovereignty_design_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(arti_be_t7, articles_of_confederation__state_sovereignty_design_reading, base_extractiveness, 7, 0.25).
narrative_ontology:measurement(arti_be_t13, articles_of_confederation__state_sovereignty_design_reading, base_extractiveness, 13, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(articles_of_confederation__state_sovereignty_design_reading, resource_allocation).
narrative_ontology:affects_constraint(articles_of_confederation__state_sovereignty_design_reading, articles_of_confederation__requisition_failure_reading).
narrative_ontology:affects_constraint(articles_of_confederation__state_sovereignty_design_reading, articles_of_confederation__unanimity_trap_reading).

% DUAL FORMULATION NOTE:
% The Articles of Confederation kernel produces three constraints via three distinct readings. state_sovereignty_design_reading (this file) interprets Article II as deliberate design preserving state autonomy (rope, ε=0.28). requisition_failure_reading interprets the same Articles as a failed revenue system that collapsed under structural pressure (tangled_rope/snare, ε=0.55+). unanimity_trap_reading interprets Article XIII unanimity rule as the core problem preventing amendment (piton/snare, ε=0.50+). Each reading is a self-contained constraint with its own perspectives, measurements, and omega variables. They are linked via network.affects_constraints to show they are readings of the same kernel. The kernel itself is ambiguous — the Articles admit of all three readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
