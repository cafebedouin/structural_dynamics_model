% ============================================================================
% CONSTRAINT STORY: eu_membership_and_exit__sovereignty_pooled_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_membership_and_exit__sovereignty_pooled_reading, []).

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
 *   constraint_id: eu_membership_and_exit__sovereignty_pooled_reading
 *   human_readable: EU Membership as Pooled Sovereignty (Profitably Exercised)
 *   domain: constitutional_law/doctrinal
 *
 * SUMMARY:
 *   When the United Kingdom joined the European Communities in 1973,
 *   Parliament enacted the European Communities Act 1972, which provided that
 *   EU law would have supremacy over conflicting English law in specified
 *   domains. For 47 years, this supremacy operated without formal
 *   constitutional change: Parliament retained the theoretical capacity to
 *   repeal the 1972 Act and restore full domestic rule-making, but no
 *   Parliament actually did so. The constraint under examination is the
 *   structural relationship created by membership: the pooling of sovereign
 *   rule-making authority in specified domains (competition law, trade,
 *   environmental protection, social policy) in exchange for access to the
 *   single market and reciprocal legal recognition from other member states.
 *   This reading emphasizes that the pooling was Parliament's continuing
 *   choice — not externally imposed, but self-imposed through statute and
 *   renewed through Parliament's failure to repeal the integrating
 *   legislation. The extractiveness of the constraint is moderate (0.38)
 *   because compliance with EU supremacy extracted a real cost (incompatible
 *   domestic statutes must be disapplied, national rule divergence is
 *   prohibited in certain domains), but Parliament received substantial
 *   compensation through market access and treaty reciprocity. The
 *   constraint's core claim is that Parliament never lost sovereignty — it
 *   exercised sovereignty BY CHOOSING to pool it, and the choice was
 *   revocable.
 *
 * KEY AGENTS:
 *   - Parliament: Institutional actor (institutional/arbitrage) — the primary beneficiary interpreting membership as continuing sovereign choice; retains structural capacity to exit
 *   - Incompatible Domestic Statutes: Powerless/trapped — must be disapplied wherever EU law supremacy applies; have no voice in the suppression that affects them
 *   - Member State Economies: Moderate/constrained — benefit from market access and regulatory harmonization but constrained by loss of divergent policy options
 *   - EU Commission and Court: Institutional/constrained — benefit from supremacy doctrine as the mechanism enabling their authority; must enforce suppression of conflicting national law
 *   - Domestic Courts: Institutional/constrained — must disapply Acts of Parliament contrary to EU law (unprecedented since 1689); experience this as procedurally enforced, not self-chosen
 *   - Brexit Referendum Coalition: Organized/mobile — demonstrates the sunset clause empirically; shows that pooling was revocable through coordinated political action
 *   - The Doctrine of Parliamentary Sovereignty: Institutional/analytical — a post-hoc rationale (piton) that reconciles the observable fact of EU supremacy with the constitutional tradition of Parliamentary supremacy; increasingly performative as the doctrine's fit with empirical reality deteriorates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_membership_and_exit__sovereignty_pooled_reading, 0.38).
domain_priors:suppression_score(eu_membership_and_exit__sovereignty_pooled_reading, 0.48).
domain_priors:theater_ratio(eu_membership_and_exit__sovereignty_pooled_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_membership_and_exit__sovereignty_pooled_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_membership_and_exit__sovereignty_pooled_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(eu_membership_and_exit__sovereignty_pooled_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_membership_and_exit__sovereignty_pooled_reading, tangled_rope).
narrative_ontology:human_readable(eu_membership_and_exit__sovereignty_pooled_reading, "EU Membership as Pooled Sovereignty (Profitably Exercised)").
narrative_ontology:topic_domain(eu_membership_and_exit__sovereignty_pooled_reading, "constitutional_law/doctrinal").

domain_priors:requires_active_enforcement(eu_membership_and_exit__sovereignty_pooled_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_membership_and_exit__sovereignty_pooled_reading, '8eff64fd-f92c-43a9-974c-c48a422801a6').
narrative_ontology:cs_kernel_codification('8eff64fd-f92c-43a9-974c-c48a422801a6', fixed_text).
narrative_ontology:cs_authority_grounding('8eff64fd-f92c-43a9-974c-c48a422801a6', lineage).
narrative_ontology:cs_interpretation_layer_present('8eff64fd-f92c-43a9-974c-c48a422801a6').
narrative_ontology:cs_reading_relation('8eff64fd-f92c-43a9-974c-c48a422801a6', eu_membership_and_exit__sovereignty_lost_reading, coexists_with).
narrative_ontology:cs_reading_relation('8eff64fd-f92c-43a9-974c-c48a422801a6', eu_membership_and_exit__sovereignty_restored_reading, influences).
narrative_ontology:cs_axiom('8eff64fd-f92c-43a9-974c-c48a422801a6', foundational, parliament_retains_sovereign_authority).
narrative_ontology:cs_axiom_status(parliament_retains_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('8eff64fd-f92c-43a9-974c-c48a422801a6', parliament_retains_sovereign_authority, deontological).
narrative_ontology:cs_axiom('8eff64fd-f92c-43a9-974c-c48a422801a6', foundational, pooling_is_continuing_sovereign_choice).
narrative_ontology:cs_axiom_status(pooling_is_continuing_sovereign_choice, holdable).
narrative_ontology:cs_axiom_grounding('8eff64fd-f92c-43a9-974c-c48a422801a6', pooling_is_continuing_sovereign_choice, conventional).
narrative_ontology:cs_reference_frame('8eff64fd-f92c-43a9-974c-c48a422801a6', parliamentary_sovereignty_doctrine).
narrative_ontology:cs_drift_state('8eff64fd-f92c-43a9-974c-c48a422801a6', contemporary_post_brexit, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8eff64fd-f92c-43a9-974c-c48a422801a6', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(eu_membership_and_exit__sovereignty_pooled_reading, eu_membership_and_exit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_membership_and_exit__sovereignty_pooled_reading, parliament_as_sovereign_actor).
narrative_ontology:constraint_beneficiary(eu_membership_and_exit__sovereignty_pooled_reading, market_integration_beneficiaries).
narrative_ontology:constraint_beneficiary(eu_membership_and_exit__sovereignty_pooled_reading, treaty_reciprocity_agents).
narrative_ontology:constraint_victim(eu_membership_and_exit__sovereignty_pooled_reading, incompatible_domestic_statutes).
narrative_ontology:constraint_victim(eu_membership_and_exit__sovereignty_pooled_reading, autonomy_of_national_rule_formation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARLIAMENT AS SOVEREIGN ACTOR (ROPE) — Experiences membership as a continuing choice made under Doctrine of Parliamentary Sovereignty. Supremacy of EU law is not imposed but self-imposed through Acts of Parliament. Exit is structurally available (revocation through statute). The constraint coordinates Parliament's commitment to reciprocal market access with retained legal authority to withdraw. This is pure coordination: no extraction, only the pooling of rule-making authority in exchange for treaty benefits. Parliament benefits from arbitrage — can adjust participation terms within treaty framework or exit entirely.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_pooled_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: INCOMPATIBLE DOMESTIC STATUTES (SNARE) — Cannot coexist with supremacy of EU law; must be disapplied or repealed. No exit option: the statute exists but is legally inert when EU law conflicts. From the standpoint of the domestic legal order conceived as independent, the statute is suppressed. This perspective experiences the constraint as pure extraction: the statute is eliminated from operative law without any reciprocal benefit to itself. The suppression is near-total (0.90+) because the statute has no legal standing once EU law supremacy applies.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_pooled_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MEMBER STATE ECONOMIES INTEGRATED (TANGLED ROPE) — Constrained by dependency on cross-border market access and value chains that require legal alignment. But also benefits substantially from EU single-market rules, capital mobility, and regulatory harmonization. The constraint produces both coordination (mutual market access, reciprocal legal recognition) and asymmetric extraction (regulatory harmonization costs, loss of divergent domestic policy options). Member state actors see suppression of alternative regulatory paths, but also genuine coordination benefits. Exit is costly but not impossible (constrained, not trapped).
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_pooled_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: EU COMMISSION AND COURT (TANGLED ROPE) — Acts as both coordinator (harmonizing rules across member states) and extractor (enforcing supremacy doctrine, adjudicating non-compliance). Benefits from supremacy doctrine as the mechanism enabling their institutional authority. Suppression comes from their enforcement apparatus — member states' domestic courts must disapply conflicting national law, and member state legislatures are constrained in their rule-making by prior EU commitments. This perspective also sees a genuine coordination function (enabling cross-border law as a public good) coupled with asymmetric extraction (the EU institutions gain authority and budgetary control in exchange for this coordination).
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_pooled_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: DOCTRINE OF PARLIAMENTARY SOVEREIGNTY (PITON) — From the civilizational view, the claim that 'Parliament retained sovereignty because it could revoke membership at any time' is substantially performative. The doctrine does not functionally adjudicate whether Parliament actually retained meaningful authority; it is maintained as a conceptual reconciliation between British constitutional law (Parliamentary Supremacy) and the observable fact of EU legal supremacy. The performance of this doctrine increases over time (theater ratio rises) as the empirical tension between the doctrine and observed legal reality becomes more acute. The doctrine persists through institutional inertia (law schools teach it, courts cite it, constitutional scholars defend it) rather than through demonstrable functional force.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_pooled_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: BREXIT REFERENDUM COALITION (SCAFFOLD) — Organized agents (Parliament under the European Union Referendum Act 2015, then under the Great Repeal Bill logic) demonstrate that the sovereignty-pooling reading is empirically testable. If membership is truly revocable at will (sunset clause), then the referendum and exit acts should succeed in restoring Parliament's domestic authority. The coalition experiences the constraint as temporary and solvable through coordinated action. The sunset is not institutional (fading away) but political (exit enacted through statute). Low effective extraction because the exit mechanism is visible and available.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_pooled_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational scope, this perspective treats the structure of the constraint as an immutable principle of international law: once legally integrated into a supranational authority, a state's domestic sovereignty becomes structurally unrecoverable because the integration creates facts on the ground (vested rights, reliance interests, interconnected rule networks) that cannot be simply unspun. This view naturalizes the constraint as a law of institutional physics. However, the Brexit process falsified this perspective empirically: Parliament did unwind supremacy through statute (European Union (Withdrawal) Act 2018). The analytical observer's mountain classification becomes a false summit, revealing that the immutability framing obscures political-constitutional choices that were actually revocable.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_pooled_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_membership_and_exit__sovereignty_pooled_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_pooled_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_pooled_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_membership_and_exit__sovereignty_pooled_reading, TR),
    TR >= 0.70.

:- end_tests(eu_membership_and_exit__sovereignty_pooled_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint requires suppression of certain domestic rules and loss of divergent rule-making capacity in specified domains. However, Parliament receives substantial compensation through market access and treaty reciprocity. The extractiveness is not zero (there are real costs to compliance) but also not high (the benefits are substantial). The trajectory over time (0.15 → 0.38) reflects the accumulation of regulatory dependency: as EU rules became more extensive and as member-state economies became more integrated, the cost of exit or rule divergence increased. This is not rising extraction so much as rising exit costs — the constraint becomes more binding as the structure around it solidifies. Suppression (0.48): Moderate-to-high. Domestic statutes that conflict with EU law must be disapplied; member states are prohibited from adopting rules that diverge from EU harmonization in specified domains. The suppression is not total (many domains remain under national control) and it is explicitly consented to through the EU treaty framework. But suppression of alternative national rules is real and enforced (domestic courts must disapply the incompatible statute). Theater ratio (0.55): Moderate. The Doctrine of Parliamentary Sovereignty performs a reconciliation function: it claims that Parliament retained authority (continuing choice, revocable at will) even though the observable legal reality is EU supremacy. The doctrine is not purely false — Parliament did retain the structural capacity to repeal the 1972 Act. But the doctrine's force is increasingly performative: it asserts continuing choice without examining whether that choice was truly revocable in practice, or whether integration had created path dependencies and reliance interests that made revocation structurally difficult or economically catastrophic. The theater ratio rises over the membership period as the tension between the doctrinal claim and empirical subordination becomes more acute.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies the perspectival gap created by competing axes of classification. From Parliament's own institutional position with arbitrage exit options, the constraint is pure Rope — coordination mechanism with no asymmetric extraction, just reciprocal benefit. From the standpoint of the domestic legal order (embodied in the incompatible statute), the constraint is Snare — pure suppression with no benefit to the suppressed element. From the standpoint of integrated member-state economies constrained by regulatory dependency, the constraint is Tangled Rope — genuine coordination (single market) coupled with extraction (loss of divergent policy). From the EU institutions' perspective, the constraint is also Tangled Rope but with inverted beneficiary/victim positioning: they coordinate the harmonization and benefit from the supremacy doctrine as their institutional foundation, while suppressing national legal alternatives. The piton perspective reveals that the Doctrine of Parliamentary Sovereignty — the theoretical reconciliation of observed subordination with claimed retained authority — is increasingly performative as the doctrine's empirical fit deteriorates. The analytical mountain perspective risks naturalizing these political-constitutional choices as laws of institutional physics, treating integration as irreversible in principle. But the Brexit case falsifies this perspective: Parliament did successfully revoke membership through statute (European Union (Withdrawal) Acts), demonstrating that the pooling was empirically revocable, though at substantial economic and political cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) of each perspective reflects the agent's structural position in the extraction flow. Parliament as sovereign actor has d ≈ 0.15 (beneficiary + arbitrage exit): Parliament benefits from treaty access and carries structural capacity to exit, so experienced extractiveness is low or negative (the constraint subsidizes Parliament's position through market gains). Incompatible domestic statutes have d ≈ 0.95 (victim + trapped): the statute is completely suppressed (cannot coexist with EU law supremacy) with no exit option, so experienced extraction is maximum. Member state economies have d ≈ 0.60 (moderate victim position + constrained exit): they experience suppression and regulatory loss but also market benefits, and exit is costly but not impossible. EU institutions have d ≈ 0.35 (beneficiary with enforcement responsibility): they benefit from supremacy doctrine (foundation of their authority) but must actively enforce suppression, creating secondary costs. The piton perspective has d ≈ 0.72 (analytical/trapped in post-hoc rationalization): the doctrine is increasingly unable to reconcile its claims with empirical reality, so the institutional actor maintaining the doctrine experiences it as a performative trap. The Boltzmann scale modifier σ(S) for national scope (1.0) and continental scope (1.1) reflects that the constraint's verification difficulty and enforcement complexity scale with the coordination burden of harmonizing member-state legal systems.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this reading centers on whether 'continuing choice' and 'revocable at will' are empirically descriptive (Parliament genuinely could exit without constitutional trauma) or doctrinally performative (the doctrine asserts capacity that Parliament never actually exercises, and exit would prove costlier than the doctrine suggests). The tangled-rope classification resolves this by acknowledging both elements: there is genuine coordination function (Parliament does benefit from EU market access, treaty reciprocity, and regulatory harmonization) and real suppression (domestic rules must yield in specified domains). The constraint is neither pure coordination (Rope) nor pure extraction (Snare) but a hybrid with genuine beneficiary (Parliament, market actors) and genuine victim (incompatible domestic statutes, national legislative autonomy in certain domains). The Brexit referendum and European Union (Withdrawal) Acts provide empirical evidence that the revocability claim was not purely doctrinal: Parliament did revoke membership through statute, demonstrating that the pooling was genuinely revocable. However, the post-exit challenges to restoring full parliamentary rule-making authority (legacy EU rule entrenchment, regulatory path dependency, vested reliance interests) suggest that the revocation was revocable in law but not cost-free in fact — the constraint had created structural dependencies that made exit practically difficult even though it was legally possible. This supports the tangled-rope reading: the constraint involves both coordination (which Parliament benefits from) and extraction (cost of compliance, loss of rule divergence), and the revocability claim was empirically testable and empirically confirmed, though the test revealed that 'revocable at will' meant 'revocable through Parliament's sovereign choice' not 'costlessly revocable.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revocability_empirical_test,
    'Was the claim that membership was ''revocable at will'' a genuine structural feature (Parliament could exit and restore domestic rule-making) or a doctrinal fiction concealing actual subordination?',
    'The Brexit referendum and the European Union (Withdrawal) Acts provide the empirical test: did Parliament successfully restore its domestic rule-making authority through unilateral statutory action? Or did exit fail to undo the deep legal integration?',
    'If exit succeeded in restoring Parliament''s authority: the sovereignty-pooled reading is confirmed — Parliament retained the structural capacity to revoke. If exit revealed irreversible integration costs or persistent EU legal effects: the reading is falsified, and the sovereignty-lost reading is structurally more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revocability_empirical_test, empirical, 'Whether membership was empirically revocable or only theoretically so').

omega_variable(
    doctrine_vs_factual_authority,
    'Does the Doctrine of Parliamentary Sovereignty describe actual legal authority (Parliament genuinely retained the capacity to overrule EU law through statute) or does it describe a post-hoc rationale invented to reconcile observed EU supremacy with a constitutional tradition that claimed Parliamentary supremacy?',
    'Historical examination of parliamentary capacity to overrule EU law during membership (1973-2020): did Parliament pass statutes that successfully contradicted EU law obligations? Or was such contradiction functionally impossible despite the doctrinal claim? Post-exit analysis: has Parliament''s domestic rule-making authority been restored to its pre-1973 scope and effectiveness?',
    'If Parliament could always overrule EU law through statute: the doctrine is accurate; sovereignty was genuinely pooled, not lost. If Parliament could not overrule in practice: the doctrine is a fiction; the sovereignty-lost reading is more structurally accurate. Partial evidence (some domains more constrained than others) supports tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_factual_authority, empirical, 'Whether Parliamentary Sovereignty doctrine reflects actual capacity or post-hoc rationalization').

omega_variable(
    committer_frame_kernel_contest,
    'This story is ONE READING of a contested kernel. Which sibling reading (sovereignty-lost vs sovereignty-restored) correctly identifies the core structural feature of EU membership from Parliament''s perspective?',
    'The kernel contest cannot be resolved by the framework itself — it is a committer-axis ambiguity. Different readings foreclose or coexist depending on which doctrinal tradition the committer adopts: the reading that emphasizes Parliamentary authority (pooled sovereignty) vs the reading that emphasizes observed subordination (lost sovereignty) vs the reading that emphasizes exit capacity (restored sovereignty). Each reading is internally coherent; they differ in which structural element they privilege as foundational (retained authority vs empirical subordination vs demonstrated revocability).',
    'Adoption of the pooled-sovereignty reading commits the analyst to the view that Parliament''s choice to join was a continuing choice; adoption of the lost-sovereignty reading commits to the view that integration was irreversible in fact; adoption of the restored-sovereignty reading commits to empirical revocation through exit. These are different constitutional narratives grounded in different axioms about what ''sovereignty'' means.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_kernel_contest, conceptual, 'Kernel contest: which reading privileges the true structural feature of EU membership').

omega_variable(
    beneficiary_identity_shift_post_exit,
    'If Parliament''s reclaimed domestic authority post-Brexit proves difficult to exercise (due to legacy EU rule entrenchment, vested reliance interests, or path dependency in regulatory systems), does this retroactively prove that Parliament was never truly sovereign during membership, or does it merely demonstrate that exit carries transition costs?',
    'Comparative analysis of Parliament''s capacity to diverge from EU norms post-exit: (a) can Parliament pass contradictory statutes without triggering economic or legal dysfunction? (b) do regulatory systems restructure themselves to accommodate divergence, or do they persist in EU-aligned form through institutional inertia? (c) do vested interests lobby for continued alignment despite exit?',
    'If Parliament can easily diverge: the pooled-sovereignty reading is confirmed — authority was retained. If Parliament cannot easily diverge despite legal capacity: the doctrine''s claim of ''continuing choice'' becomes questionable — the constraint may have created path dependencies that make true revocation structurally difficult (supporting elements of the lost-sovereignty reading). Most likely: partial divergence with path-dependent sectoral variation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_shift_post_exit, empirical, 'Whether post-exit parliamentary authority is structurally recoverable or path-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_membership_and_exit__sovereignty_pooled_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eumem_tr_t0, eu_membership_and_exit__sovereignty_pooled_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eumem_tr_t10, eu_membership_and_exit__sovereignty_pooled_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(eumem_tr_t20, eu_membership_and_exit__sovereignty_pooled_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(eumem_be_t0, eu_membership_and_exit__sovereignty_pooled_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(eumem_be_t10, eu_membership_and_exit__sovereignty_pooled_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(eumem_be_t20, eu_membership_and_exit__sovereignty_pooled_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eumem_su_t0, eu_membership_and_exit__sovereignty_pooled_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(eumem_su_t10, eu_membership_and_exit__sovereignty_pooled_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(eumem_su_t20, eu_membership_and_exit__sovereignty_pooled_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_membership_and_exit__sovereignty_pooled_reading, resource_allocation).
narrative_ontology:affects_constraint(eu_membership_and_exit__sovereignty_pooled_reading, eu_membership_and_exit__sovereignty_lost_reading).
narrative_ontology:affects_constraint(eu_membership_and_exit__sovereignty_pooled_reading, eu_membership_and_exit__sovereignty_restored_reading).

% DUAL FORMULATION NOTE:
% The kernel 'eu_membership_and_exit' decomposes into three constraint stories, each a distinct reading grounded in different foundational axioms about what 'sovereignty' means and what membership revealed about parliamentary authority. The sovereignty-pooled reading (this file) emphasizes Parliament's retained capacity to choose. The sovereignty-lost reading emphasizes empirical subordination. The sovereignty-restored reading emphasizes demonstrated revocability. Each reading has its own ε value, its own beneficiary/victim structure, and its own classification. They are linked via network.affects_constraints to enable analysis of how the kernel contest structures different institutional and doctrinal positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
