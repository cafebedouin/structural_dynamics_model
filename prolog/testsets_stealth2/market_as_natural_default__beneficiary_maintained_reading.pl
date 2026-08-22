% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Naturalization as Beneficiary-Maintained Settlement
 *   domain: political economy/ideology studies/economic history
 *
 * SUMMARY:
 *   The standing arrangement under contest is the settlement by which market
 *   allocation is publicly treated as the natural default of economic life —
 *   the background against which planning, commons, and decommodified
 *   provision appear as deviations requiring special justification. This file
 *   instantiates the beneficiary_maintained_reading of that kernel: the
 *   settlement is not an inheritance but an artifact, installed mid-century
 *   and actively maintained since by an identifiable beneficiary class —
 *   finance, incumbent corporations, and the funded policy networks that
 *   speak for them — through opinion placement, funded scholarship,
 *   curriculum shaping, and institutional capture. On this reading the
 *   alternatives were suppressed, not forgotten. Per the epsilon-invariance
 *   principle, the kernel contest is NOT described inside this constraint:
 *   the lapsed_alternative_reading (passive forgetting, low agency, no victim
 *   set) and the hybrid_amnesia_reading (lapsed closure enabling later
 *   capture) are separate stories with their own epsilon values, linked
 *   through network.affects_constraints. This file authors one clean,
 *   epsilon-invariant constraint: the maintained settlement as this reading
 *   sees it. The claim/metric gap is deliberate where present: claimed_type
 *   is authored from structure (a real residual coordination function plus
 *   asymmetric, enforced advantage), while the metrics are authored from the
 *   arrangement's observed operation — the engine measures any divergence.
 *
 * KEY AGENTS:
 *   - market_fundamentalist_policy_networks: Primary agenda-setter (powerful/mobile) — operates the reproduction machinery: think tanks, opinion placement, advisory channels
 *   - finance_sector: Concentrated beneficiary (institutional/arbitrage) — funds the apparatus and collects its most direct returns
 *   - incumbent_corporations: Secondary beneficiary (institutional/arbitrage) — converts the frame into defensive cover for market position
 *   - mainstream_economics_profession: Identity-locked beneficiary (institutional/identity_locked) — supplies the theoretical warrant; disciplinary self-conception is bound to the frame's neutrality
 *   - organized_labor: Primary target (organized/constrained) — bargains inside a frame that pre-classifies its power as distortion
 *   - wage_earning_households: Diffuse target (powerless/trapped) — absorbs the frame's consequences and its vocabulary as common sense
 *   - heterodox_economic_scholars: Documenting target (moderate/constrained) — produces the construction record from marginalized positions
 *   - economic_democracy_advocates: Excluded proposer (organized/constrained) — proposals enter debate pre-refuted by the frame
 *   - post_growth_and_commons_movements: Excluded challenger (organized/constrained) — kept outside the venues where the frame is reproduced
 *   - economic_historians_of_naturalization: Analytical observer (analytical/analytical) — traces the apparatus against archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.48).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization as Beneficiary-Maintained Settlement").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political economy/ideology studies/economic history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, 'c4ddcbae-57d5-4584-ae6f-d0b366fe65fa').
narrative_ontology:cs_kernel_codification('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', implicit).
narrative_ontology:cs_authority_grounding('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', extraction).
narrative_ontology:cs_interpretation_layer_present('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa').
narrative_ontology:cs_reading_relation('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', foundational, naturalization_is_agentive_defense).
narrative_ontology:cs_axiom_status(naturalization_is_agentive_defense, holdable).
narrative_ontology:cs_axiom_grounding('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', naturalization_is_agentive_defense, empirically_contingent).
narrative_ontology:cs_axiom('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', foundational, alternatives_are_suppressed_not_forgotten).
narrative_ontology:cs_axiom_status(alternatives_are_suppressed_not_forgotten, holdable).
narrative_ontology:cs_axiom_grounding('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', alternatives_are_suppressed_not_forgotten, empirically_contingent).
narrative_ontology:cs_reference_frame('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', engineered_naturalization_settlement).
narrative_ontology:cs_drift_state('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c4ddcbae-57d5-4584-ae6f-d0b366fe65fa', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, finance_sector).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, incumbent_corporations).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, market_fundamentalist_policy_networks).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, mainstream_economics_profession).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, organized_labor).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, wage_earning_households).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, heterodox_economic_scholars).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, economic_democracy_advocates).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, spontaneous_order_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, market_inevitability_thesis).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, exchange_neutrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the interlocking set of funded think tanks, opinion-placement operations, and legislative advisory channels that reproduce the frame in which market allocation is the natural order of economic life. Write the op-eds, staff the commissions, supply the expert testimony, and set which economic questions count as settled. Staff circulate freely among network nodes, government posts, and corporate boards; if the frame lost value, the personnel and infrastructure could pivot to adjacent message markets.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, market_fundamentalist_policy_networks, agenda_setter,
    powerful, generational, mobile, global).

% Funds the policy networks and university programs that maintain the frame, and collects the most direct returns: deregulated positioning, crisis backstops presented as technical necessities, and the presumption that capital mobility is a fact of nature rather than a policy choice. Portfolio capital relocates across jurisdictions, which gives the sector leverage over any single regulator and makes its commitment to any one country's frame revocable.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, finance_sector, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, finance_sector, agenda_setter).

% Draw on the frame whenever their market positions are challenged: concentration reads as efficiency, labor demands read as distortions, subsidies read as competitiveness policy. They contribute to the maintenance apparatus through associations and sponsorships and receive diffuse protective cover in return. Production and registration can shift across borders, though less fluidly than portfolio capital.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, incumbent_corporations, beneficiary,
    institutional, biographical, arbitrage, global).

% Supplies the formal models in which market equilibria are the benchmark and intervention the deviation. Departmental rankings, journal hierarchies, and hiring pipelines all presume the benchmark. A career built on the benchmark does not easily survive declaring the benchmark a constructed choice; the discipline's self-understanding as the science of a discovered order is bound up with the frame's neutrality, so adherence is maintained as much from inside as from outside.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, mainstream_economics_profession, beneficiary,
    institutional, generational, identity_locked, global).

% Bargains inside a frame that treats union power as interference with a natural order. Every gain must be justified as an exception; every loss is absorbed as equilibrium. Relocating to a jurisdiction with a friendlier frame means abandoning members, industries, and communities, so the practical response is sustained contest inside the frame rather than exit from it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, organized_labor, payer,
    organized, biographical, constrained, national).

% Live inside the frame's consequences — eroded bargaining power, privatized risk, public goods presented as unaffordable luxuries — while absorbing its vocabulary as common sense. There is no household-level exit from the economic order the frame describes; opting out means poverty, so the frame's account of what is possible functions as the boundary of the household's imaginable future.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, wage_earning_households, payer,
    powerless, immediate, trapped, national).

% Produce the documentation that the frame was built and is maintained — histories of thought, funding studies, comparative analyses of non-market arrangements — from positions the journal hierarchy and the hiring market marginalize. Switching to the mainstream paradigm would end the marginalization and the research program together, which is why the exit exists formally and is rarely taken.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_economic_scholars, payer,
    moderate, generational, constrained, global).

% Propose worker ownership, participatory budgeting, and public banking — arrangements the frame pre-classifies as unnatural before argument begins. They rarely appear in the venues where the frame is reproduced; their proposals enter public debate already carrying the burden of proving they are not category errors.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, economic_democracy_advocates, excluded,
    organized, generational, constrained, national).

% Challenge the frame's growth commitment directly and are received as moral sentiment rather than economic analysis. Their exclusion from expert venues is the clearest demonstration of which questions the frame keeps off the table; their organizing continues at the margins the apparatus does not bother to police.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, post_growth_and_commons_movements, excluded,
    organized, generational, constrained, global).

% Trace the apparatus's construction — the memoranda, the funding flows, the personnel circuits — and publish where the other seats' accounts can be checked against archives. Neither funded by the networks nor organized to bargain with them, they hold the only seat from which the whole structure is visible at once.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, economic_historians_of_naturalization, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A society-scale economy needs a shared default framework for the allocation decisions that are never deliberated case-by-case; the naturalized-market frame provides a common ontology — prices, incentives, efficiency — that lets millions of actors coordinate expectations without renegotiating first principles each time. Stated without evaluation: whatever else it does, the frame solves this expectation-coordination problem.
% TRANSFER_FUNCTION: Moves legitimacy and agenda-setting discretion from democratic publics and labor toward financed policy networks and incumbent capital: distributional outcomes are converted from contestable political choices into background facts, and attention, funding, and career opportunity flow toward institutions that reproduce the frame.
% ABSENT_VOICES: Economic democracy advocates, post-growth and commons movements, and heterodox traditions outside the funded circuit would object that the default was chosen and can be re-chosen. They are absent from the op-ed pages, the funded conference circuit, and the legislative hearing rooms where the frame is reproduced; their absence is not incidental but is what the enforcement machinery sustains.
% DISAPPEARANCE_RATIONALE: Overnight removal would expose allocation as chosen: tax, property, labor, and monetary arrangements would face open re-legitimation contests; the networks' clients would lose their primary defensive asset; heterodox and democratic proposals would enter debate without pre-refutation. The beneficiary seats would immediately begin paying to rebuild the frame — which is the point: the arrangement is maintained, not merely inherited, and its disappearance would trigger visible, expensive reconstruction.
% FOUNDING_PROBLEM: Mid-century states needed a durable public account of why market allocation, rather than planning, would organize production and distribution — first against wartime command economies, then against the Soviet rival. The naturalization supplied it: if markets are simply what economies are, the choice between systems is not a choice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: economic historians and political scientists document the apparatus's construction and its post-1989 pivot from rival-defense to regulation-defense (funding-flow studies of policy networks, archival work on the diffusion of the Powell memorandum, Polanyian scholarship on the political origins of market society); former network insiders have described the shift in memoir and testimony. The beneficiary seats dispute the dead-status finding, citing new rivals — that dispute is recorded here, not resolved; the corroborating sources stand outside the benefiting parties.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.48, inside the reading's expected band: the settlement transfers real, recurring advantage to a concentrated class while retaining a genuine residual coordination function (a shared economic default does solve a society-scale expectation problem), which caps it below snare territory. Suppression is authored at 0.62 as a raw structural property — discursive and institutional coercion (funding asymmetry, gatekeeping, pre-refutation), unscaled by power or scope; only extractiveness is scaled downstream. Theater_ratio at 0.31 reflects a maintenance operation that is mostly functional for its purpose, with a growing ritual share after the founding rival collapsed (triumphalist indices, prize circuits, anniversary rhetoric) that was partially pruned when post-2008 embarrassment made overt theater costly — hence the t40 peak and t50 dip. Accessibility_collapse at 0.45: alternatives remain visible and locally workable (cooperatives, Nordic arrangements, commons experiments) but collapse as serious options in mainstream venues once the frame is applied. Resistance at 0.55: sustained movements (labor, heterodox economics, post-growth, economic democracy) actively contest the frame rather than merely suffering it. The three temporal series run on one shared grid (t=0,10,20,30,40,50) with every metric authored at every point; the small base_extractiveness dip at t40 records the 2008 strain, when crisis management temporarily exposed the frame's constructedness faster than the apparatus could re-cover it. Claimed_type (tangled_rope) and metrics were authored independently, from structure and from observation respectively.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently, and so should two beneficiaries who look alike. From the policy networks' position the settlement is a functioning information environment they competently run; from organized labor's position it is a rigged tribunal where every claim must be argued as an exception. More subtly, finance_sector and mainstream_economics_profession both sit on the benefiting side, but with opposite exit profiles: finance holds arbitrage-grade exit (capital relocates across jurisdictions, so the sector can abandon any single frame-hostile regime), while the profession is identity-locked — a career built on the market-as-benchmark cannot easily survive declaring the benchmark a constructed choice, so the profession defends the frame more fiercely than its direct material stake alone would predict. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for finance_sector (lowest — arbitrage exit puts it nearest the beneficiary pole), incumbent_corporations, and the policy networks; the profession derives low d from its beneficiary position but its identity_locked exit keeps it from the arbitrage pole. Victim declarations drive high directionality for organized_labor (slightly below full-target: organized capacity blunts but does not remove exposure), wage_earning_households (nearest the full-target pole: trapped, no household-level exit from the economic order the frame describes), and heterodox_economic_scholars (high, tempered by partial insider access to academic channels). The excluded seats sit outside the derivation proper — their exclusion is the enforcement object — and are recorded as structural absences rather than tuned directions. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the intended differentiation without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — supplying a durable public account of why market allocation rather than planning would organize economic life, first against wartime command economies and then against the Soviet rival — substantially died with that rival. The apparatus did not die with it; on the available record it grew. The R5 interview records founding_problem_status=dead against disappearance_verdict=world_rearranges, which is precisely the mismatch the consumer reads as a capture/zombie flag, cross-checked against the theater path. The tangled_rope classification is what prevents mislabeling in both directions: a pure-rope reading would credit the settlement's genuine residual coordination function while ignoring the engineered asymmetry and the suppressed alternatives; a pure-snare reading would deny the real default-coordination problem any large society must solve somehow. The piton test fails cleanly: a piton has no concentrated beneficiary able to maintain it, whereas here the administrators (the policy networks) are paid to maintain the arrangement and their principal client (finance) demonstrably profits from its outputs — cost-asymmetry does not obtain, capture does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel market_as_natural_default — the beneficiary_maintained_reading. Would instantiating either sibling reading instead produce a different constraint with a different epsilon and a different victim set?',
    'Generate the sibling files (lapsed_alternative_reading, hybrid_amnesia_reading) and compare compiled classifications. The disagreement is located at agency attribution — active engineered defense versus passive historical forgetting — which determines whether identifiable victims exist and where epsilon sits.',
    'Under lapsed_alternative_reading, epsilon falls toward rope territory (drift, no active suppressor, diffuse harm, no victim set). Under hybrid_amnesia_reading, epsilon concentrates in the post-lapse capture phase rather than across the whole interval. This file''s epsilon (0.48) is valid only for the beneficiary-maintained reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one of three readings of the market-as-natural-default kernel; siblings are separate constraints.').

omega_variable(
    natural_substrate_residual,
    'How much of the frame''s stability rests on genuine human exchange propensities and coordination economies, as opposed to engineered maintenance?',
    'Comparative economic anthropology and the historical record of non-market allocation systems; measure frame adherence in populations with minimal exposure to the maintenance apparatus.',
    'A large natural substrate pushes effective extraction down toward coordination cost (rope-side pressure); a negligible substrate supports the full engineered-closure account (snare-side pressure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_substrate_residual, empirical, 'Residual naturality beneath the engineered frame.').

omega_variable(
    counterfactual_discourse_baseline,
    'What share of current market-default acceptance would persist if the maintenance apparatus were removed — that is, how much of the measured suppression reflects manufactured consent versus settled preference?',
    'Cross-jurisdiction comparison of economic discourse where apparatus funding differs sharply (Nordic versus Anglophone media-academic ecosystems), holding material conditions roughly constant.',
    'Calibrates the structural share of the suppression scalar; a high persistent-acceptance share would downgrade suppression and soften the reading toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_discourse_baseline, empirical, 'Counterfactual acceptance absent the maintenance apparatus.').

omega_variable(
    disciplinary_lock_mechanism,
    'Is the mainstream economics profession''s adherence to the frame structural (journal gates, hiring pipelines, funding dependence) or internalized (trained intuitions constituting professional identity)?',
    'Track frame revision and heterodox engagement among economists who gain access to pluralist departments, open-access venues, or funding sources outside the apparatus.',
    'If internalized, suppression persists after structural barriers fall and the arrangement outlives its enforcement machinery; if structural, funding and gate reform alone unwinds much of the lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disciplinary_lock_mechanism, empirical, 'Structural versus internalized mechanism of professional adherence.').

omega_variable(
    post_cold_war_persistence_test,
    'Did the apparatus contract when its founding rival collapsed, or did it grow — is the founding problem dead while the arrangement persists?',
    'Budget, staffing, and output series for the major policy networks across 1985-2000, checked against the disappearance of the systemic-planning rival.',
    'Growth after 1991 confirms a dead founding problem with a living arrangement — the capture signature this reading predicts; contraction would support a live-problem reading and weaken the mandatrophy finding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_cold_war_persistence_test, empirical, 'Dead-problem test via post-1991 apparatus trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t10, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(mark_tr_t10, observed).
narrative_ontology:measurement(mark_tr_t20, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(mark_tr_t20, observed).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(mark_tr_t30, observed).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement_basis(mark_tr_t40, observed).
narrative_ontology:measurement(mark_tr_t50, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(mark_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t10, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(mark_be_t10, observed).
narrative_ontology:measurement(mark_be_t20, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement_basis(mark_be_t20, observed).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(mark_be_t30, observed).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement_basis(mark_be_t40, observed).
narrative_ontology:measurement(mark_be_t50, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement_basis(mark_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(mark_su_t0, observed).
narrative_ontology:measurement(mark_su_t10, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement_basis(mark_su_t10, observed).
narrative_ontology:measurement(mark_su_t20, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(mark_su_t20, observed).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(mark_su_t30, observed).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(mark_su_t40, observed).
narrative_ontology:measurement(mark_su_t50, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(mark_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'market naturalization' per the epsilon-invariance principle. The label covers three structurally distinct claims: (1) this file — the settlement is actively maintained by identifiable beneficiaries (epsilon 0.48, victim set present, enforcement machinery central); (2) lapsed_alternative_reading — dominance results from passive historical forgetting (expected low epsilon, no active suppressor, no victim set); (3) hybrid_amnesia_reading — lapsed closure enables later capture (epsilon concentrated in the capture phase). Each member gets its own epsilon, beneficiaries, victims, and classification; all three link through affects_constraints. Upstream/downstream structure: this reading's mechanism is presupposed by the hybrid reading's second stage, so evidentiary findings here propagate to it; the lapsed reading stands as an independent competitor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
