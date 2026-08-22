% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Peer-War Exclusion — Structural Contraction Reading
 *   domain: strategic studies / international relations
 *
 * SUMMARY:
 *   Since the late 1950s the nuclear-armed peer states have possessed weapons
 *   whose use in war against each other guarantees the attacker's own
 *   annihilation: any full-scale exchange destroys both societies, so victory
 *   — the objective organized war exists to pursue — has no remaining path.
 *   This story instantiates the structural_contraction_reading of that fact:
 *   peer war has not become expensive or improbable but unreachable, removed
 *   from the set of actions available to rational statecraft. The standing
 *   arrangement assessed here is the post-1945 order built inside that
 *   exclusion: arsenals maintained permanently to keep the guarantee intact,
 *   alliance blocs organized around extended forms of it, and four decades of
 *   great-power rivalry discharged through peripheral wars rather than direct
 *   collision. The reading treats the exclusion itself as a physical limit;
 *   the structural data below records who is kept safe by it, who pays for
 *   keeping it, and where the displaced rivalry landed. KEY AGENTS (by
 *   structural relationship): - nuclear_great_power_homelands: Primary
 *   beneficiary (institutional/constrained) — receives the war-absence as a
 *   standing condition; funds its maintenance -
 *   nuclear_defense_establishments: Concentrated beneficiary
 *   (institutional/identity_locked) — operates the survivability machinery
 *   and collects the budgets it justifies - proxy_war_zone_populations:
 *   Primary target (powerless/trapped) — bore the ground combat the
 *   principals avoided - nuclear_state_taxpayers: Secondary target
 *   (moderate/constrained) — funded the apparatus across the whole interval -
 *   extended_deterrence_allies: Dual-positioned beneficiary-payer
 *   (organized/constrained) — protected without arming, constrained in
 *   alignment - frontline_host_states: Target-hosts (moderate/constrained) —
 *   carried forward-based weapons and targeting plans - arms_control_regimes:
 *   Administrative observer (institutional/analytical) -
 *   strategic_studies_community: Analytical observer (analytical/analytical)
 *   - abolitionist_movements: Excluded objectors (organized/constrained)
 *
 * KEY AGENTS:
 *   - - nuclear_great_power_homelands: Primary beneficiary (institutional/constrained) — the war-exclusion's protected cores; pay for its upkeep
 *   - - nuclear_defense_establishments: Concentrated beneficiary (institutional/identity_locked) — collect budgets and mission justification from the standoff's continuation
 *   - - proxy_war_zone_populations: Primary target (powerless/trapped) — Korea, Vietnam, Afghanistan, Angola and the rest of the displaced battlefield
 *   - - nuclear_state_taxpayers: Secondary target (moderate/constrained) — tens of trillions in cumulative arsenal spending
 *   - - extended_deterrence_allies: Dual-positioned beneficiary-payer (organized/constrained) — protection exchanged for basing, alignment, and target status
 *   - - frontline_host_states: Target-hosts (moderate/constrained) — divided Germany and Korea, Cuba, Turkey; compulsory alignment, probable battlefield
 *   - - arms_control_regimes: Administrative observer (institutional/analytical) — NPT/SALT/START/INF verification machinery
 *   - - strategic_studies_community: Analytical observer (analytical/analytical) — the epistemic profession organized around the framework
 *   - - abolitionist_movements: Excluded objectors (organized/constrained) — test-ban, freeze, and humanitarian-consequences campaigns without deployment-decision seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.38).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.28).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Peer-War Exclusion — Structural Contraction Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic studies / international relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__structural_contraction_reading).
domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, 'cfcae8b5-d3a5-470e-b7a9-b201eb788641').
narrative_ontology:cs_kernel_codification('cfcae8b5-d3a5-470e-b7a9-b201eb788641', distributed).
narrative_ontology:cs_authority_grounding('cfcae8b5-d3a5-470e-b7a9-b201eb788641', expertise).
narrative_ontology:cs_interpretation_layer_present('cfcae8b5-d3a5-470e-b7a9-b201eb788641').
narrative_ontology:cs_reading_relation('cfcae8b5-d3a5-470e-b7a9-b201eb788641', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_reading_relation('cfcae8b5-d3a5-470e-b7a9-b201eb788641', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('cfcae8b5-d3a5-470e-b7a9-b201eb788641', foundational, victory_path_physically_empty).
narrative_ontology:cs_axiom_status(victory_path_physically_empty, holdable).
narrative_ontology:cs_axiom_grounding('cfcae8b5-d3a5-470e-b7a9-b201eb788641', victory_path_physically_empty, empirically_contingent).
narrative_ontology:cs_axiom('cfcae8b5-d3a5-470e-b7a9-b201eb788641', secondary, proxy_conflict_is_substitution_not_continuation).
narrative_ontology:cs_axiom_status(proxy_conflict_is_substitution_not_continuation, holdable).
narrative_ontology:cs_axiom_grounding('cfcae8b5-d3a5-470e-b7a9-b201eb788641', proxy_conflict_is_substitution_not_continuation, empirically_contingent).
narrative_ontology:cs_reference_frame('cfcae8b5-d3a5-470e-b7a9-b201eb788641', war_absent_from_reachable_set).
narrative_ontology:cs_drift_state('cfcae8b5-d3a5-470e-b7a9-b201eb788641', contemporary_multipolar_rearmament, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('cfcae8b5-d3a5-470e-b7a9-b201eb788641', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_great_power_homelands).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_defense_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, extended_deterrence_allies).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, proxy_war_zone_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_state_taxpayers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, frontline_host_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_state_taxpayers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_great_power_homelands).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, extended_deterrence_allies).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, mutual_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, assured_second_strike_sufficiency).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, stability_instability_paradox).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The societies of the nuclear-armed peer states. Since the late 1950s each has lived under the condition that a full war with the other ends both, and none has fought the other directly since the arrangement formed. What flows to them is the absence of peer war and the mobilization it would demand; what flows from them is the funding, basing, and political cover that keep their arsenals survivable. Leaving unilaterally means disarming into a world where rivals' arsenals remain — an exit available in principle, exercised never.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_great_power_homelands, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_great_power_homelands, payer).

% The military services, weapons laboratories, and industrial contractors that operate and modernize the arsenals. They run the submarines, silos, early-warning radars, and command systems whose survivability is what makes the mutual-destruction condition hold. Their budgets, missions, and institutional purpose attach to the standoff's continuation; after each rivalry downturn they have re-described their tasks in deterrence terms to keep both funding and function. Exit would mean dismantling the institutions themselves.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_defense_establishments, beneficiary,
    institutional, biographical, identity_locked, global).

% The peoples of Korea, Vietnam, Laos, Cambodia, Afghanistan, Angola, and other territories where the patrons' rivalry was fought out through local armies and insurgencies from 1950 onward. They hosted the ground combat the principals avoided, absorbing casualties, displacement, and unexploded ordnance at scales the principals' homelands never approached. Their exit was flight, and flight was often blocked by the front lines the patron competition drew through their societies.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, proxy_war_zone_populations, payer,
    powerless, biographical, trapped, regional).

% Citizens of the arsenal states, who funded warhead production, delivery systems, and alert operations across the whole period — cumulative spending measured in the tens of trillions of inflation-adjusted dollars. They receive the war-absence their payments help maintain and have rarely been offered a direct vote on the trade; anti-nuclear majorities at various points translated into policy only at the margins.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_state_taxpayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_state_taxpayers, beneficiary).

% Non-armed allies under the patrons' security guarantees — Western Europe, Japan, South Korea. They receive protection they do not provide for themselves, and pay in basing rights, alignment discipline, and acceptance of target status on their territory; several explored independent arsenals and were pressed back under the umbrella. Exit means self-provision of security against neighbors armed with the weapons the umbrella offsets.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, extended_deterrence_allies, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, extended_deterrence_allies, payer).

% States positioned on the confrontation lines — divided Germany and Korea, Cuba, Turkey — whose territory carried forward-based weapons and whose cities sat on targeting plans. They gained patron protection and lost autonomous options: alignment was compulsory in practice, neutrality was treated as defection, and crisis periods converted them into the likely battlefield.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, frontline_host_states, payer,
    moderate, biographical, constrained, regional).

% The treaty and verification machinery — NPT, SALT, START, INF, CTBT processes — that measures, limits, and audits the arsenals. It administers the standoff's rules without setting them; its leverage rises and falls with great-power consent, and its recent erosion (INF collapse, Open Skies exit, START suspension) has narrowed what it can see.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, arms_control_regimes, observer,
    institutional, generational, analytical, global).

% The academic and governmental analysts who theorize deterrence, stability, and escalation — the profession whose concepts (assured destruction, escalation ladders, stability criteria) organize how the standoff is discussed. Careers, curricula, and journal agendas attach to the framework's centrality; the community critiques particular doctrines while presupposing the framework that makes them thinkable.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, strategic_studies_community, observer,
    analytical, biographical, analytical, global).

% Transnational campaigns and humanitarian initiatives pressing for the arsenals' elimination — the test-ban and freeze movements of the Cold War, and the humanitarian-consequences campaign that produced the 2017 prohibition treaty. They object to the arrangement's permanence and its risks; they possess moral authority and treaty text but no seat where deployment decisions are made, and the arsenal states reject the prohibition treaty outright.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, abolitionist_movements, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_defense_establishments).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__structural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes total war between nuclear-armed peer states from the set of actions available to statecraft, converting the recurring historical pattern of hegemonic great-power wars into a managed standoff in which the principals' homelands are mutually off-limits.
% TRANSFER_FUNCTION: Moves mortality risk and fiscal resources along three channels: war-risk is lifted off the principals' homelands and deposited on proxy-zone populations; tax revenue moves from arsenal-state publics to the defense establishments that operate the survivability machinery; and foreign-policy autonomy moves from allies and frontline states to the patrons whose alignment they accept.
% ABSENT_VOICES: Proxy-zone populations had no seat when the exclusion architecture was designed — the spheres-of-interest settlements that routed the rivalry to their territories were drawn at Yalta and Potsdam without them. Future generations bear the accident and legacy risk voicelessly. Decolonizing societies absorbed the displaced competition without consenting to it. The abolitionist movements hold the objection seat today and hold no deployment-decision power; their exclusion is the arrangement's design working as built.
% DISAPPEARANCE_RATIONALE: If the exclusion vanished overnight — arsenals dismantled or the guarantee broken — the war option would reopen between rearmed peers: alliance architectures built around extended deterrence would rebalance or dissolve, the establishments' budgets and missions would lose their object, patron subsidy channels to periphery conflicts would close, and every state's war planning would rewrite itself around the restored possibility. Arrangements across the entire international system depend on the exclusion holding.
% FOUNDING_PROBLEM: Industrial great-power war had already become self-destroying at scale before 1945 — the World Wars consumed the belligerents' treasuries and generations — and nuclear weapons made the pre-existing problem terminal: a full peer war now ends both societies. The arrangement's founding problem is how peer great powers coexist indefinitely without the periodic total war that their industrial and then nuclear capability made suicidal.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the Pugwash scientific community and neutral-state commissions (the Palme Commission on Common Security), whose analyses attest the founding problem as live; by declassified near-miss records (Arkhipov 1962, Petrov 1983, Able Archer 1983, Norwegian rocket 1995) showing the problem persisting independent of establishment testimony; and by hibakusha testimony on what the capability does. The arsenal establishments also attest liveness, but they sit inside the beneficiary set and are discounted accordingly.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.38: the exclusion itself takes nothing — physics levies no fee — but the arrangement around it runs two real transfer streams: fiscal capture (arsenal budgets flowing to the establishments that operate the survivability machinery, cumulatively the largest peacetime expenditure category in the arsenal states) and mortality displacement (the rivalry's ground combat routed through Korea, Vietnam, Laos, Cambodia, Afghanistan, Angola and other peripheries, killing in the millions while the principals' homelands stayed untouched). Against this stands the arrangement's protective output, which dwarfs its costs — but extraction measures the transfer imposed, not the net balance. Suppression 0.28: physics coerces no one, and the raw structural suppression is modest — secrecy regimes, alliance discipline, the political marginalization of abolitionist and counterforce-dissenting voices, and the vulnerability penalty on unilateral exit. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. Theater ratio 0.31: the operational core (boomer patrols, early warning, command continuity) is functional, wrapped in a performative layer — duck-and-cover drills, shelter crazes, parade-scale displays, Star Wars rhetoric, modernization announcements — that peaks in crisis-scare phases. Accessibility collapse 0.72: once the physics is understood, the seek-victory alternative collapses almost completely — no serious planner pursues it — but arrangement-level alternatives (minimum deterrence, no-first-use, negotiated disarmament) remain visible and argued-for, politically unreachable rather than unthinkable, which is why this sits below mountain-grade collapse. Resistance 0.22: abolitionist campaigns, the French force de frappe as a partial exit attempt, counterforce revisionism inside the establishments — real but marginal. Claim/metric independence: the claimed mountain is this reading's sincere structural commitment (the impossibility is asserted as physical, categorical, M-set-contracting); the metrics describe the arrangement's actual operation including its rent surfaces; the divergence, if the engine finds one, is the datum. Identity-lock dynamics: the establishments exhibit institutional identity fusion — after each rivalry downturn (1991 most sharply) they re-described their tasks in deterrence terms rather than shrinking, the organization having become its function; breaking that frame would convert them from beneficiaries into a redundant bureaucracy facing dissolution. Coalition note: the proxy-zone victims are dispersed across rival blocs' client states, frequently armed against each other by the patrons — their coalition power has historically been nil, which is why powerless-seat extraction persisted for four decades. Cyclical pattern: the series shows two crisis-driven humps (1962, 1983) on a build-crisis-manage-relax-accumulate cycle; each scare (missile gap, Able Archer) functioned as intermittent reinforcement for appropriations, so the oscillation is partly the extraction mechanism itself, not noise. Base properties are measured on the rising limb of the current cycle (2026): renewed multipolar rearmament, treaty collapse, and a large-scale proxy war in Europe.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from identical structural data. From the homeland seat the exclusion is experienced as free physics — nothing enforced, everything received, a mountain-shaped world. From the establishment seat the same structure is a mission-justifying rent engine whose budgets attach to the standoff's continuation. From the proxy-zone seat the identical arrangement is the machine that routed the war to them: the center's peace was purchased with their casualties, and they never sat where that routing was decided. From the taxpayer seat it is a bill never put to a vote; from the ally seat, protection with an autonomy invoice attached. The engine computes these per-seat classifications from the declared roles, power atoms, and exit options; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The homelands are declared beneficiaries with constrained exit: derivation places them near the beneficiary end, and rightly so — they receive the war-absence while paying maintenance, a net subsidy position despite the dual payer undertow recorded in their secondary role. The establishments are the deepest beneficiaries: identity_locked exit means the arrangement's continuation is their institutional existence, so their derived directionality sits nearest the subsidy pole and their capture of the fiscal stream is the concentrated receipt surface (recorded in gain_flow). Extended-deterrence allies are beneficiaries with a payer undertow (autonomy, basing, target status) — secondary_role marks the dual position rather than an override, because the role-plus-exit derivation already lands them on the beneficiary side. The proxy-zone populations are powerless and trapped: derivation drives them to the full-target end, where effective extraction is maximally amplified — the displacement stream lands on the seats least able to refuse it. Taxpayers sit mid-range (they fund the apparatus and receive the protection it provides); frontline host states sit elevated (compulsory alignment, probable-battlefield status, no neutrality exit). Observers carry analytical atoms and sit outside the transfer arithmetic. No directionality overrides are used: for every seat, the declared role plus power plus exit produces the correct qualitative position, and the residual dualities are carried by secondary roles rather than by overriding the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — industrial peer war turned terminally destructive — is live for as long as peer rivals hold survivable arsenals, so no resolved-mandatrophy flag is warranted; the 1991 trough shows the mandate flexing with rivalry intensity rather than dying. The classification risk this story guards against runs in both directions. Read as pure mountain, the arrangement's displacement economy gets laundered as physics: if war is simply impossible, then whatever happens at the periphery is just war happening elsewhere, and the establishments' budgets become the price of gravity. The beneficiary and victim declarations prevent that laundering by forcing the fiscal capture and the mortality displacement into the ledger alongside the genuine protective function. Read as pure snare, the annihilation-prevention function — the largest collective-action good in the record — gets dismissed as cover story, which the casualty counterexamples refute. The honest structure holds both: a real coordination function of civilizational scale, operating through machinery that concentrates receipts and displaces costs. Whether the engine certifies the reading's mountain claim or reclassifies through the false-summit chain is exactly the measurement this story is built to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_engineered_standoff,
    'Is the impossibility a physical invariant of the nuclear-armed world, or a maintained construction contingent on engineered second-strike survivability and continuing political choices?',
    'Engineering-margin analysis of second-strike survivability under counterforce and missile-defense breakthroughs; archival study of how often the guarantee depended on individual luck (Arkhipov 1962, Petrov 1983, Able Archer) rather than physics.',
    'If the guarantee is engineered and luck-dependent, the mountain claim fails false-summit evaluation and the arrangement reclassifies toward the coordination-plus-extraction family, with the maintaining establishments as agenda setters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_engineered_standoff, empirical, 'Whether the war-exclusion is natural law or a maintained artifact.').

omega_variable(
    displacement_causality,
    'Are peripheral proxy wars caused by the war-exclusion (violence displaced from a closed center) or would equivalent periphery conflicts have occurred without it?',
    'Comparative conflict-frequency analysis across the 1945 boundary controlling for decolonization; quasi-experiments where nuclear symmetry lapsed or was absent between otherwise comparable rivals.',
    'If displacement is causal, proxy-zone mortality belongs in the arrangement''s transfer ledger, raising effective extraction and supporting a coordination-plus-extraction classification; if not, extraction falls toward pure fiscal capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_causality, empirical, 'Whether proxy wars are substitution downstream of the exclusion.').

omega_variable(
    reading_seat_epsilon_divergence,
    'This epsilon is authored from the structural_contraction seat over the standing post-1945 arrangement; what epsilon would the sibling seats author over the same referent, and does the kernel''s classification depend on which seat measures?',
    'Generate the sibling-reading stories (rational_dropout_reading, credibility_paradox_reading) over the identical referent and compare authored epsilon, victim sets, and computed types.',
    'If the dropout seat authors materially lower epsilon (an option priced out rather than an option erased), the kernel''s classification is seat-relative and the contraction reading''s categorical claim is doing classification work; identical profiles would mean the readings are rhetorically distinct but structurally equivalent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_seat_epsilon_divergence, conceptual, 'Committer-frame omega: epsilon is reading-indexed; sibling readings may classify the same arrangement differently.').

omega_variable(
    accident_risk_accounting,
    'Does the arrangement''s recurrent near-accident stream (Cuban proximity 1962, Petrov 1983, Able Archer 1983, Norwegian rocket 1995) constitute a material expected loss borne by all seats, and how should it be priced?',
    'Declassified incident archives combined with probabilistic risk models of near-miss frequency and escalation pathways.',
    'Material accident risk adds extraction borne even by beneficiary seats, compressing the directionality spread and potentially shifting classification away from the reading''s pure-limit claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accident_risk_accounting, empirical, 'Whether crisis-luck losses belong in the extraction ledger.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.06).
narrative_ontology:measurement_basis(nucl_tr_t1945, observed).
narrative_ontology:measurement(nucl_tr_t1955, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1955, 0.18).
narrative_ontology:measurement_basis(nucl_tr_t1955, observed).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1962, 0.29).
narrative_ontology:measurement_basis(nucl_tr_t1962, observed).
narrative_ontology:measurement(nucl_tr_t1972, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1972, 0.23).
narrative_ontology:measurement_basis(nucl_tr_t1972, observed).
narrative_ontology:measurement(nucl_tr_t1983, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1983, 0.4).
narrative_ontology:measurement_basis(nucl_tr_t1983, observed).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1991, 0.19).
narrative_ontology:measurement_basis(nucl_tr_t1991, observed).
narrative_ontology:measurement(nucl_tr_t2026, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2026, 0.31).
narrative_ontology:measurement_basis(nucl_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement_basis(nucl_be_t1945, observed).
narrative_ontology:measurement(nucl_be_t1955, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1955, 0.24).
narrative_ontology:measurement_basis(nucl_be_t1955, observed).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.45).
narrative_ontology:measurement_basis(nucl_be_t1962, observed).
narrative_ontology:measurement(nucl_be_t1972, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1972, 0.37).
narrative_ontology:measurement_basis(nucl_be_t1972, observed).
narrative_ontology:measurement(nucl_be_t1983, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1983, 0.46).
narrative_ontology:measurement_basis(nucl_be_t1983, observed).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1991, 0.25).
narrative_ontology:measurement_basis(nucl_be_t1991, observed).
narrative_ontology:measurement(nucl_be_t2026, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(nucl_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1945, 0.04).
narrative_ontology:measurement_basis(nucl_su_t1945, observed).
narrative_ontology:measurement(nucl_su_t1955, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1955, 0.17).
narrative_ontology:measurement_basis(nucl_su_t1955, observed).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1962, 0.33).
narrative_ontology:measurement_basis(nucl_su_t1962, observed).
narrative_ontology:measurement(nucl_su_t1972, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1972, 0.27).
narrative_ontology:measurement_basis(nucl_su_t1972, observed).
narrative_ontology:measurement(nucl_su_t1983, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1983, 0.37).
narrative_ontology:measurement_basis(nucl_su_t1983, observed).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1991, 0.15).
narrative_ontology:measurement_basis(nucl_su_t1991, observed).
narrative_ontology:measurement(nucl_su_t2026, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2026, 0.28).
narrative_ontology:measurement_basis(nucl_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the nuclear revolution' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story constraint family: structural_contraction_reading (this file — war exits the reachable set; categorical impossibility), rational_dropout_reading (victory remains structurally possible but cost-dominated), and credibility_paradox_reading (deterrence rests on an inherently incredible threat). Each story carries its own epsilon, beneficiary/victim structure, and claimed type. Upstream/downstream structure: this reading is the categorical upstream claim — the paradox reading arose historically as a response to it (how to manufacture credibility for a threat whose execution is impossible), and the dropout reading relaxes its categorical premise by one degree. All three files link one to another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
