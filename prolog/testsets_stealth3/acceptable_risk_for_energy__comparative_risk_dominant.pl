% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative-Risk-Only Acceptability Standard for Nuclear Energy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   The standing arrangement under contest is the governance norm that
 *   nuclear risk is acceptable only insofar as it compares favorably against
 *   the available alternative — chiefly coal's particulate mortality and
 *   climate catastrophe — with no absolute threshold at which reactor risk
 *   would be unacceptable in itself. This standard is administered through
 *   probabilistic licensing methodology, sustained by operator-funded
 *   comparative studies, and reinforced by procedural rules that classify
 *   categorical-limit arguments as inadmissible. It coordinates a genuine
 *   collective problem (choosing a generation mix under uncertainty) while
 *   concentrating site, tail, and intergenerational risk on seats that did
 *   not bargain for it. This file instantiates ONE reading
 *   (comparative_risk_dominant) of the kernel acceptable_risk_for_energy; the
 *   sibling readings catastrophic_tail_dominant and expected_value_dominant
 *   are separate constraint files with their own epsilon values and
 *   classifications, linked via network.affects_constraints. Epsilon's
 *   referent is the standing comparative-only arrangement as this reading
 *   itself assesses it — the reading judges the overall trade net-justified,
 *   which is why epsilon stays well below snare range despite the real
 *   concentration asymmetry. The claim (tangled_rope) and the metric values
 *   are independent authored facts: the claim states what this reading
 *   believes is structurally true; the metrics describe observed operation;
 *   the engine computes per-seat classifications and any claim/metric
 *   divergence is signal, not error.
 *
 * KEY AGENTS:
 *   - - nuclear_regulatory_agencies: Agenda-setter (institutional/identity_locked) — administers the comparative licensing methodology and defines what counts as admissible risk argument
 *   - - nuclear_operators_licensees: Primary beneficiary (powerful/constrained) — collects license continuity, revenue, and capped liability under the arrangement
 *   - - reactor_host_communities: Primary target (powerless/trapped) — bears concentrated site and evacuation-zone risk
 *   - - uranium_mining_communities: Front-end target (powerless/trapped) — bears extraction-phase exposure far from consuming grids
 *   - - waste_burden_future_generations: Intergenerational target (powerless/trapped, civilizational horizon) — inherits deferred waste custody with no seat
 *   - - liability_backstop_taxpayers: Residual target (powerless/trapped) — absorbs tail losses beyond statutory liability ceilings
 *   - - climate_vulnerable_populations: Comparand constituency (powerless/trapped, global) — their prospective climate losses ground the acceptability judgment; they hold no negotiating seat
 *   - - climate_policy_institutions: Secondary beneficiary (institutional/constrained, global) — target arithmetic depends on fleet continuation
 *   - - absolute_threshold_advocates: Excluded voice (moderate/constrained) — categorical-limit arguments ruled procedurally inadmissible
 *   - - independent_risk_analysts: Analytical observer (analytical/analytical) — computes the comparison tables every camp cites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.63).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.58).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.63).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative-Risk-Only Acceptability Standard for Nuclear Energy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '09dd5d25-d70f-4680-9032-8977be6b4e9a').
narrative_ontology:cs_kernel_codification('09dd5d25-d70f-4680-9032-8977be6b4e9a', distributed).
narrative_ontology:cs_authority_grounding('09dd5d25-d70f-4680-9032-8977be6b4e9a', expertise).
narrative_ontology:cs_interpretation_layer_present('09dd5d25-d70f-4680-9032-8977be6b4e9a').
narrative_ontology:cs_reading_relation('09dd5d25-d70f-4680-9032-8977be6b4e9a', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('09dd5d25-d70f-4680-9032-8977be6b4e9a', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('09dd5d25-d70f-4680-9032-8977be6b4e9a', foundational, no_absolute_risk_threshold).
narrative_ontology:cs_axiom_status(no_absolute_risk_threshold, holdable).
narrative_ontology:cs_axiom_grounding('09dd5d25-d70f-4680-9032-8977be6b4e9a', no_absolute_risk_threshold, empirically_contingent).
narrative_ontology:cs_axiom('09dd5d25-d70f-4680-9032-8977be6b4e9a', foundational, climate_urgency_precedence_over_waste_horizon).
narrative_ontology:cs_axiom_status(climate_urgency_precedence_over_waste_horizon, holdable).
narrative_ontology:cs_axiom_grounding('09dd5d25-d70f-4680-9032-8977be6b4e9a', climate_urgency_precedence_over_waste_horizon, instrumental).
narrative_ontology:cs_axiom('09dd5d25-d70f-4680-9032-8977be6b4e9a', secondary, system_level_comparison_supersedes_local_consent).
narrative_ontology:cs_axiom_status(system_level_comparison_supersedes_local_consent, holdable).
narrative_ontology:cs_axiom_grounding('09dd5d25-d70f-4680-9032-8977be6b4e9a', system_level_comparison_supersedes_local_consent, conventional).
narrative_ontology:cs_reference_frame('09dd5d25-d70f-4680-9032-8977be6b4e9a', comparative_alternative_baseline).
narrative_ontology:cs_drift_state('09dd5d25-d70f-4680-9032-8977be6b4e9a', contemporary_renewables_parity_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('09dd5d25-d70f-4680-9032-8977be6b4e9a', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_operators_licensees).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, grid_consumers).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_policy_institutions).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, reactor_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, uranium_mining_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, waste_burden_future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, liability_backstop_taxpayers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, grid_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and administers the licensing methodology by which reactor hazards are evaluated: probabilistic safety assessments, dose-limit compliance, and periodic reviews framed against the generation mix that would replace the plant. Staff are recruited and trained inside the probabilistic-assessment tradition; submissions arguing from categorical prohibitions rather than quantified comparison are ruled procedurally out of order. The agency publishes the risk comparisons that anchor parliamentary debates on fleet continuation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_regulatory_agencies, agenda_setter,
    institutional, generational, identity_locked, national).

% Operate reactor fleets and sell the output. Continuation depends on passing periodic reviews framed as comparisons against replacement generation; owners fund the studies and advocacy that sustain that framing. Assets are site-bound with multi-decade book lives; divesting mid-life strands capital, so owners defend continuation even while diversifying into gas and renewables. Statutory liability ceilings cap their worst-case losses.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_operators_licensees, beneficiary,
    powerful, generational, constrained, continental).

% Receive electricity whose carbon content is lowered by reactor output, and pay through retail tariffs, decommissioning levies folded into bills, and the tax share of accident-liability backstops. Individual households cannot opt out of the pooled arrangements; engagement is occasional and electoral.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, grid_consumers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, grid_consumers, payer).

% Ministries, treaty secretariats, and scenario bodies that count reactor output toward decarbonization targets and publish pathways in which nuclear substitutes fossil capacity. Their target arithmetic assumes the fleet continues; redesigning pathways to exclude nuclear forces wholesale revision of compliance plans and renegotiation inside treaty frameworks.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_policy_institutions, beneficiary,
    institutional, generational, constrained, global).

% Live beside reactor sites and repository candidates. Site municipalities receive employment, tax-base transfers, and negotiated community funds; the surrounding population carries evacuation-zone exposure and post-incident stigma that depresses property values. Households cannot relocate the hazard; moving away means abandoning homes and local economies built around the site.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, reactor_host_communities, payer,
    powerless, biographical, trapped, local).

% Inhabit districts where uranium extraction and milling occur, frequently on Indigenous or historically marginalized territory. Exposure arises at the front end of the fuel cycle, far from the consuming grids; monitoring and remediation commitments have been repeatedly deferred, and residents lack the economic base to relocate.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, uranium_mining_communities, payer,
    powerless, generational, trapped, regional).

% Will inherit spent fuel and vitrified waste held in interim storage pending repository programs that successive governments have postponed. They hold no seat in any current proceeding; their interests enter only as projections prepared by present-day analysts, and each postponement shifts custody costs and uncertainty outward.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, waste_burden_future_generations, payer,
    powerless, civilizational, trapped, regional).

% Stand behind statutory liability ceilings: insurance and operator reserves cover losses up to the cap, and claims beyond it fall to public accounts. The arrangement is invisible in ordinary budgets and surfaces only in severe accidents; taxpayers neither chose the ceiling nor can decline the exposure.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, liability_backstop_taxpayers, payer,
    powerless, biographical, trapped, national).

% Face drought, flood, and heat mortality that decarbonization is supposed to reduce. Their prospective losses are the quantity against which reactor hazards are judged tolerable, yet they are absent from the siting, licensing, and liability negotiations where that judgment is made. If displaced fossil generation actually retires they share the air-quality and climate gains; if the trade stalls they absorb continued fossil harm on both ends of the comparison.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, beneficiary).

% Jurists, ethicists, and movement lawyers who argue that some hazards — long-lived waste without a demonstrated repository route, accident exposure imposed without consent — should be categorically barred regardless of system-wide comparisons. Licensing procedures classify categorical arguments as non-quantitative and inadmissible, limiting their participation to commentary outside the decision record.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, absolute_threshold_advocates, excluded,
    moderate, generational, constrained, global).

% Academic and institute researchers who compute mortality-per-TWh tables, externality valuations, and accident-frequency updates for all generation technologies. Defenders and opponents of reactor fleets alike cite their outputs; the researchers hold no enforcement role and publish across the whole dispute.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, independent_risk_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one shared, quantitative decision standard for choosing among hazardous generation options under uncertainty — enabling licensing, portfolio planning, and cross-border regulatory harmonization, and preventing locality-level vetoes from freezing the entire generation mix.
% TRANSFER_FUNCTION: Moves concentrated site, accident-tail, and deferred-waste risk onto host communities, mining districts, future custodians, and public liability accounts; moves dispatchable low-carbon power and target-compliance credit to consumers and climate institutions; moves license continuity and capped-loss protection to operators.
% ABSENT_VOICES: Absolute-threshold advocates are procedurally excluded from licensing records (categorical arguments are classified as non-quantitative); future waste custodians have no representative and enter only as analyst projections; climate-vulnerable populations are cited as the comparand that legitimizes reactor risk but hold no seat in siting, licensing, or liability negotiation.
% DISAPPEARANCE_RATIONALE: Overnight removal of the comparative-only standard would force every renewal and new-build decision onto an unbargained absolute question. Most existing fleets would fail a categorical review while waste routes remain undemonstrated, forcing near-term fossil backfill in nuclear-dependent grids, an emissions and price shock, and stranded operator assets — while host, mining, future-custodian, and taxpayer seats simultaneously shed imposed exposures. Both halves of the arrangement rearrange.
% FOUNDING_PROBLEM: Early nuclear expansion required a defensible public rationale for licensing an unprecedented hazard whose worst case exceeded ordinary industrial bounds, and that rationale had to survive comparison against the visibly lethal incumbent system — coal smog, mining disasters — and later against climate change.
% FOUNDING_PROBLEM_CORROBORATION: Partly corroborated from outside the benefiting parties: UNSCEAR mortality compilations and IPCC pathway literature independently attest both the severity of the fossil alternative and the reality of the decarbonization problem. But no attester outside the benefiting set affirms that the comparative-only form — the absence of any absolute floor — remains necessary rather than habitual: categorical-limit advocates, the German constitutional-political exit of 2011, and parts of the public-health literature explicitly deny it. That silence outside the beneficiary set is itself signal.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.63 (terminal measurement): the standard concentrates non-consensual site risk, defers waste custody indefinitely, and socializes catastrophic tails through liability ceilings — real transfers the reading itself concedes, though it judges the aggregate trade justified, which holds epsilon below snare range. Suppression is 0.58 and is authored as a RAW STRUCTURAL PROPERTY, unscaled by power or scope (only extractiveness is engine-scaled by directionality and scope): procedural admissibility rules and harmonized international frameworks block categorical alternatives, but exits demonstrably exist (Germany's exit, Austria's ban), so suppression stays well below total. Theater ratio 0.35: the comparative computations are real, but each crisis cycle produces ritualized reassurance reviews whose conclusions are largely pre-decided. Accessibility_collapse 0.40 — the categorical-standard alternative remains legally instantiated and achievable, so alternatives do not collapse. Resistance 0.55 — recurrent mass mobilization that has won outright in several polities. The measurement series run on ONE shared eight-point grid (1979, 1986, 1990, 2000, 2011, 2015, 2020, 2025) so every tracked metric is authored at every examined point. The pattern is CYCLICAL, not monotonic: crisis (Three Mile Island-era formalization, Chernobyl 1986, Fukushima 2011) drives an enforcement ratchet (suppression_requirement spikes to 0.62 and 0.66), followed by partial decay as normalization proceeds (0.55, 0.50; 0.60, 0.56) — two-plus full cycles visible in the series. The oscillation is partly functional (genuine post-accident learning) and partly an extraction mechanism: each ratchet's new machinery is retained in decayed form between crises, so the floor of both suppression and theater steps upward across cycles. Independent of the cycle, base_extractiveness trends upward (0.50 to 0.63) via a second mechanism this reading's own logic exposes: the standard is INVARIANT to the comparand improving. As renewable-plus-storage costs fall, the fossil-alternative justification thins, yet the no-absolute-floor clause means the standard never tightens in response — the extraction ratchet runs on the comparand's decay, not on crisis events.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter and operator seats, the arrangement appears as a hard-won coordination achievement that made any low-carbon buildout administratively possible; from the host, mining, future-custodian, and taxpayer seats, the same structure operates as imposed risk without bargaining. Same-power lateral divergence is stark: French and German regulators entered Fukushima 2011 with identical institutional standing, near-identical methodological training, and access to the same comparative data — and reached opposite fleet verdicts within months. What differentiated them was not power but exit-option structure: German grids had import capacity, domestic coal alternatives, and a renewable buildout path, while French fleet dependency made exit prohibitively costly. Equal global standing, divergent constraint experience, driven entirely by constraint-specific substitutability. Identity-lock dynamics bind the agenda-setter seat: agency staff professional identity is constituted BY the probabilistic-comparative tradition — acknowledging an absolute floor would amount to admitting the method cannot answer the question citizens actually ask ('how much is too much'), which is why categorical submissions are ruled procedurally inadmissible rather than answered. If that identity frame broke (a jurisdiction adopting a categorical standard and surviving economically), the admissibility barrier would lose its enforcement backbone quickly.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (operators, consumers, climate institutions) sit near the beneficiary end: operators collect license continuity and capped liability directly; consumers receive the power product while paying decommissioning levies and backstop taxes (dual-positioned, pulling them off the pure-beneficiary pole toward symmetry); climate institutions collect target compliance. Declared victims sit near the target end: host communities (trapped, local scope — verification easy but exit nil), mining communities (trapped, regional), future custodians (trapped at civilizational horizon, maximal d since no exit is conceivable), taxpayers (trapped behind invisible liability ceilings), and climate-vulnerable populations (dual-positioned per the manifest delta: they supply the comparand that legitimizes the trade and would share its gains if delivered, but bear the downside if it stalls, holding no seat where the bargain is struck). The engine derives per-seat directionality from these declarations plus exit structure; no overrides were needed because the structural data already differentiates the dual-positioned seats from the pure poles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — licensing an unprecedented hazard against a visibly lethal incumbent — retains a live core: the climate alternative is not hypothetical, and UNSCEAR/IPCC data corroborate the comparison's material. But the no-absolute-floor clause has detached from that necessity: it originated as a way to keep the comparison honest and now functions to prevent the comparison from ever concluding unfavorably, since the standard is structurally incapable of tightening when the alternative improves. The R5 mismatch (status contested x verdict world_rearranges) flags this capture-risk without collapsing the arrangement into pure extraction. Mandatrophy discipline prevents both symmetrical errors: reading the whole arrangement as a snare ignores the genuine coordination function (without a shared comparative standard, locality vetoes freeze every generation mix, and the fossil incumbent kills deterministically in the interim); reading it as pure rope ignores the concentrated, unconsented imposition on five distinct payer seats. The theater_ratio trajectory (0.20 to 0.35, stepping upward across crisis cycles) is the early-warning indicator for piton drift: if the comparative machinery continues accreting ritual review while the underlying waste question stays frozen, the arrangement trends toward performance-maintained inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This story is one reading (comparative_risk_dominant) of kernel acceptable_risk_for_energy. Which reading should govern acceptability judgments, and how does the computed classification shift under each?',
    'Political or legal settlement of the acceptability-criterion dispute, or corpus meta-analysis across the three sibling files comparing computed per-seat classifications against observed policy outcomes (fleet continuations, exits, relicensing records).',
    'Adopting catastrophic_tail_dominant converts the waste-custodian and host seats to categorical-victim status and pushes classification toward snare; adopting expected_value_dominant compresses the victim set to probability-weighted cost bearers and lowers measured extraction across seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Classification is contingent on kernel-reading selection; siblings are separate files linked via network.affects_constraints.').

omega_variable(
    comparand_parity_erosion,
    'This reading''s warrant requires the fossil alternative to remain materially worse than nuclear. As renewable-plus-storage costs decline, does the standing standard tighten as the comparand improves — and if it structurally cannot, at what parity point does the no-absolute-floor clause lose factual warrant?',
    'System-level cost and reliability studies of firm low-carbon portfolios without nuclear, tracked against the replacement-generation assumptions embedded in actual licensing reviews.',
    'If parity arrives and the standard does not tighten, the no-absolute-floor clause is exposed as serving its administrators rather than tracking the comparison, and the arrangement trends snare for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparand_parity_erosion, empirical, 'Whether the comparand''s decay invalidates the reading''s warrant faster than licensing practice adapts.').

omega_variable(
    consent_vs_compensation_hosts,
    'Are host-community compensation packages (employment, tax transfers, community funds) genuine consent to site risk, or acquiescence produced by economic dependence on the site?',
    'Longitudinal survey and exit-behavior data comparing communities with and without compensation schemes, including post-announcement property-market responses.',
    'Genuine consent lowers the host-seat extraction estimate and supports the coordination reading; dependence-driven acquiescence raises it and strengthens the imposed-risk reading of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_compensation_hosts, conceptual, 'Whether the host seat''s position reflects bargained exchange or structural capture of the local option set.').

omega_variable(
    intergenerational_override_legitimacy,
    'The reading''s temporal-urgency premise treats present climate mortality as overriding deferred waste custody. Is that priority ordering a legitimate weighting, or an illegitimate discount of parties with no representation?',
    'Cross-jurisdictional ethical and legal analysis of intergenerational obligations, plus revealed preference in repository-program funding trajectories (whether custody spending tracks stated urgency or defers systematically).',
    'If the override is judged illegitimate, the future-custodian seat computes as a trapping mechanism and the arrangement trends snare from that seat; if legitimate, the current classification holds and the sibling catastrophic_tail reading loses its strongest attack vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_override_legitimacy, conceptual, 'Legitimacy of the temporal-urgency-over-waste precedence that distinguishes this reading from its siblings.').

omega_variable(
    tail_socialization_magnitude,
    'How large is the segment of catastrophic accident cost that sits outside operator balance sheets under statutory liability ceilings?',
    'Actuarial reconstruction of full-probability loss distributions for severe accidents versus insured layers and state backstop provisions across jurisdictions.',
    'A large uninsured segment raises the taxpayer seat''s extraction share and the arrangement''s overall measured extraction; a small segment supports the reading that operators internalize their own risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_socialization_magnitude, empirical, 'Magnitude of the liability-ceiling socialization channel through which tail risk reaches public accounts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 1979, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1979, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1979, 0.2).
narrative_ontology:measurement(acce_tr_t1986, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1986, 0.24).
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(acce_tr_t2011, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2011, 0.33).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(acce_tr_t2020, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2020, 0.32).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(acce_be_t1979, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1979, 0.5).
narrative_ontology:measurement(acce_be_t1986, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1986, 0.56).
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(acce_be_t2011, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2011, 0.6).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(acce_be_t2020, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2025, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1979, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1979, 0.48).
narrative_ontology:measurement(acce_su_t1986, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1986, 0.62).
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(acce_su_t2011, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2011, 0.66).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(acce_su_t2020, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% The colloquial question 'is nuclear risk acceptable?' decomposes into three structurally distinct constraints, one per reading of kernel acceptable_risk_for_energy: they differ in victim set (who counts as harmed), temporal weighting (whose losses dominate), and threshold structure (absolute floor vs relative ranking vs computed balance). Authoring them as one story would force observable-dependent epsilon; instead each reading gets its own file, linked via affects_constraints. This reading currently dominates regulatory practice and thereby shapes the operating environment of the expected_value_dominant sibling (which must internalize counterfactual fossil baselines to stay policy-relevant) while coexisting with the catastrophic_tail_dominant sibling as rival live camps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
