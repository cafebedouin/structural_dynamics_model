% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade Limit)
 *   domain: constitutional law/federalism/commerce regulation
 *
 * SUMMARY:
 *   The originalist narrow reading of the Commerce Clause holds that federal
 *   regulatory power extends only to trade crossing state lines and to the
 *   instrumentalities of that movement — the Gibbons formulation — leaving
 *   the general police power with the states. As an operative rule this
 *   reading governed the federal system from Gibbons v. Ogden (1824) to the
 *   1937 switch (NLRB v. Jones & Laughlin), and persists as a minority
 *   position partially revived in United States v. Lopez and United States v.
 *   Morrison. This story models the reading as the standing arrangement
 *   during its tenure. Per the ε-invariance principle, this is one of three
 *   structurally distinct constraints generated from the commerce_clause_text
 *   kernel: the expansive and substantial-effects readings have different ε
 *   values, different victim sets, and their own stories; this file
 *   instantiates only the narrow reading and links its siblings by network
 *   edge. The claim/metric relationship is authored independently: the
 *   reading CLAIMS a genuine federalism allocation (tangled_rope from its own
 *   seat — real coordination plus real asymmetric costs), while the metrics
 *   describe the historical record — extraction rising as the economy
 *   nationalized past the fixed line, active judicial enforcement, formalist
 *   strain, and concentrated arbitrage gains.
 *
 * KEY AGENTS:
 *   - us_supreme_court: agenda_setter (institutional/constrained) — administers the border-crossing line by judicial review; struck the federal child labor acts, the NRA, and the bituminous coal act
 *   - us_congress: primary institutional target (institutional/constrained) — its regulatory reach confined to crossing trade; constitutional amendment is its only exit and it failed (1924 Child Labor Amendment)
 *   - state_governments: primary beneficiary (organized/constrained) — retain undisturbed police power; collect governance authority
 *   - low_standards_industries: extraction beneficiary (organized/arbitrage) — pocket the differential between lax-state compliance costs and national standards; the seat the gains accrue to
 *   - anti_federal_consolidation_advocates: beneficiary (organized/mobile) — collect vindication of the limited-government structure
 *   - child_laborers and manufacturing_workers: victims (powerless/trapped) — bore the regulatory gap; Hammer v. Dagenhart is the canonical case
 *   - national_businesses: dual-positioned payer/beneficiary (powerful/constrained) — fragmented compliance costs, shielded from federal social regulation
 *   - cross_border_externality_communities: victims (powerless/trapped) — bear pollution and disease originating in other states with no federal recourse
 *   - constitutional_scholars: analytical observer — sees the full structure and the formalism strain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.71).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.74).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade Limit)").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional law/federalism/commerce regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '0278d408-49c6-417f-9113-2c2c2920d10b').
narrative_ontology:cs_kernel_codification('0278d408-49c6-417f-9113-2c2c2920d10b', fixed_text).
narrative_ontology:cs_authority_grounding('0278d408-49c6-417f-9113-2c2c2920d10b', lineage).
narrative_ontology:cs_interpretation_layer_present('0278d408-49c6-417f-9113-2c2c2920d10b').
narrative_ontology:cs_reading_relation('0278d408-49c6-417f-9113-2c2c2920d10b', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('0278d408-49c6-417f-9113-2c2c2920d10b', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('0278d408-49c6-417f-9113-2c2c2920d10b', foundational, commerce_means_border_crossing_trade).
narrative_ontology:cs_axiom_status(commerce_means_border_crossing_trade, holdable).
narrative_ontology:cs_axiom_grounding('0278d408-49c6-417f-9113-2c2c2920d10b', commerce_means_border_crossing_trade, conventional).
narrative_ontology:cs_axiom('0278d408-49c6-417f-9113-2c2c2920d10b', foundational, general_police_power_vests_in_states).
narrative_ontology:cs_axiom_status(general_police_power_vests_in_states, holdable).
narrative_ontology:cs_axiom_grounding('0278d408-49c6-417f-9113-2c2c2920d10b', general_police_power_vests_in_states, conventional).
narrative_ontology:cs_reference_frame('0278d408-49c6-417f-9113-2c2c2920d10b', founding_era_dual_sovereignty).
narrative_ontology:cs_drift_state('0278d408-49c6-417f-9113-2c2c2920d10b', post_new_deal_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('0278d408-49c6-417f-9113-2c2c2920d10b', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, low_standards_industries).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, child_laborers).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, manufacturing_workers).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, national_businesses).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, cross_border_externality_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, national_businesses).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, us_congress).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, dual_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, enumerated_powers_limitation).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, police_power_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reviews federal statutes against the border-crossing standard and strikes those regulating intrastate activity (Hammer v. Dagenhart, Schechter, Carter Coal). Its docket choices and doctrine define where the line sits; reversing the line costs institutional capital and required an existential legitimacy crisis in 1937. It collects no material rent, but its interpretive authority depends on the allocation remaining judicially managed.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, us_supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Holds regulatory authority only over trade crossing state lines and the instrumentalities of that movement; every attempt to reach manufacturing conditions, labor standards, or agriculture was voided (the child labor acts, the NRA, the AAA, the bituminous coal act). Its only formal exit is constitutional amendment, attempted for child labor in 1924 and never ratified.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, us_congress, payer,
    institutional, generational, constrained, national).

% Retain undisturbed police power over intrastate health, safety, labor, and morals, with no federal statute preempting their regulatory field. Each bears the countervailing pressure of competing against other states for industry, which limits how far any single state can raise standards, but each collects the governance authority itself and cannot exit the union that guarantees it.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    organized, generational, constrained, regional).

% States' rights politicians, limited-government jurists, and constitutional theorists who treat the border-crossing line as the constitutional bulwark against national consolidation. They collect vindication and political platform from the line's maintenance; their exit is ideological repositioning, which is always available to them.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, generational, mobile, national).

% Manufacturers concentrated in states with lax labor and safety regulation — Southern textiles after Hammer v. Dagenhart is the canonical case — who would bear real costs under a national standard. They pocket the differential between their actual compliance costs and what uniform federal regulation would impose, and can relocate production to whichever jurisdiction regulates least.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, low_standards_industries, beneficiary,
    organized, immediate, arbitrage, regional).

% Work under conditions set solely by their state's law and their employer's preference; where the state regulates least, they absorb the difference. Federal hours, wages, and safety legislation was voided on their behalf for the line's entire tenure, and they cannot relocate on the wages they earn.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, manufacturing_workers, payer,
    powerless, biographical, trapped, regional).

% The concrete case: the 1916 federal child labor law was struck in Hammer v. Dagenhart (1918) as regulation of intrastate manufacturing, leaving minimum-age standards to the states. Children in mill and mine districts worked under whatever their state allowed; exit was not available to them at all.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, child_laborers, payer,
    powerless, immediate, trapped, regional).

% Operate across state lines and must comply with a patchwork of state regimes for everything the federal government cannot reach; they testified before Congress for federal uniformity in the New Deal era. The same weakness of federal power also shielded them from federal taxation and social regulation, so they are simultaneously fragmented-compliance payers and incidental beneficiaries of federal restraint.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_businesses, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, national_businesses, beneficiary).

% Downstream and downwind communities bearing pollution, disease, and resource depletion originating in other states, where the source state has no incentive to regulate and the federal government lacks jurisdiction over the intrastate activity causing the harm. Exit means abandoning homes and land.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, cross_border_externality_communities, payer,
    powerless, generational, trapped, regional).

% Track the doctrine's fit against the economy it governs, document the growing formalism strain, and supply the historical and textual arguments both camps deploy. They collect nothing and pay nothing; their seat is analytical.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, low_standards_industries).
narrative_ontology:fixing_cost_class(commerce_clause_text__originalist_narrow_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides regulatory authority between two sovereign levels: the federal government governs trade crossing state lines and the instrumentalities of that movement; the states govern health, safety, labor, and morals within their borders. The division prevents either level from absorbing the whole regulatory field, keeps police-power regulation close to the governed communities, and gives national markets a single referee for crossing transactions.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction from the federal government to the states; concretely, it moves the costs of unmanaged cross-border externalities and fragmented compliance onto workers, children, downstream communities, and national businesses, while moving the arbitrage rents of regulatory differentials to industries positioned in low-standards states.
% ABSENT_VOICES: The people harmed in the gaps had no seat in the interpretive conversation: child laborers, mill workers, and downstream communities were categorized as subjects of 'local' regulation and therefore no one's federal concern, while the conversation was conducted among the Court, Congress, and the states. The states best positioned to speak for lax-standard interests were fully represented; those bearing the externalities of other states' choices were not in the room.
% DISAPPEARANCE_RATIONALE: If the border-crossing line vanished overnight — if federal commerce power were unlimited — federal statutes would immediately occupy labor standards, manufacturing conditions, agriculture, and cross-border externalities; state regulatory autonomy and the arbitrage rents built on it would evaporate; and the dual-sovereignty structure the union was organized around would be replaced by a single national regulatory sovereign. Every seat in the story rearranges.
% FOUNDING_PROBLEM: Under the Articles of Confederation each state erected tariffs and navigation barriers against its neighbors and no national authority could reach interstate trade; the Constitutional Convention's commerce power was built to give the federal government reach over crossing trade while leaving local police powers untouched — the narrow reading is that original allocation taken as binding.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: economic historians document the confederation-era trade wars, so the founding problem's reality is not a states'-rights invention; and national business testimony before Congress in the 1930s — a payer seat, not a beneficiary — attests that interstate friction and fragmentation remained real problems the narrow allocation no longer managed. The states'-rights seats attest the problem's persistence self-servingly; the corroboration that matters comes from the historical and business record.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises across the interval because the constraint's category was fixed while the economy was not: in 1824 the border-crossing line tracked most economically significant regulation, but as markets nationalized the gap between what the line reached (crossing trade) and what needed governing (production conditions, labor, externalities) widened, and the costs fell on the gap's occupants. Suppression tracks enforcement intensity: judicial review moved from largely dormant (0.30) to maximal (0.74 at Schechter and Carter Coal) as Congress legislated more and the Court struck more — the enforcement ratchet is the dynamic this story tracks, which is why suppression_requirement is authored on the shared grid. Theater rises as the direct/indirect-effects formalism strained against obvious national harms: by 1937 the doctrine's categories were visibly detached from the economy they governed (0.44). Accessibility collapse is 0.60: constitutional amendment was the only clean exit and it failed; the tax-and-spend route was struck in Butler; state regulation remained a partial but collective-action-impaired alternative. Resistance is 0.62: Congress re-enacted in new forms, progressive movements litigated and lobbied, and the 1937 court-packing threat finally broke enforcement. All three series run on one shared time grid (1824–1937 in calendar years) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the state_governments and anti-consolidation seats the arrangement is a genuine, load-bearing allocation: dual sovereignty, local accountability, no federal preemption — coordination. From the child_laborer, worker, and externality-community seats the same structure operates as enforced extraction: democratically enacted protections voided by an unelected tribunal, with the material gains accruing to industries positioned in lax jurisdictions. From the national_businesses seat it is both at once — fragmented compliance they lobbied to end, federal restraint they profited from. From the Court's seat it is a legitimacy budget: enforcing the line spent institutional capital until the budget ran out in 1937. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real structural relationships. state_governments receive undisturbed police power and hold no exit from the union (d near the beneficiary end). low_standards_industries receive arbitrage rents and hold arbitrage-grade exit — they can relocate production to whichever jurisdiction regulates least — placing them nearest the full-beneficiary end. anti_federal_consolidation_advocates collect ideological vindication with mobile exit. Victims map with equal directness: child_laborers and manufacturing_workers bear the regulatory gap with trapped exit (d near the full-target end); cross_border_externality_communities bear harms the line makes federally unreachable; national_businesses sit mid-range through their declared dual position — payer on fragmentation, incidental beneficiary on federal restraint. us_congress is a structural target: the arrangement extracts regulatory jurisdiction from it, with amendment as a prohibitively costly exit. The Court is neither declared beneficiary nor victim; it administers, spending legitimacy to enforce, and no directionality override is authored because the coarse power-atom override would also misstate Congress's target position.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two mislabelings. As pure coordination: the reading's own seat frames the arrangement as the founding allocation pure and simple, but the named victims and the concentrated gain_flow seat (low_standards_industries) show asymmetric costs riding the coordination — keeping both facts on the table is exactly the tangled-rope discipline. As pure extraction: the costs are real, but the coordination function is genuine and load-bearing — the division of regulatory authority solved a real founding-era collective-action problem and still does — so the structure is not cover. On mandatrophy proper: the founding problem (interstate trade friction, police-power preservation) is not dead; it is contested, which is why the arrangement did not atrophy into an inertial relic. It was repudiated while still functioning (1937) and then partially revived (1995) — a constraint abandoned by repudiation rather than decay — and the modern revival question is routed to an omega rather than resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates one reading of the commerce_clause_text kernel; what would the sibling readings (expansive_federal_reading, substantial_effects_limited_reading) change in the beneficiary/victim structure if either governed instead?',
    'Doctrinal seat-set analysis: under the expansive reading the victim set shifts to state regulatory autonomy holders and federalism-value losers; under the substantial-effects reading victims are confined to activity lacking a jurisdictional nexus. Judicial appointment outcomes and doctrine selection resolve which reading governs.',
    'The ε referent stays fixed (the border-crossing allocation) but the operative victim set and hence per-seat classification change completely under a sibling reading — this story''s classification is valid only for the originalist narrow reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings instantiate structurally different constraints.').

omega_variable(
    founding_allocation_vs_coalition_maintenance,
    'Is the border-crossing allocation a structural feature of the constitutional design that would persist regardless of who defends it, or a contested interpretive position sustained by an identifiable coalition (states'' rights blocs, limited-government jurists)?',
    'Counterfactual persistence test: the reading largely collapsed after 1937 when its judicial coalition was threatened and revived with a new coalition in the 1990s — track whether it survives electoral defeat of its holders or persists as doctrine independent of coalition.',
    'If coalition-maintained, the reading''s persistence depends on active enforcement by its holders and its operative profile sits nearer the enforced end; if structural, it would reassert itself regardless of coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_allocation_vs_coalition_maintenance, conceptual, 'Whether the reading''s persistence is structural or coalition-dependent.').

omega_variable(
    coordination_price_vs_removable_overhead,
    'How much of the measured extraction is the intrinsic price of dual-sovereignty coordination, and how much is removable overhead specific to the border-crossing line (arbitrage rents, externality gaps a nexus test would close)?',
    'Compare regulatory outcomes and externality management under the narrow regime (pre-1937), the expansive regime (post-1937), and the hybrid (post-1995): if the hybrid captures most coordination value while closing most gaps, the border-crossing line''s overhead is largely removable.',
    'If most extraction is removable overhead, the constraint drifts toward pure extraction riding a coordination cover; if most is coordination price, it is a genuine allocation with a high but honest floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_price_vs_removable_overhead, empirical, 'Decomposing measured extraction into coordination cost versus arbitrage rent.').

omega_variable(
    modern_revival_trajectory,
    'Does the post-1995 partial revival (Lopez, Morrison, the NFIB commerce-power discussion) re-operate the historical extraction profile on modern externality gaps (pandemic response, climate, platform economy)?',
    'Track the post-Lopez doctrine''s actual strike-down rate against federal statutes and the emergence of externality gaps the line cannot reach; project whether the revival deepens or remains rhetorical.',
    'If the revival deepens, the historical trajectory (rising extraction as economic integration outpaces the line) repeats at larger scale; if it stays rhetorical, the reading remains a minority position with negligible operative extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modern_revival_trajectory, empirical, 'Whether the reading''s modern revival re-operates its historical extraction profile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1824, 1937).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1824, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1824, 0.1).
narrative_ontology:measurement(comm_tr_t1856, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1856, 0.12).
narrative_ontology:measurement(comm_tr_t1887, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1887, 0.18).
narrative_ontology:measurement(comm_tr_t1905, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1905, 0.25).
narrative_ontology:measurement(comm_tr_t1918, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1918, 0.32).
narrative_ontology:measurement(comm_tr_t1930, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1930, 0.38).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.44).

% Extraction over time
narrative_ontology:measurement(comm_be_t1824, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1824, 0.38).
narrative_ontology:measurement(comm_be_t1856, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1856, 0.42).
narrative_ontology:measurement(comm_be_t1887, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1887, 0.5).
narrative_ontology:measurement(comm_be_t1905, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1905, 0.55).
narrative_ontology:measurement(comm_be_t1918, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1918, 0.63).
narrative_ontology:measurement(comm_be_t1930, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1930, 0.68).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1824, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1824, 0.3).
narrative_ontology:measurement(comm_su_t1856, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1856, 0.34).
narrative_ontology:measurement(comm_su_t1887, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1887, 0.42).
narrative_ontology:measurement(comm_su_t1905, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1905, 0.52).
narrative_ontology:measurement(comm_su_t1918, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1918, 0.6).
narrative_ontology:measurement(comm_su_t1930, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1930, 0.68).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the commerce_clause_text kernel per the ε-invariance principle: the colloquial label 'the Commerce Clause' covers three structurally distinct claims — the narrow border-crossing allocation (this story), the substantial-effects expansion, and the limited hybrid with nexus and non-pretext requirements. Their ε values differ because their victim sets differ: this reading's costs fall on those in the regulatory gap (workers, children, externality communities, fragmented businesses); the expansive reading's costs fall on state autonomy and federalism-value holders. The narrow reading is upstream in legitimacy lineage (the founding allocation), and its partial revival (Lopez/Morrison) created the structural pressure that shaped the hybrid's limiting doctrines — hence the influences edge to the substantial-effects reading. The dormant commerce clause (the negative implication barring state discrimination against interstate trade) is an adjacent structure not modeled in this family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
