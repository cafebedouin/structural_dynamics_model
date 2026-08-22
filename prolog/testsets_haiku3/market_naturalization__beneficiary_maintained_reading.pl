% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Defended Capital Privilege
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the BENEFICIARY_MAINTAINED reading of
 *   the market_naturalization kernel. The core claim: market dominance by
 *   incumbent capital holders is actively defended through identifiable
 *   suppression mechanisms (regulatory capture, vertical integration, patent
 *   stockpiling, financing gatekeeping, network effects exploitation) that
 *   would not persist without continuous maintenance by beneficiary actors.
 *   This reading frames market structure as a product of ongoing incumbent
 *   defense, not as a natural outcome of competitive efficiency or
 *   technological inevitability. The reading is contested by the
 *   lapsed_alternative_reading (which argues dominance is structural entropy
 *   requiring no maintenance) and by the hybrid_reading (which admits both
 *   active and passive mechanisms). Under this reading's premises, market
 *   dominance contains high extractiveness (rents captured above competitive
 *   level + enforcement costs), high suppression (barriers to entry actively
 *   maintained), and moderate theater (efficiency claims made about
 *   maintenance activity that is primarily exclusionary). This story does NOT
 *   adjudicate the contest between readings — it authors the structural data
 *   that instantiates one reading's perspective completely, leaving empirical
 *   resolution to the sibling stories and to domain evidence.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: Owners/controllers of large incumbent enterprises benefiting from market dominance; defend through lobbying, legal/patent mechanisms, vertical integration, financing gatekeeping, network-effect exploitation — their power is institutional, exit options are arbitrage (can redeploy capital), spatial scope is global.
 *   - excluded_entrants: Would-be competitors and new firms barred from or severely constrained in market entry; constrained exit (sunk R&D, cannot reallocate without loss), trapped or identity_locked (aspirational entrepreneurs), power is powerless-to-moderate depending on sector.
 *   - labor_force: Workers in incumbent-dominated sectors whose wages are suppressed by monopsony power and whose outside options are degraded by absence of competing employers; constrained or trapped exit.
 *   - consumer_welfare_absorbers: Consumers paying above-competitive prices and receiving below-competitive quality due to lack of competitive pressure; moderate power (organized collective action is difficult), mobile exit (can seek alternatives at higher search cost).
 *   - regulatory_apparatus: Sits between incumbent and entrant, staffed/influenced by incumbent capital through revolving door, funding, lobbying; observer and partially captured seat.
 *   - technology_incumbents: Platform/infrastructure owners (cloud, payment networks, supply chain hubs) who maintain dominance through network effects and switching costs, often serving as force multipliers for incumbent capital in product markets.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.81).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.76).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance as Actively Defended Capital Privilege").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, 'ae5e4275-458c-4fc3-a449-f17cd7019463').
narrative_ontology:cs_kernel_codification('ae5e4275-458c-4fc3-a449-f17cd7019463', distributed).
narrative_ontology:cs_authority_grounding('ae5e4275-458c-4fc3-a449-f17cd7019463', extraction).
narrative_ontology:cs_reading_relation('ae5e4275-458c-4fc3-a449-f17cd7019463', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae5e4275-458c-4fc3-a449-f17cd7019463', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('ae5e4275-458c-4fc3-a449-f17cd7019463', foundational, incumbent_defense_actively_maintained).
narrative_ontology:cs_axiom_status(incumbent_defense_actively_maintained, holdable).
narrative_ontology:cs_axiom_grounding('ae5e4275-458c-4fc3-a449-f17cd7019463', incumbent_defense_actively_maintained, empirically_contingent).
narrative_ontology:cs_axiom('ae5e4275-458c-4fc3-a449-f17cd7019463', foundational, market_dominance_suppresses_alternatives).
narrative_ontology:cs_axiom_status(market_dominance_suppresses_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('ae5e4275-458c-4fc3-a449-f17cd7019463', market_dominance_suppresses_alternatives, empirically_contingent).
narrative_ontology:cs_reference_frame('ae5e4275-458c-4fc3-a449-f17cd7019463', competitive_market_discipline).
narrative_ontology:cs_drift_state('ae5e4275-458c-4fc3-a449-f17cd7019463', contemporary_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae5e4275-458c-4fc3-a449-f17cd7019463', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, excluded_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, labor_force).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, consumer_welfare_absorbers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, consumer_welfare_absorbers).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, technology_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large incumbent enterprises and their capital-owning controllers who benefit from market dominance and actively deploy resources to defend it. They set entry standards (via lobbying), acquire potential competitors, use patent systems strategically, control access to complementary resources (financing, distribution, supply chain), and leverage network effects. They justify these actions as necessary for efficiency, stability, and continued R&D investment. They directly experience defending dominance as costly (legal expenses, regulatory overhead, acquisition premium for preemption) and collectively benefit from the rents that result.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Would-be competitors, entrepreneurs, and new firms unable or severely constrained in entering incumbent-dominated markets. They face capital barriers (financing unavailable except at incumbent-serving rates), legal barriers (patent clusters, licensing requirements favoring incumbents), supply chain barriers (incumbents control access to complementary goods), and information barriers (network effects and switching costs make entrant offerings invisible to consumers). Exiting entrant status requires abandoning industry-specific R&D and human capital, or accepting employment in subcompetitive roles. They perceive market dominance as suppression, not efficiency.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, excluded_entrants, payer,
    powerless, biographical, trapped, global).

% Workers in incumbent-dominated sectors whose wage options are constrained by monopsony power — absence of competing employers in their skill area. Market dominance directly suppresses labor mobility: workers cannot easily move to competitor firms (few exist), cannot negotiate for higher wages (employers know outside options are limited), cannot use competitive labor market threat to extract productivity gains. Their skill specialization locks them into the sector; retraining is costly. They bear extraction through wage suppression and reduced bargaining power.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, labor_force, payer,
    organized, biographical, identity_locked, global).

% End consumers paying above-competitive prices and receiving below-competitive quality/service due to lack of competitive pressure on incumbents. They simultaneously benefit from stable, large-scale platforms with network effects (coordination benefit — it is easier to use one large payment system than many fragmented ones). Their exit is costly but available: they can search for substitutes, accept lower quality in exchange for lower price, or organize collective action. The constraint extracts through pricing power while coordinating through reliability and scale.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, consumer_welfare_absorbers, payer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, consumer_welfare_absorbers, beneficiary).

% Government agencies (antitrust, sector regulators, finance authorities) tasked with preventing market dominance from becoming extractive. Partially captured by incumbent capital (revolving door, funding influence, lobbying penetration) but also constrained by political pressure from excluded entrants and labor. They have formal authority to break up dominance, impose behavioral restrictions, or mandate access but lack political will or resources to exercise it consistently. Their seat experiences the constraint as oscillating between capture (doing incumbent bidding) and reform pressure (responding to constituency complaints).
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, regulatory_apparatus, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, regulatory_apparatus, agenda_setter).

% Infrastructure and platform incumbents (cloud providers, payment networks, data analytics providers, supply chain hubs) who maintain their own market dominance through network effects and who simultaneously serve as force multipliers for product-market incumbents — they can condition access to infrastructure on incumbents' market position, refuse service to entrants, or integrate vertically to foreclose competition. They benefit both from their own market power and from the fragmentation of incumbent industries that limits their collective bargaining power.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, technology_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordination of large-scale capital investments in R&D, distribution, and operations that benefit from network effects and coordination: a single payment system is cheaper than many, a stable platform attracts investment, unified supply chains achieve economies of scale. The constraint channels this coordination benefit through incumbent incumbents specifically — it solves the real coordination problem but does so by restricting access.
% TRANSFER_FUNCTION: Transfers wealth from consumers (above-competitive prices), from labor (suppressed wages and constrained mobility), and from excluded entrants (foregone market opportunities) to incumbent capital holders (rents from pricing power, monopsony power, and reduced competitive pressure). The transfers also subsidize R&D and large-scale operation that benefits coordinated value creation — distinguishing transfers FOR coordination (necessary) from transfers BEYOND coordination (extraction).
% ABSENT_VOICES: Potential entrants who could testify to barriers are excluded from markets, making it difficult for them to organize testimony. Labor mobility is suppressed, so alternative-industry workers cannot easily testify to wage suppression comparisons. Consumer preference is revealed through behavior (choosing incumbent service despite above-market prices) but absent from formal governance structures (consumer welfare is not represented in boards, regulatory capture circles, or political donation channels). Startup ecosystems and new industry advocates are themselves excluded from the conversation.
% DISAPPEARANCE_RATIONALE: If incumbent-maintained market dominance disappeared overnight (suppose all active defense mechanisms dissolved but technology and infrastructure remained), markets would rearrange: new entrants would form within weeks, pricing would compress toward competitive levels over months to years, wage competition would intensify among employers suddenly in competitive labor markets, consumer alternatives would proliferate. The fact that dominance persists only under active maintenance is the claim of this reading. The constraint is not a natural law (mountain) because its disappearance would trigger reorganization; it is not pure coordination (rope) because the coordination benefit could be preserved without the dominance; it is tangled because coordination and extraction are embedded together.
% FOUNDING_PROBLEM: Early large-scale markets required capital concentration and coordination to achieve network effects, large-scale operations, and reliable long-term investment in infrastructure. Competition among undercapitalized firms produced inefficiency, service fragmentation, and inadequate R&D. Incumbent firms solved this by concentrating capital, standardizing networks, and making long-term investments. Market dominance and the exclusion of small competitors were justified as necessary for solving the coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent capital holders and their economists attest the founding problem is still live: markets require large-scale capital, entry barriers protect necessary investments, dominance is justified. Independent economists, entrant advocates, and labor advocates attest the founding problem was solved 20+ years ago; current dominance is inertia and rent-seeking, not solution of the founding problem. Regulatory agencies have split testimony: some (captured agencies) side with incumbents; others (labor/antitrust advocates) contest the incumbents' narrative. The corroboration is split — no consensus outside the beneficiary class.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.62→0.81 over the interval) because incumbent capital accumulates rents through multiple, reinforcing mechanisms: pricing power from limited competition, monopoly/monopsony power, capital redeployment barriers, access rationing to complementary services. The measurement series shows accumulation through t=16, then plateauing by t=20, suggesting the constraint reaches a ceiling where further extraction meets increasing resistance or regulatory risk. Suppression is high (0.76 at end) and rising steadily, reflecting increasing investment in entry barriers: legal challenges to entrants, patent clustering, vertical integration to deny competitors access to supply chain, financing gatekeeping through incumbent-controlled credit channels, acquisition/pre-emption of potential rivals. Theater is moderate (0.42) and rising, indicating growing share of enforcement activity that is performative — efficiency narratives around 'scale economics' and 'best practices' that rationalize barriers that would not survive open justification. Accessibility_collapse is moderate-high (0.68) because alternatives DO exist in principle (new entrants could form, different ownership structures are technically feasible), but are rendered inaccessible by active suppression: not a natural law (mountain), but an actively constructed closure. Resistance is high (0.72) because excluded entrants, labor, consumers, and regulators all mount ongoing resistance — the constraint persists not through unanimous acceptance but through incumbent power asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (incumbent capital holders) and the victim seats (excluded entrants, labor, consumers) should compute very different classifications from the same structural data. Incumbent seat: experiences the constraint as necessary coordination (large-scale operations require stability, exclusion of price-cutting entrants preserves value creation capacity, network effects are legitimate coordination mechanics). Victim seats: experience the same structure as pure extraction (pricing power unrelated to service quality, barriers prevent superior alternatives from entering, rents are transferred from consumers and labor). The engine computes directionality per seat from power/exit/beneficiary declarations: incumbents near d=0.0 (beneficiary end, arbitrage exit), entrants near d=1.0 (target end, trapped exit), consumers near d=0.5 (symmetric: genuine coordination benefit from stable platforms offset by extraction via pricing). Labor is near d=0.8 (target end: monopsony power, identity_locked via skill specialization in incumbent sectors). The divergence is structural, not a misunderstanding — it follows from the asymmetric relationship to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders (institutional power, arbitrage exit): d ≈ 0.0-0.2. They benefit from the constraint's operation (rents, barriers that protect their position), exit is arbitrage-grade (can reallocate capital to any market segment), so effective extraction flows negative or near zero from their seat — they are subsidized by the constraint. Excluded entrants (powerless-to-moderate power, trapped exit): d ≈ 0.9-1.0. They bear the cost of exclusion (cannot enter, must seek employment in subcompetitive sectors), exit is trapped (would require abandoning industry-specific investments and human capital), so effective extraction is high. Labor in incumbent sectors (powerless-to-organized power, identity_locked exit): d ≈ 0.75-0.85. They are targets of monopsony extraction (wages suppressed by lack of competing employers), exit is identity_locked (career is defined by sector) or at best constrained (retraining costs are high). Consumers (organized power, mobile exit): d ≈ 0.4-0.6. They pay above-competitive prices (extraction) but receive service benefits from stable platforms and network coordination (subsidy side). No override is needed; the derivation chain (beneficiary/victim + exit → d) produces accurate directionality without adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate — market dominance preserved through incumbent defense — is LIVE and contested. The founding problem (efficient allocation of capital and goods) is still live and the reading disputes whether the constraint solves or subverts it. If the founding problem is solved by incumbent-maintained dominance (as the beneficiary seat claims), the reading is a rope or resource-allocation coordination. If the founding problem is actively UNSOLVED or WORSENED by incumbent extraction (as the victim seats and some regulators claim), the reading is a snare or tangled_rope. The classification decision (tangled_rope, not snare) rests on whether a genuine coordination function (network effects, stable platforms, large-scale R&D funding) remains embedded in the constraint, even though extraction is high. Evidence for this: even excluded entrants would accept market access at competitive rates; even labor and consumers support high-efficiency large platforms — they oppose the extraction, not the coordination. Thus the constraint coordinates legitimate functions AND extracts above the cost of those functions → tangled_rope. If incumbents were pure parasites adding no coordination value, it would be snare. The distinction turns on whether the victims would preserve the coordinating function while removing the extraction, or would jettison both — under this reading, they would preserve the function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contestation_market_naturalization,
    'Is market dominance maintained by incumbent capital actively suppressing alternatives, or is it a lapsed closure that persists through inertia without ongoing defense?',
    'Sibling readings (lapsed_alternative_reading, hybrid_reading) decompose the same kernel differently. This reading''s core claim — identifiable capital holders continuously defend their dominance through mechanisms that would not persist without active maintenance — is directly contested by the lapsed reading (which argues the closure is structural entropy requiring no defense) and partially contested by the hybrid reading (which admits both active and passive maintenance). Empirical resolution requires tracking enforcement expenditure, exit barrier construction timing, and counterfactual analysis of what market structures would emerge under unchanged technology if enforcement stopped.',
    'If active defense is dominant, the constraint is tangled_rope or snare (depending on whether coordination function survives without extraction). If lapsed closure is dominant, the constraint may be piton (theatrical maintenance of a function that no longer requires defending). If hybrid, the classification diverges by market segment and time period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_market_naturalization, empirical, 'Kernel contestation: active defense vs. lapsed closure as the sustaining mechanism.').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who precisely constitutes the ''incumbent capital holder'' beneficiary class? Does it include all capital, or only a subset (e.g., monopoly-scaled enterprises, incumbent families, specific sectors)?',
    'Profit/rent distribution analysis by firm size, age, and market concentration; measurement of whether SMEs or new entrants capture rents equivalent to incumbents; tracking of capital mobility between incumbent-held and entrant-available assets.',
    'Diffuse beneficiary class narrows the extractive surplus per holder and suggests the constraint operates partly through coordination side effects; concentrated beneficiary class (e.g., top 0.1% of capital holders) indicates pure rent extraction. Beneficiary diffuseness affects whether the constraint is rope, tangled_rope, or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'The scope and concentration of the beneficiary class.').

omega_variable(
    suppression_internalization_interpersonal_layer,
    'To what extent is the suppression of entrants and alternatives structural (legal barriers, capital requirements, technological lock-in) versus internalized (entrants believe they cannot succeed, aspirational entrepreneurs are socialized into accepting incumbent dominance as natural)?',
    'Post-barrier removal analysis: do exit rates from incumbent markets stay suppressed after legal/capital barriers are removed? Do aspirants from suppression-removed cohorts enter at historical rates or remain belief-locked? Comparison across jurisdictions with different barrier removal timelines.',
    'If suppression is primarily internalized, the constraint''s effective suppression value is higher than the structural measure suggests — targets carry the suppression with them after barriers are physically removed. If structural, removal of barriers should permit rapid market entry. This distinction affects whether the constraint can be fixed by policy change alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_interpersonal_layer, empirical, 'Structural vs. internalized suppression mechanism in market entry barriers.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does this reading''s claim — that market dominance is ''actively defended'' — refer to intentional strategies by beneficiaries, or to systemic preservation mechanisms that operate whether or not any individual actor consciously defends them?',
    'Genealogical analysis of specific entry-barrier mechanisms (patent law, vertical integration, regulatory capture, finance-sector gatekeeping) to establish whether they were intentionally constructed to exclude competitors or were adopted for other reasons and now serve as de facto barriers. Interview data from capital holders re: explicit vs. incidental maintenance. Compare jurisdictions with vs. without explicit anti-entry statutes.',
    'If defenses are intentional, the reading attributes agency and moral culpability; if systemic, the reading becomes a description of institutional momentum. Either way, the empirical structure (barriers exist, are maintained, suppress alternatives) is unchanged, but the committer frame differs. This is a conceptual distinction within the same structural referent — the reading''s ε remains constant but its normative framing shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Intentional vs. systemic preservation of market dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mark_tr_t4, market_naturalization__beneficiary_maintained_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__beneficiary_maintained_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(mark_tr_t12, market_naturalization__beneficiary_maintained_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__beneficiary_maintained_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__beneficiary_maintained_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(mark_be_t4, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(mark_be_t8, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 8, 0.74).
narrative_ontology:measurement(mark_be_t12, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 12, 0.78).
narrative_ontology:measurement(mark_be_t16, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 16, 0.81).
narrative_ontology:measurement(mark_be_t20, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 20, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(mark_su_t4, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(mark_su_t8, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(mark_su_t12, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(mark_su_t16, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(mark_su_t20, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 20, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__beneficiary_maintained_reading, 0.22).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, regulatory_capture__incumbent_gatekeeping).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, patent_system__monopoly_extension).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, vertical_integration__barrier_construction).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, monopsony_labor_markets).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the market_naturalization kernel. Sibling readings decompose the same empirical phenomenon (incumbent market dominance) into different causal stories: beneficiary_maintained (this story) attributes dominance to active incumbent defense; lapsed_alternative attributes it to institutional momentum; hybrid admits both. Each reading instantiates a distinct constraint with its own ε value, beneficiary structure, and suppression profile. The three stories form a constraint family linked via affects_constraints. Sibling constraints inherit the same core empirical referent but author different ε values because their causal premises differ: the beneficiary-maintained reading sees rents and enforcement costs as extractive overhead; the lapsed reading sees dominance as entropy-driven and the 'defense' as theater; the hybrid reading splits the difference. Empirical resolution requires comparing enforcement expenditure, measuring exit barriers' temporal dynamics, and analyzing counterfactual market structures under suspension of specific defense mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
