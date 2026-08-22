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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Beneficiary-Maintained Naturalization of Markets
 *   domain: political economy / ideology studies / economic history
 *
 * SUMMARY:
 *   Since the late twentieth century, market arrangements have been presented
 *   across policymaking, media, and academic life as the natural default
 *   condition of economic life, the baseline against which other proposals
 *   are judged unrealistic. This story instantiates ONE reading of the
 *   contested kernel market_as_natural_default: the
 *   beneficiary_maintained_reading, which holds that this naturalization is
 *   not a residue of history but an actively maintained closure, defended
 *   post-hoc by an identifiable beneficiary class (finance, incumbent
 *   corporations, and the ideological infrastructure they fund) through
 *   public-relations campaigns, institute funding, editorial gatekeeping, and
 *   capture of the economics discipline. The epsilon referent is the standing
 *   arrangement under contest: the existing naturalized-market order as this
 *   reading sees it, with its suppressed alternatives and protected rents,
 *   never the reading's endorsed alternative. Sibling readings
 *   (lapsed_alternative_reading, hybrid_amnesia_reading) are separate
 *   constraints with their own epsilon values and are not averaged into this
 *   file. Claim/metric independence: the constraint is CLAIMED as
 *   tangled_rope, a genuine coordination function fused with asymmetric
 *   extraction held together by active enforcement, while the metrics are
 *   authored from the operation this reading describes; the engine computes
 *   per-seat types and any divergence is the measurement the corpus exists to
 *   take.
 *
 * KEY AGENTS:
 *   - financial_services_industry: Primary beneficiary (institutional/arbitrage) — collects the largest protected rents, funds the maintenance apparatus, insulated by capital mobility
 *   - incumbent_multinational_corporations: Secondary beneficiary (institutional/arbitrage) — collects regulatory restraint and bailout expectation, relocates against deviant jurisdictions
 *   - corporate_funded_think_tanks: Agenda-setter (organized/mobile) — produces the naturalization discourse, exit is funding-dependent
 *   - neoclassical_economics_establishment: Agenda-setter with beneficiary position (institutional/identity_locked) — certifies economic literacy, professional identity fused with the frame
 *   - organized_labor: Primary payer (organized/trapped) — bears suppressed bargaining power, cannot exit the wage relation
 *   - heterodox_economists: Excluded voice (moderate/constrained) — would testify alternatives existed, gated out of venues
 *   - alternative_economy_practitioners: Excluded voice and payer (moderate/constrained) — cooperatives and commons bearing marginalization costs
 *   - democratic_publics: Payer (moderate/trapped) — inherits the narrowed policy menu
 *   - economic_historians: Analytical observer (analytical/analytical) — documents the constructed genealogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.46).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.59).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.59).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Beneficiary-Maintained Naturalization of Markets").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political economy / ideology studies / economic history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, '15a2a7b3-5ce6-4215-a05a-8138cfa69fd0').
narrative_ontology:cs_kernel_codification('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', distributed).
narrative_ontology:cs_authority_grounding('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', extraction).
narrative_ontology:cs_interpretation_layer_present('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0').
narrative_ontology:cs_reading_relation('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', foundational, naturalization_is_engineered_closure).
narrative_ontology:cs_axiom_status(naturalization_is_engineered_closure, holdable).
narrative_ontology:cs_axiom_grounding('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', naturalization_is_engineered_closure, empirically_contingent).
narrative_ontology:cs_axiom('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', foundational, alternatives_suppressed_not_forgotten).
narrative_ontology:cs_axiom_status(alternatives_suppressed_not_forgotten, holdable).
narrative_ontology:cs_axiom_grounding('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', alternatives_suppressed_not_forgotten, empirically_contingent).
narrative_ontology:cs_reference_frame('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', spontaneous_market_order_baseline).
narrative_ontology:cs_drift_state('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', post_global_financial_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('15a2a7b3-5ce6-4215-a05a-8138cfa69fd0', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_services_industry).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, incumbent_multinational_corporations).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, corporate_funded_think_tanks).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, organized_labor).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, heterodox_economists).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, alternative_economy_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, neoclassical_economics_establishment).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, democratic_publics).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, spontaneous_order_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, market_inevitability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns and manages the pooled savings of the global economy. When market outcomes are treated as the natural baseline, questions about financial regulation, capital taxation, and bailout responsibility are reframed as technical rather than political, and the industry's preferred answers become the defaults. It funds a network of institutes, university programs, and media fellowships that produce and amplify this baseline. Its capital moves across jurisdictions within days, so no single government's deviation threatens it; it can route around any one country's rules.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, financial_services_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, financial_services_industry, agenda_setter).

% Operates established market positions built over decades. A naturalized baseline shields those positions: antitrust action reads as interference with nature, labor regulation as friction, and industrial policy as picking losers. It sustains trade associations and sponsored research that reinforce the baseline, and it can shift production or headquarters to friendlier jurisdictions when a government departs from it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, incumbent_multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Produces the daily stream of commentary, briefing papers, and broadcast appearances that keeps the baseline salient in policy debate. Its budget arrives from donors whose interests the baseline serves; editors and program directors select for work that treats market outcomes as given. Staff careers advance within a network of allied institutions, so departure carries real professional cost, but the skills transfer to adjacent organizations.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, corporate_funded_think_tanks, agenda_setter,
    organized, biographical, mobile, national).

% Trains the economists who staff ministries, central banks, and international agencies, and certifies what counts as serious economic argument. Its core curriculum presents market equilibrium as the reference case and deviations as anomalies requiring justification. Faculty careers, journal hierarchies, and prize visibility are built inside this framework; a senior figure who publicly abandoned it would forfeit standing accumulated over a lifetime. Counter-evidence such as crises, bubbles, and persistent unemployment is absorbed by extending the framework rather than revising the baseline.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, neoclassical_economics_establishment, agenda_setter,
    institutional, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, neoclassical_economics_establishment, beneficiary).

% Represents workers whose bargaining power depends on the right to strike, organize, and legislate working conditions. Under a naturalized baseline, each of those levers is framed as interference with market forces, and each defeat is recorded as the market speaking. Membership decline compounds the weakness: the less labor can deliver, the less governments consult it. Exit is not available, because the wage relation is where its members live.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, organized_labor, payer,
    organized, biographical, trapped, national).

% Works in traditions that treat market arrangements as historical constructions open to redesign. Its members publish in low-ranked journals, teach at peripheral departments, and are rarely invited to central-bank panels or editorial boards. The gatekeeping that excludes them operates through funding decisions, citation networks, and hiring norms rather than explicit bans, so the exclusion is deniable and hard to litigate.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_economists, excluded,
    moderate, biographical, constrained, continental).

% Runs worker cooperatives, community land trusts, credit unions, and commons-based provisioning at small scale. Every scaling attempt meets friction the baseline generates: lenders cite lack of comparable precedents, regulators apply templates built for investor-owned firms, and press coverage frames successes as curiosities. The practitioners bear these costs directly; their models remain legible mainly to each other.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, alternative_economy_practitioners, excluded,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, alternative_economy_practitioners, payer).

% Elects governments and inherits the policy menu those governments may choose from. Where market outcomes are natural, redistributive options drop off ballots before voters ever see them, not by ban but by being pre-classified as unrealistic. Voters also hold much of their savings in the very assets the baseline protects, which gives them a stake in its continuation even where they bear its costs in wages, housing, and eroding public services. Leaving the polity is possible only at the price of emigration.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, democratic_publics, payer,
    moderate, generational, trapped, national).

% Studies how the baseline was assembled: the transnational network of market-liberal intellectuals, the funding of particular economics programs, the deliberate cultivation of journalists, and the long campaign after the calculation debate. Its findings are published in academic presses and read mostly by each other; it holds no lever over the arrangement it documents, which is what makes its testimony usable as corroboration.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__beneficiary_maintained_reading, financial_services_industry).
narrative_ontology:fixing_cost_class(market_as_natural_default__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real coordination problem: a complex economy needs a shared, stable answer to how production and exchange are organized so that contracts, investments, prices, and policy expectations can be formed without continuously renegotiating the economic constitution. A common default frame supplies that answer cheaply.
% TRANSFER_FUNCTION: Moves legitimacy and decision rights upward: framing distributional outcomes as natural converts political questions into technical ones, transferring surplus share, regulatory discretion, and downside insurance from wage earners and publics to asset owners and creditors.
% ABSENT_VOICES: Heterodox economists, labor historians, and cooperative-movement practitioners would object that workable alternatives existed and were starved rather than refuted. They sit outside the venues where the baseline is reproduced, including mainstream journals, editorial pages, economics curricula, and central-bank panels, excluded by funding and gatekeeping rather than argument.
% DISAPPEARANCE_RATIONALE: If the naturalization frame vanished overnight, distributional questions would reopen as political questions: windfall taxes, public options, antitrust expansion, shorter-work-time regulation, and commons-based provision would re-enter the feasible set; the think-tank and media apparatus would lose its organizing frame; asset prices would reprice political risk; and careers built on administering the baseline would need new foundations.
% FOUNDING_PROBLEM: The mid-twentieth-century contest over whether complex industrial economies could be organized any other way: the socialist calculation debate and the Cold War competition made the existence of alternatives the defining question, and naturalization was built to close it by recasting markets as the default condition of exchange rather than one arrangement among many.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: economic historians (the Polanyian genealogy of market society; documented histories of the transnational neoliberal network and its institution-building) and economic anthropologists attest the baseline was assembled by identifiable people with fundable motives. The beneficiary set attests the opposite, that the problem the arrangement solved remains live and unsolved. No disinterested party attests the founding problem is still open, and that absence is itself signal.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).
:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.46: the arrangement genuinely coordinates (a shared economic default is cheap and useful) and simultaneously protects a large, identifiable rent pool (financial seigniorage, regulatory restraint, bailout asymmetry), placing it mid-band in the reading's expected 0.35-0.55 range. Suppression is authored at 0.59 as a raw structural property, unscaled by power or scope: gatekeeping, funding asymmetry, and career penalties actively narrow the option set, but democratic contestation persists, so nothing like the closure of a physical limit. Theater_ratio 0.41: a growing share of the apparatus's output is defensive performance (inevitability rhetoric, crisis reinterpretation, realism-policing) rather than original analysis; the series below shows this share roughly doubling across the interval. Accessibility_collapse 0.52: alternatives remain thinkable (post-2008 heterodox revival, cooperative growth, degrowth organizing) but meet steep institutional friction at every scaling step. Resistance 0.62: sustained movements keep the closure contested. Coordination type is declared identity_coordination because the dominant function is boundary maintenance: what counts as economically literate, realistic, or serious. The measurement series run on one shared six-point grid, every tracked metric authored at every point, showing a monotonic ratchet with no oscillation; the dynamic is accumulation, not cycling.
 *
 * PERSPECTIVAL GAP:
 *   From the financial seat the baseline is simply reality: the frame disappears into common sense and its defense feels like defending gravity. From the labor and public seats the same frame is a visible hand closing options that were open within living memory. The economics establishment occupies the hinge: it experiences the frame as scientific consensus while its identity, funding, and certification power depend on the frame's stability, making it the seat most likely to compute a different type than the payers because its exit is identity-locked rather than material. The historian seat sees construction without stakes. The engine computes these divergences from power, exit, and role data; the prose here explains why they should diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (financial_services_industry, incumbent_multinational_corporations, corporate_funded_think_tanks) drive those seats toward the beneficiary end of d; arbitrage-grade exit pins finance and corporates nearest zero. Victim declarations (organized_labor, heterodox_economists, alternative_economy_practitioners) drive those seats toward the target end; trapped exit (labor, publics) sits nearer full-target than constrained exit (heterodox academics, who retain credential mobility). Democratic_publics are declared victims but carry partial offsetting consumer benefit, so their d lands high but not maximal. No directionality_overrides are authored: the derivation from declared roles plus exit options already separates the seats correctly, and the establishment's identity lock enters through exit_options rather than d. Global spatial scope modestly amplifies effective extraction for target seats relative to a national arrangement, since verification of the frame's claims is harder at scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, settling whether complex economies had any workable alternative as posed by the socialist calculation debate and sharpened by the Cold War, is at best half-alive: the Soviet comparison is gone, modern computation and mixed systems have reopened parts of the question, yet the apparatus persists and its theater share rises. Reading the arrangement as pure extraction would erase the genuine coordination service a shared default performs; reading it as pure coordination would erase four decades of documented funding asymmetry and gatekeeping. The tangled_rope claim holds both truths apart. The rising theater_ratio series is the mandatrophy signature: the analytic function atrophies while the defensive function grows, meaning the arrangement increasingly maintains itself rather than solving anything.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_active_vs_lapsed_closure,
    'Is the naturalization actively produced and defended by identifiable beneficiaries (this reading), or does market dominance persist through passive historical forgetting of alternatives (the lapsed_alternative_reading)?',
    'Archival tracing of funding flows, public-relations campaigns, editorial gatekeeping records, and curriculum battles across the interval; the presence or absence of documented defensive mobilization at decision points (crises, electoral threats) discriminates the readings.',
    'If closure is passive, this reading''s epsilon collapses toward the sibling''s lower value and the classification drifts toward rope/piton profiles; if active, the tangled_rope/snare boundary sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_active_vs_lapsed_closure, empirical, 'Active defense versus passive forgetting as the persistence mechanism of market naturalization.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of alternatives structural (funding asymmetry, gatekeeping, career penalties) or internalized (policymakers and publics genuinely believing there is no alternative, persisting independently of the machinery)?',
    'Compare policy-option sets across jurisdictions with different funding and gatekeeping intensity at similar income levels; test belief persistence among officials trained inside the frame after they leave it.',
    'An internalized component means effective suppression exceeds the structural measure and outlasts the apparatus; dismantling funding streams alone would not reopen the option set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between external machinery and internalized inevitability beliefs.').

omega_variable(
    beneficiary_coordination_vs_emergent_selection,
    'Do beneficiaries coordinate the defense of naturalization deliberately (shared strategy, funder networks acting in concert), or does an emergent selection effect, in which profitable framings attract funding and unprofitable ones die, produce the same observable pattern without coordination?',
    'Documented coordination evidence such as memo networks, funder-coalition minutes, and synchronized campaign launches, contrasted with dispersed giving patterns better explained by independent selection.',
    'Deliberate coordination pushes the classification toward the snare boundary; emergent selection supports tangled_rope with weaker intent attribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_coordination_vs_emergent_selection, empirical, 'Whether the maintenance apparatus reflects strategy or selection.').

omega_variable(
    residual_naturalness_of_exchange,
    'How much residual naturalness survives even under this reading, given that exchange, specialization, and price responsiveness appear in every recorded society, so part of the baseline''s stability may reflect real regularities rather than manufactured closure?',
    'Comparative economic anthropology and history of large-scale non-market provisioning; identify which elements of the baseline replicate across unrelated societies and which appear only where the maintenance apparatus operated.',
    'Higher residual naturalness lowers attributable epsilon and narrows the gap between this reading and the lapsed_alternative_reading; near-zero residual naturalness makes the engineered-closure claim nearly total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_naturalness_of_exchange, conceptual, 'Constructed-versus-genuine share of the naturalization baseline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.21).
narrative_ontology:measurement(mark_tr_t8, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(mark_tr_t16, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(mark_tr_t24, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(mark_tr_t32, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mark_be_t8, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(mark_be_t16, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(mark_be_t24, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(mark_be_t32, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 32, 0.44).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 40, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(mark_su_t8, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(mark_su_t16, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(mark_su_t24, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(mark_su_t32, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 40, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'markets are the natural default' decomposes, per the epsilon-invariance principle, into three structurally distinct claims about how the naturalization persists: this file (beneficiary_maintained_reading) authors epsilon near 0.46 for active, beneficiary-funded closure; lapsed_alternative_reading authors a lower epsilon for passive historical forgetting; hybrid_amnesia_reading authors an intermediate epsilon for lapse-then-capture sequencing. Each is a separate constraint with its own beneficiaries, victims, and type, linked through affects_constraints arrays on both sides. The upstream/downstream structure runs from this reading's evidentiary base into the hybrid's capture phase.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
