% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Market Naturalization as Beneficiary-Maintained Closure
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   This constraint instantiates the 'beneficiary-maintained' reading of the
 *   contested 'market as natural default' kernel: the claim that market
 *   allocation's apparent naturalness is not a historical accident of
 *   forgotten alternatives but an actively engineered and continuously
 *   re-produced closure, funded and maintained by identifiable beneficiaries
 *   (finance, large corporate shareholders, and the think tanks they fund)
 *   because it is profitable for them to keep the arrangement's specific
 *   distributional consequences off the table of legitimate political
 *   contest. This reading is distinct from, and shares the same kernel text
 *   with, the 'lapsed alternative' reading (which attributes naturalization
 *   to historical amnesia rather than active maintenance) and the 'hybrid
 *   amnesia' reading (which sees an initial lapse subsequently colonized by
 *   beneficiary capture). Each reading has its own ε, its own
 *   beneficiary/victim structure, and its own classification — this file
 *   authors only the beneficiary-maintained claim.
 *
 * KEY AGENTS:
 *   - financial_sector_incumbents: Primary beneficiary (institutional/arbitrage) — collects returns the naturalized arrangement legitimates
 *   - market_apologetics_thinktanks: Agenda-setter (organized/constrained) — produces and circulates the naturalizing intellectual apparatus, funded by beneficiaries
 *   - precarious_workers: Primary target (powerless/trapped) — bears the foreclosed-alternative cost with no standing to contest the framing
 *   - cooperative_and_mutualist_sector: Secondary target (moderate/constrained) — viable alternative structurally disadvantaged by the naturalized baseline
 *   - economic_historians: Analytical observer (analytical/analytical) — documents the engineered-closure history from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.47).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.58).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization as Beneficiary-Maintained Closure").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, '778d21a9-99d3-45aa-84d2-25967d2ef3c9').
narrative_ontology:cs_kernel_codification('778d21a9-99d3-45aa-84d2-25967d2ef3c9', distributed).
narrative_ontology:cs_authority_grounding('778d21a9-99d3-45aa-84d2-25967d2ef3c9', extraction).
narrative_ontology:cs_interpretation_layer_present('778d21a9-99d3-45aa-84d2-25967d2ef3c9').
narrative_ontology:cs_reading_relation('778d21a9-99d3-45aa-84d2-25967d2ef3c9', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('778d21a9-99d3-45aa-84d2-25967d2ef3c9', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('778d21a9-99d3-45aa-84d2-25967d2ef3c9', foundational, naturalization_requires_continuous_active_maintenance).
narrative_ontology:cs_axiom_status(naturalization_requires_continuous_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('778d21a9-99d3-45aa-84d2-25967d2ef3c9', naturalization_requires_continuous_active_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('778d21a9-99d3-45aa-84d2-25967d2ef3c9', foundational, identifiable_beneficiary_class_funds_the_closure).
narrative_ontology:cs_axiom_status(identifiable_beneficiary_class_funds_the_closure, holdable).
narrative_ontology:cs_axiom_grounding('778d21a9-99d3-45aa-84d2-25967d2ef3c9', identifiable_beneficiary_class_funds_the_closure, empirically_contingent).
narrative_ontology:cs_reference_frame('778d21a9-99d3-45aa-84d2-25967d2ef3c9', postwar_planning_orthodoxy_contest).
narrative_ontology:cs_drift_state('778d21a9-99d3-45aa-84d2-25967d2ef3c9', contemporary_neoliberal_consensus, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('778d21a9-99d3-45aa-84d2-25967d2ef3c9', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, large_corporate_shareholders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, market_apologetics_thinktanks).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, precarious_workers).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, cooperative_and_mutualist_sector).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, policy_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold dominant capital allocation positions built under the current market arrangement. Fund think tanks, sponsor academic chairs, and place former staff into regulatory bodies to keep the market's naturalness as background assumption rather than contested policy choice. Have the resources to move capital across jurisdictions if any single naturalization campaign fails, but the campaign itself is a continuous, funded activity, not a one-time historical accident.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents, agenda_setter).

% Receive the returns that flow from treating current allocation and ownership arrangements as the default state of nature rather than one of several viable institutional designs. Lobby to keep alternative ownership models (cooperatives, public options, sectoral bargaining) off legislative agendas by framing them as utopian or economically illiterate.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, large_corporate_shareholders, beneficiary,
    organized, generational, arbitrage, global).

% Produce and circulate the intellectual apparatus — textbook framing, op-eds, model curricula — that presents market allocation as the physics-like default and any deviation as an intervention requiring special justification. Funded substantially by the beneficiary class; their institutional survival depends on continued production of this framing, which locks their own exit options even as they administer the closure for others.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, market_apologetics_thinktanks, agenda_setter,
    organized, biographical, constrained, national).

% Bear the downside of an arrangement presented to them as simply how the economy works, foreclosing the sense that wage-setting, employment protection, or ownership structure could be otherwise. Lack the resources or standing to fund a competing narrative; individual attempts to exit into cooperative or alternative arrangements face financing and market-access barriers actively maintained by the beneficiary class.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, precarious_workers, payer,
    powerless, immediate, trapped, national).

% Operates viable alternative ownership and allocation models but faces systematically higher financing costs, weaker legal recognition, and exclusion from favorable tax and regulatory treatment relative to conventional corporate forms — treatment justified by the framing that the conventional form is simply 'the market' and alternatives are subsidized deviations from it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, cooperative_and_mutualist_sector, payer,
    moderate, biographical, constrained, regional).

% Propose institutional alternatives (sectoral bargaining, public banking, worker ownership incentives) and are consistently required to prove these alternatives against an implicit 'natural' baseline that is never itself required to justify its naturalness. Rarely granted equal standing in policy debate framing; their proposals are treated as the burden-bearing side of the argument by design.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, policy_reformers, excluded,
    moderate, biographical, constrained, national).

% Document the specific historical episodes — postwar reconstruction of neoclassical orthodoxy, corporate-funded economics departments, coordinated media campaigns following market crises — in which naturalization was actively produced and re-produced rather than simply inherited through forgetting. Their scholarship is the primary corroborating evidence for this reading, distinct from the beneficiaries' own self-narration.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents).
narrative_ontology:fixing_cost_class(market_as_natural_default__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine coordination function underneath the closure: shared expectations about price signals, property rights, and contract enforcement do reduce transaction costs and allow decentralized economic actors to plan. This reading does not deny that function exists — it asserts that the function has been used as cover for maintaining a much more specific, contestable set of ownership and distribution arrangements as if they were entailed by the coordination function itself.
% TRANSFER_FUNCTION: Moves legitimacy and policy-agenda space from institutional alternatives (cooperative ownership, sectoral bargaining, public allocation mechanisms) to the incumbent market arrangement, and moves the associated economic returns and the burden of justification onto workers, alternative-sector actors, and reformers who must argue against a baseline treated as need-no-defense.
% ABSENT_VOICES: Cooperative-sector organizers and heterodox economists are largely absent from mainstream policy fora and business-school curricula; when present, they are structurally positioned as the side bearing the burden of proof rather than as co-equal participants in a genuine debate about institutional design.
% DISAPPEARANCE_RATIONALE: If the active naturalization apparatus (funded think tanks, curriculum capture, media framing operations) disappeared overnight, the underlying coordination function of markets could persist in some form, but the specific ownership and distribution arrangements currently treated as inevitable would immediately become contestable policy questions — financing terms for cooperatives would shift, curriculum framing would open to alternatives, and legislative agendas would likely admit proposals currently excluded by the naturalized baseline.
% FOUNDING_PROBLEM: In the mid-20th century, following depression-era interventionism and wartime planning, market-oriented actors faced a genuine legitimacy problem: state planning and mixed-economy models had strong intellectual and popular standing, and market allocation needed active intellectual and institutional defense to remain a live policy option at all.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the beneficiary class (business history and history-of-economic-thought scholarship documenting the funding of economics departments, chambers of commerce PR campaigns, and post-1970s think tank formation) attest that the original defensive posture succeeded decades ago and that the apparatus has since shifted from defending a contested position to actively suppressing the memory that it was ever contested. The beneficiary class itself frames the arrangement as simply descriptive of how economies work, which is precisely the naturalization this reading identifies as engineered rather than discovered.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.47, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.47, within the expected moderate-to-high band for this reading: the coordination function (price coordination, contract enforcement) is real and non-trivial, so extraction is not total, but the closure captures a durable and growing share of policy-agenda space and financing terms for the beneficiary class specifically. Suppression (0.58) exceeds extractiveness because the mechanism operates less through direct coercion of any single actor and more through systematic exclusion of alternatives from legitimate consideration — curriculum capture, funding asymmetries, media framing — which the metric treats as raw structural fact, unscaled by power or scope per the framework's rule. Theater ratio (0.42) reflects that a substantial share of the apparatus's current activity (op-eds restating settled orthodoxy, ceremonial invocations of 'the market' in policy debate) is performative maintenance of a closure achieved decades ago, rather than fresh persuasive work — consistent with the founding_problem_status of 'dead.' Accessibility collapse (0.62) and resistance (0.55) reflect a constructed constraint under active contest, not a natural law: alternatives have not vanished (cooperative sector persists, heterodox economics persists) but access to legitimate policy standing has substantially collapsed, and resistance remains real and organized rather than negligible.
 *
 * PERSPECTIVAL GAP:
 *   From the financial-incumbent seat, the arrangement reads as simple background fact requiring no active defense — 'this is just how markets work.' From the precarious-worker and cooperative-sector seats, the same arrangement reads as an actively defended and continuously reinforced closure that forecloses live alternatives. The engine should compute a tangled-rope classification from the beneficiary seat (or possibly rope, if the seat's own metrics dominate) diverging sharply from a snare-like reading computed from the payer seats — this divergence is the data point the beneficiary-maintained reading exists to capture, distinguishing it from the lapsed-alternative sibling where no active maintenance would be visible from any seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial incumbents and large shareholders sit near the full-beneficiary end of directionality: they fund the closure, collect its returns, and retain arbitrage-grade exit if any particular jurisdiction's naturalization narrative weakens. Think tanks occupy an intermediate position — they administer the closure (agenda_setter) but their own institutional survival is funded and thus partially locked to the beneficiary class's continued willingness to pay, constraining their exit despite their organized power. Precarious workers sit at the full-target end: trapped exit, immediate time horizon, no resources to fund a competing framing. The cooperative sector is a target with somewhat more mobility (constrained rather than trapped) because it retains institutional form and legal existence, just on disadvantaged terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defending market allocation as a live policy option against strong mid-century planning orthodoxy) is dead — market allocation has been dominant and largely unquestioned in mainstream policy discourse for decades. Yet the defensive apparatus (funded think tanks, curriculum production, media framing) persists and has, per the temporal measurements, intensified rather than atrophied. This is the signature of mandatrophy: a mandate whose original justifying problem has been resolved for so long that continued 'defense' activity is better read as rent-protection than persuasion. Classifying this as tangled_rope (not simple snare) preserves the fact that a genuine coordination function underlies the arrangement — collapsing it to pure snare would deny that markets coordinate anything real; classifying it as rope would deny that the closure is actively, asymmetrically maintained for identifiable beneficiaries against payers who cannot exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_maintenance_vs_inherited_lapse,
    'Is the naturalization of market allocation actively and continuously re-produced by identifiable beneficiaries, or is it substantially inherited from a historical forgetting of alternatives that beneficiaries merely benefit from without actively maintaining?',
    'Archival and funding-flow analysis: trace whether think tank output, curriculum design, and media framing activity correlates with continuous, ongoing beneficiary funding (supporting this reading) versus whether the apparatus could be defunded today with no measurable change in naturalization strength over a multi-decade horizon (supporting the lapsed-alternative sibling).',
    'If funding and output are shown to track a genuinely ongoing maintenance campaign, this reading''s tangled_rope classification and moderate-to-high extractiveness stand. If the apparatus is shown to be causally inert relative to naturalization strength (i.e., the closure persists independent of current funding), the lapsed_alternative_reading better describes the phenomenon and this reading''s beneficiary-class attribution overstates active agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_vs_inherited_lapse, empirical, 'Whether naturalization is actively maintained by beneficiaries or is an inert historical residue they merely benefit from.').

omega_variable(
    coordination_extraction_separability_market_naturalization,
    'Is the genuine price-coordination function of markets structurally separable from the specific ownership/distribution arrangements the naturalization narrative also legitimates, such that the coordination function could persist under a different, less naturalized institutional design?',
    'Comparative institutional analysis of jurisdictions or sectors with strong price-coordination markets but weaker naturalization narratives (e.g., co-determination economies, strong cooperative sectors with market pricing) — if coordination function persists undiminished, separability is supported.',
    'If separable, the naturalization apparatus''s defense of the specific ownership arrangement is pure rent-protection riding on the coordination function''s legitimacy; if inseparable, some of the measured extraction is properly attributable to a genuine (if contestable) coordination requirement rather than beneficiary capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability_market_naturalization, conceptual, 'Whether market coordination and the naturalized ownership structure are separable, or whether the naturalization defends something genuinely load-bearing.').

omega_variable(
    kernel_framing_choice_beneficiary_vs_lapse,
    'Given that the same surface phenomenon (market allocation presenting as natural default) admits at least three structurally distinct readings (active beneficiary maintenance, historical lapse, hybrid), what evidence or context specifically warrants selecting the beneficiary-maintained framing for this file rather than treating the phenomenon as primarily amnesia-driven?',
    'This omega documents CS-framing under-determination per the authoring guidance: the choice was guided by the documented existence of funded, identifiable institutional actors (named think tanks, corporate-funded economics departments) engaged in continuous production activity, which is affirmative evidence of active maintenance rather than mere absence-of-memory. A reader adopting the lapsed_alternative_reading instead would need to show this activity is either causally inert or a much smaller share of the naturalization''s actual force than the historical-forgetting mechanism.',
    'Adopting the lapsed_alternative framing instead would lower authored extractiveness substantially (no identifiable beneficiary class actively capturing the closure) and would likely shift claimed_type toward rope or piton (degraded memory rather than defended extraction) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_beneficiary_vs_lapse, conceptual, 'Documents why the beneficiary-maintained framing was selected among the three kernel readings and what would change under the sibling framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mark_tr_t10, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(mark_tr_t20, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(mark_tr_t50, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(mark_tr_t60, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mark_be_t10, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(mark_be_t20, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(mark_be_t50, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement(mark_be_t60, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 60, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mark_su_t10, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(mark_su_t20, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(mark_su_t50, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(mark_su_t60, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__beneficiary_maintained_reading, 0.12).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the market_as_natural_default kernel, decomposed per the ε-invariance principle because the natural-language label 'market naturalization' conflates structurally distinct claims about mechanism (active maintenance vs. historical amnesia vs. hybrid capture) with correspondingly different ε values. This file (beneficiary_maintained_reading, ε=0.47) links to lapsed_alternative_reading (expected lower ε, closer to piton/mountain given no active maintainer) and hybrid_amnesia_reading (expected intermediate ε, initial lapse subsequently colonized by capture). Each sibling should independently link back to this file in its own network.affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
