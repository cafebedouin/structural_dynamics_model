% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default: Hybrid Amnesia Reading (Lapsed Closure, then Beneficial Capture)
 *   domain: political economy / ideology studies / economic history
 *
 * SUMMARY:
 *   Time mapping: t=0 is 1930, t=90 is 2020. Between the 1930s and the 1970s,
 *   Western publics and professions genuinely forgot that markets are
 *   instituted artifacts — designed, regulated, historically contingent — as
 *   the interwar debates over planning, Ordoliberal market construction, and
 *   the Polanyian counter-movement dropped out of curricula and journalism.
 *   Nobody had to defend the forgetting; it was a lapsed closure. From the
 *   1980s onward, actors whose wealth and authority depended on market
 *   arrangements moved from inheriting that amnesia to actively defending it:
 *   think tanks, editorial ecosystems, curriculum capture, and policy taboos
 *   ('structural reform', TINA) rationalized the default after the fact.
 *   Extraction rose accordingly. The claim/metric gap is deliberate and
 *   load-bearing: the story CLAIMS tangled_rope — a genuine shared-baseline
 *   coordination function with increasingly extractive defense wrapped around
 *   it — while the metrics independently describe the rising trajectory
 *   (epsilon 0.20 to 0.45 across the interval); the engine computes per-seat
 *   types from the structural data and the divergence is the datum. This file
 *   is ONE reading of the kernel market_as_natural_default; the siblings
 *   lapsed_alternative_reading and beneficiary_maintained_reading are
 *   separate constraints with their own epsilon, beneficiaries, and
 *   classifications, linked through network.affects_constraints. Family
 *   decomposition follows the epsilon-invariance principle: the colloquial
 *   label 'the market is just natural' conflates three structurally distinct
 *   claims about HOW the naturalization arose and is maintained, and forcing
 *   them into one story would make epsilon observer-relative. KEY AGENTS (by
 *   structural relationship): - incumbent_capital_holders: Primary
 *   beneficiary (powerful/arbitrage) — inherited the cleared discursive
 *   field; funds its defense - large_incumbent_firms: Beneficiary-agenda
 *   participant (institutional/arbitrage) — dominant positions legitimated as
 *   natural outcomes - financial_services_sector: Concentrated beneficiary
 *   (institutional/arbitrage) — collects most directly from naturalized
 *   capital mobility - organized_labor: Principal payer
 *   (organized/constrained) — bears 'flexibility' framed as economic law -
 *   deindustrialized_regional_communities: Principal payer
 *   (powerless/trapped) — absorb closures announced as nature's verdict -
 *   cooperative_and_public_enterprises: Secondary payer
 *   (moderate/constrained) — alternative ownership forms priced as anomalies
 *   - independent_central_banks: Agenda-setter (institutional/constrained) —
 *   administers the technocratic-monetary edge of the framing -
 *   global_south_policy_planners: Excluded voice (moderate/trapped) —
 *   industrial policy ruled illegitimate by conditionality -
 *   heterodox_economists: Excluded voice (moderate/identity_locked) — keepers
 *   of the forgotten memory, professionally marginalized -
 *   polanyian_analytical_observers: Analytical observer
 *   (analytical/analytical) — sees the full construction and its two-stage
 *   history
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.52).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default: Hybrid Amnesia Reading (Lapsed Closure, then Beneficial Capture)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political economy / ideology studies / economic history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '7ba0cd7d-4d11-43e2-863c-1ce92221c096').
narrative_ontology:cs_kernel_codification('7ba0cd7d-4d11-43e2-863c-1ce92221c096', distributed).
narrative_ontology:cs_authority_grounding('7ba0cd7d-4d11-43e2-863c-1ce92221c096', diffuse_epistemic).
narrative_ontology:cs_reading_relation('7ba0cd7d-4d11-43e2-863c-1ce92221c096', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('7ba0cd7d-4d11-43e2-863c-1ce92221c096', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('7ba0cd7d-4d11-43e2-863c-1ce92221c096', foundational, amnesia_precedes_capture).
narrative_ontology:cs_axiom_status(amnesia_precedes_capture, holdable).
narrative_ontology:cs_axiom_grounding('7ba0cd7d-4d11-43e2-863c-1ce92221c096', amnesia_precedes_capture, empirically_contingent).
narrative_ontology:cs_axiom('7ba0cd7d-4d11-43e2-863c-1ce92221c096', foundational, two_stage_periodization).
narrative_ontology:cs_axiom_status(two_stage_periodization, holdable).
narrative_ontology:cs_axiom_grounding('7ba0cd7d-4d11-43e2-863c-1ce92221c096', two_stage_periodization, empirically_contingent).
narrative_ontology:cs_reference_frame('7ba0cd7d-4d11-43e2-863c-1ce92221c096', genuine_forgetting_baseline).
narrative_ontology:cs_drift_state('7ba0cd7d-4d11-43e2-863c-1ce92221c096', contemporary_post_2008_contested_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ba0cd7d-4d11-43e2-863c-1ce92221c096', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, large_incumbent_firms).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, financial_services_sector).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, organized_labor).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, deindustrialized_regional_communities).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, cooperative_and_public_enterprises).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, independent_central_banks).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, market_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, spontaneous_order_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, comparative_advantage_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold wealth whose returns depend on existing market arrangements continuing unexamined. Inherited a public conversation in which market outcomes already read as natural facts rather than policy results, and fund journalists, academics, and advocacy organizations when challenges to that reading emerge. Mobile holdings let them relocate across jurisdictions if any government disturbs the arrangement.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Operate dominant market positions that read as meritocratic outcomes under the default framing. Finance trade associations and policy campaigns that cast regulation as interference with nature; preach competition publicly while consolidating quietly. Can shift registration, supply chains, and lobbying across borders.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, large_incumbent_firms, beneficiary,
    institutional, biographical, arbitrage, global).

% Collects most directly from the arrangement: capital's freedom of movement is treated as a natural right, financialization reads as inevitable modernization, and rescue operations arrive as technical necessities rather than policy choices. Preaches discipline for debtors while relying on official backstops; relocates booking entities freely.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, financial_services_sector, beneficiary,
    institutional, biographical, arbitrage, global).

% Represents workers whose bargaining position eroded while workplace 'flexibility' was described as an economic law rather than a negotiable choice. Proposing structural counterweights came to sound like arguing with gravity, draining recruitment and political ambition. Members and industries cannot relocate; organizing proceeds within the terms the default framing permits.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, organized_labor, payer,
    organized, biographical, constrained, national).

% Absorb plant closures announced as verdicts of impersonal forces. Housing, skills, and family ties anchor residents where the jobs left; there is no realistic exit from the region. They carry diffuse long-run costs in health, cohesion, and opportunity while having no seat in the conversations that classified their livelihoods as obsolete by nature.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, deindustrialized_regional_communities, payer,
    powerless, generational, trapped, regional).

% Run firms owned by workers, municipalities, or the public. Financing channels price their ownership structures as risky deviations from the standard form, and policy designers treat their existence as anomalies needing special justification. They persist at the margins of capital markets that read their way of organizing as unnatural.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, cooperative_and_public_enterprises, payer,
    moderate, generational, constrained, national).

% Administer the arrangement's monetary edge: operational independence and inflation targeting are presented as technocratic necessities discovered rather than designed, and the banks gain prestige and insulation from political challenge under that presentation. Abandoning the mandate would forfeit the standing the framing confers; enforcing it is the job they have become.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, independent_central_banks, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, independent_central_banks, beneficiary).

% Officials in developing economies who pursued industrial policy from the 1980s onward and met loan conditionality, credit-rating penalties, and advisory consensus declaring such policy illegitimate. Caught between domestic development needs and external rules that treat market-led allocation as the only serious option; no exit from the lending system that enforces the terms.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, global_south_policy_planners, excluded,
    moderate, generational, trapped, global).

% Work in traditions — institutionalist, Post-Keynesian, Polanyian — that kept alive the memory that markets are instituted. Marginalized from leading journals, major departments, and advisory circles; career advancement runs through demonstrating command of the default toolkit. Leaving the tradition would dissolve the research program and professional community that constitute their working identity.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, heterodox_economists, excluded,
    moderate, generational, identity_locked, continental).

% Scholars and commentators outside policy-making who study how the default framing was built, forgotten, and later defended. They neither collect nor pay under the arrangement; they publish analyses of its construction and maintain the historical record its defenders would prefer stayed forgotten.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, polanyian_analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives investors, firms, households, and officials a shared baseline for how goods and capital move, sparing every decision a relitigation of economic first principles; under that shared default, price signals coordinate decentralized plans at scale.
% TRANSFER_FUNCTION: Moves discursive legitimacy and policy option-space away from anyone proposing non-default arrangements and toward defenders of incumbent positions; materially, moves bargaining power from labor, deindustrialized regions, and alternative enterprise forms toward capital holders by making structural renegotiation look unnatural.
% ABSENT_VOICES: Heterodox economists, cooperative and municipal enterprise movements, and Global South industrial planners object from outside the room — in marginalized journals, underfinanced sectors, and conditionality-bound governments respectively. Future generations bearing climate costs also have no seat: the default framing books their losses as externalities.
% DISAPPEARANCE_RATIONALE: If the default framing vanished overnight — if every actor suddenly saw market arrangements as instituted and revisable — industrial policy would normalize within years, antitrust and ownership experiments would multiply, capital taxation and mobility rules would reopen everywhere, and the funded defense industry would lose its object. Arrangements across trade, finance, labor, and regional policy are organized around the framing's continued acceptance.
% FOUNDING_PROBLEM: No deliberate founder: the framing accreted into the vacuum left when the interwar debates over market construction, planning, and countermovements fell out of professional and public memory; retrospectively it served to legitimize postwar market expansion and, later, to shield liberalized arrangements from democratic revision.
% FOUNDING_PROBLEM_CORROBORATION: Historians of economic thought and science-and-technology-studies scholars, working outside every benefiting party, attest that the original closure was lapsed rather than engineered and that today's narrative serves functions its origin never had; the growth of funded defense organizations after the 1970s, documented in foundation and advocacy budgets, independently corroborates the shift from inherited amnesia to active maintenance.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.45: moderate, because the underlying price-and-baseline coordination solves real problems while a growing layer of rent defense rides on top of it — the two-stage trajectory (trough 0.12 at embedded-liberalism peak, climb after the 1970s) is the signature of the hybrid reading. Suppression is 0.52 and structural: the enforcement machinery is epistemic (advocacy funding, journal gatekeeping, policy taboo enforcement), not physical; suppression is authored as a raw structural property and the engine — not this story — scales it by directionality and scope. Theater ratio 0.42 and rising: the rhetoric-practice gap widened in stage 2 as laissez-faire sermonizing coexisted with bailouts, agricultural subsidies, and industrial policy conducted apologetically — the defense increasingly performs markets-as-nature while administering markets-by-design. Accessibility_collapse 0.48: alternatives remain visible (cooperatives exist, industrial-policy manuals exist) but are systematically delegitimized as unnatural — partial collapse, characteristic of a maintained construct rather than a natural law. Resistance 0.55: heterodox revival, antitrust renewal, post-2008 and post-pandemic industrial-policy normalization meet the framing head-on. Coordination type: identity_coordination — the framing's dominant function is boundary maintenance over who counts as economically serious, which is why its floor is the conservative 0.08 and why coupling that concentrates costs on the exit-less deserves scrutiny despite the complexity offset. Measurements run on one shared grid (every tracked metric authored at every point 0..90 by 15) so no row borrows another metric's end-state. Receipt surface: gains demonstrably accrue to incumbent capital holders as the class whose asset positions the framing shields (the financial sector takes a concentrated substream); fixing is prohibitive because the remedy — re-teaching constructedness against funded denial — costs any single fixer more than it can capture.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the beneficiary seats the framing is simply realism: their asset returns and market positions look like weather, and their funded defense looks like education. From the trapped payer seats the same structure operates as gaslighting-by-commonsense — plant closures recoded as nature's verdict, bargaining losses recoded as physics — and effective extraction is amplified by their immobility and the framing's global scope. The central-bank agenda-setter seat sits near symmetric: it administers the technocratic edge and collects prestige-legitimacy, but pays little and could not easily abandon its own mandate. The excluded voices carry high directional exposure with zero agenda access; their exclusion is not incidental but the very surface the enforcement maintains. Note the same-level lateral split inside one nominal profession: mainstream economists and heterodox economists hold similar credentials and formal standing, yet their exit options differ sharply — the heterodox are identity_locked (leaving dissolves the research program and community constituting their working identity), so identical-seeming actors experience opposite sides of the arrangement depending on constraint-specific exit structure, not global power.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: incumbent_capital_holders, large_incumbent_firms, and financial_services_sector sit near the beneficiary end (low d), with arbitrage-grade exit damping their effective burden further — capital mobility is precisely what the framing naturalizes. Organized_labor, deindustrialized_regional_communities, and cooperative_and_public_enterprises sit near the target end (high d), amplified by constrained or trapped exits and, for the communities, regional scope with global-scale causes. Independent_central_banks derive near symmetric (administer without collecting much; secondary beneficiary nudge downward from 0.5). The excluded seats — global_south_policy_planners, heterodox_economists — derive high d with no compensating agenda access. No directionality overrides are needed: the structural data (roles, exits, scope) already yields the correct relationships, and the engine owns the arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead: no deliberate architect existed, and the conditions that produced the original lapse (generational turnover, archival neglect of the interwar debates) are complete — nothing ongoing sustains the innocent phase. Yet the arrangement persists and the world still rearranges around it, because beneficiaries inherited and then weaponized the residue. The dead-problem-times-world-rearranges mismatch is the capture signal this reading exists to document. Classification discipline prevents both adjacent errors: labeling the whole arrangement rope would miss the funded defense machinery that grew after the 1970s; labeling it snare would erase the genuine coordination the shared baseline still performs for ordinary investment and exchange decisions. Tangled_rope holds both truths: coordination function intact, extraction layered on and actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates the hybrid_amnesia_reading of the kernel market_as_natural_default: do the structural facts support the hybrid periodization (genuine forgetting, then weaponization) over the sibling readings lapsed_alternative_reading and beneficiary_maintained_reading?',
    'Cross-reading comparison of the three compiled family stories, anchored by archival evidence on when organized defense of the default framing actually began; the sibling with the best-supported mechanism attribution becomes the reference account.',
    'If pure forgetting explains persistence, this reading collapses toward the lapsed_alternative account and the arrangement reads as inertial rather than defended; if active defense ran throughout, it collapses toward beneficiary_maintained and the arrangement reads as fully captured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which sibling reading of the market-as-natural-default kernel the structural evidence favors.').

omega_variable(
    weaponization_onset_timing,
    'When did genuine forgetting end and organized defense begin? The Mont Pelerin Society (1947) predates the assumed 1980 inflection — was stage 1 shorter than the hybrid reading''s periodization claims?',
    'Funding trails, think-tank founding and budget chronologies, citation-network analysis of when market-design literature stopped being taught versus when it started being attacked.',
    'An earlier organized-defense onset shortens the innocent phase, raises attributed extraction across the interval, and shifts weight toward the beneficiary_maintained sibling reading; a late onset confirms the hybrid trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weaponization_onset_timing, empirical, 'Timing of the transition from inherited amnesia to funded defensive rationalization.').

omega_variable(
    coordination_extraction_separability,
    'Is the expectation-coordination function of the market-default framing separable from its extraction-shielding function, or does shielding depend on the same shared-baseline mechanism?',
    'Comparative cases: jurisdictions or periods where the shared market baseline persisted without concentrated incumbents, or where incumbents thrived without the naturalization narrative.',
    'If separable, this constraint decomposes into a coordination component and an extraction component (two stories per epsilon-invariance); if inseparable, the tangled_rope classification stands as one structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the framing''s coordination and extraction components are structurally separable.').

omega_variable(
    forgotten_alternatives_viability,
    'Were the forgotten alternatives (planning, cooperation, public ownership) ever viable at scale, or is their recovery partly romantic memory that inflates the estimated harm of their exclusion?',
    'Comparative institutional history of surviving and scaled alternative enterprises (Mondragon-type cooperatives, municipal utilities, developmental-state planning records).',
    'If the alternatives were never scalable, the harm attributable to their foreclosure falls and the arrangement''s extraction estimate drops; if viable, the foreclosure is a substantive loss and extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forgotten_alternatives_viability, empirical, 'Viability of the alternatives the default framing foreclosed.').

omega_variable(
    internalized_fatalism_share,
    'How much of the measured suppression is structural (funded defense machinery, curricula, editorial gatekeeping) versus internalized (there-is-no-alternative fatalism that persists even where material barriers have loosened)?',
    'Post-2008 and post-pandemic attitude and policy-imagination surveys tracking whether option-space widens where enforcement spending did not visibly change.',
    'A high internalized share means suppression travels with the population after the enforcement machinery weakens — the arrangement could outlive its own defense budget, changing persistence predictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_fatalism_share, empirical, 'Structural versus internalized share of the suppression keeping alternatives closed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mkt_nat_hybrid_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t0, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t15, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t15, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t30, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t30, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t45, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 45, 0.12).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t45, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t60, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t60, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t75, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t75, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t90, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 90, 0.42).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(mkt_nat_hybrid_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t0, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t15, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 15, 0.13).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t15, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t30, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t30, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t45, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 45, 0.17).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t45, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t60, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t60, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t75, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 75, 0.4).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t75, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t90, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 90, 0.45).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(mkt_nat_hybrid_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t0, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t15, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 15, 0.09).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t15, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t30, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 30, 0.07).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t30, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t45, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 45, 0.12).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t45, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t60, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t60, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t75, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 75, 0.44).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t75, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t90, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 90, 0.52).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (market_as_natural_default), three readings, three files. Epsilon-invariance decomposition: the colloquial claim 'the market is the natural default' conflates three structurally distinct accounts of how the naturalization arose and persists. lapsed_alternative_reading authors the lowest epsilon (pure historical forgetting, no capturer — inertial profile); beneficiary_maintained_reading authors the highest (organized defense throughout — captured profile); this hybrid reading authors the middle with a dynamic trajectory (0.20 rising to 0.45), because it holds that forgetting preceded and enabled the capture. Upstream/downstream: the lapsed account supplies this reading's stage 1; the beneficiary-maintained account rivals this reading's stage 2. Each file links the other two via network.affects_constraints so contamination and purity analysis propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
