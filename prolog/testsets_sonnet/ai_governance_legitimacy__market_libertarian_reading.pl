% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: Market-Libertarian Reading: AI Governance Legitimacy via Voluntary Exchange and Property Rights
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This story authors the market-libertarian reading of the contested
 *   AI-governance-legitimacy kernel: legitimacy flows from voluntary
 *   exchange, property rights, and exit options rather than from centralized
 *   political or ecclesial authority. The reading treats property rights and
 *   contract enforceability as pre-political — a mountain, not a policy
 *   choice — and reads the encyclical's subsidiarity principle as friendly to
 *   decentralization while rejecting its solidarity demands as coercive
 *   overreach by political authority into a domain (economic exchange) that
 *   this reading holds is naturally self-organizing. The reading is generated
 *   cleanly on its own terms: no sibling reading (magisterial_subsidiarity,
 *   technocratic_optimization, democratic_pluralist) is described inside this
 *   file, and no ε is averaged across readings — this file's ε is stable at
 *   approximately 0.20-0.26 across the interval, reflecting the reading's own
 *   low-extraction self-conception.
 *
 * KEY AGENTS:
 *   - ai_startup_founders: Primary beneficiary (organized/arbitrage) — captures upside from unencumbered innovation and low regulatory friction
 *   - venture_investors: Primary beneficiary (institutional/arbitrage) — deploys capital under low-oversight property-rights regime, exits freely across jurisdictions
 *   - high_autonomy_technical_workers: Secondary beneficiary (powerful/mobile) — commands scarce technical skill, genuine market power gives real exit options
 *   - gig_platform_workers: Primary target (powerless/trapped) — depends on algorithmically-mediated platforms with no meaningful exit or bargaining power
 *   - rural_broadband_dependent_communities: Secondary target (powerless/constrained) — coordination failure (infrastructure buildout) unaddressed by market mechanisms alone
 *   - workers_in_monopsony_ai_labor_markets: Primary target (powerless/trapped) — data-labeling and content-moderation workforces facing single-buyer labor markets
 *   - encyclical_drafters: Excluded interlocutor (institutional/analytical) — the solidarity-demanding voice this reading explicitly rejects as illegitimate coercion
 *   - antitrust_regulators: Observer (institutional/analytical) — could assess whether the 'voluntary exchange' this reading names is actually constrained by market concentration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.26).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.22).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "Market-Libertarian Reading: AI Governance Legitimacy via Voluntary Exchange and Property Rights").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, 'c17a6419-1025-4aa2-bc3a-fefb302f17d4').
narrative_ontology:cs_kernel_codification('c17a6419-1025-4aa2-bc3a-fefb302f17d4', distributed).
narrative_ontology:cs_authority_grounding('c17a6419-1025-4aa2-bc3a-fefb302f17d4', distributed).
narrative_ontology:cs_reading_relation('c17a6419-1025-4aa2-bc3a-fefb302f17d4', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('c17a6419-1025-4aa2-bc3a-fefb302f17d4', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('c17a6419-1025-4aa2-bc3a-fefb302f17d4', ai_governance_legitimacy__democratic_pluralist_reading, influences).
narrative_ontology:cs_axiom('c17a6419-1025-4aa2-bc3a-fefb302f17d4', foundational, property_rights_as_prepolitical).
narrative_ontology:cs_axiom_status(property_rights_as_prepolitical, holdable).
narrative_ontology:cs_axiom_grounding('c17a6419-1025-4aa2-bc3a-fefb302f17d4', property_rights_as_prepolitical, deontological).
narrative_ontology:cs_axiom('c17a6419-1025-4aa2-bc3a-fefb302f17d4', foundational, solidarity_mandates_as_illegitimate_coercion).
narrative_ontology:cs_axiom_status(solidarity_mandates_as_illegitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('c17a6419-1025-4aa2-bc3a-fefb302f17d4', solidarity_mandates_as_illegitimate_coercion, conventional).
narrative_ontology:cs_reference_frame('c17a6419-1025-4aa2-bc3a-fefb302f17d4', classical_liberal_natural_rights_framework).
narrative_ontology:cs_drift_state('c17a6419-1025-4aa2-bc3a-fefb302f17d4', contemporary_ai_market_concentration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c17a6419-1025-4aa2-bc3a-fefb302f17d4', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_startup_founders).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technical_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, gig_platform_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, rural_broadband_dependent_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_ai_labor_markets).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, property_rights_as_prepolitical).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, voluntary_exchange_as_legitimating_mechanism).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, exit_over_voice_as_dignity_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds and deploys AI products under a governance regime that treats property rights, contract, and voluntary exchange as the sole legitimate basis for constraint. Can incorporate in favorable jurisdictions, structure IP and liability to minimize exposure, and treat regulatory mandates as costs to be arbitraged around. Genuinely benefits from low collective-mandate friction.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_startup_founders, beneficiary,
    organized, biographical, arbitrage, global).

% Allocates capital across AI ventures under a legal regime that treats property and contract rights as prepolitical bedrock rather than as policy choices subject to solidarity-based redistribution claims. Moves capital freely across borders and sectors in response to regulatory friction, giving genuine, not nominal, exit.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, venture_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Holds scarce technical skills (frontier model research, specialized engineering) that command real bargaining leverage. Can credibly threaten to move between competing employers, which makes the 'voluntary exchange' framing largely accurate for this seat — market competition for their labor genuinely protects their interests without collective mandate.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technical_workers, beneficiary,
    powerful, biographical, mobile, global).

% Performs algorithmically-managed tasks (delivery, data annotation, content moderation) for platforms that set terms unilaterally. Nominally free to leave, but alternative platforms offer functionally identical terms set by the same market dynamics, so 'exit' does not translate into improved terms. Bears the downside of a governance frame that treats their situation as a matter of individual contract rather than collective bargaining or solidarity-based protection.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, gig_platform_workers, payer,
    powerless, immediate, trapped, national).

% Needs infrastructure investment (broadband, compute access, digital literacy programs) to participate meaningfully in an AI-driven economy. This is a coordination-failure problem that individual voluntary exchange systematically underprovides — no single actor's rational self-interest funds regional infrastructure at the necessary scale. The market-libertarian frame treats this as outside its scope rather than as a legitimate governance claim.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, rural_broadband_dependent_communities, payer,
    powerless, generational, constrained, regional).

% Works in data-labeling, content-moderation, or specialized annotation roles where a small number of firms (often a single dominant contractor in a region) set wages and conditions. The market-libertarian reading names this 'voluntary exchange,' but the absence of competing buyers means exit is theoretical rather than real — the reading's own exit-based dignity claim fails structurally for this seat.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_ai_labor_markets, payer,
    powerless, biographical, trapped, national).

% Articulates a solidarity-based claim that economic freedom must be subordinated to a common good defined through political and moral authority beyond individual contract. This reading explicitly rejects that claim as illegitimate coercion, which means the drafters' voice — and the coordination-failure and vulnerable-population concerns it raises — has no standing inside this reading's own legitimacy criteria, even though it remains a live claim held by other parties (see magisterial_subsidiarity_reading).
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, encyclical_drafters, excluded,
    institutional, civilizational, analytical, global).

% Investigates whether the 'voluntary exchange' this reading treats as self-legitimating is in fact constrained by concentrated market power in compute, data, and labor markets for AI. Could, through enforcement action, alter the actual competitiveness this reading's naturalness claim depends on.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, antitrust_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__market_libertarian_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__market_libertarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI development and deployment decisions through decentralized price signals, contract, and voluntary association rather than centralized mandate — genuinely solves the problem of enabling rapid, low-friction innovation without requiring unanimous political agreement on values or priorities before any actor may proceed.
% TRANSFER_FUNCTION: Moves decision-making authority and the resulting economic surplus toward those who hold capital, scarce technical skill, or existing market position, and away from those who would need collective bargaining, regulatory mandate, or redistributive solidarity claims to secure comparable protection or share of surplus.
% ABSENT_VOICES: The encyclical's own solidarity claim — that economic freedom must answer to a common good defined by political/moral authority — is present in the narrative only to be rejected; it is not weighed as a live governance option inside this reading. Workers in monopsony labor markets and communities facing infrastructure coordination failures are described as the affected parties but have no seat in the market-libertarian legitimacy criteria itself, which recognizes only exchanging parties as claimants.
% DISAPPEARANCE_RATIONALE: If this reading's legitimacy claim were simply abandoned overnight, capital-holders and technical elites would experience real rearrangement (loss of a framework that currently privileges their position and forecloses solidarity-based claims on their gains); but workers and coordination-failure-affected communities would likely see any replacement framework (technocratic or magisterial) as itself requiring justification, so whether 'the world rearranges' or 'stays the same' depends heavily on which party is asked — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Protecting nascent AI innovation and individual economic liberty from being captured or throttled by a centralized political or ecclesial authority claiming to define the common good on everyone's behalf, in an era when AI markets were still multi-polar and genuinely competitive.
% FOUNDING_PROBLEM_CORROBORATION: Antitrust regulators and labor economists studying AI-sector market concentration attest that the competitive, multi-polar market conditions this reading's naturalness claim depends on have eroded significantly as compute, data, and distribution have concentrated in a small number of firms — a corroboration from outside the beneficiary set that the founding problem (threat of illegitimate centralized mandate) may now coexist with, or have been partly superseded by, a different problem (illegitimate concentrated private power) that this reading's own framework has no vocabulary to name as coercive.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.26, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.20-0.26) because this is the reading's own self-conception: coordination via price signals and contract, not extraction via mandate. Suppression is likewise low (0.22) because the reading claims no coercive apparatus beyond ordinary contract and property enforcement. Accessibility collapse is moderate-high (0.62) — once the property-rights-as-prepolitical framing is accepted, alternative governance frames (solidarity-based redistribution, democratic mandate) are treated as illegitimate coercion and effectively excluded from the reading's own frame, even though they remain live options for other parties. Resistance is authored moderate (0.55): the reading is actively contested by labor advocates, the Magisterium, and technocratic planners, so it is not a frictionless natural law even on its own terms — it is defended, which is itself evidence against pure naturalness (see the prepolitical_property_naturalness omega).
 *
 * PERSPECTIVAL GAP:
 *   From the entrepreneur/investor seat, this constraint computes as something very close to a genuine mountain: low extraction, high accessibility collapse (the alternative — regulatory mandate — looks obviously worse), low resistance felt personally. From the monopsony-labor-market worker seat, the same structural claim computes very differently: the 'natural' property-rights regime is the mechanism that leaves them without bargaining power, and the 'voluntary exchange' language obscures that their only counterparty is a single employer. This divergence is the point of authoring beneficiaries and victims on a claimed mountain — it is the FSM (false-summit) signature the schema is built to surface, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrepreneurs, investors, and high-skill technical workers sit near the beneficiary end: they hold genuine market power, real exit options (arbitrage across jurisdictions, capital mobility, scarce-skill leverage), and the property-rights regime concentrates gains toward them. Gig platform workers and monopsony-labor-market workers sit near the target end: their 'voluntary exchange' is nominal — a single dominant platform-employer with no comparable competitor makes exit theoretical, not real, so effective extraction runs higher for them than the low base ε suggests once directionality is applied. Rural broadband-dependent communities represent a distinct failure mode: not extraction from an active counterparty but a coordination-failure the market-only frame structurally cannot solve (infrastructure requiring collective investment that individual voluntary exchange cannot aggregate), which is exactly the class of case the encyclical's solidarity principle exists to address and which this reading dismisses as illegitimate coercion by naming it a matter for markets alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem — protecting innovation and individual liberty from centralized mandate-based control of a nascent technology — was live when the encyclical-style solidarity framework was mostly aspirational and AI markets were genuinely competitive and multi-polar. As AI compute, data, and distribution have concentrated into a small number of dominant firms, the coordination-failure and monopsony problems the market-libertarian frame cannot solve have grown, while the reading's rhetorical claim to be defending against coercion has not updated. Whether this constitutes mandatrophy (a founding problem now substantially solved or transformed, with the reading persisting as cover for concentrated market power) or a live problem (genuine ongoing threat of overreaching centralized AI mandates) is exactly the founding_problem_status question this story leaves as contested rather than resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prepolitical_property_naturalness,
    'Are property rights and contract enforceability genuinely pre-political facts that AI governance merely discovers, or are they themselves constructed and continuously re-enforced by state courts, arbitration regimes, and IP law that this reading treats as background rather than as the mechanism doing the work?',
    'Trace the actual enforcement chain behind ''voluntary exchange'' in AI compute/data markets — courts, antitrust non-enforcement, state-backed IP monopolies — and ask whether the exchange would remain voluntary and efficient absent this scaffolding.',
    'If property/contract enforcement is itself a constructed, state-backed apparatus rather than a natural fact, this reading''s mountain claim is a false summit: a constructed constraint dressed as natural law, benefiting capital-holders who did not build the enforcement apparatus but rely on it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prepolitical_property_naturalness, conceptual, 'Whether market-libertarian property/contract legitimacy is genuinely natural or a constructed and enforced regime.').

omega_variable(
    kernel_reading_selection,
    'Is the market-libertarian reading of the AI-governance-legitimacy kernel the uniquely correct interpretation of ''legitimate authority over AI,'' or one of several coherent readings whose selection depends on prior commitments about the nature of political authority and the common good?',
    'Compare the market-libertarian, magisterial-subsidiarity, technocratic-optimization, and democratic-pluralist readings against the same empirical record of AI harm and benefit distribution; examine whether any reading is compelled by evidence alone or whether all four require an antecedent normative commitment.',
    'If no reading is compelled by evidence, the ''mountain'' framing of this reading is a rhetorical move — treating one contestable normative commitment (property-rights-as-prepolitical) as bedrock physics rather than as one live position among several.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether this reading''s mountain status survives comparison against sibling kernel readings.').

omega_variable(
    exit_option_reality_for_labor,
    'Do the ''exit options and competitive markets'' this reading names as dignity-protecting actually exist for workers in concentrated AI labor markets (data labelers, content moderators, compute-constrained researchers), or is the exit option theoretical for capital and largely unavailable for labor?',
    'Empirical labor-market concentration data (HHI) for AI data-labeling and content-moderation sectors; wage and mobility studies for workers dependent on a small number of platform employers.',
    'If exit is asymmetrically available (real for capital, illusory for labor), the reading''s dignity-through-exit claim inverts for the victim population, and effective extraction on trapped workers is understated by the reading''s own framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_reality_for_labor, empirical, 'Whether exit-based dignity protection is symmetrically available across market participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 24, 0.18).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 4, 0.21).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 24, 0.26).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_governance_legitimacy__market_libertarian_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__market_libertarian_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the single natural-language label 'AI governance legitimacy' per the ε-invariance principle. Each reading (market_libertarian, magisterial_subsidiarity, technocratic_optimization, democratic_pluralist) has its own stable ε, its own beneficiary/victim structure, and its own claimed type — they are not measurement-basis variants of one constraint but four structurally distinct constraints sharing a kernel. This reading's low ε and mountain claim contrast with the magisterial_subsidiarity_reading's expected higher ε (framed around identifiable coordination-plus-extraction dynamics of institutional authority) and the technocratic_optimization_reading's expert-authority framing. Network edges here are declared forward to all three siblings for contamination-propagation completeness; each sibling should declare a reciprocal or complementary edge back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__market_libertarian_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
