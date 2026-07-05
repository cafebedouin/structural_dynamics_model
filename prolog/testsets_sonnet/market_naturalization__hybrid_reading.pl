% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance as Naturalized Order — Hybrid Reading (Lapsed Alternatives + Active Maintenance)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the market_naturalization
 *   kernel: market dominance is neither a pure lapsed closure (no active
 *   maintenance, per the lapsed_alternative_reading) nor a purely
 *   actively-defended structure (per the beneficiary_maintained_reading), but
 *   a genuine composite where some barriers to entry are residual historical
 *   sediment — nobody defends them because nobody needs to — and other
 *   barriers are freshly and continuously constructed through litigation,
 *   exclusivity, and capture. The distinguishing empirical claim of this
 *   reading is that BOTH mechanisms are simultaneously present and
 *   load-bearing, and that their proportions vary by sub-market and cannot be
 *   collapsed into either sibling's single-mechanism account. This reading's
 *   ε (0.52, moderate) sits deliberately between what a pure-lapsed reading
 *   would show (very low, near-mountain) and what a pure-maintained reading
 *   would show (high, clearly snare-adjacent) — this is not fence-sitting but
 *   the structurally correct value for a constraint whose actual mechanism is
 *   mixed.
 *
 * KEY AGENTS:
 *   - incumbent_market_leaders: agenda_setter/beneficiary (institutional/arbitrage) — benefit from both inherited and actively defended advantage
 *   - concentrated_capital_holders: beneficiary (institutional/arbitrage) — collect regardless of which mechanism operates
 *   - would_be_market_entrants: payer (moderate/constrained) — cannot distinguish lapsed from maintained barriers
 *   - downstream_consumers: payer (powerless/constrained) — bear price effects of reduced competition of mixed origin
 *   - displaced_worker_communities: payer (powerless/trapped) — regional dependency partly inherited, partly reinforced
 *   - antitrust_regulators: observer (institutional/analytical) — must diagnose mechanism type case by case
 *   - economic_historians: observer (analytical/analytical) — attempt the empirical separation the hybrid makes difficult
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.52).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.48).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance as Naturalized Order — Hybrid Reading (Lapsed Alternatives + Active Maintenance)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'eaaf62ba-8c87-487b-98f9-e542668470de').
narrative_ontology:cs_kernel_codification('eaaf62ba-8c87-487b-98f9-e542668470de', distributed).
narrative_ontology:cs_authority_grounding('eaaf62ba-8c87-487b-98f9-e542668470de', distributed).
narrative_ontology:cs_reading_relation('eaaf62ba-8c87-487b-98f9-e542668470de', market_naturalization__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('eaaf62ba-8c87-487b-98f9-e542668470de', market_naturalization__beneficiary_maintained_reading, influences).
narrative_ontology:cs_axiom('eaaf62ba-8c87-487b-98f9-e542668470de', foundational, dominance_mechanisms_are_mixed_and_coextensive).
narrative_ontology:cs_axiom_status(dominance_mechanisms_are_mixed_and_coextensive, holdable).
narrative_ontology:cs_axiom_grounding('eaaf62ba-8c87-487b-98f9-e542668470de', dominance_mechanisms_are_mixed_and_coextensive, empirically_contingent).
narrative_ontology:cs_axiom('eaaf62ba-8c87-487b-98f9-e542668470de', secondary, mechanism_diagnosis_must_be_submarket_specific).
narrative_ontology:cs_axiom_status(mechanism_diagnosis_must_be_submarket_specific, holdable).
narrative_ontology:cs_axiom_grounding('eaaf62ba-8c87-487b-98f9-e542668470de', mechanism_diagnosis_must_be_submarket_specific, instrumental).
narrative_ontology:cs_reference_frame('eaaf62ba-8c87-487b-98f9-e542668470de', post_consolidation_market_structure).
narrative_ontology:cs_drift_state('eaaf62ba-8c87-487b-98f9-e542668470de', contemporary_antitrust_scrutiny_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eaaf62ba-8c87-487b-98f9-e542668470de', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_market_leaders).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, concentrated_capital_holders).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, would_be_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, downstream_consumers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, displaced_worker_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy dominant market position built partly from historically contingent advantages (early scale, legacy infrastructure, network effects that consolidated decades ago) and partly maintained through ongoing lobbying, exclusive contracts, and litigation against entrants. They benefit from both the sedimented past and the active present, and cannot easily separate which parts of their advantage are 'natural' versus defended.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_market_leaders, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, incumbent_market_leaders, beneficiary).

% Hold equity and debt claims on dominant firms; benefit from the persistence of market structure whether it is actively defended or merely inertial. Their capital can move to other dominant positions elsewhere if this one erodes, so they experience the constraint as low-cost and low-risk regardless of its internal composition.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, concentrated_capital_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Face a market where some barriers are genuinely residual (customers habituated to the incumbent, standards built around its products) and other barriers are freshly erected or renewed (exclusive supplier agreements, targeted litigation, regulatory capture). They cannot tell in advance which obstacle is which, so every barrier reads as equally binding even though the underlying mechanism differs — this is the hybrid's distinctive cost.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, would_be_market_entrants, payer,
    moderate, biographical, constrained, national).

% Pay prices set by a market with reduced competitive pressure. Some of this reduced pressure comes from switching costs and habituation nobody is actively defending; some comes from deliberate suppression of alternatives (exclusivity deals, predatory responses to new entrants). Consumers cannot distinguish and simply experience higher prices and fewer choices.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, downstream_consumers, payer,
    powerless, biographical, constrained, national).

% Regions built around now-dominant firms or their suppliers bear concentrated employment risk. Some of the concentration is a lapsed historical accident of where the industry settled; some is actively reinforced by continued subsidy and site-selection leverage the incumbent wields over local governments. Workers cannot exit the regional dependency even when they recognize the asymmetry.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, displaced_worker_communities, payer,
    powerless, generational, trapped, regional).

% Investigate market concentration and must decide, case by case, whether a given barrier is inertial residue (not a violation) or actively maintained exclusion (potentially a violation). Their remedies differ sharply depending on which diagnosis they reach, and the hybrid structure of the constraint is exactly what makes this diagnosis difficult and contestable.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, antitrust_regulators, observer,
    institutional, generational, analytical, national).

% Study the trajectory of market concentration over decades, attempting to separate which structural features are inherited path-dependence and which are actively renewed rent extraction. Their work is cited by both incumbents (to claim naturalness) and challengers (to claim ongoing capture).
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, incumbent_market_leaders).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized infrastructure, supplier relationships, and customer expectations that consolidated around the dominant firm reduce transaction costs across the market — this residual coordination benefit is real, is not manufactured, and would not disappear merely because active maintenance also exists alongside it.
% TRANSFER_FUNCTION: Moves surplus from entrants, consumers, and dependent regional workforces to incumbent firms and their capital holders — but through two distinct channels operating simultaneously: passive extraction from unchallenged historical advantage, and active extraction from renewed barrier construction (litigation, exclusivity, capture).
% ABSENT_VOICES: Smaller regional governments dependent on incumbent tax revenue and employment are structurally unable to challenge the incumbent's site-selection leverage without risking the jobs they depend on; genuinely novel entrants who never attempted entry because the hybrid barrier looked uniformly insurmountable are invisible to any dataset — the market shows no trace of the competition that never tried.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the actively-maintained component (litigation threats, exclusive contracts, capture) would release immediately and entrants would test the market — but the lapsed component (habituation, sunk infrastructure, standards lock-in) would persist as genuine friction even absent any defender. Parties disagree sharply on the ratio: incumbents claim the lapsed share dominates (so little would change); challengers claim the maintained share dominates (so much would change). The hybrid's defining feature is that this ratio is empirically unresolved.
% FOUNDING_PROBLEM: Early market consolidation solved a genuine coordination problem: fragmented suppliers, incompatible standards, and unreliable quality created transaction costs that a dominant, standard-setting firm reduced by absorbing them.
% FOUNDING_PROBLEM_CORROBORATION: Antitrust regulators and independent economic historians attest that the original coordination problem was substantially real but has been at least partially solved for decades in most sub-markets studied; incumbent firms and their capital holders attest the coordination problem remains live (citing quality assurance and standards maintenance). No party outside the incumbent's own orbit corroborates that the FULL scope of current market power is still justified by live coordination need — independent analysis instead finds a mixed picture consistent with the hybrid reading itself.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) and suppression (0.48) are both moderate and rising over the interval, reflecting a constraint where active maintenance has been layered onto and increasingly dominates an originally more inertial structure — the temporal trend itself is evidence for the hybrid reading over the pure-lapsed reading, since a truly lapsed closure would show flat or declining suppression, not a rising one. Theater ratio (0.44) is elevated because a substantial share of the incumbent's public justification ('we maintain quality standards,' 'we ensure supply reliability') has become performative cover for barriers that no longer serve the coordination function they once did — this is exactly the mechanism by which a lapsed element gets quietly converted into an actively maintained one under a continuous cover story.
 *
 * PERSPECTIVAL GAP:
 *   Incumbents genuinely experience much of their position as inherited and unremarkable — 'this is just how the market settled' — while entrants and regulators experience the same position as freshly and deliberately defended in the specific moments they try to compete. Both experiences are partially correct, and the hybrid classification exists precisely to hold both without collapsing into either party's self-serving simplification.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent firms and capital holders sit near the beneficiary end regardless of mechanism — they collect whether the barrier standing between them and entrants is inertial or defended. Would-be entrants, consumers, and displaced workers sit toward the target end, but with an important qualification the hybrid reading makes visible: the SAME nominal barrier height produces different remedies depending on mechanism, so their effective directionality is harder to correct with policy than either sibling reading alone would suggest — you cannot dissolve a hybrid barrier with a single lever.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk here runs in both directions: reading the constraint as purely lapsed lets active maintenance escape scrutiny (nothing to investigate, it's just history); reading it as purely maintained treats genuinely inert historical sediment as if it required active dismantling, wasting enforcement resources on barriers that would erode on their own. The hybrid classification forces regulators and historians to do the harder, mechanism-specific diagnostic work the sibling readings let them skip.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_proportion_ambiguity,
    'In any given sub-market, what proportion of the observed market dominance barrier is genuinely lapsed (inertial, undefended) versus actively maintained (defended through current-period action)?',
    'Natural experiments where incumbents withdraw active defense (e.g., expiring exclusivity contracts, lapsed patents, settled litigation) while historical infrastructure remains constant — observed entry rates after withdrawal would reveal how much of the prior barrier was load-bearing maintenance versus residual friction.',
    'If post-withdrawal entry surges, the maintained component dominated and this reading should shift closer to the beneficiary_maintained sibling''s higher-extraction profile in that sub-market. If entry remains sluggish, the lapsed component dominated and the sub-market more closely resembles the lapsed_alternative sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_proportion_ambiguity, empirical, 'The empirical ratio of lapsed to maintained mechanism within the hybrid, which varies by sub-market and is not resolved by this story alone.').

omega_variable(
    kernel_reading_selection_criterion,
    'What observational signal justifies classifying a given instance of market dominance under the hybrid reading rather than one of the two pure-mechanism sibling readings?',
    'A documented decision procedure — e.g., requiring both (a) evidence of at least one genuinely inertial barrier (no defender, no recent renewal) AND (b) evidence of at least one actively renewed barrier (recent litigation, new exclusivity terms, fresh capture activity) — before a market qualifies for hybrid rather than pure-mechanism classification.',
    'Without an explicit selection criterion, the hybrid reading risks becoming a default catch-all applied whenever analysts are uncertain, which would blur its distinct empirical content and make it indistinguishable from simple agnosticism between the two sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'Whether the hybrid reading has a principled selection criterion distinguishing it from mere uncertainty between the pure-lapsed and pure-maintained readings.').

omega_variable(
    capture_conversion_dynamic,
    'Does the observed rise in suppression and theater ratio over the interval reflect lapsed elements being actively converted into maintained ones (a genealogical claim about mechanism transition), or does it reflect improved measurement of maintenance that was present but undetected from the start?',
    'Archival reconstruction of incumbent lobbying expenditure, litigation frequency, and exclusivity-contract renewal rates over the same interval — a rising trend in these direct indicators would support genuine conversion; a flat trend alongside rising suppression/theater metrics would support a measurement-artifact explanation.',
    'Genuine conversion supports treating this as an active, worsening tangled-rope trajectory requiring intervention; a measurement artifact would mean the underlying structure was already this extractive from t=0, changing the historical narrative but not the current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capture_conversion_dynamic, empirical, 'Whether the temporal trend reflects mechanism change or measurement improvement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__hybrid_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__hybrid_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__hybrid_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__hybrid_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(mark_be_t8, market_naturalization__hybrid_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(mark_be_t16, market_naturalization__hybrid_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(mark_be_t24, market_naturalization__hybrid_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(mark_be_t32, market_naturalization__hybrid_reading, base_extractiveness, 32, 0.49).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mark_su_t8, market_naturalization__hybrid_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(mark_su_t16, market_naturalization__hybrid_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(mark_su_t24, market_naturalization__hybrid_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(mark_su_t32, market_naturalization__hybrid_reading, suppression_requirement, 32, 0.45).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language claim 'market dominance is naturalized.' The lapsed_alternative_reading asserts dominance persists via inertia alone (near-mountain, low ε); the beneficiary_maintained_reading asserts dominance is actively and continuously defended (snare/tangled-rope, high ε); this hybrid_reading asserts both mechanisms co-occur with variable proportion (moderate ε, 0.52). Per the ε-invariance principle, these are three distinct constraints, not one constraint measured three ways — each has its own stable ε, its own beneficiary/victim structure, and its own claimed_type. They are linked via affects_constraints because evidence bearing on one reading's mechanism proportion is directly relevant to distinguishing it from the others (see the mechanism_proportion_ambiguity and kernel_reading_selection_criterion omegas).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
