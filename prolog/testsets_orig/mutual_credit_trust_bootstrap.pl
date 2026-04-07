% ============================================================================
% CONSTRAINT STORY: mutual_credit_trust_bootstrap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mutual_credit_trust_bootstrap, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mutual_credit_trust_bootstrap
 *   human_readable: Mutual Credit Trust Bootstrap Problem
 *   domain: economic/social/coordination
 *
 * SUMMARY:
 *   Mutual credit systems (like LETS, time banking, and blockchain-based
 *   credit networks) solve the double-coincidence-of-wants problem by
 *   allowing deferred exchange: Alice provides goods to Bob on credit, and
 *   Bob later settles with Carol, whose debt flows back to Alice. But all
 *   such systems face a trust bootstrap problem: early adopters must be
 *   incentivized to adopt before network effects lock in late arrivals. This
 *   creates an asymmetric extraction structure: early joiners get favorable
 *   credit terms, established reputation, and preferential access to scarce
 *   goods; late joiners face high-friction onboarding, adverse exchange
 *   rates, and collateral requirements. The constraint exhibits tension
 *   between genuine coordination (solving asynchronous exchange) and
 *   extractive rent-seeking (early-adopter premium). The theater ratio (0.55)
 *   reflects that mutual credit systems invest substantial effort in
 *   trust-building ritual (reputation reputation scoring, community
 *   governance, exchange documentation) that both serves and obscures the
 *   underlying extraction mechanism.
 *
 * KEY AGENTS:
 *   - Late Joiners: Primary victims (powerless/trapped) — must accept adverse terms relative to early adopters; no exit without abandoning access to goods/credit
 *   - Asymmetric Traders: Secondary victims (moderate/constrained) — benefit from credit access but face high collateral/rate requirements; can exit to formal finance at higher cost
 *   - Early Adopters: Primary beneficiaries (institutional/arbitrage) — capture favorable terms, reputation premium, collateral arbitrage; can exit to formal currency without penalty
 *   - Platform Governance Coalition: Organized actors (organized/constrained) — aim to manage bootstrap through inclusive design; can influence terms but constrained by need to incentivize initial adoption
 *   - Legacy Exchange Ritual: Institutional persistence mechanism (institutional/arbitrage) — mutual credit norms embedded in community identity and social narrative; maintains system through cultural continuity rather than function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing network effects (insiders/outsiders) as immutable rather than as remediable design choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mutual_credit_trust_bootstrap, 0.38).
domain_priors:suppression_score(mutual_credit_trust_bootstrap, 0.48).
domain_priors:theater_ratio(mutual_credit_trust_bootstrap, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mutual_credit_trust_bootstrap, extractiveness, 0.38).
narrative_ontology:constraint_metric(mutual_credit_trust_bootstrap, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(mutual_credit_trust_bootstrap, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mutual_credit_trust_bootstrap, tangled_rope).
narrative_ontology:human_readable(mutual_credit_trust_bootstrap, "Mutual Credit Trust Bootstrap Problem").
narrative_ontology:topic_domain(mutual_credit_trust_bootstrap, "economic/social/coordination").

domain_priors:requires_active_enforcement(mutual_credit_trust_bootstrap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mutual_credit_trust_bootstrap, early_adopters).
narrative_ontology:constraint_beneficiary(mutual_credit_trust_bootstrap, system_operators).
narrative_ontology:constraint_victim(mutual_credit_trust_bootstrap, late_joiners).
narrative_ontology:constraint_victim(mutual_credit_trust_bootstrap, asymmetric_traders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE JOINER (SNARE) — Arrives after trust network is established. Must accept unfavorable exchange rates relative to early adopters. Cannot exit because all alternative currency systems have higher adoption barriers or worse terms. Faces full extraction with no alternative; trust asymmetry is enforced by network effects.
constraint_indexing:constraint_classification(mutual_credit_trust_bootstrap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ASYMMETRIC TRADER (TANGLED ROPE) — Specializes in goods with high unit cost but low frequency of exchange. Bears suppression from extended credit lines (high counterparty risk) but benefits from access to credit unavailable in mainstream banking. Mixed coordination-extraction: system enables their trade but extracts through adverse credit terms.
constraint_indexing:constraint_classification(mutual_credit_trust_bootstrap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EARLY ADOPTER COLLECTIVE (ROPE) — Enjoys favorable exchange terms and credit allocation. Can arbitrage between mutual credit and mainstream currency systems. Experiences the constraint as pure coordination: mutual credit solves their double-coincidence-of-wants problem. Low suppression; high exit optionality.
constraint_indexing:constraint_classification(mutual_credit_trust_bootstrap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PLATFORM GOVERNANCE COALITION (SCAFFOLD) — Organized agents (cooperative boards, digital currency platforms, credit unions) see bootstrap asymmetry as a temporary problem solvable through governance: expanding inclusive onboarding, graduated credit tiers, and anti-discriminatory protocols. Theater ratio moderate because governance oversight partially replaces trust arbitrage. Sunset clause: as system matures, inclusion mechanisms mature.
constraint_indexing:constraint_classification(mutual_credit_trust_bootstrap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY EXCHANGE RITUAL (PITON) — Mutual credit bootstrapping replicates pre-modern gift economy norms (favor tracking, reciprocal obligation, reputation economy) within digital infrastructure. But the underlying coordination function (managing asynchronous exchange) is increasingly served by formal credit institutions. The ritual persists through community identity and nostalgia, not function. High theater ratio indicates performative continuation of what was once necessary coordination.
constraint_indexing:constraint_classification(mutual_credit_trust_bootstrap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LIQUIDITY CONSTRAINT VIEW (MOUNTAIN) — From a universal/civilizational perspective, the bootstrap problem is inherent to all credit systems: you cannot generate trust from zero; initial trust asymmetry (insiders vs outsiders) is a universal property of network formation. Any mutual credit system must solve the bootstrap, and all solutions involve temporary extraction from late joiners. This perspective sees the bootstrap as an immutable feature of network growth. However, the structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of what is a remediable institutional design choice.
constraint_indexing:constraint_classification(mutual_credit_trust_bootstrap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mutual_credit_trust_bootstrap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mutual_credit_trust_bootstrap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mutual_credit_trust_bootstrap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(mutual_credit_trust_bootstrap, TR),
    TR >= 0.70.

:- end_tests(mutual_credit_trust_bootstrap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The mutual credit system extracts value from late joiners through asymmetric credit terms, collateral requirements, and reputation thresholds. However, the extraction is not maximal — late joiners retain some agency, can negotiate terms, and gain genuine access to credit unavailable in formal systems. The value reflects that extraction is real but not total-capture. Suppression (0.48): Moderate. Barriers to exit include opportunity cost of participation (time spent on governance and community maintenance), switching cost to formal credit (higher interest rates), and social pressure within community. But suppression is not extreme — late joiners can exit and have done so when terms became too harsh. Theater ratio (0.55): Moderate-high. Mutual credit systems dedicate substantial labor to governance, reputation scoring, and community ritual (meetings, documentation, dispute resolution) that both enables trust and obscures the extraction mechanism. The ritual has increased as systems have matured — early bootstrap is chaotic; mature systems develop formal governance theater.
 *
 * PERSPECTIVAL GAP:
 *   The most acute gap appears between the early adopter (Rope) and late joiner (Snare) perspectives. Early adopters see a coordination mechanism that solved their double-coincidence problem; late joiners see an extraction mechanism that locks them into unfavorable terms. The organized governance coalition attempts to bridge this gap through inclusive design (Scaffold) but cannot fully eliminate the early-adopter premium without undermining bootstrap incentives. The piton perspective (legacy ritual) reflects the system's maturation: as formal credit alternatives improve, mutual credit persists through community identity and nostalgia rather than necessity. The analytical observer risks naturalizing the bootstrap problem (Mountain) as inherent to all network growth, but structural data shows that inclusive governance design can significantly reduce asymmetry — making the classification a remediable design choice rather than an immutable feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters (institutional/arbitrage) have low directionality d because they benefit from the constraint and can exit to formal finance without cost — beneficiary status + arbitrage exit produces d ≈ 0.15. Late joiners (powerless/trapped) have high d because they bear extraction and cannot exit — victim status + trapped exit produces d ≈ 0.95. Asymmetric traders (moderate/constrained) occupy middle ground: they benefit from credit access but pay high rates and face suppression from extended credit lines — victim status + constrained exit produces d ≈ 0.65. The platform governance coalition (organized/constrained) attempts to lower the extraction rate through inclusive design (subsidized onboarding, graduated credit tiers, anti-discriminatory protocols) — beneficiary status (they control the system) + constrained exit (they're locked into governance) produces d ≈ 0.40. The piton classification derives from high theater_ratio (0.55) combined with degraded primary function: trust-building ritual persists as community identity maintenance rather than as essential coordination mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the bootstrap phase from the maturation phase. During bootstrap (early time points), extraction is unavoidable — you cannot generate trust from zero without creating insider/outsider asymmetry. The Snare perspective (late joiner at biographical timescale) is structurally accurate during bootstrap. But the constraint's claimed type (Tangled Rope) reflects the mature system where governance mechanisms partially remediate the asymmetry. The platform coalition's Scaffold perspective indicates that governance can create a sunset: as systems mature, inclusive onboarding protocols, graduated credit tiers, and anti-discriminatory rules reduce the extraction rate. The piton perspective captures the system's degradation risk: if mutual credit becomes primarily community ritual rather than functional coordination, it persists as theater. The mandatrophy prevents misclassifying the bootstrap phase (which genuinely requires early-adopter incentives) as pure extraction, while also preventing the false summit that would naturalize the mature-phase asymmetry as immutable rather than governance-remediable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_for_inclusive_design,
    'At what extraction level does a mutual credit system transition from necessary bootstrap to extractive network rent-seeking?',
    'Longitudinal comparison of onboarding terms: systems with inclusive tiers (graduated credit limits, subsidized new-member rates) vs closed-cohort systems; measurement of late-joiner welfare improvement over time',
    'If inclusive design keeps extraction below 0.25: system classifies as Scaffold with genuine sunset. If extraction persists above 0.40: system has mutated into Snare with permanent underclass.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_for_inclusive_design, empirical, 'Threshold distinguishing remedial bootstrap asymmetry from permanent extraction').

omega_variable(
    trust_asset_or_liability,
    'Is trust itself the scarce asset being extracted, or are transaction-processing asymmetries the actual mechanism?',
    'Decoupling analysis: separate measurement of trust-premium extraction (credit terms worse for new members) vs operational-cost asymmetries (platform fees, liquidity spreads, collateral requirements)',
    'If trust is primary: suppression is cognitive/relational, extractiveness comes from reputation arbitrage. If operational costs dominate: suppression is structural/financial, extractiveness comes from efficiency differentials. Different omegas emerge depending on mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trust_asset_or_liability, conceptual, 'Whether extraction mechanism is trust-asymmetry or operational-cost-differential').

omega_variable(
    network_effect_inevitability,
    'Can the bootstrap problem be eliminated through institutional design, or does network effect inevitably create permanent insiders/outsiders asymmetry?',
    'Empirical study of systems with identity-spanning onboarding (subsidized or sponsored entry, algorithmic credit assignment) vs quota-based systems; measurement of whether early-adopter premium persists after maturation',
    'If eliminable: bootstrap is Scaffold (solvable). If inevitable: bootstrap is Mountain (immutable feature of all credit systems). Classification hinges entirely on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_inevitability, empirical, 'Whether network effect insiders/outsiders gap is remediable or inevitable').

omega_variable(
    coordination_function_necessity,
    'Does mutual credit actually solve coordination problems that formal credit institutions cannot solve, or is it primarily a hedge against institutional exclusion?',
    'Comparative analysis of transaction types facilitated: does mutual credit enable trades that formal credit explicitly blocks (unbanked populations, high-frequency barter, reputation-based collateral) vs trades that formal credit could theoretically handle but doesn''t at accessible rates?',
    'If genuine coordination: Rope or Tangled Rope classification is justified. If primarily exclusion-hedge: extraction mechanism is more fundamental (institutional discrimination) and classification shifts toward Snare. Piton becomes less likely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether mutual credit fills coordination gap or hedges institutional exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mutual_credit_trust_bootstrap, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mctb_tr_t0, mutual_credit_trust_bootstrap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mctb_tr_t3, mutual_credit_trust_bootstrap, theater_ratio, 3, 0.45).
narrative_ontology:measurement(mctb_tr_t6, mutual_credit_trust_bootstrap, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(mctb_be_t0, mutual_credit_trust_bootstrap, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(mctb_be_t3, mutual_credit_trust_bootstrap, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(mctb_be_t6, mutual_credit_trust_bootstrap, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mutual_credit_trust_bootstrap, resource_allocation).
narrative_ontology:affects_constraint(mutual_credit_trust_bootstrap, double_coincidence_of_wants).
narrative_ontology:affects_constraint(mutual_credit_trust_bootstrap, financial_exclusion_from_formal_banking).

% DUAL FORMULATION NOTE:
% The mutual credit bootstrap is downstream of the double-coincidence-of-wants problem (which requires deferred exchange to solve) but represents a distinct structural constraint. The upstream constraint has its own extractiveness reflecting the coordination necessity; the bootstrap constraint has its own extractiveness reflecting the early-adopter premium and network-effect insiders/outsiders asymmetry. The two constraints are linked: solving double-coincidence requires trusting the deferred exchange partner, which creates the bootstrap problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mutual_credit_trust_bootstrap, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
