% ============================================================================
% CONSTRAINT STORY: oc_donation_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_oc_donation_model, []).

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
 *   constraint_id: oc_donation_model
 *   human_readable: Open Culture's Voluntary Donation-Based Funding Model
 *   domain: economic/social/cultural_commons
 *
 * SUMMARY:
 *   Open Culture operates a structurally hybrid model: it provides genuine
 *   coordination (aggregation, curation, accessibility) for content consumers
 *   while simultaneously relying on unpaid or under-compensated content
 *   creators and voluntary donors to sustain operations. The constraint
 *   exhibits characteristics of both coordination (Rope) and extraction
 *   (Snare/Tangled Rope) depending on the observer's structural position.
 *   Content consumers experience pure coordination — they solve the discovery
 *   problem at zero cost and can exit freely. Content creators are trapped by
 *   professional norms that equate attribution with compensation, facing
 *   suppression of alternative monetization paths. The Open Culture
 *   organization itself operates as a mixed coordination-extraction hybrid:
 *   it aggregates value but cannot adequately fund the creators whose work
 *   generates that value. The donation model's increasing theater ratio
 *   (0.35→0.55 over 14 years) reflects growing reliance on altruistic framing
 *   and attribution aesthetics as the sustainability gap widens. The
 *   constraint resembles a Scaffold if institutional alternatives (library
 *   partnerships, public broadcasting models) mature; it resembles a Tangled
 *   Rope if voluntary funding remains primary; it approaches Snare if creator
 *   suppression increases without compensation.
 *
 * KEY AGENTS:
 *   - Content Consumers: Primary beneficiary (powerless/mobile) — access aggregated cultural content at zero cost; can exit to paywalled alternatives
 *   - Content Creators: Primary victim (powerless/trapped) — work is freely distributed and aggregated without direct compensation; trapped by attribution norms and opportunity cost of alternative monetization
 *   - Open Culture Organization: Operator (moderate/constrained) — provides aggregation and curation coordination but depends on voluntary funding and unpaid creator attribution; constrained by non-profit structure
 *   - Voluntary Donors: Secondary participant (moderate/mobile) — fund operations through discretionary gifts; can exit by withholding support; key to constraint sustainability
 *   - Digital Rights Movement: Organized advocate (organized/arbitrage) — frames voluntary donation as transitional bridge to institutional funding models; sees sunset path
 *   - Legacy Copyright/Publishing Regime: Institutional incumbent (institutional/constrained) — compliance theater persists (attribution, licensing) while economic control atrophies; piton perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oc_donation_model, 0.28).
domain_priors:suppression_score(oc_donation_model, 0.35).
domain_priors:theater_ratio(oc_donation_model, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oc_donation_model, extractiveness, 0.28).
narrative_ontology:constraint_metric(oc_donation_model, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(oc_donation_model, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(oc_donation_model, scaffold).
narrative_ontology:human_readable(oc_donation_model, "Open Culture's Voluntary Donation-Based Funding Model").
narrative_ontology:topic_domain(oc_donation_model, "economic/social/cultural_commons").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(oc_donation_model, content_consumers).
narrative_ontology:constraint_beneficiary(oc_donation_model, cultural_commons).
narrative_ontology:constraint_victim(oc_donation_model, content_creators).
narrative_ontology:constraint_victim(oc_donation_model, sustainability_risk).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CONSUMERS (ROPE) — Can access curated cultural content at zero marginal cost; can exit by switching to paywalled alternatives or directly accessing sources. Experience the constraint as pure coordination: aggregation solves the discovery problem. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.00. Net beneficiary with full mobility.
constraint_indexing:constraint_classification(oc_donation_model, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATORS (SNARE) — Work is aggregated and freely distributed without direct compensation mechanism. Trapped by professional norm that 'exposure' counts as payment and by Open Culture's aggregation advantage. High suppression: alternative monetization paths (Patreon, licensing, direct sales) require upfront audience building that Open Culture's free distribution undercuts. d≈0.90, f(d)≈1.35, σ=1.2 → χ≈0.45. Extraction without exit.
constraint_indexing:constraint_classification(oc_donation_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: OPEN CULTURE ORGANIZATION (TANGLED ROPE) — Provides genuine coordination function (aggregation, curation, accessibility) but funding mechanism creates asymmetry: relies on voluntary donor support while freely distributing others' work. Constrained by non-profit structure and dependence on donor goodwill; cannot easily monetize or charge users. Benefits from volunteer labor and content creator attribution. Requires active enforcement of fair use and copyright compliance. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.25. Mixed coordination (aggregation value) and extraction (unpaid reliance on creator work).
constraint_indexing:constraint_classification(oc_donation_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL RIGHTS MOVEMENT (SCAFFOLD) — Sees Open Culture as a temporary solution enabling transition from scarcity-based copyright to abundance-based access norms. Organized advocates (Creative Commons, EFF, library associations) frame voluntary donation as a bridge mechanism with sunset logic: as institutional funding for cultural commons matures (library partnerships, public broadcasting models, education endowments), voluntary donation becomes less critical. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.09. Low extraction because exit path is visible (institutional funding alternatives).
constraint_indexing:constraint_classification(oc_donation_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY COPYRIGHT REGIME (PITON) — Open Culture's model is performatively compliant with copyright law (fair use, public domain, licensing) but structurally degrades the traditional publishing incentive chain. Theater ratio 0.55: compliance theater (attribution, licensing notices) persists while the underlying payment mechanism fails. The regime remains through institutional inertia and legal frameworks, but its functional control of cultural distribution has atrophied. d≈0.80, f(d)≈1.18, σ=1.2 → χ≈0.41. Extraction mechanism (copyright control) persists but is effectively neutered.
constraint_indexing:constraint_classification(oc_donation_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, digital information exhibits natural non-excludability (zero marginal cost to distribute, cannot prevent copying). Open Culture's donation model could be framed as an inevitable response to information economics: once content is digital, voluntarism is the natural equilibrium. However, the structural data (ε=0.28, suppression=0.35) contradicts pure mountain classification — content creators ARE suppressed through career disincentives, suggesting institutional contingency rather than natural law. False summit risk: information economics does not mandate unpaid creator work, only free distribution.
constraint_indexing:constraint_classification(oc_donation_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oc_donation_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(oc_donation_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(oc_donation_model, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(oc_donation_model, TR),
    TR >= 0.70.

:- end_tests(oc_donation_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. Open Culture's model extracts value from creators (attribution without direct compensation) but does not depend on active suppression — the suppression comes from professional norms (attribution as payment) and structural barriers (difficulty monetizing work post-aggregation). The constraint is neither pure extraction nor pure coordination. Base extractiveness reflects the unpaid reliance on creator work, but the value to consumers exceeds the extraction cost. Suppression (0.35): Moderate. Creators face barriers to alternative monetization (aggregated work undercutting direct sales, opportunity cost of independent distribution, professional norm that 'exposure' counts as payment). But suppression is not total — some creators can monetize directly, patreon/licensing are available, and professional institutions (universities, publishers) still pay. Theater ratio (0.55): Moderate. The model relies partly on attribution aesthetics and altruistic framing ('support free culture') to justify the funding gap, but the aggregation and curation are functionally valuable. Theater has increased over time as donors increasingly fund 'access' ideology rather than sustainable creator compensation. The ratio reflects growing gap between nominal function (free access) and structural sustainability (unpaid reliance).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how structural position determines classification. Content consumers see pure coordination (Rope) — the aggregation solves their discovery problem at zero cost. Content creators see pure extraction (Snare) — trapped by attribution norms and facing suppression of alternatives. The Open Culture organization sees hybrid coordination-extraction (Tangled Rope) — it aggregates value but cannot fund it adequately. The digital rights movement sees a temporary bridge (Scaffold) — institutional funding alternatives are emerging. The legacy copyright regime sees its own degradation (Piton) — copyright compliance theater persists while economic control fails. The civilizational observer risks seeing information economics inevitability (Mountain) — but the extraction of creator labor is institutional contingency, not natural law. The perspectival gap reveals that the same constraint means opposite things to creators versus consumers, and no single indexical view captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Content consumers: Beneficiary + mobile → d≈0.15, f(d)≈-0.01. Near-zero extraction from their perspective; net beneficiary. Content creators: Victim + trapped → d≈0.90, f(d)≈1.35. High extraction; trapped by norms and opportunity cost. Open Culture organization: Mixed beneficiary (from creator work) + victim (from donor dependence), constrained → d≈0.55, f(d)≈0.75. Moderate extraction; organization extracts creator value but is itself constrained by funding uncertainty. Voluntary donors: Secondary + mobile → d≈0.25, f(d)≈0.05. Low extraction; donors can exit and are framed as altruistic partners. Digital rights movement: Organized + arbitrage → d≈0.35, f(d)≈0.28. Low extraction; organized advocates see exit path (institutional funding). Legacy regime: Institutional + constrained → d≈0.80, f(d)≈1.18. High extraction from its own degraded perspective; piton classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Open Culture is genuinely hybrid but asymmetric. The coordination function (aggregation, curation, accessibility) is real and valuable for consumers. The extraction (unpaid creator reliance) is real and harmful for creators. The two are not separable — the aggregation creates value that is captured by consumers while creators bear the cost. The constraint is Tangled Rope at the analytical level: it has both genuine coordination (solves discovery problem) and asymmetric extraction (creators subsidize it). The Scaffold perspective is forward-looking: if institutional funding alternatives mature, the constraint becomes temporary coordination. The Snare perspective from creators is real: they are trapped by norms. The Rope perspective from consumers is real: they benefit with no cost. The constraint is not 'really' one type — it is multiple types in a coherent presheaf. The mandatrophy teaches that such hybrids are structurally stable only if extraction remains below consciousness threshold (theater_ratio high, extraction framed as altruism) or if beneficiaries actively defend creators. Open Culture's theater increasing (0.35→0.55) indicates that the sustainability gap is widening, and the constraint is moving toward explicit Snare classification unless creator compensation mechanisms are institutionalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_donor_sustainability,
    'Is voluntary donation a structurally sustainable funding model for large-scale cultural aggregation, or does it require hidden subsidies (volunteer labor, institutional partnerships)?',
    'Financial audit tracing all funding sources (direct donations, grants, institutional support, volunteer valuation); comparison with operational costs and content licensing fees.',
    'If sustainable: constraint is primarily coordination (Rope/Scaffold). If dependent on hidden subsidies: constraint is extractive (Snare/Tangled Rope) with unpaid labor as the true cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_donor_sustainability, empirical, 'Whether voluntary donations sustain operations without hidden subsidies').

omega_variable(
    creator_attribution_sufficiency,
    'Does attribution without compensation genuinely benefit content creators, or does it function as a legitimizing narrative for free extraction?',
    'Tracking of creator career outcomes: correlation between Open Culture aggregation and creator income, audience growth, licensing deals, or career advancement; interviews with attributed creators.',
    'If attribution drives tangible benefits: extraction level is moderate (Tangled Rope). If purely performative: extraction is severe (Snare), and suppression is higher than estimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_attribution_sufficiency, empirical, 'Whether attribution delivers tangible creator benefits').

omega_variable(
    alternative_institutional_maturity,
    'Are institutional alternatives (library partnerships, public broadcasting models, education endowments, grant funding) sufficiently mature to replace voluntary donation, or is the scaffold sunset speculative?',
    'Mapping of institutional funding pathways available to cultural aggregators; analysis of funding trends in open-access infrastructure and cultural commons projects.',
    'If alternatives mature: scaffold perspective is grounded and sunset is real (10-20 year horizon). If alternatives stall: scaffold is aspirational, and the constraint remains Tangled Rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_institutional_maturity, empirical, 'Maturity of institutional funding alternatives to voluntary donation').

omega_variable(
    moral_licensing_effect,
    'Does the voluntary donation model''s framing as ''altruistic cultural sharing'' reduce donor scrutiny of extraction mechanisms and suppress collective action by creators?',
    'Comparison of creator advocacy and unionization efforts in Open Culture versus other free-content platforms; analysis of donor knowledge regarding creator compensation.',
    'If licensing effect is strong: suppression is higher (0.50+), and Snare classification is justified. If weak: suppression is accurately estimated at 0.35.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_licensing_effect, conceptual, 'Whether altruistic framing suppresses critique of extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(oc_donation_model, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ocd_tr_t0, oc_donation_model, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ocd_tr_t7, oc_donation_model, theater_ratio, 7, 0.48).
narrative_ontology:measurement(ocd_tr_t14, oc_donation_model, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(ocd_be_t0, oc_donation_model, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ocd_be_t7, oc_donation_model, base_extractiveness, 7, 0.22).
narrative_ontology:measurement(ocd_be_t14, oc_donation_model, base_extractiveness, 14, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(oc_donation_model, information_standard).
narrative_ontology:affects_constraint(oc_donation_model, creator_compensation_norms).
narrative_ontology:affects_constraint(oc_donation_model, free_culture_sustainability).

% DUAL FORMULATION NOTE:
% Open Culture's donation model is downstream of broader free culture ideology and upstream of specific creator compensation outcomes. The constraint family includes: (1) free_culture_sustainability (ε≈0.35, Scaffold) — the institutional viability of free-access cultural models, (2) oc_donation_model (ε≈0.28, Tangled Rope) — the specific funding mechanism, (3) creator_compensation_norms (ε≈0.55, Snare) — professional norms that treat attribution as payment. These are distinct constraints with different ε values reflecting their structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(oc_donation_model, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
