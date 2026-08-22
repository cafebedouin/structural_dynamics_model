% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Regulated Legal Drug Commerce Authority
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story authors the legalization reading of the contested
 *   substance-control-authority kernel: the state's authority to regulate
 *   drug markets as licensed, taxed commerce with quality and access
 *   controls, rather than criminalizing use (prohibition_reading) or
 *   accepting use while minimizing harm through public health means without
 *   commercializing supply (harm_reduction_reading). Under this reading,
 *   users exit both the criminal-justice victim set and the
 *   unregulated-supply victim set — legalization's central claimed
 *   achievement — but a new extraction structure emerges: licensed producers
 *   and tax authorities acquire a shared commercial interest in market
 *   volume, including from heavy and dependent users, and enforcement is not
 *   eliminated but narrowed and re-justified against the residual illicit
 *   market and against retail siting externalities borne by powerless
 *   communities. The ε for this reading (0.42) is lower than a plausible
 *   prohibition-reading ε (which would carry the weight of mass
 *   criminalization of users) because a large victim class exits the
 *   constraint entirely; it is not zero because the regulatory-commercial
 *   structure creates its own extraction relationships that a
 *   coordination-only framing would obscure.
 *
 * KEY AGENTS:
 *   - state_regulatory_agency: agenda-setter, designs and enforces the licensing regime
 *   - licensed_drug_producers: primary beneficiary, captures commercial value of formerly illicit trade
 *   - adult_recreational_users: exits criminal/unregulated-supply victim sets, becomes taxed consumer
 *   - residual_illicit_market_participants: remains criminalized under a narrower justification
 *   - communities_near_licensed_retail_outlets and heavy_use_dependent_populations: bear new, diffuse costs of commercialized availability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.42).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Regulated Legal Drug Commerce Authority").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, 'e0d13368-b68d-4606-a614-09d7e96430aa').
narrative_ontology:cs_kernel_codification('e0d13368-b68d-4606-a614-09d7e96430aa', distributed).
narrative_ontology:cs_authority_grounding('e0d13368-b68d-4606-a614-09d7e96430aa', distributed).
narrative_ontology:cs_reading_relation('e0d13368-b68d-4606-a614-09d7e96430aa', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('e0d13368-b68d-4606-a614-09d7e96430aa', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('e0d13368-b68d-4606-a614-09d7e96430aa', foundational, regulated_commerce_reduces_net_harm_versus_criminalization).
narrative_ontology:cs_axiom_status(regulated_commerce_reduces_net_harm_versus_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('e0d13368-b68d-4606-a614-09d7e96430aa', regulated_commerce_reduces_net_harm_versus_criminalization, empirically_contingent).
narrative_ontology:cs_axiom('e0d13368-b68d-4606-a614-09d7e96430aa', secondary, state_licensed_market_access_is_legitimate_state_function).
narrative_ontology:cs_axiom_status(state_licensed_market_access_is_legitimate_state_function, holdable).
narrative_ontology:cs_axiom_grounding('e0d13368-b68d-4606-a614-09d7e96430aa', state_licensed_market_access_is_legitimate_state_function, conventional).
narrative_ontology:cs_reference_frame('e0d13368-b68d-4606-a614-09d7e96430aa', criminalized_illicit_market_baseline).
narrative_ontology:cs_drift_state('e0d13368-b68d-4606-a614-09d7e96430aa', post_multi_jurisdiction_legalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e0d13368-b68d-4606-a614-09d7e96430aa', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_drug_producers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_tax_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, adult_recreational_users).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, former_illicit_market_participants_now_licensed).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, residual_illicit_market_participants).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, communities_near_licensed_retail_outlets).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, heavy_use_dependent_populations).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, youth_exposed_to_normalized_marketing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, adult_recreational_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the licensing, taxation, potency-labeling, and retail-access rules that convert what was an illegal market into a regulated commercial one. Collects license fees and tax revenue, sets quality and advertising standards, and enforces against unlicensed sale. Its authority rests on the claim that regulated commerce controls harms better than either criminalization or non-intervention.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agency, agenda_setter,
    institutional, generational, analytical, national).

% Operate legally within licensing rules, capturing the commercial value of a formerly criminalized market. Face compliance costs (potency limits, labeling, taxation) but gain legal protection, access to banking and advertising, and the ability to scale distribution. Their exit option is strong: they can lobby for looser rules or relocate production to more permissive jurisdictions.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_drug_producers, beneficiary,
    organized, biographical, arbitrage, national).

% Collect excise and sales tax revenue from the newly legal market, revenue that funds enforcement of the regulatory regime itself and general public spending. Have a direct fiscal interest in market volume growing, which creates a structural incentive misaligned with harm minimization.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Exit the criminal victim set entirely — no longer face arrest or unregulated-supply risk (adulteration, unknown potency, violence-prone black-market transactions). Pay taxed retail prices and are subject to marketing exposure that legalization not only permits but commercially incentivizes; their consumption may rise as access and social acceptability increase.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, adult_recreational_users, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, adult_recreational_users, payer).

% Small-scale growers, sellers, or distributors who previously operated illegally and now attempt to obtain licenses. Benefit from legal status when licensing succeeds, but licensing costs and capital requirements often favor larger operators, so many are structurally excluded from formalizing even as the market they built is legalized around them.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, former_illicit_market_participants_now_licensed, beneficiary,
    moderate, biographical, constrained, national).

% Continue operating outside the licensed system — either priced out of licensing, unable to meet compliance requirements, or serving demand the regulated market doesn't reach (lower prices, no ID checks). Face continued criminal enforcement, now framed as protecting the legal market's integrity rather than protecting the public from drugs as such; the legalization regime does not eliminate this population, it narrows and re-justifies its criminalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, residual_illicit_market_participants, payer,
    powerless, immediate, trapped, regional).

% Bear concentrated externalities of retail siting decisions made by state licensing boards — traffic, public consumption, local market saturation — often in lower-income neighborhoods where zoning resistance is weaker. Have limited voice in siting decisions relative to producer lobbying and municipal tax-revenue incentives.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, communities_near_licensed_retail_outlets, payer,
    powerless, biographical, trapped, local).

% Bear the health costs of increased availability and commercial marketing optimized for frequent use; the legal market's revenue model depends disproportionately on heavy users, giving licensed producers and tax authorities a shared interest in sustaining rather than reducing dependent consumption. Treatment and public health support are typically funded from a fraction of the tax revenue their consumption generates.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, heavy_use_dependent_populations, payer,
    powerless, biographical, identity_locked, national).

% Experience a social environment where drug commerce is normalized, advertised, and commercially present, even where direct sales to minors remain illegal. Have no voice in the regulatory design and bear diffuse long-term exposure risk that is difficult to attribute causally to any single licensing decision.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, youth_exposed_to_normalized_marketing, payer,
    powerless, generational, trapped, national).

% Study use-volume trends, health outcomes, and market structure after legalization, comparing outcomes to both the prohibition baseline and harm-reduction alternatives. Provide the empirical record that different readings of the kernel cite selectively in support of their own framing.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__legalization_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_authority__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an unregulated or criminalized transaction into a licensed, taxed, quality-controlled commercial one — solving the genuine problems of adulterated supply, unpredictable potency, and violence-prone illicit distribution that unregulated markets produce.
% TRANSFER_FUNCTION: Moves the drug trade's economic value from illicit operators to licensed commercial entities and the state treasury, while moving criminal-justice risk away from consumers and onto the shrinking population that remains outside the licensed system. Health costs of increased availability are moved onto heavy users and their communities, offset only partially by tax-funded services.
% ABSENT_VOICES: Communities where retail outlets are sited, and populations who become heavy users under expanded commercial availability, are not meaningfully represented in licensing board proceedings, which are structured around producer applications and municipal revenue projections. Small-scale illicit operators who cannot afford licensing are treated as a residual enforcement problem rather than as stakeholders in the transition design.
% DISAPPEARANCE_RATIONALE: If the regulatory authority disappeared, the market would not return to the pre-legalization status quo — a large licensed commercial infrastructure (retail chains, tax revenue streams, supply chains) would either collapse or continue unregulated, producing acute quality and access chaos; users currently protected from criminal liability would face renewed legal exposure at scale, and the state would lose a substantial and often politically difficult-to-replace revenue source.
% FOUNDING_PROBLEM: Prohibition-era illicit drug markets caused significant harm through criminal violence, unregulated (adulterated, unpredictable-potency) supply, and mass incarceration of users and low-level sellers, while doing little to reduce use. Legalization with regulation was built to eliminate the illicit market's violence and supply-safety problems by bringing the trade under state licensing and quality control.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers and criminal-justice reform advocates outside the licensed industry attest that arrest and incarceration harms have substantially declined and supply-safety has improved in legalized jurisdictions — corroborating the founding problem as partially resolved. However, independent public health data also show illicit markets persist at the margins (untaxed, unlicensed) and use-volume and heavy-use indicators have risen in several jurisdictions, which licensed industry associations do not foreground; researchers not funded by industry or state tax offices are the source of this countervailing evidence.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).
:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects a genuine coordination achievement (eliminating criminal-market violence and adulteration risk for the exiting user population) alongside a real, if smaller, extraction structure: licensed producers and tax authorities benefit from consumption volume, including from populations whose increased use the regime does little to discourage. Suppression (0.35) is authored as declining over the interval because the population subject to active state coercion under this reading shrinks as the licensed market absorbs former users — but it does not go to zero, since residual illicit market participants continue to face enforcement, now justified as protecting the integrity of the licensed market rather than suppressing drug use as such. Theater ratio is low-to-moderate and rising modestly (0.10 to 0.20) reflecting some performative compliance activity (health warnings, marketing restrictions) that grows as public health critique of commercialization accumulates, without yet dominating the regime's function.
 *
 * PERSPECTIVAL GAP:
 *   From the state regulatory agency's seat, this reads as successful coordination: crime is down, supply is safer, revenue funds public goods. From the residual illicit market participant's seat, criminalization continues essentially unchanged, now with less public sympathy because 'the state already solved the problem.' From the heavy-use dependent population's seat, the constraint looks like a for-profit apparatus with a fiscal interest in their continued consumption. The engine's per-seat computation should surface this divergence rather than average it into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Licensed producers and tax authorities sit near the beneficiary end: they collect commercial and fiscal value directly from the constraint's operation, with strong or institutional exit options. Adult recreational users sit closer to symmetric-to-beneficiary: real gains from decriminalization and safer supply, offset by taxed prices and marketing exposure, with mobile exit (they can choose not to consume or to consume less). Residual illicit market participants, communities near retail outlets, heavy users, and youth sit at the target end: trapped or identity-locked exit options, powerless structural position, and costs that flow directly from decisions made by agenda-setters and beneficiaries they cannot meaningfully influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure rope) prevents this reading from being mistaken for costless harm elimination: legalization genuinely solves the coordination problem prohibition created (violence, adulteration, mass incarceration of users) but layers a new, real extraction relationship onto the solution — commercial and fiscal actors profiting from consumption volume, with the heaviest costs falling on populations with the least power to object. Calling this a pure rope would erase the youth-marketing, retail-siting, and dependent-use costs; calling it a pure snare would erase the substantial and real benefit to the population that exits the criminal and unregulated-supply victim sets. The tangled_rope classification is the one that can hold both truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercialization_vs_harm_reduction_tradeoff,
    'Does converting drug supply into commercial enterprise systematically increase total consumption and dependent use beyond what harm-reduction-without-commerce would produce, and if so, is that increase an acceptable cost of eliminating criminal-market harms?',
    'Longitudinal comparison of use-volume, dependency-rate, and mortality/morbidity trends across jurisdictions choosing legalization, harm-reduction, and prohibition regimes, controlling for regional and demographic confounds.',
    'If commercialization substantially increases dependent use relative to harm-reduction regimes, this reading''s coordination claim weakens relative to the harm_reduction_reading sibling, and the tangled_rope extraction component (producer/tax-authority interest in volume) becomes the dominant structural feature rather than a secondary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercialization_vs_harm_reduction_tradeoff, empirical, 'Whether legalization''s commercial structure causally drives higher problematic use than non-commercial harm reduction.').

omega_variable(
    residual_criminalization_justification,
    'Is continued enforcement against residual illicit market participants, under a legalization regime, structurally different from prohibition-era enforcement, or is it the same enforcement apparatus re-labeled as protecting market integrity?',
    'Comparative analysis of enforcement targeting, sentencing patterns, and resource allocation before and after legalization in jurisdictions that legalized, focusing on whether enforcement intensity per-capita against unlicensed sellers changed.',
    'If enforcement patterns are structurally continuous with prohibition, the legalization reading''s claim to have ''eliminated illegal markets'' is partly rhetorical, and the residual_illicit_market_participants victim group''s structural position is closer to unchanged than to newly liberated — strengthening the tangled_rope reading over a rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_criminalization_justification, empirical, 'Whether post-legalization enforcement against unlicensed sellers is functionally continuous with prohibition enforcement.').

omega_variable(
    licensing_capital_barrier_effect,
    'Do licensing capital and compliance requirements systematically exclude former illicit-market participants (often from communities most harmed by prohibition) from formalizing, transferring the market''s value to better-capitalized new entrants?',
    'Track licensing approval rates and applicant demographics/financial profiles against pre-legalization market participant demographics in jurisdictions with social-equity licensing provisions versus those without.',
    'If capital barriers systematically exclude the population most harmed by prohibition from capturing the legal market''s value, the beneficiary set for this reading (licensed_drug_producers) is structurally narrower and more extractive relative to the exiting victim population than the coordination narrative suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_capital_barrier_effect, empirical, 'Whether licensing capital requirements reproduce exclusion of prohibition-harmed populations from the legal market''s benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__legalization_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__legalization_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__legalization_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__legalization_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__legalization_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__legalization_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__legalization_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__legalization_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__legalization_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__legalization_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__legalization_reading, 0.15).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the substance_control_authority kernel, each authored as a separate constraint story per the ε-invariance principle: prohibition_reading (criminalization to protect third parties), harm_reduction_reading (public-health acceptance without commercialization), and legalization_reading (this file — regulated commercial legality). Each reading has a distinct beneficiary/victim structure and a distinct ε; they are linked here rather than merged because measuring 'drug policy' by different observables (criminal-justice outcomes vs. public-health outcomes vs. market-structure outcomes) yields structurally different constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
