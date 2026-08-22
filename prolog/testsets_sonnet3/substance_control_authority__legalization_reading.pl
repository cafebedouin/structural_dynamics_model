% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Licensed Legal Commerce
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint models the legalization reading of a contested kernel —
 *   state authority over drug markets. Under this reading, the state licenses
 *   commercial production and sale, applies quality and potency controls,
 *   taxes transactions, and enforces the licensing perimeter against
 *   unlicensed operators. This reading eliminates two victim sets
 *   simultaneously relative to the prohibition reading: users no longer face
 *   criminal prosecution for possession/purchase, and users no longer face
 *   unregulated-supply poisoning risk from adulterated product. But it
 *   creates a new coordination/extraction hybrid: licensed incumbents capture
 *   the legal market and its tax-advantaged position, informal producers who
 *   cannot meet capital/compliance thresholds are pushed out and criminalized
 *   more starkly than before (since a 'no viable legal alternative existed'
 *   defense evaporates once legalization exists), and the state's tax
 *   dependence on volume creates a soft structural incentive against
 *   aggressive potency/marketing restriction. The sibling readings
 *   (prohibition_reading, harm_reduction_reading) are separate constraint
 *   stories with their own ε and victim sets — this reading does not average
 *   over them.
 *
 * KEY AGENTS:
 *   - state_regulatory_agency: agenda_setter, licenses and enforces the legal market perimeter
 *   - licensed_drug_producers: primary beneficiary, captures legal-market rents
 *   - state_tax_authorities: secondary beneficiary, captures excise revenue
 *   - former_illicit_market_users: beneficiary, exits both criminal and poisoning victim sets
 *   - informal_growers_and_sellers: primary payer, displaced and newly criminalized by licensing thresholds
 *   - heavy_use_populations: payer, bears use-volume increase risk
 *   - communities_near_retail_outlets: payer, bears siting externalities
 *   - public_health_researchers: observer, tracks outcomes against sibling readings
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
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Licensed Legal Commerce").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, 'd0a03cfc-175f-4c17-aa26-412ee84a4d40').
narrative_ontology:cs_kernel_codification('d0a03cfc-175f-4c17-aa26-412ee84a4d40', distributed).
narrative_ontology:cs_authority_grounding('d0a03cfc-175f-4c17-aa26-412ee84a4d40', practice).
narrative_ontology:cs_interpretation_layer_present('d0a03cfc-175f-4c17-aa26-412ee84a4d40').
narrative_ontology:cs_reading_relation('d0a03cfc-175f-4c17-aa26-412ee84a4d40', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('d0a03cfc-175f-4c17-aa26-412ee84a4d40', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('d0a03cfc-175f-4c17-aa26-412ee84a4d40', foundational, regulated_commerce_supersedes_criminal_sanction).
narrative_ontology:cs_axiom_status(regulated_commerce_supersedes_criminal_sanction, holdable).
narrative_ontology:cs_axiom_grounding('d0a03cfc-175f-4c17-aa26-412ee84a4d40', regulated_commerce_supersedes_criminal_sanction, instrumental).
narrative_ontology:cs_axiom('d0a03cfc-175f-4c17-aa26-412ee84a4d40', secondary, market_licensing_is_sufficient_third_party_protection).
narrative_ontology:cs_axiom_status(market_licensing_is_sufficient_third_party_protection, holdable).
narrative_ontology:cs_axiom_grounding('d0a03cfc-175f-4c17-aa26-412ee84a4d40', market_licensing_is_sufficient_third_party_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('d0a03cfc-175f-4c17-aa26-412ee84a4d40', criminalized_illicit_supply_baseline).
narrative_ontology:cs_drift_state('d0a03cfc-175f-4c17-aa26-412ee84a4d40', post_early_adopter_jurisdictions, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d0a03cfc-175f-4c17-aa26-412ee84a4d40', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_drug_producers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_tax_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, former_illicit_market_users).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, informal_growers_and_sellers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, heavy_use_populations).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, communities_near_retail_outlets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Licenses producers and retailers, sets potency/labeling/purity standards, collects excise taxes, and enforces the licensing perimeter against unlicensed sellers. Its authority is justified as replacing criminal-market violence and unregulated-supply poisoning with inspectable commerce, but it now depends on continuous market surveillance and licensing enforcement to hold the line against informal supply.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agency, agenda_setter,
    institutional, generational, analytical, national).

% Obtain exclusive legal right to produce and sell what was previously criminalized, capturing the price premium and brand value of legality. Can lobby the regulator on potency caps, retail density, and taxation, and can exit to other jurisdictions or product categories if terms sour.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_drug_producers, beneficiary,
    organized, biographical, arbitrage, national).

% Collect excise revenue from a newly legal commerce stream and redirect it to general funds or earmarked programs. Revenue dependence creates an interest in sustained or growing sales volume that sits uneasily beside public-health messaging.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Previously bought from criminal or unregulated suppliers, risking both prosecution and unknown-purity product. Now purchase tested, labeled product without fear of arrest, exiting both the criminal-justice victim set and the unregulated-supply victim set that prohibition and status-quo illegality created.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, former_illicit_market_users, beneficiary,
    moderate, biographical, mobile, national).

% Operated in the pre-legalization gray or black market, often as a livelihood of last resort. Licensing requirements — capital, compliance costs, background checks — exclude them from the newly legal market, criminalizing their continued operation more sharply than before since the 'reasonable coordination need' cover story no longer applies to them; they cannot afford the entry costs the new regime imposes.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, informal_growers_and_sellers, payer,
    powerless, biographical, trapped, regional).

% Bear the brunt of any use-volume increase that follows normalized commercial availability, targeted marketing, and reduced stigma. Regulatory design (potency caps, advertising limits) is the primary lever meant to protect them, but enforcement capacity and industry lobbying determine how tightly that lever is actually pulled.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, heavy_use_populations, payer,
    powerless, biographical, constrained, local).

% Live near licensed retail sites and absorb externalities of concentrated commercial availability — public consumption, local traffic in secondary informal resale, uneven siting that clusters outlets in lower-income neighborhoods. Have limited say over licensing-siting decisions made at higher levels of government.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, communities_near_retail_outlets, payer,
    moderate, biographical, constrained, local).

% Study use-volume, harm, and market-formalization outcomes after legalization, comparing them against the prohibition and harm-reduction counterfactuals. Have no enforcement power but produce the evidence base other seats cite selectively.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__legalization_reading, licensed_drug_producers).
narrative_ontology:fixing_cost_class(substance_control_authority__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces an unregulated or criminalized supply chain with an inspectable, licensed one — solving the genuine coordination problems of product safety (unknown purity/adulteration), tax capture, and elimination of the violence and enforcement costs associated with illegal distribution networks.
% TRANSFER_FUNCTION: Moves the economic surplus previously captured by criminal or informal supply networks into licensed commercial actors and state tax revenue; moves risk of arrest and poisoning away from users; moves market access away from informal producers who cannot meet licensing capital requirements; may move some harm-exposure toward heavy-use populations and outlet-adjacent communities if commercial incentives outrun regulatory restraint.
% ABSENT_VOICES: Informal growers/sellers displaced by licensing requirements rarely have a seat in legislative design processes that set capital and compliance thresholds; heavy-use populations are frequently represented only through public-health advocates rather than directly, and outlet-siting decisions are typically made without binding local community input.
% DISAPPEARANCE_RATIONALE: If state licensing and quality-control authority disappeared overnight, either the market reverts to unregulated/criminal supply (restoring the poisoning and enforcement-violence problems this reading was built to solve) or continues unregulated, exposing users to variable potency and adulteration with no tax capture or quality assurance — either direction is a substantial rearrangement, not a null result.
% FOUNDING_PROBLEM: Criminalized or unregulated drug markets produced violent illicit-supply competition, unknown-purity product causing preventable poisoning deaths, and enforcement costs that fell disproportionately on marginalized users without reducing use — the legalization reading proposes market regulation as a structural fix to all three simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers outside the licensed industry corroborate reduced overdose/poisoning variance and reduced criminal-justice contact for users in jurisdictions that adopted this reading; however, the same independent research documents that informal-market displacement and use-volume increases persist as unresolved side effects, meaning the founding problem is only partially retired rather than fully solved.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) and rising modestly over the interval — legalization removes the severe extraction of criminal-market violence and poisoning risk, but introduces a milder, more diffuse extraction as licensed incumbents consolidate market share and informal producers are squeezed out under a legal regime with no exemption for those unable to meet capital requirements. Suppression is moderate and DECLINING over the interval (0.45→0.35) — this is the reading's central structural claim: legalization substitutes market licensing for criminal enforcement as the coordination mechanism, so the raw suppressive apparatus needed to hold the arrangement together decreases relative to a prohibition baseline, even though a non-zero enforcement function (licensing perimeter policing) remains. Theater ratio is low and rising slightly (0.12→0.20) reflecting the residual performative layer of compliance marketing ('regulated and safe') that grows as the licensed industry matures its public messaging.
 *
 * PERSPECTIVAL GAP:
 *   The state regulatory agency and licensed producers should compute this constraint as much closer to a genuine coordination structure (rope-like) — real safety, tax, and violence-reduction functions are present and visible from their seat. Informal growers/sellers should compute it as extractive and exclusionary — the same licensing apparatus that removes ordinary users from the criminal-justice victim set puts them INTO a sharper one, since 'the market had no legal channel' is no longer a mitigating narrative once legalization exists. This is the seat divergence the tangled_rope classification is designed to hold rather than average away.
 *
 * DIRECTIONALITY LOGIC:
 *   Licensed producers and former illicit-market users get low derived d — they gain price premium/legal cover or gain safety/decriminalization respectively, both net beneficiaries of the arrangement as structured. Informal growers/sellers get high derived d — trapped exit, powerless, directly displaced by the licensing threshold that legalization introduces. Heavy-use populations and outlet-adjacent communities sit closer to symmetric-but-tilted-toward-target: they get some protective benefit from quality control but bear the downside of commercialized availability with limited say over marketing/siting decisions, hence constrained rather than trapped exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (violent illicit competition + poisoning deaths + disproportionate enforcement cost) is only partially retired: criminal-justice contact and poisoning variance for MAINSTREAM users are substantially reduced (founding problem 'dead' for that population), but informal-market displacement and use-volume-driven harm concentrate on a different, still-live population (heavy users, informal sellers). Classifying this as tangled_rope rather than declaring blanket victory (rope) or blanket failure (snare) prevents both mislabelings: it is not pure extraction dressed as coordination, because the safety/tax coordination function is real and independently verifiable; but it is not pure coordination either, because a real victim set persists structurally, not incidentally, and requires ongoing licensing enforcement to maintain the boundary between the sanctioned and unsanctioned market.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informal_market_criminalization_intensification,
    'Does legalization''s licensing threshold effectively criminalize informal producers/sellers MORE severely than status-quo prohibition did, by removing the ''no legal alternative existed'' framing that softened enforcement discretion pre-legalization?',
    'Compare enforcement intensity (arrest rates, sentence severity) against informal/unlicensed sellers before and after legalization in jurisdictions that adopted licensing regimes, controlling for overall market size.',
    'If enforcement against informal sellers intensifies post-legalization, the tangled_rope classification is strongly supported — the same act that liberates ordinary users tightens the noose on displaced informal producers. If enforcement is unchanged or relaxes, the victim-set claim for informal_growers_and_sellers weakens and the constraint moves closer to a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_market_criminalization_intensification, empirical, 'Whether legalization sharpens or softens enforcement against displaced informal-market actors.').

omega_variable(
    use_volume_increase_causal_attribution,
    'Is any observed increase in use volume post-legalization attributable to the commercial/marketing dynamics of the licensed market, or to reduced stigma/detection effects that would occur under any legalization-adjacent reform including harm_reduction_reading?',
    'Compare use-volume trajectories across jurisdictions adopting legalization_reading vs. harm_reduction_reading vs. maintained prohibition, isolating the commercial-marketing variable.',
    'If volume increase tracks commercial marketing specifically (rather than general destigmatization), it strengthens the case that the beneficiary structure (licensed producers profiting from volume) actively works against the protective interest of heavy_use_populations — sharpening the tangled_rope diagnosis rather than treating it as an unavoidable side effect of any reform direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(use_volume_increase_causal_attribution, empirical, 'Whether use-volume increase is a commercial-incentive effect specific to this reading or a general legalization-adjacent effect.').

omega_variable(
    framing_kernel_vs_institution,
    'Is the object under contest here the STATE''S AUTHORITY to regulate (an institutional/legal-power framing) or the LEGITIMACY CLAIM that market mechanisms are the correct instrument for drug policy (a policy-philosophy framing layered above the institutional authority)?',
    'Track whether legislative and judicial contests over this reading focus on the state''s constitutional/statutory authority to license (institutional framing) or on contested empirical/normative claims about market efficacy vs. criminal or public-health approaches (legitimacy-claim framing).',
    'Under the institutional framing, this constraint''s authority_grounding is closer to ''practice'' (the regulatory apparatus interpreting its own licensing mandate); under the legitimacy-claim framing, authority_grounding is closer to ''expertise'' (economists/public-health researchers adjudicating whether market regulation actually achieves its stated goals). The chosen framing (institutional/practice) was selected here because the kernel context frames this as a question of STATE AUTHORITY specifically, per the source material''s own language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_kernel_vs_institution, conceptual, 'Whether the contested object is institutional licensing authority or the underlying policy-legitimacy claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__legalization_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__legalization_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__legalization_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__legalization_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__legalization_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__legalization_reading, base_extractiveness, 16, 0.4).
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
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'drug policy authority' concept per the ε-invariance principle: prohibition_reading (criminalization, high ε concentrated on user/community victim set), legalization_reading (this story — moderate ε, licensing-based victim set shifted to informal producers and use-volume-exposed populations), and harm_reduction_reading (state accepts use, focuses on health-service intervention without commercial licensing, expected lower ε with a smaller, more service-dependent beneficiary/victim structure). Each reading emits a structurally distinct constraint with its own beneficiary/victim declarations; they are linked here, not merged, because measuring 'drug policy' by different observables (criminal-justice outcomes vs. market outcomes vs. health outcomes) yields genuinely different ε values, which is the decomposition trigger, not a modeling convenience.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
