% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint models the legalization reading of state authority over
 *   drug markets: the state exits the criminalization business and instead
 *   licenses production, sale, and distribution as regulated commerce, using
 *   potency limits, testing requirements, retail density controls, and
 *   taxation as its instruments. This is distinct from the prohibition
 *   reading (criminalizing use/possession to protect third parties from
 *   disorder) and the harm-reduction reading (accepting use while minimizing
 *   health harms through public health interventions rather than commercial
 *   licensing). Under legalization, users exit both the criminal-record
 *   victim set and the unregulated-supply-poisoning victim set that
 *   prohibition generated, but a new victim set emerges: unlicensed legacy
 *   participants who cannot meet licensing capital requirements, and heavy
 *   users exposed to a commercial industry with a structural incentive to
 *   maximize consumption. The ε for this reading is distinct from the ε for
 *   prohibition (which extracts primarily through incarceration and asset
 *   forfeiture) and from harm reduction (which is not commerce-based and
 *   carries much lower extraction because no commercial actor profits from
 *   volume). These are three separate constraints with three separate
 *   extraction profiles, linked as siblings of one kernel.
 *
 * KEY AGENTS:
 *   - state_regulatory_agency: agenda_setter (institutional/analytical) — designs licensing regime, collects fees and taxes
 *   - licensed_producers_and_retailers: beneficiary (organized/mobile) — profits from legal commerce structure
 *   - former_illicit_market_users: beneficiary/payer (powerless/constrained) — exits criminal risk, bears commercial marketing exposure
 *   - unlicensed_legacy_market_participants: payer (powerless/trapped) — criminalized more sharply once a legal lane exists
 *   - public_health_researchers: observer (analytical) — evaluates whether the reading delivers on its harm-reduction and market-elimination claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.42).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, 'c9d45dff-7a61-460a-9e17-fada8697765c').
narrative_ontology:cs_kernel_codification('c9d45dff-7a61-460a-9e17-fada8697765c', distributed).
narrative_ontology:cs_authority_grounding('c9d45dff-7a61-460a-9e17-fada8697765c', distributed).
narrative_ontology:cs_reading_relation('c9d45dff-7a61-460a-9e17-fada8697765c', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('c9d45dff-7a61-460a-9e17-fada8697765c', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('c9d45dff-7a61-460a-9e17-fada8697765c', foundational, regulated_commerce_reduces_net_harm_versus_criminalization).
narrative_ontology:cs_axiom_status(regulated_commerce_reduces_net_harm_versus_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('c9d45dff-7a61-460a-9e17-fada8697765c', regulated_commerce_reduces_net_harm_versus_criminalization, empirically_contingent).
narrative_ontology:cs_axiom('c9d45dff-7a61-460a-9e17-fada8697765c', secondary, state_licensing_capacity_is_adequate_substitute_for_criminal_deterrence).
narrative_ontology:cs_axiom_status(state_licensing_capacity_is_adequate_substitute_for_criminal_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('c9d45dff-7a61-460a-9e17-fada8697765c', state_licensing_capacity_is_adequate_substitute_for_criminal_deterrence, instrumental).
narrative_ontology:cs_reference_frame('c9d45dff-7a61-460a-9e17-fada8697765c', commercial_licensing_as_harm_substitution).
narrative_ontology:cs_drift_state('c9d45dff-7a61-460a-9e17-fada8697765c', post_legalization_market_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c9d45dff-7a61-460a-9e17-fada8697765c', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_producers_and_retailers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_tax_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, former_illicit_market_users).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, criminal_justice_diversion_beneficiaries).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, unlicensed_legacy_market_participants).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, heavy_users_facing_commercialized_marketing).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, communities_near_licensed_outlets).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, excluded_small_producers_lacking_licensing_capital).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, former_illicit_market_users).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, regulated_commerce_reduces_net_harm_versus_prohibition).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, state_licensing_capacity_can_substitute_for_criminal_deterrence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the licensing regime: sets potency limits, retail density, advertising restrictions, testing standards, and tax rates. Collects licensing fees and tax revenue. Can expand or contract regulatory scope but is politically exposed if either overdose deaths or illicit-market resurgence rise under its watch.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agency, agenda_setter,
    institutional, generational, analytical, national).

% Operate within the new legal market, paying licensing fees and taxes in exchange for legal protection, brand-building, and access to formal supply chains. Lobby to loosen restrictions once established, benefiting from incumbency the regulatory scheme itself created.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_producers_and_retailers, beneficiary,
    organized, biographical, mobile, national).

% Receive excise and sales tax revenue from the newly legal commerce, revenue that did not previously exist in taxable form. Have a direct fiscal interest in market volume growing, which sits in tension with public-health goals of reducing consumption.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Exit the criminal-record risk and unregulated-supply poisoning risk that prohibition imposed. Gain predictable potency and legal recourse. Bear commercialized pricing, marketing exposure, and any new taxes passed through to retail price; some are structurally targeted by industry marketing optimized for heavy repeat use.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, former_illicit_market_users, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, former_illicit_market_users, payer).

% Former illicit-market growers, couriers, and small sellers who lack the capital, records, or credentials to obtain licenses. Are criminalized more sharply post-legalization because the legal channel now defines the only legitimate lane; enforcement against remaining illicit supply intensifies precisely because a legal alternative exists.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, unlicensed_legacy_market_participants, payer,
    powerless, biographical, trapped, local).

% Face a commercial industry with a structural incentive to maximize consumption frequency and potency-seeking behavior, since legal firms profit from volume the same way alcohol and tobacco firms do. Public-health framing of legalization does not remove this commercial incentive; it relocates it inside a regulated market.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, heavy_users_facing_commercialized_marketing, payer,
    powerless, biographical, identity_locked, national).

% Bear externalities of outlet density, local traffic in intoxicated persons, and public consumption, often concentrated in lower-income neighborhoods where licensing costs are lower and zoning resistance is weaker.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, communities_near_licensed_outlets, payer,
    moderate, biographical, constrained, local).

% Would want a seat in setting licensing thresholds — the capital and compliance requirements that determine who can legally enter the market they once served illegally — but are not represented in the regulatory design process, which is dominated by incumbent capital and existing licensed operators.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, excluded_small_producers_lacking_licensing_capital, excluded,
    powerless, biographical, trapped, local).

% Track overdose rates, use prevalence, market concentration, and criminal justice outcomes across legalization jurisdictions to evaluate whether the regime is delivering the harm reduction and market elimination it claims.
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
% COORDINATION_FUNCTION: Provides a mechanism for coordinating drug supply, quality assurance, and access controls under transparent state administration, replacing the coordination failures of an unregulated illicit market (unknown potency, contamination, violence-based dispute resolution) with licensing, testing, and taxation.
% TRANSFER_FUNCTION: Moves the drug trade's economic surplus from criminal organizations and unlicensed sellers to licensed firms and state tax authorities; moves risk from criminalized users (arrest, unregulated-supply poisoning) toward commercialized consumption risk (marketing-driven overuse) and toward those excluded from licensing.
% ABSENT_VOICES: Unlicensed legacy market participants and would-be small producers who cannot meet licensing capital requirements are not represented in the regulatory design process; they experience intensified enforcement post-legalization without having had input into the licensing thresholds that exclude them.
% DISAPPEARANCE_RATIONALE: If state licensing authority over the drug market disappeared overnight, the legal supply chain would collapse into either unregulated criminal markets (reverting toward the prohibition-era structure) or into an entirely unregulated legal free-for-all with no quality or access controls — either way, the entire apparatus of licensed retailers, tax revenue streams, and quality-testing infrastructure would need to be rebuilt or abandoned.
% FOUNDING_PROBLEM: Prohibition-era drug markets produced widespread unregulated-supply poisoning, mass criminalization of users, and violent illicit-market competition; legalization was built to move the trade into a taxed, quality-controlled, licensed channel that reduces those specific harms.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers in multiple legalized jurisdictions attest that overdose deaths from contamination and arrest rates for simple possession have measurably declined, corroborating that part of the founding problem was live and has been substantially addressed. Independent economists and community organizations in the same jurisdictions attest that market concentration among well-capitalized licensed operators and continued criminalization of unlicensed legacy sellers show the founding problem has been only partially solved, with a new distributional problem (who gets licensed) substituted in its place — this corroboration comes from outside the licensed industry, which has an interest in declaring the founding problem fully resolved.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) is moderate: a genuine coordination function exists (quality control, predictable supply, tax capture from an activity previously untaxed), but commercial licensing creates a new extraction channel — profit-maximizing firms structurally incentivized toward consumption volume, and a licensing threshold that excludes legacy participants without capital. Suppression (0.35) is lower than under prohibition because criminalization of the *use* itself ends, but does not go to zero: enforcement against unlicensed supply persists and, per the founding-problem corroboration, can intensify once a legal alternative exists (illicit supply no longer has an ambiguity defense). Theater ratio (0.28) reflects a real but partial coordination function — testing and licensing infrastructure does meaningful work, but a portion of compliance activity (retail signage, point-of-sale warnings) is performative relative to the underlying commercial incentive structure. Accessibility collapse (0.4) is moderate: once legalized, informal/illicit alternatives do not fully disappear (a genuine illicit market persists at the margins, particularly where licensing is expensive), so alternatives collapse only partially. Resistance (0.55) is substantial: communities near outlets, public-health advocates concerned about commercialization, and excluded legacy producers all actively contest specific regulatory design choices.
 *
 * PERSPECTIVAL GAP:
 *   From the state regulatory agency's seat, this looks like successful problem-resolution: crime down, tax revenue up, health outcomes improved on the poisoning/overdose axis. From the unlicensed legacy participant's seat, the same structure looks like an enforcement intensification dressed in public-health language — the legal alternative removes their ambiguity defense and legitimizes more aggressive action against remaining illicit supply. The engine should compute these as different seat-level types from the same structural data, not reconcile them to one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Licensed producers and state tax authorities sit near the full-beneficiary end: they collect fees, revenue, and market position the regulatory structure itself creates. Former illicit-market users are genuine beneficiaries on the criminalization axis (d shifts toward beneficiary there) but payers on the commercialization axis (d shifts toward target on marketing/pricing exposure) — this dual position is captured with a secondary_role rather than forced into one seat. Unlicensed legacy participants and heavy users facing commercialized marketing sit near the full-target end: the former because they are structurally excluded from the legal channel and face intensified enforcement, the latter because commercial firms are incentivized to target them specifically. Excluded small producers are the clearest absent-voice case: they are payers of the exclusion but have no role in setting the thresholds that exclude them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unregulated-supply poisoning, mass criminalization of simple possession) is substantially addressed where legalization has been implemented, which is real coordination function, not cover story — this is why the story does not classify as snare. But the tangled_rope classification holds because a second, less visible transfer has been substituted: from criminal-market violence extraction toward commercial-market volume extraction, with a partially new victim class (unlicensed legacy participants, marketing-targeted heavy users) replacing the old one (arrested users, poisoned users). Declaring mandatrophy_resolved would overstate the case — the original problem is only partially retired, and a structurally distinct extraction channel has taken its place under the same institutional label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_incentive_versus_public_health_goal,
    'Can a for-profit licensed commercial market structurally deliver reduced overall consumption, or does profit-maximization under licensing inevitably drift the market toward the same volume-maximizing incentives that characterize alcohol and tobacco industries?',
    'Longitudinal comparison of consumption volume, heavy-user concentration of sales, and marketing expenditure across jurisdictions with different licensing/advertising restriction regimes, benchmarked against the alcohol/tobacco commercialization precedent.',
    'If commercial incentives reliably override public-health intent, the legalization reading''s coordination claim weakens over time relative to its extraction profile, pushing the classification toward snare as theater_ratio and extractiveness both climb; if restrictions can durably constrain the commercial incentive, the tangled_rope classification with a real coordination function is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_incentive_versus_public_health_goal, empirical, 'Whether licensed commercial supply structurally reproduces the volume-maximization dynamics of other legal vice industries.').

omega_variable(
    licensing_threshold_as_new_exclusion_mechanism,
    'Is the licensing capital/compliance threshold a neutral administrative necessity for quality control, or a constructed barrier that reallocates market position from illicit incumbents to capital-holding entrants regardless of prior market experience?',
    'Comparative analysis of licensing design across jurisdictions: do lower-barrier licensing tracks (e.g., social equity licensing programs) produce meaningfully different participant demographics than high-barrier tracks, controlling for market size?',
    'If the threshold is primarily an exclusion mechanism rather than a quality-control necessity, the victim set (excluded small producers, unlicensed legacy participants) is larger and more central to the constraint''s actual operation than the beneficiary-facing coordination story suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_threshold_as_new_exclusion_mechanism, conceptual, 'Whether licensing capital requirements are a necessary quality-control feature or a constructed market-entry barrier.').

omega_variable(
    kernel_reading_selection_as_political_outcome,
    'Is the choice among the prohibition, harm-reduction, and legalization readings of the substance_control_authority kernel determined by empirical evidence about harm outcomes, or by which reading best serves the political-economic interests of the parties positioned to become licensed incumbents?',
    'Track which reading gets adopted in jurisdictions with strong existing licensed-industry lobbying capacity (alcohol, tobacco, pharmaceutical) versus jurisdictions without it, controlling for measured harm profiles under prior regimes.',
    'If reading-selection tracks incumbent lobbying capacity more than harm evidence, the legalization reading''s self-presentation as the empirically superior reading is partly cover for a redistribution of extraction rights toward capital-holding entrants — this would not change this story''s own ε, but would bear on how the sibling relationship should be interpreted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_as_political_outcome, conceptual, 'Whether kernel-reading adoption is evidence-driven or interest-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__legalization_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__legalization_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__legalization_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__legalization_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__legalization_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__legalization_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__legalization_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__legalization_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__legalization_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__legalization_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__legalization_reading, 0.15).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the substance_control_authority kernel. The prohibition_reading and harm_reduction_reading are separate constraint files with distinct ε values and distinct victim/beneficiary structures — prohibition extracts primarily through incarceration and asset forfeiture (much higher suppression, victims are all users/possessors); harm_reduction has a public-health service delivery mechanism with no commercial extraction channel (much lower extraction, no commercial beneficiary class). This legalization reading sits structurally between them: lower suppression than prohibition (criminalization of use ends) but non-trivial extraction via commercialization (unlike harm reduction, which has no profit-seeking intermediary). The three readings are linked via affects_constraints because adopting one reading directly forecloses or displaces the political space for the others within a given jurisdiction at a given time — a jurisdiction cannot simultaneously operate full prohibition and full commercial legalization for the same substance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__legalization_reading, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
