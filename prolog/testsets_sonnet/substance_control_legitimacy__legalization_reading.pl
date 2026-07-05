% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Legalization Reading: Adult Autonomy Bounded by Third-Party Harm Prevention
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the legalization reading of the contested
 *   substance-control kernel: competent adults hold a presumptive right to
 *   use substances, and state authority is legitimate only insofar as it
 *   targets and prevents harm to third parties, not the user's own choices.
 *   Under this reading, the harm-principle boundary reorganizes the
 *   constraint's beneficiary and victim structure relative to the prohibition
 *   and harm-reduction readings: users themselves largely exit the victim set
 *   (they are treated as autonomous agents bearing their own risk), and the
 *   primary victims become non-consenting third parties — people harmed by
 *   impaired drivers, secondhand exposure, and the externalities of a
 *   commercialized legal market. A new extraction vector opens that neither
 *   sibling reading has in the same form: licensed corporate actors capture
 *   commercial surplus from a legal market and have a structural incentive to
 *   maximize consumption, which sits in tension with the harm-prevention
 *   mandate that legitimizes the whole framework. Enforcement (DUI/DUID law,
 *   licensing, advertising limits) persists but is retargeted from use itself
 *   to the negative externalities of use.
 *
 * KEY AGENTS:
 *   - competent_adult_users: primary beneficiary (moderate/mobile) — retains autonomy, bears own risk
 *   - licensed_cannabis_and_alcohol_corporations: commercial beneficiary and de facto agenda-setter (organized/arbitrage) — captures surplus, incentivized to expand consumption
 *   - state_tax_authorities: fiscal beneficiary and agenda-setter (institutional/analytical) — collects revenue, conflicted mandate
 *   - third_party_harm_bystanders: primary victim (powerless/trapped) — bears uncompensated externalities
 *   - impaired_driving_victims: primary victim (powerless/trapped) — the harm the framework claims to specifically target
 *   - low_income_heavy_users_facing_commercialized_marketing: secondary victim despite nominal beneficiary status (powerless/constrained) — autonomy exercised inside an engineered-consumption market
 *   - public_health_regulators: agenda-setter (institutional/analytical) — mediates the tax-revenue vs. harm-prevention tension
 *   - prohibitionist_advocacy_groups: excluded voice (organized/constrained) — locked out of routine regulatory design post-legalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.52).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.28).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Legalization Reading: Adult Autonomy Bounded by Third-Party Harm Prevention").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, 'e72c974b-034a-452b-99ac-ba6f8ac8ee87').
narrative_ontology:cs_kernel_codification('e72c974b-034a-452b-99ac-ba6f8ac8ee87', distributed).
narrative_ontology:cs_authority_grounding('e72c974b-034a-452b-99ac-ba6f8ac8ee87', distributed).
narrative_ontology:cs_reading_relation('e72c974b-034a-452b-99ac-ba6f8ac8ee87', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('e72c974b-034a-452b-99ac-ba6f8ac8ee87', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('e72c974b-034a-452b-99ac-ba6f8ac8ee87', foundational, competent_adult_autonomy_is_primary).
narrative_ontology:cs_axiom_status(competent_adult_autonomy_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('e72c974b-034a-452b-99ac-ba6f8ac8ee87', competent_adult_autonomy_is_primary, deontological).
narrative_ontology:cs_axiom('e72c974b-034a-452b-99ac-ba6f8ac8ee87', foundational, state_authority_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_authority_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('e72c974b-034a-452b-99ac-ba6f8ac8ee87', state_authority_limited_to_third_party_harm, deontological).
narrative_ontology:cs_reference_frame('e72c974b-034a-452b-99ac-ba6f8ac8ee87', harm_principle_liberal_boundary).
narrative_ontology:cs_drift_state('e72c974b-034a-452b-99ac-ba6f8ac8ee87', post_commercialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e72c974b-034a-452b-99ac-ba6f8ac8ee87', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adult_users).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, licensed_cannabis_and_alcohol_corporations).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, state_tax_authorities).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_party_harm_bystanders).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, impaired_driving_victims).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, low_income_heavy_users_facing_commercialized_marketing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, low_income_heavy_users_facing_commercialized_marketing).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, harm_principle_as_limit_on_state_authority).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, bodily_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase and consume regulated substances (alcohol, and in legalized jurisdictions, cannabis) without criminal liability so long as their use does not harm others. They bear taxes and regulatory compliance costs but retain choice over consumption; exit from any single regulatory regime is possible via jurisdiction shopping or simply abstaining.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adult_users, beneficiary,
    moderate, biographical, mobile, national).

% Operate licensed production, distribution, and marketing under the legalization framework. Lobby for favorable licensing rules, tax structures, and marketing latitude. Capture most of the commercial surplus created by legal status while the state's harm-prevention mandate limits but does not eliminate their ability to intensify use through advertising and product potency.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, licensed_cannabis_and_alcohol_corporations, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, licensed_cannabis_and_alcohol_corporations, agenda_setter).

% Collect excise revenue from legal substance sales and set licensing and taxation policy. Have a direct fiscal interest in sustained consumption volume, which sits in tension with any duty to minimize third-party harm through use reduction.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_tax_authorities, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, state_tax_authorities, agenda_setter).

% Non-users exposed to secondhand smoke, impaired drivers, or public intoxication effects. They did not choose the exposure and have no direct voice in licensing or enforcement decisions; their only recourse is after-the-fact civil or criminal claims once harm has already occurred.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, third_party_harm_bystanders, payer,
    powerless, immediate, trapped, local).

% Bear the concrete third-party harm the legalization reading's harm principle is supposed to prevent — injury or death from substance-impaired driving. Enforcement (DUI/DUID law, sobriety checkpoints) is the constraint's active suppression mechanism, applied after legalization has already expanded the population of users.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, impaired_driving_victims, payer,
    powerless, immediate, trapped, national).

% Gain formal legal autonomy over use but face aggressive marketing, high-potency product proliferation, and normalized availability from a commercialized legal industry with financial incentive to maximize consumption. Their nominal autonomy is real but exercised inside a market actively engineered to increase their use.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, low_income_heavy_users_facing_commercialized_marketing, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, low_income_heavy_users_facing_commercialized_marketing, beneficiary).

% Administer licensing, potency limits, advertising restrictions, and DUI enforcement standards under the harm-principle mandate. Structurally positioned between the fiscal interest in tax revenue and the harm-prevention duty that legitimizes the whole framework; their enforcement choices determine how much of the third-party harm gets internalized versus externalized onto bystanders.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Argue the legalization reading understates aggregate harm and that the harm principle is being applied too narrowly (excluding harm to the user's own family, community norms, and long-term public health costs). Largely locked out of licensing and regulatory design once legalization is enacted; their voice persists mainly in periodic ballot fights and legislative rollback attempts.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibitionist_advocacy_groups, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, legally legible boundary — third-party harm — past which state coercion is justified, allowing competent adults to coordinate around predictable rules for lawful use, production, and sale instead of a blanket prohibition regime that pushes all transactions underground.
% TRANSFER_FUNCTION: Moves tax revenue from consumption to the state, moves commercial surplus from consumption to licensed producers and retailers, and moves uncompensated harm costs (injury, secondhand exposure, impaired-driving deaths) from the state/producers onto bystanders who never consented to the transaction.
% ABSENT_VOICES: Prohibitionist advocacy groups and public health researchers who study aggregate-harm externalities are structurally absent from ongoing licensing and marketing-rule design once a legalization regime is enacted; their objections resurface mainly in referenda or attempted rollback legislation, not in routine regulatory process.
% DISAPPEARANCE_RATIONALE: If the legalization framework vanished overnight, licensed markets would collapse into either criminalized underground markets or a harm-reduction/decriminalization regime; tax revenue streams would disappear, corporate actors would lose licensed status, and the third-party harm exposure currently mediated through regulated products (potency labeling, sobriety enforcement) would shift into unregulated channels — a substantial rearrangement, not a null result.
% FOUNDING_PROBLEM: Blanket criminalization of substance use was producing enforcement costs, mass incarceration, and black-market violence disproportionate to the harms of use itself, while doing little to prevent the genuine third-party harms (impaired driving, exposure) that justify state involvement at all.
% FOUNDING_PROBLEM_CORROBORATION: Independent criminological research and cost-benefit studies from public health economists outside the cannabis and alcohol industries corroborate that criminalization's enforcement costs exceeded its harm-prevention benefits, supporting the founding problem as once-live. However, addiction medicine researchers and prohibitionist advocacy groups — also outside the beneficiary set — contest whether the legalization reading has genuinely solved the harm-prevention problem or merely relocated harm from state punishment to commercialized overconsumption, so the founding problem's current status remains disputed rather than settled.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects moderate-to-substantial extraction concentrated in the commercial and externality layers: licensed producers capture surplus from a market structurally incentivized toward overconsumption, and third parties bear uncompensated harm costs. This is lower than a straightforward corporate-capture snare because genuine coordination value exists (predictable legal rules replace black-market chaos) and users retain real autonomy and exit. Suppression (0.28) is comparatively low and declining over the interval — legalization by design reduces the coercive apparatus applied to users themselves, though DUI/DUID enforcement against third-party-harm-producing conduct remains active. Theater ratio (0.30) captures a moderate but rising share of regulatory activity (packaging warnings, responsible-use campaigns) that is more performative than harm-reducing, alongside genuinely functional licensing and potency-limit enforcement. Accessibility collapse (0.35) is moderate: alternatives (abstaining, jurisdiction shopping, black-market avoidance) remain genuinely available, unlike a mountain-type collapse. Resistance (0.45) reflects the active political contest between prohibitionist rollback efforts and continued liberalization pressure — this reading is far from settled.
 *
 * PERSPECTIVAL GAP:
 *   From the competent adult user's seat, this constraint reads close to a rope: a coordination mechanism that replaced criminalization with a workable legal framework, net beneficial. From the third-party harm bystander's seat, the same structure reads as tangled rope shading toward snare: they bear externalized costs from a commercial system they never consented to and have no voice in regulating. The engine should compute this divergence directly from the beneficiary/victim/power/exit declarations rather than from any authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent adult users and licensed corporations sit near the beneficiary end of directionality — the former through restored autonomy and low suppression, the latter through captured commercial surplus and organized lobbying power. Third-party harm bystanders and impaired-driving victims sit near the full-target end: powerless, trapped (they cannot avoid exposure to others' legal use), and bearing costs the framework was specifically supposed to prevent. Low-income heavy users occupy an intermediate position — nominally autonomous beneficiaries who function partly as targets of a commercialized market's consumption-maximizing incentives; this is captured via their dual role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (excessive criminalization enforcement costs relative to harm prevented) was substantially real and is corroborated by independent criminological and economic research outside the industries that now benefit from legalization — this blocks a naive mandatrophy read where the whole framework is dismissed as captured. But the fixing_cost/gain_flow surface reveals a live tension: commercial actors and tax authorities now have a vested interest in sustained or growing consumption that partially diverges from the harm-prevention mandate that legitimizes the arrangement. The founding_problem_status is authored as contested rather than dead precisely because this reading has not fully resolved into either a captured-industry snare or a genuinely harm-minimizing rope — it is measured here as tangled rope, with the coordination function real and the extraction real and structurally distinct from the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_principle_scope_ambiguity,
    'Does ''harm to third parties'' include diffuse societal costs (healthcare system burden, family/community harm from addiction) or only direct, identifiable harms (impaired driving injuries, secondhand exposure)? The legalization reading''s entire legitimacy claim depends on where this line is drawn.',
    'Track how courts and regulators actually apply the harm principle in licensing disputes and criminal liability cases — a narrow application (direct harm only) versus broad application (diffuse social cost) produces materially different victim sets and extraction levels.',
    'A narrow harm-principle reading keeps this constraint closer to a low-extraction rope; a broad reading that still permits commercial sale would expose the framework as internally inconsistent — claiming a harm-prevention mandate broad enough to justify some restriction while permitting an industry structurally organized to maximize the very harm being restricted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_principle_scope_ambiguity, conceptual, 'How broadly ''third-party harm'' is construed determines this reading''s coherence and extraction level.').

omega_variable(
    commercial_capture_vs_genuine_liberalization,
    'Is the legalization reading, as actually implemented, closer to genuine adult-autonomy liberalization or to a switch from state-run prohibition extraction to corporate-run commercialization extraction?',
    'Compare consumption-intensity trends, marketing spend, and product-potency escalation in legalized jurisdictions against harm-reduction-only jurisdictions (e.g., Portugal-style decriminalization without commercial legal markets) to isolate the commercialization-specific effect.',
    'If legalized markets show consumption/harm trends similar to decriminalized-but-non-commercial regimes, the corporate beneficiary''s extraction is modest; if legalized markets show significantly higher consumption growth and harm concentrated in low-income heavy users, this reading is more tangled-rope-to-snare than currently scored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_capture_vs_genuine_liberalization, empirical, 'Whether commercialization independently drives extraction beyond what pure decriminalization would produce.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''competent adult'' premise itself a stable, non-contested foundation, or does it hide unresolved questions about addiction''s effect on competence that would blur the boundary between this reading and the harm_reduction_reading?',
    'Addiction medicine literature on whether chronic substance dependency undermines the autonomous-choice premise this reading relies on; if dependency substantially degrades competence for a large share of users, the autonomy framing may not hold for exactly the population generating the most third-party harm.',
    'If competence is substantially undermined by dependency in the harm-generating population, the legalization reading''s foundational axiom (competent_adult_autonomy_is_primary) weakens, and the constraint''s actual operation may be closer to the harm_reduction_reading than its declared framing suggests — this is a conceptual, not merely empirical, ambiguity about which reading the observed institution actually is.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the competence premise holds for the population most responsible for third-party harm, threatening this reading''s distinctness from harm_reduction_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__legalization_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__legalization_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__legalization_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__legalization_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__legalization_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__legalization_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__legalization_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__legalization_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__legalization_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__legalization_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__legalization_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__legalization_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__legalization_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__legalization_reading, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__legalization_reading, suppression_requirement, 24, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the substance_control_legitimacy kernel. prohibition_reading grounds state authority in a moral duty to prevent harm through criminalization (users are the primary victim set, extraction runs through the carceral apparatus). harm_reduction_reading grounds state authority in a public-health duty to minimize harm without criminalizing use (users remain a protected class needing state support, not autonomous risk-bearers). This reading (legalization_reading) is structurally distinct from both: it removes users from the victim set entirely, narrows state authority to third-party harm alone, and introduces a corporate commercialization extraction vector absent from either sibling. Each story carries its own ε, beneficiary/victim structure, and classification; they are linked here as siblings in the same commitment-system contest, not merged into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
