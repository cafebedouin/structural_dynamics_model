% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Legalization-and-Taxation Reading of Substance Control
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the legalization reading of the substance control
 *   kernel: substance use is an individual liberty matter, and the state's
 *   only legitimate intervention is at the boundary where use generates
 *   third-party harm (impaired driving, secondhand exposure) or where
 *   externality costs need to be captured through taxation. Under this
 *   reading, the user population that prohibition treated as the primary
 *   target exits the victim set entirely — users are reclassified as
 *   rights-holders and, later, as taxpaying customers. New victims enter:
 *   third parties bearing externality risk, and legacy-market participants
 *   excluded from the licensing regime that replaces criminal enforcement
 *   with capital and regulatory barriers. A legal cannabis/substance industry
 *   and the state's tax apparatus emerge as new, concentrated beneficiaries
 *   where none existed under prohibition. This is a genuinely different
 *   constraint from the prohibition reading (which criminalizes users
 *   directly) and the harm-reduction reading (which treats use as a health
 *   condition independent of legal status) — not a different measurement of
 *   the same one. Each reading has a distinct victim set, distinct ε, and
 *   distinct beneficiary structure, and is authored as its own file per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - adult_recreational_users: former primary target, now beneficiary of legal liberty (moderate/mobile)
 *   - licensed_cannabis_industry: new concentrated beneficiary capturing legal margin (organized/arbitrage)
 *   - state_tax_authorities: new revenue collector, sets excise structure (institutional/analytical)
 *   - dui_and_impairment_third_parties: bears externality risk the reading exists to address (powerless/trapped)
 *   - secondhand_exposure_bystanders: bears diffuse exposure harm (powerless/constrained)
 *   - unlicensed_legacy_market_sellers: excluded from licensing, gray-market persistence (powerless/trapped)
 *   - low_income_users_facing_new_tax_burden: legal but taxed into continued gray-market incentive (powerless/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.42).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Legalization-and-Taxation Reading of Substance Control").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '4f2e2cfa-332e-422b-80bd-a525d1d7a611').
narrative_ontology:cs_kernel_codification('4f2e2cfa-332e-422b-80bd-a525d1d7a611', distributed).
narrative_ontology:cs_authority_grounding('4f2e2cfa-332e-422b-80bd-a525d1d7a611', distributed).
narrative_ontology:cs_reading_relation('4f2e2cfa-332e-422b-80bd-a525d1d7a611', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('4f2e2cfa-332e-422b-80bd-a525d1d7a611', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('4f2e2cfa-332e-422b-80bd-a525d1d7a611', foundational, use_itself_is_not_a_legitimate_state_intervention_trigger).
narrative_ontology:cs_axiom_status(use_itself_is_not_a_legitimate_state_intervention_trigger, holdable).
narrative_ontology:cs_axiom_grounding('4f2e2cfa-332e-422b-80bd-a525d1d7a611', use_itself_is_not_a_legitimate_state_intervention_trigger, deontological).
narrative_ontology:cs_axiom('4f2e2cfa-332e-422b-80bd-a525d1d7a611', secondary, externality_pricing_via_taxation_satisfies_harm_principle).
narrative_ontology:cs_axiom_status(externality_pricing_via_taxation_satisfies_harm_principle, holdable).
narrative_ontology:cs_axiom_grounding('4f2e2cfa-332e-422b-80bd-a525d1d7a611', externality_pricing_via_taxation_satisfies_harm_principle, instrumental).
narrative_ontology:cs_reference_frame('4f2e2cfa-332e-422b-80bd-a525d1d7a611', harm_principle_liberal_governance).
narrative_ontology:cs_drift_state('4f2e2cfa-332e-422b-80bd-a525d1d7a611', post_commercialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f2e2cfa-332e-422b-80bd-a525d1d7a611', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, licensed_cannabis_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_tax_authorities).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, adult_recreational_users).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, dui_and_impairment_third_parties).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, secondhand_exposure_bystanders).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, unlicensed_legacy_market_sellers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, low_income_users_facing_new_tax_burden).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, harm_principle_of_state_intervention).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, externality_internalization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase substances legally through licensed retail, paying an excise-inflated price in exchange for exiting criminal exposure entirely. No longer counted among the constraint's victims under this reading; their liberty interest is the reading's founding premise. Can exit to unlicensed markets if taxes rise too far, but otherwise face few barriers.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, adult_recreational_users, beneficiary,
    moderate, biographical, mobile, regional).

% Operates under state license, captures the legal margin between wholesale cost and taxed retail price, and lobbies for favorable licensing rules and tax structure. Increasingly co-drafts the regulatory regime it operates under, converting a liberty-protecting framework into a rent-protected incumbency.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, licensed_cannabis_industry, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, licensed_cannabis_industry, agenda_setter).

% Sets excise rates, licensing fees, and enforcement priorities; collects substantial new tax revenue justified as internalizing externality costs (enforcement, health system burden, impaired-driving enforcement). Revenue collection creates an incentive to maintain rather than minimize the taxed activity.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_tax_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_tax_authorities, beneficiary).

% Bear the risk and cost of impaired driving, workplace accidents, or other externalities caused by legal users. Have no relationship to the transaction that produced the risk and no exit from exposure to it; their harm is the entire justification the reading offers for any state role at all, yet enforcement of impairment standards lags legal availability.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, dui_and_impairment_third_parties, payer,
    powerless, immediate, trapped, local).

% Neighbors, co-tenants, and children exposed to secondhand smoke or vapor in multi-unit housing and public spaces where use is newly legal. Can move or complain but often lack the resources or standing to escape or enforce restrictions, especially in dense low-income housing.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, secondhand_exposure_bystanders, payer,
    powerless, immediate, constrained, local).

% Former or continuing informal-market participants, often with prior criminal records, who are structurally excluded from licensing (capital requirements, background-check bars, geographic zoning against prior offense locations) even as the substance they once sold is now legal for licensed competitors. Continue operating in the gray market at continued legal risk, undercutting the reading's claim that the black market collapses.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, unlicensed_legacy_market_sellers, excluded,
    powerless, biographical, trapped, local).

% Purchase legally but face compounding excise, sales, and licensing-pass-through taxes that push habitual costs upward, sometimes reintroducing incentive to buy from the untaxed gray market the reading claims should have collapsed.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, low_income_users_facing_new_tax_burden, payer,
    powerless, biographical, constrained, regional).

% Track externality rates (DUI incidence, ER visits, secondhand exposure complaints) and tax revenue allocation to evaluate whether captured externality costs actually fund harm mitigation or are absorbed into general revenue.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, licensed_cannabis_industry).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces criminalized, unregulated substance markets with a licensed, taxed, quality-controlled retail structure, letting adults consume without criminal liability while giving the state a mechanism to price and fund the externalities use generates (impaired driving enforcement, health costs, exposure harms).
% TRANSFER_FUNCTION: Moves substances from criminal-liability exposure to a taxed legal market: users pay excise taxes that flow to the state; licensed producers/retailers capture the legal margin; the tax revenue is nominally earmarked to offset externality costs borne by third parties, though allocation is frequently diffuse or diverted to general funds.
% ABSENT_VOICES: Unlicensed legacy sellers barred from licensing by capital or record requirements would object that legalization protects incumbents rather than dismantling the punitive apparatus that criminalized them; DUI/exposure victims are rarely consulted in tax-rate or licensing design despite being the reading's sole remaining justification for state involvement.
% DISAPPEARANCE_RATIONALE: If the legalization-and-taxation framework vanished overnight, licensed industry and state revenue streams would collapse immediately (world_rearranges for those seats), but many users would simply revert to informal-market purchase with little change to their consumption behavior, and externality rates might barely move without enforcement of impairment/exposure standards separately from the tax-and-license apparatus — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Prohibition-era criminalization produced mass incarceration, black-market violence, adulterated/unsafe products, and no mechanism to price the genuine third-party costs of use; the legalization reading was built to replace punitive control with a liberty-respecting framework that only intervenes at the externality boundary.
% FOUNDING_PROBLEM_CORROBORATION: Legal industry and tax authorities attest the founding problem (mass criminalization, unsafe unregulated products) is substantially solved by licensing and quality control. Independent public health researchers and criminal-justice reform advocates outside the beneficiary set attest that decriminalization of low-income and legacy-market participants remains incomplete — licensing barriers reproduce exclusion along similar lines to the prior criminal regime, and externality enforcement (DUI standards, exposure protections) has not kept pace with commercial expansion, so the founding liberty problem is only partly resolved even as revenue capture is fully realized.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, contested).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).
:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises modestly over the interval (0.22 to 0.42) as licensing regimes mature and excise rates climb — the coordination function (liberty protection, quality control, externality funding) remains real, but the tax-and-license apparatus increasingly extracts rents beyond what externality-internalization requires, particularly from low-income users and excluded legacy sellers. Suppression falls over the interval (0.50 to 0.35) as criminal enforcement against users recedes — this is the reading's central achievement, the collapse of state coercion against the user population itself. Theater ratio rises slowly (0.15 to 0.28) as 'public health' and 'harm reduction' framing is increasingly used to justify tax structures that primarily serve revenue capture rather than externality mitigation.
 *
 * PERSPECTIVAL GAP:
 *   From the state tax authority's seat, this is coordination succeeding exactly as designed: liberty protected, externalities priced, revenue flowing. From the DUI/exposure third party's seat, the same structure looks like a regime that solved the users' criminal-liability problem while leaving the third-party harm problem essentially where it was, since enforcement of impairment standards and exposure protections has not scaled with legal availability. From the unlicensed legacy seller's seat, legalization looks like enclosure: the activity that once carried criminal risk for everyone now carries capital and regulatory barriers that exclude exactly the population most burdened by the prior prohibition regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Adult users derive low d (near-beneficiary) because the reading's entire premise is their exit from the victim set — they trade criminal risk for tax cost, a substantial net improvement. Licensed industry and tax authorities derive low d as concentrated capturers of the legal margin and revenue stream respectively. Third parties bearing externality harm derive high d (near-target) because they bear costs generated by a transaction they have no part in and the reading's own logic makes their harm the sole legitimate trigger for intervention, yet enforcement lags. Legacy market sellers derive high d despite technically being 'excluded' rather than 'targeted' — their trapped exit options and continued legal exposure in the gray market functionally place them near the target end even though the reading nominally decriminalizes the substance itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass criminalization, unsafe unregulated markets, no externality pricing) is only partially resolved: criminalization of adult personal use has substantially receded (live-to-dead transition for that component), but the exclusionary licensing apparatus reproduces some of the same stratification the criminal regime produced, and externality enforcement has not matured at the same pace as commercial legalization. Classifying this as tangled_rope rather than a clean rope prevents mislabeling a structure with real coordination (liberty protection, quality control, externality funding) as pure extraction, while also preventing the legalization industry's revenue-capture dynamics from being laundered as pure coordination success.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substance_control_kernel_reading_disagreement_location,
    'This constraint is one reading (legalization_reading) of the contested substance_control_kernel. The prohibition_reading treats use itself as the wrong the state exists to punish; the harm_reduction_reading treats use as a health condition warranting intervention independent of legal status. Where exactly does the disagreement between readings live?',
    'The disagreement is located at the definition of the state''s legitimate intervention trigger: prohibition triggers on the act of use itself; legalization triggers only at the externality boundary (third-party harm); harm reduction triggers on health outcomes regardless of legal status or externality presence. A sibling reading adopting the legalization framework would remove users from the victim set and add externality-bearing third parties and excluded legacy-market participants — exactly the delta modeled in this story''s beneficiary/victim declarations.',
    'Adopting the prohibition reading instead would restore users to the primary victim set and eliminate the licensed industry beneficiary entirely, producing a structurally different ε and classification (a snare or tangled_rope with users as victims rather than beneficiaries). Adopting the harm_reduction reading would remove the tax-revenue beneficiary framing and center service-provision beneficiaries (clinics, needle exchanges) instead of licensed retailers, again producing a distinct constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substance_control_kernel_reading_disagreement_location, conceptual, 'Documents where the three kernel readings structurally diverge and why each requires its own constraint file.').

omega_variable(
    externality_capture_sincerity,
    'Does the excise tax revenue this reading generates actually flow to funding externality mitigation (DUI enforcement, exposure protection, health system offset), or is externality-internalization framing used to legitimize general revenue collection?',
    'Audit trail of earmarked-versus-general-fund allocation of substance excise revenue across jurisdictions that have implemented this reading; compare mitigation program funding growth against tax revenue growth.',
    'If revenue is substantially diverted to general funds, the reading''s tangled_rope classification strengthens (extraction exceeding the coordination/externality-funding justification); if revenue tracks mitigation spending closely, the constraint moves closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capture_sincerity, empirical, 'Whether the state''s revenue collector role is a genuine externality-funding mechanism or an extraction dressed in externality language.').

omega_variable(
    licensing_exclusion_intentionality,
    'Is the exclusion of legacy-market participants (via capital requirements, background-check bars, zoning) an incidental consequence of a genuine regulatory need for quality control, or a designed barrier protecting incumbent licensed industry from competition?',
    'Compare licensing criteria across jurisdictions with and without social-equity licensing provisions; examine whether capital thresholds and record-based bars track any demonstrated quality/safety risk or primarily track capacity to pay for compliance.',
    'If exclusion is primarily incumbent-protective rather than safety-driven, the licensed_cannabis_industry seat shifts from coordination-beneficiary to something closer to a captured-agenda-setter, strengthening the tangled_rope reading and its enforcement requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_exclusion_intentionality, empirical, 'Whether licensing exclusion of legacy sellers is safety-motivated or rent-protective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__legalization_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__legalization_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__legalization_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(subs_tr_t24, substance_control_kernel__legalization_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__legalization_reading, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__legalization_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__legalization_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(subs_be_t24, substance_control_kernel__legalization_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__legalization_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__legalization_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__legalization_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(subs_su_t24, substance_control_kernel__legalization_reading, suppression_requirement, 24, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the substance_control_kernel. legalization_reading (this file) removes users from the victim set and introduces a licensed-industry beneficiary and third-party externality victim set; prohibition_reading criminalizes users directly with the state as sole enforcement beneficiary; harm_reduction_reading treats use as a health condition with service-provision beneficiaries independent of legal status. Each reading carries a distinct ε and distinct beneficiary/victim structure; they are linked here via affects_constraints rather than merged, per the ε-invariance principle. Legalization structurally influences the other two readings' viability: successful tax capture and industry formation under this reading reduce political appetite for reverting to prohibition, and compete with harm_reduction's service-funding model for the same public health budget lines.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
