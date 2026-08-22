% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Legalization-Reading Substance Control Framework
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the legalization reading of the
 *   substance control kernel: substance use is framed as an individual
 *   liberty issue; the state's legitimate role is limited to preventing
 *   third-party harm and capturing externality costs via taxation and
 *   regulation. Under this reading, users are decriminalized and exit the
 *   victim set; third parties (exposed to secondhand harm, traffic risk,
 *   occupational exposure) enter the victim set via uncompensated
 *   externalities; legal commercial operators become beneficiaries; and the
 *   state becomes a revenue collector administering a regulated market rather
 *   than a punitive enforcement apparatus. This reading coexists with the
 *   prohibition reading (substance use as moral transgression requiring
 *   punishment) and the harm-reduction reading (substance use as health
 *   condition requiring pragmatic intervention independent of legalization
 *   status). The three readings share a contested kernel—what is the nature
 *   of substance use and the state's legitimate authority over it—but each
 *   instantiates a distinct constraint with different beneficiary/victim
 *   structures, extraction mechanisms, and enforcement requirements.
 *
 * KEY AGENTS:
 *   - substance_users: Removed from victim status (decriminalization) and repositioned as beneficiaries; carry new exposure via taxation and regulatory monitoring.
 *   - legal_commercial_operators: Institutional beneficiary; capture rents from licensing and supply monopoly.
 *   - state_revenue_collection: Agenda-setter; administers regime, collects taxes, defines regulatory boundaries.
 *   - third_party_harm_bearers: New victim set under this reading; bear uncompensated externalities (secondhand exposure, traffic risk, developmental harm).
 *   - unregulated_suppliers: Excluded; displaced from market by enforcement but not eliminated.
 *   - formerly_incarcerated_persons: Beneficiary (removed from criminal status) but carrying permanent collateral consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.62).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.41).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Legalization-Reading Substance Control Framework").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '07745774-e15d-43ec-9739-1ad8530f71de').
narrative_ontology:cs_kernel_codification('07745774-e15d-43ec-9739-1ad8530f71de', fixed_text).
narrative_ontology:cs_authority_grounding('07745774-e15d-43ec-9739-1ad8530f71de', distributed).
narrative_ontology:cs_reading_relation('07745774-e15d-43ec-9739-1ad8530f71de', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('07745774-e15d-43ec-9739-1ad8530f71de', substance_control_kernel__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('07745774-e15d-43ec-9739-1ad8530f71de', foundational, individual_liberty_baseline).
narrative_ontology:cs_axiom_status(individual_liberty_baseline, holdable).
narrative_ontology:cs_axiom_grounding('07745774-e15d-43ec-9739-1ad8530f71de', individual_liberty_baseline, deontological).
narrative_ontology:cs_axiom('07745774-e15d-43ec-9739-1ad8530f71de', foundational, state_authority_limited_to_externalities).
narrative_ontology:cs_axiom_status(state_authority_limited_to_externalities, holdable).
narrative_ontology:cs_axiom_grounding('07745774-e15d-43ec-9739-1ad8530f71de', state_authority_limited_to_externalities, deontological).
narrative_ontology:cs_axiom('07745774-e15d-43ec-9739-1ad8530f71de', secondary, externality_internalization_via_taxation).
narrative_ontology:cs_axiom_status(externality_internalization_via_taxation, holdable).
narrative_ontology:cs_axiom_grounding('07745774-e15d-43ec-9739-1ad8530f71de', externality_internalization_via_taxation, instrumental).
narrative_ontology:cs_reference_frame('07745774-e15d-43ec-9739-1ad8530f71de', substance_use_as_individual_liberty).
narrative_ontology:cs_drift_state('07745774-e15d-43ec-9739-1ad8530f71de', contemporary_regulatory_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('07745774-e15d-43ec-9739-1ad8530f71de', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_commercial_operators).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_revenue_collection).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_party_harm_bearers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, regulatory_compliance_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, regulatory_compliance_actors).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, formerly_incarcerated_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under legalization, users are decriminalized and can access regulated supply without criminal liability. They avoid incarceration, felony records, and asset forfeiture. They enter a legal market with quality assurance and medical support. Exit option is straightforward: abstain, migrate to jurisdiction with different rules, or use within legalized bounds. The constraint removes their victim status relative to prohibition.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, substance_users, beneficiary,
    powerless, biographical, mobile, national).

% Licensed producers and distributors operate in a regulated market protected from unlicensed competition. They capture rents from licensing requirements, supply-chain monopolies, and brand capture. They benefit from legal certainty and access to financial and legal systems. They bear compliance costs but treat them as a cost of market legitimacy. Exit option is to leave the jurisdiction or sector.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_commercial_operators, beneficiary,
    institutional, generational, arbitrage, national).

% The state designs and administers the regulatory regime, collecting excise taxes, licensing fees, and administrative revenue. Sets potency limits, packaging standards, age-gating, and marketing restrictions. Enforces the boundary between legal and illegal supply through prosecution of unlicensed operators and tax evasion. No exit option: the state defines the regime.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_revenue_collection, agenda_setter,
    institutional, generational, analytical, national).

% Bear externalities of legalized substance use: secondhand smoke inhalation, impaired-driving accidents, workplace intoxication, pediatric poisoning, developmental harm to children in co-exposed households, noise from production facilities. Exit options are geographic relocation or social avoidance, both costly and incomplete. The constraint assigns them victim status via uncompensated or under-compensated externalities.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_party_harm_bearers, payer,
    powerless, biographical, constrained, national).

% Licensed operators and health professionals bear compliance costs: laboratory testing, record-keeping, pharmacovigilance, reporting to regulators, staff training, facility inspections. They benefit from the barrier-to-entry these costs create (competitors must invest similarly). Small operators bear costs without market-capture benefit; larger operators treat compliance as a cost of legitimacy. Exit is to leave the sector or operate illegally.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, regulatory_compliance_actors, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, regulatory_compliance_actors, beneficiary).

% Black-market and gray-market producers are structurally barred from legal markets by licensing, testing, and tax requirements. They continue operating in illegal space, competing on price and availability. Legalization displaces rather than eliminates them, creating persistent enforcement costs. Excluded from policy conversations; would argue for decriminalization without licensing but have no seat at the negotiating table.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, unregulated_suppliers, excluded,
    powerful, biographical, trapped, national).

% Freed from criminal liability for past possession or distribution under legalization; eligible for record expungement and workforce re-entry. The constraint removes their victim status retroactively. However, collateral consequences persist: employment discrimination, housing barriers, family disruption, loss of custody. The benefit is real but bounded by path dependence and incomplete remedies.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, formerly_incarcerated_persons, beneficiary,
    powerless, biographical, constrained, national).

% Monitor outcomes: use prevalence, addiction rates, emergency-department presentations, overdose deaths, secondhand exposure, traffic fatalities. Provide evidence on whether legalization's stated purpose (prevent third-party harm while respecting user autonomy) is achieved. Assess divergence between nominal and actual functions. Position enables detection of mandatrophy and regulatory drift.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, state_revenue_collection).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal, regulated supply chain for substances that eliminates the coordination problem solved by prohibition (criminal enforcement) and replaces it with the coordination problem solved by legalization (market regulation, quality assurance, taxation, age-gating, externality internalization). Users can access supply without black-market risk; operators can invest in production and distribution without legal jeopardy; the state captures tax revenue and can monitor use patterns.
% TRANSFER_FUNCTION: Moves excise tax revenue (from substance sales) to the state; moves compliance costs (testing, licensing, record-keeping) from consumers to legal operators; moves criminal liability from users to unregulated suppliers; moves administrative costs (inspection, enforcement) from users to the state. Enforcement redirects from individual users to market-boundary maintenance (suppressing unlicensed competitors).
% ABSENT_VOICES: Unregulated suppliers are structurally excluded: they cannot legally participate in the market and are prosecuted for tax evasion and licensing violations. They would argue for decriminalization without licensing (reducing state control further) but have no seat. Users with strong privacy concerns about state surveillance of substance data are marginalized in policy design. Addiction-medicine specialists advocating harm-reduction-first (treatment accessibility independent of legalization) are present but subordinated to the legalization framework.
% DISAPPEARANCE_RATIONALE: If legalization disappeared, users would face re-criminalization and return to black-market supply; incarceration would resume; tax revenue would vanish; compliance infrastructure (testing labs, licensing bureaus) would be decommissioned; and the legal operators' market would collapse. The regime's removal is catastrophic for users and operators and would fully restore prohibition-style criminalization.
% FOUNDING_PROBLEM: Prohibition's enforcement creates mass incarceration, black-market violence, denial of user autonomy, and criminalization of hundreds of thousands for substance possession. Legalization proposes to solve this by replacing punishment with taxation and regulation, thereby respecting user liberty while internalizing third-party harms through pricing and regulatory constraint.
% FOUNDING_PROBLEM_CORROBORATION: Users and civil liberties advocates attest that prohibition's enforcement remains destructive and unjust. Criminal-justice researchers document mass incarceration from drug offenses. Legal operators in jurisdictions with functioning legalization attest that regulated supply is operationally feasible and has reduced arrest rates. Public health data shows use prevalence has not dramatically increased in legalization jurisdictions (relative to prohibition jurisdictions with persistent underground supply). Third-party harm bearers and public health advocates attest that legalization does NOT fully internalize externalities: secondhand exposure, impaired driving, and pediatric poisoning persist at measurable rates, contradicting the claim that taxation and regulation completely internalize harms.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 (projected at t=0, before full market establishment) to 0.62 (observed plateau by t=12) because taxation and licensing fees accumulate as the legal market scales and state regulatory capacity matures. The constraint is claimed as rope (genuine coordination benefit: safe supply, eliminated criminal enforcement for users) while measured metrics show substantial extraction (0.62 extractiveness, 0.41 suppression of unlicensed supply). Theater ratio rises early (0.15 to 0.28) as the regime matures: enforcement activity shifts from internal quality assurance (genuine) to boundary maintenance against black-market operators (increasingly theatrical—the black market does not disappear). Suppression rises from 0.25 to 0.41 because enforcement against unlicensed operators intensifies to protect the state's tax base and licensed operators' market capture. Accessibility collapse (0.48) is moderate because alternatives persist: users can still access black-market supply at higher risk; geographic arbitrage across jurisdictions is possible for users near borders; and harm-reduction substitutes (abstinence, treatment) remain available outside the legalization frame. Resistance is high (0.71) from multiple directions: black-market suppliers resist enforcement; prohibition-reading advocates resist legalization entirely; users chafe against taxation and potency limits; harm-reduction advocates argue the framework is insufficient. The measurement series represents a realistic trajectory: initial extraction rises as market scales and state captures tax revenue, then plateaus once the regime stabilizes and the black market finds its residual niche. Theater ratio rises concurrently because enforcement shifts from legitimacy-building (safety review) to market protection (suppressing unlicensed competition). Suppression is comparatively low compared to prohibition reading because the constraint does not rest primarily on criminalizing users; it rests on criminalizing unlicensed supply, which is structurally harder to enforce at scale.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state) and the payers (unregulated suppliers, third-party harm bearers) should compute radically different constraint types from the same structural data. The state experiences this as genuine coordination (markets work, tax revenue flows, users are safer than under prohibition) and sees extraction only as a necessary internalization of externalities. Unregulated suppliers experience it as pure snare (they are excluded from legitimate markets, criminalized for tax evasion and licensing violations, and have no recourse). Third-party harm bearers experience it as inadequate rope (they get some protection from regulation but bear residual externalities the taxation does not internalize). Users experience it as improved rope from prohibition's baseline (decriminalization is a massive benefit; taxation is a manageable cost). The engine computes these divergences from the structural data; this reading's claim of rope reflects the beneficiary narrative, while the metrics capture the extractiveness and suppression that emerge from other seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance users sit near the beneficiary end (d ~0.2–0.3) because they exit criminalization (decriminalization is a massive removal of victim status), gain legal supply access, and avoid arrest/incarceration costs. However, they still pay taxation and face regulatory constraints (potency limits, age-gating, access hours), so d is not near zero. Legal operators sit at the strong beneficiary end (d ~0.1–0.2): they capture rents from supply monopoly and licensing barriers without bearing the operational costs of enforcement themselves. The state as agenda-setter is analytically positioned (d = 0.5 by convention: it both collects from taxation and bears regulatory administration costs). Third-party harm bearers sit near the target end (d ~0.8–0.9) because they bear uncompensated externalities and have constrained exit (cannot avoid exposure without geographic or social relocation). Unregulated suppliers sit at the extreme target end (d ~0.95–1.0) because enforcement actively suppresses their supply without their consent. Regulatory compliance actors sit mid-range (d ~0.5–0.6): they pay compliance costs but also benefit from the barrier-to-entry those costs create. The engine derives these values from beneficiary/victim declarations plus exit options; the commentary explains why beneficiary and victim status assignments are structurally sound under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition's enforcement costs and user criminalization) remains live in jurisdictions with prohibition, so the legalization reading's solution is not mandatory yet—there is active contestation between readings. In jurisdictions where legalization has been implemented, the founding problem's status is complex: prohibition's direct enforcement has ended (solving the stated problem) but new problems have emerged (uncompensated externalities, unequal regulatory burden, persistent black markets). The constraint's mandate is not dead but transformed: it no longer primarily solves prohibition's problems; it now serves as a revenue and regulatory apparatus. Theater ratio rising from 0.15 to 0.28 signals the early stages of mandatrophy: as market stabilization occurs and safety review becomes routine, enforcement energy shifts toward black-market suppression, which is increasingly theatrical (defending tax revenue and market capture rather than solving the original coordination problem). A mandatrophy diagnosis is not yet warranted (the regime is still active and relatively new), but the trajectory is observable—theater rising as the founding problem recedes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_party_harm_internalization,
    'Does the taxation and regulatory framework actually internalize third-party harms (secondhand exposure, traffic risk, developmental harm), or does it under-price them and leave residual uncompensated externalities?',
    'Economic analysis comparing tax rates to estimated externality costs; epidemiological data on secondhand exposure, impaired-driving rates, pediatric poisoning after legalization; regret analysis from third-party harm bearers.',
    'If externalities are substantially under-internalized, the constraint is a tangled rope with an extraction component (users and operators benefit from legalization while third parties bear costs that taxation does not recover). If internalized, the constraint is genuine rope. This determines whether third parties are truly victims or merely bearers of residual inevitable costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_internalization, empirical, 'Whether legalization''s tax and regulatory design actually prices externalities.').

omega_variable(
    black_market_displacement,
    'Does legalization eliminate the black market or merely displace it to gray/semi-legal operations and jurisdictions where it remains a substantial shadow economy?',
    'Comparative analysis of pre- and post-legalization black-market supply volumes; supply-chain tracing of unlicensed operators; enforcement data on tax-evasion and licensing-violation prosecutions.',
    'If the black market is largely eliminated, legalization succeeds at its coordination goal (open supply). If it persists as a shadow economy, the constraint''s enforcement (against unregulated suppliers) is ongoing and expensive, suggesting the constraint is more extractive than claimed (the extraction cost is suppression of unregulated supply, not internalization of harms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_displacement, empirical, 'Whether legalization achieves supply consolidation or creates dual markets.').

omega_variable(
    user_autonomy_vs_regulatory_paternalism,
    'Does the legalization framework preserve genuine user autonomy (choice over substance, dose, consumption method) or does regulatory paternalism (potency limits, packaging restrictions, marketing bans, pharmacist gatekeeping) re-create state control under the guise of consumer protection?',
    'Comparative analysis of user choice set under legalization vs. prohibition; user testimony on whether regulatory constraints feel like autonomy-respecting bounds on externalities or return to paternalistic control; longitudinal data on regulatory creep (increasing restrictions over time).',
    'If autonomy is substantially preserved, the legalization reading''s core premise holds (users are beneficiaries of decriminalization). If regulatory paternalism expands over time, the reading''s claim diverges from its operation: users experience increasing extraction via regulatory constraint even as criminalization remains removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(user_autonomy_vs_regulatory_paternalism, conceptual, 'Whether legalization produces genuine user autonomy or regulatory paternalism in new form.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the legalization reading''s core premise (individual liberty as baseline, state authority limited to preventing externalities) logically foreclose the prohibition reading''s core premise (state authority to ban substances for moral reasons), or can both readings coexist in different institutional jurisdictions?',
    'Normative analysis of the contradiction: if ''individual liberty is the baseline'' (legalization) and ''the state has authority to ban for moral reasons'' (prohibition) cannot both be true in the same legal framework, then legalization forecloses prohibition. If they can be held by different polities with different foundational commitments, they coexist.',
    'If foreclosure is real, the kernel contest is not merely empirical disagreement but conceptual contradiction—only one reading can be true in a given jurisdiction. If coexistence holds, both readings can shape policy in different places or eras, making the constraint''s classification reading-indexed (different constraint stories, same physical substance use).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the legalization and prohibition readings foreclose each other or coexist.').

omega_variable(
    formerly_incarcerated_collateral_consequences,
    'Does legalization retroactively remove collateral consequences (employment discrimination, housing barriers, family disruption) for formerly incarcerated persons, or does it only remove criminal liability while leaving path-dependent harms intact?',
    'Longitudinal employment and housing outcome data for formerly incarcerated persons before and after legalization; survey data on employer and landlord discrimination despite expungement; policy analysis of whether legalization jurisdictions mandate affirmative remedies or only passive removal of criminal status.',
    'If collateral consequences persist, formerly incarcerated persons are beneficiaries-with-caveats: they gain legal status but not material recovery. This affects the constraint''s true beneficiary set and whether legalization is as emancipatory as claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formerly_incarcerated_collateral_consequences, empirical, 'Whether legalization''s benefits to formerly incarcerated persons are substantial or nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__legalization_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__legalization_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__legalization_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__legalization_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__legalization_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__legalization_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__legalization_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__legalization_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__legalization_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% The substance control kernel is contested across three readings: legalization_reading (this constraint), prohibition_reading, and harm_reduction_reading. Each reading instantiates a different constraint because the kernel (what is substance use and what is the state's legitimate authority) is interpreted differently. The three constraints share a physical referent (substance use regulation) but different beneficiary/victim structures, extraction mechanisms, and enforcement requirements. Users are beneficiaries under legalization, victims under prohibition, and health subjects under harm reduction. Third parties are victims under legalization (externalities), beneficiaries under harm reduction (served by health intervention), and absent under prohibition (not a concern). The three readings are linked by kernel-family causality: legalization reading influences harm-reduction reading (health-centered intervention is parasitic on legalization's decriminalization) and forecloses prohibition reading (individual liberty baseline contradicts state moral authority to ban). Each story carries omega variables addressing whether its reading's core premises logically foreclose siblings or merely coexist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
