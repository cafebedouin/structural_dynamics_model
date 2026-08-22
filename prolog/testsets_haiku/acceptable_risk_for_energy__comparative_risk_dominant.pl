% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Acceptability Standard for Nuclear Energy
 *   domain: risk_assessment/energy_policy/public_safety
 *
 * SUMMARY:
 *   Nuclear energy is evaluated in energy policy through competing risk
 *   frameworks. This constraint represents the COMPARATIVE-RISK-DOMINANT
 *   reading: nuclear acceptability is contingent on fossil fuel alternatives,
 *   not absolute thresholds. A coal/gas/nuclear choice framework justifies
 *   nuclear operation and expansion by treating coal's documented emissions
 *   and health costs as the reference baseline. The reading subordinates
 *   intergenerational waste concerns (multi-millennial stewardship) to
 *   near-term climate urgency (0–50 year decarbonization window). Victims
 *   include nuclear waste-receiving communities (concentrated, certain, local
 *   risk), climate-vulnerable populations (who benefit from rapid
 *   decarbonization but face immediate catastrophe if it doesn't happen), and
 *   future generations who inherit both climate restoration and perpetual
 *   waste hazards without having chosen the tradeoff.
 *
 * KEY AGENTS:
 *   - nuclear_industry_operators: institutional beneficiary and agenda-setter; control risk communication and license justification
 *   - climate_mitigation_governments: institutional agenda-setter and partial beneficiary; enforce comparative-risk standard in policy and emissions accounting
 *   - climate_vulnerable_populations: powerless payers; depend on rapid decarbonization now, trapped in the belief that nuclear acceleration is necessary
 *   - nuclear_waste_receiving_communities: moderate-power payers; host concentrated, certain, multi-millennial hazard; excluded from comparative-risk framing that pre-empts their objections
 *   - catastrophic_tail_theorists: moderate-power excluded; argue tail-risk dominance should supersede comparative framing; structurally out of the policy conversation
 *   - intergenerational future bearing: non-agent placeholder; embodies the time-asymmetric burden transfer the constraint enacts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.72).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Acceptability Standard for Nuclear Energy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, 'd13626b5-0793-4c65-b151-d1e2ea200132').
narrative_ontology:cs_kernel_codification('d13626b5-0793-4c65-b151-d1e2ea200132', distributed).
narrative_ontology:cs_authority_grounding('d13626b5-0793-4c65-b151-d1e2ea200132', extraction).
narrative_ontology:cs_interpretation_layer_present('d13626b5-0793-4c65-b151-d1e2ea200132').
narrative_ontology:cs_reading_relation('d13626b5-0793-4c65-b151-d1e2ea200132', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('d13626b5-0793-4c65-b151-d1e2ea200132', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('d13626b5-0793-4c65-b151-d1e2ea200132', foundational, fossil_alternative_baseline_necessary).
narrative_ontology:cs_axiom_status(fossil_alternative_baseline_necessary, holdable).
narrative_ontology:cs_axiom_grounding('d13626b5-0793-4c65-b151-d1e2ea200132', fossil_alternative_baseline_necessary, empirically_contingent).
narrative_ontology:cs_axiom('d13626b5-0793-4c65-b151-d1e2ea200132', foundational, temporal_discount_of_intergenerational_burden).
narrative_ontology:cs_axiom_status(temporal_discount_of_intergenerational_burden, holdable).
narrative_ontology:cs_axiom_grounding('d13626b5-0793-4c65-b151-d1e2ea200132', temporal_discount_of_intergenerational_burden, instrumental).
narrative_ontology:cs_reference_frame('d13626b5-0793-4c65-b151-d1e2ea200132', coal_gas_nuclear_choice_framework).
narrative_ontology:cs_drift_state('d13626b5-0793-4c65-b151-d1e2ea200132', renewable_cost_collapse_scenario, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d13626b5-0793-4c65-b151-d1e2ea200132', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, carbon_mitigation_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, developed_economy_energy_consumers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_waste_receiving_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, intergenerational_future_bearing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_mitigation_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate nuclear plants and manage waste. Frame nuclear acceptability through comparative risk: the coal alternative is demonstrably worse (emissions, air pollution deaths), so nuclear's residual risks (catastrophic tail, intergenerational burden) are justified as the lesser harm. Control technical risk communication and license-renewal justifications. Depend on governments enforcing this comparative standard to secure continued operation and delay decommissioning decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators, agenda_setter,
    institutional, generational, constrained, global).

% Adopt the comparative-risk frame in emissions accounting and energy policy because it enables near-term decarbonization without closing the nuclear option. A coal/gas/nuclear choice framework justifies keeping nuclear plants running and greenlighting new builds as the fastest path to carbon reduction. The constraint subordinates intergenerational waste concerns (long tail) to near-term climate urgency.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_mitigation_governments, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, climate_mitigation_governments, beneficiary).

% Bear catastrophic climate impacts now (sea level rise, crop failure, displacement) and in the next 30 years. The comparative-risk reading treats climate as the immediate, measurable threat (monetized in expected-value calculations) while treating nuclear waste burden as speculative future cost. Their immediate survival depends on rapid decarbonization; the comparative-risk frame justifies nuclear acceleration to achieve it, even at cost of multi-millennial waste responsibility.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer,
    powerless, immediate, trapped, regional).

% Host nuclear waste repositories, storage facilities, or processing sites with hazard horizons spanning millennia. The comparative-risk reading defers their burden to a future so distant it is treated as externality, not obligation. They bear concentrated, certain risk (geological hazard, failure of institutional continuity) while the beneficiaries bear diffuse, near-term climate risk (monetized and insured). Institutional capture prevents them from blocking siting decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_waste_receiving_communities, payer,
    moderate, civilizational, trapped, local).

% Future generations who inherit both the climate restoration from accelerated nuclear decarbonization AND the multi-millennial waste stewardship burden. The comparative-risk frame is time-asymmetric: it weights near-term climate urgency (0–50 years) heavily in decisions that create perpetual hazards (10,000+ years). A non-agent placeholder for the intergenerational obligation the constraint embeds.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, intergenerational_future_bearing, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__comparative_risk_dominant, intergenerational_future_bearing).

% Argue that low-probability, high-consequence nuclear events (Fukushima-scale meltdown cascades, waste repository failure, terrorism) should dominate the risk calculus independent of coal emissions context, because irreversibility and intergenerational harm cannot be discounted at market rates. Are structurally excluded from policy-setting because the comparative-risk frame pre-empts their voice: once coal is the reference baseline, tail-risk objections appear obstructionist rather than analytically grounded.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, catastrophic_tail_theorists, excluded,
    moderate, civilizational, constrained, global).

% Measure, report, and certify the comparative-risk standard. Model coal emissions vs. nuclear risk, commission technical studies, issue licenses conditional on the framework. Operationalize the reading through regulatory standards that require applicants to justify nuclear acceptability relative to fossil alternatives rather than against absolute safety thresholds.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, energy_policy_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__comparative_risk_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for rapid decarbonization by positioning nuclear as the risk-justified alternative to carbon-intensive fossil fuels, enabling policy coordination around climate urgency rather than stalling on nuclear tail risks.
% TRANSFER_FUNCTION: Moves the burden of multi-millennial radioactive stewardship from current generations (who benefit from near-term climate mitigation and reliable low-carbon energy) to future generations who inherit the waste repositories and hazard-management institutions without having chosen the tradeoff.
% ABSENT_VOICES: Intergenerational future stakeholders have no seat at the table; catastrophic-tail risk theorists and indigenous communities hosting repositories are structurally excluded from the comparative-risk framing that pre-empts their objections; long-term waste recipients lack the political standing to block decisions made on shorter time horizons.
% DISAPPEARANCE_RATIONALE: If the comparative-risk standard vanished and societies reverted to absolute safety thresholds or tail-risk dominance, near-term nuclear capacity would contract sharply (decommissioning accelerates, new builds freeze), forcing faster renewable scaling or continued reliance on fossil fuels. Climate policy would reorganize around non-nuclear decarbonization pathways with different intergenerational burden profiles.
% FOUNDING_PROBLEM: Climate change acceleration in the 2010s–2020s created policy urgency requiring near-term emissions elimination; coal and gas are demonstrably catastrophic on carbon and public health grounds; nuclear was the available low-carbon baseload technology. The problem: how to operationalize climate urgency without the multi-decade build timelines that renewable-only systems require.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists, energy engineers, and decarbonization policy bodies (IPCC, IEA, national climate commitments) attest the founding problem remains urgent. Waste repositories and indigenous communities affected by siting attest that the problem's urgency justifies imposing perpetual stewardship on them without their consent or seat at the decision. Intergenerational ethics theorists outside the energy industry contest whether near-term urgency legitimates infinite-horizon liability transfer.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint redistributes risk and burden across time in a way that benefits current decision-makers and climate-urgent populations at the cost of future generations and waste-repository hosts. Suppression is higher still (0.72) because the constraint actively excludes tail-risk voices and intergenerational objections from policy discourse — the comparative frame pre-empts them, making them appear obstructionist. Theater is moderate (0.42) and rising: early periods feature genuine technical risk management; later periods show increasing performative justification as intergenerational burden becomes undeniable. The measurement series tracks the constraint's maturation: extractiveness and suppression increase over time as the framework hardens and alternatives (renewable acceleration without nuclear) seem less politically feasible; theater increases as the gap between the coordination story (climate urgency justifies nuclear) and the lived experience (waste keeps piling up, climate mitigation timelines slip) widens. All metrics share a single time grid so values are authored at every measured point.
 *
 * PERSPECTIVAL GAP:
 *   From the operator and government seats, the constraint is genuine coordination justified by climate urgency — a necessary tradeoff where near-term climate catastrophe outweighs long-term waste concerns. From the waste repository seat, the same constraint is extraction camouflaged as coordination: beneficiaries escape the choice and impose it unilaterally. From the intergenerational seat (if it could speak), the constraint is a temporal externality — a theft of future autonomy to fund present utility. The engine computes per-seat classification from power, exit options, and the beneficiary/victim structure; the authored metrics describe operation across all seats simultaneously. Where the computation diverges from the claimed type, that is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   The operator and climate-mitigation governments sit at the beneficiary end (d near 0.0): they collect operational licenses, policy legitimacy, and time to address climate through their preferred technology pathway. Climate-vulnerable populations sit near symmetric (d~0.5): they genuinely benefit from rapid decarbonization but also bear diffuse indirect cost if nuclear expansion crowds out renewable investment or locks in centralized energy systems. Nuclear waste repositories and intergenerational future sit at the target end (d near 1.0): they bear concentrated, certain, multi-millennial hazard with no voice in the decision and no exit option. The constraint's enforceability depends on keeping the target seats' voices excluded (suppression) — if waste communities had equal standing, the comparative-risk frame would collapse under explicit negotiation over who bears perpetual stewardship. The asymmetry is structural, not incidental.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (climate urgency) is live and pressing. The constraint's type claim (tangled_rope) asserts it solves genuine coordination (climate mitigation) with asymmetric extraction (future burden). That claim is plausible: climate mitigation IS the coordination function; nuclear IS capable of providing decarbonized baseload; the extraction (multi-millennial waste stewardship) is real and asymmetric. The risk of mandatrophy is deferred not eliminated — if climate becomes stabilized over the next 30–50 years (renewable costs collapse, carbon pricing eliminates coal, fusion arrives), and nuclear waste becomes the dominant concern rather than climate, the constraint's founding problem dies while the extraction persists. At that point (founding_problem_status would shift to 'dead'), the measured theater_ratio and suppression become piton signals. The constraint has not yet become a piton, but its trajectory is toward one — the measurement series shows theater rising and suppression holding. The mandatrophy vulnerability is real and named here as a structural risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comparative_baseline_stability,
    'Will coal and gas remain the reference baseline, or will renewable cost collapse eliminate the comparative-risk frame''s legitimacy?',
    'Empirical: if renewable energy becomes cheaper and faster to deploy than nuclear across a large jurisdiction, the comparative frame loses rhetorical force and policy attention shifts to tail risks or expected-value optimization.',
    'If the coal baseline becomes obsolete, the comparative-risk constraint loses its founding legitimacy. The constraint would reclassify toward piton (extraction persisting after coordination function atrophies) or be abandoned entirely. Type would shift from tangled_rope to snare (pure extraction) or dissolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_baseline_stability, empirical, 'Whether fossil fuel alternatives remain the policy reference baseline or become economically/technically obsolete.').

omega_variable(
    intergenerational_voice_inclusion,
    'Can intergenerational interests be represented in policy without the speaking generations ceding autonomy, or is the time-asymmetry irreducible?',
    'Institutional experimentation: if jurisdictions adopt intergenerational ombudspersons or constitutional future-stewardship rights, and if these institutions block high-extraction comparative-risk decisions, the structure changes. If they are ignored or captured, the asymmetry persists.',
    'Inclusion would shift the victim set and require comparative-risk framing to negotiate with the waste-bearing seat explicitly. Exclusion persists the current extraction where future voices are pre-emptively suppressed. The constraint''s suppression metric would drop if intergenerational objections gained standing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_voice_inclusion, conceptual, 'Whether multi-generational decision-making can accommodate the interests of future stakeholders without current generations forfeiting policy control.').

omega_variable(
    waste_repository_institutional_continuity,
    'Can institutional commitments to waste stewardship survive the multi-millennial timescales the constraint assumes, or is perpetual stewardship a form of coercion disguised as technical impossibility?',
    'Historical and speculative: examine analogues (ancient structures'' institutional survival, civilizational collapse patterns) and direct evidence of institutional decay. If multi-millennial stewardship proves institutionally impossible, the constraint''s risk calculus becomes incoherent (extraction with no feasible payoff).',
    'If stewardship cannot be guaranteed, nuclear expansion on comparative-risk grounds becomes reckless harm, not justified risk. The constraint would reclassify as snare (pure extraction with externalized catastrophic risk) and lose policy legitimacy. Current operators would face liability for impossible commitments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waste_repository_institutional_continuity, empirical, 'Whether multi-millennial institutional stewardship of radioactive waste is practically achievable given historical patterns of institutional failure and civilizational change.').

omega_variable(
    reading_foreclosure_empirical,
    'Do the comparative-risk frame''s core empirical premises (coal''s demonstrated harm, renewable cost timelines, nuclear construction speed) hold stable, or does evidence shift the epistemic ground to catastrophic-tail or expected-value readings?',
    'Empirical data: coal-vs-nuclear health studies, renewable cost curves, modeled climate pathways, catastrophic event frequencies. If coal''s harm becomes deniable, or renewables become as fast as nuclear, or tail events more probable, the comparative frame loses empirical anchor.',
    'Empirical shift would not foreclose the reading (different parties hold different readings regardless of evidence), but would destabilize its authority grounding. Policy attention would migrate toward readings that better fit the shifted evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_empirical, empirical, 'Whether the comparative-risk reading''s empirical premises (coal harm dominance, renewable-timeline constraints, nuclear-speed advantage) remain robust as evidence evolves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(acce_tr_t8, observed).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(acce_tr_t16, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(acce_tr_t25, observed).
narrative_ontology:measurement(acce_tr_t35, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(acce_tr_t35, projected).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(acce_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(acce_be_t8, observed).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(acce_be_t16, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(acce_be_t25, observed).
narrative_ontology:measurement(acce_be_t35, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(acce_be_t35, projected).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(acce_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(acce_su_t8, observed).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(acce_su_t16, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(acce_su_t25, observed).
narrative_ontology:measurement(acce_su_t35, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(acce_su_t35, projected).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(acce_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__comparative_risk_dominant, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'acceptable_risk_for_energy'. The comparative-risk-dominant reading treats nuclear acceptability as contingent on fossil fuel alternatives (coal/gas baseline). Sibling constraints catastrophic_tail_dominant and expected_value_dominant instantiate alternative readings of the same kernel with different ε values, victim sets, and temporal asymmetries. All three are live positions held by different policy constituencies; the readings coexist rather than foreclose each other, but this reading influences the others by establishing comparative-risk as the default policy frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
