% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: NDCs as Binding Ratcheting Commitments with International Accountability (Supranational Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   Under the supranational reading, Nationally Determined Contributions are
 *   binding commitments on a mandatory ratcheting trajectory toward net-zero,
 *   policed by international accountability: transparency reviews, global
 *   stocktakes, compliance machinery, reputational sanction, financial
 *   conditionality, and trade measures with real cost. The arrangement solves
 *   a genuine collective-action problem while imposing steep asymmetric
 *   costs: carbon-intensive industries face scheduled regulatory extinction,
 *   fossil-structured developing states have their development paths bounded,
 *   and Northern taxpayers fund institutionalized transfers southward. KEY
 *   AGENTS (by structural relationship): small_island_states: primary
 *   beneficiary (organized/trapped) - existential stake, negligible
 *   emissions; climate_vulnerable_developing_states: beneficiary with payer
 *   undertow (moderate/constrained) - receives transfers, resists review
 *   intrusiveness; renewable_energy_industries: secondary beneficiary
 *   (powerful/arbitrage) - market expands with each ratchet;
 *   unfccc_secretariat: agenda setter (institutional/identity_locked) -
 *   administers stocktake and review machinery; eu_climate_bloc: agenda
 *   setter bearing payer costs (institutional/mobile) - authors bindingness,
 *   pays domestically; carbon_intensive_industries: primary target
 *   (powerful/constrained) - scheduled asset extinction, borders closed;
 *   fossil_dependent_developing_states: target (moderate/trapped) -
 *   development path bounded; northern_taxpayers: target
 *   (organized/constrained) - fund transfers and price pass-through;
 *   fossil_fuel_worker_communities: target (powerless/trapped) - timeline set
 *   elsewhere; future_generations: absent party (powerless/trapped) - no
 *   seat, terminal stakes; ipcc_assessment_community: analytical observer
 *   (analytical/analytical) - defines the ambition baseline. FAMILY NOTE
 *   (epsilon-invariance decomposition): the colloquial label 'Paris NDC
 *   commitments' conflates three structurally distinct constraints. This
 *   supranational reading authors epsilon 0.76 for a binding, externally
 *   accountable arrangement with heavy payer burdens. The sovereigntist
 *   sibling (voluntary pledges, no penalty mechanism) authors low epsilon
 *   with a thin victim set and high theater. The equity sibling
 *   (CBDR-structured differentiation) authors moderate-high epsilon
 *   concentrated on developed-state shoulders, with developing-state
 *   development space protected. Same treaty text, three different
 *   constraints, three different victim sets - linked here via
 *   network.affects_constraints rather than averaged into one story.
 *
 * KEY AGENTS:
 *   - small_island_states: primary beneficiary (organized/trapped)
 *   - climate_vulnerable_developing_states: beneficiary with payer undertow (moderate/constrained)
 *   - renewable_energy_industries: secondary beneficiary (powerful/arbitrage)
 *   - unfccc_secretariat: agenda setter (institutional/identity_locked)
 *   - eu_climate_bloc: agenda setter with payer costs (institutional/mobile)
 *   - carbon_intensive_industries: primary target (powerful/constrained)
 *   - fossil_dependent_developing_states: target (moderate/trapped)
 *   - northern_taxpayers: target (organized/constrained)
 *   - fossil_fuel_worker_communities: target (powerless/trapped)
 *   - future_generations: absent party, non-actor entity listed for completeness (powerless/trapped)
 *   - ipcc_assessment_community: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.76).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.62).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "NDCs as Binding Ratcheting Commitments with International Accountability (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '42fdda25-9fef-43d5-bc32-f594803ddfbd').
narrative_ontology:cs_kernel_codification('42fdda25-9fef-43d5-bc32-f594803ddfbd', fixed_text).
narrative_ontology:cs_authority_grounding('42fdda25-9fef-43d5-bc32-f594803ddfbd', lineage).
narrative_ontology:cs_interpretation_layer_present('42fdda25-9fef-43d5-bc32-f594803ddfbd').
narrative_ontology:cs_reading_relation('42fdda25-9fef-43d5-bc32-f594803ddfbd', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('42fdda25-9fef-43d5-bc32-f594803ddfbd', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('42fdda25-9fef-43d5-bc32-f594803ddfbd', foundational, ndc_binding_with_external_consequences).
narrative_ontology:cs_axiom_status(ndc_binding_with_external_consequences, holdable).
narrative_ontology:cs_axiom_grounding('42fdda25-9fef-43d5-bc32-f594803ddfbd', ndc_binding_with_external_consequences, conventional).
narrative_ontology:cs_axiom('42fdda25-9fef-43d5-bc32-f594803ddfbd', foundational, ratcheting_trajectory_obligatory).
narrative_ontology:cs_axiom_status(ratcheting_trajectory_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('42fdda25-9fef-43d5-bc32-f594803ddfbd', ratcheting_trajectory_obligatory, instrumental).
narrative_ontology:cs_reference_frame('42fdda25-9fef-43d5-bc32-f594803ddfbd', binding_ratchet_accountability_framework).
narrative_ontology:cs_drift_state('42fdda25-9fef-43d5-bc32-f594803ddfbd', contemporary_post_paris_implementation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('42fdda25-9fef-43d5-bc32-f594803ddfbd', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, small_island_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_dependent_developing_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, northern_taxpayers).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_worker_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, eu_climate_bloc).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coalition of low-lying island nations for whom sea-level rise and reef collapse are existential. They pressed hardest for binding temperature limits and accountability machinery, contribute negligible emissions, and receive adaptation and loss-and-damage flows. They cannot exit the climate system's physics; their leverage is moral authority and coalition voting weight rather than market power.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, small_island_states, beneficiary,
    organized, civilizational, trapped, global).

% Developing countries exposed to drought, flood, and heat extremes. They accept the ratcheting trajectory in exchange for institutionalized finance, technology transfer, and flexibility provisions; they receive the regime's transfer flows but experience the review cycles as intrusive conditionality attached to money they regard as owed. Leaving would forfeit finance access and diplomatic standing.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_developing_states, beneficiary,
    moderate, generational, constrained, global).

% Manufacturers and developers of solar, wind, storage, and grid technology. Each tightening of national targets and each new border adjustment expands their addressable market; they lobby for ambition ratchets and prosper under the subsidy regimes the accountability architecture legitimizes. Capital is mobile across jurisdictions.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_industries, beneficiary,
    powerful, biographical, arbitrage, global).

% The treaty secretariat and COP presidency machinery: they convene global stocktakes, run the transparency review cycles, maintain the pledge registry, and staff the compliance committee. Budgets, staffing, and diplomatic relevance scale with the regime's scope; the institution's identity is constituted by the process it administers.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, identity_locked, global).

% The European Union and aligned parties that drive bindingness: they authored the net-zero-by-2050 framing, operate the hardest internal targets, and built the border carbon adjustment that gives external accountability its teeth. They bear real domestic compliance costs in heavy industry and pay into international finance while collecting reputational and regulatory-leadership returns.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, eu_climate_bloc, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, eu_climate_bloc, payer).

% Coal, oil and gas producers, cement, steel, chemicals, and aviation. Under a binding ratcheting trajectory their core assets lose operating license and book value on a scheduled curve; border adjustments close the relocation route that would otherwise let production flee to lenient jurisdictions. Remaining options are managed decline, costly abatement retrofit, or litigation delay.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% States whose fiscal systems and development plans are structured around hydrocarbon exports or cheap fossil energy. External accountability bounds the development path open to them while large shares of their populations still lack reliable energy access; they lack the capital to transition and the market power to resist, and the finance promised in exchange arrives late and undersized.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_dependent_developing_states, payer,
    moderate, generational, trapped, regional).

% Households and firms in developed economies who fund the institutionalized transfers through public budgets and consumer prices. They absorb compliance costs passed through energy bills and the finance commitments appropriated year to year; their recourse is electoral, exercised intermittently, and tends to punish whichever government is paying.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, northern_taxpayers, payer,
    organized, biographical, constrained, national).

% Regions and households dependent on coal mining, oil refining, and combustion-engine manufacturing for employment. Transition schedules negotiated in distant forums determine their livelihood timeline; retraining programs tend to arrive after closures; geographic rootedness and skill specificity leave them with the narrowest option set of any seat.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_worker_communities, payer,
    powerless, immediate, trapped, local).

% Not present in any negotiating room. They inherit the terminal consequences of whether the ratcheting trajectory holds or stalls, and are voiced only through proxy arguments invoked by other seats. Listed for completeness of the moral ledger; they collect nothing and sign nothing.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(paris_article_4_ndc__supranational_reading, future_generations).

% The scientific assessment body whose carbon budgets and warming projections define what the ratcheting trajectory must achieve. It supplies the epistemic substrate every seat argues over, holds no enforcement power, and its periodic reports reset the ambition baseline that all parties must then respond to.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, ipcc_assessment_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__supranational_reading, climate_vulnerable_developing_states).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__supranational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global collective-action problem of emission reduction: common timetables, a shared transparency and review framework, and periodic global stocktakes make each state's effort visible and comparable, deterring free-riding; the five-year ratchet cycle prevents early under-pledging from locking in; accountability converts pledges into public expectations that domestic constituencies can police.
% TRANSFER_FUNCTION: Moves compliance costs onto carbon-intensive industries and fossil-structured development paths; moves financial resources from Northern taxpayers to Southern adaptation, mitigation, and loss-and-damage channels; moves reputational and regulatory-leadership capital to compliant states and to the institutions administering the review machinery.
% ABSENT_VOICES: Future generations hold the largest stake and no seat. Fossil-fuel worker communities learn transition timelines after they are set. States outside the regime's normative gravity, and the holders of the sovereigntist position during periods when their governments are out of office, would object to bindingness but enter the conversation only episodically through electoral turnover.
% DISAPPEARANCE_RATIONALE: If the binding-ratchet-accountability architecture vanished overnight, national targets would lose their comparability frame, climate finance channels would lose their justification and disbursement schedule, corporate net-zero planning would lose its regulatory anchor, border carbon measures would lose their treaty cover, and the diplomatic coalitions built around stocktake cycles would dissolve into ad hoc bilateralism.
% FOUNDING_PROBLEM: Free-riding in global emission reduction: no state wants to bear decarbonization cost first if others may defect, and no participant can verify at low cost whether others are delivering. The binding-commitments-with-accountability design descends from the Kyoto compliance lineage and was rebuilt at Paris as a universal ratchet to solve precisely this.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and the atmospheric CO2 record attest independently of any negotiating party that the collective-action problem remains unsolved (global emissions still rising); NGFS central-bank scenario work corroborates the financial-materiality framing from outside the beneficiary set. No participating government's self-attestation is relied upon.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76 at interval end) because the binding-ratchet design imposes scheduled, non-consensual costs: asset extinction curves for heavy industry, bounded development paths for fossil-structured states, and annually appropriated transfers from Northern budgets. It is not higher because a large fraction of the imposed cost is the price of addressing a genuine existential commons problem whose benefits accrue broadly, and because compensation channels return part of the taken value southward. Suppression (0.62) is structural rather than physical: trade measures, financial conditionality, normative isolation, and litigation raise the price of exit and non-compliance without eliminating either - the demonstrated possibility of withdrawal keeps alternatives partly alive, hence accessibility_collapse at 0.55 rather than mountain-grade levels. Resistance (0.58) is sustained and real: withdrawal episodes, border-adjustment retaliation threats, producer-state obstruction, and litigation counter-mobilization. Theater_ratio (0.42) tracks the pledge-ceremony layer - headline announcements, offset accounting, delivery gaps - against functioning machinery (review cycles, border adjustments, disclosure regulation); the series shows a Copenhagen-era hump (0.45 at t=8) falling to a post-disillusionment trough (0.36 at t=16) before climbing again with the net-zero pledge wave, an externally event-driven excursion rather than an oscillating extraction cycle. All three metric series are authored on one shared eight-point grid (t=0..28, mapping 1997-2025) so the engine samples every metric at every examined time point; endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From small_island_states the arrangement is life-saving coordination they subsidize with moral labor - effective extraction near zero or negative. From carbon_intensive_industries the same structure operates as scheduled confiscation of asset value with the exit hatch welded shut - near-full-target extraction. From eu_climate_bloc it is a legitimate burden-sharing order they authored and partly pay for - intermediate. From fossil_dependent_developing_states it reads as sovereignty-stripping review discipline whose compensation arrives late and undersized - high extraction despite nominal beneficiary-adjacent flows. The engine computes these divergences from the declared roles, power atoms, and exit options; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: small_island_states (trapped exit amplifies their stake but they pay almost nothing into the mechanism), climate_vulnerable_developing_states (net recipients of transfers, dampened further by constrained exit), renewable_energy_industries (arbitrage-grade mobility plus expanding market - nearest the full-beneficiary end among payers-adjacent seats). Victim declarations drive high directionality: carbon_intensive_industries (constrained exit via border adjustments pushes them toward full-target), fossil_dependent_developing_states (trapped, no capital to adapt), northern_taxpayers (constrained, recurring appropriations), fossil_fuel_worker_communities (trapped, least mobility of any seat). The agenda setters sit mid-low: unfccc_secretariat collects institutional scale from the regime it runs; eu_climate_bloc is genuinely dual-positioned - it authors the bindingness and absorbs part of the payer burden, pulling its derived directionality up from a pure-administrator value. No overrides were needed: the beneficiary/victim declarations plus exit options produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - free-riding in emission reduction - is verifiably still live (independently attested by the physical emissions record), so this is not a resolved-mandatrophy case and none is declared. The classification guards against two mislabels. Against pure-snare: although extraction is high, no seat captures the regime's gains - the transfers land programmatically across many recipient states, the market gains are spread across a competitive industry, and the reputational returns are diffuse - so the receipt surface names the largest institutionalized recipient rather than a captor, and the coordination function is primary, not cover. Against pure-rope: identifiable seats bear extinction-scale asymmetric costs through the same structure that coordinates everyone else, and the arrangement demonstrably requires active enforcement to hold. The forward risk is slow piton drift: if the delivery gap persists while enforcement stays anticipatory, the theater_ratio series is the variable to watch - a sustained rise above 0.5 with flat realized enforcement would signal proxy-goal substitution inside the accountability machinery itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_character_contest,
    'Is this supranational instantiation faithful to what Article 4 legally is, or does the sovereigntist reading (explicitly facilitative design, nationally determined content, no penalty mechanism) better match the treaty text and observed state practice?',
    'Treaty-text legal analysis cross-checked against behavioral evidence: withdrawal episodes, compliance-committee case outcomes, and whether non-compliance actually attracts consequential sanction or only facilitative review.',
    'If the sovereigntist reading is the accurate instantiation, this constraint''s epsilon drops sharply toward the voluntary regime''s profile, the victim set thins, and the enforcement structure described here dissolves into theater; the two stories would then differ categorically rather than in degree.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_character_contest, conceptual, 'Committer-frame omega: this story is one reading of the paris_article_4_ndc kernel; the sovereigntist sibling would restructure victim sets and enforcement wholesale.').

omega_variable(
    enforcement_realization_gap,
    'How much of the measured extraction reflects enforcement that has actually been applied versus enforcement that payers merely anticipate (border adjustments announced, litigation threatened, finance conditionalized)?',
    'Compare outcomes for covered versus uncovered exposure: trade flows and investment inside versus outside CBAM coverage, compliance-committee cases with and without consequences, jurisdictions before and after hard-edged mechanism adoption.',
    'If anticipation dominates realization, current epsilon overstates present extraction and understates contingent extraction; the trajectory''s slope becomes a policy-choice variable rather than an institutional fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_realization_gap, empirical, 'Whether the accountability regime bites through realized mechanisms or through credible threat alone.').

omega_variable(
    transfer_compensation_or_dependency,
    'Do the institutionalized North-to-South transfers compensate recipients for constrained development paths, or do they constitute a conditionality structure that is itself extractive in operation?',
    'Longitudinal analysis of finance disbursement conditionality, loan-versus-grant composition, and recipient-state debt trajectories tied to climate finance access.',
    'If transfers operate as compensatory, climate_vulnerable_developing_states are clean beneficiaries; if they operate as conditional dependency, that seat is dual-positioned and the regime''s extraction footprint widens to include its own remedy channel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_compensation_or_dependency, empirical, 'Whether the wealth-transfer leg of the regime relieves or extends extraction on recipient seats.').

omega_variable(
    extinction_design_vs_incidence,
    'Is the scheduled extinction of carbon-intensive industries a designed targeting decision embedded in the regime, or an incidental incidence of a coordination instrument aimed at aggregate emissions?',
    'Policy-design genealogy: trace drafting records, lobbying archives, and the placement of just-transition compensation schemes to determine whether payer burdens were calibrated deliberately or emerged as side-effects.',
    'Designed targeting would sharpen the asymmetry between coordination and extraction components and push the computed classification toward the snare boundary; incidental incidence keeps the components fused and supports the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinction_design_vs_incidence, conceptual, 'Whether payer costs were chosen or inherited.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndc_supranational_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ndc_supranational_tr_t4, paris_article_4_ndc__supranational_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(ndc_supranational_tr_t8, paris_article_4_ndc__supranational_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(ndc_supranational_tr_t12, paris_article_4_ndc__supranational_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(ndc_supranational_tr_t16, paris_article_4_ndc__supranational_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(ndc_supranational_tr_t20, paris_article_4_ndc__supranational_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ndc_supranational_tr_t24, paris_article_4_ndc__supranational_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(ndc_supranational_tr_t28, paris_article_4_ndc__supranational_reading, theater_ratio, 28, 0.42).

% Extraction over time
narrative_ontology:measurement(ndc_supranational_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ndc_supranational_be_t4, paris_article_4_ndc__supranational_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(ndc_supranational_be_t8, paris_article_4_ndc__supranational_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(ndc_supranational_be_t12, paris_article_4_ndc__supranational_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(ndc_supranational_be_t16, paris_article_4_ndc__supranational_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(ndc_supranational_be_t20, paris_article_4_ndc__supranational_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(ndc_supranational_be_t24, paris_article_4_ndc__supranational_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(ndc_supranational_be_t28, paris_article_4_ndc__supranational_reading, base_extractiveness, 28, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(ndc_supranational_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ndc_supranational_su_t4, paris_article_4_ndc__supranational_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(ndc_supranational_su_t8, paris_article_4_ndc__supranational_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(ndc_supranational_su_t12, paris_article_4_ndc__supranational_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(ndc_supranational_su_t16, paris_article_4_ndc__supranational_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(ndc_supranational_su_t20, paris_article_4_ndc__supranational_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(ndc_supranational_su_t24, paris_article_4_ndc__supranational_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(ndc_supranational_su_t28, paris_article_4_ndc__supranational_reading, suppression_requirement, 28, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'Paris Article 4 NDC commitments' decomposes into three structurally distinct stories per the epsilon-invariance principle. This supranational reading (epsilon 0.76, binding with external accountability, victims include industry and fossil-structured states) sits downstream of the same treaty text that the sovereigntist reading (low epsilon, facilitative design, thin victim set, high theater) and the equity reading (moderate-high epsilon concentrated on developed states, differentiation protected) instantiate differently. The supranational reading structurally influences the equity sibling - uniform accountability machinery and uniform border pricing erode CBDR differentiation without eliminating it - and stands in logical contradiction to the sovereigntist sibling's core premise within any single party's framework. Each member links the others via network.affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
