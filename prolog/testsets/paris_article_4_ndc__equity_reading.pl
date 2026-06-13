% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Article 4 NDC Equity Reading: Common But Differentiated Responsibilities
 *   domain: political_economy/international_law/climate
 *
 * SUMMARY:
 *   Article 4 of the Paris Agreement requires all signatories to submit
 *   Nationally Determined Contributions (NDCs) reflecting emissions-reduction
 *   ambitions. The equity reading interprets this article through the
 *   principle of Common But Differentiated Responsibilities and Respective
 *   Capabilities (CBDR-RC): developed states, bearing historical carbon
 *   responsibility and possessing greater capacity, face binding constraints
 *   and transfer obligations; developing states retain policy discretion
 *   framed as 'national circumstances' and receive climate finance as
 *   obligation. This reading legitimizes coalition veto power held by
 *   developing-state blocs over supranational enforcement mechanisms. The
 *   structural delta from other readings is substantial: developed states
 *   experience this reading as a payer constraint (extraction from them
 *   toward transfers and reduced policy autonomy), while developing states
 *   experience it as beneficiary positioning (discretion, legitimated
 *   differentiation, finance access). The sibling sovereigntist reading
 *   treats NDCs as purely voluntary self-determined pledges; the
 *   supranational reading treats them as binding commitments on a ratcheting
 *   trajectory independent of equity distinctions.
 *
 * KEY AGENTS:
 *   - Developed States: Institutional power, constrained exit (climate commitments are now domestically locked), payer position (transfer obligations, binding targets). Experience the constraint as extraction framed as fairness.
 *   - Developing-State Coalitions (BASIC, LDC, AOSIS): Organized power, mobile exit (can threaten regime exit or non-ratification), agenda-setter + beneficiary dual position (set coalition positions, claim discretion). Institutionalize equity framing to preserve policy space.
 *   - Equity Constituencies: Organized power, mobile exit (can shift coalition allegiance, fund advocacy), beneficiary role (use equity framing to demand larger transfers, veto supranational enforcement). Climate justice language legitimates the constraint structure.
 *   - Subnational Low-Carbon Actors: Moderate power, constrained exit (tethered to national positions they do not control), payer role (face slowed mitigation from developing-state discretion, crowded-out domestic investment from transfer flows). Structurally silent in coalition negotiation.
 *   - Supranational Monitoring Bodies: Institutional power, analytical exit, agenda-setter role (frame technical adequacy). Operationally deferred by equity framing; can assess but not enforce.
 *   - High-Ambition Coalition: Powerful but excluded (veto power held by equity blocs). Advocates for binding enforcement, ratcheting targets independent of equity. Reframed by equity coalitions as climate colonialism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.48).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.62).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Article 4 NDC Equity Reading: Common But Differentiated Responsibilities").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "political_economy/international_law/climate").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670').
narrative_ontology:cs_kernel_codification('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', fixed_text).
narrative_ontology:cs_authority_grounding('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', lineage).
narrative_ontology:cs_interpretation_layer_present('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670').
narrative_ontology:cs_reading_relation('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', paris_article_4_ndc__paris_article_4_ndc_sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', paris_article_4_ndc__paris_article_4_ndc_supranational_reading, coexists_with).
narrative_ontology:cs_axiom('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', foundational, differentiated_responsibility_fairness).
narrative_ontology:cs_axiom_status(differentiated_responsibility_fairness, holdable).
narrative_ontology:cs_axiom_grounding('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', differentiated_responsibility_fairness, deontological).
narrative_ontology:cs_axiom('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', foundational, capacity_based_obligation_allocation).
narrative_ontology:cs_axiom_status(capacity_based_obligation_allocation, holdable).
narrative_ontology:cs_axiom_grounding('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', capacity_based_obligation_allocation, deontological).
narrative_ontology:cs_reference_frame('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', paris_article_4_equity_baseline).
narrative_ontology:cs_drift_state('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', contemporary_2030, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6ed1bff7-e4f8-47d3-b5f8-6ef4cdda9670', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_state_coalitions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_constituencies).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, subnational_low_carbon_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signatories to Paris Agreement bound by Article 4 to submit NDCs. Under the equity reading, their NDCs must reflect historical responsibility for atmospheric carbon and their capacity to bear mitigation costs. They face binding emissions-reduction targets (harder to revise downward), transfer obligations to developing states (climate finance), and reduced policy autonomy (cannot unilaterally raise targets without accepting fiscal consequences). Their exit options are constrained by regime legitimacy and domestic climate legislation that has locked in commitments. They can negotiate supranational enforcement harder, but cannot escape the equity-framing constraint without rupturing the regime.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_states, payer,
    institutional, generational, constrained, global).

% BASIC (Brazil, South Africa, India, China), LDC bloc, AOSIS, and climate-justice coalitions collectively hold veto power over binding supranational enforcement through coalition negotiations. The equity reading legitimates their position: NDCs are interpreted as differentiated by national circumstances, capacity, and responsibility—they retain discretion over ambition levels. They can threaten regime exit (withdrawal) or non-cooperation on enforcement mechanisms to preserve policy space. They benefit from framing that obligates developed states to transfer resources and accept lower emissions targets, while their own NDCs remain more modest. Their mobile exit (credible threat to rupture coalition negotiations) gives them leverage over interpretation of Article 4.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_state_coalitions, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, developing_state_coalitions, agenda_setter).

% Climate justice movements, indigenous rights organizations, loss-and-damage networks, and small-island-state advocacy groups institutionalize the equity reading through COP statements, formal bloc positions, and treaty language. They benefit from the constraint because it legitimates demanding larger transfers, preserves developing-state policy autonomy (which they frame as protection against climate colonialism), and blocks supranational enforcement architecture (which they frame as imperial imposition). Their exit options are mobile: they can shift coalition allegiance, fund advocacy in different blocs, or activate subnational constituencies. They use the constraint to expand resources and veto power for developing states.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_constituencies, beneficiary,
    organized, biographical, mobile, global).

% Cities, regional governments, renewable-energy developers, and decentralized climate initiatives in both developed and developing states face ambiguous incentives under the equity reading. In developed states, they face binding national targets (constrain their policy choices). In developing states, they benefit rhetorically from policy discretion but face weak enabling infrastructure and ambiguous investment signals (if the nation delays NDC ambition, subnational actors cannot easily defect). Their exit options are constrained by national negotiating positions they do not control; they cannot unilaterally raise ambition if national government uses equity framing to maintain lower targets. They are structurally silent in coalition negotiations despite bearing mitigation implementation costs.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, subnational_low_carbon_actors, payer,
    moderate, biographical, constrained, national).

% UNFCCC Secretariat, IPCC technical bodies, and international legal-interpretation frameworks technically frame Article 4 and assess NDC adequacy relative to climate physics. They publish technical reports on the mitigation gap, carbon budgets, and equity-adjusted pathways. Under the equity reading, their technical assessments are operationally deferred to national sovereignty—they can measure and describe shortfalls, but cannot enforce binding remedies when equity framing legitimates developing-state discretion and shields them from escalation. Their agenda-setting power is constrained by political will; they serve as technical observers with limited enforcement authority.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, supranational_monitoring_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Developed states, island-state coalitions seeking survival-level action, and climate-leading corporations and subnational actors that prioritize binding supranational enforcement and ratcheting targets independent of equity distinctions. They are excluded from binding-constraint design by coalition veto power held by developing-state blocs. Their advocacy for higher targets, supranational accountability mechanisms, and emissions-trading linkage is reframed by equity coalitions as climate colonialism, imperialism, or unfair burden-shifting. They maintain institutional positions (some developed-state governments, IPCC authors, multilateral development banks) but lack coalition veto in treaty negotiations. Their exit option is constrained: they cannot impose supranational enforcement without regime fracture.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, high_ambition_coalition, excluded,
    powerful, biographical, constrained, global).

% International law scholarship, IPCC expert panels, and legal interpretation frameworks assess which readings of Article 4 are defensible. They produce technical interpretation (grammatical analysis, prior treaty language, negotiating history), but enforcement turns on political will, not interpretation clarity. The equity reading is currently dominant in formal COP language and developing-state bloc statements; supranational readings maintain institutional presence in UNFCCC secretariat and IPCC but lack coalition veto. The analytical seat reports on dispute without resolving it.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, treaty_interpreter_consensus, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, developing_state_coalitions).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes global emissions-reduction burden across states according to principles of historical carbon responsibility and differential capacity: developed states accept higher targets and transfer obligations; developing states retain policy discretion for national-circumstances considerations. Coordinates multiple institutional levels (national NDCs, supranational monitoring, bilateral finance) under a common legitimating principle (equity differentiation) that enables coalition formation and reduces likelihood of total regime failure.
% TRANSFER_FUNCTION: Moves climate-finance resources, technology-transfer commitments, and capacity-building from developed to developing states as obligation framed through equity principle; moves policy space and NDC-setting autonomy toward developing-state coalitions; moves veto power over supranational enforcement mechanisms to equity-organized blocs. Reduces binding target surface on developing states' energy and industrial policy; increases binding obligation surface on developed states' fiscal transfers.
% ABSENT_VOICES: High-ambition coalition (developed states and vulnerable-island advocates seeking binding supranational enforcement); subnational climate actors who depend on faster mitigation pathways (cities, renewable developers, loss-and-damage communities); future-generation climate impacts and physical constraints that exceed the regime's current ambition. These voices are either excluded by coalition veto or subordinated to present-day equity demands.
% DISAPPEARANCE_RATIONALE: If the equity-reading interpretation of Article 4 disappeared overnight—if NDCs were reinterpreted as binding supranational commitments independent of equity distinctions—the Paris regime's political coalition would fracture: developing-state coalitions would either exit or demand renegotiation of transfer terms; climate finance would shift from obligation-based to discretionary; national energy policy autonomy would compress under binding enforcement architecture; subnational actors would have clearer (faster) mitigation signals but face less policy flexibility. The regime's current institutional equilibrium depends on the equity reading. Removing it would force either a new equilibrium (higher global mitigation, lower autonomy) or regime dissolution.
% FOUNDING_PROBLEM: Historical carbon emissions are concentrated in industrialized (developed) states, who built wealth through high-carbon development pathways; developing states now bear disproportionate climate impacts despite minimal responsibility for the problem; development pathways for emerging and least-developed economies should not be constrained by the same emissions targets applied to mature industrial states, because doing so would perpetuate inequality and deny developing states the development benefits that developed states obtained. Climate obligations must reflect fairness and capacity differences, or the regime cannot hold legitimacy in the Global South and will collapse into non-compliance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by IPCC Working Group III synthesis on equity dimensions of mitigation (technical document external to UNFCCC beneficiary class); UNFCCC Secretariat formal position papers citing CBDR-RC as foundational principle; developing-state bloc statements from every COP 2015-2024 (BASIC, LDC Group, AOSIS); independent climate justice scholarship (Oxfam, World Resources Institute, climate-justice academics outside negotiating-bloc positions). Contested by high-ambition coalition states and supranational-enforcement advocates who argue: (1) the founding problem has been substantially addressed through climate finance and technology mechanisms, so equity-differentiated targets are no longer justified; (2) delaying developing-state action costs future generations more than developed-state historical responsibility costs present-day developing states; (3) the principle has been gamed—middle-income countries like China claim developing status while using autonomy to pursue high-emissions development. The corroboration is credible but contested.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as Tangled Rope: it coordinates a multi-level climate regime respecting sovereignty AND distributes burdens asymmetrically. Extractiveness is moderate (0.48) because the constraint redistributes resources and policy autonomy rather than capturing absolute rents—developed states pay, developing states gain discretion, but the total pie is the same. Suppression is elevated (0.62) because the constraint depends on institutionalizing equity framing and coalition veto to prevent developed states from unilaterally raising targets or developing-state blocs from accepting supranational enforcement. Theater ratio rises from 0.28 (2015) to 0.41 (2024-2030), tracking the constraint's increasing reliance on narrative maintenance: as the gap between NDC ambition and climate physics widens, equity language does more work defending the status quo and less work coordinating actual mitigation. Accessibility collapse is high (0.65) because Article 4's text is fixed, and alternative readings require rejecting the equity-framing interpretation wholesale—but the collapse is not absolute (0.65 not 0.85) because high-ambition coalitions continue to advocate supranational reading and supranational bodies maintain technical adequacy frameworks. Resistance is moderate (0.58) because developed states mount legal and diplomatic challenges, and subnational actors develop de facto exit routes, but developed states remain constrained by regime legitimacy dependencies. Measurement time grid is shared: every metric authored at every time point (2015, 2018, 2021, 2024, 2027, 2030). Theater and suppression_requirement rise through 2027 (constraint-maintenance intensification), then slight dip by 2030 (projected: pressure peaks, then either supranational breakthrough or regime fragmentation begins). The leveled coercion grid shows asymmetric pressure: structural-level stakes inflation (0.64→0.71) reflects escalating climate impacts making energy-policy choices higher-stakes; organizational resistance (0.68→0.61) reflects coalition solidity eroding as developing-state members face domestic climate pressure; individual-level resistance declines (0.52→0.48) as climate-affected populations grow more demanding than their state negotiators.
 *
 * PERSPECTIVAL GAP:
 *   Developed states and subnational actors perceive high extraction and suppression; equity coalitions and developing-state governments perceive legitimate differentiation and necessary policy space. The engine computes per-seat classification from power, exit, and beneficiary/victim data: developed states sit as payers (high d), developing-state coalitions as beneficiaries (low d). The gap is structural, not observational—the same constraint produces different type calculations for different seats because directionality reflects asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states (institutional power, constrained exit from climate regime, victim declaration) derive high d—they bear costs (transfer obligations, binding targets, policy constraint) without veto power over supranational interpretation, though they can slow implementation. Developing-state coalitions (organized power, mobile exit—can threaten treaty withdrawal—beneficiary declaration, agenda-setter role in coalition formation) derive low d—they collect discretion and finance framing as obligation. Equity constituencies (organized power, mobile exit, beneficiary role) derive low d—they use the constraint to demand larger transfers and veto enforcement. Subnational actors (moderate power, constrained exit, payer role) derive high d—they pay through slowed mitigation and crowded-out domestic investment. The directionality overrides would apply if a developed-state negotiator's actual influence over binding-mechanism design differed from the power atom and victim declaration suggest; none are authored here because the structural assignment (developed = payer, developing coalition = beneficiary + agenda-setter) reflects observable negotiating outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by maintaining a live coordination function: the regime still coordinates emissions reduction across states with differential capacity, even as the equity reading increasingly serves to preserve policy autonomy rather than drive ambition. The founding problem (historical responsibility, capacity disparity, development pathways) remains live but contested—developing states claim it is still operative (climate finance is insufficient, capacity gaps are real), while developed states increasingly claim the problem has been addressed by technology and finance mechanisms (the founding problem is solved; NDCs should ratchet independent of equity). The theater-ratio rise (toward 0.41) and the gap between NDC ambition and climate-physics requirements signal the constraint is performing more narrative work and less coordination work. A true mandatrophy resolution would require either: (1) supranational enforcement breakthrough, or (2) regime fragmentation where developed states exit and build parallel supranational mechanisms. Current state: contested founding problem, rising theater, stable but asymmetric extraction—a Tangled Rope under pressure, not yet mandatrophic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_framing_as_cover_story,
    'Does the equity-differentiated reading reflect genuine fairness principles, or does it serve primarily to legitimize developing-state policy autonomy that slows global mitigation?',
    'Comparative analysis of climate-finance disbursement vs. NDC ambition gap; investigation of whether developed states'' transfer obligations have actually constrained their energy policy or remained discretionary; examination of whether developing-state discretion has enabled faster deployment or merely delayed action.',
    'If equity framing is primarily cover story for policy autonomy, the constraint should reclassify toward snare (extraction of policy space from subnational actors and future-generation impacts, legitimized through equity narrative). If equity principles are genuine and producing real transfers, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_framing_as_cover_story, empirical, 'Whether equity differentiation reflects fairness or legitimizes policy autonomy.').

omega_variable(
    coalition_bloc_veto_power,
    'Is the veto power held by developing-state coalitions structurally necessary to protect policy space against supranational pressure, or has it become a mechanism to block enforcement that would constrain all states equally?',
    'Analysis of coalition positions on supranational enforcement proposals; counterfactual: would binding enforcement without equity differentiation be rejected by developing states because the burden is unfair, or because autonomy is preferred even under equal rules?',
    'If veto power is equity-protective, it is part of the coordination function and justifies beneficiary status for developing-state coalitions. If veto is autonomy-protective regardless of fairness, the constraint should reclassify toward piton (inertial maintenance of policy discretion through coalition performance).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_bloc_veto_power, conceptual, 'Whether coalition veto protects fairness or merely autonomy.').

omega_variable(
    supranational_enforcement_foreclosure,
    'Does the equity reading logically foreclose supranational enforcement readings of Article 4, or do the two readings coexist as live political positions?',
    'Legal interpretation: does CBDR-RC language in Article 4 grammatically foreclose binding supranational mechanisms, or can a supranational reading accommodate equity principles in target-setting while applying binding accountability to implementation?',
    'If the equity reading forecloses supranational enforcement within a single Article 4 framework, it is structurally more powerful than coexistence; if they coexist, the contest is political, not logical—shifting balances can shift which reading governs interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supranational_enforcement_foreclosure, conceptual, 'Whether equity and supranational readings are logically incompatible or politically contested.').

omega_variable(
    capital_flows_extraction_mechanism,
    'Do climate-finance flows from developed to developing states represent redistribution of extraction gains, or do they constitute new extraction mechanisms (debt, conditionality, technology-lock-in)?',
    'Analysis of climate-finance terms, conditionality, debt burdens, and intellectual-property constraints; comparison of financial flows to actual capacity-building and emission-reduction outcomes.',
    'If finance represents clean redistribution, developed states'' victim status is reinforced (pure payer, no return). If finance creates new extraction mechanisms benefiting intermediaries, the beneficiary analysis requires revision—developing states may be partly victims of the finance architecture while benefiting from policy discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_flows_extraction_mechanism, empirical, 'Whether climate finance redistributes or creates new extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(pari_tr_t2015, observed).
narrative_ontology:measurement(pari_tr_t2018, paris_article_4_ndc__equity_reading, theater_ratio, 2018, 0.33).
narrative_ontology:measurement_basis(pari_tr_t2018, observed).
narrative_ontology:measurement(pari_tr_t2021, paris_article_4_ndc__equity_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement_basis(pari_tr_t2021, observed).
narrative_ontology:measurement(pari_tr_t2024, paris_article_4_ndc__equity_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(pari_tr_t2024, observed).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__equity_reading, theater_ratio, 2027, 0.44).
narrative_ontology:measurement_basis(pari_tr_t2027, projected).
narrative_ontology:measurement(pari_tr_t2030, paris_article_4_ndc__equity_reading, theater_ratio, 2030, 0.41).
narrative_ontology:measurement_basis(pari_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement_basis(pari_be_t2015, observed).
narrative_ontology:measurement(pari_be_t2018, paris_article_4_ndc__equity_reading, base_extractiveness, 2018, 0.42).
narrative_ontology:measurement_basis(pari_be_t2018, observed).
narrative_ontology:measurement(pari_be_t2021, paris_article_4_ndc__equity_reading, base_extractiveness, 2021, 0.46).
narrative_ontology:measurement_basis(pari_be_t2021, observed).
narrative_ontology:measurement(pari_be_t2024, paris_article_4_ndc__equity_reading, base_extractiveness, 2024, 0.49).
narrative_ontology:measurement_basis(pari_be_t2024, observed).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__equity_reading, base_extractiveness, 2027, 0.51).
narrative_ontology:measurement_basis(pari_be_t2027, projected).
narrative_ontology:measurement(pari_be_t2030, paris_article_4_ndc__equity_reading, base_extractiveness, 2030, 0.48).
narrative_ontology:measurement_basis(pari_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement_basis(pari_su_t2015, observed).
narrative_ontology:measurement(pari_su_t2018, paris_article_4_ndc__equity_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement_basis(pari_su_t2018, observed).
narrative_ontology:measurement(pari_su_t2021, paris_article_4_ndc__equity_reading, suppression_requirement, 2021, 0.61).
narrative_ontology:measurement_basis(pari_su_t2021, observed).
narrative_ontology:measurement(pari_su_t2024, paris_article_4_ndc__equity_reading, suppression_requirement, 2024, 0.63).
narrative_ontology:measurement_basis(pari_su_t2024, observed).
narrative_ontology:measurement(pari_su_t2027, paris_article_4_ndc__equity_reading, suppression_requirement, 2027, 0.65).
narrative_ontology:measurement_basis(pari_su_t2027, projected).
narrative_ontology:measurement(pari_su_t2030, paris_article_4_ndc__equity_reading, suppression_requirement, 2030, 0.62).
narrative_ontology:measurement_basis(pari_su_t2030, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=2015, tn=2030
narrative_ontology:measurement(pari_grid_01, paris_article_4_ndc__equity_reading, accessibility_collapse(class), 2015, 0.68).
narrative_ontology:measurement(pari_grid_02, paris_article_4_ndc__equity_reading, accessibility_collapse(class), 2030, 0.65).
narrative_ontology:measurement(pari_grid_03, paris_article_4_ndc__equity_reading, accessibility_collapse(individual), 2015, 0.55).
narrative_ontology:measurement(pari_grid_04, paris_article_4_ndc__equity_reading, accessibility_collapse(individual), 2030, 0.58).
narrative_ontology:measurement(pari_grid_05, paris_article_4_ndc__equity_reading, accessibility_collapse(organizational), 2015, 0.58).
narrative_ontology:measurement(pari_grid_06, paris_article_4_ndc__equity_reading, accessibility_collapse(organizational), 2030, 0.63).
narrative_ontology:measurement(pari_grid_07, paris_article_4_ndc__equity_reading, accessibility_collapse(structural), 2015, 0.72).
narrative_ontology:measurement(pari_grid_08, paris_article_4_ndc__equity_reading, accessibility_collapse(structural), 2030, 0.68).
narrative_ontology:measurement(pari_grid_09, paris_article_4_ndc__equity_reading, resistance(class), 2015, 0.58).
narrative_ontology:measurement(pari_grid_10, paris_article_4_ndc__equity_reading, resistance(class), 2030, 0.52).
narrative_ontology:measurement(pari_grid_11, paris_article_4_ndc__equity_reading, resistance(individual), 2015, 0.52).
narrative_ontology:measurement(pari_grid_12, paris_article_4_ndc__equity_reading, resistance(individual), 2030, 0.48).
narrative_ontology:measurement(pari_grid_13, paris_article_4_ndc__equity_reading, resistance(organizational), 2015, 0.68).
narrative_ontology:measurement(pari_grid_14, paris_article_4_ndc__equity_reading, resistance(organizational), 2030, 0.61).
narrative_ontology:measurement(pari_grid_15, paris_article_4_ndc__equity_reading, resistance(structural), 2015, 0.62).
narrative_ontology:measurement(pari_grid_16, paris_article_4_ndc__equity_reading, resistance(structural), 2030, 0.55).
narrative_ontology:measurement(pari_grid_17, paris_article_4_ndc__equity_reading, stakes_inflation(class), 2015, 0.48).
narrative_ontology:measurement(pari_grid_18, paris_article_4_ndc__equity_reading, stakes_inflation(class), 2030, 0.52).
narrative_ontology:measurement(pari_grid_19, paris_article_4_ndc__equity_reading, stakes_inflation(individual), 2015, 0.38).
narrative_ontology:measurement(pari_grid_20, paris_article_4_ndc__equity_reading, stakes_inflation(individual), 2030, 0.42).
narrative_ontology:measurement(pari_grid_21, paris_article_4_ndc__equity_reading, stakes_inflation(organizational), 2015, 0.52).
narrative_ontology:measurement(pari_grid_22, paris_article_4_ndc__equity_reading, stakes_inflation(organizational), 2030, 0.58).
narrative_ontology:measurement(pari_grid_23, paris_article_4_ndc__equity_reading, stakes_inflation(structural), 2015, 0.64).
narrative_ontology:measurement(pari_grid_24, paris_article_4_ndc__equity_reading, stakes_inflation(structural), 2030, 0.71).
narrative_ontology:measurement(pari_grid_25, paris_article_4_ndc__equity_reading, suppression(class), 2015, 0.54).
narrative_ontology:measurement(pari_grid_26, paris_article_4_ndc__equity_reading, suppression(class), 2030, 0.58).
narrative_ontology:measurement(pari_grid_27, paris_article_4_ndc__equity_reading, suppression(individual), 2015, 0.48).
narrative_ontology:measurement(pari_grid_28, paris_article_4_ndc__equity_reading, suppression(individual), 2030, 0.52).
narrative_ontology:measurement(pari_grid_29, paris_article_4_ndc__equity_reading, suppression(organizational), 2015, 0.58).
narrative_ontology:measurement(pari_grid_30, paris_article_4_ndc__equity_reading, suppression(organizational), 2030, 0.62).
narrative_ontology:measurement(pari_grid_31, paris_article_4_ndc__equity_reading, suppression(structural), 2015, 0.61).
narrative_ontology:measurement(pari_grid_32, paris_article_4_ndc__equity_reading, suppression(structural), 2030, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__equity_reading, 0.16).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc_sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc_supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, climate_finance_architecture).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, emissions_trading_international_linkage).

% DUAL FORMULATION NOTE:
% The constraint family paris_article_4_ndc consists of three structurally distinct readings of Article 4: equity_reading (this story) — NDCs are differentiated by responsibility and capacity, developing states retain policy discretion, developed states bear transfer obligations (moderate epsilon, asymmetric distribution); sovereigntist_reading — NDCs are purely voluntary self-determined pledges, no binding architecture implied, equity and supranational enforcement both rejected; supranational_reading — NDCs are binding commitments on ratcheting trajectory toward net-zero, independent of equity distinctions, supranational accountability trumps national discretion (high epsilon, symmetric extraction on all states). Each reading instantiates a different constraint because ε differs (moderate vs. low vs. high), beneficiary structure differs (developing coalitions vs. none/all-equal vs. all-equal), and institutional implications differ. The equity reading influences both siblings: it creates structural pressure on supranational enforcement (coalition veto can slow ratcheting); it coexists with sovereigntist reading (some states hold pure voluntarism, others hold equity differentiation, no logical foreclosure). Network links established: equity→sovereigntist (coexists), equity→supranational (influences+coexists), both→climate_finance_architecture (equity reading activates finance-obligation framing), equity→emissions_trading (developing-state discretion under equity reading affects whether emerging economies adopt linked carbon markets or preserve policy space).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
