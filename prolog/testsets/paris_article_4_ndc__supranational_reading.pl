% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: NDC Binding Commitments on Ratcheting Trajectory Toward Net-Zero with International Accountability (Supranational Reading)
 *   domain: international/climate/political_economy
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 requires all signatory states to submit
 *   Nationally Determined Contributions (NDCs) outlining their climate action
 *   and targets. The supranational reading interprets NDCs as legally binding
 *   commitments on a mandatory ratcheting trajectory toward net-zero, with
 *   international accountability mechanisms that create material consequences
 *   for insufficient ambition. Under this reading, successive NDCs must
 *   become progressively more stringent, carbon-intensive industries face
 *   regulatory extinction, and wealth flows from high-emission states to
 *   climate-vulnerable ones through climate finance conditionality. This
 *   reading is contested by sovereigntist interpretations (NDCs as voluntary
 *   self-determined pledges) and equity readings (NDCs must embed Common But
 *   Differentiated Responsibilities distinguishing developed from developing
 *   states). This constraint story instantiates ONLY the supranational
 *   reading as a clean, ε-invariant constraint—the other readings are
 *   separate constraint stories linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - UNFCCC Secretariat and Compliance Bodies: institutional agenda-setter, defines binding interpretation and transparency requirements
 *   - Climate-Vulnerable States (Small Island Developing States, LDCs): organized beneficiaries, face existential risk mitigation through binding NDC commitments
 *   - Carbon-Intensive Industries (fossil fuels, cement, steel, aviation): powerful payers, face regulatory extinction through ratcheting decarbonization
 *   - Incumbent Fossil Fuel States (Saudi Arabia, Russia, Australia): institutional payers, face revenue contraction and diplomatic pressure
 *   - Renewable Energy Industries: powerful beneficiaries, capture mandated decarbonization market expansion without bearing transition costs
 *   - Energy-Poor Developing States: powerless payers, trapped between binding NDCs and lack of capital for transition
 *   - Indigenous Communities: excluded payers, face land conscription for carbon sequestration without sovereignty recognition
 *   - Climate Finance Gatekeepers (World Bank, MDBs): institutional agenda-setters and beneficiaries, wield conditionality and grow mandates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.81).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.72).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "NDC Binding Commitments on Ratcheting Trajectory Toward Net-Zero with International Accountability (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international/climate/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8').
narrative_ontology:cs_kernel_codification('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', formalized).
narrative_ontology:cs_authority_grounding('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', extraction).
narrative_ontology:cs_interpretation_layer_present('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8').
narrative_ontology:cs_reading_relation('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', foundational, ndc_legal_bindingness).
narrative_ontology:cs_axiom_status(ndc_legal_bindingness, holdable).
narrative_ontology:cs_axiom_grounding('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', ndc_legal_bindingness, empirically_contingent).
narrative_ontology:cs_axiom('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', foundational, ratcheting_trajectory_institutional_requirement).
narrative_ontology:cs_axiom_status(ratcheting_trajectory_institutional_requirement, holdable).
narrative_ontology:cs_axiom_grounding('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', ratcheting_trajectory_institutional_requirement, instrumental).
narrative_ontology:cs_reference_frame('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', legally_binding_international_decarbonization_framework).
narrative_ontology:cs_drift_state('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', id_2026_onward_contested_reading_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5b3ec006-b1ab-4e78-bfdd-bb08729e8ee8', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_industries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, global_carbon_accountability_system).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, incumbent_fossil_fuel_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, energy_poor_developing_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).

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
 *   Extractiveness is high and rising (0.52→0.87 over the interval) because the supranational reading encodes binding decarbonization obligations that create asymmetric costs: carbon-intensive industries and fossil fuel states bear steep transition expenses while renewable energy and climate-vulnerable actors collect benefits. The ratcheting mechanism amplifies extraction over time—each successive NDC round increases decarbonization stringency, raising the cost of compliance for payers and the value of compliance for beneficiaries. Suppression is substantial (0.48→0.78) because the constraint's persistence depends on UNFCCC enforcement of transparency and accountability mechanisms that prevent states from submitting weak NDCs; suppression is most intense against energy-poor developing states, who are trapped between binding commitments and lack of finance, and against indigenous communities, whose land is conscripted for carbon credits without consent. Theater is low-moderate (0.15→0.31) because the NDC process involves genuine technical work (emissions inventories, reduction pathways) but an increasing share of institutional activity defends the binding interpretation against sovereigntist challenge—performative consensus-building becomes more necessary as political resistance rises. Accessibility collapse is high across all levels (0.48→0.71 individual; 0.62→0.84 structural) because once the supranational reading consolidates as UNFCCC doctrine, alternatives (market-driven decarbonization, state-led energy sovereignty) are foreclosed by the binding framework. Resistance is moderate and declining (0.58→0.52 structural; 0.72→0.68 individual) because fossil fuel state and skeptical coalitions have fewer exit options as capital markets price in carbon liabilities, and because climate impacts make denial increasingly costly politically. The coercion grid shows that suppression intensifies most at the structural and organizational levels (state and industry enforcement) while individual resistance remains highest—a classic pattern of high-level coordination suppressing low-level exit.
 *
 * PERSPECTIVAL GAP:
 *   The UNFCCC and climate-vulnerable states perceive the constraint as rope-plus-justice: coordination that solves emissions tragedy plus a mechanism for wealth transfer from North to South (tangled_rope, genuine coordination with asymmetric distribution of benefits). Fossil fuel states and skeptical coalitions perceive it as snare: binding commitments imposed without their consent, enforced through financial and diplomatic pressure, from which they cannot exit without cost. Energy-poor developing states experience it as a complex dual trap: tangled_rope at the beneficiary end (binding emission reductions by wealthy states benefit them) and snare at their own seat (binding NDCs on themselves despite lacking transition capital). The coercion grid shows this: suppression is lowest at the individual level (people still have informal exit through migration, black markets, energy theft) and highest at the organizational and structural levels (states and industries face institutional enforcement). Resistance mirrors this: individuals mount the most resistance (climate strikes, fossil fuel divestment campaigns), while states and industries have fewer defection options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and differential exit options. Climate-vulnerable states benefit from binding NDCs (low d, ~0.15) because the constraint reduces warming risk without requiring them to decarbonize themselves; their exit option is 'constrained'—they can participate in future UNFCCC rounds but cannot leave without losing negotiating power and climate finance. Renewable energy industries benefit (low d, ~0.20) and have mobile exit—they can shift to other markets if NDCs fail, but ratcheting guarantees expanding demand. Carbon-intensive industries are victims (high d, ~0.85) with constrained exit: they must transition or face regulatory exclusion, and relocating to less-regulated jurisdictions invites capital flight and reputational risk. Energy-poor developing states are victims (high d, ~0.88) with trapped exit: they cannot refuse binding NDCs (no state exits Paris without diplomatic cost), cannot fund compliance without climate finance (which is conditioned on NDC stringency), and cannot maintain coal-powered development without violating their pledges. Indigenous communities are victims (high d, ~0.82) with trapped exit: their land is conscripted for NDC carbon credits without consent, and they have no formal veto in UNFCCC processes. Fossil fuel states have high d (~0.80) and constrained exit: they face binding decarbonization obligations they view as unilateral impositions but cannot leave the framework without economic sanctions. The supranational reading assigns highest directionality toward victimhood (most constrained exit + highest extraction burden) to powerless developing states and indigenous communities, moderate directionality to powerful carbon-intensive actors (who have arbitrage options through capital relocation), and lowest directionality to beneficiary seats (which face no extraction and have mobile or analytical exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is real: global carbon emissions must decline and no single state will decarbonize without assurance others will follow (tragedy of the commons). The supranational reading solves this by making NDCs binding and ratcheting—each state has incentive to exceed its peers' targets (to avoid appearing insufficient) and to enforce peers' compliance (to avoid being undercut). However, mandatrophy risk exists: as carbon-intensive industries lobby for weaker targets and fossil fuel states threaten exit, the ratcheting mechanism comes under pressure. If the UNFCCC relaxes the binding interpretation or allows successive NDCs to weaken, the constraint would become a piton—institutionally maintained theater of commitment without real decarbonization. The measurement series assumes ratcheting holds through 2035, but the omega variable 'ratcheting_mechanism_credibility' flags this as the constraint's critical stress point. A snare/mandatrophy hybrid is possible: the supranational reading persists as official UNFCCC doctrine (theater of binding commitment) while actual state behavior shows non-compliance and weaker NDCs (degraded coordination function). The three-way reading contest (supranational vs. sovereigntist vs. equity) itself performs mandatrophy: as the readings compete, the institutional energy invested in the contest increases even if the coordination function declines. An omega on 'reading_contest_and_contestation_closure' documents this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_voluntary_semantics,
    'Are NDCs legally binding commitments or politically binding pledges? Does ''binding'' mean enforceability through sanctions or merely reputational cost?',
    'Legal interpretation by the International Court of Justice or subsequent UNFCCC decisions that clarify Article 4 compliance mechanisms and consequences for non-compliance. Case law from states that have formally withdrawn or violated NDCs.',
    'If binding = legally enforceable with material sanctions, the supranational reading holds and extraction rises to 0.85+. If binding = reputational only, the reading shifts toward rope or piton; extraction drops to 0.55–0.65. The measurement series assumes binding = legally enforceable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_vs_voluntary_semantics, conceptual, 'The core semantic distinction underlying the supranational vs. sovereigntist readings.').

omega_variable(
    ratcheting_mechanism_credibility,
    'Can the ratcheting trajectory (each NDC more ambitious than the last) be sustained against state defection and fossil fuel state alliance-building?',
    'Observation of NDC submissions in 2025, 2030, and 2035 rounds. If successive NDCs show declining ambition or explicit weakenings, ratcheting has failed; if they accelerate, it holds. COP outcomes and UNFCCC synthesis reports on collective progress.',
    'If ratcheting fails, the supranational reading collapses into sovereignism—NDCs revert to voluntary self-determined pledges. Extraction would fall to 0.45–0.55 and claimed type would shift from tangled_rope to rope or piton. If ratcheting holds, the 0.81 supranational extraction is conservative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ratcheting_mechanism_credibility, empirical, 'Whether the institutional machinery can sustain escalating decarbonization demands.').

omega_variable(
    climate_finance_adequacy_and_conditionality,
    'Will developed states transfer sufficient capital to developing states for NDC compliance, or will finance become a control mechanism that subordinates energy-poor states to Northern carbon preferences?',
    'Tracking climate finance flows (promised vs. delivered), interest rates and conditionality on climate finance loans, and developing-state outcomes in energy access and NDC achievement. Comparative analysis of states with strong NDCs but weak financing vs. weak NDCs with preferential access.',
    'If finance is adequate and non-coercive, the tangled_rope coordinating function is strengthened and energy-poor states shift from payers to beneficiaries. If finance is inadequate or coercive, the constraint becomes a snare for developing states; extraction on that seat rises to 0.88+ while extraction on climate-vulnerable seats falls. The measurement series assumes mixed finance (sufficient in aggregate, coercive at margins).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_finance_adequacy_and_conditionality, empirical, 'Whether climate finance is a genuine transfer mechanism or a control apparatus.').

omega_variable(
    reading_contest_and_contestation_closure,
    'Will the supranational reading consolidate as the dominant UNFCCC interpretation, or will sovereigntist and equity readings continue to compete and fragment compliance?',
    'COP decisions and UNFCCC secretariat guidance: do successive COPs reaffirm the supranational reading or weaken it? Do fossil fuel states and climate skeptics achieve dilution of enforcement? Do equity-focused states secure structural changes that embed differentiation? Political economy of UNFCCC governance and coalition dynamics.',
    'If supranational reading consolidates, this constraint story is descriptively true and extractiveness rises toward 0.90+ by 2035. If sovereigntist reading advances, the constraint fragments into seat-specific types: supranational for climate-vulnerable seats, sovereigntist (rope) for high-emission states. If equity reading dominates, the beneficiary/victim structure inverts and extraction on energy-poor states falls while extraction on unequal-emission states rises. The measurement series assumes supranational consolidation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_and_contestation_closure, conceptual, 'The constitutional meta-question: which reading of the Paris kernel will the UNFCCC institutionalize?').

omega_variable(
    indigenous_land_rights_and_ndc_conflicts,
    'How will the tension between NDC land-use targets and indigenous sovereignty be resolved? Will conservation become indigenous-led or state-controlled carbon sequestration?',
    'Empirical tracking of land-use NDC implementation: which territories are subject to conservation easements, who controls those territories, and what happens to indigenous communities living there. Legal cases and precedents on indigenous rights vs. state climate policy.',
    'If indigenous communities retain control and benefit from land-based NDCs, they shift from excluded to beneficiary and the constraint''s equity profile improves. If states colonize territories for carbon credits, suppression on indigenous communities rises to 0.88+ and the constraint becomes a snare for that seat. The base measurement assumes state-led conservation with indigenous suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_land_rights_and_ndc_conflicts, empirical, 'Whether NDC implementation preserves or erodes indigenous land sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__supranational_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(pari_tr_t2020, paris_article_4_ndc__supranational_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(pari_tr_t2025, paris_article_4_ndc__supranational_reading, theater_ratio, 2025, 0.24).
narrative_ontology:measurement(pari_tr_t2030, paris_article_4_ndc__supranational_reading, theater_ratio, 2030, 0.27).
narrative_ontology:measurement(pari_tr_t2035, paris_article_4_ndc__supranational_reading, theater_ratio, 2035, 0.31).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__supranational_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(pari_be_t2020, paris_article_4_ndc__supranational_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(pari_be_t2025, paris_article_4_ndc__supranational_reading, base_extractiveness, 2025, 0.76).
narrative_ontology:measurement(pari_be_t2030, paris_article_4_ndc__supranational_reading, base_extractiveness, 2030, 0.81).
narrative_ontology:measurement(pari_be_t2035, paris_article_4_ndc__supranational_reading, base_extractiveness, 2035, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__supranational_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(pari_su_t2020, paris_article_4_ndc__supranational_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(pari_su_t2025, paris_article_4_ndc__supranational_reading, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement(pari_su_t2030, paris_article_4_ndc__supranational_reading, suppression_requirement, 2030, 0.72).
narrative_ontology:measurement(pari_su_t2035, paris_article_4_ndc__supranational_reading, suppression_requirement, 2035, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__supranational_reading, 0.12).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, carbon_markets_article_6).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, climate_finance_conditionality).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, indigenous_land_carbon_sequestration).

% DUAL FORMULATION NOTE:
% The paris_article_4_ndc kernel has three structurally distinct readings: supranational (this story, binding commitments with ratcheting), sovereigntist (voluntary pledges preserving national sovereignty), and equity (differentiated obligations per CBDR-RC). Each reading produces different epsilon values, different beneficiary/victim structures, and different terminal constraint types. They are NOT the same constraint viewed from different angles—their ε values differ by >0.35 and their failure modes are distinct. They are linked through network.affects_constraints to enable the engine to compute cross-reading contamination and reading-contest dynamics. Each story is authored with zero reference to its siblings' claims; the readings are brought into relationship only through the network layer and through the omega variables documenting the reading contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
