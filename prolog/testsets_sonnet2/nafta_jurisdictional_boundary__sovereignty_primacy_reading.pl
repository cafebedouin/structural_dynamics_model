% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__sovereignty_primacy_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA/USMCA Text as Coordination Mechanism Subordinate to Sovereign Domestic Law
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-primacy reading of the
 *   contested NAFTA/USMCA jurisdictional-boundary kernel: the treaty text
 *   functions as a voluntary coordination mechanism among sovereign states,
 *   entering each state's domestic compliance-cost calculus as one policy
 *   consideration among many, never as a legal instrument capable of
 *   overriding a domestic statute absent affirmative implementing legislation
 *   through the ordinary legislative process. Under this reading, dispute
 *   panels may authorize compensation or retaliatory tariffs, but cannot
 *   strike down or suspend domestic law; a state retains the option to accept
 *   the political and trade cost of noncompliance and keep its regulatory
 *   choice intact. This is a distinct constraint from the
 *   capital_supremacy_reading (which treats treaty obligations as overriding
 *   domestic regulatory standards) and from the embedded_liberalism_reading
 *   (which treats non-discriminatory domestic standards as compatible with,
 *   and balanced against, trade obligations as a matter of treaty design).
 *   All three readings share the same text but diverge on where binding legal
 *   authority sits — a difference in kind, not degree, which is why ε for
 *   this reading (0.22, low, dominated by voluntary compliance costs) differs
 *   substantially from what the capital_supremacy_reading would author for
 *   the same clauses.
 *
 * KEY AGENTS:
 *   - domestic_regulatory_agencies: agenda_setter (institutional/analytical) — retains full jurisdictional authority to set and enforce standards
 *   - exporting_firms: beneficiary (organized/mobile) — gains tariff and customs coordination benefits
 *   - importing_consumers: beneficiary (moderate/constrained) — gains price benefits from tariff reduction
 *   - treaty_dispute_panels: observer (institutional/analytical) — adjudicates claims but cannot override domestic statutes under this reading
 *   - civil_society_labor_environmental_groups: excluded (moderate/constrained) — raises regulatory-chill concerns treated as political, not legal, under this reading
 *   - foreign_treaty_partner_states: observer (institutional/analytical) — symmetric sovereign counterpart
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA/USMCA Text as Coordination Mechanism Subordinate to Sovereign Domestic Law").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e68f42d7-6a22-4ebe-8e18-76758748194c').
narrative_ontology:cs_kernel_codification('e68f42d7-6a22-4ebe-8e18-76758748194c', fixed_text).
narrative_ontology:cs_authority_grounding('e68f42d7-6a22-4ebe-8e18-76758748194c', practice).
narrative_ontology:cs_interpretation_layer_present('e68f42d7-6a22-4ebe-8e18-76758748194c').
narrative_ontology:cs_reading_relation('e68f42d7-6a22-4ebe-8e18-76758748194c', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e68f42d7-6a22-4ebe-8e18-76758748194c', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('e68f42d7-6a22-4ebe-8e18-76758748194c', foundational, dualist_incorporation_required_for_domestic_effect).
narrative_ontology:cs_axiom_status(dualist_incorporation_required_for_domestic_effect, holdable).
narrative_ontology:cs_axiom_grounding('e68f42d7-6a22-4ebe-8e18-76758748194c', dualist_incorporation_required_for_domestic_effect, conventional).
narrative_ontology:cs_axiom('e68f42d7-6a22-4ebe-8e18-76758748194c', foundational, state_retains_unilateral_regulatory_exit_from_compliance).
narrative_ontology:cs_axiom_status(state_retains_unilateral_regulatory_exit_from_compliance, holdable).
narrative_ontology:cs_axiom_grounding('e68f42d7-6a22-4ebe-8e18-76758748194c', state_retains_unilateral_regulatory_exit_from_compliance, conventional).
narrative_ontology:cs_reference_frame('e68f42d7-6a22-4ebe-8e18-76758748194c', westphalian_dualist_sovereignty_baseline).
narrative_ontology:cs_drift_state('e68f42d7-6a22-4ebe-8e18-76758748194c', post_investor_state_arbitration_expansion, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e68f42d7-6a22-4ebe-8e18-76758748194c', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, exporting_firms).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, importing_consumers).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, dualist_treaty_incorporation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces labor, environmental, and health standards within its territory under domestic statutory authority. Treats treaty text as a coordination framework that lowers transaction costs for cross-border trade but does not treat any provision as capable of overriding a domestic statute absent affirmative implementing legislation. Can raise standards unilaterally at any time; the only cost of doing so is a possible trade dispute claim, which enters the agency's compliance-cost calculus as one factor among many, not as a veto.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Uses the treaty's tariff schedules and dispute-resolution channels to gain predictable market access across borders. Benefits from reduced duplication of customs and certification procedures. Where a partner state raises a domestic standard, absorbs the compliance cost or petitions its own government to raise a dispute; does not gain a mechanism to compel the other state to change its law.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, exporting_firms, beneficiary,
    organized, biographical, mobile, continental).

% Gains from lower-cost imported goods enabled by tariff coordination, while continuing to be protected by whatever domestic health, safety, and environmental standards their own government chooses to set and enforce independent of the treaty text.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, importing_consumers, beneficiary,
    moderate, biographical, constrained, national).

% Adjudicates state-to-state or investor claims that a measure breaches the agreement, and can recommend compensation or authorize retaliatory tariffs, but under this reading has no power to strike down, suspend, or directly override a state's domestic regulatory statute; a state can accept the political and trade cost of noncompliance and retain its law intact.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, treaty_dispute_panels, observer,
    institutional, biographical, analytical, continental).

% Advocate for stronger enforceable labor and environmental chapters and worry that even non-binding treaty text creates political pressure to weaken standards preemptively. Under this reading their concern is treated as a live policy debate to be resolved through domestic legislative process, not as evidence that the treaty text itself compels deregulation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, civil_society_labor_environmental_groups, excluded,
    moderate, generational, constrained, national).

% Negotiate and ratify the same instrument, retaining identical domestic primacy over their own labor, environmental, and health law. Can bring or face dispute claims but cannot compel a partner's domestic legislature to amend a statute.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, foreign_treaty_partner_states, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__sovereignty_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared tariff schedule, dispute-resolution forum, and predictable rules of origin so firms and states do not need to renegotiate market access terms bilaterally and repeatedly; this is a genuine coordination problem (reducing transaction costs and uncertainty in cross-border commerce) that the text solves without requiring any state to surrender domestic regulatory authority.
% TRANSFER_FUNCTION: Moves reduced tariff and customs friction to exporting firms and, secondarily, price reductions to importing consumers; does not move regulatory authority from domestic legislatures to any supranational or treaty body — compliance costs from dispute outcomes remain a voluntary line item a state may choose to bear or not.
% ABSENT_VOICES: Civil society labor and environmental groups worry the treaty's mere existence creates a chilling effect on future regulation (a 'regulatory chill' dynamic) even without formal override authority; under this reading their objection is a claim about political dynamics, not about the text's binding legal effect, and is not treated as evidence against the sovereignty-primacy account.
% DISAPPEARANCE_RATIONALE: If the treaty text vanished overnight, tariff schedules would revert to MFN rates, the shared dispute forum would disappear, and firms would lose the predictability the coordination mechanism provides — real coordination value would be lost even though, under this reading, no state's domestic regulatory authority would change at all, since that authority was never contingent on the treaty in the first place.
% FOUNDING_PROBLEM: Cross-border trade among the three states faced duplicative tariffs, inconsistent customs procedures, and no shared forum for resolving trade disputes, raising transaction costs and discouraging investment and exchange.
% FOUNDING_PROBLEM_CORROBORATION: Independent customs-economics literature and WTO trade-facilitation studies (produced outside the negotiating governments and outside the firms that benefit from tariff coordination) continue to document measurable transaction-cost savings from harmonized rules of origin and dispute forums, corroborating that the coordination problem persists and the mechanism continues to address it.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because, under this reading, no domestic regulatory authority is transferred or subordinated — the only cost states or firms bear is the possibility of a dispute claim and its associated compensation or retaliatory tariff, which is a voluntary and bounded cost, not an enforceable override. Suppression is authored low (0.18) because states retain the unilateral option to legislate as they choose; the treaty imposes no coercive barrier to domestic regulatory action, only a foreseeable political/economic consequence. Theater ratio is low (0.15) and rises only slightly over the interval, reflecting the genuine and largely stable coordination function (tariff schedules, customs harmonization, dispute forum) the text performs. Accessibility collapse is low-moderate (0.25): states could in principle exit the treaty, renegotiate, or simply accept dispute costs, so alternatives to the standing arrangement are not foreclosed. Resistance is moderate (0.35), reflecting persistent civil-society contestation of the sovereignty-primacy account itself, even though that contestation does not, under this reading, register as evidence against the account.
 *
 * DIRECTIONALITY LOGIC:
 *   Domestic regulatory agencies are the agenda-setters and, under this reading, bear essentially no extraction — they retain full authority and choose whether to internalize dispute-cost risk. Exporting firms and importing consumers are net beneficiaries of the coordination function (lower tariffs, predictable customs treatment) and are declared beneficiaries accordingly. No victim group is declared: under the sovereignty-primacy reading, there is no party whose domestic regulatory authority is structurally extracted from, because that authority was never ceded to the treaty in the first place. Civil society groups are excluded from the negotiating and adjudicating conversation but are not victims of extraction under this reading — their objection is about a possible political dynamic (chilling effect), not about a binding legal transfer of authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (duplicative tariffs, absent dispute forum, high cross-border transaction costs) remains live and is corroborated by independent trade-economics literature outside the negotiating states and benefiting firms, so this reading resists a piton or snare re-classification: the mechanism continues to perform the function it was built for, and no party's domestic regulatory authority has been quietly extracted under cover of that function persisting. The classification prevents mislabeling ordinary treaty-dispute activity (compensation, retaliatory tariffs) as coercive override, which is precisely the mislabeling the capital_supremacy_reading would make.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispute_outcome_as_de_facto_override,
    'When a dispute panel repeatedly rules against a state''s domestic standard and the state repeatedly amends its law to avoid retaliatory tariffs, does the treaty text function as de facto binding override even though it lacks formal legal supremacy under this reading?',
    'Track a panel of domestic regulatory changes following adverse dispute rulings across multiple sectors and states; if amendment rates approach near-uniform compliance regardless of the state''s stated policy preference, that pattern would support the capital_supremacy_reading''s account over this one.',
    'If de facto override is empirically dominant, this reading''s low ε (0.22) is descriptively wrong for the arrangement''s actual operation, and the constraint the sovereignty-primacy reading describes may not be the one actually governing state behavior — the two readings would need to be evaluated against different observed behavior sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispute_outcome_as_de_facto_override, empirical, 'Whether formal non-override coexists with practical behavioral override.').

omega_variable(
    regulatory_chill_anticipatory_effect,
    'Does the mere existence of dispute-cost exposure cause states to decline to raise standards they otherwise would have raised, even absent any actual dispute filing?',
    'Compare regulatory proposal and enactment rates in trade-exposed sectors before and after treaty accession, controlling for other policy drivers, and interview regulators about anticipatory self-restraint.',
    'If chill is substantial and systematic, the sovereignty-primacy reading''s claim of ''full regulatory authority retained'' would be formally true but practically hollow — this would not change ε as authored here (which measures binding legal effect) but would sharpen the omega about which reading better describes lived regulatory practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_chill_anticipatory_effect, conceptual, 'Whether formal sovereign authority is compatible with anticipatory behavioral constraint.').

omega_variable(
    kernel_framing_selection,
    'Is the choice to treat ''binding legal authority location'' as the decisive axis (rather than ''observed regulatory behavior'' or ''stated treaty purpose'') itself a contestable framing that favors this reading over its siblings?',
    'Compare how legal scholars, trade economists, and affected regulatory agencies themselves characterize the decisive question — dualist constitutional doctrine points to this reading''s framing; international-law monist traditions and investor-state arbitration practice point toward the capital_supremacy framing.',
    'If the dualist/monist framing choice is itself contested rather than settled, no single reading can claim to be the uncontested structural truth about the kernel — all three readings remain live and jointly necessary rather than one being simply correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether the axis used to decide between readings (legal-authority location) is itself a neutral fact or a contested framing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2006, 0.12).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2012, 0.13).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.18).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2000, 0.19).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2006, 0.2).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2012, 0.21).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2018, 0.21).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2024, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nafta_jurisdictional_boundary__sovereignty_primacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the nafta_jurisdictional_boundary kernel and the same treaty text but diverge on where binding legal authority sits. This story (sovereignty_primacy_reading) authors low ε (0.22) and no victims, treating treaty obligations as voluntary compliance-cost inputs. capital_supremacy_reading authors high ε and declares domestic regulatory agencies and affected populations as victims of an overriding legal instrument. embedded_liberalism_reading authors moderate ε with a balanced-compatibility structure and no victims but a different coordination/transfer account than this reading. Each is a separate ε-invariant constraint per the decomposition principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
