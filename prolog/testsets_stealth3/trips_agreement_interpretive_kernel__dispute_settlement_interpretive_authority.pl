% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_interp_authority, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: TRIPS Dispute Settlement Interpretive Authority (Binding Panel Interpretation with Retaliation Enforcement)
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   Under the Dispute Settlement Understanding, WTO panels issue reasoned
 *   interpretations of the TRIPS text that become binding on adoption by
 *   reverse consensus, with enforcement backed by authorization to suspend
 *   trade concessions. Adopted reports accumulate into precedent that decides
 *   which reading of the contested TRIPS kernel hardens into settled law -
 *   making this constraint the referee of the exclusivity-versus-flexibility
 *   contest rather than a contestant in it. The Appellate Body's paralysis
 *   since December 2019 (appointments blocked by consensus) broke the
 *   completion of appealed cases, and bilateral leverage increasingly
 *   substitutes for adjudicated settlement. KEY AGENTS (by structural
 *   relationship): see commentary.key_agents. This file is one reading of a
 *   kernel; the substantive siblings are separate stories with independent
 *   epsilon authorship. The claim/metric independence rule is honored
 *   deliberately: the claimed type is what I believe structurally true of
 *   this arrangement, and the metric values are what I believe descriptively
 *   true of its actual operation as assessed by this reading's own lights -
 *   where they diverge from any predicted engine output, the divergence is
 *   the datum.
 *
 * KEY AGENTS:
 *   - wto_dispute_settlement_body: agenda-setter/administrator (institutional/constrained) - adopts reports and authorizes retaliation; appointment unanimity is the choke point
 *   - major_litigant_powers: primary beneficiary (institutional/arbitrage) - funds the litigation that writes precedent; market size deters retaliation against them
 *   - united_states_trade_representative: dual-positioned beneficiary and payer (institutional/arbitrage) - heaviest user of the machinery and source of the appellate-tier collapse
 *   - pharmaceutical_export_industries: secondary beneficiary (powerful/arbitrage) - collects expansively-read patent protections through government-sponsored complaints
 *   - small_trading_economies: primary target (powerless/trapped) - bears precedent and retaliation costs without litigation capacity
 *   - developing_country_generic_sectors: target with partial benefit (organized/constrained) - gained lawful flexibilities, operates under standing litigation risk
 *   - global_health_access_movement: excluded voice (organized/trapped) - no standing in proceedings whose outcomes govern medicine access
 *   - wto_trade_law_community: analytical observer (analytical/analytical) - documents precedent formation and the design-versus-operation gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.55).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.5).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.55).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "TRIPS Dispute Settlement Interpretive Authority (Binding Panel Interpretation with Retaliation Enforcement)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '215a1090-0010-4b36-be00-dc6d3fcb54e7').
narrative_ontology:cs_kernel_codification('215a1090-0010-4b36-be00-dc6d3fcb54e7', fixed_text).
narrative_ontology:cs_authority_grounding('215a1090-0010-4b36-be00-dc6d3fcb54e7', lineage).
narrative_ontology:cs_interpretation_layer_present('215a1090-0010-4b36-be00-dc6d3fcb54e7').
narrative_ontology:cs_reading_relation('215a1090-0010-4b36-be00-dc6d3fcb54e7', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('215a1090-0010-4b36-be00-dc6d3fcb54e7', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('215a1090-0010-4b36-be00-dc6d3fcb54e7', foundational, treaty_meaning_fixed_by_adjudication).
narrative_ontology:cs_axiom_status(treaty_meaning_fixed_by_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('215a1090-0010-4b36-be00-dc6d3fcb54e7', treaty_meaning_fixed_by_adjudication, conventional).
narrative_ontology:cs_axiom('215a1090-0010-4b36-be00-dc6d3fcb54e7', foundational, adjudication_may_not_add_or_diminish_rights).
narrative_ontology:cs_axiom_status(adjudication_may_not_add_or_diminish_rights, holdable).
narrative_ontology:cs_axiom_grounding('215a1090-0010-4b36-be00-dc6d3fcb54e7', adjudication_may_not_add_or_diminish_rights, conventional).
narrative_ontology:cs_reference_frame('215a1090-0010-4b36-be00-dc6d3fcb54e7', binding_adjudicated_interpretation).
narrative_ontology:cs_drift_state('215a1090-0010-4b36-be00-dc6d3fcb54e7', post_appellate_body_paralysis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('215a1090-0010-4b36-be00-dc6d3fcb54e7', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, major_litigant_powers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_export_industries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, united_states_trade_representative).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, small_trading_economies).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_generic_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_generic_sectors).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, united_states_trade_representative).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, vienna_convention_interpretation_canon).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, reverse_consensus_adoption_discipline).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, reasoned_written_report_precedent_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The membership acting through the Dispute Settlement Body adopts panel reports by reverse consensus, authorizes suspension of concessions against non-complying members, and appoints appellate members by unanimity - which is how the appellate tier emptied after 2017. It administers everything and originates nothing: once a panel is seised, the process runs on its own rails until adoption.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, constrained, global).

% Large developed members file most TRIPS complaints and defend most responses. They maintain permanent Geneva delegations and specialized trade-law teams, so each case costs little relative to their trade volume, and adopted reports accumulate into a body of interpretation their positions largely wrote. Because their markets are large, authorized retaliation against them is expensive for any challenger to sustain. Where the multilateral channel disappoints them they shift weight to plurilateral and bilateral venues where they hold comparable leverage.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, major_litigant_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Designed the dispute machinery in 1994 and remains its heaviest user, winning most of the cases it brings while occasionally bearing compliance costs when it loses. Since 2017 it has blocked appellate-body appointments over grievances that adjudicators exceeded their mandate, leaving the enforcement tier it built unable to complete appealed cases. Its market size lets it pursue the same goals through unilateral investigations and bilateral agreements regardless of what the multilateral channel yields.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, united_states_trade_representative, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, united_states_trade_representative, payer).

% Research-based drug companies in exporting countries benefit when adopted reports read patent protections expansively and exceptions narrowly. They supply the technical arguments and lobbying pressure behind many government complaints, and they route investment, manufacturing siting, and pricing decisions around jurisdictions whose interpretations displease them.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_export_industries, beneficiary,
    powerful, biographical, arbitrage, global).

% Rarely litigate TRIPS: the legal cost of one panel proceeding can exceed a trade ministry's annual budget, and a lost case binds them through adopted reports they had no hand in writing. Authorized retaliation against them is cheap for large complainants to impose and nearly impossible to answer in kind. Leaving the organization would forfeit every market-access commitment at once, so exit is theoretical.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, small_trading_economies, payer,
    powerless, biographical, trapped, global).

% Generic pharmaceutical industries in countries such as India, Brazil, Thailand and South Africa gained lawful room through the public-health flexibilities affirmed in 2001, and some won early cases. They nonetheless operate under standing litigation risk: every expansion of generic output invites complaint drafts, and the cost of defending a case disciplines production decisions regardless of outcome. Their position improves when flexibility-friendly interpretations prevail and erodes when precedent narrows health exceptions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_generic_sectors, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_generic_sectors, beneficiary).

% Health ministries of non-litigating members, treatment-access organizations, and patient groups have no standing before panels - only member governments can be parties, and amicus material reaches a panel only if the panel invites it. They advocate readings that prioritize medicine access and would contest precedent that narrows health exceptions, but the venue admits them solely at the discretion of the adjudicators they seek to persuade.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, global_health_access_movement, excluded,
    organized, generational, trapped, global).

% Academic specialists, former panelists, and secretariat lawyers publish the case-by-case record, track which readings harden into precedent and which stall, and document the widening gap between the machinery's stated design and its post-2019 operation. They collect no revenue from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_trade_law_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, major_litigant_powers).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single rules-based venue where disagreements about what TRIPS obligations mean are argued, decided through a reasoned written report, and (before 2019) reviewable on appeal - replacing ad hoc bilateral retaliation with a shared procedure whose outputs every member can anticipate. The authorization requirement converts retaliation from a unilateral act into a collectively sanctioned one.
% TRANSFER_FUNCTION: Moves interpretive control toward members able to fund repeated litigation and toward the readings their complaints advance; moves compliance pressure onto respondents through authorized withdrawal of tariff and IP concessions; distributes legal certainty to all members while concentrating precedent-setting capacity in a handful of heavy users.
% ABSENT_VOICES: Non-governmental health actors and the populations they represent, together with generic-industry associations of members that never litigate, are absent from the table: participation requires member-government sponsorship, and most members whose populations bear the medicine-access stakes have never brought a TRIPS case. Their objections surface only in political bodies such as the World Health Assembly, whose outputs do not bind panels.
% DISAPPEARANCE_RATIONALE: Overnight removal returns TRIPS interpretation to diplomatic note exchanges and unilateral retaliation lists: the 1980s pattern of section-301-style investigations would resume immediately, readings would settle according to market size rather than argument, and the existing stock of settled interpretations would stop constraining anyone. The rearrangement is already visible where the appellate tier went dark - disputes increasingly end in negotiated settlements weighted by leverage rather than adopted reasoning.
% FOUNDING_PROBLEM: Before 1995, intellectual-property trade conflicts were resolved by unilateral investigation and threat, chiefly the US Special 301 process, which invited tit-for-tat escalation and left smaller trading partners without any forum. The Uruguay Round negotiators built a mandatory, binding dispute procedure so that obligations under the new IP treaty would be interpreted and enforced by a common adjudicator rather than by the strongest economy's trade representative.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: US Government Accountability Office reporting and congressional testimony document renewed reliance on unilateral Section 301-type investigations after 2019; European Commission statements describe the appellate paralysis as reviving power-based settlement; and published trade-law scholarship on appeal-into-the-void adjudication reaches the same conclusion. No corroborating source outside the heavy-user set treats the founding problem as solved.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 by this reading's own lights over the standing arrangement it endorses: it concedes the litigation-cost asymmetry that prices small members out, the precedent stock written disproportionately by heavy users, and retaliation leverage that scales with market size, while treating these as partly the price of having any common adjudicator at all. Suppression is a raw, unscaled structural property and sits at 0.50: authorized retaliation and precedent chill deviating interpretations, but the machinery's suppressive force is currently degraded. The temporal series runs on ONE shared grid (1995, 2000, 2005, 2010, 2015, 2020, 2024) with every tracked metric authored at every point. Suppression_requirement traces an enforcement-capacity arc - machinery maturation through 2015 (compliance ratcheting, AB backlog clearance), then decay after the appellate tier emptied - which is why it is authored here despite being otherwise discouraged: the story specifically tracks enforcement-machinery buildup and erosion. Theater_ratio jumps sharply between 2015 and 2020 as panel reports began to be appealed into the void: adjudicatory activity continued while its binding completion did not, so a growing share of activity became performative maintenance of the appearance of bindingness. Accessibility_collapse at 0.50: alternatives persist (MPIA arbitration, FTA dispute chapters, unilateral 301-type measures, diplomatic settlement) but each carries certainty or legitimacy costs that keep members inside the multilateral channel. Resistance at 0.60: the appointment blockade is resistance mounted from inside the beneficiary class itself, joined by developing-country insistence on flexibility language and by unilateral-measure workarounds. Scalars equal the interval-end series values.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the major-litigant seats the arrangement presents as predictable rules-based settlement protecting export interests - rope-like. From the small-trading-economy seats the same machinery presents as binding discipline it cannot afford to invoke and cannot resist - extraction without voice. From the US seat the arrangement is ambivalent in an unusual way: its principal architect and beneficiary is also the source of its enforcement decay, resisting the appellate tier it built over perceived judicial overreach. From the excluded health-access seat the machinery is invisible except in its outputs, which arrive as adopted reports narrowing the flexibility space the 2001 ministerial declaration promised. Coalition capacity partially offsets the weakest seats: Brazil, India, and allied members won early cases and forced the 2001 declaration, showing organized payer coalitions can move outcomes even where individual seats cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the low-directionality end: major litigant powers and pharmaceutical exporters collect the arrangement's outputs (favorable precedent, deterred challenges), and their arbitrage-grade exits (plurilateral forums, bilateral deals, investment rerouting) push their derived d toward the beneficiary extreme. Small trading economies anchor the target end: trapped exit (single undertaking), zero litigation capacity, and binding precedent they did not write maximize their effective extraction. Developing-country generic sectors sit mid-range - formally payers whose secondary benefit from affirmed flexibilities dampens their derived d. The US seat derives low d as a beneficiary, but its secondary payer role and its active sabotage of the enforcement tier mark a genuine intra-beneficiary divergence the flat derivation understates; no directionality override is authored because the structural data (dual role, arbitrage exit) carries the signal and overrides keyed only by power atom cannot distinguish seats sharing the institutional atom. Excluded voices lie outside the derivation entirely - commentary-grade absence, never correction-grade input.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, not dead: unilateral power-based settlement has visibly returned wherever adjudication stalls, so the arrangement's original mandate has not outlived its function - mandatrophy_resolved is deliberately not declared. The tangled-rope classification is what prevents mislabeling in both directions: a pure-rope reading would ignore the measurable rents - precedent lock-in favoring repeat litigants, retaliation asymmetry, litigation-cost exclusion - that the payer seats demonstrably bear; a pure-snare reading would ignore that the same machinery replaced tit-for-tat trade warfare with reasoned public reports and that weaker members have occasionally won through it. The coordination function is genuine and the extraction rides on it through the same structure, which is exactly the hybrid signature. The post-2019 theater rise is monitored as a leading indicator: if bindingness continues to decay while adjudicatory performance continues, the arrangement drifts toward piton-like theatrical maintenance with bilateral snares doing the real work offstage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the trips_agreement_interpretive_kernel - the dispute_settlement_interpretive_authority reading. Would instantiating the strong_exclusivity_reading or public_health_flexibility_reading yield a structurally different constraint with a different epsilon and different victim sets?',
    'Classify each sibling as its own story and compare epsilon, beneficiary/victim structure, and per-seat classifications across the family; the disagreement lives at the interpretive-authority axis (who fixes meaning), which this file isolates from the substantive-content axes the siblings isolate.',
    'If assessments merge the readings into one label, the resulting epsilon matches no actual seat and the family''s indexical structure is destroyed; kept separate, the engine can detect which reading panel precedent is actually locking in.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed decomposition of the TRIPS kernel: this story authors one reading only.').

omega_variable(
    precedent_lock_in_extraction_boundary,
    'Does the accumulated stock of adopted reports constitute legitimate development of the treaty acquis, or extraction-by-lock-in that entrenches the readings of repeat litigants against the readings of members who never got to argue them?',
    'Counterfactual analysis of which readings would plausibly have prevailed absent adjudicated precedent (for example, flexibility readings suppressed before the 2001 declaration), combined with distributional analysis of who authors adopted reports versus who is bound by them.',
    'If lock-in dominates, effective extraction on payer seats runs well above the authored 0.55 and the arrangement''s classification trends snare-ward despite its real coordination function; if acquis development dominates, the coordination framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_lock_in_extraction_boundary, conceptual, 'Boundary between legitimate precedent accumulation and entrenched extraction.').

omega_variable(
    bilateral_substitution_pressure,
    'After the Appellate Body paralysis, is the declining formal suppression_requirement offset by equivalent bilateral coercive pressure operating outside the adjudicative machinery?',
    'Track unilateral investigation activity, bilateral settlement terms, and enforcement-regime actions against the formal suppression series; if target-side pressure holds steady while the formal series falls, the substitution is real.',
    'If substitution holds, the end-state scalar understates what targets actually face, and per-seat effective extraction for trapped members should be weighted higher than the falling formal trajectory suggests; if pressure genuinely fell, the enforcement-decay reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bilateral_substitution_pressure, empirical, 'Whether bilateral coercion replaces, rather than supplements, decaying formal enforcement.').

omega_variable(
    mpia_bifurcation_effect,
    'Does the Multi-Party Interim Appeal Arbitration arrangement restore bindingness for its participants, splitting the constraint into two regimes with different classifications for participant and non-participant seats?',
    'Compare compliance rates, precedent uptake, and settlement quality for MPIA-covered disputes versus appeal-into-the-void disputes over the coming interval.',
    'If bifurcation solidifies, the remaining coordination benefit concentrates among participants while non-participants bear the full extraction of an incomplete machinery - deepening seat divergence and pushing non-participant seats toward snare-side classifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mpia_bifurcation_effect, empirical, 'Two-tier regime risk from the interim arbitration substitute.').

omega_variable(
    retaliation_symmetry_question,
    'Is authorized retaliation a symmetric disciplinary instrument available to all members, or structural leverage that only large markets can wield credibly?',
    'Econometric comparison of authorized-retaliation episodes by complainant market size against respondent concession behavior, including cross-retaliation (IP-sector suspension) usage patterns.',
    'An asymmetry finding raises effective extraction on small-economy seats and strengthens tangled-rope certification over rope; a symmetry finding would support the coordination-first framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_symmetry_question, empirical, 'Whether the enforcement tier disciplines symmetrically or amplifies market-size asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_interp_authority_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.08).
narrative_ontology:measurement_basis(trips_interp_authority_tr_t1995, observed).
narrative_ontology:measurement(trips_interp_authority_tr_t2000, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2000, 0.1).
narrative_ontology:measurement_basis(trips_interp_authority_tr_t2000, observed).
narrative_ontology:measurement(trips_interp_authority_tr_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2005, 0.13).
narrative_ontology:measurement_basis(trips_interp_authority_tr_t2005, observed).
narrative_ontology:measurement(trips_interp_authority_tr_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2010, 0.17).
narrative_ontology:measurement_basis(trips_interp_authority_tr_t2010, observed).
narrative_ontology:measurement(trips_interp_authority_tr_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2015, 0.22).
narrative_ontology:measurement_basis(trips_interp_authority_tr_t2015, observed).
narrative_ontology:measurement(trips_interp_authority_tr_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2020, 0.36).
narrative_ontology:measurement_basis(trips_interp_authority_tr_t2020, observed).
narrative_ontology:measurement(trips_interp_authority_tr_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(trips_interp_authority_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(trips_interp_authority_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement_basis(trips_interp_authority_be_t1995, observed).
narrative_ontology:measurement(trips_interp_authority_be_t2000, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement_basis(trips_interp_authority_be_t2000, observed).
narrative_ontology:measurement(trips_interp_authority_be_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement_basis(trips_interp_authority_be_t2005, observed).
narrative_ontology:measurement(trips_interp_authority_be_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement_basis(trips_interp_authority_be_t2010, observed).
narrative_ontology:measurement(trips_interp_authority_be_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement_basis(trips_interp_authority_be_t2015, observed).
narrative_ontology:measurement(trips_interp_authority_be_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement_basis(trips_interp_authority_be_t2020, observed).
narrative_ontology:measurement(trips_interp_authority_be_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2024, 0.55).
narrative_ontology:measurement_basis(trips_interp_authority_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(trips_interp_authority_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement_basis(trips_interp_authority_su_t1995, observed).
narrative_ontology:measurement(trips_interp_authority_su_t2000, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2000, 0.47).
narrative_ontology:measurement_basis(trips_interp_authority_su_t2000, observed).
narrative_ontology:measurement(trips_interp_authority_su_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2005, 0.56).
narrative_ontology:measurement_basis(trips_interp_authority_su_t2005, observed).
narrative_ontology:measurement(trips_interp_authority_su_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2010, 0.61).
narrative_ontology:measurement_basis(trips_interp_authority_su_t2010, observed).
narrative_ontology:measurement(trips_interp_authority_su_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement_basis(trips_interp_authority_su_t2015, observed).
narrative_ontology:measurement(trips_interp_authority_su_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement_basis(trips_interp_authority_su_t2020, observed).
narrative_ontology:measurement(trips_interp_authority_su_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2024, 0.5).
narrative_ontology:measurement_basis(trips_interp_authority_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_flexibility_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'TRIPS interpretive authority' decomposes, per the epsilon-invariance principle, into one second-order authority constraint (this file) and two substantive readings of the same treaty kernel (strong_exclusivity_reading, public_health_flexibility_reading). Each carries its own epsilon over its own referent: this reading authors epsilon over the standing adjudicative arrangement by its own lights; the flexibility reading authors epsilon over the access-restrictive patent regime it contests; the exclusivity reading authors epsilon over the diluted-protection regime it opposes. The family is linked through affects_constraints. This reading sits upstream of both siblings: adopted panel reports change the operating environment - the precedent stock, the legitimacy conditions, the cost of advancing either substantive reading - without logically foreclosing either camp.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
