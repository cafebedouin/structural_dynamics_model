% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Article 4 NDC Regime Read Through Common But Differentiated Responsibilities
 *   domain: international climate governance / treaty law / political economy
 *
 * SUMMARY:
 *   This story instantiates the equity reading of the Paris Agreement's
 *   Article 4 kernel: NDCs are structurally differentiated obligations,
 *   interpreted through Common But Differentiated Responsibilities and
 *   Respective Capabilities (CBDR-RC), such that developed states carry
 *   binding mitigation trajectories and finance/technology-transfer duties
 *   while developing states retain wide policy space. This is a distinct
 *   constraint from the sovereigntist reading (voluntary self-determined
 *   pledges with no structural differentiation gate) and the supranational
 *   reading (binding ratchet toward net-zero with uniform international
 *   accountability) — each is authored as its own file with its own epsilon.
 *   Under the equity reading specifically, extraction is moderate and
 *   asymmetrically distributed: real transfer flows from developed-state
 *   fiscal and industrial actors toward developing-state coalitions and, more
 *   contestedly, toward the frontline populations those coalitions claim to
 *   represent.
 *
 * KEY AGENTS:
 *   - developing_state_coalitions: primary structural beneficiary and co-agenda-setter (organized/constrained) — extracts negotiating leverage and finance commitments via CBDR language
 *   - vulnerable_frontline_states: nominal ultimate beneficiary (powerless/trapped) — the equity claim is made in their name but they do not control its terms
 *   - developed_state_taxpayers and developed_state_industrial_sectors: primary payers (moderate-powerful/constrained) — bear binding mitigation and finance costs
 *   - major_emerging_emitters: secondary beneficiary (institutional/mobile) — the reading's differentiation architecture shelters their current high-emissions trajectory under legacy developing-country status
 *   - unfccc_secretariat_and_bodies: agenda-setting administrator (institutional/analytical) — operationalizes which reading governs day-to-day treaty practice
 *   - future_generations_globally: excluded (powerless/trapped) — invoked by all parties, represented by none
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.47).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.4).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Article 4 NDC Regime Read Through Common But Differentiated Responsibilities").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international climate governance / treaty law / political economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, 'f13e41c7-7b21-43f5-aa78-84242a44c1c6').
narrative_ontology:cs_kernel_codification('f13e41c7-7b21-43f5-aa78-84242a44c1c6', fixed_text).
narrative_ontology:cs_authority_grounding('f13e41c7-7b21-43f5-aa78-84242a44c1c6', distributed).
narrative_ontology:cs_reading_relation('f13e41c7-7b21-43f5-aa78-84242a44c1c6', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f13e41c7-7b21-43f5-aa78-84242a44c1c6', paris_article_4_ndc__supranational_reading, influences).
narrative_ontology:cs_axiom('f13e41c7-7b21-43f5-aa78-84242a44c1c6', foundational, historical_emissions_ground_differentiated_duty).
narrative_ontology:cs_axiom_status(historical_emissions_ground_differentiated_duty, holdable).
narrative_ontology:cs_axiom_grounding('f13e41c7-7b21-43f5-aa78-84242a44c1c6', historical_emissions_ground_differentiated_duty, deontological).
narrative_ontology:cs_axiom('f13e41c7-7b21-43f5-aa78-84242a44c1c6', secondary, capacity_asymmetry_requires_structural_not_procedural_differentiation).
narrative_ontology:cs_axiom_status(capacity_asymmetry_requires_structural_not_procedural_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('f13e41c7-7b21-43f5-aa78-84242a44c1c6', capacity_asymmetry_requires_structural_not_procedural_differentiation, empirically_contingent).
narrative_ontology:cs_reference_frame('f13e41c7-7b21-43f5-aa78-84242a44c1c6', rio_1992_common_but_differentiated_responsibilities_compact).
narrative_ontology:cs_drift_state('f13e41c7-7b21-43f5-aa78-84242a44c1c6', post_paris_ndc_implementation_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f13e41c7-7b21-43f5-aa78-84242a44c1c6', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_state_coalitions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, vulnerable_frontline_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_taxpayers).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_industrial_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, major_emerging_emitters).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, major_emerging_emitters).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, common_but_differentiated_responsibilities_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, historical_emissions_liability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiating blocs (G77, LMDC, AOSIS-adjacent groupings) invoke CBDR-RC to retain wide policy space in their own NDCs while pressing for binding finance and technology-transfer obligations on developed parties. They hold real veto leverage inside the UNFCCC consensus process — CBDR language cannot be struck from decision text without their agreement — which is where their power comes from, not from wealth or enforcement capacity.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_state_coalitions, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, developing_state_coalitions, agenda_setter).

% Small island and least-developed states face existential climate exposure with negligible historical emissions. The equity reading channels loss-and-damage and adaptation finance claims toward them, but they depend entirely on the coalition's negotiating weight and on developed-state compliance they cannot compel directly; their own exit from the physical impacts does not exist.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, vulnerable_frontline_states, beneficiary,
    powerless, civilizational, trapped, global).

% Public finance commitments (the $100bn/yr goal and its successor NCQG) are funded through appropriations that draw on general revenue. Individual taxpayers have no direct voice in the CBDR allocation formula and bear the fiscal transfer as a diffuse cost embedded in budgets they did not choose, with no practical exit short of electoral change in donor-country politics.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_taxpayers, payer,
    moderate, biographical, constrained, national).

% Energy-intensive industries in Annex I economies face binding domestic mitigation trajectories and carbon-pricing exposure justified in part by CBDR's historical-responsibility logic, while comparable industries in major emerging economies retain differentiated, less binding NDC commitments. They can lobby and relocate production but cannot exit the treaty framework without their home state withdrawing from Paris entirely.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_industrial_sectors, payer,
    powerful, biographical, constrained, continental).

% States like China, India, and Brazil are formally classified as developing under the UNFCCC's 1992 annexes despite now being major current emitters. The equity reading preserves their differentiated obligations, giving them mitigation flexibility disproportionate to their present emissions share — a genuine benefit of the reading, though they also face growing informal pressure and reputational cost for using it.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, major_emerging_emitters, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, major_emerging_emitters, payer).

% The Secretariat, the Adaptation Fund, and related bodies administer the differentiation architecture — determining which finance windows, reporting formats, and review flexibilities apply to which states. They interpret and operationalize CBDR-RC in ways that shape which reading of Article 4 actually governs practice, without themselves being a party bound by any NDC.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, unfccc_secretariat_and_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Bear the cumulative consequence of however the differentiation is calibrated, but have no seat in either the equity coalition's negotiating position or the developed-state fiscal debate. Their interests are invoked rhetorically by all sides but structurally represented by none.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, future_generations_globally, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves the problem that a uniform mitigation obligation applied to states at radically different levels of historical emissions, current capacity, and development need would be both unjust and politically unratifiable — CBDR-RC lets a near-universal treaty exist at all by making differentiation the price of participation.
% TRANSFER_FUNCTION: Moves finance, technology-transfer commitments, and binding-mitigation burden from developed-state public budgets and regulated industrial sectors toward developing-state governments and, nominally, toward vulnerable frontline populations — though the coalition capturing the negotiating leverage and the frontline populations actually exposed to climate harm are not the same set of actors.
% ABSENT_VOICES: Future generations and non-state climate-vulnerable populations (indigenous communities, small farmers) inside both developed and developing states have no direct standing in the state-to-state negotiation that fixes the differentiation formula; frontline states' interests are represented by coalition partners whose own emissions profiles and priorities diverge from theirs.
% DISAPPEARANCE_RATIONALE: If the CBDR-RC reading were abandoned in favor of undifferentiated obligations, the entire NDC finance architecture (Green Climate Fund allocations, NCQG negotiations, loss-and-damage fund eligibility) would need renegotiation, several major developing-state parties would face acute domestic political pressure to reconsider participation, and the treaty's near-universal ratification (achieved partly because differentiation made joining low-cost for most states) could not be assumed to hold.
% FOUNDING_PROBLEM: The 1992 UNFCCC and later Paris Agreement needed near-universal participation, but developed states had emitted the overwhelming majority of cumulative historical carbon while developing states needed emissions headroom for basic development — a uniform obligation would have been both empirically unjust and politically impossible to ratify.
% FOUNDING_PROBLEM_CORROBORATION: Developing-state coalitions and independent equity scholars (e.g. climate justice academics, IPCC WG3 equity chapter contributors) attest the underlying disparity in historical responsibility and adaptive capacity remains live. Developed-state governments and some environmental economists counter that the 1992 developed/developing binary is now empirically stale given emissions growth in major emerging economies, making the founding problem's original diagnosis partially obsolete even as its remedy persists in treaty text.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.47, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.47) is moderate rather than high because CBDR-RC's transfer obligations, while real, are not backed by supranational enforcement — developed states can and do underdeliver on finance pledges without treaty-level sanction, capping how much is actually extracted relative to the nominal commitment. Suppression (0.4) sits at a similar moderate band: consensus-based UNFCCC procedure gives the coalition real veto power over any effort to strip differentiation from decision text, but no party is coerced into the regime by force — withdrawal (as the US demonstrated) remains a live exit. Theater ratio rises across the interval (0.2 to 0.38) as the gap widens between the rhetorical weight placed on CBDR-RC in negotiating text and the actual finance delivered against pledges (the persistent shortfall against the $100bn/yr goal is the clearest evidence). Accessibility collapse is moderate-low (0.35): alternative readings of Article 4 remain live and contested in every COP, so no single interpretation has foreclosed the others.
 *
 * PERSPECTIVAL GAP:
 *   From the developing-coalition seat, this is coordination correcting a historical injustice — a rope with a strong distributive justification. From the developed-industrial-sector seat, the same structural clause reads as an enforced asymmetric transfer riding on a real but partial coordination rationale — closer to tangled rope. Both seats are looking at the identical treaty text; the divergence is exactly the seat-relative computation the engine is built to surface, not an error to be reconciled by picking one 'true' reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Developing-state coalitions and frontline states are coded as beneficiaries because the equity reading's entire structural point is to route obligations away from them and toward developed parties; their directionality sits toward the beneficiary end despite frontline states' near-total powerlessness, because the constraint's design — not their capacity — determines the flow. Developed-state taxpayers and industrial sectors are coded as payers with constrained exit: they cannot unilaterally exit the differentiation architecture without their state withdrawing from Paris altogether, a civilizational-stakes political act. Major emerging emitters occupy a genuinely dual position (beneficiary + payer) and are given mobile exit options reflecting their real capacity to renegotiate their own classification over time, which the two Annex-locked payer groups lack.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (historical emissions asymmetry requiring differentiated burden) remains substantively live for the poorest and most exposed states, but the 1992 developed/developing binary the reading still relies on has become empirically strained by three decades of emissions growth in classified-developing economies. This is not full mandatrophy — the equity function has not died — but it is a live status:contested case: the same treaty architecture that channels genuine transfer to frontline states also shelters emissions growth in states whose current capacity increasingly resembles the developed category the differentiation was built to distinguish from.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    differentiation_criteria_staleness,
    'Does the 1992 UNFCCC Annex I/non-Annex I binary that CBDR-RC''s differentiation still formally relies on accurately track present-day capacity and responsibility, or has it become a legacy classification that shelters high-current-emissions states under a historical-liability rationale that no longer matches their emissions profile?',
    'Comparative analysis of current per-capita and cumulative emissions trajectories for major non-Annex I emitters against Annex I states, cross-referenced against IPCC equity-chapter capacity indices; formal renegotiation attempts (or their absence) at successive COPs as evidence of live contestation.',
    'If the classification is substantially stale, the equity reading''s coordination justification weakens for the major-emerging-emitter subset while remaining intact for genuinely poor and vulnerable states — suggesting the single ''developing'' category should itself decompose into further constraint stories rather than being treated as a unitary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differentiation_criteria_staleness, empirical, 'Whether the developed/developing binary underlying this reading still tracks the responsibility and capacity facts it was built to encode.').

omega_variable(
    coalition_representation_gap,
    'Do the negotiating coalitions that hold CBDR-RC''s veto leverage actually represent the interests of the frontline populations the equity reading is rhetorically built to protect, or has coalition leverage been captured by the interests of larger, higher-capacity developing-state members?',
    'Track record analysis of loss-and-damage and adaptation-finance allocation outcomes against AOSIS/LDC-specific proposals versus G77-bloc-wide negotiating positions; interview-based or documentary evidence of intra-coalition bargaining.',
    'If representation is substantially captured, the beneficiary designation for vulnerable_frontline_states is partly nominal, and the constraint''s actual beneficiary set narrows toward the larger, more capable coalition members — a finding that would push the classification toward tangled_rope more strongly and identify a victim-within-beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_representation_gap, empirical, 'Whether coalition leverage under CBDR-RC actually flows through to the most vulnerable states or is substantially captured by larger developing-state members.').

omega_variable(
    kernel_reading_selection_stakes,
    'Given that sovereigntist, equity, and supranational readings of Article 4 all remain textually defensible, is the ongoing selection among them primarily a legal-interpretive question or primarily a power contest determined by which coalition can hold a blocking position at COP?',
    'This is the committer-structure question for the kernel as a whole: it is documented here as an omega rather than resolved within this single reading, per the framework''s Rule 2 routing.',
    'If selection is primarily a power contest rather than interpretive, the equity reading''s stability depends entirely on the coalition''s continued blocking capacity at COP, not on any settled legal meaning of Article 4 — meaning this constraint''s classification is more contingent on ongoing negotiating leverage than its treaty-text framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_stakes, conceptual, 'Whether the three sibling readings of Article 4 are adjudicated by legal interpretation or by ongoing negotiating power, and where that leaves this reading''s stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t1992, paris_article_4_ndc__equity_reading, theater_ratio, 1992, 0.2).
narrative_ontology:measurement(pari_tr_t1997, paris_article_4_ndc__equity_reading, theater_ratio, 1997, 0.24).
narrative_ontology:measurement(pari_tr_t2009, paris_article_4_ndc__equity_reading, theater_ratio, 2009, 0.28).
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(pari_tr_t2019, paris_article_4_ndc__equity_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(pari_tr_t2024, paris_article_4_ndc__equity_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(pari_be_t1992, paris_article_4_ndc__equity_reading, base_extractiveness, 1992, 0.28).
narrative_ontology:measurement(pari_be_t1997, paris_article_4_ndc__equity_reading, base_extractiveness, 1997, 0.31).
narrative_ontology:measurement(pari_be_t2009, paris_article_4_ndc__equity_reading, base_extractiveness, 2009, 0.36).
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(pari_be_t2019, paris_article_4_ndc__equity_reading, base_extractiveness, 2019, 0.44).
narrative_ontology:measurement(pari_be_t2024, paris_article_4_ndc__equity_reading, base_extractiveness, 2024, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t1992, paris_article_4_ndc__equity_reading, suppression_requirement, 1992, 0.22).
narrative_ontology:measurement(pari_su_t1997, paris_article_4_ndc__equity_reading, suppression_requirement, 1997, 0.26).
narrative_ontology:measurement(pari_su_t2009, paris_article_4_ndc__equity_reading, suppression_requirement, 2009, 0.3).
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.33).
narrative_ontology:measurement(pari_su_t2019, paris_article_4_ndc__equity_reading, suppression_requirement, 2019, 0.37).
narrative_ontology:measurement(pari_su_t2024, paris_article_4_ndc__equity_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__equity_reading, 0.12).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, green_climate_fund_allocation).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, loss_and_damage_fund_eligibility).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'the Paris Agreement NDC obligation' per the ε-invariance principle: equity_reading (this file, moderate epsilon, asymmetric distribution, tangled_rope), sovereigntist_reading (near-zero epsilon, minimal differentiation gate, closer to rope), and supranational_reading (higher epsilon concentrated uniformly across major emitters regardless of development status, closer to snare/tangled_rope from the sovereignty-preserving seat). Each reading is a distinct constraint with its own beneficiary/victim structure and its own stable epsilon; they are linked here rather than merged because measuring 'the NDC obligation' under different interpretive lenses produces genuinely different extraction profiles, which is precisely the decomposition trigger the framework specifies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
