% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Practice Norm Complex (Hegemonic Extraction Reading)
 *   domain: international/political_economy
 *
 * SUMMARY:
 *   The Rules-Based International Order (RBIO) comprises formal norms of
 *   state sovereignty, non-intervention, human rights, and market-opening
 *   conditionality. This constraint is ONE READING of the RBIO kernel — the
 *   hegemonic extraction reading. This reading treats RBIO norms as a
 *   formally revisable but practically un-amendable system maintained by P5
 *   veto, where enforcement selectivity (norms enforced against the Global
 *   South, selectively ignored when applied to the Global North) reveals
 *   extractive intent rather than universal principle. The reading's core
 *   premise is: interventions lacking target-state authorization are coercive
 *   impositions dressed in legality language; conditionality that locks in
 *   capital advantage is coerced contract; the system benefits U.S./European
 *   capital and harms Global South states and subordinated populations. This
 *   is structurally distinct from the liberal institutional reading (which
 *   treats norms as consensus-based and capacity-enforcement gaps as
 *   innocent), and from the sovereignty-maximalist reading (which treats any
 *   intervention as illegitimate). The ENGINE computes per-seat
 *   classification from the structural data; seat divergence (what the P5
 *   member experiences vs. what a Global South state experiences vs. what a
 *   multinational corporation experiences) is the measurement the
 *   constraint-story apparatus takes.
 *
 * KEY AGENTS:
 *   - U.S./European capital: primary beneficiary, shapes which norms are enforced and against whom
 *   - Security Council P5: structural agenda-setters, preserve hegemonic order via veto
 *   - Global South states: formally included but structurally blocked, bear extraction costs
 *   - Populations under IMF/World Bank conditionality: powerless targets of enforced norm compliance
 *   - Unauthorized intervention targets: victimized by selective enforcement, no consent sought
 *   - Liberal institutional scholars: intellectual legitimacy suppliers for the order
 *   - Rising powers (China, India, Brazil): excluded from norm-revision via P5 veto
 *   - Human rights advocacy: operate inside legitimacy frame, constrained by dependence on elite funding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Practice Norm Complex (Hegemonic Extraction Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, 'b17d9ba3-5495-4eaa-90bb-046c5a5f14c5').
narrative_ontology:cs_kernel_codification('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', formalized).
narrative_ontology:cs_authority_grounding('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', extraction).
narrative_ontology:cs_interpretation_layer_present('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5').
narrative_ontology:cs_reading_relation('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', foundational, intervention_without_consent_is_coercion).
narrative_ontology:cs_axiom_status(intervention_without_consent_is_coercion, holdable).
narrative_ontology:cs_axiom_grounding('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', intervention_without_consent_is_coercion, deontological).
narrative_ontology:cs_axiom('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', foundational, enforcement_selectivity_reveals_extractive_intent).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reveals_extractive_intent, holdable).
narrative_ontology:cs_axiom_grounding('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', enforcement_selectivity_reveals_extractive_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', universal_rule_of_law_order).
narrative_ontology:cs_drift_state('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', contemporary_post_cold_war_enforcement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b17d9ba3-5495-4eaa-90bb-046c5a5f14c5', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, security_council_p5_members).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, populations_under_structural_adjustment).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, unauthorized_intervention_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutional_scholars).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, human_rights_advocacy_organizations).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, western_liberal_order_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, humanitarian_intervention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Capital based in the United States and Western Europe benefits from RBIO norms that protect property rights, enforce contracts in their favor, and legitimize market opening conditionality. Through World Bank, IMF, and bilateral mechanisms, they shape which norms are enforced against which states. They author the 'universal values' framing and define which violations warrant intervention.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital, agenda_setter).

% Permanently authorized to veto any resolution that would constrain them, the P5 (USA, Russia, China, UK, France) jointly author the formal revisability fiction. They maintain the veto coalition by implicit understanding: the norms are revisable in principle, but revisability is structurally blocked by consensus of the most powerful. The United States and European members use this to sustain norms favorable to Western capital; Russia and China use it to block norms that would constrain them on regional matters. The veto preserves hegemonic order.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, security_council_p5_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Formally included in the rule-making process through the UN General Assembly, but enforcement selectivity and P5 veto ensure that norms disadvantageous to them (on capital controls, intellectual property, resource extraction, conditionality) are enforced while norms meant to constrain powerful states (on intervention, sanctions, resource colonialism) are selectively ignored. They pay through conditionality, capital flight, and sovereign curtailment. Exit means regional isolation or sanctions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    organized, generational, constrained, global).

% Subordinate to states that have accepted IMF/World Bank conditionality backed by RBIO norms of market opening and fiscal discipline. They bear the costs directly: privatization of healthcare and education, austerity-driven wage compression, currency devaluation. The norms are enforced via loan conditions they never agreed to and cannot exit without state default. They have no formal seat in the rule-making process.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, populations_under_structural_adjustment, payer,
    powerless, biographical, trapped, national).

% States and populations targeted by military or economic intervention justified under RBIO humanitarian norms, without UN Security Council authorization or target-state consent. They experience the constraint as coercive law-breaking dressed in legality language. Exit is not possible; resistance ranges from non-compliance to violent opposition.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, unauthorized_intervention_targets, payer,
    powerless, biographical, trapped, national).

% Provide intellectual legitimacy for the RBIO order by teaching that norms are universal, consent-based, and revisable through legitimate processes. Their institutional position and grant funding derive from their role in maintaining the order's narrative coherence. They genuinely believe the framing, which strengthens the belief system's transmission.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutional_scholars, beneficiary,
    moderate, generational, mobile, global).

% Rising powers (China, India, Brazil, Russia) that would reshape RBIO norms if they could but lack the voting coalition to override P5 veto or rewrite the institutional architecture. They are formally included but structurally blocked. Their exclusion is enforced by the veto mechanism itself.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, alternative_authority_claimants, excluded,
    powerful, generational, trapped, global).

% Document violations of RBIO norms and call for enforcement, but their leverage is credibility and moral suasion. They operate inside the legitimacy frame provided by Western capital and P5 states, so they cannot credibly challenge the frame itself without losing institutional access and funding. They bear costs when their advocacy threatens elite interests but lack power to force change.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, human_rights_advocacy_organizations, observer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, human_rights_advocacy_organizations, payer).

% International relations scholars and policy analysts examining the constraint's structure. They see the formal revisability fiction and the enforcement selectivity without claiming to resolve whether the order is legitimate, natural, or constructed.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single rule-set for all states and populations (no unilateral regime change, sanctions require authorization, non-intervention norm, human rights standards). This provides predictability for capital flows, treaty-making, and conflict resolution; without it, the international order would fragment into competing claim-sets.
% TRANSFER_FUNCTION: Transfers sovereignty constraints from powerful states to weaker ones (Global South states accept IMF conditionality, accept no-intervention norm but see others intervened in unauthorized ways, accept property-rights enforcement that locks in colonial-era inequalities). Transfers resources from populations under structural adjustment to creditors and multinational capital. Transfers legitimacy from formal consent processes to hegemonic enforcement.
% ABSENT_VOICES: Populations under conditionality have no vote on IMF/World Bank boards; states that lost intervention disputes (Vietnam, Iraq, Libya, Syria, Yemen) have no seat in the tribunal that evaluated the interventions as legitimate or illegitimate. Rising powers that would revise the norm-set are blocked by P5 veto. Alternative framings of legitimate international order (sovereignty-maximalist, non-aligned, regional autarky) are excluded from the authoritative conversation, not by vote but by institutional architecture.
% DISAPPEARANCE_RATIONALE: If RBIO norms and their enforcement disappeared, capital flows would face radically reduced legal certainty, conditionality would lose its legitimacy framing and revert to naked coercion, humanitarian interventions would no longer benefit from UN-backed legality, and the sovereignty-maximalist reading would become the default. The Global South would immediately attempt norm revision. Rising powers would demand P5 veto abolition or institutional restructuring. The entire international legal order would reconfigure within years.
% FOUNDING_PROBLEM: Post-WWII instability: states lacked a shared rule-set to prevent wars, manage trade, and handle humanitarian crises. The UN system was built to codify respect for sovereignty while permitting collective action on existential threats. Early RBIO norms (non-intervention, collective security, self-determination) promised to embed shared authority that protected both sovereignty and humanitarian values.
% FOUNDING_PROBLEM_CORROBORATION: Western institutional scholars and diplomats attest the founding problem remains live (terrorism, regional aggression, humanitarian crises still require international coordination). Global South states and their scholars attest the founding problem was solved by the 1990s and the persistence of enforcement selectivity reveals the norms now serve extraction, not protection. NGOs focused on Global South rights attest to a schism: the coordination problem for the Global North exists; the extraction problem for the Global South is demonstrable and urgent.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading measures extractiveness at 0.78 because the norms lock in structural advantages (capital flows favor Western investors, property rights enforcement protects colonial-era holdings, conditionality transfers resources from Global South to creditors) and lack exit options for victim states. Suppression is 0.72 because enforcement requires active coercion (military intervention, sanctions, loan conditionality) and institutional power to define which norms apply to whom. Theater ratio is 0.61 because formal revisability is proclaimed but institutionally blocked (the veto ensures no revision can occur without P5 consensus), and humanitarian language disguises resource extraction. Accessibility_collapse is 0.68 because Global South states formally have the same vote-status but structurally cannot exit or revise (P5 veto is insurmountable, regionalism triggers sanctions, sovereignty assertion provokes intervention). Resistance is 0.59 because Global South states and rising powers mount continuous opposition (G77, BRICS, AU) but lack structural power to force change within the RBIO frame; resistance increases as the frame becomes more visible. The measurement series shows extraction accumulation from 1945 (minimal, founding legitimacy still strong) through 1990s (sharply rising as Cold War veto-trading ends and U.S./Europe enforce norms unilaterally) to 2024 (high and stable, enforcement selectivity now documented). Theater ratio rises steeply from 1990 onward as humanitarian framing intensifies alongside naked enforcement selectivity.
 *
 * PERSPECTIVAL GAP:
 *   The P5 agenda-setter and the Global South payer should compute as profoundly different types from the same constraint. For the agenda-setter, the RBIO is genuinely coordinative — a rule-set that permits action and provides legitimacy. For the payer, it is extraction with a coordination costume. The liberal scholar sits in analytical position but is incentivized to see the agenda-setter's frame (funding, career, institutional position all depend on legitimacy-framework maintenance). The engine computes this from: (1) power differences (institutional vs. organized vs. powerless), (2) exit option differences (arbitrage vs. constrained vs. trapped), (3) beneficiary/victim declarations. The readings diverge because the structural data diverges, not because observers disagree on facts — they disagree on which constraint they are observing.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S./European capital is a structural beneficiary (d ≈ 0.1): norms protect their capital, enforce contracts in their favor, legitimize market opening that disadvantages competitors. P5 members are hybrid (d ≈ 0.3-0.4): they benefit from veto power and selective enforcement, but also constrained by the rules they cannot unilaterally break without risking order collapse. Global South states are targets (d ≈ 0.85): they pay through conditionality, constrained sovereignty, selective enforcement, and lack arbitrage. Populations are trapped targets (d ≈ 0.95): they have no structural leverage at all. Alternative authority claimants (rising powers) are blocked targets (d ≈ 0.8): they would benefit from norm revision but cannot access it. The directionality ranges track power and exit options through the engine's derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII instability, need for shared rules) was real and acute in 1945–1970. By 1990, it was substantially solved: the Cold War ended, major-power war became unthinkable, international law codified and functioned. However, the institutional apparatus built to solve coordination persists and has been repurposed for extraction. The constraint is therefore in mandatrophy — the mandate has outlived its function. The theater ratio's rise (0.1 to 0.61) documents this: the proportion of enforcement activity that maintains formal legitimacy language grows as the proportion that solves the founding problem shrinks. This is exactly what permits classification as a tangled rope: real coordination (the rule-set does prevent some wars, does structure trade predictably) rides alongside real extraction (selective enforcement, conditionality, veto-blocking). Without the coordination residue, it would be a snare; without the extraction, it would be a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_revisability_fiction,
    'Is the P5 veto structurally necessary for RBIO stability (preventing unilateral changes that would destabilize the order), or is it structurally employed to preserve hegemonic advantage by blocking revisions that would distribute power more equally?',
    'Counterfactual: if veto were removed and decision-making moved to qualified majority (60% UN General Assembly), what norm revisions would succeed? Compare the set of blocked revisions against the interests of veto-holding powers. If all blocked revisions disadvantage veto-holders and advantage the Global South, the veto is hegemonic; if blocked revisions are genuinely destabilizing (would trigger Great Power war or order collapse), the veto is stabilizing.',
    'If the veto is hegemonic, the tangled-rope classification holds: real coordination (the rule-set is functional) rides alongside extractive maintenance (veto prevents revision). If the veto is stabilizing, the constraint shifts toward rope (coordination predominates; extraction is a side effect of maintaining order). This impacts the policy question of whether veto abolition is institutional reform or hegemonic displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_revisability_fiction, conceptual, 'The functional necessity of the P5 veto for order stability vs. its employment for hegemonic advantage.').

omega_variable(
    enforcement_selectivity_causation,
    'Does enforcement selectivity reveal extractive intent (this reading''s core claim), or does it result from capacity constraints (the liberal reading) and accident of history?',
    'Compare: (a) enforcement decisions where capacity would permit equal treatment but selectivity occurs anyway; (b) documented statements of intent by enforcement actors; (c) resource allocation patterns (do enforcement institutions receive more funding when selectivity is highest); (d) counterfactual: if powerful states were equally vulnerable to international enforcement, would the same norms be enforced equally?',
    'If selectivity is revealed-intent, the extractiveness remains high (0.78) and theater rises (the performance is deliberate). If selectivity is capacity-constraint-plus-accident, extractiveness might fall (0.50s) and the classification shifts toward rope or scaffold (temporary coordination asymmetry while capacity builds). The difference turns on whether the actors know what they are doing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_causation, empirical, 'Whether enforcement selectivity is intentional extraction or unintended consequence of capacity constraints.').

omega_variable(
    reading_under_determination_axiom,
    'Which reading of the RBIO kernel is correct — hegemonic extraction, liberal institutional, or sovereignty maximalist — depends on which foundational axiom about legitimacy is true. These axioms are irreducibly contested; no empirical evidence can resolve them.',
    'None. This is a preference-class omega. The dispute is over normative premises: whether humanitarian intervention requires consent (sovereignty-maximalist), whether lack of consent is a capacity problem to be solved (liberal-institutional), or whether lack of consent reveals the intervention is a coercive extraction (hegemonic-extraction). Different actors have incommensurable foundational commitments.',
    'This reading''s classification (tangled_rope) depends on accepting that extractive intent is real and harmful, not merely a pessimistic interpretation of ambiguous facts. The liberal reading rejects this axiom and classifies as rope. The sovereignty reading rejects both and classifies as snare or non-constraint. The engine computes per-seat classification; this omega documents that seat divergence is rooted in axiom choice, not in factual disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_under_determination_axiom, preference, 'Axiom-level disagreement over the legitimacy of consent-based vs. capacity-constrained vs. extraction-revealing interpretations of RBIO norms.').

omega_variable(
    conditionality_coercion_boundary,
    'At what point does loan conditionality become coercive extraction rather than legitimate contracting? The Global South reading places it near zero (any conditionality on loan receipt is coercion); the liberal reading places it at extreme invasiveness (only conditionality that violates human rights is coercive); the hegemonic extraction reading places it where the borrower has no real alternative and the lender exploits that dependency.',
    'Compare conditions offered by multilateral institutions (IMF, World Bank) to conditions offered by alternative lenders (bilateral development banks, regional banks, private markets). If multilateral conditions are more invasive than market conditions, conditionality is extractive (using state power to impose harsher terms than the market would). If multilateral conditions match market conditions, they are contractual. If borrower alternatives exist and are costlier, coercion exists but is not unique to RBIO.',
    'High: this determines whether structural adjustment itself is part of the extraction mechanism (as this reading holds) or a regrettable side effect of weak state capacity (liberal reading). If extractive, victims should include populations and the constraint is tangled_rope. If contractual, victims are limited to states that made poor deals, and the constraint shifts to rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_coercion_boundary, empirical, 'Where the boundary lies between contractual conditionality and coercive extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement_basis(rbio_tr_t1945, projected).
narrative_ontology:measurement(rbio_tr_t1970, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement_basis(rbio_tr_t1970, observed).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement_basis(rbio_tr_t1990, observed).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement_basis(rbio_tr_t2005, observed).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2015, 0.59).
narrative_ontology:measurement_basis(rbio_tr_t2015, observed).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2024, 0.61).
narrative_ontology:measurement_basis(rbio_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement_basis(rbio_be_t1945, projected).
narrative_ontology:measurement(rbio_be_t1970, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement_basis(rbio_be_t1970, observed).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement_basis(rbio_be_t1990, observed).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement_basis(rbio_be_t2005, observed).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2015, 0.74).
narrative_ontology:measurement_basis(rbio_be_t2015, observed).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2024, 0.78).
narrative_ontology:measurement_basis(rbio_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1945, 0.25).
narrative_ontology:measurement_basis(rbio_su_t1945, projected).
narrative_ontology:measurement(rbio_su_t1970, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement_basis(rbio_su_t1970, observed).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1990, 0.61).
narrative_ontology:measurement_basis(rbio_su_t1990, observed).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement_basis(rbio_su_t2005, observed).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement_basis(rbio_su_t2015, observed).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(rbio_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.25).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, sovereignty_norm_institution).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, humanitarian_intervention_legitimacy).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, capital_controls_prohibition).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, intellectual_property_enforcement).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_conditionality).

% DUAL FORMULATION NOTE:
% The RBIO kernel decomposes into at least three structurally distinct constraint readings. This file (hegemonic_extraction_reading) treats norms as extractive; see rbio_practice_norm_complex__liberal_institutional_reading (treats norms as universal-and-revisable) and rbio_practice_norm_complex__sovereignty_maximalist_reading (treats norms as legitimate only when protecting sovereignty). Sibling readings have different ε values, different beneficiary/victim structures, and different terminal types. The readings coexist as live positions held by different actors; they do not foreclose one another logically, though each claims the others are invalid. Network links here reflect the hegemonic extraction reading's structural influence on related constraints (sovereignty norms, humanitarian framing, capital rules, IP enforcement, conditionality) — all are reinterpreted as extraction mechanisms by this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
