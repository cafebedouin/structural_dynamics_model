% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath Reciprocity (Vassal Coordination Reading)
 *   domain: political/legal/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the vassal-coordination reading of the
 *   feudal oath kernel: feudal oath as a mutual-obligation coordination
 *   mechanism enforced by charter text and witnessed by superior lords and
 *   ecclesiastical authority. The oath binds BOTH the lord (to provide
 *   protection and justice) and the vassal (to provide service and renders)
 *   to specified, written obligations. Neither party can unilaterally extract
 *   beyond the charter terms without triggering breach of oath — a violation
 *   that costs legitimacy, triggers vassal withdrawals, and invites superior
 *   feudal intervention. Under this reading, the extractiveness is LOW
 *   because the lord is constrained by the same charter text that binds the
 *   vassal; a lord who demands excess service risks oath-breaking suits and
 *   collective vassals' appeals to higher authority. The constraint is
 *   CLAIMED as rope (genuine mutual coordination) and the authored metrics
 *   describe low extractiveness, low suppression, and minimal theater —
 *   alignment that reflects the coherence of this reading's own framing.
 *
 * KEY AGENTS:
 *   - vassal_network: oath-bound military servants; moderate power; constrained exit (oath renunciation costs protection and land); generational time horizon
 *   - lord_estate_operator: sets charter terms; powerful position; arbitrage exit (can shift to extractive reading or seek merger with larger lord); generational time horizon
 *   - superior_lord_or_church: witnesses and enforces charter legitimacy; institutional power; analytical exit; regional scope
 *   - merchant_or_unfree_commons: excluded from oath rank; powerless; trapped exit; outside the coordination network
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.28).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.15).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath Reciprocity (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "political/legal/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '0f814fa2-2aef-4d88-affd-c7a94666ec35').
narrative_ontology:cs_kernel_codification('0f814fa2-2aef-4d88-affd-c7a94666ec35', fixed_text).
narrative_ontology:cs_authority_grounding('0f814fa2-2aef-4d88-affd-c7a94666ec35', lineage).
narrative_ontology:cs_interpretation_layer_present('0f814fa2-2aef-4d88-affd-c7a94666ec35').
narrative_ontology:cs_reading_relation('0f814fa2-2aef-4d88-affd-c7a94666ec35', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f814fa2-2aef-4d88-affd-c7a94666ec35', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('0f814fa2-2aef-4d88-affd-c7a94666ec35', foundational, mutual_charter_obligation_binding).
narrative_ontology:cs_axiom_status(mutual_charter_obligation_binding, holdable).
narrative_ontology:cs_axiom_grounding('0f814fa2-2aef-4d88-affd-c7a94666ec35', mutual_charter_obligation_binding, conventional).
narrative_ontology:cs_axiom('0f814fa2-2aef-4d88-affd-c7a94666ec35', foundational, oath_breach_liability_symmetric).
narrative_ontology:cs_axiom_status(oath_breach_liability_symmetric, holdable).
narrative_ontology:cs_axiom_grounding('0f814fa2-2aef-4d88-affd-c7a94666ec35', oath_breach_liability_symmetric, deontological).
narrative_ontology:cs_reference_frame('0f814fa2-2aef-4d88-affd-c7a94666ec35', charter_enforced_reciprocity).
narrative_ontology:cs_drift_state('0f814fa2-2aef-4d88-affd-c7a94666ec35', high_medieval_consolidation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('0f814fa2-2aef-4d88-affd-c7a94666ec35', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassal_network).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, lord_estate_operator).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, mutual_feudal_obligation_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, bounded_service_reciprocity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound vassals receive fixed military protection, dispute adjudication, and title security from their lord in return for fixed service obligations (military duty, council attendance, occasional renders of cash or goods). The charter text specifies both the lord's duties and the vassal's obligations, making both enforceable. Exit by renunciation of oath is structurally available but socially and economically costly: it means loss of protection, forfeiture of held lands, and severance of kinship-adjacent bonds with the lord's household.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassal_network, beneficiary,
    moderate, generational, constrained, local).

% Sets the terms of the oath via charter text (negotiated with each vassal or groups of similar rank), enforces the oath through custom and (where necessary) through exclusion from the protectorate, and collects the fixed service obligations and renders. The oath binds the lord to specified protective and adjudicative duties; breach exposes the lord to vassals' collective withdrawal of support or appeal to superior lords.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lord_estate_operator, agenda_setter,
    powerful, generational, arbitrage, local).

% Oversees the oath structure's legitimacy through feudal hierarchy or ecclesiastical authority. A superior lord can require oath-taking vassals to uphold their charter-sworn duties and may hear disputes if the direct lord breaches. The church sanctifies the oath, making breach a matter of conscience as well as custom.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, superior_lord_or_church, observer,
    institutional, generational, analytical, regional).

% Are not bound by feudal oath (not of oath-capable rank) and thus receive no contractual protection from the feudal reciprocity arrangement. They exist under the lord's dominion but outside the coordination network; their treatment depends on custom and the lord's unbound discretion.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, merchant_or_unfree_commons, excluded,
    powerless, immediate, trapped, local).

% Records the oath, its breaches, its renewal, and its enforcement — the archive that makes the charter enforceable across generations and across disputes. The scribe's record is the mechanism by which the bounded reciprocity is maintained against self-serving rewriting.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, chronicle_scribe_or_historian, observer,
    analytical, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a fixed, two-way binding of military protection, dispute adjudication, and title security from the lord in return for specified military service, council attendance, and monetary/commodity renders from the vassal. Both parties' obligations are written in charter text and thus revisable only by mutual assent or through superior feudal authority.
% TRANSFER_FUNCTION: Moves military labor and fixed renders (cash, goods, hospitality, counsel) from vassal to lord; moves military protection and adjudication (and implied legitimacy/honor) from lord to vassal. The flows are bounded by charter text specification, making them reciprocal rather than arbitrary.
% ABSENT_VOICES: Merchants, unfree commons, and lower-rank peasants are structurally excluded from oath-capable ranks and thus have no voice in the charter negotiation. They would object to their non-coverage if heard; they remain silent because the oath is not meant to include them.
% DISAPPEARANCE_RATIONALE: If feudal oath and its charter-enforced reciprocity vanished overnight, vassals would lose fixed military protection and title security, lords would lose reliable service and renders, dispute adjudication would collapse into pure hierarchical dominance, and the entire feudal pyramid would require reconstruction on new grounds — perhaps pure conquest, perhaps contract-of-service unbounded by mutual oath.
% FOUNDING_PROBLEM: Early medieval warfare and fragmented authority created constant low-level violence and title insecurity. A lord with unstable retinue could not hold territory; a vassal with no lord had no protection and no claim to land. Feudal oath solved: how to bind military loyalty stably, how to allocate land with enforceable claims, how to resolve disputes without pitched battles.
% FOUNDING_PROBLEM_CORROBORATION: Regional lords and their vassal councils attest the system still solves these problems (as long as charter terms are honored). Ecclesiastical authority corroborates that oath bonds are essential to order. However, emerging territorial monarchs contest whether feudal reciprocity (bounded obligations) or hierarchical extraction (unbounded authority) better serves kingdom stability — a strategic disagreement, not a factual one about whether the founding problem is solved.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness remains LOW across the 300-unit interval (0.18 to 0.28) because the charter-text binding is mutual and enforced. A lord who extracts beyond charter terms loses oath legitimacy and faces vassal collectives withdrawing military support — the internal cost is immediate and severe. The slight rise in extractiveness (0.18→0.28) reflects drift toward hierarchical interpretation of oath (lords gradually claiming wider discretion in interpreting 'service' and 'renders') rather than movement toward pure extraction; the plateau after t=150 suggests lords reach a limit beyond which vassals' collective resistance hardens. Suppression remains minimal because enforcement is chiefly through charter text review and oath-breach suits, not coercive dominance — theater is correspondingly low (mostly functional review). The measurement grid samples at 50-unit intervals so temporal drift is visible; every metric is authored at every time point (aligned grid).
 *
 * PERSPECTIVAL GAP:
 *   From the lord's seat, the oath is a framework for stable retinue and predictable revenue. From the vassal's seat, it is a guarantee against arbitrary extraction. From the superior lord's seat, it is an institutional mechanism that holds the feudal pyramid together. From the merchant's seat (excluded), it is an irrelevant aristocratic theater. The engine will compute these divergences from the power/exit atoms and the role declarations; the authored claim (rope) expects low inter-seat divergence in the type classification because the structure is genuinely symmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Vassal network: beneficiary (receives protection, title security, adjudication) with moderate power and constrained exit (oath renunciation is structurally possible but socially/economically costly). Directionality should compute near 0.3–0.4 (partial beneficiary, with some friction). Lord: agenda_setter (sets charter terms) with powerful position and arbitrage exit (can seek merger upward or reinterpret oath downward). Directionality should compute near 0.5 (setting the terms but also bound by them). The key structural fact is MUTUALITY: neither party can unilaterally escape the charter without loss of legitimacy and material consequence. This mutual binding is what makes it rope, not tangled_rope (which requires an identified asymmetric victim).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (military insecurity, title instability) is LIVE under this reading — the oath's mutual binding is what solves it. As monarchical authority hardens (t>150), lords begin reading the oath differently (toward extraction), and vassals begin reading it differently (toward resistance), but under THIS reading the problem persists and the solution remains. Mandatrophy would emerge only if the founding problem had been solved AND the oath persisted for other reasons — but this reading maintains that the oath persists BECAUSE it still solves the founding problem. No mandatrophy is authored here; the institutional stability from t=0 to t=300 supports that reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_text_enforceability_ambiguity,
    'How stable is the charter text''s enforceability across generations and disputes? Does the written oath maintain its binding power, or does interpretive drift allow lords to effectively rewrite obligations?',
    'Historical analysis of oath breach cases: do superior lords and church courts enforce charter terms as written, or do they allow lords'' interpretations to drift upward (toward extraction)? Examine recorded oath disputes across the interval.',
    'If charter text holds as written, this reading sustains as rope. If interpretive drift permits lords to extract beyond written terms without oath-breach liability, the constraint shifts toward lord_extraction_reading and becomes tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_text_enforceability_ambiguity, empirical, 'Whether charter-text binding enforceability sustains across the interval or erodes to reinterpretation.').

omega_variable(
    vassal_collective_action_capacity,
    'Can vassals actually organize collective oath-breach suits or withdrawal of support, or do they face coordination barriers that isolate individual breaches?',
    'Historical case studies of oath enforcement: when a lord breaches, do vassals coordinate a response, or does each vassal face the breach individually and capitulate? Count collective oath suits vs. individual appeals.',
    'If vassals can coordinate (high collective action), the mutual binding sustains because breach costs are visible and shared. If collective action is weak, each vassal''s exit option is individually trapped, and the constraint becomes snare (apparent mutuality masking isolated vulnerability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vassal_collective_action_capacity, empirical, 'Whether vassals have the capacity to enforce the oath mutually against a breaching lord.').

omega_variable(
    ecclesiastical_constraint_on_extraction,
    'How much of the measured low extractiveness is due to the oath''s charter-text reciprocity (this reading''s mechanism), and how much is due to ecclesiastical moral constraint (the sibling ecclesiastical_mediation_reading)?',
    'Compare regions where ecclesiastical authority is weak vs. strong, holding charter text constant. If extractiveness rises in low-ecclesiastical areas despite identical charters, the ecclesiastical constraint is load-bearing and should be treated as a separate coexisting mechanism.',
    'If ecclesiastical constraint carries substantial weight, the constraint is actually hybrid (rope + ecclesiastical mediation), and decomposition into separate stories may be warranted. If charter text binds equally regardless of ecclesiastical pressure, this reading is independent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_constraint_on_extraction, conceptual, 'Whether the constraint''s low extractiveness flows from charter-text reciprocity or from ecclesiastical moral constraint.').

omega_variable(
    extraction_drift_mechanism,
    'What drives the slight rise in extractiveness from t=0 (0.18) to t=150 (0.28)? Is it reinterpretation of charter terms, or accumulation of unwritten custom that erodes the charter''s binding effect?',
    'Examine charter amendments and dispute records: do lords claim broader authority through charter reinterpretation, or do they simply practice extractions that go unchallenged because vassals'' resistance weakens?',
    'If drift is reinterpretation, the constraint is moving toward lord_extraction_reading under the same kernel. If drift is erosion of enforcement, the constraint is becoming piton (the charter persists performatively while actual practice drifts toward extraction). Classification consequence: the t=150 plateau may indicate the sustainable limit of extraction under mutual oath framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_drift_mechanism, empirical, 'The mechanism driving the measured extractiveness rise and its plateau.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t50, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement_basis(feud_tr_t50, observed).
narrative_ontology:measurement(feud_tr_t100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 100, 0.07).
narrative_ontology:measurement_basis(feud_tr_t100, observed).
narrative_ontology:measurement(feud_tr_t150, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 150, 0.08).
narrative_ontology:measurement_basis(feud_tr_t150, observed).
narrative_ontology:measurement(feud_tr_t200, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 200, 0.09).
narrative_ontology:measurement_basis(feud_tr_t200, observed).
narrative_ontology:measurement(feud_tr_t300, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 300, 0.08).
narrative_ontology:measurement_basis(feud_tr_t300, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t50, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement_basis(feud_be_t50, observed).
narrative_ontology:measurement(feud_be_t100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 100, 0.26).
narrative_ontology:measurement_basis(feud_be_t100, observed).
narrative_ontology:measurement(feud_be_t150, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 150, 0.28).
narrative_ontology:measurement_basis(feud_be_t150, observed).
narrative_ontology:measurement(feud_be_t200, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 200, 0.27).
narrative_ontology:measurement_basis(feud_be_t200, observed).
narrative_ontology:measurement(feud_be_t300, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 300, 0.28).
narrative_ontology:measurement_basis(feud_be_t300, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t50, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(feud_su_t50, observed).
narrative_ontology:measurement(feud_su_t100, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 100, 0.14).
narrative_ontology:measurement_basis(feud_su_t100, observed).
narrative_ontology:measurement(feud_su_t150, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 150, 0.15).
narrative_ontology:measurement_basis(feud_su_t150, observed).
narrative_ontology:measurement(feud_su_t200, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 200, 0.16).
narrative_ontology:measurement_basis(feud_su_t200, observed).
narrative_ontology:measurement(feud_su_t300, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 300, 0.15).
narrative_ontology:measurement_basis(feud_su_t300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feudal_oath_reciprocity kernel. The kernel is a contested commitment: the same charter text and oath practice are read differently by different seats, producing different structural constraints. The vassal_coordination_reading instantiates low-ε rope (mutual binding). Sibling readings: lord_extraction_reading (high-ε snare, where the oath authorizes maximal extraction) and ecclesiastical_mediation_reading (moderate-ε rope, where oath is bounded by Christian charity). All three readings share the same kernel (the oath text and feudal institution); they differ in which structural facts about obligation and enforcement are salient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
