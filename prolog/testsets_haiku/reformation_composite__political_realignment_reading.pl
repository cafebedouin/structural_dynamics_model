% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Territorial Sovereignty via Religious Differentiation (Political Realignment Reading)
 *   domain: political_economy/religious_history
 *
 * SUMMARY:
 *   Under the political realignment reading, the Reformation is fundamentally
 *   a state-formation event: emerging territorial rulers—particularly in the
 *   Germanic territories, Scandinavia, and England—use religious
 *   differentiation (adoption of reformed theology, breaking with Rome) as
 *   the mechanism to consolidate uncontested ecclesiastical authority within
 *   their borders and extract property and legitimacy that previously flowed
 *   to the papacy and Holy Roman Empire. The theological content of the
 *   Reformation (justification by faith, clerical marriage, vernacular
 *   scripture) is real but, under this reading, structurally secondary to the
 *   political function: theology provides the vocabulary and legitimacy for
 *   what is fundamentally a redistribution of authority and resources. The
 *   constraint's operation is a tangled rope: there is genuine coordination
 *   problem being solved (competing authority frameworks destabilizing
 *   political legitimacy), but the solution extracts from the papal and
 *   imperial institutional seats to benefit the territorial rulers. Cuius
 *   regio eius religio—the doctrine that the prince controls the church
 *   within his territory—is the primary observable of this reading and
 *   becomes the operative principle by 1648 (Peace of Westphalia).
 *
 * KEY AGENTS:
 *   - Emerging territorial rulers (Henry VIII of England, Frederick of Saxony, various Scandinavian and Germanic princes): assert sovereignty by adopting reformed theology and expropriating ecclesiastical property and appointment authority.
 *   - Papal ecclesiastical hierarchy: loses territorial revenue, appointment power, and doctrinal jurisdiction to breakaway rulers; bears the extraction.
 *   - Holy Roman Empire: its coordination framework (imperial-papal condominium) fractures; territorial rulers assert independent religious authority that dissolves the Empire's claim to universal Christian coordination.
 *   - Reformation theologians (Luther, Calvin, Zwingli): provide doctrinal vocabulary; benefit from princely patronage; identity-locked to theological content.
 *   - Peasants, urban artisans, lower clergy: drive initial reformist theology; excluded from the political settlement once rulers weaponize the movement.
 *   - Anabaptists and radical reformers: articulate theological reform without political consolidation; violently suppressed by territorial rulers (Catholic and Protestant alike).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.68).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.71).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Territorial Sovereignty via Religious Differentiation (Political Realignment Reading)").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "political_economy/religious_history").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '318ea696-9110-44d8-b4ed-4106f63be1ff').
narrative_ontology:cs_kernel_codification('318ea696-9110-44d8-b4ed-4106f63be1ff', distributed).
narrative_ontology:cs_authority_grounding('318ea696-9110-44d8-b4ed-4106f63be1ff', extraction).
narrative_ontology:cs_interpretation_layer_present('318ea696-9110-44d8-b4ed-4106f63be1ff').
narrative_ontology:cs_reading_relation('318ea696-9110-44d8-b4ed-4106f63be1ff', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('318ea696-9110-44d8-b4ed-4106f63be1ff', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('318ea696-9110-44d8-b4ed-4106f63be1ff', foundational, territorial_ruler_ecclesiastical_supremacy).
narrative_ontology:cs_axiom_status(territorial_ruler_ecclesiastical_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('318ea696-9110-44d8-b4ed-4106f63be1ff', territorial_ruler_ecclesiastical_supremacy, conventional).
narrative_ontology:cs_axiom('318ea696-9110-44d8-b4ed-4106f63be1ff', foundational, cuius_regio_eius_religio_legitimacy).
narrative_ontology:cs_axiom_status(cuius_regio_eius_religio_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('318ea696-9110-44d8-b4ed-4106f63be1ff', cuius_regio_eius_religio_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('318ea696-9110-44d8-b4ed-4106f63be1ff', papal_universal_ecclesiastical_authority).
narrative_ontology:cs_drift_state('318ea696-9110-44d8-b4ed-4106f63be1ff', reformation_consolidation_1648, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('318ea696-9110-44d8-b4ed-4106f63be1ff', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, reformed_political_autonomy).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_ecclesiastical_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_imperial_coordination).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, reformation_theologians).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, cuius_regio_eius_religio_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, sovereign_territorial_state_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central European princes, Scandinavian kings, and English monarchs assert control over ecclesiastical appointments, property, and doctrine within their territories. They use religious differentiation—adopting reformed theology or breaking with Rome—as a legitimacy mechanism to consolidate sovereignty against papal oversight and Holy Roman Imperial coordination. They directly collect ecclesiastical property and appointment revenue, and eliminate the papal pipeline of authority claims that historically challenged their domestic power.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_territorial_rulers, agenda_setter,
    powerful, generational, mobile, regional).

% The papacy and its episcopal hierarchy lose territorial revenue, appointment authority, and doctrinal jurisdiction to the breakaway rulers. From Rome's structural position, the Reformation is an extraction: the territorial rulers expropriate ecclesiastical property, exclude papal appointment power, and consolidate the resources and legitimacy that flowed to Rome. The papacy cannot exit—it is constitutively invested in Christendom-wide authority—but it can harden enforcement (Council of Trent, Inquisition intensification) to defend what remains.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_ecclesiastical_authority, payer,
    institutional, civilizational, constrained, continental).

% The Holy Roman Emperor's framework of coordination—universal Christendom under imperial-papal condominium—fractures as territorial rulers assert independent religious authority. The Empire's claim to coordinate spiritual and temporal power dissolves. From the imperial seat, the Reformation is extractive: the territorial rulers extract autonomy and resources by destroying the coordination architecture the Empire depended on.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_imperial_coordination, payer,
    institutional, generational, constrained, continental).

% The doctrine that the prince controls the church within his territory (cuius regio eius religio) becomes the operative principle. Rulers benefit from the legitimacy of leading a reformed church and from the property and authority that flow from it. This is not a person or institution but the consolidated autonomy-function itself—the political outcome the constraint enables.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, reformed_political_autonomy, beneficiary,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_non_agent(reformation_composite__political_realignment_reading, reformed_political_autonomy).

% Luther, Calvin, and other theologians provide the doctrinal vocabulary the territorial rulers use. They benefit from the protection and patronage of their adopting princes and from the authority their theology acquires through state backing. However, their primary function in this reading is observational: they translate political conflicts into theological language that rulers then weaponize. Their identity is fused to the theological content, which makes exit (renouncing the cause) psychologically unavailable even after the political realignment is complete.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, reformation_theologians, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, reformation_theologians, observer).

% Smaller principalities and city-states are forced to align with the larger powers' chosen religion or face coercive pressure. They are structurally excluded from setting the terms of the religious realignment even though their territories are affected. They cannot articulate a third theological position without being attacked by whichever major power uses their nonalignment as pretext.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, city_states_and_lesser_rulers, excluded,
    moderate, biographical, trapped, local).

% Academic observers of the Reformation can see multiple readings simultaneously. This reading privileges political causation; sibling readings privilege theology or technology. The observer seat perceives the structure without inherent commitment to any reading.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, reformation_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, emerging_territorial_rulers).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Legitimate the new territorial nation-state as the primary unit of Christian Christendom and consolidate authority structures fractured between Rome, the Empire, and emerging rulers. Before the Reformation, no single ruler could claim uncontested spiritual authority within his territory—the papacy retained appointment power and doctrinal veto. The Reformation allows each ruler to become the 'vicar of Christ' within his borders, solving the competing-authority problem by eliminating the supranational arbitrator.
% TRANSFER_FUNCTION: Transfers ecclesiastical property, appointment authority, doctrinal jurisdiction, and legitimacy from the papacy and Holy Roman Empire to territorial rulers. Money moves: rulers gain control of monastery lands, tithes, and indulgence revenues. Authority moves: rulers appoint bishops and interpret doctrine for their subjects. The papal and imperial seat loses both.
% ABSENT_VOICES: The peasants, urban artisans, and lower clergy who initially drove reformist theology are excluded from the settlement. Their voice—a genuine demand for doctrinal reform and ecclesiastical humility—is structurally eliminated once the territorial rulers have weaponized the movement for political consolidation. The Anabaptists, Radical Reformation figures, and sectarian movements are the most audible absent voices: they articulate theological reform without political coordination, and they are violently suppressed by both Catholic and Protestant territorial rulers once the political realignment is complete.
% DISAPPEARANCE_RATIONALE: If this constraint—the territorial ruler's assertion of ecclesiastical authority—had not emerged, the papacy would have retained its appointment and revenue streams, the Holy Roman Empire's coordination framework would have persisted longer, and the modern nation-state system would have crystallized differently or not at all. The disappearance of this constraint means the papacy retains its medieval territorial power, the Empire remains the operative framework for Christian unity, and the political economy of Europe follows a different trajectory.
% FOUNDING_PROBLEM: Competing claims to authority within Christian territories: popes claim universal spiritual jurisdiction, emperors claim temporal-spiritual coordination, emerging territorial rulers claim sovereign domestic authority. The medieval framework cannot accommodate the rise of nation-states without resolving which authority claims are senior. The Reformation is the mechanism that resolves this—by allowing rulers to claim the reformist mantle and establish uncontested authority within borders.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem—competing authority frameworks in early modern European politics—is attested by contemporary rulers' appeals to reformed theology as legitimacy for sovereignty claims (Henry VIII's break with Rome, the Peace of Augsburg's cuius regio eius religio formula), by papal and imperial countermeasures (Council of Trent, Jesuit counter-reformation, military support for Catholic powers), and by independent historical analysis of state formation in early modern Europe from scholars outside the benefiting parties (scholarship on the Treaty of Westphalia and the birth of the Westphalian system of sovereign nation-states). The problem statement is corroborated by political historians and institutional economists; the papal and territorial rulers' own testimony serves as evidence of the structure, not as the source.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1517, when dissent is localized and Rome's authority is still assumed) to 0.68 (1648, when cuius regio eius religio is settled doctrine and territorial rulers control appointment and revenue). Suppression rises from 0.42 to 0.71, tracking the escalation from initially tolerated dissent to the coordinated military and doctrinal enforcement that culminates in the Thirty Years' War (1618–1648). Theater rises from 0.18 to 0.42: the initial Reformation is genuinely about theological reform; by 1648, a growing share of the religious machinery is performative—preserving the doctrine of princely ecclesiastical authority even where genuine doctrinal commitment has cooled. Accessibility_collapse is moderate (0.64): alternatives to the new authority framework (continuing papal supremacy, a unified Catholic Christendom, radical reformation outside territorial control) are structurally suppressed but not impossible—they persist as suppressed counterfactuals until the Peace of Westphalia codifies the territorial system. Resistance is high (0.73): the papacy actively resists the extraction (Council of Trent, Jesuit counter-reformation, military support for Catholic powers), and the radical reformation resists the territorial rulers' cooption of reformism. Suppression and resistance rise together because the constraint's persistence requires crushing both papal authority and sectarian alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the territorial rulers' seat, this is pure coordination: solving the competing-authority problem and stabilizing the political order. From the papal/imperial seat, it is pure extraction: losing property, authority, and legitimacy that the medieval framework assigned to Rome and the Empire. The reformation theologians perceive it as theological victory and spiritual liberation (a beneficiary reading of their own position), but they are identity-locked to content they did not control—the rulers weaponized their theology for political ends. The radical reformers perceive it as a betrayal: the political realignment captures and suppresses the genuine religious reform they initiated. The engine should compute different types from different seats: the ruler seat computes rope or scaffold (coordination, temporary measure to reach a new equilibrium); the papal/imperial seat computes snare or tangled_rope (extraction, no meaningful exit). The claimed type (tangled_rope) reflects the mixture: real coordination function + asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers (powerful, mobile, gaining authority): d near 0.0–0.25 (beneficiaries). Papal and imperial institutional seats (institutional power, constrained by civilizational mandate): d near 0.75–1.0 (targets). Reformation theologians (moderate power, identity-locked): d near 0.5–0.65 (coordinated into the structure, bearing the identity fusion cost). Radical reformers (moderate power, trapped or identity-locked): d near 0.8–1.0 (targets, their dissent is suppressed). The directionality derivation from these beneficiary/victim declarations should produce the expected spread across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mislabeling the constraint as pure theology (which would undercount the extraction) or pure political opportunism (which would undercount the real coordination problem being solved). The medieval framework's competing authority claims (papal, imperial, territorial) genuinely destabilize political legitimacy and create a coordination problem. The territorial rulers' solution—claim the reformist mantle and assert ecclesiastical authority—is a real coordination mechanism that stabilizes the political order. BUT: the solution extracts from the papal and imperial seats. By 1648, the founding problem is solved (territorial sovereignty is consolidated), yet the religious machinery persists and thickens (baroque Catholicism, hardened Protestantism)—theater_ratio rises, suggesting the constraint is becoming performative. The mandatrophy signal: if the founding problem (competing authorities) is live, the constraint persists as tangled_rope; if the founding problem is dead (territorial sovereignty is settled), the constraint should downshift to piton or dissolve. The six_questions assessment (founding_problem_status = live at corroboration time) aligns with the tangled_rope claim, but future analysis should track whether the founding problem remains genuinely contested or has been settled by Westphalia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_theological_primacy,
    'Is the political realignment the PRIMARY cause of the Reformation, with theology as the vocabulary, or is theology PRIMARY with political rulers as secondary appropriators?',
    'Comparative analysis of other reformation movements where theology flourished but political realignment did not occur (early dissent movements), and of other political realignments where religious differentiation played no role. If theological movements without political realignment persist without gaining continental scale, and political realignments without theological content also succeed, the two are separable causal variables; if the Reformation required both, causation flows bidirectionally.',
    'If theology is truly secondary, this reading''s ε remains high and the extraction from the papal/imperial seat is clear. If theology is primary, the beneficiary/victim mapping inverts: theologians become primary beneficiaries and rulers become secondary appropriators; the constraint reclassifies to theological_fragmentation_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_theological_primacy, conceptual, 'Whether political realignment is the primary driver or theology is.').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who is the actual beneficiary of the political realignment: the territorial rulers themselves, or the abstract principle of territorial sovereignty?',
    'Trace the property and authority flows: rulers personally gain ecclesiastical property, appointment power, and revenue. ''Territorial sovereignty'' is the principle that enables this gain. If the constraint were removed but territorial principle persisted, would rulers still benefit? Empirical test: examine cases where territorial rulers adopt reformed theology but lose political power within a generation (weaker princes in the Swiss cantons, Scandinavian reversals).',
    'This affects the directionality of rulers: if they are the beneficiaries, d < 0.5; if the abstract principle is the beneficiary (and rulers are coordinated into supporting it), d approaches 0.5–0.65. The classification likely remains tangled_rope either way, but the seat divergence changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether territorial rulers or the sovereignty principle is the structural beneficiary.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of papacy/empire structural (they are locked out of territories by force and exclusionary laws) or internalized (they accept the loss and reframe it theologically)?',
    'Examine post-1648 papal and Catholic behavior: do they attempt military reconquest (structural suppression persists) or accept the territorial division (internalization)? The Council of Trent and Jesuit missions suggest structural suppression (Rome did not accept the loss, only hardened enforcement). By 1648, suppression appears structural, sustained by military power (Thirty Years'' War is the enforcement event).',
    'If purely structural, the constraint remains suppressive and extractive. If internalized (Rome reframes loss as necessary or theologically legitimate), the suppression metric should decline post-Westphalia, and the constraint might shift toward piton (atrophied but institutionally maintained). Evidence suggests structural suppression, supporting the tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of papal/imperial authority is structural or internalized.').

omega_variable(
    kernel_reading_underdetermination,
    'Does this reading''s privileging of political causation reflect the actual causal structure, or does it reflect a choice of what to measure?',
    'Per ε-invariance principle: this reading fixes its ε referent as the political realignment (territorial rulers'' assertion of ecclesiastical authority) assessed by this reading''s own lights. The theological_fragmentation_reading fixes its ε referent as the doctrinal incompatibility. The technological_mediation_reading fixes its ε referent as the printing press''s role. Each reading instantiates a different constraint with a different ε. The ambiguity is not resolvable by data—it is a choice about which causal chain to track (DP-001 irreducibility). This omega documents that choice.',
    'The three readings are epistemically incommensurable on the causation question. No empirical discovery will resolve which reading is ''really true''—each reading''s truth is internal to its own causal framing. The constraint as authored here is correct under the political realignment framing; the sibling readings are correct under theirs. The manifold of readings captures the irreducible underdetermination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Causal primacy is reading-indexed; no external fact settles which reading is ''the'' causal explanation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.18).
narrative_ontology:measurement_basis(refo_tr_t1517, observed).
narrative_ontology:measurement(refo_tr_t1550, reformation_composite__political_realignment_reading, theater_ratio, 1550, 0.28).
narrative_ontology:measurement_basis(refo_tr_t1550, observed).
narrative_ontology:measurement(refo_tr_t1580, reformation_composite__political_realignment_reading, theater_ratio, 1580, 0.38).
narrative_ontology:measurement_basis(refo_tr_t1580, observed).
narrative_ontology:measurement(refo_tr_t1610, reformation_composite__political_realignment_reading, theater_ratio, 1610, 0.41).
narrative_ontology:measurement_basis(refo_tr_t1610, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.42).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement_basis(refo_be_t1517, observed).
narrative_ontology:measurement(refo_be_t1550, reformation_composite__political_realignment_reading, base_extractiveness, 1550, 0.52).
narrative_ontology:measurement_basis(refo_be_t1550, observed).
narrative_ontology:measurement(refo_be_t1580, reformation_composite__political_realignment_reading, base_extractiveness, 1580, 0.61).
narrative_ontology:measurement_basis(refo_be_t1580, observed).
narrative_ontology:measurement(refo_be_t1610, reformation_composite__political_realignment_reading, base_extractiveness, 1610, 0.67).
narrative_ontology:measurement_basis(refo_be_t1610, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.68).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.42).
narrative_ontology:measurement_basis(refo_su_t1517, observed).
narrative_ontology:measurement(refo_su_t1550, reformation_composite__political_realignment_reading, suppression_requirement, 1550, 0.58).
narrative_ontology:measurement_basis(refo_su_t1550, observed).
narrative_ontology:measurement(refo_su_t1580, reformation_composite__political_realignment_reading, suppression_requirement, 1580, 0.67).
narrative_ontology:measurement_basis(refo_su_t1580, observed).
narrative_ontology:measurement(refo_su_t1610, reformation_composite__political_realignment_reading, suppression_requirement, 1610, 0.71).
narrative_ontology:measurement_basis(refo_su_t1610, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.71).
narrative_ontology:measurement_basis(refo_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_composite__political_realignment_reading, 0.12).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% The Reformation constraint family decomposes into three structurally distinct claims sharing a kernel (reformation_composite) but differing in causal attribution and beneficiary/victim structure. The political_realignment_reading privileges territorial ruler agency and the consolidation of ecclesiastical authority within borders. The theological_fragmentation_reading privileges doctrinal incompatibility as the causal engine. The technological_mediation_reading privileges the printing press's role in scaling dissent. Each reading instantiates a different constraint with different ε, beneficiary sets, and observable effects. The readings coexist—different historians emphasize different causal chains—rather than logically foreclosing each other. Treat as a constraint family linked by network edges; do not attempt to unify into one constraint (violates ε-invariance principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
