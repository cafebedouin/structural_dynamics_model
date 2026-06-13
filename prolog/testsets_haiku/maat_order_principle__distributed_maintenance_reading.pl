% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Distributed Maintenance Principle (Shared Cosmic Responsibility Reading)
 *   domain: political_philosophy/religious_studies/ancient_history
 *
 * SUMMARY:
 *   This constraint embodies the distributed-maintenance reading of the Ma'at
 *   principle in ancient Egyptian political theology. Under this reading,
 *   cosmic order is sustained through the proper conduct of all actors in
 *   their station — Pharaoh through ritual and justice, priesthood through
 *   interpretation and performance, officials through administration,
 *   craftspeople through honest work, peasants through proper labor and
 *   respect for hierarchy. Authority and legitimacy flow from demonstrated
 *   maintenance function, not from inherent status or divine mandate alone.
 *   The constraint is CLAIMED as rope (genuine coordination function with net
 *   benefits for the parties) and the metrics support this: low
 *   extractiveness (0.31), low suppression (0.28), and moderate theater ratio
 *   (0.22). The distributed-maintenance reading offers the lowest extraction
 *   profile of the three sibling readings because authority is grounded in
 *   observable function rather than in concentrated power or pure status.
 *   However, the reading remains contested — the divine-mandate reading
 *   denies that maintenance is truly distributed, while the reciprocity
 *   reading emphasizes mutual obligation rather than station-based role.
 *
 * KEY AGENTS:
 *   - Pharaoh (as station-holder, not divine incarnate): maintains order through ritual and justice, legitimacy grounded in function
 *   - Priesthood (interpreters and witnesses): maintain Ma'at doctrine and ritual, authority grounded in demonstrated religious knowledge
 *   - Administrative officials (order-keepers at their level): maintain order through justice and resource management, legitimacy from functional necessity
 *   - Educated scribal class: maintain Ma'at through documentation and legal support, authority from knowledge
 *   - Craftspeople and merchants: maintain Ma'at through honest conduct in their station
 *   - Peasants (agricultural labor): maintain cosmic order through proper work and respect for hierarchy
 *   - Conquered populations (structurally excluded from full party to the covenant): bear obligations but cannot be full authorities under this reading
 *   - Theological analyst: observes the constraint from outside the Egyptian cosmology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.31).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.28).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Distributed Maintenance Principle (Shared Cosmic Responsibility Reading)").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "political_philosophy/religious_studies/ancient_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '2d63aaf5-65a1-4fef-8a24-7be46ca21734').
narrative_ontology:cs_kernel_codification('2d63aaf5-65a1-4fef-8a24-7be46ca21734', distributed).
narrative_ontology:cs_authority_grounding('2d63aaf5-65a1-4fef-8a24-7be46ca21734', practice).
narrative_ontology:cs_interpretation_layer_present('2d63aaf5-65a1-4fef-8a24-7be46ca21734').
narrative_ontology:cs_reading_relation('2d63aaf5-65a1-4fef-8a24-7be46ca21734', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d63aaf5-65a1-4fef-8a24-7be46ca21734', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('2d63aaf5-65a1-4fef-8a24-7be46ca21734', foundational, maintenance_distributed_across_stations).
narrative_ontology:cs_axiom_status(maintenance_distributed_across_stations, holdable).
narrative_ontology:cs_axiom_grounding('2d63aaf5-65a1-4fef-8a24-7be46ca21734', maintenance_distributed_across_stations, conventional).
narrative_ontology:cs_axiom('2d63aaf5-65a1-4fef-8a24-7be46ca21734', foundational, authority_grounded_in_demonstrated_function).
narrative_ontology:cs_axiom_status(authority_grounded_in_demonstrated_function, holdable).
narrative_ontology:cs_axiom_grounding('2d63aaf5-65a1-4fef-8a24-7be46ca21734', authority_grounded_in_demonstrated_function, instrumental).
narrative_ontology:cs_reference_frame('2d63aaf5-65a1-4fef-8a24-7be46ca21734', hierarchical_order_sustained_by_collective_maintenance).
narrative_ontology:cs_drift_state('2d63aaf5-65a1-4fef-8a24-7be46ca21734', late_dynastic_period_weakening_centralization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2d63aaf5-65a1-4fef-8a24-7be46ca21734', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, all_maintainers_collective).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, cosmic_order_itself).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh_as_station_holder).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, priesthood_interpreters).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, administrative_officials).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, scribal_educated_class).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, craftspeople_merchants).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, peasants_agricultural_labor).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, pharaoh_as_station_holder).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, administrative_officials).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, peasants_agricultural_labor).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, conquered_populations).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, distributed_moral_agency).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, station_based_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the highest station in the cosmic order and bears responsibility for maintaining Ma'at at that level: conducting proper rituals, ensuring justice in disputes affecting the realm, maintaining order of the Nile, and performing religious duties. Benefits from the principle by having legitimate authority grounded in demonstrable maintenance rather than arbitrary command. Constrained by the principle: failure to perform maintenance duties — visible neglect of ritual, unjust judgment, allowing disorder — undermines the Pharaoh's legitimacy to rule.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh_as_station_holder, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh_as_station_holder, payer).

% Maintains and interprets Ma'at doctrine, performs rituals that sustain cosmic order, and acts as witness to whether the Pharaoh and other stations are meeting their maintenance duties. Benefits from the principle by holding institutional authority grounded in demonstrated religious knowledge and function. Constrained by accountability: if priestly interpretations diverge radically from observable consequences, the priesthood's authority to interpret comes into question.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, priesthood_interpreters, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, priesthood_interpreters, beneficiary).

% Maintain Ma'at through administrative function: ensure grain distribution, adjudicate disputes, oversee public works, and enforce contracts. Benefit from the principle by having legitimate authority grounded in cosmic necessity rather than pure extractive power. Constrained by demonstrable accountability: officials whose decisions produce disorder, injustice, or waste fail the Ma'at maintenance standard and lose legitimacy.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, administrative_officials, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, administrative_officials, payer).

% Educated to understand Ma'at principle and required to document and enforce it: record-keeping, legal adjudication support, and ritual documentation. Benefit from the principle by having recognized status and authority flowing from knowledge and function. Constrained by the same accountability: a scribe who falsifies records or supports unjust judgments fails Ma'at maintenance and forfeits legitimacy.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, scribal_educated_class, beneficiary,
    moderate, biographical, constrained, national).

% Maintain Ma'at through proper conduct in their station: honest dealings in trade, quality workmanship, and fair pricing. Benefit from the principle by having their livelihoods recognized as morally necessary to cosmic order. Constrained by demonstrable accountability: fraud, theft, or deliberate poor work undermines the craftsperson's standing in the community.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, craftspeople_merchants, beneficiary,
    moderate, biographical, mobile, local).

% Maintain Ma'at through proper agricultural labor, paying obligations to temple and state, and accepting station hierarchy. Benefit from the principle by having their labor recognized as morally necessary to cosmic order. Constrained by trapped exit options: the principle protects them from arbitrary cruelty but does not change their structural position. The Pharaoh and officials, by the principle itself, must provide security and basic subsistence to peasants who sustain the realm, but cannot provide freedom or equality.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, peasants_agricultural_labor, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, peasants_agricultural_labor, payer).

% Incorporated into the cosmic order and required to accept Egyptian administrative order while bearing tribute and labor obligations. The distributed maintenance principle offers them limited protection: they are not full parties to the Ma'at covenant and cannot be full authorities, but the principle does constrain Egyptian officials from arbitrary cruelty. Their exit options are zero — resistance provokes suppression; departure is prevented by force.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, conquered_populations, payer,
    powerless, immediate, trapped, regional).

% Examines the constraint from outside the Egyptian cosmology: studies how the distributed maintenance principle operates in practice, whether it genuinely constrains power holders, and how it distinguishes itself from pure authority-justification narratives. Does not collect from or pay into the system; analyzes its structure and compares it to sibling readings.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, observer_theological, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of how a hierarchical society maintains internal order, legitimacy, and resource allocation without a centralized enforcement mechanism independent of the hierarchy itself. Each actor from Pharaoh to peasant maintains order through proper conduct in their station; collective stability emerges from distributed accountability rather than top-down coercion alone. The principle generates a shared framework for what counts as legitimate authority and what counts as failure, enabling peaceful resolution of succession disputes and administrative challenges when a station-holder fails their maintenance duty.
% TRANSFER_FUNCTION: Moves resources and labor upward through hierarchy — peasant produce to officials, official revenue to Pharaoh, Pharaonic resources to priesthood — justified by the principle that each level must maintain Ma'at at its station, requiring coordinated resource concentration. The principle itself does not generate extraction; it transfers legitimacy for the resource transfers that occur. Authority flows downward because the Pharaoh must maintain order; obligations flow upward because maintaining cosmic order requires resource coordination.
% ABSENT_VOICES: Conquered populations and slaves are structurally excluded from full party to the Ma'at covenant — they bear obligations but cannot be full interpreters or legitimate authorities. They would contest whether the principle genuinely constrains upper stations or merely justifies hierarchy. The divine_mandate_reading (sibling) competes directly by asserting that Ma'at flows from cosmic order through the Pharaoh alone, making distributed maintenance secondary to Pharaonic divinity. The reciprocity_reading emphasizes that Pharaoh must provide mutual benefits, shifting focus from distributed maintenance to bilateral obligation.
% DISAPPEARANCE_RATIONALE: If the distributed maintenance principle disappeared, the Egyptian state would lose its primary legitimacy narrative and would need to replace it with explicit coercion, divine mandate, or reciprocity-based authority. Different seats have different stakes: priesthood and officials depend on the principle for their authority; Pharaoh under the divine-mandate reading might prefer pure divinity or pure power; peasants would lose the moral recognition element but not gain freedom (their trapped exit options would persist). The principle's disappearance would require another legitimizing framework — not merely enforcement, but a new story about why the hierarchy is legitimate.
% FOUNDING_PROBLEM: Ancient Egypt faced a coordination challenge: how to maintain order across a vast, hierarchical realm dependent on the Nile's irregular flooding, without a standing police force, without written law codes at the early period, and without centralized bureaucratic machinery for every local dispute. The distributed maintenance principle offered an answer: all actors participate in maintaining the cosmic order through proper conduct in their station. Legitimacy flows from demonstrated function rather than from assertion of divine superiority alone or raw power.
% FOUNDING_PROBLEM_CORROBORATION: The priesthood and scribal class attest this is the foundational principle. Egyptian texts (Pyramid Texts, Coffin Texts, wisdom literature like the Instructions of Ptahhotep, Amenemope) repeatedly emphasize that Ma'at maintenance is distributed. Scholars studying Egyptian administration and jurisprudence (including Assmann's work on Egyptian religion, studies of the vizier's role, analysis of judicial practice) document that authority claims were grounded in demonstrated maintenance function. However, the divine_mandate_reading (attested in Pharaonic titulature and late theological texts) and the reciprocity_reading (emphasized in later wisdom texts and New Kingdom literature) offer competing solutions to the same founding problem. All three readings acknowledge the founding problem; they dispute which reading solves it most adequately.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, contested).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.31) because the principle does not concentrate extraction in a single seat — every station has maintenance duties and every station benefits from the order that collective maintenance produces. A Pharaoh who hoards resources without performing ritual fails Ma'at maintenance and loses legitimacy. Officials who exploit without maintaining order undermine themselves. Suppression is low (0.28) because the principle relies on distributed accountability and demonstrated function rather than on coercive enforcement machinery — failure is visible (drought, disorder, injustice) and delegitimizes the failing actor. Theater ratio is moderate (0.22): genuine maintenance work occurs (Pharaonic ritual, official adjudication), but as the interval progresses the ratio rises slightly (0.18→0.23 at midpoint), suggesting that in later periods or in practice under weak Pharaohs, more performative reassurance of Ma'at maintenance replaces actual function. The ratio then stabilizes (0.22 at end), indicating that even when theater rises, the principle's enforcement remains grounded in observable function, not pure performance. Accessibility_collapse (0.72) reflects that under this reading, the alternative to distributed maintenance is chaos and disorder — alternatives collapse nearly completely because the cosmic order's necessity is taken as axiomatic. Resistance (0.35) indicates moderate contestation: the priesthood and educated class support the principle, but Pharaohs facing resource scarcity and external conquest may resist its obligations; conquered populations and slaves offer structural resistance by failing to fully internalize the principle.
 *
 * PERSPECTIVAL GAP:
 *   The Pharaoh and priesthood seats should compute differently from the peasant and conquered-population seats. From the Pharaoh's institutional position, the distributed maintenance principle legitimizes his rule and distributes accountability across all stations, reducing the isolation of Pharaonic power — this should compute as net-beneficial rope. From the peasant's powerless position, the same principle offers moral recognition of their labor but does not alter their trapped exit options — it constrains how the Pharaoh can treat them (arbitrary cruelty is disorder), but it does not grant them voice in interpretation or authority. The principle is more beneficial to the upper stations (who gain legitimacy with distributed authority) than to the lower stations (who gain moral recognition but not exit options). The engine should compute higher directionality toward target for peasants and conquered populations (their exit remains trapped) despite the principle's claimed benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh, priesthood, and educated official class are beneficiaries of this reading: it legitimizes their authority as grounded in function rather than in pure coercion or assertion. Their exit options (identity_locked or constrained but still institutional) feed moderate d values biased toward beneficiary. Craftspeople and merchants have mobile exit options — they can leave a station for another trade — which moderates their extraction exposure. Peasants and conquered populations are trapped and cannot exit the hierarchical order; despite the principle's stated benefits, their d values should remain high (near target end) because suppression comes not from active enforcement but from structural barriers (no education, no economic independence). The principle constrains what the upper stations can do to them but does not change their structural position. Conquered populations are particularly high-d: they are incorporated into the order without consent and without representation in interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The distributed-maintenance reading resolves the mandatrophy risk by grounding authority in demonstrated function rather than in status alone. The founding problem (how to maintain order across a vast realm without centralized enforcement) remains live: officials must adjudicate disputes, Pharaoh must perform rituals, priesthood must interpret doctrine. The reading avoids the mandatrophy trap by making the founding functions continuously visible and contestable — a Pharaoh who fails at ritual or justice is obviously failing his maintenance duty. The divine-mandate reading forecloses this visibility: a Pharaoh is Horus incarnate and cannot fail by definition, which creates the mandatrophy risk. The reciprocity reading avoids mandatrophy differently: by making maintenance conditional on mutual obligation. This distributed-maintenance reading survives mandatrophy by keeping function central and observable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distribution_vs_narrative_cover,
    'Is the distributed-maintenance principle genuinely a constraint on behavior, or is it primarily a narrative cover story Pharaohs use to justify extraction while claiming maintenance function?',
    'Historical analysis of Pharaonic failure: when a Pharaoh visibly fails maintenance duties (allowing disorder, neglecting ritual, providing unjust judgment), does the principle constrain his power or does he merely perform ritual reassurance? Archaeological evidence of administrative collapse correlates with perceived Ma''at failure; textual record of Pharaohs who explicitly acknowledged failed maintenance and lost legitimacy.',
    'If genuine constraint: the principle is rope with low extraction and distributed accountability. If primarily narrative cover: the principle is closer to tangled_rope or piton — theater covering continued extraction despite claims of distributed function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distribution_vs_narrative_cover, empirical, 'Whether the principle constrains behavior or merely justifies hierarchy').

omega_variable(
    trapped_vs_distributed_benefit,
    'Do peasants and conquered populations genuinely benefit from the distributed-maintenance principle, or does the principle''s constraint on arbitrary cruelty mask structural extraction that persists regardless of whether the principle is invoked?',
    'Comparison of peasant welfare under rulers who publicly ground their rule in Ma''at maintenance versus rulers who claim divine mandate or pure coercion; examination of whether peasant obligations increase or decrease as priesthood and officials perform more maintenance; study of peasant resistance and exit attempts under different readings.',
    'If genuine benefit: the principle protects lower stations from worst-case extraction and the mechanism is rope. If structural extraction persists unchanged: the principle is piton — theater of distributed responsibility masking unchanged hierarchical extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trapped_vs_distributed_benefit, empirical, 'Whether distributed maintenance genuinely protects lower stations or masks unchanged extraction').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can the distributed-maintenance reading and the divine-mandate reading coexist in the same framework (one Pharaoh holds both; different sectors hold different readings), or does accepting one reading logically rule out the other?',
    'Textual analysis: do Egyptian sources blend the two readings, or are they kept separate? Historical periods where one reading was dominant versus periods of reading coexistence. Whether a single Pharaoh or administration explicitly adopted both readings or treated them as mutually exclusive.',
    'If coexist: the readings are different legitimate interpretations held by different parties; if foreclose: accepting one reading has structural implications that eliminate the other. Affects whether the sibling readings should be classified as coexists_with (current) or forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the three Ma''at readings logically coexist or foreclose each other').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is the suppression present in this constraint internalized (peasants have accepted the principle and self-limit in alignment with their station) versus structural (peasants have no exit options regardless of their beliefs)?',
    'Post-exit analysis: when peasants escaped the Egyptian system (via migration to neighboring territories, joining nomadic groups, or in later periods leaving Egypt entirely), did the suppression persist? Textual evidence of peasant resistance and how they articulate their relationship to Ma''at. Whether peasant uprisings invoke the principle to justify demands for shared maintenance or reject it as illegitimate narrative.',
    'If internalized: the suppression (0.28) should be understood as partially self-enforced and the constraint''s accessibility_collapse (0.72) reflects genuine axiomatic acceptance. If structural: the suppression masks barriers that persist independently and the principle''s constraint relies on inability to exit rather than shared commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression in this constraint is internalized or purely structural').

omega_variable(
    priesthood_authority_source,
    'Does priesthood authority derive from their demonstrated maintenance function (as the distributed-maintenance reading claims) or from inherent status and inherited office (closer to divine-mandate reading)?',
    'Historical analysis of how priests were selected, trained, and replaced; whether failure to perform rituals or maintain doctrine resulted in removal; whether access to priesthood was open to demonstrated capability or restricted to heredity; whether priests who failed maintenance duties were replaced or retained.',
    'If function-grounded: priesthood is a seat of authority within the rope structure with distributed accountability. If status-grounded: priesthood''s authority is closer to capture of the Ma''at principle and the priesthood becomes a separate seat extracting benefit from maintenance claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priesthood_authority_source, empirical, 'Whether priesthood authority is grounded in function or inherited status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(maat_tr_t0, observed).
narrative_ontology:measurement(maat_tr_t5, maat_order_principle__distributed_maintenance_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(maat_tr_t5, observed).
narrative_ontology:measurement(maat_tr_t10, maat_order_principle__distributed_maintenance_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(maat_tr_t10, observed).
narrative_ontology:measurement(maat_tr_t15, maat_order_principle__distributed_maintenance_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(maat_tr_t15, observed).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(maat_tr_t20, observed).
narrative_ontology:measurement(maat_tr_t25, maat_order_principle__distributed_maintenance_reading, theater_ratio, 25, 0.23).
narrative_ontology:measurement_basis(maat_tr_t25, observed).
narrative_ontology:measurement(maat_tr_t30, maat_order_principle__distributed_maintenance_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(maat_tr_t30, observed).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__distributed_maintenance_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(maat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(maat_be_t0, observed).
narrative_ontology:measurement(maat_be_t5, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement_basis(maat_be_t5, observed).
narrative_ontology:measurement(maat_be_t10, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(maat_be_t10, observed).
narrative_ontology:measurement(maat_be_t15, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement_basis(maat_be_t15, observed).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(maat_be_t20, observed).
narrative_ontology:measurement(maat_be_t25, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement_basis(maat_be_t25, observed).
narrative_ontology:measurement(maat_be_t30, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(maat_be_t30, observed).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(maat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(maat_su_t0, observed).
narrative_ontology:measurement(maat_su_t5, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement_basis(maat_su_t5, observed).
narrative_ontology:measurement(maat_su_t10, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement_basis(maat_su_t10, observed).
narrative_ontology:measurement(maat_su_t15, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 15, 0.27).
narrative_ontology:measurement_basis(maat_su_t15, observed).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(maat_su_t20, observed).
narrative_ontology:measurement(maat_su_t25, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 25, 0.29).
narrative_ontology:measurement_basis(maat_su_t25, observed).
narrative_ontology:measurement(maat_su_t30, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(maat_su_t30, observed).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(maat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__distributed_maintenance_reading, 0.12).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel maat_order_principle. The divine_mandate_reading and reciprocity_reading are sibling constraints from the same kernel. The distributed_maintenance_reading (this story) emphasizes distributed authority grounded in demonstrated function; it produces the lowest extraction profile and lowest suppression of the three readings. The divine_mandate_reading centralizes authority in the Pharaoh and should produce higher extraction and suppression. The reciprocity_reading emphasizes mutual obligation and should produce intermediate extraction with conditional beneficiary responsibilities. Each reading has a different ε value and should be authored as a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, powerless, 0.62).
constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
