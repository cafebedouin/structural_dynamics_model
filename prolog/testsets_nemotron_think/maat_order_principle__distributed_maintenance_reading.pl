% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at as Distributed Maintenance Responsibility
 *   domain: ancient_history/religious_studies/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'distributed_maintenance_reading'
 *   of the Ma'at kernel: the claim that cosmic order is sustained by every
 *   actor performing the duties proper to their station, from Pharaoh's royal
 *   rituals to the peasant's field work. The reading presents this as genuine
 *   coordination (rope) with minimal extraction — authority flows from
 *   demonstrated maintenance competence, not inherent status, and multiple
 *   legitimate interpreters (priesthood, vizier, local elders) exist.
 *   Historically, the arrangement extracted substantial labor and surplus
 *   from commoners and conscripts while concentrating interpretive power in
 *   the priesthood, especially after the New Kingdom Amun priesthood's rise.
 *   The claimed_type (rope) and the authored metrics (moderate extraction,
 *   rising over time) diverge — the engine will compute per-seat
 *   classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.35).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.25).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at as Distributed Maintenance Responsibility").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/religious_studies/political_philosophy").

domain_priors:requires_active_enforcement(maat_order_principle__distributed_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, 'b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67').
narrative_ontology:cs_kernel_codification('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', fixed_text).
narrative_ontology:cs_authority_grounding('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', practice).
narrative_ontology:cs_interpretation_layer_present('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67').
narrative_ontology:cs_reading_relation('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', foundational, maat_maintenance_is_distributed).
narrative_ontology:cs_axiom_status(maat_maintenance_is_distributed, holdable).
narrative_ontology:cs_axiom_grounding('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', maat_maintenance_is_distributed, conventional).
narrative_ontology:cs_axiom('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', foundational, pharaoh_has_no_inherent_maat_authority).
narrative_ontology:cs_axiom_status(pharaoh_has_no_inherent_maat_authority, holdable).
narrative_ontology:cs_axiom_grounding('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', pharaoh_has_no_inherent_maat_authority, conventional).
narrative_ontology:cs_reference_frame('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', distributed_maintenance_framework).
narrative_ontology:cs_drift_state('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', new_kingdom_amun_priesthood_hegemony, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b5a8fefd-2ea4-48c2-ac7e-d69d4a292b67', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, state_officials).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, commoners).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, conscripted_laborers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, state_officials).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, cosmic_order_requires_human_action).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, station_based_ethics_sustain_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds supreme ritual responsibility for Ma'at; must perform correctly or cosmic order fails. Cannot exit the role — kingship fuses identity with Ma'at maintenance. Bears cost of massive temple building and ritual performance. In this reading, is first among equals, not the sole embodiment.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh, payer).

% Interpret Ma'at through ritual, text, and oracle; maintain temple estates. Receive offerings, land, tax exemption, and social prestige. Exit requires abandoning hereditary office and ritual purity — difficult but possible. Their authority derives from demonstrated ritual competence, not inherent status.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, priesthood, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, priesthood, beneficiary).

% Administer redistribution, justice, and labor corvées as Ma'at enactment. Gain status, income, and advancement through correct performance. Failures risk demotion or exile. Exit means leaving state service — possible but loses the position that enables Ma'at action.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, state_officials, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, state_officials, payer).

% Provide labor (corvée), grain taxes, and ritual participation. Their Ma'at is correct action in their station — farming, craft, family duty. No realistic exit: born into village, bound by kinship and state demand. Bear the material cost of the order they sustain.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, commoners, payer,
    powerless, immediate, trapped, local).

% Drafted for state monuments (pyramids, temples) as Ma'at labor. Receive rations but no choice. Death or injury common. Exit only through desertion (punishable) or death. Their bodies are the extraction surface for cosmic order's materialization.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, conscripted_laborers, payer,
    powerless, immediate, trapped, local).

% Operate outside Ma'at framework; their order is commercial contract, not cosmic balance. Would object to being forced into Egyptian ritual economy but are structurally excluded — Ma'at does not claim them. Their mobility lets them avoid the constraint entirely.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, foreign_traders, excluded,
    moderate, biographical, mobile, regional).

% Reconstruct Ma'at from texts, archaeology, and comparative anthropology. No stake in the constraint's operation; their analysis feeds back into contemporary readings of the kernel. Exit is trivial — they study it, they don't live it.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, modern_egyptologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains cosmic and social order in a hydraulic civilization by distributing ritual and ethical responsibility across every station — Pharaoh's correct rule, priest's correct rite, official's correct judgment, commoner's correct labor — so that the Nile's inundation, the harvest, and the state's continuity are secured through collective right action.
% TRANSFER_FUNCTION: Moves labor (corvée, farming surplus), material resources (grain, stone, gold), and ritual attention (offerings, festivals, temple service) from commoners and conscripted laborers upward through officials to priesthood and Pharaoh, who deploy them in the ritual-material complex that sustains Ma'at.
% ABSENT_VOICES: Women in non-royal stations (their Ma'at defined by male kin), enslaved persons (no station, only instrument), foreign residents in Egypt (metics, mercenaries — obligations ambiguous), and the dead (whose Ma'at depends on living descendants' offerings). These voices would contest the station assignments and the distribution of ritual burden but were not seated in the discourse.
% DISAPPEARANCE_RATIONALE: If Ma'at as distributed maintenance vanished overnight, the ritual calendar structuring agricultural labor would collapse, the legitimacy of redistribution would evaporate, temple estates would lose their economic base, and the Pharaoh's authority to command corvée would dissolve — the Old Kingdom state would reorganize into competing local powers or foreign conquest within a generation.
% FOUNDING_PROBLEM: How to coordinate a riverine civilization's survival across unpredictable inundation cycles, requiring centralized storage, mass labor mobilization, and a unifying cosmic narrative that makes centralized authority appear as natural law rather than conquest.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological and textual evidence (Wilkinson 1999, Kemp 2006, Assmann 1990) corroborates the hydraulic/order founding problem from outside the beneficiary set. The priesthood's own texts (Pyramid Texts, Coffin Texts) claim divine origin for Ma'at, not hydraulic necessity — the hydraulic reading is a modern analytical reconstruction, not an Egyptian self-understanding.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.35 at interval end) reflects the growing priestly/state share of surplus: Old Kingdom pyramid building was relatively low-extraction (labor as civic/religious duty); New Kingdom temple estates and Amun priesthood captured 15-20% of agricultural output. Suppression (0.25) is low because the constraint operated primarily through internalized cosmology — commoners believed their station was Ma'at — not coercion. Theater ratio (0.20) rises as rituals elaborate while material conditions for commoners stagnate. Accessibility collapse (0.40) is moderate: foreign cults (Isis, Serapis) and philosophical alternatives (Late Period skepticism) existed but were marginal. Resistance (0.30) appears in tomb robberies, labor strikes (Deir el-Medina), and heretical texts (Eloquent Peasant, Admonitions of Ipuwer).
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood seat, Ma'at is genuine coordination — they perform the rituals that (they believe) maintain the world. From the commoner seat, the same structure extracts labor and grain for temple estates they cannot access. From the Pharaoh seat, the constraint is both the source of legitimacy and an inescapable burden — a correct reading computes different types for each seat. The engine will capture this divergence; the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh is identity_locked — kingship fuses person with Ma'at duty; exit is death or deposition. Priesthood is constrained — hereditary office with ritual purity requirements; exit means losing the only structure that validates their authority. Officials are constrained — state service is the only path to Ma'at-action at scale. Commoners and conscripts are trapped — born into village corvée system, no alternative cosmology available. Foreign traders are mobile — they operate in commercial space outside Ma'at. Modern scholars are analytical. The directionality derivation from beneficiary/victim + exit places Pharaoh nearer target (d~0.6) despite agenda_setter role, because identity_lock makes the constraint inescapable and costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (hydraulic civilization coordination) is dead — the Nile's management no longer requires Pharaonic ritual. Yet the constraint persisted for 3000 years. The mandate atrophied into tradition: priesthood maintained rituals whose cosmic rationale no longer matched material reality, extracting surplus through inertia. The distributed_maintenance_reading claims the coordination function persists (station-based ethics), but the extraction vector (priestly capture) dominates the late period. This is not a snare (coordination is not pure cover) but a tangled_rope in late phase — the engine's per-seat computation will likely show tangled_rope for commoner seat, rope for priesthood seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state coercion, corvée enforcement) or internalized (commoners genuinely believe their station-duty is cosmic order)?',
    'Compare suppression metrics during state collapse periods (First/Second Intermediate Periods) vs. stable periods: if suppression drops when state capacity drops, it was structural; if commoners maintain Ma''at practices without state enforcement, internalization dominates.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent after exit. This would raise computed χ for trapped/identity_locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in Ma''at maintenance').

omega_variable(
    coordination_extraction_boundary,
    'Where does genuine coordination (ritual calendar synchronizing agriculture) end and priestly extraction (temple estates capturing surplus) begin?',
    'Quantify temple estate share of agricultural output vs. redistributive state share across periods; identify when ritual expenditure exceeds coordination necessity.',
    'If extraction > coordination after New Kingdom, the constraint shifts from rope toward tangled_rope for payer seats. The reading''s claim of ''lowest extraction'' would be falsified for late period.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Boundary between Ma''at''s coordination function and its extraction vector').

omega_variable(
    pharaoh_role_ambiguity,
    'In this reading, is Pharaoh a coordinator (first among equals) or a constrained actor (bearing highest cost, least exit)?',
    'Analyze Pharaoh''s resource control vs. ritual obligation across dynasties: if Pharaoh controls surplus but must deploy it for Ma''at rituals, the role is dual. Compare with divine_mandate_reading where Pharaoh is pure beneficiary.',
    'If Pharaoh is net payer, the constraint extracts from the apex — unusual for hierarchical systems. Would support this reading''s claim of distributed accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaoh_role_ambiguity, conceptual, 'Pharaoh''s structural position in distributed Ma''at maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(maat_tr_t6, maat_order_principle__distributed_maintenance_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(maat_tr_t12, maat_order_principle__distributed_maintenance_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(maat_tr_t18, maat_order_principle__distributed_maintenance_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(maat_tr_t24, maat_order_principle__distributed_maintenance_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(maat_tr_t30, maat_order_principle__distributed_maintenance_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(maat_be_t6, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement(maat_be_t12, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(maat_be_t18, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 18, 0.34).
narrative_ontology:measurement(maat_be_t24, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(maat_be_t30, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(maat_su_t6, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 6, 0.18).
narrative_ontology:measurement(maat_su_t12, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement(maat_su_t18, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 18, 0.26).
narrative_ontology:measurement(maat_su_t24, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 24, 0.3).
narrative_ontology:measurement(maat_su_t30, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__distributed_maintenance_reading, 0.08).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the Ma'at kernel into three readings with distinct ε values and authority structures. distributed_maintenance_reading claims lowest extraction (0.35) via distributed accountability; divine_mandate_reading claims higher extraction (est. 0.55) via Pharaonic embodiment; reciprocity_reading claims moderate extraction (est. 0.40) via bilateral obligation. All three share the fixed_text kernel (Pyramid Texts, Coffin Texts, Book of the Dead) but differ in authority_grounding (practice vs. lineage vs. conventional) and reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
