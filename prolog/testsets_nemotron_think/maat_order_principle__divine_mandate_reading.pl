% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Divine Mandate Reading of Ma'at: Pharaoh as Cosmic Channel
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   The divine mandate reading of Ma'at positions the Pharaoh as the
 *   ontological source and channel of cosmic order (Ma'at) — he does not
 *   merely uphold Ma'at, he embodies it, and by definition cannot violate it.
 *   This reading instantiates a constraint where the ruler stands outside the
 *   normative system he generates. Extraction (taxation, corvée labor, temple
 *   endowments) is justified as the necessary flow of Ma'at from cosmos
 *   through Pharaoh to society. The priesthood maintains an active
 *   interpretive layer that suppresses rival readings (reciprocity,
 *   distributed maintenance) as heresy/chaos. The constraint shows rising
 *   extractiveness and theater over 3000 years as the coordination function
 *   (genuine early state formation) atrophies into ritualized extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.78).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.85).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Divine Mandate Reading of Ma'at: Pharaoh as Cosmic Channel").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '32dba884-fa89-4eed-acaa-0941e396d593').
narrative_ontology:cs_kernel_codification('32dba884-fa89-4eed-acaa-0941e396d593', formalized).
narrative_ontology:cs_authority_grounding('32dba884-fa89-4eed-acaa-0941e396d593', lineage).
narrative_ontology:cs_interpretation_layer_present('32dba884-fa89-4eed-acaa-0941e396d593').
narrative_ontology:cs_reading_relation('32dba884-fa89-4eed-acaa-0941e396d593', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('32dba884-fa89-4eed-acaa-0941e396d593', maat_order_principle__distributed_maintenance_reading, forecloses).
narrative_ontology:cs_axiom('32dba884-fa89-4eed-acaa-0941e396d593', foundational, pharaoh_embodies_maat).
narrative_ontology:cs_axiom_status(pharaoh_embodies_maat, holdable).
narrative_ontology:cs_axiom_grounding('32dba884-fa89-4eed-acaa-0941e396d593', pharaoh_embodies_maat, theological).
narrative_ontology:cs_axiom('32dba884-fa89-4eed-acaa-0941e396d593', foundational, maat_flows_through_pharaoh_only).
narrative_ontology:cs_axiom_status(maat_flows_through_pharaoh_only, holdable).
narrative_ontology:cs_axiom_grounding('32dba884-fa89-4eed-acaa-0941e396d593', maat_flows_through_pharaoh_only, theological).
narrative_ontology:cs_reference_frame('32dba884-fa89-4eed-acaa-0941e396d593', divine_pharaonic_order).
narrative_ontology:cs_drift_state('32dba884-fa89-4eed-acaa-0941e396d593', late_period_decline, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('32dba884-fa89-4eed-acaa-0941e396d593', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, royal_court).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, priesthood).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, common_people).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, peasantry).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, corvee_laborers).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, divine_kingship_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, cosmic_order_requires_pharaonic_mediation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Embodies Ma'at as living horus; the constraint flows through him as its source and channel. He sets the agenda for cosmic order maintenance, commands resources and labor for monumental projects justified as Ma'at-maintenance, and stands outside the constraint system — by definition he cannot violate Ma'at. His exit from the role is analytically impossible within the framework (the office and person are fused); historically, exit meant dynasty collapse.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaoh, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, pharaoh, beneficiary).

% High officials, viziers, and provincial governors who administer the extraction apparatus (taxation, corvée labor, temple endowments) in Ma'at's name. They collect status, land, and proximity to the divine channel. Exit means losing office and the cosmological legitimacy it confers; constrained by the same theological framework that elevates them.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, royal_court, beneficiary,
    powerful, generational, constrained, national).

% Temple hierarchies that ritualize and theologize the divine mandate. They control the interpretive layer (oracle, festival calendar, doctrinal enforcement) and receive massive land endowments and tax exemptions. Their professional and religious identity is fused with the reading — exit would mean abandoning the cosmological framework that constitutes their authority. They actively suppress rival readings (reciprocity, distributed) as heresy or chaos.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, priesthood, beneficiary,
    organized, generational, identity_locked, national).

% Peasant farmers and urban dwellers who bear the extraction (grain taxes, labor drafts, temple offerings) justified as their participation in Ma'at. They have no voice in the interpretation of Ma'at; the theological framework defines their duty as obedience. Geographic and economic exit is nearly impossible (Nile valley bounded by desert, state granary control). Resistance appears as flight, petition, or rare revolt — framed as isfet (chaos) by the system.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, common_people, payer,
    powerless, immediate, trapped, local).

% Rural cultivators whose surplus feeds the entire extraction chain. The inundation cycle makes their labor legible and extractable; the Ma'at narrative frames their contribution as cosmic duty rather than exploitation. Village-level enforcement (scribes, overseers) ensures compliance. No viable exit — the desert offers death, the city offers no alternative cosmology.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, peasantry, payer,
    powerless, biographical, trapped, regional).

% Conscripted labor for pyramids, temples, and state works — the most visible extraction. Framed as sacred service to Ma'at (building the machines that maintain cosmic order). Coercion is direct (overseers, quotas, punishment); the theological frame makes resistance not just illegal but cosmologically evil. Exit is physically prevented during service periods.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, corvee_laborers, payer,
    powerless, immediate, trapped, local).

% Neighboring states (Nubia, Libya, Levant, Hittites) whose own cosmic orders are structurally incompatible. They are excluded from the Ma'at framework entirely — treated as isfet (chaos) by definition. Their perspective (that Ma'at is a local imperial ideology) is structurally silenced; diplomatic correspondence shows they sometimes adopted Egyptian theological language strategically but never accepted the constraint.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, foreign_powers, excluded,
    powerful, biographical, mobile, global).

% Scholars who reconstruct the constraint from texts, archaeology, and comparative analysis. They see the full structural topology: the theological cover, the extraction mechanics, the suppression of alternatives. Their seat is outside the constraint's operation — they neither pay nor collect, but their classifications (divine mandate vs. reciprocity vs. distributed) shape modern reception.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, modern_egyptologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains cosmic and social order (Ma'at) against chaos (isfet) by channeling divine authority through a single sovereign node — the Pharaoh — who organizes labor, resource distribution, and ritual calendar to sustain the cosmos.
% TRANSFER_FUNCTION: Moves agricultural surplus, corvée labor, and ritual service from the peasantry and common people upward through the royal court and priesthood to the Pharaoh, justified as the necessary flow that sustains Ma'at. The Pharaoh redistributes a portion (temples, granaries, state projects) but the net flow is extractive.
% ABSENT_VOICES: The peasantry and corvée laborers who would object to the extraction level if they had a theological vocabulary for dissent — but the constraint defines their dissent as isfet (chaos). Also absent: the reciprocity reading's voice (Pharaoh as obligated partner) and distributed maintenance reading's voice (commoner as cosmic agent) — both suppressed as heretical by the priesthood.
% DISAPPEARANCE_RATIONALE: If the divine mandate reading vanished overnight, the theological legitimation of Pharaonic extraction would collapse. The priesthood would lose its interpretive monopoly, the royal court its cosmic warrant, and the extraction apparatus (taxation, corvée, temple endowments) would face immediate legitimacy crisis. The world would rearrange toward either a reciprocity-based kingship (Pharaoh as servant of Ma'at) or distributed maintenance (local temples and communities sustaining order) — or state collapse.
% FOUNDING_PROBLEM: Early Dynastic/Old Kingdom state formation required a unifying cosmology to legitimize centralized extraction (labor for monuments, grain for redistribution, loyalty for bureaucracy) across the Nile Valley. The divine mandate reading solved this by making the king the incarnation of cosmic order — extraction became participation in Ma'at, not exploitation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (state formation requiring cosmological legitimation) is attested by archaeologists (Kemp, Trigger, Wilkinson) from outside the beneficiary set — they document the material extraction apparatus and its theological packaging. The priesthood and royal court self-attest the problem as still live (cosmic order always threatened); modern scholars contest whether the original problem persists or the arrangement became self-serving extraction.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.78 reflects the massive surplus extraction (grain, labor, land) justified as cosmic duty. Suppression 0.85 reflects the theological, political, and coercive apparatus that silences alternative readings and enforces compliance — dissent is not just punished but defined as cosmological evil (isfet). Theater ratio 0.42 captures the growing gap between ritual performance (festivals, monumental building, royal titulary) and actual cosmic-order maintenance; by the Late Period the ritual theater substantially exceeds functional coordination. Accessibility collapse 0.72: the theological framework makes alternatives conceptually difficult (to question Pharaoh's Ma'at is to embrace chaos). Resistance 0.38: real but structurally constrained — flight, petition, rare revolt, all framed as isfet.
 *
 * PERSPECTIVAL GAP:
 *   From Pharaoh's seat (and the priesthood's), the constraint is genuine coordination — the only thing holding back isfet. From the payer seats, it is enforced extraction with a theological cover. The engine computes this divergence from the structural data: same constraint, different directionalities, different effective extraction. The claimed type (tangled_rope) acknowledges both the coordination function (early state formation, genuine cosmic-order maintenance) and the asymmetric extraction (Pharaoh outside the system).
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh is the structural beneficiary (d ≈ 0.05) — he collects the extraction, controls the rules, and stands outside the constraint. Royal court and priesthood are beneficiaries (d ≈ 0.15-0.25) — they collect status, resources, and interpretive authority, with constrained/identity-locked exit. Common people, peasantry, and corvée laborers are targets (d ≈ 0.85-0.95) — they bear the extraction with trapped exit. Foreign powers are excluded (not in the constraint system). Modern scholars are analytical observers (d = 0.5). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state formation requiring cosmological legitimation) was live in the Early Dynastic/Old Kingdom. By the New Kingdom, the extraction apparatus had substantially outgrown the coordination need — the priesthood's land holdings rivaled the crown's, corvée labor built monuments with diminishing redistributive return. The mandate atrophied but the constraint persisted through institutional inertia and theatrical maintenance (temple rituals, royal jubilees). The founding problem is now contested: scholars debate whether the cosmological framework still solves a coordination problem or merely legitimizes extraction. The constraint shows classic mandatrophy signals: rising theater, rising extractiveness, suppression hardening, no sunset clause.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_constructed_constraint,
    'Is the divine mandate reading a genuine theological truth about cosmic structure, or a constructed constraint that benefits identifiable agents (Pharaoh, priesthood, court)?',
    'Comparative analysis of Egyptian theological texts across periods: if the reading''s core claims (Pharaoh as Ma''at''s embodiment, inability to violate) are stable and uncontested internally, it functions as genuine cosmology; if they shift to accommodate extraction needs (e.g., new titles, expanded temple endowments, suppression of rival priesthoods), it functions as constructed constraint.',
    'If genuine theological truth, the constraint approaches mountain (natural law of cosmos). If constructed constraint with beneficiaries, it triggers false_summit_mountain detection (currently claimed as tangled_rope but with mountain-like inaccessibility). The omega documents the irreducible ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_constructed_constraint, conceptual, 'Whether the divine mandate reflects cosmic reality or elite construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.85) primarily structural (state coercion, priesthood enforcement, geographic boundedness) or internalized (theological belief that makes dissent unthinkable)?',
    'Post-exit suppression trajectory: examine periods of state collapse (First Intermediate Period, Late Period) — if commoners continued to frame their world in Ma''at/isfet terms and sought Pharaonic restoration, suppression was substantially internalized. If they immediately adopted alternative frameworks, suppression was primarily structural.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent after exit. This would increase χ for payer seats and strengthen snare/tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the divine mandate reading.').

omega_variable(
    coordination_extraction_boundary,
    'Is the cosmic-order-maintenance coordination function genuine and separable from the extraction, or is the coordination story pure cover for extraction?',
    'Counterfactual: if extraction were removed (no taxes, no corvée, no temple endowments), would the ritual/calendrical/coordinative functions of Ma''at persist? Evidence from temple archives (ritual calendars independent of state funding) vs. state records (monument building as primary Ma''at-act).',
    'If coordination is genuine and separable, the constraint is tangled_rope (coordination + extraction). If coordination is pure cover, it is snare (pure extraction). The current claimed_type (tangled_rope) assumes genuine but atrophied coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether cosmic order maintenance is real coordination or extraction cover.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the divine mandate reading logically foreclose the reciprocity and distributed_maintenance readings within a single theological framework, or do they coexist as complementary emphases?',
    'Textual analysis of Egyptian theological corpus: do any texts simultaneously affirm Pharaoh as Ma''at''s embodiment AND Pharaoh as obligated under Ma''at? The ''King as Sun Priest'' theology vs. ''King as Shepherd'' texts. If mutual affirmation exists, foreclosure is false; if texts treat them as rival claims, foreclosure holds.',
    'If foreclosure holds, the kernel has mutually exclusive readings — the engine''s cs_foreclosure detection will activate. If they coexist, the kernel has a pluralistic structure and the engine''s coexistence mapping applies. This determines the cs_structure.reading_relations values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between divine mandate and sibling readings of Ma''at.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_divine_mandate_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(maat_divine_mandate_tr_t5, maat_order_principle__divine_mandate_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(maat_divine_mandate_tr_t10, maat_order_principle__divine_mandate_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(maat_divine_mandate_tr_t15, maat_order_principle__divine_mandate_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(maat_divine_mandate_tr_t20, maat_order_principle__divine_mandate_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(maat_divine_mandate_tr_t25, maat_order_principle__divine_mandate_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(maat_divine_mandate_tr_t30, maat_order_principle__divine_mandate_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(maat_divine_mandate_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(maat_divine_mandate_be_t5, maat_order_principle__divine_mandate_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(maat_divine_mandate_be_t10, maat_order_principle__divine_mandate_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(maat_divine_mandate_be_t15, maat_order_principle__divine_mandate_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(maat_divine_mandate_be_t20, maat_order_principle__divine_mandate_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(maat_divine_mandate_be_t25, maat_order_principle__divine_mandate_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(maat_divine_mandate_be_t30, maat_order_principle__divine_mandate_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(maat_divine_mandate_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(maat_divine_mandate_su_t5, maat_order_principle__divine_mandate_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(maat_divine_mandate_su_t10, maat_order_principle__divine_mandate_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(maat_divine_mandate_su_t15, maat_order_principle__divine_mandate_reading, suppression_requirement, 15, 0.81).
narrative_ontology:measurement(maat_divine_mandate_su_t20, maat_order_principle__divine_mandate_reading, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(maat_divine_mandate_su_t25, maat_order_principle__divine_mandate_reading, suppression_requirement, 25, 0.84).
narrative_ontology:measurement(maat_divine_mandate_su_t30, maat_order_principle__divine_mandate_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__divine_mandate_reading, 0.1).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, pharaonic_state_extraction_apparatus).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, temple_economy_endowment_system).

% DUAL FORMULATION NOTE:
% The maat_order_principle kernel decomposes into three constraint stories: divine_mandate_reading (this file, tangled_rope, Pharaoh as source), reciprocity_reading (tangled_rope or snare, Pharaoh as obligated partner), distributed_maintenance_reading (rope or scaffold, all actors as maintainers). They differ in ε (0.78 vs. ~0.45 vs. ~0.25), beneficiary/victim structure, and suppression. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__divine_mandate_reading, institutional, 0.05).
constraint_indexing:directionality_override(maat_order_principle__divine_mandate_reading, organized, 0.2).
constraint_indexing:directionality_override(maat_order_principle__divine_mandate_reading, powerful, 0.25).
constraint_indexing:directionality_override(maat_order_principle__divine_mandate_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
