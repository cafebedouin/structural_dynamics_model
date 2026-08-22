% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)
 *   domain: medieval_political_economy/institutional_analysis/legal_history
 *
 * SUMMARY:
 *   This story models the feudal oath specifically through the reading that
 *   emphasizes ecclesiastical authority and Christian charity theology as
 *   limits on secular extraction. The Church claims interpretive supremacy
 *   over oaths: because the oath is sworn before God and structured by
 *   sacramental theology, only the Church can judge whether a lord's
 *   extraction violates charity and covenant. This reading instantiates a
 *   tangled_rope: the Church and vassals both benefit from the constraint on
 *   lordly power (it is genuine coordination—a shared theological framework
 *   for binding relationships), but the Church also benefits from its
 *   position as indispensable arbiter (asymmetric extraction of authority and
 *   legitimacy). Lords pay the cost of constrained authority. This reading
 *   contests two sibling readings: the lord_extraction_reading (where oaths
 *   authorize maximal extraction bounded only by service capacity) and the
 *   vassal_coordination_reading (where the oath is a fixed contract enforced
 *   by charter text, not theological interpretation).
 *
 * KEY AGENTS:
 *   - ecclesiastical_authority: Interprets oaths through charity theology; gains authority and legitimacy.
 *   - lords: Constrained by theological limits on extraction; lose unilateral power to interpret their own obligations.
 *   - vassals: Protected by charity doctrine; gain recourse to ecclesiastical arbitration but remain obligated.
 *   - ecclesiastical_hierarchy: Benefits from role as theological arbiter; collects donations and maintains centrality.
 *   - alternative_interpretive_authority: Excluded by the sacramental framing; secular courts lack standing to judge matters of conscience.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.51).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/institutional_analysis/legal_history").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '2323c80a-6d80-4d1a-a13a-e291a3237521').
narrative_ontology:cs_kernel_codification('2323c80a-6d80-4d1a-a13a-e291a3237521', distributed).
narrative_ontology:cs_authority_grounding('2323c80a-6d80-4d1a-a13a-e291a3237521', lineage).
narrative_ontology:cs_interpretation_layer_present('2323c80a-6d80-4d1a-a13a-e291a3237521').
narrative_ontology:cs_reading_relation('2323c80a-6d80-4d1a-a13a-e291a3237521', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2323c80a-6d80-4d1a-a13a-e291a3237521', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('2323c80a-6d80-4d1a-a13a-e291a3237521', foundational, sacramental_binding_of_oaths).
narrative_ontology:cs_axiom_status(sacramental_binding_of_oaths, holdable).
narrative_ontology:cs_axiom_grounding('2323c80a-6d80-4d1a-a13a-e291a3237521', sacramental_binding_of_oaths, theological).
narrative_ontology:cs_axiom('2323c80a-6d80-4d1a-a13a-e291a3237521', foundational, ecclesiastical_interpretive_supremacy_on_charity).
narrative_ontology:cs_axiom_status(ecclesiastical_interpretive_supremacy_on_charity, holdable).
narrative_ontology:cs_axiom_grounding('2323c80a-6d80-4d1a-a13a-e291a3237521', ecclesiastical_interpretive_supremacy_on_charity, deontological).
narrative_ontology:cs_reference_frame('2323c80a-6d80-4d1a-a13a-e291a3237521', oaths_bound_by_theological_charity).
narrative_ontology:cs_drift_state('2323c80a-6d80-4d1a-a13a-e291a3237521', late_medieval_charter_ascendancy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2323c80a-6d80-4d1a-a13a-e291a3237521', '2026-06-11T14:30:00Z').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals_via_charity_protection).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lords_constrained_by_theology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, christian_charity_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_binding_of_oaths).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_interpretive_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Church hierarchy adjudicates the interpretation of feudal oaths through the lens of Christian charity and sacramental obligation. Bishops and papal courts hear disputes, pronounce theological judgment on whether extraction violates charity, and threaten excommunication or spiritual sanction against lords deemed to violate the covenant. The Church gains interpretive authority over the feudal contract itself—not a neutral arbiter but a theological authority reframing what reciprocity means.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Desire maximal extraction from their vassals but find their authority constrained by the Church's claim that oaths are sacramental and binding under Christian theology. Lords cannot arbitrarily increase demands, renege on implicit protections, or treat vassals as chattel without risking theological condemnation and loss of ecclesiastical legitimacy. The Church's intervention caps extraction that lords would otherwise pursue.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lords, payer,
    powerful, generational, constrained, regional).

% Owe fixed service and surplus to their lords but gain a theological shield: the Church teaches that lords who extract beyond the bounds of charity violate a sacramental covenant and face spiritual consequences. Vassals can appeal to ecclesiastical authority to constrain excessive demands. They benefit from the Church's interpretive role, though they remain bound to the obligation itself and cannot exit the feudal relationship.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals, payer).

% Benefits from the position as theological arbiter of feudal relationships. Parishes and bishops gain authority, donations (tithes, bequests from grateful parties), and legitimacy by offering interpretive settlement of disputes. The role of ecclesiastical mediation makes the Church indispensable to feudal governance—neither lords nor vassals can resolve binding questions without reference to theological authority.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, beneficiary,
    institutional, generational, arbitrage, continental).

% Secular courts, royal authority, or lay nobles who might claim to interpret feudal obligations are structurally excluded from this reading: the Church's sacramental framing makes only ecclesiastical authority legitimate to judge the oath. Alternative secular arbiters are treated as incompetent to judge matters of conscience and covenant.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, alternative_interpretive_authority, excluded,
    powerful, generational, trapped, regional).

% Some religious communities and friars argue for maximal charity interpretations, pushing the Church's own role toward protection of the poorest vassals and most aggressive constraint on lordly extraction. Other clergy defend broader lordly prerogatives. This internal clerical debate shapes how the Church actually exercises its interpretive authority, but the assumption of ecclesiastical authority itself remains uncontested within this reading.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, dissenting_clergy, observer,
    moderate, generational, constrained, regional).

% The doctrinal corpus of Christian charity, sacramental theology, and covenant interpretation that grounds the Church's authority to mediate feudal oaths. Not itself an agent but the epistemic framework that legitimates ecclesiastical intervention.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theological_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theological_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__ecclesiastical_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an interpretive boundary on feudal extraction by binding the oath to Christian theology: lords and vassals both accept that their relationship is sacramental (not merely contractual or coercive) and that charity limits what lords may demand. The Church provides a third-party settlement mechanism for disputes about whether extraction violates that boundary.
% TRANSFER_FUNCTION: Moves surplus (labor, goods, military service) from vassals to lords, as in all feudal arrangements. The ecclesiastical mediation reading adds a constraint on the scale and nature of transfer: extraction that violates charity principles can be condemned and may be spiritually sanctioned. The Church also collects ecclesiastical income (tithes, donations, bequests) from gratitude or fear of spiritual judgment.
% ABSENT_VOICES: Secular courts and lay authorities who might claim to interpret feudal contracts are structurally excluded by the reading's framing. Heretical or heterodox interpretations of charity are also absent—only orthodox ecclesiastical theology counts. Vassals and lords who reject the sacramental framing have no seat at the table within this reading.
% DISAPPEARANCE_RATIONALE: If ecclesiastical authority over oath interpretation vanished, feudal relationships would reorganize around purely secular power (lords enforcing extraction by force alone) or written charters with lay courts as arbiters. The theological constraint would lift, extraction would accelerate, and the Church would lose its role as indispensable mediator. The entire institutional balance would shift.
% FOUNDING_PROBLEM: Feudal oaths, once sworn before God, created binding relationships, but the extent of lords' obligation to protect and limit extraction was ambiguous. Without a theological framework, lords could extract arbitrarily and claim breach only in clear violation of military service; vassals had no recourse except flight or rebellion. The Church stepped in to interpret the oath as inherently limited by Christian charity.
% FOUNDING_PROBLEM_CORROBORATION: Canon law records, papal bulls, and episcopal court decisions from the 11th–13th centuries (e.g., Gregory VII's interventions in secular disputes, conciliar rulings on clerical authority over lay oaths) corroborate that the Church actively claimed interpretive authority. Royal chronicles and monastic records document cases where ecclesiastical judgment constrained lords' extraction. The claim is corroborated by multiple independent medieval sources outside the Church's own official documents, though the Church's motivation to maintain the role is evident.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the Church's role constrains but does not eliminate lordly extraction—the measurement captures the actual extraction that occurs under the constraint, reduced from what lords would extract without theological limits but still substantial. Suppression is moderate (0.51) because the constraint relies on ecclesiastical authority and threat of spiritual sanction rather than pure coercion; the mechanism is interpretive and ideological rather than directly violent. Theater rises from 0.22 to 0.40 and plateaus: the Church must perform its role as arbiter constantly (holding courts, pronouncing judgment) and increasingly must justify why theological limits have not accelerated or why certain lords escape judgment—the performative dimension of maintaining authority grows as the constraint matures. The measurement series covers 250 years (roughly 11th–13th centuries in Western Europe), capturing the rise of ecclesiastical authority over feudal relationships, its apex around 120–170 years into the interval, and stabilization as the reading becomes institutional orthodoxy. Accessibility collapse is moderate (0.62): vassals and lords can imagine alternative arrangements (pure secular power, written charters, lay arbitration), but the theological framing makes these alternatives spiritually costly or delegitimized.
 *
 * PERSPECTIVAL GAP:
 *   From the Church's seat, this arrangement is genuine coordination: a shared theological framework that brings stability and binding force to relationships. The Church is not extracting but serving as the custodian of a sacred trust. From the lords' seat, the same arrangement appears as constraint: the Church has inserted itself into feudal relationships and stolen interpretive authority that should belong to secular lords. From the vassals' seat, the Church offers protection and a voice in disputes, a genuine benefit. The engine computes per-seat classifications from this asymmetry: the Church and vassals compute toward rope/coordination, lords compute toward snare/extraction (their power is constrained by an external actor gaining authority over their prerogatives). This reading's structural claim is tangled_rope because both coordination (the shared sacramental framework) and asymmetric extraction (the Church's capture of interpretive authority) are present and necessary to describe the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authority: d near 0.2 (beneficiary and agenda-setter; collects authority and legitimacy). Vassals: d near 0.35 (beneficiary of protection, but trapped and obligated; benefits from constraint on lords, so not a pure target). Lords: d near 0.75 (constrained by external authority; their power is the cost-bearer). The directionality of lords would be higher (closer to pure target) if they had viable exit, but they do not—the sacramental framing is continental and institutional, so lords face unified ecclesiastical authority everywhere. Alternative secular authorities are excluded or delegitimized, eliminating exit options. Lords are trapped in a system where their own authority is reinterpreted by an external actor.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap by anchoring the Church's role to a live problem: without ecclesiastical interpretation, feudal oaths have no theological content and lords can extract arbitrarily. The Church solved a genuine coordination problem (how to bind relationships when both parties invoke God as witness). However, the reading is vulnerable to later mandatrophy: as written charters and royal courts become dominant (later medieval development), the founding problem (ambiguity without theological guidance) diminishes, the Church's role becomes ritual and legitimation theater, and the constraint flips from tangled_rope to piton. The measurement series shows theater climbing through the interval, which is consistent with incipient mandatrophy—more performance, less functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecclesiastical_authority_actual_power,
    'How much actual power did ecclesiastical authority wield in practice? Were excommunication and spiritual sanction effective against powerful lords, or merely performative?',
    'Historical case studies: tracking which lords actually modified extraction after ecclesiastical condemnation, measuring the correlation between bishops'' judgments and lords'' behavior, examining instances where lords ignored or resisted Church authority.',
    'If ecclesiastical authority was nominal and lords regularly ignored it, the constraint collapses toward pure lordly extraction (snare) and the Church''s role becomes pure theater (piton). If ecclesiastical authority was genuinely constraining, the tangled_rope classification holds. If effectiveness was regionally or temporally variable, the classification may depend on specific geographic/temporal scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_authority_actual_power, empirical, 'Whether ecclesiastical condemnation actually modified lordly extraction behavior').

omega_variable(
    theological_constraint_vs_cover_story,
    'Was the Church''s appeal to Christian charity and sacramental theology a genuine constraint on extraction, or a cover story that ecclesiastical authority used to insert itself into feudal relationships, while extraction continued at lords'' discretion?',
    'Examine the temporal dynamics: (a) If extraction actually declined after ecclesiastical interventions, the theology was constraining; (b) If extraction continued at same rates while the Church gained authority and legitimacy, the theology was cover story. Also: compare regions where ecclesiastical authority was weak vs. strong; if extraction rates differ systematically, theology was operative.',
    'If theology was genuine constraint: tangled_rope (coordination + asymmetric authority gain). If theology was cover story: snare (extraction via fraudulent coordination claim). The reading itself asserts the theological constraint is real; this omega documents the ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_constraint_vs_cover_story, empirical, 'Whether the theological framework actually constrained extraction or served as legitimation for ecclesiastical power').

omega_variable(
    foreclusion_logic_vs_coexistence,
    'Does the ecclesiastical_mediation_reading logically foreclose the lord_extraction_reading, or do they coexist as competing institutional claims?',
    'Examine whether medieval actors treated the readings as mutually exclusive (one true, one false) or as simultaneously operative (different authorities claiming jurisdiction). If lords publicly accepted the Church''s right to interpret oaths theologically while privately extracting as they wished, the readings coexist rather than foreclose.',
    'If foreclusion is real: the relationship is stark (one reading must be false for the other to be true). If coexistence: the readings are different institutional framings held by different parties, making the constraint a site of permanent contest, not settlement. This affects how the classification changes if authority conditions shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclusion_logic_vs_coexistence, conceptual, 'Whether the ecclesiastical and lordly readings are logically exclusive or institutionally coexistent').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Was the suppression mechanism structural (vassals constrained by lords'' military power and control of resources) or internalized (vassals believed in the Church''s authority and charity theology, self-suppressing their own resistance)?',
    'Post-ecclesiastical-decline data: if suppression persists after Church authority collapses, it was internalized; if suppression dissipates, it was structural. Regional variation: where Church authority was weak from the start, compare suppression levels and vassal resistance to regions where authority was strong.',
    'If structural: the measured suppression is accurate to the constraint''s material operation. If internalized: the constraint''s effective suppression is higher than measured because the internalized belief persists even after institutional authority decays—the constraint is more ''sticky'' than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of vassal resistance was externally imposed by the Church or internalized as belief in charity theology').

omega_variable(
    reading_membership_ambiguity,
    'Which reading of the feudal_oath_reciprocity kernel is THIS constraint a member of? The kernel context names this as ecclesiastical_mediation_reading, but was that reading uniformly endorsed, or did different regions/periods read differently?',
    'Map when and where each reading dominated: ecclesiastical sources emphasize the charity reading; secular court records emphasize lord extraction; charter sources emphasize written reciprocity. If readings are geographically/temporally segmented, the constraint itself is not one unified thing but a superposition that should potentially be decomposed further.',
    'If readings are tightly segmented: the constraint should be decomposed into three stories (ecclesiastical region, secular region, charter region) with different ε and classification. If readings coexist everywhere: the constraint remains unified but the omegas document permanent underdetermination about which reading is ''true'' at any site.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_membership_ambiguity, conceptual, 'Whether this reading is uniformly applicable across medieval Europe or if regional/temporal variation suggests further decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(feud_tr_t40, observed).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement_basis(feud_tr_t80, observed).
narrative_ontology:measurement(feud_tr_t120, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 120, 0.39).
narrative_ontology:measurement_basis(feud_tr_t120, observed).
narrative_ontology:measurement(feud_tr_t170, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 170, 0.4).
narrative_ontology:measurement_basis(feud_tr_t170, observed).
narrative_ontology:measurement(feud_tr_t220, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 220, 0.38).
narrative_ontology:measurement_basis(feud_tr_t220, observed).
narrative_ontology:measurement(feud_tr_t250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 250, 0.38).
narrative_ontology:measurement_basis(feud_tr_t250, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(feud_be_t40, observed).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement_basis(feud_be_t80, observed).
narrative_ontology:measurement(feud_be_t120, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 120, 0.61).
narrative_ontology:measurement_basis(feud_be_t120, observed).
narrative_ontology:measurement(feud_be_t170, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 170, 0.6).
narrative_ontology:measurement_basis(feud_be_t170, observed).
narrative_ontology:measurement(feud_be_t220, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 220, 0.58).
narrative_ontology:measurement_basis(feud_be_t220, observed).
narrative_ontology:measurement(feud_be_t250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 250, 0.58).
narrative_ontology:measurement_basis(feud_be_t250, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(feud_su_t40, observed).
narrative_ontology:measurement(feud_su_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement_basis(feud_su_t80, observed).
narrative_ontology:measurement(feud_su_t120, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 120, 0.52).
narrative_ontology:measurement_basis(feud_su_t120, observed).
narrative_ontology:measurement(feud_su_t170, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 170, 0.51).
narrative_ontology:measurement_basis(feud_su_t170, observed).
narrative_ontology:measurement(feud_su_t220, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 220, 0.51).
narrative_ontology:measurement_basis(feud_su_t220, observed).
narrative_ontology:measurement(feud_su_t250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 250, 0.51).
narrative_ontology:measurement_basis(feud_su_t250, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feudal_oath_reciprocity kernel. The ecclesiastical_mediation_reading instantiates a tangled_rope where the Church gains interpretive authority and vassals gain theological protection, while lords lose unilateral extraction power. The sibling readings—lord_extraction_reading and vassal_coordination_reading—present competing institutional framings of the same oath relationship. All three are linked via network.affects_constraints to enable analysis of how different readings occupy the same historical space and compete for institutional dominance. The ε values differ substantially between readings because they assess different referents: the ecclesiastical reading assesses extraction constrained by theology; the lord reading assesses extraction bounded only by service capacity; the charter reading assesses fixed obligations specified in text. The kernel decomposition follows the ε-invariance principle: different observables (theological interpretation, lord power, written terms) yield different ε values, so they are different constraints, not one constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
