% ============================================================================
% CONSTRAINT STORY: assembly_supremacy__radical_self_rule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_assembly_supremacy__radical_self_rule_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: assembly_supremacy__radical_self_rule_reading
 *   human_readable: Assembly Supremacy: Radical Self-Rule Reading (Athenian Ekklesia)
 *   domain: legal/doctrinal/ancient_governance
 *
 * SUMMARY:
 *   The radical self-rule reading of assembly supremacy asserts that the
 *   Athenian ekklesia instantiated pure self-governance without remainder:
 *   the attending demos decided war, peace, food distribution, and matters of
 *   death directly, and no government existed apart from the governed
 *   assembled. This reading suppresses the ruler-ruled distinction entirely —
 *   the citizen attending assembly is the sovereign, and no authority figure
 *   stands above or outside the collective decision-making body. The reading
 *   is doctrinally powerful because it encodes the principle that legitimacy
 *   flows from democratic participation, not from institutional delegation.
 *   However, the reading is also a contested kernel: it conflicts with the
 *   mytilene_volatility_reading (which emphasizes how assembly speed and
 *   emotional volatility enabled collective reversal of massacre decisions,
 *   suggesting the assembly was not a stable sovereign but a dangerous mob),
 *   and it is historically complexified by the nomothetai_maturation_reading
 *   (which shows how fourth-century Athens added legislative filters —
 *   distinguishing laws from decrees and routing major decisions through
 *   nomothetic panels — precisely because the radical assembly had proven
 *   unstable). The constraint story for this reading treats it as a pure rope
 *   (coordination mechanism with genuine collective decision-making), while
 *   acknowledging that the reading masks extraction experienced by
 *   non-attending excluded groups and depends on an assumption about
 *   magistrate subordination that historical evidence complicates.
 *
 * KEY AGENTS:
 *   - Attending Male Citizen: Primary beneficiary (powerful/mobile) — holds sovereign decision-making power within the assembly; experiences no extraction because no ruler-ruled distinction exists for attendees.
 *   - Demos Assembled: Collective agent (institutional/arbitrage) — the body whose decision is binding; experiences constraint as pure coordination; can reconvene or dissolve.
 *   - Magistrates (Archons, Strategoi): Institutional actors (institutional/constrained) — in the radical reading, fully subordinate to assembly (execute decisions, initiate nothing); in practice, may hold gatekeeping power over agenda and policy options. This is the site of the radical_self_rule vs nomothetai_maturation dispute.
 *   - Non-Attending Excluded (Women, Slaves, Foreigners, Non-Landowners): Structural victims (powerless/trapped) — decisions affecting their survival made without their input; excluded from the 'demos' by law or material barriers; experience the constraint as pure extraction masked by the radical reading's narrative.
 *   - Analytical Observer: Logical perspective (analytical/analytical) — assesses whether the constraint is a logical necessity (mountain) or a contingent institutional arrangement (rope with myths masking reality).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(assembly_supremacy__radical_self_rule_reading, 0.18).
domain_priors:suppression_score(assembly_supremacy__radical_self_rule_reading, 0.08).
domain_priors:theater_ratio(assembly_supremacy__radical_self_rule_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(assembly_supremacy__radical_self_rule_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(assembly_supremacy__radical_self_rule_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(assembly_supremacy__radical_self_rule_reading, theater_ratio, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(assembly_supremacy__radical_self_rule_reading, rope).
narrative_ontology:human_readable(assembly_supremacy__radical_self_rule_reading, "Assembly Supremacy: Radical Self-Rule Reading (Athenian Ekklesia)").
narrative_ontology:topic_domain(assembly_supremacy__radical_self_rule_reading, "legal/doctrinal/ancient_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(assembly_supremacy__radical_self_rule_reading, 'd41efa7c-2052-4066-adeb-7edeb5287b14').
narrative_ontology:cs_kernel_codification('d41efa7c-2052-4066-adeb-7edeb5287b14', fixed_text).
narrative_ontology:cs_authority_grounding('d41efa7c-2052-4066-adeb-7edeb5287b14', lineage).
narrative_ontology:cs_interpretation_layer_present('d41efa7c-2052-4066-adeb-7edeb5287b14').
narrative_ontology:cs_reading_relation('d41efa7c-2052-4066-adeb-7edeb5287b14', assembly_supremacy__mytilene_volatility_reading, coexists_with).
narrative_ontology:cs_reading_relation('d41efa7c-2052-4066-adeb-7edeb5287b14', assembly_supremacy__nomothetai_maturation_reading, influences).
narrative_ontology:cs_axiom('d41efa7c-2052-4066-adeb-7edeb5287b14', foundational, no_government_apart_from_assembled_demos).
narrative_ontology:cs_axiom_status(no_government_apart_from_assembled_demos, holdable).
narrative_ontology:cs_axiom_grounding('d41efa7c-2052-4066-adeb-7edeb5287b14', no_government_apart_from_assembled_demos, deontological).
narrative_ontology:cs_axiom('d41efa7c-2052-4066-adeb-7edeb5287b14', foundational, suppression_of_ruler_ruled_distinction).
narrative_ontology:cs_axiom_status(suppression_of_ruler_ruled_distinction, holdable).
narrative_ontology:cs_axiom_grounding('d41efa7c-2052-4066-adeb-7edeb5287b14', suppression_of_ruler_ruled_distinction, deontological).
narrative_ontology:cs_reference_frame('d41efa7c-2052-4066-adeb-7edeb5287b14', demos_as_undelegated_sovereign).
narrative_ontology:cs_drift_state('d41efa7c-2052-4066-adeb-7edeb5287b14', nomothetai_fourth_century_reforms, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('d41efa7c-2052-4066-adeb-7edeb5287b14', '').
narrative_ontology:cs_kernel_id(assembly_supremacy__radical_self_rule_reading, assembly_supremacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(assembly_supremacy__radical_self_rule_reading, attending_male_citizen).
narrative_ontology:constraint_beneficiary(assembly_supremacy__radical_self_rule_reading, demos_assembled).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTENDING CITIZEN (ROPE) — The radical self-rule reading locates sovereignty entirely in the citizen present at assembly. No extraction occurs because no ruler-ruled distinction exists — the attendee IS the decision-maker. Low extractiveness (0.18) reflects the genuine coordination function: assembling to decide war, peace, food, and death requires mechanism and quorum, not suppression. The citizen sees the constraint as enabling their power.
constraint_indexing:constraint_classification(assembly_supremacy__radical_self_rule_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: DEMOS ASSEMBLED (ROPE) — The collective body of citizens experiences the ekklesia as pure coordination: the mechanism for translating individual will into binding decision. No victim set exists because the demos is both the decision-maker and the entity bound by its decisions. Arbitrage exit: the demos can choose to dissolve or reconvene. Theater is minimal (0.12) because the mechanism is transparent — votes are counted, decisions immediately binding, no elaborate procedure.
constraint_indexing:constraint_classification(assembly_supremacy__radical_self_rule_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a purely logical perspective, the radical self-rule reading asserts an irreducible structure: self-rule without remainder means no gap between decision-maker and governed. This is a logical theorem about the assembly form itself — if the attending demos makes all decisions, then by definition no external ruler exists. The classification as mountain reflects that this is a deductive necessity, not an empirical claim. Emerges naturally from the logical structure of the constraint.
constraint_indexing:constraint_classification(assembly_supremacy__radical_self_rule_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: NON-ATTENDING EXCLUDED (SNARE) — The radical self-rule reading suppresses the ruler-ruled distinction for attendees but creates it brutally for non-attendees. Women, slaves, resident foreigners, and citizens unable to attend bear full extraction: decisions that affect their survival (war, food, death) are made by others with no mechanism for their input. High suppression (0.08 is understated for this perspective) reflects absolute barriers to participation. Extractiveness from this perspective is masked by the radical reading's narrative (no government exists) even though for the excluded, government-by-the-room is as extractive as rule by a tyrant.
constraint_indexing:constraint_classification(assembly_supremacy__radical_self_rule_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(assembly_supremacy__radical_self_rule_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(assembly_supremacy__radical_self_rule_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(assembly_supremacy__radical_self_rule_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(assembly_supremacy__radical_self_rule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The radical self-rule reading asserts that no extraction occurs because no ruler-ruled distinction exists for attendees — they are the sovereign. Low extractiveness reflects the genuine coordination function: assembling to decide war, peace, food, and death requires a mechanism, but that mechanism is not extractive (it is enabling). However, 0.18 is understated if the reading masks extraction through exclusion (non-attendees, non-citizens) or through prior magistrate gatekeeping. The value assumes the reading's internal coherence. Suppression (0.08): Low. Within the narrow demos of attending male citizens, suppression is minimal — anyone with property status and citizenship can attend and vote. Barriers to attendance are low (location, time, quorum requirements are manageable). However, 0.08 is drastically underestimated when the constraint is evaluated from the perspective of excluded groups — the suppression of women, slaves, and foreigners from citizenship is total (0.95+), masked by the radical reading's silence about exclusion. Theater ratio (0.12): Minimal. The radical reading depicts decision-making as direct and transparent — no elaborate procedures, votes counted immediately, decisions binding without delay. Theater is low because there is no gap between decision and execution, no symbolic performance masking real power. This assumes that magistrates truly execute without discretion and that the assembly agenda is not controlled by prior institutional gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The radical_self_rule_reading produces dramatically different classifications depending on the observer's structural position. Attending citizens (powerful/mobile) see rope — pure coordination enabling their power. The demos assembled (institutional/arbitrage) sees rope — the collective body experiencing itself as sovereign. The analytical observer (civilizational/universal) sees mountain — a logical necessity that no government can exist above the sovereign people. But the non-attending excluded (powerless/trapped) sees snare — a system of total suppression masked by the radical reading's narrative that erases their existence from the definition of 'demos.' The perspectival gap reveals that the radical reading is internally coherent for the included group but inverts to pure extraction for the excluded. The radical reading does not describe 'self-rule without remainder' for the political community as a whole — it describes self-rule without remainder FOR THE ATTENDING MALE CITIZENS, with remainder suppression FOR EVERYONE ELSE.
 *
 * DIRECTIONALITY LOGIC:
 *   The attending citizen's directionality (d) is very low (approximately 0.15-0.25) because they are the beneficiary of the constraint and have mobile exit options (they can choose to attend or not attend assembly). The analytical observer's directionality is high (approximately 0.70-0.80) because they are assessing the structure logically without being embedded in it, and the analytical position has no exit option (you cannot escape the logic of the constraint you are analyzing). The non-attending excluded have maximum directionality (d ≈ 0.95) because they are the complete target of suppression with no exit option (trapped).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_remainder,
    'What counts as a ''government'' separate from the assembled demos? Does the magistracy (archons, strategoi) constitute remainder government, or are magistrates merely executors of assembly decisions?',
    'Historical analysis of magistrate discretion and assembly oversight. If magistrates can initiate policy independent of assembly instruction: remainder government exists (reading is contested). If magistrates execute only assembly decrees: no remainder (reading holds).',
    'If remainder government exists in practice: constraint classifies as tangled_rope (coordination + asymmetric authority structure), not rope. If magistrates are fully subordinate: rope classification holds. This is the core dispute between radical_self_rule_reading and nomothetai_maturation_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_remainder, empirical, 'Whether magistrate discretion constitutes a separate government structure').

omega_variable(
    non_attendance_extraction_masking,
    'Does the radical self-rule reading''s emphasis on assembly sovereignty mask the extraction experienced by excluded non-attendees (women, slaves, foreigners, non-property-owning citizens)?',
    'Comparative analysis: constraints experienced by included vs excluded groups. If the radical reading treats excluded groups as outside the ''demos'' entirely (not as citizens with no-exit), the reading''s universality claim collapses. If the reading treats excluded groups as citizens with constrained attendance due to material barriers, the extraction is massive and masked by narrative.',
    'If masking occurs: the radical_self_rule_reading is a false summit—it naturalizes radical asymmetry as logically necessary. The true constraint includes the extraction mechanism (suppression of women, slavery, dependent status). If no masking: the reading is internally coherent only within the narrow demos of attending male citizens, and does not describe ''self-rule'' for the political community as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_attendance_extraction_masking, conceptual, 'Whether the radical reading masks extraction experienced by excluded groups').

omega_variable(
    decision_capacity_of_assembly,
    'Can the assembled demos genuinely decide complex policy (food distribution, military strategy, treaty terms) in real time, or does the assembly''s decision-making capacity depend on a prior bureaucratic infrastructure (magistrate networks, advisory boards, pre-written proposals)?',
    'Examination of assembly voting procedures and topic complexity. If assembly debates and votes on pre-written proposals generated by magistrates: extraction point exists (magistrates have gatekeeping power over agenda). If assembly generates options and votes in situ: true real-time decision-making.',
    'If gatekeeping exists: the constraint is tangled_rope (coordination + extraction via agenda control), not rope. If real-time decision exists: rope classification holds but theater may be higher (0.12 is optimistic if procedures are elaborate or time-constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decision_capacity_of_assembly, empirical, 'Whether assembly decision-making is autonomous or dependent on prior magistrate gatekeeping').

omega_variable(
    myth_versus_practice_split,
    'Does the radical self-rule reading describe actual assembly practice in classical Athens (5th-4th century), or does it describe an idealized narrative that obscured real structural power (magistrate discretion, wealthy citizen influence, rhetor agenda-setting)?',
    'Comparison of radical_self_rule_reading''s claims against documented assembly proceedings, magistrate records, and empirical analysis of whose proposals passed. If systematic gaps exist between the reading and the record: the reading is a doctrinal commitment that shapes law and legitimacy, not an accurate description of practice.',
    'If significant gap exists: the radical_self_rule_reading is a commitment to a legal fiction that enabled egalitarian legitimacy claims while masking real structural power. Constraint type remains rope (the coordination function is real), but theater_ratio should rise significantly (0.12 is underestimated if the reading involves performative egalitarianism masking reality). The constraint becomes a piton (degraded institution maintained by narrative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(myth_versus_practice_split, empirical, 'Gap between radical self-rule reading and actual assembly practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(assembly_supremacy__radical_self_rule_reading, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(assembly_supremacy__radical_self_rule_reading, identity_coordination).
narrative_ontology:affects_constraint(assembly_supremacy__radical_self_rule_reading, assembly_supremacy__mytilene_volatility_reading).
narrative_ontology:affects_constraint(assembly_supremacy__radical_self_rule_reading, assembly_supremacy__nomothetai_maturation_reading).

% DUAL FORMULATION NOTE:
% The assembly_supremacy kernel decomposes into three structurally distinct constraint readings with different ε values and different empirical status. The radical_self_rule_reading (this file) treats assembly sovereignty as logically necessary and fully realized. The mytilene_volatility_reading treats assembly volatility as empirically problematic and doctrinally concerning. The nomothetai_maturation_reading treats the fourth-century addition of legislative procedure as an institutional response that revised the radical structure. All three readings share the same kernel (what does 'assembly supremacy' mean?) but disagree on what that meaning entails. The ε values differ: radical_self_rule is ε=0.18 (pure coordination for attendees); mytilene_volatility is higher (extraction of collective emotional power); nomothetai_maturation is higher still (extraction by the nomothetic panels that filtered assembly decisions). Each reading is a valid lens on the same doctrinal history.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
