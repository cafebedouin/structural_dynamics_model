% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__metaphysical_equality_reading, []).

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
 *   constraint_id: homoousios_nicene__metaphysical_equality_reading
 *   human_readable: Homoousios as Full Ontological Equality (Nicene-Constantinopolitan Reading)
 *   domain: historical theology / ecclesiastical history / philosophy of religion
 *
 * SUMMARY:
 *   This constraint is the metaphysical equality reading of the contested
 *   homoousios kernel from the Council of Nicaea (325 CE), as consolidated at
 *   Constantinople (381 CE) and reinforced through Chalcedon (451 CE). On
 *   this reading, homoousios asserts that the Son shares numerically the same
 *   divine essence as the Father, is co-eternal, and admits no subordination
 *   in being — full ontological equality within the Godhead. This is ONE of
 *   three structurally distinct readings of the same kernel term: the
 *   subordinationist_reading holds homoousios compatible with the Son
 *   deriving being from the Father, and the honorific_similarity_reading
 *   holds the term signifies likeness rather than strict identity. Each
 *   reading is authored as its own constraint with its own epsilon,
 *   beneficiary/victim structure, and classification; they are not
 *   measurement variants of one constraint. This file does not adjudicate
 *   between them — it only characterizes the equality reading's own
 *   structural operation, which distributed interpretive authority to the
 *   episcopal hierarchy, distributed the coordination benefit of a unified
 *   confession to trinitarian communities, and imposed anathematization costs
 *   on the theological alternatives it displaced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.62).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.81).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Homoousios as Full Ontological Equality (Nicene-Constantinopolitan Reading)").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical theology / ecclesiastical history / philosophy of religion").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, '078ee75b-e884-42eb-a67e-a6aa49f4ba0d').
narrative_ontology:cs_kernel_codification('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', fixed_text).
narrative_ontology:cs_authority_grounding('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', lineage).
narrative_ontology:cs_interpretation_layer_present('078ee75b-e884-42eb-a67e-a6aa49f4ba0d').
narrative_ontology:cs_reading_relation('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', homoousios_nicene__honorific_similarity_reading, influences).
narrative_ontology:cs_axiom('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', foundational, son_numerically_identical_essence_to_father).
narrative_ontology:cs_axiom_status(son_numerically_identical_essence_to_father, holdable).
narrative_ontology:cs_axiom_grounding('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', son_numerically_identical_essence_to_father, theological).
narrative_ontology:cs_axiom('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', foundational, no_ontological_subordination_within_godhead).
narrative_ontology:cs_axiom_status(no_ontological_subordination_within_godhead, holdable).
narrative_ontology:cs_axiom_grounding('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', no_ontological_subordination_within_godhead, theological).
narrative_ontology:cs_reference_frame('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', nicene_conciliar_settlement).
narrative_ontology:cs_drift_state('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', post_chalcedonian_consolidation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('078ee75b-e884-42eb-a67e-a6aa49f4ba0d', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_church_authority).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, trinitarian_orthodox_laity).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, arian_clergy).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, subordinationist_congregations).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, homoiousian_moderates).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, post_nicene_dissenting_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops who championed and enforce the homoousios formula as the boundary of orthodox confession, controlling ordination, communion, and creedal subscription. They administer the anathema clauses attached to the Nicene formula and hold the interpretive authority to determine who counts as within the fold. Their institutional standing, imperial backing, and control of church offices are secured precisely by the formula's success as the settled boundary.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% The imperial state apparatus (from Constantine onward) that convened the council and enforces its decisions through civil law, exile, and property confiscation against dissenting clergy. Benefits from a unified doctrinal formula that stabilizes religious conflict across the empire and consolidates political authority behind a single creedal standard.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_church_authority, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, imperial_church_authority, agenda_setter).

% Ordinary believers whose worship, sacramental access, and communal identity are organized around the equality formula. They receive doctrinal stability, a settled liturgical and catechetical framework, and communion with the dominant church structure — but their access to alternative christological framings is foreclosed by the same boundary that gives them this stability.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, trinitarian_orthodox_laity, beneficiary,
    moderate, generational, constrained, continental).

% Clergy holding that the Son is a created being subordinate to the Father in essence, not merely function. Anathematized at Nicaea, deposed from sees, exiled by imperial decree (e.g. Arius himself, later Eunomians), and excluded from communion. Their theological position becomes a crime against imperial religious law rather than a live doctrinal option.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, arian_clergy, payer,
    moderate, biographical, trapped, continental).

% Local communities whose received teaching held the Son to derive being from the Father in a graded hierarchy. After the equality formula becomes imperial orthodoxy, their worship practices are criminalized, their church buildings reassigned, and their communal continuity broken across generations.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, subordinationist_congregations, payer,
    powerless, generational, trapped, regional).

% Bishops and theologians (the 'similar-substance' party) who sought a middle position preserving likeness without asserting numerical identity of essence. Squeezed by the equality reading's success: forced either to subscribe to the stricter homoousios formula against their preferred wording or be treated as crypto-Arian and marginalized from councils.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, homoiousian_moderates, payer,
    moderate, biographical, constrained, continental).

% Bishops who continued to contest the equality reading in the decades after 325 CE, during the long doctrinal struggle before Constantinople 381 CE settled it. Faced repeated deposition, exile, and reversal of fortune as imperial favor shifted between pro-Nicene and Homoian emperors — their theological position's political precarity is itself a cost the formula's ultimate victors did not bear equally.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, post_nicene_dissenting_bishops, payer,
    moderate, biographical, constrained, continental).

% Contemporary scholars and interfaith dialogue participants who examine the historical formation of the equality reading, its political entanglement with imperial power, and its exclusionary consequences for the theological alternatives it suppressed, without being bound by ecclesiastical sanction to hold any particular reading.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, modern_ecumenical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, shared confessional boundary that lets a fractured, empire-spanning church body identify co-religionists, coordinate sacramental communion, and present a unified doctrinal front to the imperial state and to rival religious movements — solving a real problem of doctrinal fragmentation threatening ecclesial and political unity in the fourth century.
% TRANSFER_FUNCTION: Moves interpretive authority over the nature of Christ from local congregations and diverse regional theological traditions to a centralized conciliar-episcopal hierarchy backed by imperial coercive power; moves standing, office, and communion access away from subordinationist and moderate clergy toward bishops who subscribe to the equality formula.
% ABSENT_VOICES: Arian, subordinationist, and homoiousian clergy who were present at or affected by the councils but were outvoted, exiled, or excluded from subsequent gatherings had no durable voice in the settlement once imperial enforcement began; their theological arguments survive mostly through hostile citation in the orthodox polemical record that opposed them.
% DISAPPEARANCE_RATIONALE: If the equality reading's enforced status vanished, the boundary between orthodox and heterodox christology would reopen: subordinationist and homoiousian positions would re-enter as live liturgical and confessional options, ordination and communion practices tied to strict Nicene subscription would lose their exclusive claim, and the historical unification of doctrine with imperial political authority that stabilized the fourth-century church would need a different foundation.
% FOUNDING_PROBLEM: The church faced a genuine crisis of doctrinal fragmentation over the nature of Christ's relationship to God the Father, threatening both theological coherence and imperial political unity; a shared formula was needed to determine who could commune together and speak for the church with one voice.
% FOUNDING_PROBLEM_CORROBORATION: Pro-Nicene historians (Athanasius, later ecclesiastical historiography written from within the victorious tradition) attest the problem was doctrinal error requiring correction. Independent historians of late antiquity (outside the confessional tradition) corroborate that a real coordination problem existed, but many also document that the specific equality formula's victory was substantially contingent on imperial political favor and shifting court alliances rather than settled by theological argument alone — a reading corroborated by the repeated imperial reversals between Nicene and Homoian factions across the fourth century.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__metaphysical_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__metaphysical_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62 by 451 CE, rising from 0.38 at Nicaea) reflects that the formula's coordination benefit — a shared confessional identity across a fragmented empire-wide church — is real but increasingly bundled with concentrated institutional and political power accruing to the pro-Nicene hierarchy and the imperial state. Suppression is high (0.81) and rises sharply between 325 and 381 as imperial enforcement machinery (exile, deposition, property confiscation) hardens around the formula; it is a raw structural property, not scaled by scope or power, but the sheer coercive apparatus deployed against dissenting clergy is substantial and well-documented. Theater ratio is moderate-low (0.28): most of the enforcement is functionally directed at maintaining doctrinal boundary rather than performative, though ongoing conciliar restatements (381, 451) show increasing theatrical reaffirmation of a settlement whose live theological contest had mostly already been resolved by political attrition rather than argument. Accessibility collapse is high (0.72): once the formula becomes imperial law, alternative christologies become nearly impossible to practice openly. Resistance is high (0.7): subordinationist and homoiousian parties contested the formula vigorously for decades before losing political ground.
 *
 * PERSPECTIVAL GAP:
 *   From the pro-Nicene episcopal seat, homoousios-as-equality is simply Christian truth recovered and defended against error — a mountain, not a construction. From the seat of an anathematized Arian bishop stripped of his see, the same formula is an instrument of political-theological extraction backed by imperial coercion. The engine computes these divergent seat classifications from the structural beneficiary/victim/enforcement data; the claimed_type here (tangled_rope) reflects the analytical observer's synthesis, not either side's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   The episcopal hierarchy and imperial authority are structural beneficiaries and agenda-setters: they collect the doctrinal settlement's political and institutional stability and administer its enforcement. Trinitarian orthodox laity are secondary beneficiaries — they receive coordination benefit (settled worship, communion access) without bearing the formula's coercive costs directly. Arian clergy, subordinationist congregations, homoiousian moderates, and post-Nicene dissenting bishops are targets: their theological positions are criminalized, their offices are stripped, and their exit options are trapped or constrained by the same imperial-ecclesial apparatus that enforces the formula. The formula's coordination function is genuine (a real doctrinal fragmentation problem existed) but its persistence depends on continued suppression of the alternatives, which is why this reading computes as tangled_rope rather than pure rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — doctrinal fragmentation threatening church unity — was genuinely live at Nicaea in 325 CE. By 451 CE, the practical need for a single doctrinal formula to hold the empire's church together had been substantially achieved through generations of enforcement rather than through the persistence of live theological contest; the formula's continued anathema clauses increasingly functioned to police memory of a settled dispute rather than resolve an ongoing one, which is part of why the founding_problem_status is authored as contested rather than simply live: the coordination need did not vanish, but the balance between coordination-preservation and extraction-of-continued-loyalty shifted over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_political_settlement,
    'Is the equality reading''s dominance the recovery of a theological truth that was always latent in apostolic teaching, or a politically contingent settlement that could have gone differently (e.g., toward the subordinationist or homoiousian reading) under different imperial alignments?',
    'Historical-critical analysis of the fourth-century political record (imperial correspondence, conciliar attendance and voting patterns, exile and reinstatement timelines) weighed against internal theological argument quality; comparison with counterfactual trajectories where Homoian emperors'' religious policy had prevailed durably rather than being reversed under Theodosius I.',
    'If the settlement is substantially politically contingent, the equality reading''s status as a mountain (natural theological truth) is a false summit — the formula would classify closer to tangled_rope or even snare from the losing parties'' structural position, which is the classification this story already leans toward. If theologically necessitated independent of imperial politics, the extraction reading weakens substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_political_settlement, conceptual, 'Whether Nicene equality orthodoxy is discovered truth or contingent political-theological settlement.').

omega_variable(
    kernel_reading_indeterminacy,
    'Does the Greek term homoousios as used at Nicaea (325 CE) itself underdetermine between the equality, subordinationist, and honorific-similarity readings, such that the term''s semantic content was genuinely contested and only settled by post-hoc conciliar and political consolidation (esp. Constantinople 381, Cappadocian theological work) rather than being clear from the outset?',
    'Philological and historical-theological analysis of homoousios''s usage in pre-Nicene sources (including its earlier condemnation at the Synod of Antioch 268 for Paul of Samosata''s usage) and comparison with how quickly and by whom the equality reading was asserted as the term''s obvious meaning versus argued for over decades.',
    'If the term was genuinely underdetermined at Nicaea and the equality reading was a later interpretive achievement (chiefly Cappadocian and post-381), this reading''s claim to represent the ''original'' Nicene intent is weaker, and its high accessibility_collapse and suppression figures more clearly reflect retrospective consolidation of power rather than execution of a clear original mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, empirical, 'Whether homoousios had determinate content at Nicaea or was interpretively settled toward equality only later.').

omega_variable(
    conciliar_authority_framing_ambiguity,
    'Should the kernel be framed as the conciliar TEXT (the Nicene-Constantinopolitan Creed''s wording) or as the institutional AUTHORITY CLAIM layered above it (that ecumenical councils, once ratified, possess binding doctrinal authority over all local churches)? These are two coherent framings: reading the kernel as fixed_text foregrounds interpretive drift in later commentary; reading it as the authority claim foregrounds the extraction of interpretive power itself as the primary structural fact.',
    'Compare classification outcomes under each framing: does treating the creed as fixed_text (with interpretation_layer_present via episcopal lineage) versus treating conciliar authority itself as the kernel change which party is positioned as agenda_setter versus payer?',
    'Under the fixed_text framing (adopted here), the episcopal hierarchy is an interpreter of a stabilized text, and drift enters through later theological elaboration (Cappadocian settlement, Chalcedonian definition). Under an authority-claim framing, the very legitimacy of ecumenical conciliar binding force would be the kernel, and the equality/subordinationist/honorific readings would be downstream disputes about a still-more-fundamental commitment to conciliar authority as such — a different, higher-level constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conciliar_authority_framing_ambiguity, conceptual, 'Alternative kernel framings: creedal text versus the authority claim of ecumenical conciliar binding force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 350, 0.16).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 381, 0.2).
narrative_ontology:measurement(homo_tr_t400, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 400, 0.24).
narrative_ontology:measurement(homo_tr_t431, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 431, 0.26).
narrative_ontology:measurement(homo_tr_t451, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 451, 0.28).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 325, 0.38).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 350, 0.48).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 381, 0.58).
narrative_ontology:measurement(homo_be_t400, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement(homo_be_t431, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 431, 0.61).
narrative_ontology:measurement(homo_be_t451, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 451, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 325, 0.55).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 350, 0.68).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 381, 0.79).
narrative_ontology:measurement(homo_su_t400, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 400, 0.8).
narrative_ontology:measurement(homo_su_t431, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 431, 0.8).
narrative_ontology:measurement(homo_su_t451, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 451, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'homoousios' (per the epsilon-invariance principle): metaphysical_equality_reading (this file), subordinationist_reading, and honorific_similarity_reading. Each reading has a distinct beneficiary/victim structure and a distinct claimed classification because the readings pick out structurally different constraints, not different measurements of the same one. The equality reading is authored as the eventual imperially-consolidated winner (post-381), which is why its enforcement and suppression trajectories intensify over the interval while the sibling readings' stories would show contraction of their own institutional standing over the same period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
