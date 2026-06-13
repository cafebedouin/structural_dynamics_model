% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II as Composite Overdetermination
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint models the 'composite overdetermination' reading of
 *   Vatican II, which posits that the Council's documents contain
 *   irreconcilable theological tensions due to factional compromises, making
 *   a univocal interpretation (either 'continuity' or 'rupture') impossible.
 *   This reading is a Tangled Rope because it offers a genuine coordination
 *   function for academic understanding while extracting a cost from
 *   institutional authority and simplifying narratives. It requires active
 *   enforcement (intellectual rigor, historical analysis) to maintain against
 *   pressures for simpler interpretations.
 *
 * KEY AGENTS:
 *   - institutional_magisterium: Primary victim (institutional/identity_locked) — bears the cost of interpretive incoherence.
 *   - critical_theologians: Primary beneficiary (organized/mobile) — benefits from the validation of complex analysis.
 *   - ecclesial_historians: Secondary beneficiary (organized/mobile) — benefits from alignment with empirical historical methods.
 *   - traditionalist_factions: Victim (organized/identity_locked) — their rupture narrative is complicated.
 *   - progressive_factions: Victim (organized/identity_locked) — their continuity narrative is complicated.
 *   - lay_catholics: Excluded (powerless/constrained) — caught in the interpretive conflicts without clear guidance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.6).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II as Composite Overdetermination").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5').
narrative_ontology:cs_kernel_codification('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', fixed_text).
narrative_ontology:cs_authority_grounding('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', lineage).
narrative_ontology:cs_interpretation_layer_present('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5').
narrative_ontology:cs_reading_relation('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', foundational, doctrinal_incoherence_is_structural).
narrative_ontology:cs_axiom_status(doctrinal_incoherence_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', doctrinal_incoherence_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', foundational, factional_compromise_explains_ambiguity).
narrative_ontology:cs_axiom_status(factional_compromise_explains_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', factional_compromise_explains_ambiguity, empirically_contingent).
narrative_ontology:cs_reference_frame('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', idealized_univocal_conciliar_authority).
narrative_ontology:cs_drift_state('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', contemporary_theological_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bdf6ce6a-a0ca-44de-bb1f-7fbadf2d02c5', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, critical_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, ecclesial_historians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, traditionalist_factions).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, progressive_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The official teaching authority of the Catholic Church, which attempts to present Vatican II as a coherent, univocal event. This reading forces them to acknowledge internal contradictions, undermining their claim to seamless doctrinal development and creating internal conflict.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium, payer,
    institutional, generational, identity_locked, global).

% Scholars who benefit from this reading by having their work on the complexities and internal tensions of Vatican II validated. They gain intellectual credibility by demonstrating the irreducible ambiguities and the political compromises embedded in the conciliar texts.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, critical_theologians, beneficiary,
    organized, biographical, mobile, global).

% Historians who analyze the council's proceedings, drafts, and debates, finding evidence of conflicting theological agendas and political maneuvering. This reading aligns with their empirical findings, allowing for a more nuanced and accurate historical account.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, ecclesial_historians, beneficiary,
    organized, biographical, mobile, global).

% Groups that insist on a 'rupture' reading, viewing Vatican II as a betrayal of tradition. This reading, by highlighting internal contradictions rather than a clear break, complicates their narrative and forces them to confront the council's own internal logic, which they often reject outright.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_factions, payer,
    organized, generational, identity_locked, global).

% Groups that insist on a 'continuity' reading, viewing Vatican II as a legitimate and necessary evolution. This reading, by highlighting internal contradictions, challenges their narrative of seamless progress and forces them to acknowledge the compromises that limited radical change.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, progressive_factions, payer,
    organized, generational, identity_locked, global).

% Many ordinary believers are caught in the interpretive conflicts, experiencing confusion and division. This reading, while analytically robust, offers little pastoral clarity and can exacerbate their sense of disorientation, as it denies simple answers.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, lay_catholics, excluded,
    powerless, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the understanding of Vatican II among scholars by providing a framework that accounts for the observed internal inconsistencies and ongoing interpretive conflicts, allowing for more rigorous academic discourse.
% TRANSFER_FUNCTION: It transfers interpretive authority from institutional claims of univocal meaning to a more complex, historically and theologically nuanced understanding, from those seeking simple narratives to those embracing complexity.
% ABSENT_VOICES: Lay Catholics seeking clear, unified pastoral guidance are often excluded from this highly academic discourse. They would object to a reading that offers no simple resolution, as it leaves them without a clear path forward amidst doctrinal disputes.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the academic and theological landscape would revert to a simpler, binary 'continuity vs. rupture' debate, losing the nuance necessary to understand the actual historical and theological dynamics of the Council. The ongoing conflicts would be misdiagnosed as accidental rather than structural.
% FOUNDING_PROBLEM: The problem this reading addresses is the persistent failure of both 'continuity' and 'rupture' narratives to adequately explain the complex, often contradictory, outcomes and ongoing conflicts stemming from Vatican II.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing interpretive battles within the Catholic Church, the proliferation of scholarly works detailing internal conciliar tensions, and the lived experience of division among various ecclesial groups all corroborate that the problem of overdetermined ambiguity is live. This is attested by independent historians and sociologists of religion, not just the beneficiaries of this reading.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is moderate-high because this reading challenges the institutional authority's ability to maintain a coherent, unified narrative, forcing them to expend significant effort on managing internal dissent and interpretive ambiguity. Suppression (0.7) is high because institutional pressures often attempt to suppress complex readings in favor of simpler, more controllable narratives. The theater ratio (0.4) reflects the performative maintenance of a unified front by the Magisterium despite internal contradictions. The rising extractiveness and suppression over time reflect the increasing difficulty of maintaining a singular interpretation as historical and theological scholarship advances.
 *
 * PERSPECTIVAL GAP:
 *   The institutional Magisterium experiences this as a highly extractive constraint, as it undermines their authority and creates internal conflict. Critical theologians and historians, however, experience it as a beneficial framework that validates their intellectual work and provides a more accurate understanding of the Council. The divergence arises from the structural impact of acknowledging irreducible complexity on different roles within the ecclesial system.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional Magisterium is a target (high d) because this reading directly challenges their claim to univocal interpretive authority. Critical theologians and historians are beneficiaries (low d) as their work is validated and gains influence. Traditionalist and progressive factions are also targets (high d) because this reading complicates their preferred, simpler narratives of either rupture or continuity. Lay Catholics are excluded, bearing diffuse costs of confusion.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a simple 'Mountain' of inherent theological ambiguity or a 'Snare' of purely academic extraction. Instead, it highlights the active coordination function (for scholarly understanding) alongside the asymmetric extraction from institutional actors who prefer simpler, more controllable narratives. The 'contested' status of the founding problem further underscores that the constraint's persistence is not due to a universally acknowledged problem but to ongoing interpretive struggles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resolvability_of_ambiguity,
    'Are the theological ambiguities within Vatican II truly irreducible, or could a more sophisticated hermeneutic eventually reconcile them?',
    'Future theological developments or a new interpretive paradigm that successfully integrates the seemingly contradictory elements without suppressing any conciliar text.',
    'If resolvable, the constraint''s extractiveness from institutional authority would decrease, as a path to univocal interpretation would emerge. If irreducible, the constraint''s current classification as Tangled Rope (due to ongoing interpretive conflict) would be further solidified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resolvability_of_ambiguity, conceptual, 'Whether the internal contradictions of Vatican II are fundamentally irresolvable.').

omega_variable(
    institutional_acknowledgment_of_complexity,
    'To what extent does the institutional Magisterium genuinely acknowledge the internal theological tensions and ambiguities within Vatican II, rather than merely performing a unified front?',
    'Analysis of internal Magisterial documents, private correspondence, and shifts in official rhetoric over time, particularly in response to scholarly critiques.',
    'If acknowledgment is high, the ''suppression'' metric would decrease, and the ''theater_ratio'' would fall, indicating less performative maintenance of a false unity. If acknowledgment is low, the current high suppression and theater metrics are accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_acknowledgment_of_complexity, empirical, 'The degree of internal vs. external acknowledgment of Vatican II''s complexities by the Magisterium.').

omega_variable(
    framing_underdetermination_vatican_ii,
    'Is the ''composite overdetermination'' framing the only defensible way to understand Vatican II''s ambiguities, or could an alternative framing (e.g., ''dialectical tension'' or ''eschatological reserve'') offer a different classification?',
    'Development of a new, equally robust interpretive framework that accounts for the same empirical data (conciliar texts, historical context, post-conciliar conflicts) but leads to a different structural classification (e.g., a Rope of ongoing theological inquiry).',
    'An alternative framing could shift the perceived extractiveness and suppression, potentially reclassifying the constraint. If a ''dialectical tension'' framing were adopted, for instance, the constraint might appear less extractive and more like a Rope, as the tensions would be seen as productive rather than contradictory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination_vatican_ii, conceptual, 'Alternative framings of Vatican II''s ambiguities and their impact on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1980, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(vati_be_t1980, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(vati_su_t1980, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, post_conciliar_liturgical_reforms).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, ecumenical_dialogue_constraints).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Vatican II authority' kernel. It focuses on the internal contradictions and overdetermined nature of the Council, contrasting with the 'continuity' and 'rupture' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
