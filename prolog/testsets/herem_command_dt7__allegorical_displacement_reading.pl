% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem as Allegorical Interior Moral Discipline (Displacement Reading)
 *   domain: religious/hermeneutical
 *
 * SUMMARY:
 *   This is ONE READING of the contested herem command (Deuteronomy 7, Joshua
 *   6—11 and related conquest narratives). The allegorical displacement
 *   reading interprets 'nations,' 'conquest,' and 'destruction' as
 *   typological placeholders for internal spiritual enemies — sin,
 *   temptation, idolatry, vice — rather than external ethnic or territorial
 *   targets. 'Herem' (the divinely mandated destruction) becomes a metaphor
 *   for rigorous interior moral discipline and resistance to spiritual
 *   corruption. This reading relocates the constraint entirely from the
 *   domain of interethnic relations to internal psychology and spiritual
 *   practice. The victim set collapses from concrete peoples to abstract
 *   vices. Extractiveness drops to zero because there is no material
 *   extraction from the reframing — the constraint coordinates meaning-making
 *   around scriptural texts without leveraging power over any real-world
 *   group. The constraint is hermeneutical, not institutional or material.
 *
 * KEY AGENTS:
 *   - spiritual_practitioners: individuals performing interior moral struggle; benefit from a framework that makes the violent scriptural narrative spiritually applicable rather than morally horrifying.
 *   - theological_interpreters: institutional agenda-setters (clergy, scholars) who advance and transmit the allegorical displacement reading; their authority derives from scriptural coherence and institutional position.
 *   - literal_historical_interpreters: EXCLUDED from this reading's frame; they would contest the removal of ethnic reference and argue herem was a real historical command.
 *   - indigenous_peoples_and_descendants: OBSERVER seat; positioned as the 'nations' in literal-historical readings but removed from victim set by allegorical displacement, creating epistemic ambiguity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.0).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.15).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem as Allegorical Interior Moral Discipline (Displacement Reading)").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "religious/hermeneutical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'e5177e57-b845-4412-8849-4375569f3bef').
narrative_ontology:cs_kernel_codification('e5177e57-b845-4412-8849-4375569f3bef', fixed_text).
narrative_ontology:cs_authority_grounding('e5177e57-b845-4412-8849-4375569f3bef', lineage).
narrative_ontology:cs_interpretation_layer_present('e5177e57-b845-4412-8849-4375569f3bef').
narrative_ontology:cs_reading_relation('e5177e57-b845-4412-8849-4375569f3bef', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5177e57-b845-4412-8849-4375569f3bef', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_axiom('e5177e57-b845-4412-8849-4375569f3bef', foundational, herem_nations_are_spiritual_enemies).
narrative_ontology:cs_axiom_status(herem_nations_are_spiritual_enemies, holdable).
narrative_ontology:cs_axiom_grounding('e5177e57-b845-4412-8849-4375569f3bef', herem_nations_are_spiritual_enemies, deontological).
narrative_ontology:cs_axiom('e5177e57-b845-4412-8849-4375569f3bef', foundational, universal_moral_law_applies_to_all_nations).
narrative_ontology:cs_axiom_status(universal_moral_law_applies_to_all_nations, holdable).
narrative_ontology:cs_axiom_grounding('e5177e57-b845-4412-8849-4375569f3bef', universal_moral_law_applies_to_all_nations, deontological).
narrative_ontology:cs_reference_frame('e5177e57-b845-4412-8849-4375569f3bef', allegorical_hermeneutical_framework).
narrative_ontology:cs_drift_state('e5177e57-b845-4412-8849-4375569f3bef', contemporary_moral_philosophy_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e5177e57-b845-4412-8849-4375569f3bef', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, spiritual_practitioners).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, universal_moral_law).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, spiritual_internalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals engaged in internal moral struggle interpret herem as a typological framework for resisting temptation and sin. The 'conquest' narrative provides psychological structure for understanding self-discipline and spiritual warfare. They benefit from a coherent hermeneutical framework that reframes the scriptural violence as applicable to their own interior struggle rather than licensing actual conquest.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, spiritual_practitioners, beneficiary,
    moderate, biographical, mobile, universal).

% Scholars, clergy, and exegetes who advance the allegorical displacement reading in published commentary, sermons, and teaching. They establish and maintain this interpretive framework through hermeneutical argument and institutional transmission. Their authority rests on coherence with broader theological principles (universalism, moral law) and interpretive tradition.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, theological_interpreters, agenda_setter,
    institutional, generational, mobile, universal).

% Scholars and theologians who read herem as a historically bounded but real divine command for the ancient Israelite settlement period. They are not in the conversation constituted by the allegorical displacement reading; if they were, they would contest the removal of ethnic and territorial reference and the relocation of 'nations' from concrete peoples to abstract vices.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, literal_historical_interpreters, excluded,
    institutional, generational, mobile, universal).

% Communities historically positioned as the 'nations' to be conquered in literal-historical readings of herem narratives. They observe disputes about interpretation but have no seat in the theological framing that produces either reading. The allegorical displacement reading removes them from the constraint's victim set entirely by denying the ethnic reference, which is both exculpatory and a form of epistemic exclusion.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, indigenous_peoples_and_descendants, observer,
    powerless, generational, analytical, global).

% The abstract principle that moral law applies universally and operates in the interior domain of conscience. This reading vindicates that principle by relocating violence from the external world (ethnic conquest) to internal domains (spiritual struggle). Moral law is not an actor but a vindicated proposition — it collects no rents from the constraint's operation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, universal_moral_law, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, universal_moral_law).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__allegorical_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__allegorical_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutical framework for reconciling biblical violence narratives with universal moral law: allows practitioners to extract meaning and spiritual discipline from herem texts without licensing or justifying actual conquest or ethnic subjugation. Solves the coordination problem of how to maintain canonical scriptural authority while adhering to subsequently developed universalist ethics.
% TRANSFER_FUNCTION: Moves interpretive authority from literal-historical exegetes to allegorical-spiritual interpreters; shifts the meaning-content of 'nations' and 'conquest' from ethnic and territorial referents to abstract spiritual enemies. No material extraction; the transfer is hermeneutical — control over what the text is understood to mean.
% ABSENT_VOICES: Literal-historical interpreters who read herem as a bounded but real historical command are structurally excluded from the allegorical displacement frame. Indigenous peoples and descendants of peoples historically targeted in conquest narratives are also absent from the theological conversation — they have no seat authoring the reading, though the allegorical displacement removes them from the constraint's victim set (which both exonerates and epistemically marginalizes them).
% DISAPPEARANCE_RATIONALE: If the allegorical displacement reading vanished and only literal-historical or durable-separation readings remained, the material arrangements of spiritual practice would persist unchanged — individuals would still engage in prayer, moral struggle, and scriptural study. The constraint is hermeneutical, not institutional. Its disappearance would not reorganize the world; it would only alter how certain scriptural passages are understood. The underlying practices (spiritual discipline, scriptural exegesis) would continue under alternative readings.
% FOUNDING_PROBLEM: How to maintain the canonical authority of herem texts within Christian and Jewish traditions that have developed universalist ethics incompatible with literal ethnic conquest. Early Church fathers and medieval theologians faced pressure to reconcile scriptural violence with emerging moral law doctrines.
% FOUNDING_PROBLEM_CORROBORATION: Theological historians (outside the immediate beneficiary set of this reading) document the interpretive pressure: Origen and Augustine developed allegorical readings explicitly to resolve the contradiction between herem and universal moral law. Contemporary philosophy and theology scholarship (Bird, Johnson, Middlemas) documents the contest: some interpreters accept that the founding problem is live and the allegorical solution is valid; others argue the problem is solved by historical-contextual reading rather than allegorical displacement. No unified corroboration from outside the theological interpretive community itself.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_unchanged).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).
:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the constraint operates entirely in the hermeneutical domain — it determines how a text is read, not who benefits materially or who is coerced. Suppression is low (0.15) because the reading is not enforced by external coercion; it is sustained by interpretive argument, theological coherence, and institutional transmission. Theater ratio is very low (0.08) because the allegorical reading is genuinely explanatory of its own frame — it is not performing a function it does not actually serve. The measurement trajectory shows suppression_requirement DECLINING over time (from ~0.35 at founding to 0.15 in the modern era) as the reading became institutionalized in mainstream theology and required less active defense against literal-historical challenges. Accessibility collapse is very high (0.92) because once the allegorical framework is adopted, the literal-historical reading becomes difficult to hold within the same moral-law commitment — the alternatives have been substantially collapsed by the acceptance of universal moral principles.
 *
 * PERSPECTIVAL GAP:
 *   From the theological-interpreter seat, the constraint is a genuine coordination mechanism solving the problem of maintaining scriptural authority within universalist ethics. From a literal-historical interpreter's seat (excluded), the constraint is an evasion: it empties the text of its concrete historical meaning. From an indigenous-peoples observer seat, the constraint is ambiguous: it exonerates their ancestors from literal conquest but does so by denying the ethnic reference altogether, which is a form of epistemic erasure. The engine's per-seat classification should diverge sharply: the theological interpreter's seat computes the constraint as rope (genuine coordination with no extraction); the literal-historical seat (were it included) would compute tangled_rope or snare (seeing evasion and loss of textual authority); the observer seat computes something between beneficiary and target (exonerated materially, but epistemically sidelined).
 *
 * DIRECTIONALITY LOGIC:
 *   Theological interpreters are the structural agenda-setters: they author and enforce this reading through institutional channels (seminaries, publishing, pulpits). Spiritual practitioners are beneficiaries: they gain a coherent hermeneutical framework that resolves the cognitive dissonance between scriptural violence and their own moral commitments. There is no victim group in the material sense because nothing is extracted. The 'moral philosophy' proposition is a secondary beneficiary — the reading vindicates universalist ethics by excluding literal conquest from the constraint's scope. Literal-historical interpreters are excluded because their reading is not part of this constraint's frame; they would experience the allegorical displacement as a foreclosure of their interpretive position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling scriptural violence with universal moral law) remains contested. Some theologians argue it is solved by the allegorical displacement; others argue it is better solved by historical contextualization (the 'contextual_supersession_reading' sibling). Still others argue the problem is a false problem because the literal reading is still valid ('durable_separation_reading' sibling). The constraint does not exhibit mandatrophy decay within its own frame — the allegorical reading is as functionally active today as in the patristic era. However, the displacement of the constraint from material/institutional domains to hermeneutical domains suggests the founding problem itself may be shifting: the pressure to reconcile herem with universal ethics is less urgent as herem readings move further from any material application to indigenous conquest, and the theological conversation has shifted toward other reconciliation strategies (supersession via historical context, or acceptance of bounded applicability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_vs_literal_hermeneutical_gate,
    'Is the allegorical displacement reading a genuine solution to the problem of reconciling herem with universal moral law, or a hermeneutical evasion that empties the text of its historical meaning and thereby avoids rather than resolves the moral contradiction?',
    'Comparative analysis across three resolution strategies: (1) allegorical displacement (this reading); (2) historical contextualization (contextual_supersession_reading); (3) acceptance of bounded moral applicability (durable_separation_reading). The criterion is whether each strategy maintains scriptural authority while addressing the universal-law problem. A resolution mechanism would track which strategy is most coherent within contemporary hermeneutical and moral-philosophy frameworks.',
    'If allegorical displacement is genuine resolution, the constraint stabilizes as rope (coordination). If it is evasion, the constraint either collapses (the problem never resolved, just deferred) or shifts to one of the sibling readings. The type classification depends on whether the hermeneutical work is real or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegorical_vs_literal_hermeneutical_gate, conceptual, 'Whether allegorical displacement solves or evades the moral-reconciliation problem.').

omega_variable(
    ethnic_reference_removal_and_epistemic_justice,
    'Does the removal of ethnic reference from the herem constraint constitute epistemic justice (exonerating indigenous peoples from conquest narratives) or epistemic injustice (erasing the historical reality of conquest and the peoples who experienced it)?',
    'Analysis of whether the allegorical displacement enables or forecloses acknowledgment of historical conquest and its moral weight. If the constraint enables separate historical-critical scholarship to proceed while the allegorical reading provides spiritual meaning, epistemic justice is served. If the allegorical displacement becomes a barrier to historical accountability (interpreters can claim ''it was always allegorical, never literal,'' thereby denying the conquest occurred), epistemic injustice is served.',
    'If epistemic justice, the constraint gains coherence as a benign meaning-making structure alongside historical accountability. If epistemic injustice, the constraint functions as a cover story — a form of suppression encoded in hermeneutics rather than overt coercion. This would reclassify the constraint from rope toward snare (extractiveness remains 0.0, but suppression would rise substantially).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethnic_reference_removal_and_epistemic_justice, empirical, 'Whether allegorical displacement enables or forecloses historical accountability.').

omega_variable(
    reading_contest_stability,
    'Will the allegorical displacement reading remain a live interpretive option, or will it be foreclosed by one of the sibling readings gaining institutional dominance?',
    'Monitoring of theological education, published commentary, and institutional teaching across major Christian and Jewish interpretive centers over the next 30 years. The resolution would track whether allegorical displacement maintains institutional seats (seminaries, publishing, pulpits) or is marginalized by contextual supersession or durable-separation readings.',
    'If allegorical displacement is foreclosed, it becomes a historical artifact rather than an active constraint. If it remains live, it competes with the siblings for institutional authority. The temporal stability of this reading determines whether the constraint has a future or represents a phase in a longer hermeneutical arc.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_stability, empirical, 'Long-term institutional viability of the allegorical displacement reading.').

omega_variable(
    internal_spiritual_domain_scope,
    'Does the relocation of herem to the internal spiritual domain (sin, temptation, vice) actually constrain behavior, or is it a meaning-making framework with no causal force on material practice?',
    'Ethnographic and behavioral analysis comparing communities that adopt the allegorical displacement reading with those adopting literal or supersession readings: do practitioners exhibit different moral behaviors, different relationships with scriptural texts, or different stances toward actual conquest and subjugation narratives? Null hypothesis: the constraint has no behavioral effect; it is purely hermeneutical.',
    'If the constraint has genuine behavioral effects (practitioners resist actual conquest differently, have different attitudes toward indigenous sovereignty, etc.), it functions as a real constraint on moral action. If it has no behavioral effect beyond interpretive coherence, it is a pure meaning-making structure with zero extractiveness precisely because it has zero coercive or material force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_spiritual_domain_scope, empirical, 'Whether the spiritual-displacement reframing has behavioral and material effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(here_tr_t0, projected).
narrative_ontology:measurement(here_tr_t400, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement_basis(here_tr_t400, observed).
narrative_ontology:measurement(here_tr_t800, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 800, 0.08).
narrative_ontology:measurement_basis(here_tr_t800, observed).
narrative_ontology:measurement(here_tr_t1200, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1200, 0.07).
narrative_ontology:measurement_basis(here_tr_t1200, observed).
narrative_ontology:measurement(here_tr_t1600, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement_basis(here_tr_t1600, observed).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(here_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(here_be_t0, observed).
narrative_ontology:measurement(here_be_t400, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 400, 0.0).
narrative_ontology:measurement_basis(here_be_t400, observed).
narrative_ontology:measurement(here_be_t800, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 800, 0.0).
narrative_ontology:measurement_basis(here_be_t800, observed).
narrative_ontology:measurement(here_be_t1200, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1200, 0.0).
narrative_ontology:measurement_basis(here_be_t1200, observed).
narrative_ontology:measurement(here_be_t1600, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1600, 0.0).
narrative_ontology:measurement_basis(here_be_t1600, observed).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement_basis(here_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(here_su_t0, projected).
narrative_ontology:measurement(here_su_t400, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 400, 0.28).
narrative_ontology:measurement_basis(here_su_t400, observed).
narrative_ontology:measurement(here_su_t800, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 800, 0.18).
narrative_ontology:measurement_basis(here_su_t800, observed).
narrative_ontology:measurement(here_su_t1200, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1200, 0.15).
narrative_ontology:measurement_basis(here_su_t1200, observed).
narrative_ontology:measurement(here_su_t1600, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1600, 0.14).
narrative_ontology:measurement_basis(here_su_t1600, observed).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement_basis(here_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, information_standard).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__allegorical_displacement_reading, 0.05).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).

% DUAL FORMULATION NOTE:
% The herem command (Deuteronomy 7, Joshua 6—11) is a contested kernel with three structurally distinct readings instantiated as separate constraints. Each reading relocates the constraint differently: (1) allegorical_displacement_reading (this one) → internal spiritual domain, ε=0.0 on conquest, victim set = abstract vices; (2) contextual_supersession_reading → historical-institutional domain, ε=low on material conquest (superseded), victim set = prophetic universalism; (3) durable_separation_reading → identity-preservation domain, ε=high on categorical boundary maintenance, victim set = designated outsiders. Each reading produces distinct ε-invariant constraint stories. Network edges declare that all three readings are in contest over the same kernel; a change in institutional dominance of one reading affects the salience and interpretation of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
