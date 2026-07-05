% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II as Organic Development (Hermeneutic of Continuity)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the continuity_reading of the contested kernel
 *   vatican_ii_doctrinal_authority: the claim, associated with Benedict XVI's
 *   2005 Roman Curia address and much subsequent magisterial teaching, that
 *   the Second Vatican Council's texts represent organic development of
 *   implicit prior doctrine rather than rupture, and that apparent
 *   discontinuities in liturgy and pastoral practice are prudential
 *   adaptations or implementation errors, not evidence of doctrinal change.
 *   Sibling readings — composite_overdetermination_reading,
 *   rupture_progressive_reading, and rupture_traditionalist_reading — are
 *   separate constraints, not alternative measurements of this one; per the
 *   ε-invariance principle, this file does not average or hedge across them.
 *   This reading's structural signature is low ε on doctrinal substance (the
 *   magisterium claims almost no doctrinal extraction — the teaching was
 *   always latent) paired with substantially higher ε on liturgical and
 *   pastoral practice, where the lived discontinuity for traditionalist and
 *   lay communities is real and the continuity framing does definitional work
 *   to reclassify it as non-rupture.
 *
 * KEY AGENTS:
 *   - post_conciliar_magisterium: agenda_setter (institutional/arbitrage) — defines and enforces the continuity interpretation
 *   - conciliar_curial_administration: beneficiary (institutional/arbitrage) — legitimacy and administrative continuity preserved
 *   - moderate_diocesan_clergy: beneficiary/payer (moderate/constrained) — implements the frame, absorbs parish-level friction
 *   - traditionalist_communities_under_liturgical_restriction: payer (powerless/constrained) — objection foreclosed by definition
 *   - progressive_theologians_disciplined_for_overreach: payer (moderate/constrained) — proposed extensions recast as rupture
 *   - laity_experiencing_liturgical_discontinuity: payer/beneficiary (powerless/constrained) — lived discontinuity relabeled adjustment
 *   - rupture_traditionalist_dissenters: excluded (organized/trapped) — sibling-reading holders treated as canonical irregularity
 *   - ecclesial_historians: observer (analytical) — assess textual and reception record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.28).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.42).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II as Organic Development (Hermeneutic of Continuity)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, '691b184b-4a56-4f3c-9f6d-2343b0301b5e').
narrative_ontology:cs_kernel_codification('691b184b-4a56-4f3c-9f6d-2343b0301b5e', fixed_text).
narrative_ontology:cs_authority_grounding('691b184b-4a56-4f3c-9f6d-2343b0301b5e', lineage).
narrative_ontology:cs_interpretation_layer_present('691b184b-4a56-4f3c-9f6d-2343b0301b5e').
narrative_ontology:cs_reading_relation('691b184b-4a56-4f3c-9f6d-2343b0301b5e', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('691b184b-4a56-4f3c-9f6d-2343b0301b5e', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('691b184b-4a56-4f3c-9f6d-2343b0301b5e', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('691b184b-4a56-4f3c-9f6d-2343b0301b5e', foundational, doctrinal_indefectibility_precludes_rupture).
narrative_ontology:cs_axiom_status(doctrinal_indefectibility_precludes_rupture, holdable).
narrative_ontology:cs_axiom_grounding('691b184b-4a56-4f3c-9f6d-2343b0301b5e', doctrinal_indefectibility_precludes_rupture, deontological).
narrative_ontology:cs_axiom('691b184b-4a56-4f3c-9f6d-2343b0301b5e', foundational, apparent_novelty_is_explication_not_innovation).
narrative_ontology:cs_axiom_status(apparent_novelty_is_explication_not_innovation, holdable).
narrative_ontology:cs_axiom_grounding('691b184b-4a56-4f3c-9f6d-2343b0301b5e', apparent_novelty_is_explication_not_innovation, conventional).
narrative_ontology:cs_reference_frame('691b184b-4a56-4f3c-9f6d-2343b0301b5e', pre_conciliar_magisterial_teaching_corpus).
narrative_ontology:cs_drift_state('691b184b-4a56-4f3c-9f6d-2343b0301b5e', post_2005_hermeneutic_of_reform_address, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('691b184b-4a56-4f3c-9f6d-2343b0301b5e', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, conciliar_curial_administration).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, moderate_diocesan_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_communities_under_liturgical_restriction).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_theologians_disciplined_for_overreach).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, laity_experiencing_liturgical_discontinuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, laity_experiencing_liturgical_discontinuity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, moderate_diocesan_clergy).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, doctrinal_continuity_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, hermeneutic_of_reform_in_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates the authoritative interpretation of the Council's documents, issues catechisms and magisterial statements (e.g. the 2005 Roman Curia address framing 'reform in continuity' against 'rupture'), and disciplines readings on either flank. Sets which implementations count as faithful development versus deviation, and holds the institutional apparatus to enforce that line.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_magisterium, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Administers the practical machinery of implementation — liturgical commissions, bishops' conferences, seminary formation. The continuity framing preserves institutional legitimacy across the pre/post-conciliar divide without requiring an admission that doctrine changed, which protects the administration's own claim to unbroken authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, conciliar_curial_administration, beneficiary,
    institutional, generational, arbitrage, global).

% Implement the reformed liturgy and pastoral practice in parishes day to day. The continuity reading gives them a stable interpretive frame to teach from and shields them from accusations of innovation, but they also absorb the friction when parishioners on both sides accuse them of either betraying tradition or resisting reform.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, moderate_diocesan_clergy, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, moderate_diocesan_clergy, payer).

% Hold that the pre-conciliar liturgy and disciplinary norms were not merely superseded but effectively suppressed under continuity-framed reforms (e.g. later restrictions on the older liturgical form). From their position, the continuity claim is precisely what forecloses their objection — if nothing changed, their preference for the prior form has no doctrinal standing, only nostalgia, and restricting it cannot be rupture by definition.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_communities_under_liturgical_restriction, payer,
    powerless, generational, constrained, national).

% Argued that the Council's texts pointed toward further doctrinal development (on collegiality, religious liberty, ecumenism) that the continuity reading declines to authorize. Several faced censure, removed faculties, or loss of teaching posts when their proposed extensions were classified as rupture rather than legitimate development.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_theologians_disciplined_for_overreach, payer,
    moderate, biographical, constrained, global).

% Experienced the vernacular liturgy, altered devotional practice, and changed parish life as a felt discontinuity regardless of the doctrinal framing offered from above. Some found renewed access and participation; others experienced loss of a familiar sacramental world they had no institutional channel to contest, since the official narrative names their experience a matter of adjustment, not rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, laity_experiencing_liturgical_discontinuity, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, laity_experiencing_liturgical_discontinuity, beneficiary).

% Hold the sibling rupture_traditionalist_reading — that the Council documents themselves contain ambiguities exploited for heterodox implementation. They are structurally excluded from magisterial legitimacy under this reading: their position is treated as a canonical irregularity (schismatic tendency) rather than a live theological dispute, and their communities operate under canonical restriction.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, rupture_traditionalist_dissenters, excluded,
    organized, generational, trapped, global).

% Study conciliar acta, drafting history, and reception across dioceses to assess whether the textual and pastoral record supports development-in-continuity or a more discontinuous account. Their findings are cited selectively by all three kernel readings.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, ecclesial_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, conciliar_curial_administration).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpretive frame that lets a global, doctrinally-bound institution absorb a major council's reforms without fracturing its claim to unbroken teaching authority across two millennia — coordinating what 'faithful Catholic' means post-Council for hundreds of millions of adherents and tens of thousands of clergy.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy toward the central magisterium and its administrative apparatus, and away from both traditionalist communities (whose preferred prior forms are recast as merely prudential, not doctrinally binding) and progressive theologians (whose proposed extensions are recast as unauthorized rupture). Practical liturgical and pastoral costs of implementation are transferred onto parish-level laity and clergy who did not set the interpretive terms.
% ABSENT_VOICES: Traditionalist communities and disciplined progressive theologians would object that the continuity framing is doing interpretive work the texts themselves do not settle, but their objections are treated as evidence of their own error (either nostalgic attachment or doctrinal overreach) rather than live counter-readings — a mechanism the continuity reading's own logic supplies to explain away dissent from either flank.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished as the operative magisterial frame, the institution would need an alternative account of its own authority across the conciliar divide. Beneficiaries (curial administration, moderate clergy) hold the world would substantially rearrange, since their legitimacy is anchored in the claim of unbroken teaching. Rupture-reading proponents on both flanks hold their situation would improve or stay roughly the same, since they already act as though rupture occurred; only the label would change.
% FOUNDING_PROBLEM: The Catholic magisterium needed to explain how a Council that visibly altered liturgy, ecumenical posture, and language about religious liberty and collegiality could be reconciled with a doctrine of the Church's own indefectibility and non-contradiction across time.
% FOUNDING_PROBLEM_CORROBORATION: The problem's existence is attested by ecclesial historians outside the magisterium (documenting genuine textual ambiguity and drafting compromises in the conciliar acta) and by both traditionalist and progressive theologians who agree something needed reconciling, even though they disagree with the magisterium's own resolution of it. No fully external (non-Catholic, non-theological) institution corroborates the continuity resolution specifically; the resolution itself is asserted primarily by the magisterium that benefits from it, which is the structural weak point this reading's own beneficiaries are least positioned to see.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at T60) because, under this reading, the doctrinal core is asserted unchanged — there is little to extract if nothing doctrinal moved. But the metric is not zero: administrative and disciplinary costs are real, concentrated on traditionalist and dissenting-progressive communities whose practice or teaching was restricted in the name of preserving continuity. Suppression (0.42) and theater_ratio (0.38) are both moderate and rising across the measured interval, reflecting increasing institutional apparatus devoted to defending and administering the continuity frame (catechetical instruction, disciplinary proceedings, liturgical policing) relative to the frame's original explanatory function. The one shared time grid (T0–T60, seven points) tracks all three metrics together; suppression shows a small local dip at T30 reflecting a period of relative liturgical relaxation before subsequent tightening — authored honestly rather than smoothed to a monotonic curve.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium and curial administration sit near the beneficiary end: the continuity claim is precisely what secures their own unbroken authority claim, so d is low for them. Moderate clergy sit close to symmetric — they benefit from doctrinal stability but pay implementation costs. Traditionalist communities and disciplined progressive theologians sit near the target end: the continuity reading is the specific mechanism that recodes their objections as either misplaced nostalgia or unauthorized innovation, foreclosing rather than adjudicating their claims. Rupture_traditionalist_dissenters are excluded rather than coordinated — their entire position is treated as outside the legitimate interpretive space this reading defines.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare or mountain) reflects that a genuine coordination function is present and real: without SOME authoritative account of continuity, a global doctrinally-bound institution has no way to reconcile Council-era change with its own claims about indefectibility, and total fragmentation into competing local orthodoxies would follow. The extraction is that this coordination function is used to foreclose live theological and pastoral disputes by definitional fiat rather than adjudicate them on their merits — converting what could be an open interpretive question into a settled one that happens to favor incumbent authority. This is exactly the divergence the framework is built to register: claimed_type is tangled_rope (not mountain, not rope) because both the coordination function and the asymmetric foreclosure are structurally present and both require active enforcement (disciplinary proceedings, liturgical restriction) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_vs_novel_teaching,
    'Is the doctrine on religious liberty, collegiality, and ecumenism articulated at Vatican II genuinely derivable from prior magisterial teaching (implicit development), or does it introduce propositions in tension with specific prior magisterial statements (e.g. 19th-century teaching on church-state relations)?',
    'Close textual comparison of conciliar documents against the specific prior magisterial texts claimed as their antecedents, adjudicated by historians and theologians working outside magisterial employment; assess whether the ''development'' meets standard criteria for organic doctrinal development (e.g. Newman''s notes) or requires reading prior texts against their plain sense.',
    'If genuinely implicit, the continuity reading''s central claim holds and the classification here is defensible as authored. If the derivation requires strained reading of prior texts, the coordination function is doing more definitional work than genealogical work, which would push the constraint''s true extractiveness higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_vs_novel_teaching, conceptual, 'Whether Vatican II''s teaching is derivable from or in tension with specific prior magisterial statements.').

omega_variable(
    kernel_framing_choice,
    'Is the choice to treat ''organic development'' as the operative interpretive category (rather than, e.g., ''doctrinal development under changed historical consciousness,'' which some theologians use to describe genuine change in a non-rupture but non-purely-continuous way) itself a contested framing that pre-selects the continuity verdict?',
    'Compare how the continuity reading''s own proponents (e.g. Ratzinger/Benedict XVI''s hermeneutic of reform) distinguish ''reform in continuity'' from simple identity-preservation, and check whether that distinction collapses back into asserting continuity by definition wherever change is found.',
    'If the category is drawn broadly enough to absorb any change as ''development,'' the continuity reading becomes unfalsifiable from within its own terms, which would justify treating its ε as under-measured rather than genuinely low. This bears directly on the CS framing choice recorded in cs_structure — an alternative framing (the legitimacy claim layered above the magisterium''s authority to define ''development'') might classify differently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the continuity category is broad enough to be self-confirming regardless of the underlying facts.').

omega_variable(
    post_conciliar_excess_attribution,
    'Are the liturgical and pastoral excesses attributed to ''implementation error'' genuinely disconnected from the Council''s own texts and drafting compromises, or did the documents'' deliberate ambiguities (products of a divided conciliar majority) make such excesses a foreseeable consequence rather than a pure implementation failure?',
    'Study of conciliar drafting history (relatio, modi, floor debates) to assess whether specific ambiguous formulations were known at the time to admit divergent readings, and whether conciliar fathers anticipated the range of subsequent implementation.',
    'If the ambiguity was foreseeable and unaddressed, attributing post-conciliar practice entirely to implementation error understates the documents'' own causal contribution, which would raise measured extractiveness on the pastoral/liturgical axis above what is currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_conciliar_excess_attribution, empirical, 'Whether post-conciliar liturgical excess is attributable to implementation alone or partly to textual ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 50, 0.37).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 50, 0.27).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__continuity_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of four members of the vatican_ii_doctrinal_authority constraint family, each instantiating a distinct reading of the same kernel with its own ε, beneficiary/victim structure, and classification. continuity_reading (this file) authors low doctrinal ε and moderate practice ε, classified tangled_rope. rupture_progressive_reading is expected to author higher doctrinal ε (treating the 'spirit of the Council' as licensing ongoing change beyond the text) with different beneficiaries (reform-oriented theologians and clergy) and likely classifies closer to a contested rope/tangled_rope depending on enforcement data. rupture_traditionalist_reading is expected to author high suppression and high ε on both axes, with the current continuity-reading's own beneficiaries appearing as its victims, likely classifying as tangled_rope or snare from the traditionalist seat. composite_overdetermination_reading decomposes the singular 'Vatican II' label into several separable structural shifts and is expected to show the widest ε variance across its component axes, since it declines to treat the reforms as unified at all. All four link via affects_constraints; none is a re-measurement of another — each is a structurally distinct claim sharing only the historical event as raw material.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
