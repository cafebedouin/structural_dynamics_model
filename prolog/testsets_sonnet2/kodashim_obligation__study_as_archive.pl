% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Historical-Archival Obligation (Archive Reading)
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This story instantiates ONE of three contested readings of the
 *   kodashim_obligation kernel: study_as_archive. Under this reading, the
 *   Talmudic order of Kodashim (sacrificial and Temple law) documents a
 *   legal-ritual system that ceased to operate with the Temple's destruction
 *   and has no realistic prospect of restoration. Study of Kodashim in this
 *   reading is neither a binding legal obligation (the law has no applicable
 *   subject) nor a cosmically efficacious performance (there is no
 *   substitutive mechanism by which textual study accomplishes what sacrifice
 *   once did). Its function is historical preservation and
 *   identity-maintenance: it keeps alive the record and self-understanding of
 *   a tradition that once had a functioning cult, and its continued study
 *   within comprehensive curricula reinforces communal continuity narratives.
 *   As time passes, this reading holds, the study's genuine function drifts
 *   further from anything resembling live legal or cosmic content and
 *   increasingly resembles institutional theater — comprehensive coverage
 *   sustained because it is traditional to sustain it, not because the
 *   coverage does the work its position in a legal or cosmic curriculum
 *   implies. The sibling readings (study_as_performance,
 *   study_as_preparation) are NOT instantiated here; they are separate
 *   constraints with their own ε and stakeholder structures, referenced only
 *   via network links and omega variables per Rule 1.
 *
 * KEY AGENTS:
 *   - yeshiva_curricular_authorities: institutional agenda-setter administering the curriculum
 *   - students_of_applicable_law: bear the opportunity cost of study-time allocation
 *   - communal_identity_institutions: beneficiary of the continuity narrative without collecting material rent
 *   - study_as_performance_adherents and study_as_preparation_adherents: excluded rival readings of the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.42).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.28).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, piton).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical-Archival Obligation (Archive Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious_studies/jewish_law/textual_preservation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, 'fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c').
narrative_ontology:cs_kernel_codification('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', fixed_text).
narrative_ontology:cs_authority_grounding('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', lineage).
narrative_ontology:cs_interpretation_layer_present('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c').
narrative_ontology:cs_reading_relation('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', foundational, temple_system_is_historically_closed).
narrative_ontology:cs_axiom_status(temple_system_is_historically_closed, holdable).
narrative_ontology:cs_axiom_grounding('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', temple_system_is_historically_closed, empirically_contingent).
narrative_ontology:cs_axiom('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', foundational, study_function_is_archival_not_substitutive).
narrative_ontology:cs_axiom_status(study_function_is_archival_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', study_function_is_archival_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', second_temple_operative_cult).
narrative_ontology:cs_drift_state('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', post_70ce_diaspora_scholasticism, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('fdca3bda-5b18-4c0a-82c2-f0ec3e4e170c', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, communal_identity_institutions).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, yeshiva_curricular_authorities).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, students_of_applicable_law).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, practical_halachic_scholarship).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, textual_continuity_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, historical_self_understanding_of_the_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set curricula that allocate substantial study hours to Kodashim (sacrificial/Temple law) alongside or ahead of applicable civil and ritual law. They administer the tradition of comprehensive Talmudic study and can reweight curricular time, but doing so would require justifying a departure from received pedagogical order. They do not personally collect a concentrated rent from this allocation; the cost of reallocating hours away from Kodashim is primarily reputational and institutional, not financial.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, yeshiva_curricular_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Spend years of study time on a body of law (sacrificial procedure, Temple architecture, priestly purity) that has no live application in their devotional or civil life, at the opportunity cost of deeper mastery in the law they actually practice (Sabbath, family law, civil disputes, kashrut). Their exit from the curriculum is constrained by communal expectation and the structure of ordination tracks that presume comprehensive Talmudic coverage.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, students_of_applicable_law, payer,
    moderate, biographical, constrained, national).

% As a field, receives less cumulative scholarly attention, publication volume, and pedagogical innovation than it would if study hours were reallocated from a defunct legal system to actively contested and applicable questions. This is a diffuse institutional cost, not one borne by a named actor, but the field's development is measurably shaped by where communal study energy is directed.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, practical_halachic_scholarship, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, practical_halachic_scholarship).

% Draw legitimacy and continuity from a curriculum that treats the entire Talmud, including Kodashim, as a unified and unbroken inheritance. Communal self-understanding as heirs to a complete legal-historical tradition (including the Temple cult) is reinforced by universal study rather than selective study of only the 'live' tractates. They do not collect a material rent; the benefit is symbolic and identity-constitutive.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, communal_identity_institutions, beneficiary,
    organized, civilizational, analytical, global).

% Hold that studying Kodashim is not archival but cosmically efficacious in itself, substituting for actual sacrifice. From within the archive reading, this claim is treated as a rival theological framing rather than incorporated into curricular justification; adherents of the performance view are not consulted when the archive reading is used to justify or critique study-time allocation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, study_as_performance_adherents, excluded,
    organized, civilizational, identity_locked, global).

% Hold that the sacrificial law remains formally binding and unperformable pending messianic restoration, so study is preparatory technical maintenance of law-in-abeyance, not archival history. This reading treats their premise (live-but-suspended obligation, restoration as a real future contingency) as false; they are not present in the archive reading's own justificatory account.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, study_as_preparation_adherents, excluded,
    organized, civilizational, identity_locked, global).

% Study the sociology and history of Talmudic curricula from outside any of the three theological commitments, documenting how communities allocate study time and what functions (identity, legal preparedness, cosmic performance) different communities and scholars claim for the same textual corpus.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, religious_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Universal study of the full Talmudic corpus, including Kodashim, coordinates a shared curriculum across geographically dispersed communities, producing a common intellectual inheritance and a recognizable credential (comprehensive Talmudic literacy) that any community can verify in any other.
% TRANSFER_FUNCTION: Moves scholarly and pedagogical time and attention away from applicable law (civil, family, ritual law in current use) and toward the historical record of a defunct sacrificial-Temple system, in exchange for reinforced communal continuity and identity narrative.
% ABSENT_VOICES: Adherents of the study_as_performance and study_as_preparation readings would object that the archive framing strips the study of its binding or cosmically efficacious character; they are excluded from this reading's own justificatory account, which treats the Temple system as closed rather than suspended or spiritually live.
% DISAPPEARANCE_RATIONALE: If Kodashim study vanished from the curriculum, the practical halachic tracks would be largely unaffected and applicable-law scholarship might gain resources; but institutions whose legitimacy rests on comprehensive Talmudic mastery would face a real identity disruption, and ordination structures built around full-corpus study would need restructuring. Whether this counts as 'the world rearranges' depends on which institutional layer is asked, hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Original codification of Kodashim (Mishnah, redacted after the Temple's destruction) preserved the operational detail of a functioning sacrificial cult so that its procedures would not be lost to memory; later strata of study extended this into ongoing communal practice as the cult itself receded from any living possibility of restoration.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Mishnaic and Talmudic periods (outside any beneficiary community) attest that the Temple cult ceased functioning in 70 CE and that no political or infrastructural conditions for restoration have existed since; this is corroborated by archaeological and historical consensus independent of the yeshiva curricular authorities or communal identity institutions who benefit from the study's continuation.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).
:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) and rising over the interval: the archive reading holds that as the historical distance from any live Temple system grows, the justificatory gap between 'this is worth comprehensive study' and 'this documents a closed chapter' widens, and institutions increasingly must supply identity-based rather than functional justifications for the allocation — hence extraction (of study time, of curricular priority) that is not offset by applicable output. Theater ratio is authored moderate-to-high and rising (0.2 to 0.55) because, under this reading, an increasing share of the pedagogical apparatus around Kodashim (ceremonial completion-of-tractate celebrations, curricular framing as equally 'live' Torah) performs continuity rather than transmits anything usable, consistent with piton dynamics: a structure whose original function (real-time legal/ritual documentation for an operating cult) has atrophied but whose institutional maintenance continues by inertia and identity investment. Suppression is authored low-moderate (0.28) because no one is coercively barred from studying Kodashim less or reallocating time to applicable law — the constraint operates through curricular custom and communal expectation, not legal or economic coercion. Accessibility collapse is moderate-low (0.35): meaningful curricular alternatives exist and are exercised by some communities (e.g., programs emphasizing practical halacha), so alternatives have not fully collapsed even where custom is strong.
 *
 * PERSPECTIVAL GAP:
 *   From the yeshiva curricular authority's seat, comprehensive study including Kodashim is simply what complete Talmudic learning has always meant — a coordination function producing a shared, portable credential. From the seat of a student oriented toward applicable law, the same curricular structure computes as an extraction of scarce study years toward a corpus with no practical return, justified by an identity claim rather than a functional one. Neither seat is in error; the engine's per-seat computation is exactly what registers this divergence rather than forcing one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   communal_identity_institutions and yeshiva_curricular_authorities sit near the beneficiary end: they derive continuity and legitimacy without personally paying an opportunity cost measured against an alternative practical curriculum. students_of_applicable_law and the diffuse field of practical_halachic_scholarship sit nearer the target end: they bear the actual opportunity cost in study-years and field development respectively, with constrained exit because leaving the study track carries communal and credentialing costs. The excluded rival-reading adherents are not directionality targets of THIS constraint since they reject its premises entirely; they are structurally outside the archive reading's own account, which is exactly what the excluded role and the omega variables below are for.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving operational knowledge of a functioning cult) is authored as dead: no political, infrastructural, or theological consensus exists within this reading for imminent restoration, and the historical record independently corroborates that the underlying system stopped operating in 70 CE. Yet the disappearance_verdict is contested rather than world_unchanged, because institutional legitimacy has been built on top of the original function and would genuinely be disrupted by the practice's removal — this is precisely the mandatrophy signature the classification exists to catch: a mandate whose original problem is resolved (dead) but whose institutional apparatus persists and has accreted independent stakes (identity, credentialing, curricular structure) that make its removal costly regardless of whether its original justification survives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_possibility_ambiguity,
    'Is Temple restoration genuinely structurally impossible/foreclosed (supporting the archive reading), or a live theological contingency being actively prepared for (supporting the preparation reading), or already cosmically accomplished through study itself (supporting the performance reading)?',
    'No empirical resolution mechanism exists for a theological contingency; the three readings are held by different communities and sub-traditions as live, non-adjudicable commitments. Resolution here is doctrinal/preference-based, not evidentiary — this omega documents that irreducibility rather than proposing to close it.',
    'If restoration is treated as genuinely live (preparation reading), the extraction figure for this archive-reading story would not apply to that reading''s own constraint — that story author its own lower ε reflecting real preparatory function. If restoration is treated as already spiritually accomplished (performance reading), that sibling story would author near-zero ε. This story''s ε (0.42) is valid only for the archive reading and must not be read as adjudicating between the three.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_possibility_ambiguity, conceptual, 'Whether the Temple system''s closure is settled, contested, or theologically transcended — the central kernel-level disagreement.').

omega_variable(
    curricular_reallocation_counterfactual,
    'If study hours were reallocated from Kodashim to applicable law, would practical halachic scholarship and student competence measurably improve, or would the freed time simply be absorbed by other non-applicable pursuits?',
    'Comparative study of yeshiva curricula that already de-emphasize Kodashim (e.g., some practically-oriented programs) against those that maintain comprehensive coverage, tracking downstream halachic decision-making competence and publication output.',
    'If reallocation would demonstrably improve applicable-law competence, the extraction claim is strengthened (the opportunity cost is real and measurable). If freed time would simply be redirected to other non-applicable study, the victim framing (practical_halachic_scholarship as payer) weakens considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curricular_reallocation_counterfactual, empirical, 'Whether the claimed opportunity cost to applicable-law scholarship is a real, measurable transfer or a speculative framing.').

omega_variable(
    identity_versus_extraction_boundary,
    'Is the identity-continuity benefit that communal_identity_institutions derive from comprehensive study a genuine, non-rival coordination good (everyone benefits from a shared inheritance, no one is made worse off), or does it function as extraction because it is purchased with diverted study time that has a real opportunity cost for specific individuals?',
    'Distinguish cases: if study time is drawn from genuine surplus (leisure, discretionary hours) the identity benefit is closer to a pure rope; if it is drawn from a fixed, scarce curricular budget that trades off against applicable-law instruction, it is closer to extraction. Survey of actual time budgets across yeshiva types would help resolve this empirically.',
    'Resolving toward ''genuine surplus'' would push this constraint toward rope; resolving toward ''scarce, traded-off budget'' supports the piton/moderate-extraction classification authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_versus_extraction_boundary, empirical, 'Whether the coordination benefit and the extraction cost are drawn from the same scarce resource or from genuinely separate pools.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t20, kodashim_obligation__study_as_archive, theater_ratio, 20, 0.28).
narrative_ontology:measurement(koda_tr_t40, kodashim_obligation__study_as_archive, theater_ratio, 40, 0.36).
narrative_ontology:measurement(koda_tr_t60, kodashim_obligation__study_as_archive, theater_ratio, 60, 0.44).
narrative_ontology:measurement(koda_tr_t80, kodashim_obligation__study_as_archive, theater_ratio, 80, 0.5).
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_archive, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(koda_be_t20, kodashim_obligation__study_as_archive, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(koda_be_t40, kodashim_obligation__study_as_archive, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(koda_be_t60, kodashim_obligation__study_as_archive, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(koda_be_t80, kodashim_obligation__study_as_archive, base_extractiveness, 80, 0.39).
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_archive, base_extractiveness, 100, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_archive, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'the obligation to study Kodashim' per the epsilon-invariance principle. study_as_archive (this story) authors moderate, rising extraction reflecting a closed-system-with-institutional-inertia reading; study_as_performance would author near-zero extraction reflecting a fully self-sufficient cosmic-efficacy reading; study_as_preparation would author low-to-moderate extraction reflecting a genuine-future-contingency reading with scaffold-leaning structure. The three share a textual kernel (the Mishnaic/Talmudic corpus of Kodashim and its associated obligation-claims) but instantiate structurally distinct constraints with different ε, different claimed types, and different beneficiary/victim structures, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
