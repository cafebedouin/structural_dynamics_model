% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus as Performance-Only Archive
 *   domain: religious/rabbinic/commitment_system
 *
 * SUMMARY:
 *   The Kodashim corpus (Talmudic tractates on sacrificial law) functions in
 *   the 'performance_only' reading as an archived blueprint — a husk
 *   preserving the exact specifications of a worship system that cannot
 *   currently operate. The constraint is the institutional insistence that
 *   mastery of this archive constitutes *preparation* for a messianic
 *   restoration that will reactivate the sacrificial cult. This reading
 *   extracts legitimacy, resources, and devotion from adherents by presenting
 *   the impossible future as an imminent operational requirement. The
 *   extraction is high because the 'blueprint' narrative cannot be falsified
 *   (restoration is always 'imminent'), the theater ratio is high because the
 *   performative maintenance of readiness (curriculum, ritual simulation,
 *   priestly lineage tracking) increasingly substitutes for any verifiable
 *   divine transaction, and suppression operates through the exclusion of
 *   alternative readings (substitution_archive) and the marginalization of
 *   those who attempt present-tense performance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.78).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.65).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Performance-Only Archive").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/rabbinic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '27eca5d4-9fb0-492a-9d0b-dfb0e75305cb').
narrative_ontology:cs_kernel_codification('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', fixed_text).
narrative_ontology:cs_authority_grounding('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', lineage).
narrative_ontology:cs_interpretation_layer_present('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb').
narrative_ontology:cs_reading_relation('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', kodashim_corpus__substitution_archive, influences).
narrative_ontology:cs_axiom('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', foundational, sacrificial_system_suspended_not_abrogated).
narrative_ontology:cs_axiom_status(sacrificial_system_suspended_not_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', sacrificial_system_suspended_not_abrogated, deontological).
narrative_ontology:cs_axiom('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', foundational, priestly_readiness_obligation_persists).
narrative_ontology:cs_axiom_status(priestly_readiness_obligation_persists, holdable).
narrative_ontology:cs_axiom_grounding('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', priestly_readiness_obligation_persists, deontological).
narrative_ontology:cs_axiom('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', secondary, kodashim_mastery_fulfills_readiness).
narrative_ontology:cs_axiom_status(kodashim_mastery_fulfills_readiness, holdable).
narrative_ontology:cs_axiom_grounding('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', kodashim_mastery_fulfills_readiness, conventional).
narrative_ontology:cs_reference_frame('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', tannaitic_suspension_theology).
narrative_ontology:cs_drift_state('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', contemporary_institutional_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('27eca5d4-9fb0-492a-9d0b-dfb0e75305cb', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, archive_as_living_practice_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, temple_mount_activists).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, messianic_restoration_teleology).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, oral_torah_immutability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivot and kollelim that center their curriculum and fundraising on the claim that mastering Kodashim prepares the priesthood for imminent Temple service. They extract legitimacy, enrollment, and donor support from the archive's status as 'awaiting activation' — a status that cannot be falsified in their lifetime. Exit would require abandoning the messianic teleology that constitutes their institutional identity.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Devoted learners who treat intensive Kodashim study as a form of actual divine service — investing years of cognitive labor, emotional commitment, and communal standing in the belief that their study 'performs' the mitzvot in potentia. They bear the cost of misallocated devotion: the archive demands total engagement but delivers no verifiable divine transaction. Exit is constrained by sunk identity investment and communal expectations.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, archive_as_living_practice_adherents, payer,
    organized, biographical, constrained, global).

% Poskim and roshei yeshiva who authorize the curriculum and certify its spiritual value. They administer the constraint by validating the 'performance-only' reading as the normative framework for Kodashim engagement. Their position allows them to modulate emphasis — they could pivot toward study_as_exercise or substitution_archive framings without losing authority — giving them arbitrage-grade exit from any single reading's collapse.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, halakhic_authorities_mainstream, agenda_setter,
    institutional, generational, arbitrage, global).

% Political-religious groups that treat Kodashim mastery as a prerequisite for actual Temple reconstruction. They benefit from the archive's 'blueprint' status as a mobilization tool and legislative justification. Their identity is fused to the restoration narrative — exit would dissolve their raison d'être. They exert pressure on halakhic authorities to maintain the performance-only frame.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, temple_mount_activists, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, temple_mount_activists, beneficiary).

% Scholars who study Kodashim as historical text, redactional layers, and comparative ritual archive. They neither collect rents nor pay devotion. Their analytical seat sees the constraint's structural operation across all three readings without being bound by any.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, academic_talmudists, observer,
    analytical, civilizational, analytical, universal).

% Marginal figures who attempt to perform actual korbanot (sacrifices) or Temple-adjacent rituals in the present — e.g., Pascal lamb reenactments, red heifer candidates. They are structurally excluded by the performance-only reading, which defines their actions as premature or invalid. They would object that the archive is not a husk but a suspended obligation — but they lack institutional standing to enter the conversation.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, lapidary_pietists, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed, stateless people around a shared teleological horizon: the belief that the Temple will be restored and the priesthood must be ready. The archive serves as a synchronization mechanism — a common reference point that aligns curriculum, prayer, and messianic expectation across geography and generations.
% TRANSFER_FUNCTION: Moves cognitive labor, communal status, and material resources (donations, institutional funding) from adherents who treat study as living practice to institutions that monetize the 'preparation' narrative. The transfer is legitimated by the unverifiable claim that this labor accumulates merit for a future redemption.
% ABSENT_VOICES: The lapidary pietists who attempt present-tense performance are excluded. So are the quiet dissenters within yeshiva worlds who suspect the archive is a husk but cannot voice it without risking their position. The substitution_archive reading — which would declare the obligation terminated — is systematically silenced in mainstream halakhic discourse because it undermines the teleology.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished overnight, the institutional architecture built around 'preparation for Temple service' would lose its legitimating narrative. Yeshivot would need new curricular anchors; fundraising narratives would collapse; the synchronization of messianic expectation across the diaspora would fracture. The world would rearrange toward either study_as_exercise (intellectualizing the archive) or substitution_archive (memorializing it) — or toward the lapidary pietist fringe gaining legitimacy.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the sacrificial system — the core of Israel's covenantal worship — became impossible to perform. The rabbinic movement needed to preserve the *memory* and *legal structure* of that system so it could be instantly reactivated upon restoration, while simultaneously preventing the obligation from lapsing into oblivion.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the rabbinic sources themselves (Mishnah, Tosefta, Talmud) — but the *status* of that problem is contested. Messianic-preparation institutions and temple activists attest it is LIVE (restoration is imminent, the blueprint must be ready). Academic talmudists and the substitution_archive reading attest it is DEAD (the historical conditions for sacrificial worship are gone; the obligation was transformed, not suspended). No neutral arbiter exists — the corroboration split *is* the kernel contest.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) reflects the widening gap between the archive's demands (total cognitive/emotional investment) and its delivery (zero verifiable sacrificial efficacy). Theater ratio (0.72) captures the shift from early rabbinic period — where the archive was a genuine preservation effort against historical erasure — to the medieval and modern periods where 'readiness' became a self-justifying institutional engine. Suppression (0.65) is structural: the performance-only frame defines the terms of legitimate engagement, rendering substitution_archive heretical and lapidary pietism premature. Accessibility collapse (0.58) is moderate — alternatives exist intellectually but are socially/identity-blocked. Resistance (0.42) reflects that the constraint meets pushback primarily from academic and fringe quarters, not from within the core adherent population.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (institutions, activists) experience the constraint as genuine coordination — a sacred trust preserving the covenant against historical entropy. The payer seats (adherents) experience it as enforced extraction — a demand for total engagement with an archive that delivers no divine feedback. The engine will compute this divergence from the structural data; the authored claim (snare) reflects the payer-seat reality, which the beneficiary seats would contest as a category error.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions and temple activists are structural beneficiaries (d ~ 0.15-0.25): they collect legitimacy, resources, and mobilization power from the archive's 'awaiting activation' status. Archive-as-living-practice adherents are targets (d ~ 0.85): they invest devotion that the constraint cannot reciprocate with divine transaction. Halakhic authorities sit near symmetric (d ~ 0.5): they administer the frame but hold arbitrage-grade exit — they could pivot readings without losing institutional standing. Academic talmudists are analytical observers (d = 0.0 by definition). Lapidary pietists are excluded/trapped (d ~ 0.95): they bear the cost of the constraint's exclusionary boundary without any seat at the table.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving the sacrificial system for instant reactivation) was structurally live in the early rabbinic period (70-500 CE) when restoration was historically plausible. As centuries passed without restoration, the problem died — but the arrangement persisted and intensified. The mandate atrophied into a self-sustaining extraction engine: the 'preparation' narrative now generates the very resources that maintain the institutions, with no external referent to validate it. This is classic mandatrophy — a constraint whose justification has vanished but whose enforcement machinery has grown more elaborate. The performance-only reading is the *mechanism* of the mandatrophy: it converts the dead founding problem into a live extraction claim by making the future unfalsifiable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_only_naturalness,
    'Is the performance-only reading a genuine preservation of rabbinic intent, or a later institutional construction that retroactively claims the archive as a ''blueprint''?',
    'Historical-redactional analysis of early rabbinic sources: do Tannaitic and Amoraic texts frame Kodashim study as *preparation for future performance* or as *substitute performance*? The Mishnah''s redaction context (post-70 but pre-Bar Kokhba) is the critical window.',
    'If the ''blueprint'' framing is a post-Talmudic institutional overlay, the constraint''s claimed naturalness (as authentic rabbinic theology) collapses — it becomes a constructed snare with identifiable institutional authors. If it is authentically early, the extraction is at least rooted in a founding community''s genuine theological commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_only_naturalness, empirical, 'Whether the performance-only frame is authentically rabbinic or institutionally constructed.').

omega_variable(
    substitution_archive_suppression_mechanism,
    'How exactly does the performance-only reading suppress the substitution_archive reading — is it through explicit herem (ban), curricular omission, or the structural unfalsifiability of the messianic horizon?',
    'Sociology of knowledge study: trace how substitution_archive positions (e.g., Maimonides'' Guide III:32, early Reform, academic talmud) are marginalized in yeshiva curricula and halakhic discourse. Distinguish active suppression from structural crowding-out.',
    'If suppression is active (herem, censorship), the constraint''s coercive machinery is stronger than the metrics suggest. If it is structural (the messianic horizon simply makes substitution_archive unintelligible within the frame), the suppression is endogenous to the reading''s logic — harder to challenge, but also less attributable to identifiable agents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitution_archive_suppression_mechanism, conceptual, 'Mechanism of suppression against the substitution_archive sibling reading.').

omega_variable(
    messianic_horizon_unfalsifiability,
    'Is the messianic restoration horizon structurally unfalsifiable (by theological design) or merely empirically unfalsified (so far)?',
    'Theological analysis: does the performance-only reading contain internal criteria for *disconfirmation* (e.g., ''if X centuries pass without restoration, the obligation lapses'')? Or does it define the horizon as permanently open by covenantal necessity?',
    'If structurally unfalsifiable, the extraction is architecturally permanent — no future evidence can trigger sunset. If merely empirically unfalsified, a theoretical sunset condition exists (even if never activated), which would make the constraint a scaffold with a non-operational sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_horizon_unfalsifiability, conceptual, 'Whether the extraction''s temporal horizon is architecturally open or conditionally closed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.25).
narrative_ontology:measurement(koda_tr_t450, kodashim_corpus__performance_only, theater_ratio, 450, 0.42).
narrative_ontology:measurement(koda_tr_t900, kodashim_corpus__performance_only, theater_ratio, 900, 0.58).
narrative_ontology:measurement(koda_tr_t1350, kodashim_corpus__performance_only, theater_ratio, 1350, 0.67).
narrative_ontology:measurement(koda_tr_t1950, kodashim_corpus__performance_only, theater_ratio, 1950, 0.72).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(koda_be_t450, kodashim_corpus__performance_only, base_extractiveness, 450, 0.48).
narrative_ontology:measurement(koda_be_t900, kodashim_corpus__performance_only, base_extractiveness, 900, 0.62).
narrative_ontology:measurement(koda_be_t1350, kodashim_corpus__performance_only, base_extractiveness, 1350, 0.71).
narrative_ontology:measurement(koda_be_t1950, kodashim_corpus__performance_only, base_extractiveness, 1950, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(koda_su_t450, kodashim_corpus__performance_only, suppression_requirement, 450, 0.35).
narrative_ontology:measurement(koda_su_t900, kodashim_corpus__performance_only, suppression_requirement, 900, 0.52).
narrative_ontology:measurement(koda_su_t1350, kodashim_corpus__performance_only, suppression_requirement, 1350, 0.61).
narrative_ontology:measurement(koda_su_t1950, kodashim_corpus__performance_only, suppression_requirement, 1950, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__performance_only, 0.12).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, temple_mount_activism).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, priestly_lineage_registry).

% DUAL FORMULATION NOTE:
% This constraint (performance_only) and its two sibling readings form the kodashim_corpus constraint family. All three share the kernel_id 'kodashim_corpus' but instantiate different constraints with different ε values, beneficiary/victim structures, and claimed types. performance_only: ε=0.78, snare, beneficiary=messianic_preparation_institutions. study_as_exercise: ε~0.25, rope/tangled_rope, beneficiary=adherents (study-as-worship). substitution_archive: ε~0.1, mountain/rope, no concentrated beneficiaries. The family linkage enables contamination analysis: if performance_only's extraction is confirmed, it predicts pressure on the sibling readings' legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__performance_only, institutional, 0.18).
constraint_indexing:directionality_override(kodashim_corpus__performance_only, organized, 0.22).
constraint_indexing:directionality_override(kodashim_corpus__performance_only, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
