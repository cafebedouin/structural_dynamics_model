% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrifice Law as Archival/Cultural-Memory Study (No Normative Force)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates the 'archival_preservation' reading of the
 *   sacrifice_obligation_continuity kernel: the position that with the
 *   Temple's destruction, the physical performance obligation lapsed
 *   entirely, and that ongoing study of the sacrifice-law corpus is a
 *   cultural and scholarly practice preserving textual tradition and communal
 *   memory, carrying no residual normative force. This is structurally
 *   distinct from the sibling readings (study_as_performance,
 *   performance_only, messianic_suspension), which hold that some form of
 *   obligation persists — either fulfilled through study itself, suspended
 *   pending restoration, or awaiting future physical performance. Under this
 *   reading, no party is extracted from and no enforcement apparatus exists:
 *   the constraint approaches a pure, low-extraction coordination mechanism
 *   around shared memory, not a binding rule.
 *
 * KEY AGENTS:
 *   - textual_scholars: Primary beneficiary (moderate/mobile) — gain scholarly and pedagogical value from the corpus
 *   - cultural_memory_communities: Primary beneficiary (organized/mobile) — gain communal continuity and identity
 *   - comparative_religion_researchers: Analytical observer — studies the reading as a case of obligation-lapse
 *   - adherents_of_rival_kernel_readings: Excluded voice — would contest the non-normative framing but are outside this reading's operative scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.03).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.03).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Law as Archival/Cultural-Memory Study (No Normative Force)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, '5f0d75f6-5f53-480a-a49f-8f02c39b94fc').
narrative_ontology:cs_kernel_codification('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', fixed_text).
narrative_ontology:cs_authority_grounding('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', practice).
narrative_ontology:cs_interpretation_layer_present('5f0d75f6-5f53-480a-a49f-8f02c39b94fc').
narrative_ontology:cs_reading_relation('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', foundational, obligation_lapses_with_institutional_site).
narrative_ontology:cs_axiom_status(obligation_lapses_with_institutional_site, holdable).
narrative_ontology:cs_axiom_grounding('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', obligation_lapses_with_institutional_site, conventional).
narrative_ontology:cs_axiom('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', secondary, study_is_cultural_not_normative_practice).
narrative_ontology:cs_axiom_status(study_is_cultural_not_normative_practice, holdable).
narrative_ontology:cs_axiom_grounding('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', study_is_cultural_not_normative_practice, conventional).
narrative_ontology:cs_reference_frame('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', temple_era_sacrificial_obligation).
narrative_ontology:cs_drift_state('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', post_destruction_rabbinic_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('5f0d75f6-5f53-480a-a49f-8f02c39b94fc', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, textual_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, cultural_memory_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, comparative_religion_researchers).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, historical_continuity_of_tradition).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, textual_preservation_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study sacrifice law texts (Mishnah Kodashim, Temple-era sources) as historical and literary material. Gain scholarly output, teaching material, and preserved textual tradition. No obligation attaches to them beyond ordinary academic or communal study norms; they can stop studying this corpus and take up another without consequence.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, textual_scholars, beneficiary,
    moderate, generational, mobile, national).

% Communities and congregations that read these texts liturgically (e.g., in place of Temple-era practice) to maintain continuity with tradition and communal identity. They receive a sense of historical rootedness and shared memory. Nothing compels their participation; the practice is voluntary and substitutable with other forms of commemoration.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, cultural_memory_communities, beneficiary,
    organized, generational, mobile, national).

% Study the sacrifice-law corpus alongside other ritual systems to understand how obligations transform, atrophy, or get reinterpreted after the loss of their institutional site (the Temple). They have no stake in whether the law is binding; their interest is comparative and descriptive.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, comparative_religion_researchers, observer,
    analytical, civilizational, analytical, global).

% Communities holding the study_as_performance, performance_only, or messianic_suspension readings would object that this reading strips the study of any normative weight, reducing a commandment to a museum artifact. They are not present in this constraint's operation because the archival-preservation reading, by construction, does not engage their obligation claim — it treats the question as already settled in the negative.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, adherents_of_rival_kernel_readings, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__archival_preservation, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__archival_preservation, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates preservation of a textual and cultural corpus (Temple sacrifice law) so that historical knowledge, liturgical vocabulary, and communal memory are not lost after the institutional site (the Temple) that once made the law operative ceased to exist.
% TRANSFER_FUNCTION: Moves attention, scholarly labor, and communal ritual time toward preserving and transmitting a body of text. No material extraction occurs from any party; the 'transfer' is of cultural continuity, not obligation or resource.
% ABSENT_VOICES: Adherents of the study_as_performance, performance_only, and messianic_suspension readings would object that treating the law as non-binding empties the commandment of religious weight; they are structurally absent because this reading's own premise (obligation has exited constraint space) does not require adjudicating their claim.
% DISAPPEARANCE_RATIONALE: If this specific reading (archival, non-normative study) vanished overnight, no material or legal arrangement would rearrange — scholars would simply study the corpus under a different rationale (historical curiosity, comparative religion, or one of the sibling normative readings), and communities would find another vehicle for cultural memory. Nothing depends on this reading's non-normativity being the operative frame; it is closer to a natural, low-stakes descriptive fact about how one community understands its own textual practice than to an enforced arrangement.
% FOUNDING_PROBLEM: After the Temple's destruction, sacrifice could no longer be physically performed. This reading answers the resulting question — what should study of the now-unperformable law mean? — by holding that the obligation itself lapsed with the institutional site, and that study functions purely as historical and cultural preservation rather than as a substitute fulfillment.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion and comparative-law scholars (outside any beneficiary community) corroborate that many post-destruction communities did treat site-dependent obligations as lapsed rather than transmuted, supporting this reading as a live historical pattern rather than a self-serving rationalization. Adherents of the sibling readings (study_as_performance, messianic_suspension, performance_only) dispute the status question directly, holding the obligation persists in some form — the dispute is genuine and unresolved across the tradition, not settled by this reading alone.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).
:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03-0.05) because, under this reading, no party is compelled to study, contribute, or bear cost — the practice is voluntary cultural transmission. Suppression is low (0.05) because no coercive apparatus enforces study or bars its abandonment. Theater ratio is modest (0.15-0.20) reflecting that some communal/liturgical performance of study retains ceremonial trappings even though nothing normative rides on it; this declines slightly over the interval as the reading becomes more settled as a purely cultural framing. Accessibility collapse and resistance are both low, consistent with a genuinely optional, non-binding practice rather than a constructed constraint defended against alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Textual scholars and cultural memory communities are declared beneficiaries because they receive scholarly and communal value from the practice without any offsetting cost imposed on them by the reading itself — the constraint subsidizes their cultural continuity. No victims are declared because under this reading, by definition, nothing is extracted from anyone: the obligation has exited constraint space. Adherents of rival readings are excluded rather than victimized — they are unaffected in any material sense by this reading's operation but structurally shut out of having their contrary claim adjudicated within it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is a clean case where a formerly binding constraint (Temple sacrifice law, historically extractive of resources, animals, priestly labor, and pilgrimage effort) has its mandate genuinely resolved rather than merely relabeled: the founding problem (how to fulfill an obligation requiring a destroyed institutional site) is treated as dissolved rather than persisting in disguised form. This prevents mislabeling the archival practice as either a hidden Snare (no one is coerced into it) or a disguised Tangled Rope (no beneficiary captures rents from others' compliance) — the low metrics are earned by an actual absence of coercive structure, not by definitional sleight of hand.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_ambiguity,
    'Is the archival_preservation reading the historically dominant position within the tradition, or a minority/modernist reading contested by the majority who hold one of the persistence readings (study_as_performance, performance_only, messianic_suspension)?',
    'Survey of authoritative legal/halakhic literature across historical periods and denominations to establish relative prevalence and canonical status of each reading; compare liturgical practice (whether communities recite the passages as obligation-fulfilling study versus historical remembrance).',
    'If archival_preservation is a minority reading, its zero-extractiveness classification describes only a subset of the practicing community''s actual relationship to the text, and most adherents'' operative constraint is better modeled by a sibling story with non-zero extraction and normative force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_ambiguity, empirical, 'Whether this reading is majority or minority within the tradition.').

omega_variable(
    voluntary_versus_internalized_obligation,
    'Even under the archival_preservation reading''s explicit doctrine, do practicing communities experience their study practice as psychologically obligatory (internalized normative pressure) despite the formal absence of binding law?',
    'Ethnographic or interview-based study of communities practicing this reading: do participants report guilt, social pressure, or identity-based compulsion around continuing study, distinct from the doctrine''s own claim of voluntariness?',
    'If internalized obligation is substantial, the authored near-zero suppression and extractiveness understate the lived constraint — the doctrine''s non-normativity claim would be formally true but experientially incomplete, suggesting a partially internalized suppression mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_versus_internalized_obligation, empirical, 'Gap between doctrinal non-obligation and lived communal pressure to continue study.').

omega_variable(
    kernel_framing_under_determination,
    'Is the ''kernel'' here best framed as the sacrifice law text itself (fixed_text framing) or as the broader legitimacy claim about what post-Temple Judaism requires for continuity (a framing above the text, concerning tradition-survival itself)?',
    'Compare classification outcomes under each framing: text-as-kernel treats disputes as interpretive disagreements over a fixed corpus; legitimacy-claim-as-kernel treats disputes as competing theories of what makes post-Temple religious life valid at all.',
    'Under the text-as-kernel framing (adopted here), the four readings are interpretive positions on a stable corpus, and cs_pattern analysis treats authority_grounding as lineage/practice. Under the legitimacy-claim framing, the same four readings would be treated as competing foundational theories, potentially shifting authority_grounding toward distributed and increasing cross-reading foreclosure signal (since foundational theories of legitimacy are more likely to logically exclude each other than interpretive readings of a text are).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Alternative kernel framings (fixed text vs. legitimacy claim) that could shift classification signals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sacr_tr_t8, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 8, 0.18).
narrative_ontology:measurement(sacr_tr_t16, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 16, 0.17).
narrative_ontology:measurement(sacr_tr_t24, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 24, 0.16).
narrative_ontology:measurement(sacr_tr_t32, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 32, 0.15).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t8, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 8, 0.04).
narrative_ontology:measurement(sacr_be_t16, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 16, 0.04).
narrative_ontology:measurement(sacr_be_t24, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 24, 0.03).
narrative_ontology:measurement(sacr_be_t32, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 32, 0.03).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 40, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__archival_preservation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__archival_preservation, 0.05).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the sacrifice_obligation_continuity kernel, each authored as a separate constraint per the epsilon-invariance principle: archival_preservation (this story, ~0 extraction, obligation exits constraint space), study_as_performance (obligation persists through textual engagement, non-trivial extraction from adherents' study-time and identity commitment), performance_only (obligation persists as unfulfilled future duty, study is mere preparation), and messianic_suspension (obligation suspended pending restoration, readiness-maintenance extraction). All four are linked bidirectionally via affects_constraints since a shift in one community's dominant reading exerts legitimacy pressure on adherents of the others (e.g., growing archival_preservation adoption reduces social pressure supporting the persistence readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
