% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation Requires Physical Performance (Performance-Only Reading)
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint instantiates the performance-only reading of the
 *   sacrifice obligation kernel: the claim that the biblical and rabbinic
 *   commandments to bring korbanot are fulfilled ONLY by physical, ritual
 *   performance at a functioning Temple altar, and that study of the relevant
 *   laws, however rigorous, is preparatory scholarship rather than an
 *   alternative mode of discharge. Since the Temple's destruction roughly
 *   1,900 years ago, this reading holds the obligation as commanded but
 *   permanently unperformed — a structural gap between divine command and
 *   human capacity that no amount of substitute activity closes. This is one
 *   of four sibling readings of the same kernel; the
 *   study_as_exercise_reading holds that intellectual engagement itself
 *   occupies the mitzvah, the messianic_suspension_reading holds the
 *   obligation is divinely suspended rather than merely unperformed, and the
 *   symbolic_archive_reading treats the sacrificial corpus as
 *   cultural-historical memory making no live halakhic claim. Each sibling is
 *   a separate constraint with its own ε; this reading's ε is authored for
 *   what THIS reading's own lights take the standing arrangement (the
 *   unfulfilled but still-binding obligation) to be, not for any resolution
 *   the reading might imagine.
 *
 * KEY AGENTS:
 *   - observant_jewish_community: Primary bearer of the obligation (powerless/trapped) — carries the unfulfilled mitzvah across generations with no available exit
 *   - halakhic_stringency_scholarship: Structural beneficiary (institutional/arbitrage) — interpretive authority sustained by the unresolved gap
 *   - halakhic_decisors: Agenda-setters (institutional/identity_locked) — transmit and enforce the reading through codified law; their own professional identity is constituted by faithful transmission
 *   - rebuilt_temple_authority: Structural absence (non-agent) — the missing apparatus whose non-existence makes performance impossible
 *   - study_as_exercise_advocates: Excluded sibling-reading holders — foreclosed from authority within this reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.81).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.35).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation Requires Physical Performance (Performance-Only Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '79ef721d-3e5c-4dec-905d-0d51a5b6a918').
narrative_ontology:cs_kernel_codification('79ef721d-3e5c-4dec-905d-0d51a5b6a918', fixed_text).
narrative_ontology:cs_authority_grounding('79ef721d-3e5c-4dec-905d-0d51a5b6a918', lineage).
narrative_ontology:cs_interpretation_layer_present('79ef721d-3e5c-4dec-905d-0d51a5b6a918').
narrative_ontology:cs_reading_relation('79ef721d-3e5c-4dec-905d-0d51a5b6a918', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('79ef721d-3e5c-4dec-905d-0d51a5b6a918', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('79ef721d-3e5c-4dec-905d-0d51a5b6a918', sacrifice_obligation_kernel__symbolic_archive_reading, influences).
narrative_ontology:cs_axiom('79ef721d-3e5c-4dec-905d-0d51a5b6a918', foundational, fulfillment_requires_maaseh).
narrative_ontology:cs_axiom_status(fulfillment_requires_maaseh, holdable).
narrative_ontology:cs_axiom_grounding('79ef721d-3e5c-4dec-905d-0d51a5b6a918', fulfillment_requires_maaseh, conventional).
narrative_ontology:cs_axiom('79ef721d-3e5c-4dec-905d-0d51a5b6a918', secondary, study_is_preparatory_not_constitutive).
narrative_ontology:cs_axiom_status(study_is_preparatory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('79ef721d-3e5c-4dec-905d-0d51a5b6a918', study_is_preparatory_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('79ef721d-3e5c-4dec-905d-0d51a5b6a918', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('79ef721d-3e5c-4dec-905d-0d51a5b6a918', post_destruction_rabbinic_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('79ef721d-3e5c-4dec-905d-0d51a5b6a918', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, halakhic_stringency_scholarship).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, observant_jewish_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, temple_centrality_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, mitzvah_requires_maaseh_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remains formally commanded to bring korbanot (sacrifices) at fixed times and occasions, yet has had no functioning Temple or altar for roughly 1,900 years. Under this reading, no amount of study, prayer, or liturgical recitation discharges the obligation itself — only physical performance does, and physical performance is structurally impossible without a rebuilt Temple. The community carries the unfulfilled mitzvah as a standing gap between command and capacity, generation after generation, with no exit available: one cannot renounce the obligation, only await conditions that would allow its performance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, observant_jewish_community, payer,
    powerless, civilizational, trapped, global).

% Rabbinic authorities and institutions whose interpretive authority is sustained by maintaining the sharp performance/study distinction — the harder the line between substitute and fulfillment, the more indispensable the ongoing work of legal analysis, contingency planning (e.g. contemporary Temple-readiness scholarship), and pastoral guidance on how to relate to an obligation one cannot perform. This scholarship gains standing and continuity from the obligation remaining unresolved rather than dissolved; it does not extract resources directly but occupies the interpretive space the unfulfilled gap keeps open.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_stringency_scholarship, beneficiary,
    institutional, civilizational, arbitrage, global).

% The Temple priesthood and altar apparatus that would need to exist for the obligation to be performable at all. This is a non-agent structural absence rather than a party with a voice — its non-existence is precisely what makes the obligation's performance impossible under this reading, and no living stakeholder can substitute for it.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, rebuilt_temple_authority, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__performance_only_reading, rebuilt_temple_authority).

% Scholars and communities who hold that intellectual engagement with the laws of sacrifice constitutes genuine occupation of the mitzvah (the sibling study_as_exercise_reading). They are excluded from settling the matter within this reading's framework: this reading treats their position as a category error, not merely a competing emphasis, because it collapses the performance/preparation distinction this reading holds as foundational.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, study_as_exercise_advocates, excluded,
    organized, generational, constrained, global).

% Poskim (halakhic decisors) across generations who have transmitted and enforced the performance-only reading through codified law (Rambam's Sefer Avodah, later codes) and communal practice. Their authority is constituted by faithful transmission of this reading; abandoning the performance requirement would require repudiating centuries of their own tradition's rulings, which is not a live option from inside their professional and religious identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_decisors, agenda_setter,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The performance-only reading coordinates a shared, stable understanding across generations of exactly what would count as fulfilling the sacrifice commandments, preventing drift toward ad hoc or convenient substitutes and preserving a coherent legal category (maaseh, physical act) that applies uniformly across the entire corpus of commandments requiring action.
% TRANSFER_FUNCTION: The reading transfers interpretive authority and continuity-of-tradition value to the community of halakhic decisors and stringency scholarship, while transferring the weight of an unfulfillable civilizational-scale obligation onto the entire observant community, who bear the standing spiritual and legal gap between command and capacity indefinitely.
% ABSENT_VOICES: Study_as_exercise advocates and symbolic_archive readers would object that treating the obligation as strictly unfulfilled for nineteen centuries produces a permanent, structurally guaranteed failure state with no coordination benefit to the people who bear it; they are present in the broader tradition but excluded from authority within this reading's own framework, which treats their reframing as dissolving rather than resolving the obligation.
% DISAPPEARANCE_RATIONALE: If this reading's performance requirement were abandoned in favor of, say, the study_as_exercise reading, the lived religious experience of the obligation would change immediately: study and liturgical recitation would become sufficient occupation of the mitzvah rather than mere preparation, halakhic scholarship's stringency function would lose its rationale, and the psychological and communal weight of carrying an unfulfilled command would lift. The Temple-restoration hope structure, contemporary korban-readiness institutions, and a significant portion of the liturgy's theological urgency are organized around this reading remaining authoritative.
% FOUNDING_PROBLEM: The reading was built to preserve the legal category of maaseh (physical performance) as the standard for fulfilling action-commandments, ensuring that sacrificial law retained the same rigor as other commandments requiring bodily action, rather than allowing the unique catastrophe of Temple destruction to quietly redefine what 'fulfillment' means across all of halakha.
% FOUNDING_PROBLEM_CORROBORATION: Classical decisors (Rambam, codifiers of Sefer Avodah) attest the problem is live and the category must be preserved exactly. Historians of Jewish law and comparative religion scholars, writing from outside the halakhic authority structure, note that the strict performance requirement was itself a rabbinic-era construction responding to Temple loss, not a self-evidently necessary reading of the biblical text — supporting a contested rather than settled status for the founding problem's continued force.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because this reading structurally guarantees 1,900 years of continuous, unresolvable non-fulfillment for an entire population commanded to perform an act it categorically cannot perform — the gap itself is the extracted cost, borne diffusely and cumulatively rather than by any single transactional exchange. Suppression is moderate (0.35) rather than high: nothing coercively prevents individuals from adopting a different reading privately, but communal and institutional pressure (via codified law, liturgy, and rabbinic authority) makes departure from the performance-only framework costly within observant community life. Theater ratio rises over the interval (0.20 to 0.42) as more institutional energy over the centuries has gone into elaborate contingency scholarship (Temple-readiness texts, priestly genealogy preservation, ash-mixture and vessel specifications) that performs continued relevance to an obligation with zero prospect of near-term performability — a growing proxy activity substituting for the thing itself. Accessibility collapse is high (0.72): once a community accepts this reading's premises, alternative framings (study-as-fulfillment, suspension, symbolic archive) become very hard to adopt without appearing to abandon halakhic seriousness. Resistance is moderate (0.55): sibling readings persist as live minority and competing positions precisely because the cost of the performance-only reading is felt and periodically challenged.
 *
 * PERSPECTIVAL GAP:
 *   From the observant community's seat, this reading computes as extraction: a commanded act rendered permanently impossible, with the weight of noncompliance borne indefinitely and no agent to seek remedy from. From the halakhic decisors' seat, the same structure computes as faithful coordination — preserving the coherence and rigor of the entire category of action-commandments against the temptation to redefine 'fulfillment' downward after catastrophe. Neither seat is wrong about its own experience; the engine's per-seat computation is expected to diverge sharply here, which is the point of authoring the story rather than resolving it in commentary.
 *
 * DIRECTIONALITY LOGIC:
 *   The observant community is declared victim/payer: they are commanded, cannot perform, and cannot exit the obligation (trapped) — this pushes their derived directionality toward the full-target end. Halakhic stringency scholarship is declared beneficiary: its institutional and interpretive continuity is sustained by the unresolved gap remaining open, and its arbitrage-grade exit options (it can adapt its scholarly emphasis without existential risk) place it near the beneficiary end. Halakhic decisors are agenda-setters whose exit is identity-locked rather than arbitrage-grade — they administer the reading but cannot easily abandon it without repudiating their own tradition's authority, which is why they are not modeled identically to the stringency-scholarship beneficiary seat despite institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving the legal category of maaseh/physical performance against post-catastrophe redefinition) is contested rather than resolved: classical decisors hold it fully live, while historians and comparative scholars outside the benefiting authority structure note the strict reading was itself a period-specific rabbinic construction. This mismatch (a genealogy narrative asserted mostly from inside the interpretive tradition, contested from outside) is exactly the kind of signal the R5 corroboration check exists to surface — it does not resolve whether the reading is warranted, but it flags that the founding-problem narrative should not be taken as self-certifying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_vs_study_kernel_disagreement_locus,
    'Is the disagreement between this reading and study_as_exercise_reading located in the definition of ''maaseh'' (physical act) itself, or in whether ''maaseh'' is the only category that can discharge a mitzvah — i.e., is the contest about what counts as an act, or about whether acts are the only currency of fulfillment?',
    'Close textual analysis of the earliest sources distinguishing hirhur (mental intention/study) from maaseh across the broader corpus of action-commandments, tracing whether any pre-Temple-destruction source already treated study as potentially sufficient for any comparable commandment.',
    'If the disagreement is purely definitional (what counts as an act), the readings might be partially reconcilable via expanded definitions of performance. If it is about whether acts are the exclusive currency of fulfillment, the readings are genuinely incompatible at the axiom level, supporting a stronger relational classification (closer to foreclosure) than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_study_kernel_disagreement_locus, conceptual, 'Where exactly the performance-only and study-as-exercise readings diverge structurally.').

omega_variable(
    extraction_without_extractor,
    'Can a constraint be genuinely extractive (high ε) when there is no identifiable agent collecting the extracted value — i.e., is the 1,900-year unfulfilled-obligation gap a case of extraction at all, or is it better modeled as pure structural impossibility with no extraction dynamic, only cost?',
    'Compare against other constraint stories with declared ''diffuse'' or absent gain_flow to see whether the framework''s extraction concept coherently applies to cost-without-collector cases, or whether this case is better modeled with a near-zero beneficiary weight and cost treated as a separate axis.',
    'If extraction requires a collector, this reading''s high ε may be miscategorized and the constraint may be closer to a piton (inertial, no concentrated beneficiary) than a tangled_rope — though the coordination function (preserving legal category rigor) and the concentrated interpretive-authority beneficiary (halakhic stringency scholarship) argue for retaining tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_without_extractor, conceptual, 'Whether extraction without a collecting agent is coherently modeled by this constraint''s classification.').

omega_variable(
    temple_restoration_counterfactual,
    'If a functioning Temple and altar were restored, would this reading''s extractiveness immediately collapse to near zero (the obligation becomes performable and the gap closes), or would new extraction dynamics emerge around who controls access to performance?',
    'No empirical resolution is possible absent restoration; this remains a counterfactual assessable only through historical analogy to periods of functioning Temple worship and the power dynamics of priestly control over sacrificial access documented in that period.',
    'If restoration would simply resolve the gap, this reading''s extraction is purely a function of historical circumstance (Temple absence) rather than the reading''s own structure. If new extraction dynamics would emerge (priestly gatekeeping, access control), some of the currently measured ε may reflect an enduring structural feature of the performance-only framework rather than a contingent historical fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temple_restoration_counterfactual, empirical, 'Whether this reading''s high extractiveness is contingent on Temple absence or would persist under restoration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(sacr_tr_t0, projected).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 300, 0.26).
narrative_ontology:measurement_basis(sacr_tr_t300, projected).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 700, 0.32).
narrative_ontology:measurement_basis(sacr_tr_t700, projected).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1100, 0.36).
narrative_ontology:measurement_basis(sacr_tr_t1100, projected).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.39).
narrative_ontology:measurement_basis(sacr_tr_t1500, projected).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement_basis(sacr_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(sacr_be_t0, projected).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 300, 0.62).
narrative_ontology:measurement_basis(sacr_be_t300, projected).
narrative_ontology:measurement(sacr_be_t700, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 700, 0.68).
narrative_ontology:measurement_basis(sacr_be_t700, projected).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1100, 0.73).
narrative_ontology:measurement_basis(sacr_be_t1100, projected).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.77).
narrative_ontology:measurement_basis(sacr_be_t1500, projected).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1900, 0.81).
narrative_ontology:measurement_basis(sacr_be_t1900, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__performance_only_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the natural-language label 'the sacrifice obligation' (per the ε-invariance principle): performance_only_reading (this file, high ε, tangled_rope), study_as_exercise_reading (lower ε, likely rope — study genuinely occupies the mitzvah), messianic_suspension_reading (lower ε — obligation suspended rather than failed, changes the victim framing), and symbolic_archive_reading (near-zero ε — no live halakhic claim, closer to piton or rope depending on institutional framing). Each reading is authored as its own constraint with its own ε, beneficiaries, victims, and type; they share the kernel_id sacrifice_obligation_kernel and are linked here via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
