% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Substitution Archive: Prayer and Study Replace Sacrifice
 *   domain: religious/committal system
 *
 * SUMMARY:
 *   The Kodashim (orders of the Mishnah and Talmud dealing with sacrifice and
 *   Temple service) constitute a vast textual archive documenting sacrificial
 *   law in exhaustive detail. After the destruction of the Second Temple in
 *   70 CE, actual sacrifice became impossible. Rabbinic authorities
 *   articulated a substitution doctrine: prayer and Torah study replace
 *   sacrifice; engaging with the Kodashim texts is itself a form of spiritual
 *   service that fulfills the obligation. This constraint story models the
 *   substitution doctrine as a **committed reading** of the contested
 *   Kodashim kernel—one reading among three coherent but mutually exclusive
 *   interpretive positions. The substitution_archive reading claims that
 *   Kodashim is a memorial archive documenting what was superseded, not a
 *   kernel that remains occupied through study or awaiting restoration. This
 *   reading is extractive because it denies legitimacy to those who seek to
 *   restore actual sacrifice, presenting the substitution as permanent and
 *   continuity as self-evident, while obscuring that a replacement has
 *   occurred. The constraint is claimed as Tangled Rope: genuine coordination
 *   function (diaspora continuity, knowledge preservation) coupled with
 *   asymmetric extraction (denial of restoration legitimacy to those who
 *   experience it as a loss).
 *
 * KEY AGENTS:
 *   - rabbinic_study_institutions: Institutional agenda-setter; benefits from interpretive monopoly; claims study IS the substitute
 *   - restoration_seeking_practitioners: Moderate-power payer; identity-locked; told their aspiration is obsolete
 *   - diaspora_jewish_communities: Organized beneficiary; benefited from diaspora-viable substitution; also bear diffuse costs of foreclosed sovereignty discussion
 *   - textual_scholars_and_commentators: Powerful beneficiary; arbitrage-mobile; benefit from intellectual richness of archive without commitment to restoration question
 *   - analytical_observer: Sees the frame-dependency; witnesses whether substitution is claimed as continuity or as replacement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.58).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.62).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Substitution Archive: Prayer and Study Replace Sacrifice").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/committal system").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, 'fb7fd961-f977-46e8-9d29-b19a128ade9d').
narrative_ontology:cs_kernel_codification('fb7fd961-f977-46e8-9d29-b19a128ade9d', fixed_text).
narrative_ontology:cs_authority_grounding('fb7fd961-f977-46e8-9d29-b19a128ade9d', lineage).
narrative_ontology:cs_interpretation_layer_present('fb7fd961-f977-46e8-9d29-b19a128ade9d').
narrative_ontology:cs_reading_relation('fb7fd961-f977-46e8-9d29-b19a128ade9d', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('fb7fd961-f977-46e8-9d29-b19a128ade9d', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_axiom('fb7fd961-f977-46e8-9d29-b19a128ade9d', foundational, substitution_permanent).
narrative_ontology:cs_axiom_status(substitution_permanent, holdable).
narrative_ontology:cs_axiom_grounding('fb7fd961-f977-46e8-9d29-b19a128ade9d', substitution_permanent, deontological).
narrative_ontology:cs_axiom('fb7fd961-f977-46e8-9d29-b19a128ade9d', foundational, study_memorial_not_performance).
narrative_ontology:cs_axiom_status(study_memorial_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('fb7fd961-f977-46e8-9d29-b19a128ade9d', study_memorial_not_performance, deontological).
narrative_ontology:cs_reference_frame('fb7fd961-f977-46e8-9d29-b19a128ade9d', temple_destruction_necessitates_permanent_substitution).
narrative_ontology:cs_drift_state('fb7fd961-f977-46e8-9d29-b19a128ade9d', contemporary_restoration_movements, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fb7fd961-f977-46e8-9d29-b19a128ade9d', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, restoration_seeking_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, textual_scholars_and_commentators).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, continuity_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, temple_obsolescence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic academies and textual authorities (including contemporary yeshivas, Talmudic scholars, and halakhic decisors) set and maintain the interpretive framework. They claim that studying Kodashim (the sacrificial laws) is itself a form of spiritual service that substitutes for physical sacrifice. They benefit from this doctrine by securing exclusive interpretive authority over a vast and complex body of law, positioning themselves as the necessary mediators between the tradition and the community. They have no cost to maintaining the doctrine and face no real exit pressure—they can shift interpretive strategies while preserving institutional control.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_study_institutions, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_study_institutions, beneficiary).

% Individuals and movements seeking to restore animal sacrifice or a Temple-centered sacrificial practice in the present day. They view the substitution doctrine as a temporary historical accommodation, not a permanent theological truth. The constraint denies them legitimacy to pursue restoration: they are labeled heretical, premature, or theologically confused if they advocate for actual sacrifice. Their entire religious identity is bound to the aspiration for restoration, making exit from the Jewish community the only way to escape the constraint at the cost of losing that identity. They bear the cost of delegitimation while having no say in the doctrine that delegitimizes them.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, restoration_seeking_practitioners, payer,
    moderate, biographical, identity_locked, global).

% Jewish communities living outside the Land of Israel and lacking access to a functioning Temple have benefited significantly from the substitution doctrine. It enables them to maintain halakhic continuity and spiritual practice through prayer and study alone, without geographic dependence on Jerusalem or access to sacrificial facilities. The substitution framework allows diaspora Jews to experience their tradition as complete and ongoing, not truncated or incomplete. However, they also bear diffuse costs: the substitution doctrine constrains public discourse about Jewish territorial sovereignty, Temple restoration, or political solutions that might enable sacrificial practice, effectively foreclosing conversations that some community members might find spiritually or politically important.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Scholars across traditions (rabbinic, academic historical, comparative religion, Jewish philosophy) benefit intellectually from Kodashim as a rich and intricate archive. The texts support sophisticated legal reasoning, exegetical innovation, and historical-textual analysis. These scholars have high mobility in how they engage the corpus: they can approach the texts as intellectual monuments, as historical sources, as living legal documents, or as windows into ancient practice. They can hold multiple interpretive positions simultaneously across different scholarly contexts without committing to any single reading's institutional claim.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, textual_scholars_and_commentators, beneficiary,
    powerful, generational, arbitrage, global).

% Christian and Islamic scholars and authorities who might interpret the Kodashim substitution framework through their own theological lenses (replacement theology, parallel developments in Islamic jurisprudence on ritual sacrifice) are structurally excluded from the conversation about what the Kodashim corpus means and what the substitution doctrine signifies. They are not invited into the rabbinic interpretive community. Their exclusion preserves Jewish interpretive sovereignty and prevents alternative readings of the substitution from gaining institutional legitimacy within Jewish discourse.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, religious_authorities_other_traditions, excluded,
    institutional, generational, trapped, global).

% Scholars of comparative religion, commitment systems, and legal history observe the structural claim and frame-dependency. The observer measures whether the substitution doctrine genuinely claims continuity with what sacrifice accomplished, or whether it obscures a replacement by calling study a substitute. The observer witnesses the frame-dependency: the doctrine's legitimacy rests on whether one accepts that intellectual engagement with sacrificial law can functionally replace the physical performance, and for whom. No seat has to convince the observer—the observer simply records the structure.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_study_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables diaspora Jewish communities to maintain connection to halakhic law and spiritual continuity without access to a functional Temple or sacrificial facilities. Allows the vast body of sacrificial law to be preserved, transmitted, and continuously reinterpreted across generations without depending on ritual performance at a single geographic site. Solves the coordination problem: 'How do we sustain Jewish law and identity after the institutional structure (Temple) that many laws depend on has been destroyed?'
% TRANSFER_FUNCTION: Transfers the right to determine 'what counts as fulfilling this obligation' from individual practitioners or alternative movements seeking restoration, to rabbinic institutions whose authority is grounded in text-study and commentary. Moves the answer to 'How do we serve God through sacrifice?' from performance-based (actually performing the ritual) to text-intellectual (studying the law). The extraction occurs because this transfer is presented as a permanent substitution (continuity claim) while actually denying legitimacy to those who believe restoration is the true obligation (replacement reality).
% ABSENT_VOICES: Restoration-seeking practitioners are present in the constraint story but are structurally delegitimized and excluded from setting the interpretive frame. Historical voices of Jews who maintained non-rabbinic sacrificial or Temple-centered practices (Samaritans, Karaites, Temple movement adherents) are erased or minimized in the rabbinic archive itself. Voices from other religious traditions that might offer alternative readings of how substitution works (Christian supersessionism, Islamic jurisprudence on ritual replacement) are excluded from the interpretive community by design.
% DISAPPEARANCE_RATIONALE: If the substitution framework disappeared overnight, Jewish practice would bifurcate openly. Communities committed to the diaspora model would likely continue prayer and study, but would need to develop alternative theological narratives for why (no longer claiming these ARE sacrifice). Restoration-seeking practitioners would immediately become visible as a legitimate movement within Judaism rather than a heretical fringe. The rabbinic institutional monopoly on interpreting Kodashim would weaken; multiple readings would emerge in public view. Some communities might pursue Temple restoration or alternative sacrificial forms. The unified narrative of continuity would fracture into acknowledged plurality.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, animal sacrifice became institutionally impossible for diaspora communities and eventually for communities in the Land of Israel without a Temple. A mechanism was needed to (1) allow diaspora Jews to remain halakhically engaged without geographic dependence on Jerusalem, (2) preserve the knowledge of how sacrifices were performed so that if the Temple were rebuilt, the tradition would not be lost, and (3) maintain the spiritual connection to the sacrificial system that was central to Torah and Jewish identity.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic and medieval sources (Mishnah, Babylonian Talmud, later halakhic authorities like Maimonides) explicitly cite Temple destruction as the cause and cite the need to preserve knowledge and maintain diaspora continuity as the reason for continued Kodashim study. Early modern and modern scholars across traditions corroborate the historical fact that substitution was the rabbinic response to Temple loss. However, restoration-seeking practitioners contest whether the founding problem is 'live' in the relevant sense: they argue it is merely the historical occasion for substitution, not a permanent justification. Modern scholars outside the committed rabbinic framework (academic historians, comparative religionists) note that no neutral external authority has arbitrated the claim that study-as-substitute is permanent; the doctrine persists because rabbinic institutions have the power to enforce it, not because it has been independently validated. Restoration movements cite this lack of corroboration from outside the benefiting parties as evidence that the doctrine is institutional ideology, not eternal law.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, contested).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.58 (moderate-high) because the substitution doctrine extracts legitimacy from restoration-seeking practitioners while claiming to preserve continuity. It is not pure extraction (snare-grade) because the coordination function is genuine: diaspora communities DO benefit from being able to engage with the law through study. Suppression is 0.62 because the constraint's persistence depends on actively excluding or delegitimizing alternative readings (restoration-seeking is labeled heretical, premature, or Karaite). Theater ratio is 0.48 because roughly half the energy in Kodashim study goes to preserving the archive's knowledge (genuine function) and half to performing the narrative that study IS sacrifice (theatrical maintenance of the continuity claim). Measurements run across 2000 years on a shared time grid. Early trajectory (0-500): extractiveness and theater ratio rise as the substitution doctrine becomes formalized and institutionalized (Tannaitic to early Amoraic period through medieval codification). Middle trajectory (500-1500): stabilization; extractiveness rises slightly to 0.60 as the doctrine becomes unquestioned mainstream (medieval and early modern yeshiva institutionalization). Late trajectory (1500-2000): slight decline in extractiveness (0.60 → 0.58) and theater ratio (0.51 → 0.48) as modern scholarship and restoration movements begin openly contesting the doctrine; suppression also declines (0.65 → 0.62) as the monopoly on interpretation weakens.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic_study_institutions seat and the restoration_seeking_practitioners seat should compute dramatically differently. From the institutional seat: the constraint is genuine coordination + intellectual-spiritual engagement (rope-grade, low extraction). From the restoration-seeking seat: the constraint is experienced as denial of legitimacy and foreclosure of a live option (snare-grade, high extraction, high identity-lock). The analytical observer sees both as structurally true: the constraint simultaneously coordinates diaspora practice AND delegates restoration to the status of heresy. The engine computes this divergence from directionality: the institutional seat has d ≈ 0.2 (beneficiary, sets the rules); the restoration-seeking seat has d ≈ 0.85 (target, identity-locked, constrained exit, told their desire is obsolete).
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic institutions are the structural beneficiary: they hold interpretive authority, collect the benefit of institutional prestige and textual monopoly, and face no exit cost for maintaining the substitution doctrine—they can shift interpretive strategies while remaining in control. Restoration-seeking practitioners are the structural victims: they bear the cost of delegitimation, face identity-lock (their entire religious identity is bound to the practice they are told is forbidden), and have constrained exit (leaving the Jewish community is the only way to pursue restoration outside the constraint, at catastrophic identity cost). Diaspora communities and scholars occupy intermediate positions: beneficiaries of the coordination (diaspora viability, textual richness) but also bearing diffuse costs (inability to openly discuss Temple restoration as a live political/religious option without being marked as heterodox). The override would be minimal here; the structural derivation from beneficiary/victim + power + exit captures the directionality accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('how to preserve sacrificial law after Temple destruction') was genuinely live for the first few centuries of Diaspora (70–500 CE). By 1500, the problem had transformed: the question was no longer 'how do we remember what to do if sacrifice returns?' but 'how do we maintain institutional control over the interpretation of sacrifice?' The substitution doctrine persists past the point where it solves the founding problem because it has become the foundation of rabbinic institutional authority. Restoration-seeking movements (Karaites, Samaritans, modern Temple-movement activists) have repeatedly challenged the doctrine, showing that the founding problem is contestable: some actors believe restoration SHOULD happen and the doctrine is not a solution but an obstacle. The constraint shows signs of mandatrophy: it solved a real coordination problem (diaspora continuity) but now persists largely because the institutions that benefit from it have the power to enforce the substitution narrative. The doctrine is not dead (founding_problem_status = live because restoration-seeking continues), but it is increasingly theatrical—modern scholarship openly acknowledges the substitution as a historical accommodation while the rabbinic mainstream continues to claim it as permanent substitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_vs_replacement_semantics,
    'Does the rabbinic doctrine genuinely claim substitution (a functional replacement that fulfills the same obligation), or does it claim replacement (a historically necessary accommodation that denies the original obligation''s current force)?',
    'Philological and structural analysis of early rabbinic texts (Mishnah, Tosefta, Talmud) on the relationship between sacrifice and prayer/study. If early sources describe prayer as ''in place of'' (tapuach) sacrifice with full force, that supports substitution. If they describe it as temporary accommodation until restoration, that supports replacement framing. Modern restoration movements'' cited sources for their reading would provide external corroboration.',
    'If truly substitution, the constraint coordinates diaspora practice and is legitimately ongoing. If truly replacement, the constraint is extractive—it denies restoration-seeking practitioners the right to their own reading and claims permanence for an accommodation. This omega determines whether the tangled_rope classification is accurate or whether it should be higher (snare-grade).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_replacement_semantics, conceptual, 'Whether the substitution claim is doctrinally honest or a cover for permanent replacement.').

omega_variable(
    identity_locked_restoration_exit,
    'For restoration-seeking practitioners, how internalized is the suppression (they have absorbed the doctrine and believe restoration is forbidden) versus structural (external enforcement by the rabbinic institution)?',
    'Post-exit suppression trajectory: if a practitioner leaves the mainstream rabbinic community and joins a restoration movement, does the suppression (belief that restoration is forbidden) persist? If it persists, the suppression is partially internalized; if it dissolves, the suppression is mostly structural. Survey data from people who have left mainstream Judaism for Samaritan, Karaite, or modern Temple-movement communities would provide evidence.',
    'If suppression is heavily internalized, the effective suppression on restoration-seeking practitioners is higher than the structural 0.62 measure suggests—they carry the constraint with them. If suppression is mostly structural, the constraint would loosen rapidly if institutional enforcement weakened. This modifies how we understand the identity-lock on the restoration-seeking seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_restoration_exit, empirical, 'Extent to which restoration-seeking suppression is internalized versus structurally enforced.').

omega_variable(
    study_performance_equivalence_contested,
    'Is the claim that ''studying Kodashim IS the performance of the mitzvah'' a reading this constraint endorses, or does the substitution_archive reading explicitly deny that study-as-performance is sufficient?',
    'This is a committer-framing question: what distinguishes the substitution_archive reading from the study_as_exercise sibling reading? If substitution_archive claims study is merely a memorial/archive function (not performance), then the two readings foreclose each other. If both claim study is a form of engagement, they coexist. Examine early rabbinic sources on whether study is described as ''doing'' the mitzvah or ''remembering'' the mitzvah.',
    'If the readings foreclose, they cannot coexist in a single framework, and reading_relations should be ''forecloses''. If they coexist, the relation is ''coexists_with''. This affects the terminal attractor computation for the Kodashim family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_performance_equivalence_contested, conceptual, 'Whether study-as-memorial and study-as-performance are logically compatible within the substitution_archive reading.').

omega_variable(
    rabbinic_institutional_capture,
    'To what extent has the substitution doctrine become self-serving institutional ideology? Does it now persist because it genuinely solves the founding problem (diaspora continuity), or because rabbinic institutions benefit from the interpretive monopoly?',
    'Compare extractiveness trajectory: if extractiveness rose as the doctrine became institutionalized (middle period) and remains high even as the founding problem became less acute, that suggests institutional capture. If extractiveness tracks with active restoration threats (rising when challenges emerge, falling when they recede), that suggests the doctrine remains functionally responsive to the founding problem.',
    'If capture is substantial, the mandatrophy analysis strengthens: the constraint persists past its founding function. The theater_ratio would be reinterpreted as increasingly performative maintenance of the institutional authority structure, not of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_institutional_capture, empirical, 'Whether the substitution doctrine persists due to genuine coordination need or institutional rent-capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(koda_tr_t0, projected).
narrative_ontology:measurement(koda_tr_t250, kodashim_corpus__substitution_archive, theater_ratio, 250, 0.3).
narrative_ontology:measurement_basis(koda_tr_t250, projected).
narrative_ontology:measurement(koda_tr_t500, kodashim_corpus__substitution_archive, theater_ratio, 500, 0.38).
narrative_ontology:measurement_basis(koda_tr_t500, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__substitution_archive, theater_ratio, 1000, 0.48).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.51).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t2000, kodashim_corpus__substitution_archive, theater_ratio, 2000, 0.48).
narrative_ontology:measurement_basis(koda_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(koda_be_t0, projected).
narrative_ontology:measurement(koda_be_t250, kodashim_corpus__substitution_archive, base_extractiveness, 250, 0.45).
narrative_ontology:measurement_basis(koda_be_t250, projected).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__substitution_archive, base_extractiveness, 500, 0.52).
narrative_ontology:measurement_basis(koda_be_t500, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.6).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__substitution_archive, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(koda_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(koda_su_t0, projected).
narrative_ontology:measurement(koda_su_t250, kodashim_corpus__substitution_archive, suppression_requirement, 250, 0.5).
narrative_ontology:measurement_basis(koda_su_t250, projected).
narrative_ontology:measurement(koda_su_t500, kodashim_corpus__substitution_archive, suppression_requirement, 500, 0.58).
narrative_ontology:measurement_basis(koda_su_t500, observed).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__substitution_archive, suppression_requirement, 1000, 0.62).
narrative_ontology:measurement_basis(koda_su_t1000, observed).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.65).
narrative_ontology:measurement_basis(koda_su_t1500, observed).
narrative_ontology:measurement(koda_su_t2000, kodashim_corpus__substitution_archive, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(koda_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__substitution_archive, 0.12).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).

% DUAL FORMULATION NOTE:
% The Kodashim corpus kernel admits three structurally distinct readings, each instantiating a different constraint. This story (substitution_archive) models the reading that prayer and study replaced sacrifice, with Kodashim as memorial archive. Sibling stories model: (1) performance_only—Kodashim as blueprint awaiting restoration (different ε, different victims, different type); (2) study_as_exercise—study as continuous performance of the mitzvah (different ε, different beneficiary structure, likely Rope type). All three readings are live in contemporary Jewish discourse; they are not versions of the same constraint, but three separate constraints with different classifications. Linked via network.affects_constraints as members of the Kodashim constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
