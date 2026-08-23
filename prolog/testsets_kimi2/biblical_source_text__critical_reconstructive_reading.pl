% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical Reconstructive Reading of Biblical Source Text
 *   domain: biblical studies/translation theory/religious authority
 *
 * SUMMARY:
 *   The biblical source text kernel is contested among three principal
 *   readings. This constraint story instantiates the critical reconstructive
 *   reading: the claim that historical recovery of a hypothetical original
 *   text must precede and govern both structural (formal-equivalence) and
 *   semantic (dynamic-equivalence) translation decisions. Within academic
 *   biblical studies, this methodological priority functions as disciplinary
 *   common sense, coordinating scholarly labor around shared text-critical
 *   foundations. For confessional communities bound to received textual
 *   traditionsâwhether the Byzantine text, the Textus Receptus, or
 *   magisterial ecclesial textsâthe same priority destabilizes the
 *   epistemic basis of worship, preaching, and identity. The constraint is
 *   actively enforced through seminary curricula, peer-review gatekeeping,
 *   and translation-agency standards that require eclectic-text
 *   justification. The divergence between academic beneficiary seats and
 *   confessional payer seats is the core structural feature of this reading.
 *
 * KEY AGENTS:
 *   - academic_biblical_scholarship: Primary beneficiary (institutional/analytical) â disciplinary authority and resource flows accrue to the guild that controls reconstruction.
 *   - confessional_communities: Primary target (organized/identity_locked) â received-text identity is destabilized by the priority of hypothetical originals.
 *   - translation_standards_bodies: Agenda setter (institutional/constrained) â enforces the text-critical priority in funded Bible translation globally.
 *   - independent_translators: Secondary target (moderate/constrained) â must master and defer to critical apparatus to achieve academic or agency legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.55).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.48).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical Reconstructive Reading of Biblical Source Text").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "biblical studies/translation theory/religious authority").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '66b415fb-53ed-47bc-8eff-5fe1c1164f72').
narrative_ontology:cs_kernel_codification('66b415fb-53ed-47bc-8eff-5fe1c1164f72', fixed_text).
narrative_ontology:cs_authority_grounding('66b415fb-53ed-47bc-8eff-5fe1c1164f72', expertise).
narrative_ontology:cs_interpretation_layer_present('66b415fb-53ed-47bc-8eff-5fe1c1164f72').
narrative_ontology:cs_reading_relation('66b415fb-53ed-47bc-8eff-5fe1c1164f72', biblical_source_text__formal_equivalence_reading, influences).
narrative_ontology:cs_reading_relation('66b415fb-53ed-47bc-8eff-5fe1c1164f72', biblical_source_text__dynamic_equivalence_reading, influences).
narrative_ontology:cs_axiom('66b415fb-53ed-47bc-8eff-5fe1c1164f72', foundational, hypothetical_urtext_priority).
narrative_ontology:cs_axiom_status(hypothetical_urtext_priority, holdable).
narrative_ontology:cs_axiom_grounding('66b415fb-53ed-47bc-8eff-5fe1c1164f72', hypothetical_urtext_priority, empirically_contingent).
narrative_ontology:cs_axiom('66b415fb-53ed-47bc-8eff-5fe1c1164f72', foundational, critical_reconstruction_prerequisite).
narrative_ontology:cs_axiom_status(critical_reconstruction_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('66b415fb-53ed-47bc-8eff-5fe1c1164f72', critical_reconstruction_prerequisite, empirically_contingent).
narrative_ontology:cs_reference_frame('66b415fb-53ed-47bc-8eff-5fe1c1164f72', critical_text_priority).
narrative_ontology:cs_drift_state('66b415fb-53ed-47bc-8eff-5fe1c1164f72', contemporary_translation_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('66b415fb-53ed-47bc-8eff-5fe1c1164f72', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, independent_translators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The guild of text critics, exegetes, and biblical scholars whose disciplinary methods, publication venues, curricula, and grant structures are organized around the priority of historical reconstruction; they set the terms on which the biblical text enters translation and interpretation.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, beneficiary,
    institutional, generational, analytical, global).

% Communities whose liturgy, preaching, and theological identity are anchored in received textual traditions; the requirement to subordinate those traditions to hypothetical reconstructed originals undermines textual stability and forces dependence on scholarly expertise they did not choose.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_communities, payer,
    organized, generational, identity_locked, global).

% Global Bible-translation agencies and editorial boards that specify the Nestle-Aland or United Bible Societies Greek New Testament as the default base text, requiring teams to document reasons for departing from the critical text rather than from received tradition.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, translation_standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Freelance or non-affiliated translators who must demonstrate competence in textual criticism and justify translation choices against the eclectic text to gain agency contracts or academic credibility, even when their home communities use received traditions.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, independent_translators, payer,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:fixing_cost_class(biblical_source_text__critical_reconstructive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global biblical scholarship and translation around a shared, reconstructed textual basis so that exegetes and translators do not proceed from incompatible or arbitrary manuscript choices.
% TRANSFER_FUNCTION: Moves authority over the biblical text from ecclesial received tradition and congregational reading memory to the critical scholarly apparatus, its hypothetical reconstructions, and the academic institutions that certify them.
% ABSENT_VOICES: Traditional-text communities (e.g., Greek Orthodox, Textus Receptus Protestants) and confessional pastors whose textual identity depends on received rather than reconstructed text are typically absent from editorial-board and curriculum decisions that enforce the critical priority.
% DISAPPEARANCE_RATIONALE: If the priority of historical reconstruction vanished, seminary curricula would drop text-critical prerequisites, Bible translations would revert to received-text or ecclesial-text bases, and the scholarly publishing and conference economy organized around reconstruction would reorganize around theological and literary study.
% FOUNDING_PROBLEM: The multiplicity of manuscript variants and the lack of a secure, shared original made exegesis and translation dependent on arbitrary or tradition-bound textual choices that varied across communities.
% FOUNDING_PROBLEM_CORROBORATION: Academic societies (SBL, EABS) and text-critical editions (Nestle-Aland, Editio Critica Maior) attest the problem remains live. Confessional seminaries and traditional churches outside the beneficiary set attest the problem is overstated or resolved by divine preservation; their testimony is structurally discounted in text-critical discourse.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.55) is moderate because the constraint coordinates genuine scholarly work while simultaneously destabilizing confessional textual identity; it is not pure extraction, but the asymmetric cost loading is substantial. Suppression (0.48) reflects active gatekeeping: translations and hires that bypass the critical text are marginalized. Theater ratio (0.30) captures the growing performative dimension of text-critical work, where apparatus precision sometimes exceeds textual stability. Accessibility collapse (0.72) is high because once the critical method is accepted, received-text alternatives become nearly unthinkable within the academic frame. Resistance (0.58) is significant because confessional communities and traditional-text movements actively contest the priority of reconstruction.
 *
 * PERSPECTIVAL GAP:
 *   The academic beneficiary seat and the confessional payer seat should compute to markedly different types. From the scholarly position, the constraint is necessary coordination that solves a genuine manuscript-variation problem; from the confessional position, the same structure extracts epistemic security and substitutes scholarly authority for ecclesial memory. The engine computes this divergence from the structural dataâbeneficiary versus payer roles, analytical versus identity_locked exitâwithout the claim prejudging either seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic biblical scholarship is the declared beneficiary (low d, subsidized by the constraint's prestige and institutional centrality). Confessional communities are the declared victim (high d, targeted by destabilization of their textual basis). Translation standards bodies administer enforcement and sit near the agenda-setter pole with constrained exit. Independent translators bear compliance costs without capturing the scholarly benefits. The derivation chain maps these roles directly to directionalities: beneficiaries near 0.0, payers near 1.0, agenda setters near 0.2â0.3.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination (beneficiary) and extraction (victim) facts. If we saw only the academic coordination story, we might classify it as a rope; if we saw only confessional destabilization, we might classify it as a snare. The tangled_rope claim forces both facts into the same account: the same methodological priority that coordinates scholarship also extracts from confession. Active enforcement is required to maintain this hybrid because without curricula, peer review, and agency standards, confessional communities would revert to received-text practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    urtext_recoverability,
    'Is the hypothetical original text of the New Testament sufficiently recoverable to serve as a stable epistemic foundation, or does the multiplicity of variants and the gap between extant manuscripts and the autograph make the constraint rest on an empirically uncertain premise?',
    'Quantitative stemmatics and new manuscript discoveries that either converge on a recoverable archetype or demonstrate that the textual tradition is too fluid to support a single reconstructed original.',
    'If the Urtext is unrecoverable, the constraint''s coordination function collapses into a theater of scholarly consensus, and its extraction from confessional communities becomes pure epistemic rent; if recoverable, the extraction is the necessary cost of textual accuracy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urtext_recoverability, empirical, 'Whether the reconstructed original text is empirically stable enough to warrant priority over received traditions.').

omega_variable(
    confessional_suppression_mechanism,
    'Is the suppression of received-text alternatives in academic and translation contexts structural (gatekeeping by institutions and standards bodies) or internalized (confessional communities adopt critical conclusions despite identity conflict)?',
    'Survey of confessional translation projects and curricula: if they abandon received-text bases when institutional pressure is removed but retain them otherwise, suppression is structural; if they retain critical-text priorities even when free to choose, suppression is partially internalized.',
    'Internalized suppression raises effective extraction because the constraint is carried by the target communities themselves; structural suppression leaves room for resistance and exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confessional_suppression_mechanism, empirical, 'Structural versus internalized suppression of received-text alternatives.').

omega_variable(
    textual_authority_or_scholarly_authority,
    'Does the constraint genuinely privilege the original text, or does it substitute the authority of scholarly reconstruction for the stability of ecclesial text?',
    'Comparative analysis of translation committees: when scholars disagree on reconstruction, does the committee defer to the latest critical edition or to ecclesial reading tradition?',
    'If deference flows to scholarly consensus rather than to text, the constraint is a commitment system extracting authority for the scholarly guild under the cover of textual objectivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_or_scholarly_authority, conceptual, 'Whether the constraint transfers authority to text or to scholarly expertise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__critical_reconstructive_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__critical_reconstructive_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__critical_reconstructive_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__critical_reconstructive_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(bibl_tr_t80, biblical_source_text__critical_reconstructive_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(bibl_tr_t100, biblical_source_text__critical_reconstructive_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(bibl_be_t80, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(bibl_be_t100, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(bibl_su_t60, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(bibl_su_t80, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(bibl_su_t100, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 100, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the biblical_source_text kernel. The three readings (critical_reconstructive, formal_equivalence, dynamic_equivalence) form a constraint family linked by shared textual object but divergent methodological priority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
