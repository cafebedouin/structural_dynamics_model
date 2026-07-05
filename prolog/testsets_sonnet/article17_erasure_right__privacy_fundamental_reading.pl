% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__privacy_fundamental_reading, []).

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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: Article 17 Right to Erasure — Data Subject Sovereignty Reading
 *   domain: technology_governance/data_protection_law
 *
 * SUMMARY:
 *   This story instantiates the privacy-fundamental reading of the Article 17
 *   (right to erasure) kernel: the individual is the primary beneficiary,
 *   data controllers are constrained parties bearing compliance costs, the
 *   interpretation of 'erasure' is broad (data no longer necessary for its
 *   original purpose qualifies), and the epistemic friction for making a
 *   request is low (no need to prove concrete harm). Under this reading the
 *   right functions as genuine coordination — it solves a real
 *   informational-control problem individuals could not otherwise solve —
 *   with modest, non-suppressive enforcement overhead. This is one of three
 *   sibling constraints sharing the article17_erasure_right kernel; the
 *   competitive_moat_reading and censorship_mechanism_reading instantiate
 *   structurally different claims (incumbent compliance-cost advantage;
 *   strategic suppression of speech) with different beneficiary/victim sets
 *   and different epsilon values entirely — they are not alternative
 *   measurements of this constraint, they are different constraints riding
 *   the same text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.18).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.22).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "Article 17 Right to Erasure — Data Subject Sovereignty Reading").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology_governance/data_protection_law").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, 'd2e3bc6b-fed4-44f8-b459-2f245af7162a').
narrative_ontology:cs_kernel_codification('d2e3bc6b-fed4-44f8-b459-2f245af7162a', fixed_text).
narrative_ontology:cs_authority_grounding('d2e3bc6b-fed4-44f8-b459-2f245af7162a', lineage).
narrative_ontology:cs_interpretation_layer_present('d2e3bc6b-fed4-44f8-b459-2f245af7162a').
narrative_ontology:cs_reading_relation('d2e3bc6b-fed4-44f8-b459-2f245af7162a', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2e3bc6b-fed4-44f8-b459-2f245af7162a', article17_erasure_right__censorship_mechanism_reading, influences).
narrative_ontology:cs_axiom('d2e3bc6b-fed4-44f8-b459-2f245af7162a', foundational, individual_informational_self_determination).
narrative_ontology:cs_axiom_status(individual_informational_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('d2e3bc6b-fed4-44f8-b459-2f245af7162a', individual_informational_self_determination, deontological).
narrative_ontology:cs_axiom('d2e3bc6b-fed4-44f8-b459-2f245af7162a', secondary, erasure_scope_bounded_by_competing_legitimate_interests).
narrative_ontology:cs_axiom_status(erasure_scope_bounded_by_competing_legitimate_interests, holdable).
narrative_ontology:cs_axiom_grounding('d2e3bc6b-fed4-44f8-b459-2f245af7162a', erasure_scope_bounded_by_competing_legitimate_interests, conventional).
narrative_ontology:cs_reference_frame('d2e3bc6b-fed4-44f8-b459-2f245af7162a', informational_self_determination_framework).
narrative_ontology:cs_drift_state('d2e3bc6b-fed4-44f8-b459-2f245af7162a', post_gdpr_enforcement_maturation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d2e3bc6b-fed4-44f8-b459-2f245af7162a', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, privacy_advocacy_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, data_controllers).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, informational_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose personal data is held by platforms and data controllers. Under this reading, they can request erasure of data no longer necessary for the purpose it was collected, without needing to prove harm. Their leverage over their own data was previously near-zero; the right converts a diffuse informational asymmetry into an enforceable, individually-triggerable claim.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_subjects, beneficiary,
    powerless, biographical, constrained, national).

% Companies and organizations that process personal data must build request-intake pipelines, verify identity, propagate deletions across backups and downstream processors, and document compliance. They bear the operational cost of honoring erasure requests but retain the ability to refuse requests that conflict with legal obligations, free expression, or public interest — the right is not absolute.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_controllers, payer,
    institutional, biographical, constrained, continental).

% National and EU-level regulators interpret the scope of erasure obligations, adjudicate disputes between data subjects and controllers, and issue guidance narrowing or broadening what counts as valid grounds for retention. They enforce the right through fines and orders, and their interpretive choices determine how expansively the right operates in practice.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% NGOs and civil-society groups that litigate test cases, publish guidance for individuals invoking the right, and lobby for expansive interpretation. They benefit from the right's existence as a tool for constraining corporate data practices generally, even when not personally exercising it.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, privacy_advocacy_organizations, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__privacy_fundamental_reading, privacy_advocacy_organizations, observer).

% Individuals whose information appears incidentally in another person's erasure request (e.g. co-authored records, shared histories, journalistic archives referencing them) are not party to the erasure decision and have no seat in the request process, though the outcome can affect records that concern them too.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, affected_third_parties, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, diffuse).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, predictable legal mechanism by which individuals can compel deletion of personal data that has outlived its collection purpose, replacing ad hoc, inconsistent, jurisdiction-by-jurisdiction disputes with one enforceable standard.
% TRANSFER_FUNCTION: Moves control over retained personal data from the entity holding it (which previously could retain indefinitely at near-zero cost) to the individual it concerns, at the cost of the controller's compliance overhead.
% ABSENT_VOICES: Third parties incidentally referenced in another person's records are not represented in the erasure request; archivists and historians raise concerns about record completeness but are not parties to individual erasure decisions.
% DISAPPEARANCE_RATIONALE: If the erasure right vanished, individuals would lose their primary enforceable lever over data controllers' retention practices; data brokers and platforms would face materially reduced incentive to build deletion infrastructure, and de facto indefinite retention would likely resume as the default absent this specific obligation.
% FOUNDING_PROBLEM: Prior to comprehensive data protection law, individuals had no structured means to compel deletion of personal data held by corporations or institutions once initial consent or purpose had lapsed, leading to indefinite retention, secondary use, and data breach exposure disconnected from any ongoing legitimate need.
% FOUNDING_PROBLEM_CORROBORATION: Data protection authorities' enforcement statistics and academic empirical studies of retention practices (outside both data subjects and controllers) continue to document indefinite retention absent legal compulsion, corroborating that the underlying informational-control problem persists rather than having been solved by market or technical means alone.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__privacy_fundamental_reading_tests).
:- end_tests(article17_erasure_right__privacy_fundamental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the right transfers control TO individuals rather than extracting value from them; the cost falls on data controllers as compliance overhead, which is treated here as coordination cost, not extraction, because the underlying problem (indefinite retention absent compulsion) is real and the individual's position improves without an identifiable victim class. Suppression is low-moderate (0.22) — controllers can legitimately refuse requests conflicting with other legal obligations, so the right does not operate as unconditional coercion. Theater ratio is low and only mildly increasing (0.15 by t=24), reflecting some compliance-theater drift (template rejection letters, slow-walking) but a still-substantially-functional mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the data subject seat this operates as straightforward coordination — a tool that did not exist before now exists and benefits them at low cost. From the data controller seat the same mechanism registers as a bounded regulatory obligation with real but non-extractive overhead. The engine should compute these seats as structurally different but not necessarily as different constraint TYPES — the coordination function under this reading is genuine on both sides, which is the point of distinguishing this reading from the competitive_moat sibling, where the same text produces asymmetric extraction rather than mutual coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects are the structural beneficiaries — low or negative d, since the constraint subsidizes their control over their own data. Data controllers are payers bearing compliance cost, giving them a directionality nearer the target end, though tempered by the fact that the obligation is bounded (they can lawfully refuse in specified circumstances) rather than absolute. Data protection authorities sit as agenda-setters with analytical exit — they administer rather than experience the constraint directly. Privacy advocacy organizations benefit structurally from the right's existence as leverage even when not personally invoking it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (indefinite retention absent individual recourse) remains live per corroborating regulatory and academic evidence outside the beneficiary class, so this reading shows no mandatrophy — the mandate has not outlived its function. This distinguishes it from a potential future story in which the right, having solved its founding problem, persists mainly as compliance theater; the temporal measurements here show only mild theater-ratio drift, consistent with an still-functioning, not-yet-degraded coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    erasure_scope_boundary_indeterminacy,
    'Where exactly does ''no longer necessary for the purpose collected'' stop being a genuine individual-control claim and start functioning as leverage usable for other ends (competitive advantage, speech suppression)?',
    'Track adjudicated cases across the three kernel readings: cases resolved on pure privacy grounds with no competing speech or competitive-cost dimension corroborate this reading; cases where erasure functions as compliance-cost weapon or speech suppression correspond to the sibling readings. A rising share of contested-boundary cases would indicate readings are not cleanly separable in practice even though structurally distinct.',
    'If a large share of real-world Article 17 invocations sit at the boundary between readings, this reading''s low epsilon may be measuring only the uncontested core of erasure requests, understating how often the same textual right is invoked for the moat or censorship functions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erasure_scope_boundary_indeterminacy, conceptual, 'Whether the three kernel readings are empirically separable or blend at the margins in actual enforcement practice.').

omega_variable(
    third_party_data_exclusion,
    'Should individuals incidentally referenced in another person''s data (shared records, co-authored communications, public-record subjects) have any standing in an erasure request that affects records concerning them?',
    'Comparative analysis of DPA guidance and case law on erasure requests intersecting third-party data; track whether third-party interests are formally weighed or structurally absent from the adjudication process.',
    'If third parties are structurally unrepresented as a matter of legal design (not merely case-by-case oversight), this reading''s zero-victim declaration may understate a diffuse, uncounted cost class distinct from the controllers who are the named payers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_data_exclusion, conceptual, 'Whether the exclusion of incidentally-referenced third parties from the erasure decision constitutes a latent victim class not currently declared.').

omega_variable(
    controller_compliance_cost_distribution,
    'Is the compliance cost genuinely proportionate across data controllers of different sizes, or does it fall disproportionately on smaller controllers even under this privacy-focused reading?',
    'Compare per-request compliance cost as a share of revenue/operating budget across controller size tiers; if cost scales regressively even absent any moat-seeking behavior, that is a structural feature of the mechanism itself, not just the sibling competitive_moat_reading''s concern.',
    'If cost asymmetry exists even under the pure privacy-coordination framing, the boundary between this reading and the competitive_moat_reading is less clean than the kernel decomposition assumes, and some of this reading''s low extractiveness score may need revisiting for smaller-controller-specific analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(controller_compliance_cost_distribution, empirical, 'Whether erasure-compliance cost is proportionate across controller sizes under the privacy-fundamental framing alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement(arti_tr_t8, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(arti_tr_t16, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement(arti_tr_t20, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(arti_tr_t24, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 4, 0.13).
narrative_ontology:measurement(arti_be_t8, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 12, 0.16).
narrative_ontology:measurement(arti_be_t16, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(arti_be_t20, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(arti_be_t24, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 24, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(arti_su_t4, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 4, 0.19).
narrative_ontology:measurement(arti_su_t8, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(arti_su_t12, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement(arti_su_t16, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 16, 0.21).
narrative_ontology:measurement(arti_su_t20, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(arti_su_t24, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 24, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints sharing the article17_erasure_right kernel. privacy_fundamental_reading (this file) treats the individual as primary beneficiary with low extraction and genuine coordination function. competitive_moat_reading treats large platforms as incumbent beneficiaries and smaller competitors/new entrants as victims of asymmetric compliance cost — a tangled_rope or snare depending on enforcement data. censorship_mechanism_reading treats erasure requesters as strategic actors and documentary/speech interests (journalists, historical record, public accountability) as victims — likely tangled_rope or snare. Each carries an independent epsilon per the invariance principle; they are linked via affects_constraints because enforcement and interpretive choices by data_protection_authorities in this reading structurally influence how much room the other two readings have to operate (a broad, low-friction interpretation here expands the surface area both siblings can exploit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
