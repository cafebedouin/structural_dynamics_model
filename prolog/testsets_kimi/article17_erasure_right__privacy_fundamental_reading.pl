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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Article 17 Right to Erasure â Privacy Fundamental Reading
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   Article 17 of the GDPR creates a statutory right to erasure of personal
 *   data. This constraint story instantiates the privacy_fundamental_reading
 *   of the contested article17_erasure_right kernel, which frames the
 *   provision as an instantiation of individual data sovereignty â a
 *   coordination mechanism that corrects the power asymmetry between data
 *   subjects and global controllers by giving individuals a low-friction,
 *   enforceable deletion right. The kernel decomposes into three readings
 *   because the same legal text sustains three structurally distinct
 *   constraints: this reading centers on rights-protection, the
 *   competitive_moat_reading centers on incumbent-protecting compliance
 *   asymmetry, and the censorship_mechanism_reading centers on
 *   speech-suppression via strategic requests. This reading authors low
 *   theater, moderate extraction (compliance costs borne by controllers), and
 *   a coordination function for data subjects.
 *
 * KEY AGENTS:
 *   - european_data_subjects: Primary beneficiary (organized/moderate power, mobile exit) â gains statutory control over personal data retention
 *   - major_tech_platforms: Primary payer (institutional power, constrained exit) â bears bulk of compliance infrastructure, request volume, and legal uncertainty
 *   - small_online_services: Secondary payer (moderate power, constrained exit) â faces disproportionate compliance costs relative to resources
 *   - data_protection_authorities: Agenda setter (institutional power, analytical exit) â enforces and interprets the right, shapes scope through guidance
 *   - digital_rights_advocates: Observer (organized power, analytical exit) â advocates for broad erasure interpretation and monitors compliance
 *   - research_archivists: Excluded voice (moderate power, constrained exit) â concerned about information loss for research and historical record, not party to the dyad
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.32).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.42).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "Article 17 Right to Erasure â Privacy Fundamental Reading").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, '5c9ad57e-d5f3-42d5-a781-e57036c3e384').
narrative_ontology:cs_kernel_codification('5c9ad57e-d5f3-42d5-a781-e57036c3e384', formalized).
narrative_ontology:cs_authority_grounding('5c9ad57e-d5f3-42d5-a781-e57036c3e384', lineage).
narrative_ontology:cs_interpretation_layer_present('5c9ad57e-d5f3-42d5-a781-e57036c3e384').
narrative_ontology:cs_reading_relation('5c9ad57e-d5f3-42d5-a781-e57036c3e384', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c9ad57e-d5f3-42d5-a781-e57036c3e384', article17_erasure_right__competitive_moat_reading, influences).
narrative_ontology:cs_axiom('5c9ad57e-d5f3-42d5-a781-e57036c3e384', foundational, individual_data_sovereignty).
narrative_ontology:cs_axiom_status(individual_data_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5c9ad57e-d5f3-42d5-a781-e57036c3e384', individual_data_sovereignty, deontological).
narrative_ontology:cs_axiom('5c9ad57e-d5f3-42d5-a781-e57036c3e384', foundational, broad_interpretation_of_erasure).
narrative_ontology:cs_axiom_status(broad_interpretation_of_erasure, holdable).
narrative_ontology:cs_axiom_grounding('5c9ad57e-d5f3-42d5-a781-e57036c3e384', broad_interpretation_of_erasure, conventional).
narrative_ontology:cs_reference_frame('5c9ad57e-d5f3-42d5-a781-e57036c3e384', individual_data_sovereignty_framework).
narrative_ontology:cs_drift_state('5c9ad57e-d5f3-42d5-a781-e57036c3e384', post_gdpr_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c9ad57e-d5f3-42d5-a781-e57036c3e384', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, european_data_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, major_tech_platforms).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, small_online_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can request erasure of personal data with low procedural friction under GDPR Article 17; gain control over digital identity and retention; rely on a statutory mechanism rather than individual contractual negotiation with global platforms.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, european_data_subjects, beneficiary,
    organized, biographical, mobile, continental).

% Must process high volumes of erasure requests, maintain technical infrastructure for deletion across distributed systems, absorb compliance costs and legal uncertainty; operate under threat of significant administrative fines for non-compliance.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, major_tech_platforms, payer,
    institutional, generational, constrained, global).

% Face disproportionate compliance costs relative to revenue; often lack dedicated legal or engineering capacity to assess request validity; risk-averse over-deletion driven by fear of fines.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, small_online_services, payer,
    moderate, biographical, constrained, regional).

% Enforce Article 17 through investigations and fines; issue interpretive guidance on scope and exceptions; adjudicate disputes between data subjects and controllers; shape the practical boundaries of the right.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Advocate for broad erasure interpretation; assist individuals in exercising rights; monitor controller compliance and enforcement activity; publish accountability research.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, digital_rights_advocates, observer,
    organized, generational, analytical, continental).

% Concerned that broad erasure undermines research datasets, reproducibility, and historical record; lack standing in the data subject-controller dyad and are not consulted in standard erasure proceedings.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, research_archivists, excluded,
    moderate, civilizational, constrained, global).

narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates individual data-deletion demands into a uniform statutory mechanism, solving the collective-action problem of isolated individuals lacking bargaining power or information against global data controllers.
% TRANSFER_FUNCTION: Transfers control over personal data retention from data controllers to data subjects, and transfers the operational compliance burden from the regulatory state to private platforms.
% ABSENT_VOICES: Research archivists, historians, and future public-interest users of data are structurally excluded; they would argue for information-preservation exceptions and scientific-research carve-outs but are not party to the individual-platform dyad.
% DISAPPEARANCE_RATIONALE: If Article 17 vanished overnight, individuals would revert to contractual negotiation with negligible leverage; platforms would default to data retention; the digital rights landscape would reorganize around retention and processing rather than erasure; supervisory authorities would lose a primary enforcement mandate.
% FOUNDING_PROBLEM: Asymmetric power between individuals and global data processors meant personal data could be retained indefinitely, spread across opaque systems, and used for purposes beyond the individual's knowledge or control, with no effective individual remedy.
% FOUNDING_PROBLEM_CORROBORATION: Privacy advocates and academic legal scholars attest the problem remains live. Major tech platforms and some free-speech advocates contest the framing, arguing the problem is overstated or the remedy disproportionate. Independent digital-rights organizations such as EDRi corroborate the live status from outside the immediate beneficiary set.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.32, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.28â0.40) is moderate because the constraint imposes real compliance costs on controllers, but these costs are bounded by rule-of-law procedures and do not concentrate as rents to a third party. Suppression (0.30â0.50) is moderate-to-low relative to extractive constraints because enforcement is administrative and judicial rather than coercive in the snare sense. Theater ratio stays low (0.10â0.18) because the erasure function is operational and not primarily performative. Accessibility collapse is high (0.70) because the statutory right replaces individual contractual negotiation as the dominant deletion mechanism. Resistance (0.38) reflects persistent platform lobbying and strategic legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   The data subject seat experiences the constraint as genuine coordination â a granted right with low procedural friction. The platform seats experience it as a cost center and legal risk. The authority seat experiences it as an enforcement mandate. The engine computes these divergences from structural position: beneficiaries with organized power and mobile exit sit near dâ0.2, while constrained institutional payers sit near dâ0.8.
 *
 * DIRECTIONALITY LOGIC:
 *   European data subjects are declared beneficiaries; their directionality is low because the constraint subsidizes their agency. Major and small platforms are payers with constrained exit (must operate in the EU market); their directionality is high because the constraint extracts compliance effort from them. The absence of a concentrated beneficiary capturing the extracted value keeps the base extractiveness moderate rather than high.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a rope (rather than a snare) preserves the genuine coordination function â individuals historically lacked bargaining power against global controllers, and statutory erasure solves a real collective-action problem. Classifying it as a tangled rope would require naming victims of asymmetric extraction; while small platforms bear disproportionate costs, the primary structure is rights-protection rather than rent extraction. The mandatrophy risk is that the constraint could decay into a piton if enforcement becomes theatrical, or into a snare if weaponized; the temporal measurements show stable, low theater over the observed interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    erasure_speech_collision,
    'Does a broad interpretation of Article 17 structurally collide with freedom of expression, and is that collision internal to the constraint or an external sibling-reading effect?',
    'Comparative case-law tracing of CJEU and national DPA decisions balancing erasure against journalism and public-interest exceptions.',
    'If the collision is unresolved and internal, the coordination function is compromised; if external (a different constraint), the privacy reading remains a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erasure_speech_collision, conceptual, 'Speech-erasure tension location within the kernel').

omega_variable(
    compliance_cost_asymmetry,
    'Do Article 17 compliance costs fall asymmetrically on smaller controllers, converting rights-protection into an extraction vector against them?',
    'Empirical cost-survey across controller size tiers; DPA fine and complaint distribution data.',
    'If severe asymmetry exists, the constraint exhibits tangled-rope structure for the small-platform seat even under the privacy reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_asymmetry, empirical, 'Small-platform cost asymmetry under broad erasure').

omega_variable(
    kernel_reading_indeterminacy,
    'Does the legal text of Article 17 underdetermine which reading (privacy fundamental, competitive moat, censorship mechanism) is structurally dominant?',
    'Corpus-wide comparison of the three sibling constraint stories; empirical analysis of actual erasure request patterns (individual exercise vs. strategic suppression vs. compliance-barrier effects).',
    'If the text is genuinely underdetermined, no single reading captures the full constraint; the kernel must remain decomposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Underdetermination of dominant reading in the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a17_priv_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(a17_priv_tr_t12, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(a17_priv_tr_t24, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(a17_priv_tr_t36, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 36, 0.14).
narrative_ontology:measurement(a17_priv_tr_t48, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 48, 0.15).
narrative_ontology:measurement(a17_priv_tr_t60, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement(a17_priv_tr_t72, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 72, 0.18).

% Extraction over time
narrative_ontology:measurement(a17_priv_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(a17_priv_be_t12, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(a17_priv_be_t24, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(a17_priv_be_t36, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 36, 0.36).
narrative_ontology:measurement(a17_priv_be_t48, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 48, 0.37).
narrative_ontology:measurement(a17_priv_be_t60, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(a17_priv_be_t72, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 72, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(a17_priv_su_t0, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(a17_priv_su_t12, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(a17_priv_su_t24, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(a17_priv_su_t36, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement(a17_priv_su_t48, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 48, 0.47).
narrative_ontology:measurement(a17_priv_su_t60, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(a17_priv_su_t72, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 72, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Article 17' conflates three structurally distinct constraints. This story isolates the privacy-fundamental reading; the competitive-moat and censorship-mechanism siblings carry different Îµ values, beneficiary/victim structures, and directionalities. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
