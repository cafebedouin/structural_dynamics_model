% ============================================================================
% CONSTRAINT STORY: operational_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_operational_security_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: operational_security_reading
 *   human_readable: Operational-Security Reading of the Paper-Readiness Quarantine
 *   domain: governance_system_design/distributed_ledger_applications/institutional_economics
 *
 * SUMMARY:
 *   A paper describing a covenant/governance project draws a quarantine
 *   boundary around methodological detail that could re-identify its source
 *   community. Under this reading, the boundary's stated purpose is
 *   protecting the covenant project itself — its community, its internal
 *   doctrine, its unpublished successor works — from exposure or premature
 *   scrutiny. The logic is explicitly reflexive: the same non-enumerability
 *   principle the project applies to outside subjects (M-3, M-25) is applied
 *   to the project's own paper trail. The critical structural feature is that
 *   the boundary is asserted as a property of the community's collective
 *   safety, independent of any individual member's consent — it would
 *   quarantine the community's story even if fully de-anonymized and even if
 *   a member volunteered to be named. This makes the quarantine a claim about
 *   collective vulnerability that overrides individual autonomy, administered
 *   entirely by the maintainers who also stand to benefit from the shielded
 *   successor works.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(operational_security_reading, 0.42).
domain_priors:suppression_score(operational_security_reading, 0.71).
domain_priors:theater_ratio(operational_security_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(operational_security_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(operational_security_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(operational_security_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(operational_security_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(operational_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(operational_security_reading, tangled_rope).
narrative_ontology:human_readable(operational_security_reading, "Operational-Security Reading of the Paper-Readiness Quarantine").
narrative_ontology:topic_domain(operational_security_reading, "governance_system_design/distributed_ledger_applications/institutional_economics").

domain_priors:requires_active_enforcement(operational_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(operational_security_reading, '6de8a9d4-d622-499b-a2cd-c5676f8bcc82').
narrative_ontology:cs_kernel_codification('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', distributed).
narrative_ontology:cs_authority_grounding('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', practice).
narrative_ontology:cs_interpretation_layer_present('6de8a9d4-d622-499b-a2cd-c5676f8bcc82').
narrative_ontology:cs_reading_relation('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', paper_ready_boundary__citation_purity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', paper_ready_boundary__ip_provenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', paper_ready_boundary__design_philosophy_reading, influences).
narrative_ontology:cs_axiom('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', foundational, collective_non_enumerability_overrides_individual_consent).
narrative_ontology:cs_axiom_status(collective_non_enumerability_overrides_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', collective_non_enumerability_overrides_individual_consent, conventional).
narrative_ontology:cs_axiom('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', foundational, safety_rationale_applies_reflexively_to_own_paper_trail).
narrative_ontology:cs_axiom_status(safety_rationale_applies_reflexively_to_own_paper_trail, holdable).
narrative_ontology:cs_axiom_grounding('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', safety_rationale_applies_reflexively_to_own_paper_trail, instrumental).
narrative_ontology:cs_reference_frame('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', m3_m25_non_enumerability_doctrine).
narrative_ontology:cs_drift_state('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', post_publication_scrutiny, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6de8a9d4-d622-499b-a2cd-c5676f8bcc82', '').
narrative_ontology:cs_kernel_id(operational_security_reading, paper_ready_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(operational_security_reading, covenant_project_core_maintainers).
narrative_ontology:constraint_beneficiary(operational_security_reading, corpus_source_community).
narrative_ontology:constraint_victim(operational_security_reading, external_reviewers_and_replicators).
narrative_ontology:constraint_victim(operational_security_reading, prospective_downstream_researchers).
narrative_ontology:constraint_victim(operational_security_reading, willing_to_be_identified_members).
narrative_ontology:constraint_vindicates(operational_security_reading, source_community_non_enumerability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the quarantine boundary on the paper's methods section and appendices, deciding what counts as a 'map back' to the living project. Drafts the redaction rules, adjudicates edge cases, and controls the unpublished successor works the boundary is designed to shield. Can loosen or tighten the boundary at will; bears none of the cost of external non-replicability.
narrative_ontology:constraint_stakeholder(operational_security_reading, covenant_project_core_maintainers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(operational_security_reading, covenant_project_core_maintainers, beneficiary).

% The ~500-member congregation (or comparable body) whose existence, size, membership, and location the paper could render legible if fully disclosed. Benefits from non-enumerability regardless of whether any individual member consents to identification, because the protection is asserted as a property of the community as such, not of any member's preference. Cannot exit the protection even if they wanted to be named — the boundary is administered on their behalf, not by them.
narrative_ontology:constraint_stakeholder(operational_security_reading, corpus_source_community, beneficiary,
    organized, biographical, trapped, regional).

% Individual members who would consent to identification, de-anonymization, or citation of their own contribution. Their stated preference is overridden by the quarantine's collective-non-enumerability logic — the boundary quarantines the community story even when the person inside it says otherwise, because the stated rationale is the community's structural safety, not individual consent.
narrative_ontology:constraint_stakeholder(operational_security_reading, willing_to_be_identified_members, payer,
    powerless, biographical, trapped, local).

% Researchers attempting to verify claims made about the source community, its scale, or its practices. Face a paper trail deliberately engineered to prevent reconstruction of the underlying population — cannot check sampling, cannot assess selection effects, cannot replicate. Their only recourse is to trust the maintainers' account or abandon the verification attempt.
narrative_ontology:constraint_stakeholder(operational_security_reading, external_reviewers_and_replicators, payer,
    moderate, biographical, constrained, global).

% Future scholars who would build on the paper's findings but cannot access the underlying case data, cannot cite specific informants, and cannot cross-reference the community against other datasets. Bear the accumulated cost of a literature built on non-enumerable foundations.
narrative_ontology:constraint_stakeholder(operational_security_reading, prospective_downstream_researchers, payer,
    moderate, generational, constrained, global).

% The project's future papers and internal doctrine documents that the quarantine is partly designed to protect from being reverse-engineered out of the published paper's methodological trail. Not an actor — a body of future work whose existence justifies present-day redaction.
narrative_ontology:constraint_stakeholder(operational_security_reading, unpublished_successor_works, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(operational_security_reading, unpublished_successor_works).

% Evaluate whether the paper's redactions are proportionate to a genuine safety rationale or whether they exceed what operational security would require. Can request additional disclosure as a condition of publication but generally defer to the authors' stated safety rationale.
narrative_ontology:constraint_stakeholder(operational_security_reading, peer_review_committees, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(operational_security_reading, covenant_project_core_maintainers).
narrative_ontology:fixing_cost_class(operational_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents publication details from functioning as a map back to a real, currently-operating community and its unpublished internal work, protecting the source population from exposure, harassment, legal jeopardy, or premature scrutiny that could destabilize a living institution.
% TRANSFER_FUNCTION: Moves interpretive and verificatory access away from external reviewers, replicators, and downstream researchers, and away even from individual community members who would consent to disclosure, toward the maintainers who alone retain full context — trading collective replicability and individual autonomy for institutional continuity.
% ABSENT_VOICES: The willing-to-be-identified members are structurally present in the community but absent from the decision about their own disclosure; external replicators who would object to non-verifiability are not consulted in setting the boundary; the community's institutional critics (if any exist) have no channel into the redaction process at all.
% DISAPPEARANCE_RATIONALE: Maintainers would say the community itself becomes unmasked and destabilizes — harassment, legal exposure, doctrinal disputes spilling into public view, unpublished successor work compromised before completion. External researchers would say nothing about the underlying phenomenon changes; only the paper's verifiability improves, and the 'living vulnerable institution' framing may itself be doing more protective work for the maintainers' authority than for the community's safety.
% FOUNDING_PROBLEM: Early qualitative or ethnographic-adjacent research on living, small, unusually exposed communities (new religious movements, closed governance experiments, novel financial covenant projects) risked exposing real people to real-world retaliation, doxxing, legal jeopardy, or community collapse if published with enumerable specificity.
% FOUNDING_PROBLEM_CORROBORATION: Maintainers attest the problem remains live and cite the M-3/M-25 non-enumerability design principle as continuous institutional doctrine predating this paper. No corroboration exists from outside the maintaining institution itself — no independent ethics board, no named community member (including the willing-to-be-identified members), and no external safety audit has verified that the current redaction scope matches an actual, present threat rather than a generalized policy inherited and applied reflexively to the project's own paper trail.
narrative_ontology:disappearance_verdict(operational_security_reading, contested).
narrative_ontology:founding_problem_status(operational_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(operational_security_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-23',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(operational_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(operational_security_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(operational_security_reading_tests).
:- end_tests(operational_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) and rising slowly: the maintainers extract interpretive control and reputational insulation from external verification, and this compounds gently as successor works accumulate behind the same boundary. Suppression is substantially higher (0.71) because the mechanism actively overrides even consenting individuals' preferences — the boundary is not merely a default but an enforced override on autonomy, which is a stronger suppressive claim than ordinary anonymization. Theater is low-moderate (0.28) — the safety rationale is not obviously fabricated, but it is unaudited, and the gap between claimed function (protecting a real vulnerable population) and actual operation (protecting institutional authority and unpublished doctrine) is exactly the ambiguity the omega below is built to hold open.
 *
 * PERSPECTIVAL GAP:
 *   From the maintainer seat, the boundary reads as protective coordination — a genuine rope solving a genuine exposure problem. From the willing-to-be-identified member's seat, and from the external replicator's seat, the same boundary reads as an enforced extraction of verification rights, justified by a collective-safety claim that no outside party has corroborated and that overrides the very individuals it claims to protect. The engine's per-seat computation should register this divergence structurally: it is not that one seat is wrong, but that the boundary genuinely does both things through the same mechanism, which is the tangled_rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Maintainers sit at the beneficiary end: they administer the boundary, control its scope, and are also the custodians of the unpublished successor works it shields — a direct institutional interest in maintaining opacity. The corpus community as a collective is coded as beneficiary under this reading's own logic (the boundary is FOR their safety), but individual members who would consent to disclosure are coded as payers — their autonomy is the transfer's raw material. External reviewers and downstream researchers are payers who bear the accumulated verification cost. The unpublished successor works are a non-agent beneficiary: institutional future-work is protected by the same redaction that protects the community, and disentangling the two is exactly the operational-security reading's central move.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting a real, exposed community from retaliation — may well be live. But the corroboration gap (no outside verification, not even from consenting members) means the arrangement cannot currently be distinguished from a mandate that has drifted from protecting people to protecting institutional narrative control. Classifying this as tangled_rope rather than snare or rope preserves that undecided status: it names both the real coordination function (community protection) and the real extraction (overridden individual consent, unverifiable institutional benefit) without collapsing to either pole prematurely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_safety_vs_institutional_shield,
    'Is the quarantine''s true function protecting a genuinely vulnerable, currently-existing community from real external threat, or is it protecting the maintaining institution''s authority and unpublished future work by using the community''s asserted vulnerability as cover?',
    'Independent ethics review with access to threat modeling and the actual redaction scope, cross-checked against whether redactions track plausible re-identification vectors or instead track information that would simply make the maintainers'' internal doctrine and successor works legible.',
    'If the former, the tangled_rope classification is generous — this could function closer to a legitimate rope with unavoidable overhead. If the latter, the coordination story is cover and the constraint is closer to a snare with the community''s safety serving as the extraction''s justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_safety_vs_institutional_shield, conceptual, 'Whether the quarantine''s coordination rationale is genuine or is a shield for institutional control.').

omega_variable(
    consent_override_legitimacy,
    'Is it legitimate for a collective non-enumerability principle to override an individual member''s explicit consent to be identified, and who has standing to decide that question?',
    'Compare against established human-subjects research ethics frameworks (which generally treat individual consent as close to dispositive) versus community-protection frameworks in indigenous data sovereignty or similarly collectivized-consent traditions; determine which frame the project''s own stated doctrine actually claims to be operating under.',
    'If individual consent should govern, the willing_to_be_identified_members are being extracted from under a false collective-safety banner, strengthening the extraction reading. If collective consent frameworks are genuinely applicable and endorsed by the community''s own governance, the override is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_override_legitimacy, preference, 'Whether collective non-enumerability can legitimately override individual consent to disclosure.').

omega_variable(
    kernel_framing_under_determination,
    'Given that the paper-ready boundary kernel supports at least four structurally distinct readings (operational security, citation purity, IP provenance, design philosophy), which reading actually governs the maintainers'' redaction decisions in practice, and could more than one be simultaneously true of different redacted passages?',
    'Passage-level audit of the redaction log: for each redacted item, classify which reading''s rationale would justify it, and check whether the maintainers'' own internal justification documents (if disclosed) map consistently onto one reading or shift opportunistically between readings depending on which best justifies a given redaction.',
    'If redactions map cleanly onto this reading throughout, the operational_security framing is well-grounded. If maintainers shift between readings passage-by-passage to maximize justified opacity, the kernel itself is being used strategically, and no single reading — including this one — captures the actual operative logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the four sibling readings of the paper-ready boundary kernel are genuinely distinct governing logics or are opportunistically interchangeable justifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(operational_security_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oper_tr_t0, operational_security_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(oper_tr_t4, operational_security_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(oper_tr_t8, operational_security_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(oper_tr_t12, operational_security_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(oper_tr_t16, operational_security_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(oper_tr_t20, operational_security_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(oper_tr_t24, operational_security_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(oper_be_t0, operational_security_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(oper_be_t4, operational_security_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(oper_be_t8, operational_security_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(oper_be_t12, operational_security_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(oper_be_t16, operational_security_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(oper_be_t20, operational_security_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(oper_be_t24, operational_security_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(oper_su_t0, operational_security_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(oper_su_t4, operational_security_reading, suppression_requirement, 4, 0.59).
narrative_ontology:measurement(oper_su_t8, operational_security_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(oper_su_t12, operational_security_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(oper_su_t16, operational_security_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(oper_su_t20, operational_security_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(oper_su_t24, operational_security_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(operational_security_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(operational_security_reading, 0.1).
narrative_ontology:affects_constraint(operational_security_reading, citation_purity_reading).
narrative_ontology:affects_constraint(operational_security_reading, ip_provenance_reading).
narrative_ontology:affects_constraint(operational_security_reading, design_philosophy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four siblings decomposing the 'paper-ready boundary' kernel per the ε-invariance principle: measuring the same redaction decision through the lens of operational security (this story) versus citation hygiene, IP protection, or design philosophy yields different beneficiary/victim structures and plausibly different ε. Each sibling is authored as its own constraint with its own metrics; this story's distinguishing structural feature is applying the project's own M-3/M-25 non-enumerability doctrine reflexively to itself, including overriding individual member consent — a move the other three readings do not make.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
