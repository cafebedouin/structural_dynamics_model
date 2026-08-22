% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: GDPR Article 17 — Right to Erasure as Individual Data Sovereignty
 *   domain: technology_governance/data_protection_law
 *
 * SUMMARY:
 *   Article 17 of the GDPR codifies a right to erasure ('right to be
 *   forgotten') allowing individuals to compel deletion of personal data
 *   under specified conditions. This story authors the privacy_fundamental
 *   reading: the provision as a genuine instantiation of individual data
 *   sovereignty, correcting a structural asymmetry in which firms accumulated
 *   and retained personal data indefinitely with no corresponding individual
 *   control. Under this reading the coordination function is real (a workable
 *   deletion mechanism benefiting the class of data subjects broadly) and
 *   extraction is low — compliance costs on firms are the ordinary cost of
 *   respecting a right, not asymmetric rent extraction. This is a sibling
 *   story to censorship_mechanism_reading (which reads erasure requests as
 *   weaponizable against speech/archival interests) and
 *   competitive_moat_reading (which reads compliance costs as
 *   incumbent-protecting). Each reading shares the kernel text of Article 17
 *   but authors a structurally distinct ε, beneficiary/victim set, and
 *   classification, per the ε-invariance principle — this story does not
 *   average across them or hedge its own ε to accommodate the others.
 *
 * KEY AGENTS:
 *   - data_subjects: primary beneficiary (moderate/constrained) — gains a deletion mechanism where none existed
 *   - data_controllers_processing_departments: primary payer (organized/constrained) — bears compliance infrastructure cost
 *   - data_protection_authorities: agenda_setter (institutional/analytical) — administers and interprets the right's scope
 *   - downstream_data_processors: secondary payer (moderate/constrained) — inherits cascading compliance obligations
 *   - researchers_and_archivists: excluded voice (moderate/constrained) — interest in persistence not represented in individual requests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.22).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.15).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "GDPR Article 17 — Right to Erasure as Individual Data Sovereignty").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology_governance/data_protection_law").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, '63b0ab05-0e5c-4d85-b511-4885a76f035e').
narrative_ontology:cs_kernel_codification('63b0ab05-0e5c-4d85-b511-4885a76f035e', fixed_text).
narrative_ontology:cs_authority_grounding('63b0ab05-0e5c-4d85-b511-4885a76f035e', lineage).
narrative_ontology:cs_interpretation_layer_present('63b0ab05-0e5c-4d85-b511-4885a76f035e').
narrative_ontology:cs_reading_relation('63b0ab05-0e5c-4d85-b511-4885a76f035e', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('63b0ab05-0e5c-4d85-b511-4885a76f035e', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('63b0ab05-0e5c-4d85-b511-4885a76f035e', foundational, individual_data_control_is_a_primary_right).
narrative_ontology:cs_axiom_status(individual_data_control_is_a_primary_right, holdable).
narrative_ontology:cs_axiom_grounding('63b0ab05-0e5c-4d85-b511-4885a76f035e', individual_data_control_is_a_primary_right, deontological).
narrative_ontology:cs_axiom('63b0ab05-0e5c-4d85-b511-4885a76f035e', secondary, erasure_requests_should_be_granted_absent_overriding_lawful_basis).
narrative_ontology:cs_axiom_status(erasure_requests_should_be_granted_absent_overriding_lawful_basis, holdable).
narrative_ontology:cs_axiom_grounding('63b0ab05-0e5c-4d85-b511-4885a76f035e', erasure_requests_should_be_granted_absent_overriding_lawful_basis, conventional).
narrative_ontology:cs_reference_frame('63b0ab05-0e5c-4d85-b511-4885a76f035e', pre_gdpr_indefinite_retention_default).
narrative_ontology:cs_drift_state('63b0ab05-0e5c-4d85-b511-4885a76f035e', post_cjeu_balancing_test_jurisprudence, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('63b0ab05-0e5c-4d85-b511-4885a76f035e', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, privacy_advocacy_groups).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, data_controllers_processing_departments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, downstream_data_processors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose personal data is held by platforms and firms. Under this reading, they hold a standing right to demand deletion of data absent an overriding lawful basis, without needing to justify the request beyond invoking the right. Their exit from data exposure was previously nonexistent (data persisted indefinitely once collected); the right creates an exit mechanism where none existed.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_subjects, beneficiary,
    moderate, biographical, constrained, national).

% Firms holding personal data must build request-intake pipelines, verify identity, propagate deletions across backups and downstream processors, and document compliance. From this reading's perspective, this is simply the operational cost of respecting a right the firm should not have been able to withhold in the first place — not a strategic cost imposed on them by rivals.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_controllers_processing_departments, payer,
    organized, biographical, constrained, global).

% National and EU-level regulators interpret the erasure right's scope, adjudicate complaints, and levy fines for noncompliance. They administer the balancing test between erasure and competing interests (freedom of expression, legal obligations, public interest archiving) but under this reading lean toward broad, low-friction interpretation of erasure requests.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Third parties who received data from a controller must also honor erasure once notified, incurring cascading compliance costs through contracts they did not negotiate directly with the data subject.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, downstream_data_processors, payer,
    moderate, biographical, constrained, continental).

% Historical researchers, journalists, and public-interest archivists rely on data persistence for accountability and record-keeping. Under a broad erasure interpretation, their interest in retained data is not a formal party to individual erasure requests and is not weighed unless they intervene through separate exemption channels.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, researchers_and_archivists, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, enforceable mechanism by which individuals can compel deletion of their personal data, solving the collective-action problem where no single data subject could otherwise negotiate deletion terms with a data-holding firm.
% TRANSFER_FUNCTION: Moves the burden of data lifecycle management from the individual (who previously had no recourse once data was collected) to the data controller (who must now build and operate deletion infrastructure), and moves informational control from firms back to the individuals the data describes.
% ABSENT_VOICES: Researchers, journalists, and public archivists whose interest in data persistence is not represented in the individual erasure request process; they can seek exemptions but are not party to the erasure decision itself.
% DISAPPEARANCE_RATIONALE: If Article 17 vanished, data subjects would revert to having no standing mechanism to compel deletion; firms would retain data indefinitely by default; the compliance infrastructure built around erasure (request pipelines, verification systems, downstream propagation) would become unnecessary, and firms would reallocate that capacity elsewhere.
% FOUNDING_PROBLEM: Before a codified erasure right, individuals had no way to compel deletion of personal data held by firms — data persisted indefinitely regardless of continued relevance, consent withdrawal, or changed circumstances, creating an asymmetry where firms controlled data about individuals with no corresponding individual control.
% FOUNDING_PROBLEM_CORROBORATION: Data protection authorities and independent privacy researchers outside the beneficiary class (data subjects) attest that indefinite retention absent a deletion mechanism remains a live problem — documented in DPA enforcement reports and academic data-minimization literature; this is not solely attested by advocacy groups who stand to benefit from the right's continued force.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.22 at interval end) because under this reading the right transfers control to the class it was built to protect rather than extracting rent for a third party; the cost firms bear is the operational cost of the coordination function, not asymmetric extraction. Suppression is low (0.15): the right does not close off alternatives for firms beyond requiring them to honor deletion — firms retain data-processing capability for lawful purposes. Theater ratio is modest and rises slightly over the interval (0.10 to 0.20) reflecting a mild but real drift toward compliance-checkbox behavior (privacy policy boilerplate, perfunctory request forms) alongside substantive compliance, which the story tracks honestly without treating it as dominant.
 *
 * PERSPECTIVAL GAP:
 *   The data_subjects seat and the data_controllers seat compute differently: from the subject's seat, this is close to a Rope — coordination that solves a genuine problem with the subject as principal beneficiary. From the controller's seat, the same mechanism registers as a cost center with real but bounded extraction. The engine computes these divergent per-seat readings from the declared structural data (power, exit, beneficiary/victim); this story does not force convergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects are declared beneficiaries — the right subsidizes their control over personal information, pushing directionality toward the beneficiary end. Data controllers and downstream processors are declared payers bearing the compliance transfer — directionality moves toward the target end, but their exit options are merely constrained (they can adjust practices, invest in compliance tooling, or seek exemptions) rather than trapped, keeping effective extraction moderate rather than severe. Data protection authorities sit in an analytical/agenda-setting position, administering rather than benefiting or paying directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (individual inability to compel data deletion) remains live per corroboration outside the beneficiary class, and the disappearance verdict is world_rearranges — this blocks a mandatrophy read: the mandate has not outlived its function under this reading. This differentiates the privacy_fundamental reading from a piton or scaffold-gone-stale interpretation; the coordination function this reading identifies is still doing real work, not merely performing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    erasure_scope_boundary_contest,
    'Does the broad, low-friction interpretation of erasure requests this reading assumes hold consistently in practice, or does the actual boundary-drawing between erasure and countervailing interests (speech, archival, competitive access to data) shift the constraint toward one of the sibling readings in specific application domains?',
    'Track DPA adjudication outcomes and CJEU case law over time: if erasure requests are granted broadly with minimal balancing-test friction, this reading''s low-extraction characterization holds; if courts increasingly carve out speech/archival/competitive exemptions, the practical operation drifts toward the censorship or moat readings for those carved-out domains.',
    'A finding of systematic narrow interpretation (heavy balancing-test friction) would suggest this reading''s ''low epistemic friction for requests'' premise is empirically false in practice, weakening the privacy_fundamental characterization and shifting weight toward the competing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erasure_scope_boundary_contest, empirical, 'Whether broad erasure interpretation actually holds in adjudicated practice or narrows toward sibling-reading territory.').

omega_variable(
    kernel_framing_underdetermination,
    'Is Article 17''s single legal text genuinely three structurally distinct constraints (as this decomposition assumes), or does the underlying kernel''s ambiguity mean that a single unified reading with observer-relative outcomes would better capture the phenomenon, contra the ε-invariance decomposition rule applied here?',
    'Examine whether the three readings ever produce identical outcomes in the same case (convergence would suggest one underlying constraint with observer noise) versus systematically different outcomes for structurally different fact patterns (divergence supports genuine decomposition into separate constraints).',
    'If the readings converge in most applied cases, this favors treating Article 17 as one constraint with contested interpretation rather than three separate constraints — a conceptual challenge to how this kernel was decomposed for this corpus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three-reading decomposition of Article 17 tracks genuine structural distinctness or observer-relative framing of one constraint.').

omega_variable(
    compliance_cost_incidence_ambiguity,
    'Is the compliance cost borne by data_controllers genuinely proportionate to the coordination benefit delivered to data_subjects, or does it in fact fall disproportionately on smaller controllers in a way that this reading''s ''ordinary operational cost'' framing understates?',
    'Compare compliance cost as a share of revenue/operating budget across firm size bands; if costs scale regressively (smaller firms pay proportionally more), that data point would need reconciling with this reading''s assumption of a symmetric coordination cost.',
    'Regressive cost incidence would not by itself convert this reading into the competitive_moat_reading (that is a separate constraint), but it would qualify this reading''s characterization of extraction as low and uniform across the payer class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_incidence_ambiguity, empirical, 'Whether erasure compliance costs are evenly distributed across controllers or regressively concentrated on smaller firms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(arti_tr_t8, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(arti_tr_t16, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(arti_tr_t20, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(arti_tr_t24, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 4, 0.17).
narrative_ontology:measurement(arti_be_t8, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(arti_be_t16, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 16, 0.21).
narrative_ontology:measurement(arti_be_t20, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(arti_be_t24, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 24, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article17_erasure_right__privacy_fundamental_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the article17_erasure_right kernel, decomposed per the ε-invariance principle: privacy_fundamental_reading (this story, ε≈0.22, rope), competitive_moat_reading (higher ε expected, incumbent-protection framing, tangled_rope or snare candidate), and censorship_mechanism_reading (higher ε expected, speech-suppression framing, snare candidate). All three share the Article 17 legal text as their kernel but diverge sharply in beneficiary/victim structure and extraction because they are evaluating structurally different claims about what the same text does in practice. Each file must link to at least one other family member; this file links to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
