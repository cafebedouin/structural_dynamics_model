% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Judicial Deference to Congressional Copyright Term Extensions (Rational Basis Review)
 *   domain: constitutional_law/intellectual_property
 *
 * SUMMARY:
 *   This constraint story captures the judicial_ambiguity_reading of the
 *   copyright_constitutional_mandate kernel: the doctrine that copyright term
 *   length falls within a zone of legislative discretion, with courts
 *   applying rational basis review to congressional extensions. The
 *   constraint is the deference standard itself — not the Copyright Clause
 *   text, not the extensions, but the judicial practice of treating 'limited
 *   times' as a legislative judgment call. From this reading's perspective,
 *   the constraint coordinates by giving Congress stable authority to
 *   calibrate copyright terms to changing economic conditions, while
 *   extracting by disabling the constitutional text's limiting function. The
 *   claimed type is tangled_rope because the deference doctrine solves a
 *   genuine coordination problem (legislative flexibility in IP policy) but
 *   asymmetrically extracts from the public domain and constitutional fixity
 *   by making 'limited times' judicially unenforceable. The engine will
 *   compute per-seat classifications from the structural data below.
 *
 * KEY AGENTS:
 *   - Congress (institutional agenda_setter/beneficiary) — sets term lengths, collects institutional authority from deference
 *   - Copyright holder corporations (beneficiary) — lobby for and capture value from term extensions
 *   - Public domain / the public (payer/victim) — loses access to works that would have entered public domain
 *   - Constitutional fixity (victim, non-agent) — the 'limited times' constraint is rendered inoperable
 *   - Courts (agenda_setter/observer) — administer the deference doctrine, frame it as judicial restraint
 *   - Constitutional originalists/textualists (excluded) — would enforce 'limited times' as meaningful limit but lack institutional voice in precedent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.58).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference to Congressional Copyright Term Extensions (Rational Basis Review)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "constitutional_law/intellectual_property").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '53851724-a997-4bcb-946e-85ed9b1e5d67').
narrative_ontology:cs_kernel_codification('53851724-a997-4bcb-946e-85ed9b1e5d67', fixed_text).
narrative_ontology:cs_authority_grounding('53851724-a997-4bcb-946e-85ed9b1e5d67', lineage).
narrative_ontology:cs_interpretation_layer_present('53851724-a997-4bcb-946e-85ed9b1e5d67').
narrative_ontology:cs_reading_relation('53851724-a997-4bcb-946e-85ed9b1e5d67', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('53851724-a997-4bcb-946e-85ed9b1e5d67', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('53851724-a997-4bcb-946e-85ed9b1e5d67', foundational, rational_basis_suffices_for_limited_times).
narrative_ontology:cs_axiom_status(rational_basis_suffices_for_limited_times, holdable).
narrative_ontology:cs_axiom_grounding('53851724-a997-4bcb-946e-85ed9b1e5d67', rational_basis_suffices_for_limited_times, conventional).
narrative_ontology:cs_axiom('53851724-a997-4bcb-946e-85ed9b1e5d67', secondary, legislative_discretion_in_ip_calibration).
narrative_ontology:cs_axiom_status(legislative_discretion_in_ip_calibration, holdable).
narrative_ontology:cs_axiom_grounding('53851724-a997-4bcb-946e-85ed9b1e5d67', legislative_discretion_in_ip_calibration, instrumental).
narrative_ontology:cs_reference_frame('53851724-a997-4bcb-946e-85ed9b1e5d67', eldred_precedent_framework).
narrative_ontology:cs_drift_state('53851724-a997-4bcb-946e-85ed9b1e5d67', post_golan_contemporary, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('53851724-a997-4bcb-946e-85ed9b1e5d67', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congress).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holder_corporations).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_users).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, legislative_supremacy_in_ip_policy).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets copyright term lengths through legislation. Gains institutional authority and legislative flexibility from judicial deference — courts treat term choices as policy judgments Congress is entitled to make. Does not bear the cost of delayed public domain entry. Can adjust terms at will subject only to the near-toothless rational basis test. Exit is irrelevant — Congress is the constraint's author.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, congress, beneficiary).

% Major rights holders (Disney, Warner, Sony, etc.) lobby for term extensions and capture the economic value of delayed public domain entry. They benefit from the deference doctrine because it makes extensions legislatively easy and judicially safe. They have exit options — they can exploit works globally regardless of U.S. term length — but they invest heavily in maintaining the constraint because U.S. terms set global baselines through trade agreements.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holder_corporations, beneficiary,
    powerful, biographical, mobile, global).

% Creators, educators, archivists, and the general public who would use works entering the public domain. Bear the full cost of each term extension: 20+ years of lost access per extension. Exit is constrained — they cannot opt out of copyright law, cannot practically lobby against extensions, and cannot access works until terms expire. The deference doctrine means their only recourse is political (electing different legislators), which has failed repeatedly.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_users, payer,
    moderate, biographical, constrained, national).

% The constitutional text 'for limited Times' as an operative constraint on legislative power. Bears the structural cost of being rendered judicially unenforceable — the clause exists but does no work. Has no exit, no voice, no power. Listed as non-agent payer because it is a structural victim, not a human actor.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity).

% Administer the rational basis deference doctrine. Frame it as judicial restraint and democratic deference. Gain institutional legitimacy from avoiding policymaking but bear reputational costs when deference appears to abdicate the judicial duty to enforce constitutional limits (as in Eldred dissents). Exit is analytical — they could adopt a different standard (intermediate scrutiny, textualist enforcement) but stare decisis and institutional incentives discourage it.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, courts, observer).

% Judges, scholars, and advocates who argue 'limited times' imposes a meaningful, judicially enforceable limit. Structurally excluded from the operative constraint because precedent (Eldred) binds lower courts and deters Supreme Court reconsideration. Would object that deference renders the constitutional text a nullity. Their exit is trapped — they must either work within the deference framework or dissent without effect.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_originalists, excluded,
    organized, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides stable legislative authority to calibrate copyright terms to changing economic and technological conditions without judicial second-guessing of policy judgments.
% TRANSFER_FUNCTION: Moves the enforcement burden of 'limited times' from courts (who would have to define and enforce a limit) to Congress (which faces no effective judicial check), enabling value transfer from the public domain to rights holders through serial term extensions.
% ABSENT_VOICES: The public domain itself — future creators, users, and audiences who would benefit from timely entry of works — are structurally absent. They cannot lobby, litigate, or vote on extensions that affect works not yet created. Constitutional text ('limited Times') is also absent as an operative voice; it is present as text but silenced as constraint.
% DISAPPEARANCE_RATIONALE: If rational basis deference vanished overnight and courts applied meaningful scrutiny to 'limited times,' Congress would lose its unconstrained extension authority. Term extensions would require genuine justification tied to the constitutional purpose. The public domain would resume growing on schedule. Rights holders would lose the certainty of serial extensions. The copyright system would reorganize around a judicially enforced limit.
% FOUNDING_PROBLEM: Early copyright law faced uncertainty about whether courts would second-guess legislative term choices, creating instability for rights holders and disincentivizing investment. The deference doctrine emerged to give Congress clear authority to set terms without judicial interference.
% FOUNDING_PROBLEM_CORROBORATION: Congress and rights holders attest the problem is live (changing markets require legislative flexibility). Public domain advocates, originalist scholars, and Eldred dissenters (Breyer, Stevens) attest the founding problem is substantially solved — legislative authority is settled — and the doctrine now serves as cover for enclosure. The corroboration from outside the beneficiary set (dissenting justices, academic critics) supports the shifted-function reading.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is low-to-moderate: the deference doctrine doesn't directly extract value but enables the extraction cascade — each extension transfers public domain value to rights holders. Suppression (0.58) is moderate: courts actively suppress constitutional challenges by applying rational basis review, but the constraint doesn't use physical coercion. Theater ratio (0.48) is significant: the doctrine is framed as judicial restraint and democratic deference, but functions to insulate extensions from meaningful review. Accessibility collapse (0.62) is elevated: once rational basis is accepted, textualist/originalist alternatives that would enforce 'limited times' collapse. Resistance (0.52) is moderate: academic critique and dissenting opinions exist but haven't shifted the doctrinal equilibrium. The measurement series shows extraction, theater, and suppression all rising from 1976 (pre-CTEA) through Eldred (2003) to present, tracking the scaffold-to-enclosure transition the deference doctrine enables.
 *
 * PERSPECTIVAL GAP:
 *   From the congressional/agenda-setter seat, the constraint is genuine coordination: legislative discretion adapts copyright to new technologies and markets. From the public/payer seat, the same structure is extraction: 'limited times' becomes 'whatever Congress says,' and the public domain shrinks. Courts experience the constraint as institutional role-maintenance — deference protects judicial legitimacy by avoiding policymaking, but Eldred dissents show the reputational cost. The engine computes these seat divergences from the structural data; the claimed type (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress sits near the beneficiary end (d ~ 0.15): it gains institutional authority and legislative flexibility without bearing the public domain costs. Copyright holder corporations are beneficiaries (d ~ 0.2): they capture extension value but don't administer the constraint. The public/public domain are payers (d ~ 0.85): they bear the full cost of delayed entry with no exit option — the constraint applies universally. Courts are near-symmetric (d ~ 0.5): they gain institutional legitimacy from deference framing but bear reputational costs when deference looks like abdication. Constitutional fixity is a non-agent victim — it bears the structural cost of being rendered inoperative. The directionality derivation from beneficiary/victim declarations plus exit options (public has no exit from copyright terms) produces this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislative flexibility to calibrate copyright terms) remains live — technology and markets do change. But the arrangement has drifted: deference has become so deferential that 'limited times' imposes no discernible limit. The mandatrophy is not that the founding problem died, but that the solution (rational basis review) has metastasized into a permission structure for perpetual extension. The constraint persists because Congress benefits from unconstrained authority, courts avoid difficult line-drawing, and rights holders capture the value — no coalition has both incentive and power to reset the boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the copyright_constitutional_mandate kernel, or does it describe the kernel itself?',
    'Compare structural predictions across the three declared readings (judicial_ambiguity, public_scaffold, corporate_enclosure). If each produces different ε, beneficiary/victim sets, and type classifications, they are distinct constraints linked by network.affects_constraints.',
    'If not distinct, the kernel is being double-counted. If distinct, each reading gets its own story and the family structure is captured in network edges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether judicial_ambiguity_reading is a structurally separable constraint from its sibling readings').

omega_variable(
    rational_basis_as_rubber_stamp,
    'Does rational basis review in copyright term cases function as genuine constraint on Congress or as a near-automatic upholding mechanism?',
    'Empirical survey of post-Eldred term extension challenges: rate of invalidation, dissent frequency, and whether any extension has been struck down under rational basis.',
    'If rubber-stamp, suppression is higher and the coordination function is largely performative — the constraint drifts toward snare. If genuine constraint, extraction stays lower and tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_basis_as_rubber_stamp, empirical, 'Whether the deference standard has bite or is purely theatrical').

omega_variable(
    beneficiary_capture_of_deference_doctrine,
    'Does the judicial deference doctrine primarily serve congressional institutional interests, or has it been captured by corporate rights holders who lobby for extensions?',
    'Legislative history analysis of CTEA 1998 and subsequent extension pressures: lobbying expenditures, campaign contributions, and whether Congress acts on its own initiative or at industry behest.',
    'If captured, the true beneficiary shifts from congress to copyright_holder_corporations, altering directionality for the congressional seat and the constraint''s classificatory profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_deference_doctrine, empirical, 'Whether congressional authority is the genuine beneficiary or a pass-through for corporate extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccm_jar_tr_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1976, 0.22).
narrative_ontology:measurement(ccm_jar_tr_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1998, 0.38).
narrative_ontology:measurement(ccm_jar_tr_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2003, 0.45).
narrative_ontology:measurement(ccm_jar_tr_t2018, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2018, 0.48).
narrative_ontology:measurement(ccm_jar_tr_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2024, 0.52).

% Extraction over time
narrative_ontology:measurement(ccm_jar_be_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1976, 0.18).
narrative_ontology:measurement(ccm_jar_be_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1998, 0.35).
narrative_ontology:measurement(ccm_jar_be_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement(ccm_jar_be_t2018, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(ccm_jar_be_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ccm_jar_su_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1976, 0.25).
narrative_ontology:measurement(ccm_jar_su_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1998, 0.48).
narrative_ontology:measurement(ccm_jar_su_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement(ccm_jar_su_t2018, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement(ccm_jar_su_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.12).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_term_extension_act_1998).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, eldred_v_ashcroft_precedent).

% DUAL FORMULATION NOTE:
% This story is one of three in the copyright_constitutional_mandate constraint family. The kernel is the Constitution's 'limited Times' clause (Article I, Section 8, Clause 8). The three readings decompose the kernel into structurally distinct constraints with different ε, beneficiary/victim sets, and classifications. judicial_ambiguity_reading = deference doctrine (tangled_rope, ε=0.42). public_scaffold_reading = the constitutional limit as operational constraint (rope/scaffold, ε≈0.1). corporate_enclosure_reading = the maximalist property claim (snare, ε≈0.75). They are linked by network.affects_constraints because the deference doctrine (this story) structurally enables the enclosure reading's project while suppressing the scaffold reading's enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, institutional, 0.15).
constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, powerful, 0.2).
constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, moderate, 0.85).
constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
