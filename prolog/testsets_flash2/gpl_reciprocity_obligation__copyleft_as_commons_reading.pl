% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Commons Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'copyleft as commons' reading of the
 *   GPL reciprocity obligation. In this view, the GPL functions as an
 *   institutional technology to prevent the enclosure of the software
 *   commons, ensuring that modifications and derivative works remain freely
 *   available. It achieves this through a mandatory reciprocity mechanism,
 *   compelling those who use GPL-licensed software to contribute their
 *   changes back to the public domain. This reading emphasizes the collective
 *   benefit and the protection of shared resources over individual freedom or
 *   commercial flexibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.45).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.6).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation (Copyleft as Commons Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'bec9fe47-1713-4a72-bb1c-907619e27739').
narrative_ontology:cs_kernel_codification('bec9fe47-1713-4a72-bb1c-907619e27739', fixed_text).
narrative_ontology:cs_authority_grounding('bec9fe47-1713-4a72-bb1c-907619e27739', lineage).
narrative_ontology:cs_interpretation_layer_present('bec9fe47-1713-4a72-bb1c-907619e27739').
narrative_ontology:cs_reading_relation('bec9fe47-1713-4a72-bb1c-907619e27739', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('bec9fe47-1713-4a72-bb1c-907619e27739', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('bec9fe47-1713-4a72-bb1c-907619e27739', foundational, software_as_shared_resource).
narrative_ontology:cs_axiom_status(software_as_shared_resource, holdable).
narrative_ontology:cs_axiom_grounding('bec9fe47-1713-4a72-bb1c-907619e27739', software_as_shared_resource, deontological).
narrative_ontology:cs_axiom('bec9fe47-1713-4a72-bb1c-907619e27739', foundational, mandatory_reciprocity_prevents_enclosure).
narrative_ontology:cs_axiom_status(mandatory_reciprocity_prevents_enclosure, holdable).
narrative_ontology:cs_axiom_grounding('bec9fe47-1713-4a72-bb1c-907619e27739', mandatory_reciprocity_prevents_enclosure, empirically_contingent).
narrative_ontology:cs_reference_frame('bec9fe47-1713-4a72-bb1c-907619e27739', collective_ownership_and_contribution).
narrative_ontology:cs_drift_state('bec9fe47-1713-4a72-bb1c-907619e27739', contemporary_permissive_licensing_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bec9fe47-1713-4a72-bb1c-907619e27739', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_software_commons).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizing_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the continuous contribution of modifications and derivative works back into the public domain, preventing private appropriation of collective effort. The commons itself is an institutional construct, not a direct actor, but its maintenance is the core function of this reading.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_software_commons, beneficiary,
    institutional, generational, analytical, global).

% Are compelled to release source code for any modifications or derivative works of GPL-licensed software they distribute. This prevents them from enclosing the commons for private gain, but constrains their business models. They bear the cost of mandatory reciprocity.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Wish to use GPL-licensed components in proprietary projects without contributing their changes back. The GPL's viral nature prevents this, forcing them to either contribute or avoid GPL code, thus limiting their individual exit options for private gain.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizing_developers, payer,
    moderate, biographical, constrained, global).

% Administers and defends the GPL, providing legal guidance and enforcing its terms to ensure the software commons remains open and grows through mandatory reciprocity. They act as the primary institutional agent for this reading.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, free_software_foundation, agenda_setter,
    organized, generational, mobile, global).

% Observe the GPL's impact on the broader open-source ecosystem, often advocating for more permissive licenses. While not directly enforcing the GPL, their analysis influences its perception and adoption.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_initiatives, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal framework for mandatory reciprocity, ensuring that contributions to a shared software base are returned to the commons, preventing its enclosure and fostering collective development.
% TRANSFER_FUNCTION: Transfers intellectual property rights (modifications, derivative works) from individual developers and corporations back to the public domain (the software commons), in exchange for the right to use GPL-licensed software.
% ABSENT_VOICES: Proprietary software companies and developers who prioritize unrestricted commercialization of software would object, arguing that the GPL stifles innovation by limiting business models. They are excluded from the GPL's internal logic by its core premise.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, proprietary integrators would immediately cease contributing modifications back to the commons, leading to rapid enclosure of derivative works and a fragmentation of the open-source ecosystem. The collective benefit of the commons would erode.
% FOUNDING_PROBLEM: The problem of software enclosure: early software development saw code being privatized, leading to a loss of user freedom and the inability to build upon and share improvements, threatening the collaborative spirit of early computing.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and many academic researchers attest that the threat of enclosure remains live, citing ongoing attempts by corporations to privatize open-source contributions. Independent legal analysis of intellectual property trends supports the continued relevance of copyleft mechanisms.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it compels contributions, it also grants access to a vast body of free software. Suppression (0.6) is significant as it actively restricts proprietary integration and exit-maximizing behavior, requiring enforcement to maintain the commons. Theater ratio (0.1) is low, indicating that the license's stated function of protecting the commons is largely genuine. The claimed type is Tangled Rope because it provides a genuine coordination function (maintaining the commons) but does so through asymmetric extraction from those who would prefer to privatize contributions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the software commons, the GPL is a vital coordination mechanism. From the perspective of proprietary integrators, it is a restriction on their business models. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The software commons (as an institutional beneficiary) benefits from the mandatory reciprocity. Proprietary integrators and exit-maximizing developers are the payers/victims, as they bear the cost of being compelled to share their modifications. The Free Software Foundation acts as the agenda-setter, actively enforcing the constraint to protect the commons. Other open-source initiatives are observers, analyzing its impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_enclosure_threat_status,
    'Is the threat of software commons enclosure still a primary driver for the GPL''s enforcement, or has the landscape shifted to other concerns?',
    'Empirical analysis of software patent litigation trends, corporate open-source adoption strategies, and the prevalence of permissive licenses vs. copyleft licenses in new projects.',
    'If the threat is diminished, the justification for high suppression (mandatory reciprocity) weakens, potentially reclassifying the constraint towards a Snare or Piton if the enforcement persists without a clear, live problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_enclosure_threat_status, empirical, 'Assesses the ongoing relevance of the GPL''s core problem-solving function.').

omega_variable(
    alternative_commons_maintenance,
    'Are there alternative, less extractive mechanisms for maintaining the software commons that could achieve similar outcomes without mandatory reciprocity?',
    'Comparative study of projects using permissive licenses (e.g., MIT, Apache) that still foster vibrant commons, examining their governance models, funding, and community dynamics.',
    'If viable, less extractive alternatives exist, the GPL''s current form could be reclassified as a Snare or Tangled Rope with higher effective extraction, as its suppression would be unnecessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_commons_maintenance, conceptual, 'Explores whether the GPL''s specific mechanism is the only way to achieve its stated goal.').

omega_variable(
    reading_framing_bias,
    'To what extent does this ''commons'' reading overemphasize institutional protection at the expense of individual developer freedom or commercial viability, compared to other readings?',
    'Analysis of legal scholarship and developer surveys that explicitly compare the ''commons'' framing with ''freedom'' and ''restriction'' framings, identifying which values are prioritized and which are downplayed by each.',
    'If this reading is found to systematically downplay legitimate concerns of other stakeholders, its claimed coordination function might be re-evaluated, potentially increasing its effective extractiveness from those other seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_bias, conceptual, 'Examines the inherent bias in the ''commons'' framing relative to other interpretations of copyleft.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1999, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1999, 0.08).
narrative_ontology:measurement(gpl__tr_t2009, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(gpl__tr_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2019, 0.09).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1989, 0.3).
narrative_ontology:measurement(gpl__be_t1999, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1999, 0.4).
narrative_ontology:measurement(gpl__be_t2009, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2009, 0.45).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2019, 0.43).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1989, 0.4).
narrative_ontology:measurement(gpl__su_t1999, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1999, 0.55).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2009, 0.6).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_reciprocity_obligation' kernel. This 'copyleft as commons' reading focuses on the institutional technology for maintaining a shared resource, distinct from readings emphasizing individual freedom or commercial restriction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
