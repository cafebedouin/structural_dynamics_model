% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Property Reading)
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the narrow_defense_reading of the
 *   fair_use_statutory_exception kernel. Under this reading, copyright is
 *   conceptualized as traditional property and fair use is construed as a
 *   narrow affirmative defense whose purpose is to preserve market value for
 *   the rights holder. The reading places the burden of proof on the
 *   defendant, treats commercial nature as heavily dispositive, and
 *   underweights transformativeness relative to market harm. It generates
 *   genuine coordination benefits for licensing markets while asymmetrically
 *   extracting from transformative creators, educators, and documentary
 *   filmmakers. The engine is expected to compute divergent seat types:
 *   beneficiary seats may see coordination, while payer seats see extraction.
 *
 * KEY AGENTS:
 *   - major_rights_holders (powerful/arbitrage) â primary beneficiary and enforcement actor
 *   - content_licensing_intermediaries (organized/constrained) â secondary beneficiary that collects transaction rents
 *   - transformative_creators (moderate/constrained) â primary target bearing burden of proof and licensing costs
 *   - educational_institutions (organized/constrained) â institutional target facing compliance-driven extraction
 *   - documentary_filmmakers (moderate/constrained) â target whose production costs are inflated by clearance requirements
 *   - copyright_defense_bar (analytical/analytical) â observer seat that sees the structural tilt
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.84).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.76).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Property Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '9e3aad0a-a29f-48f4-83c4-fb2722c24b33').
narrative_ontology:cs_kernel_codification('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', formalized).
narrative_ontology:cs_authority_grounding('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', lineage).
narrative_ontology:cs_interpretation_layer_present('9e3aad0a-a29f-48f4-83c4-fb2722c24b33').
narrative_ontology:cs_reading_relation('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', foundational, copyright_as_traditional_property).
narrative_ontology:cs_axiom_status(copyright_as_traditional_property, holdable).
narrative_ontology:cs_axiom_grounding('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', copyright_as_traditional_property, conventional).
narrative_ontology:cs_axiom('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', foundational, fair_use_defendant_burden).
narrative_ontology:cs_axiom_status(fair_use_defendant_burden, holdable).
narrative_ontology:cs_axiom_grounding('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', fair_use_defendant_burden, conventional).
narrative_ontology:cs_reference_frame('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', strong_property_exclusivity).
narrative_ontology:cs_drift_state('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', post_campbell_transformative_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e3aad0a-a29f-48f4-83c4-fb2722c24b33', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, major_rights_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, content_licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control large catalogs of expressive works and monetize them through licensing, litigation, and statutory damage threats. They advance the property framing of copyright to resist broad fair use findings and maintain licensing revenue. They can forum-shop, lobby legislatures, and shape international treaty discourse.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, major_rights_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Operate clearinghouses and collective management organizations that collect per-use fees. They benefit from transactional friction because a narrow fair use doctrine expands the domain of licensable uses. Their business model depends on the default rule being payment-for-use rather than free reuse.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, content_licensing_intermediaries, beneficiary,
    organized, biographical, constrained, national).

% Produce remixes, appropriation art, commentary, and fan works that require engaging pre-existing copyrighted material. Under the narrow reading they bear the burden of proving fair use, face statutory damages risk, and often abandon projects or pay licenses that exceed their budgets. Their creative practice is structurally tied to existing culture, so exit means ceasing the practice.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, transformative_creators, payer,
    moderate, biographical, constrained, national).

% Universities and schools that distribute excerpts, build digital course packs, and perform displays in classrooms. The narrow reading requires licensing for uses previously treated as fair, increasing material costs and administrative overhead. Compliance is mandated by institutional risk management, limiting exit.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, payer,
    organized, biographical, constrained, national).

% Produce nonfiction films requiring archival footage, news clips, and incidental music. The narrow reading raises errors-and-omissions insurance premiums and forces expensive clearances, sometimes altering editorial choices to avoid liability. They are price-takers in a licensing market with few alternatives.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, national).

% Represents defendants in infringement actions and advises clients on fair use risk. They observe that the narrow reading systematically shifts doctrinal weight toward commercial harm and away from social benefit, increasing client exposure and chilling lawful speech.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_defense_bar, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, major_rights_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a tradable property right in expressive works, enabling a licensing market and predictable exclusivity that lowers transaction costs between creators and commercial users willing to pay.
% TRANSFER_FUNCTION: Moves money and permissions-control from downstream users (transformative creators, educators, documentary producers) to incumbent rights holders and licensing intermediaries, by treating fair use as a narrow exception rather than a robust privilege.
% ABSENT_VOICES: Transformative creators who abandon projects due to clearance uncertainty; educators who self-censor course materials to avoid licensing fees; documentary filmmakers unable to secure distribution because of errors-and-omissions insurance requirements; all are structurally absent from the adjudicative frame that treats copyright as ordinary property.
% DISAPPEARANCE_RATIONALE: If the narrow defense reading disappeared, licensing volumes would shift, documentary and educational production costs would drop, and the jurisprudential balance between rights holders and users would reorganize around a broader fair use privilege.
% FOUNDING_PROBLEM: How to incentivize the creation and publication of expressive works by granting authors temporary exclusivity against copiers, while preserving a limited public privilege for commentary, education, and transformative reuse.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders and the copyright bar assert the incentive rationale remains live and requires maximal market control. Documentary filmmakers, library associations, and independent empirical economists contest this, arguing the incentive margin was reached decades ago; corroboration from the latter group is attested in amicus briefs and economic impact studies.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.84, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84) because the narrow reading forces payment or litigation for a wide swath of socially beneficial uses that do not substitute for the original. Suppression is substantial (0.76) because the constraint depends on statutory damages, preliminary injunctions, and chilling letters to prevent unauthorized uses from proliferating. Theater ratio is moderate-high (0.48): the four-factor test and the rhetoric of balance perform a neutrality that the factor weighting consistently resolves toward rights holders. Accessibility collapse is high (0.72) because the property framing causes users to experience licensing as the only legitimate path once they understand the doctrine. Resistance is moderate (0.58) because documentary associations, library groups, and the defense bar actively contest the reading, but they lack the leverage to change it.
 *
 * PERSPECTIVAL GAP:
 *   From the rights-holder seat the arrangement looks like necessary property protection that sustains investment in expressive works. From the transformative-creator or documentary-filmmaker seat the same legal structure reads as an extraction mechanism that taxes speech and culture. The divergence is structural: the same doctrine produces subsidy at one seat and surcharge at another. The engine computes this asymmetry from the beneficiary-victim declarations and the divergent exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights holders and licensing intermediaries are declared beneficiaries: they collect licensing revenue and litigation advantages, so the engine will derive low directionality (near beneficiary). Transformative creators, educational institutions, and documentary filmmakers are declared victims: they bear the costs of licensing, insurance, and abandoned projects, yielding high directionality (near target). The copyright defense bar is an observer with analytical exit; its directionality will sit near neutral. Spatial scope amplifies extraction for national-scope users because the U.S. market is large and hard to exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâincentivizing creation through temporary exclusivityâis contested as to whether it remains live at the current scope and duration. The narrow reading persists and has intensified even as empirical evidence suggests the incentive margin is over-satisfied. This creates a mandatrophy risk: the constraint's mandate may have outlived its coordination function, leaving extraction as the dominant operative logic. The measurements show rising extractiveness and theater over the interval, consistent with coordination atrophying into rent preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_naturalness_ambiguity,
    'Is the copyright-as-property framing a natural legal category or a constructed rhetorical regime that benefits incumbent rights holders?',
    'Comparative historical analysis of copyright scope expansion; empirical measurement of whether the property metaphor tracks statutory design or serves as rhetorical cover for rent-seeking.',
    'If the property frame is constructed rather than natural, the constraint loses mountain-like immunity and its extraction is reclassified as actively enforced rent-seeking, strengthening the snare-reading of the narrow defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_naturalness_ambiguity, conceptual, 'Whether the property frame is natural or constructed').

omega_variable(
    incentive_vs_extraction_margin,
    'Does the narrow fair use reading operate at the margin where additional exclusivity produces additional expressive works, or does it extract surplus beyond the incentive floor?',
    'Cross-jurisdictional economic analysis comparing production volumes and licensing revenue in regimes with broader fair use (e.g., Campbell-expansive circuits or comparative fair dealing regimes) against narrow-reading jurisdictions.',
    'If extraction exceeds the incentive margin, the coordination story becomes cover for surplus capture, pushing classification toward snare and undermining the doctrinal justification for the narrow reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_vs_extraction_margin, empirical, 'Whether the constraint extracts beyond the incentive margin').

omega_variable(
    committer_hinge_location,
    'What is the precise structural hinge that distinguishes this reading from its siblings: the status of transformativeness, the burden of proof, or the definition of market harm?',
    'Textual analysis of judicial opinions, statutory history, and doctrinal treatises to identify which factor or premise, if changed, would collapse the narrow defense reading into a sibling reading.',
    'Identifying the hinge clarifies whether the readings are coexisting positions or whether one structurally forecloses another, altering the contamination network topology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_hinge_location, conceptual, 'Structural hinge distinguishing sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fair_tr_t10, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(fair_tr_t20, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(fair_tr_t30, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 40, 0.46).
narrative_ontology:measurement(fair_tr_t50, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(fair_be_t10, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(fair_be_t20, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(fair_be_t30, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(fair_be_t50, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 50, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fair_su_t10, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(fair_su_t20, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(fair_su_t30, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(fair_su_t40, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(fair_su_t50, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, market_licensing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fair_use_statutory_exception kernel. It is structurally distinct from transformative_right_reading (which foregrounds cultural production) and market_licensing_reading (which eliminates fair use wherever a license is possible). Decomposition follows the Îµ-invariance principle: the narrow defense reading has a higher base_extractiveness than a genuine coordination mechanism would, and a different beneficiary/victim structure than a pure public-right reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
