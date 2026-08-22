% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__public_safety_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__public_safety_coordination, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: licensing_statute_mandate__public_safety_coordination
 *   human_readable: Occupational Licensing as Minimum Competence Coordination
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   This story authors ONE reading of the licensing_statute_mandate kernel:
 *   the public-safety-coordination reading, under which statutory credential
 *   requirements exist to prevent consumer harm by establishing a verified
 *   minimum competence floor. Under this reading, consumers who cannot
 *   inspect practitioner competence before purchase are the beneficiary set,
 *   incompetent or unqualified practitioners who are excluded are the victim
 *   set (their exclusion is the mechanism doing the protective work), and the
 *   arrangement coordinates around a shared, legible quality threshold. This
 *   reading's ε is low (0.22) because, from the reading's own lights, the
 *   standard tracks a genuine and continuing harm-prevention function rather
 *   than an artificial barrier. Sibling readings — rent_seeking_suppression
 *   (labor-supply restriction for incumbent rent extraction) and
 *   graduated_access_filter (class-sorting via differential resource access
 *   to meet the barrier) — are separate constraint stories with their own ε
 *   values and are NOT blended into this one; per the ε-invariance principle,
 *   if the extraction reading differed sharply from the safety reading, that
 *   is exactly the signal that these are different constraints on the same
 *   kernel, not one constraint measured two ways.
 *
 * KEY AGENTS:
 *   - licensing_board: agenda_setter (institutional/analytical) — administers and enforces the standard
 *   - consumers_of_licensed_services: beneficiary (powerless/constrained) — relies on the credential as a competence signal
 *   - competent_licensed_practitioners: beneficiary/payer (moderate/constrained) — bears training cost, gains signaling value
 *   - incompetent_or_unqualified_practitioners: payer (powerless/trapped) — excluded by the competence threshold
 *   - consumer_protection_agencies: observer (institutional/analytical) — tracks harm outcomes independent of the board
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__public_safety_coordination, 0.22).
domain_priors:suppression_score(licensing_statute_mandate__public_safety_coordination, 0.35).
domain_priors:theater_ratio(licensing_statute_mandate__public_safety_coordination, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, extractiveness, 0.22).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__public_safety_coordination, rope).
narrative_ontology:human_readable(licensing_statute_mandate__public_safety_coordination, "Occupational Licensing as Minimum Competence Coordination").
narrative_ontology:topic_domain(licensing_statute_mandate__public_safety_coordination, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__public_safety_coordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__public_safety_coordination, '9f593f53-e587-47a4-9a95-1eb5ebd48405').
narrative_ontology:cs_kernel_codification('9f593f53-e587-47a4-9a95-1eb5ebd48405', formalized).
narrative_ontology:cs_authority_grounding('9f593f53-e587-47a4-9a95-1eb5ebd48405', extraction).
narrative_ontology:cs_interpretation_layer_present('9f593f53-e587-47a4-9a95-1eb5ebd48405').
narrative_ontology:cs_reading_relation('9f593f53-e587-47a4-9a95-1eb5ebd48405', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_reading_relation('9f593f53-e587-47a4-9a95-1eb5ebd48405', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('9f593f53-e587-47a4-9a95-1eb5ebd48405', foundational, competence_verification_solves_information_asymmetry).
narrative_ontology:cs_axiom_status(competence_verification_solves_information_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('9f593f53-e587-47a4-9a95-1eb5ebd48405', competence_verification_solves_information_asymmetry, empirically_contingent).
narrative_ontology:cs_axiom('9f593f53-e587-47a4-9a95-1eb5ebd48405', secondary, exclusion_of_unqualified_is_the_intended_protective_mechanism).
narrative_ontology:cs_axiom_status(exclusion_of_unqualified_is_the_intended_protective_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('9f593f53-e587-47a4-9a95-1eb5ebd48405', exclusion_of_unqualified_is_the_intended_protective_mechanism, instrumental).
narrative_ontology:cs_reference_frame('9f593f53-e587-47a4-9a95-1eb5ebd48405', consumer_harm_prevention_mandate).
narrative_ontology:cs_drift_state('9f593f53-e587-47a4-9a95-1eb5ebd48405', contemporary_licensing_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f593f53-e587-47a4-9a95-1eb5ebd48405', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, consumers_of_licensed_services).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, competent_licensed_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, incompetent_or_unqualified_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, aspiring_practitioners_meeting_standard).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, competent_licensed_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, aspiring_practitioners_meeting_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the minimum competence standard: education hours, examinations, continuing education, and disciplinary enforcement against practitioners who fail to meet or maintain the standard. Justifies its existence by pointing to documented harm from unqualified practice (botched procedures, financial malpractice, structural failures) that the standard is built to prevent.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, licensing_board, agenda_setter,
    institutional, generational, analytical, national).

% Cannot verify practitioner competence themselves before purchase (an electrician's wiring, a surgeon's technique, an accountant's filings are not inspectable in advance) and rely on the credential as a costly-to-fake signal that a floor of competence was met. Bear the cost of the credential indirectly through service prices, but are shielded from the tail risk of catastrophic incompetent practice.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, consumers_of_licensed_services, beneficiary,
    powerless, immediate, constrained, local).

% Pay the cost of training, examination, and renewal, but benefit from a credential that signals their competence to consumers who cannot otherwise assess it and from a floor that keeps the worst-quality practitioners out of the market they compete in.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, competent_licensed_practitioners, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, competent_licensed_practitioners, payer).

% Are excluded from practicing, or from practicing legally, because they cannot meet the competence threshold. From this reading's perspective their exclusion is the mechanism doing the protective work, not a side effect — the credential exists specifically to keep them from causing the harm the standard defines.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, incompetent_or_unqualified_practitioners, payer,
    powerless, immediate, trapped, local).

% Undertake the training and examination costs required to enter the field. Under this reading, the cost is the price of demonstrating genuine competence, and they will benefit from the same signaling value once credentialed.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, aspiring_practitioners_meeting_standard, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, aspiring_practitioners_meeting_standard, beneficiary).

% Track harm incidents attributable to unlicensed or under-competent practice and evaluate whether the licensing regime's harm-prevention function is operating as designed, independent of the licensing board's own self-assessment.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, consumer_protection_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__public_safety_coordination, consumers_of_licensed_services).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__public_safety_coordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine information asymmetry: consumers cannot verify practitioner competence before purchase in domains where incompetent practice causes serious, sometimes irreversible harm (medical, structural, financial, electrical). The credential is a coordination device that lets consumers rely on a costly, verified signal rather than assessing competence themselves case by case.
% TRANSFER_FUNCTION: Moves training and examination costs from the public (which would otherwise bear the diffuse cost of incompetent practice through injury, fraud, and structural failure) onto practitioners seeking entry, who internalize the cost of proving competence before being permitted to practice.
% ABSENT_VOICES: Unlicensed practitioners who believe themselves competent but cannot pass or afford the formal credentialing process are not represented in the board's rulemaking; their exclusion is treated as the intended outcome of the safety standard, not a grievance to be weighed.
% DISAPPEARANCE_RATIONALE: If the credential requirement vanished overnight, consumers would lose the pre-verified competence signal for high-stakes services; some would face increased risk of harm from unqualified practice, and markets would need to develop substitute signals (reputation platforms, insurance-backed guarantees, private certification) to fill the gap, which take time to mature and may not cover all consumers equally.
% FOUNDING_PROBLEM: Consumers were suffering documented harm — malpractice, structural failure, financial loss — from practitioners with no verifiable minimum competence, in domains where consumers cannot inspect quality before purchase and where failure is costly or irreversible.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection agencies and tort/malpractice litigation records, external to the licensing boards themselves, continue to document harm incidents from unlicensed practice in comparable unregulated markets, corroborating that the underlying information-asymmetry and harm risk the standard targets remains present.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__public_safety_coordination, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__public_safety_coordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__public_safety_coordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__public_safety_coordination, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__public_safety_coordination, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__public_safety_coordination_tests).
:- end_tests(licensing_statute_mandate__public_safety_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.22) and roughly flat over the interval because, under this reading, the credentialing cost tracks a real and durable coordination function (verified competence signaling) rather than growing rent extraction. Suppression is moderate (0.35): the standard does exclude those who cannot meet it, and that exclusion is enforced, but it is bounded by an objective, testable threshold rather than open-ended gatekeeping. Theater ratio is low and only mildly rising (0.10 to 0.15) — the exam and continuing-education apparatus does real verification work, though some drift toward procedural compliance over substantive competence testing is plausible over four decades. Accessibility collapse (0.40) is moderate rather than high: informal, non-credentialed alternatives (word-of-mouth referral, uninsured practice) persist in most licensed fields, they are simply disfavored and riskier for consumers, not eliminated.
 *
 * DIRECTIONALITY LOGIC:
 *   Consumers are beneficiaries with low derived d: they cannot see the underlying competence directly, but the credential subsidizes their ability to select trustworthy providers, so the constraint operates in their favor even though they bear its cost indirectly through price. Competent practitioners are near-symmetric, paying entry costs but capturing the signaling benefit. Incompetent or unqualified practitioners sit at the target end (trapped, powerless): under this reading their exclusion is not incidental damage but the constraint's designed function, which is precisely why this reading requires them explicitly in the victim set even though the coordination function is genuine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented consumer harm from unverifiable competence) is authored as still live, corroborated by consumer protection agencies and litigation records external to the licensing boards themselves — this blocks a mandatrophy finding under this reading. Divergence between this finding and a mandatrophy finding under the rent_seeking_suppression sibling reading is expected and is not a contradiction to resolve; it is the structural fact that the kernel supports genuinely different classifications depending on which premise about the standard's operative function is adopted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_vs_rent_extraction_ratio,
    'What proportion of the observed licensing barrier tracks genuine harm-prevention value versus incumbent rent extraction, for a given licensed occupation?',
    'Compare harm-incident rates and consumer outcomes across states/jurisdictions with materially different licensing stringency for the same occupation, controlling for market size and demand; a persistent harm-rate difference tracking stringency supports the safety-coordination reading, while stringency uncorrelated with harm outcomes but correlated with incumbent wages supports the rent-seeking reading.',
    'If harm outcomes track stringency strongly, this reading''s low ε is well-grounded; if stringency is uncorrelated with harm but correlated with incumbent income, the safety framing should be treated as this reading''s genuinely-held premise rather than as an empirically settled fact, and the rent-seeking sibling reading becomes the better-supported account of the same statute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_vs_rent_extraction_ratio, empirical, 'Whether the credential barrier''s magnitude is explained by harm prevention or rent extraction.').

omega_variable(
    threshold_calibration_ambiguity,
    'Is the specific competence threshold set at the level that minimizes total harm (safety-optimal), or is it set above that level to also restrict entry (rent-augmented), even if some genuine safety function exists?',
    'Engineering/cost-benefit analysis of whether requirements beyond a certain training or examination threshold produce measurably lower harm rates, versus requirements that add cost without harm-rate improvement.',
    'A threshold calibrated above the harm-minimizing point would mean this reading''s ε understates the true extractive component riding on top of a genuine safety core — the constraint would still coordinate on safety at the margin but carry an additional rent-seeking layer better captured by the sibling reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_calibration_ambiguity, conceptual, 'Whether the threshold level itself is safety-optimal or extraction-augmented.').

omega_variable(
    reading_selection_evidence,
    'What signals guided the selection of the public_safety_coordination framing for this story rather than treating the kernel as inherently ambiguous?',
    'This story was authored per the manifest''s explicit assignment of this reading; the alternative framings (rent_seeking_suppression, graduated_access_filter) are separately authored constraints on the same kernel rather than folded into this one, per the ε-invariance decomposition rule.',
    'If a reader concludes the safety framing is not defensible as a genuinely held premise for any real actor in this domain, this story''s claimed_type and low ε would need re-examination as a possible false-summit case rather than a clean rope reading — but per the committer frame, that re-examination belongs to a fourth story or to comparative analysis across the three linked readings, not to a rewrite of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidence, conceptual, 'Documents the framing choice underlying this reading''s selection, per the kernel/reading discipline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__public_safety_coordination, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__public_safety_coordination, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lice_tr_t8, licensing_statute_mandate__public_safety_coordination, theater_ratio, 8, 0.11).
narrative_ontology:measurement(lice_tr_t16, licensing_statute_mandate__public_safety_coordination, theater_ratio, 16, 0.12).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__public_safety_coordination, theater_ratio, 24, 0.13).
narrative_ontology:measurement(lice_tr_t32, licensing_statute_mandate__public_safety_coordination, theater_ratio, 32, 0.14).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__public_safety_coordination, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(lice_be_t8, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(lice_be_t16, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 24, 0.21).
narrative_ontology:measurement(lice_be_t32, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 32, 0.21).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(licensing_statute_mandate__public_safety_coordination, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__public_safety_coordination, identity_coordination).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__public_safety_coordination, 0.08).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the licensing_statute_mandate kernel, each authored as a separate ε-invariant constraint story per the ε-invariance decomposition principle. public_safety_coordination (this story, rope, ε≈0.22) treats the credential as a genuine competence-signaling coordination device with incompetent practitioners as the victim set. rent_seeking_suppression (tangled_rope or snare, higher ε) treats the same statute as incumbent labor-supply restriction, with excluded qualified entrants as the victim set and incumbents as beneficiaries. graduated_access_filter (tangled_rope, moderate-high ε) treats the same statute as a class-sorting mechanism where prior resource access, not competence, determines who clears the barrier, with under-resourced aspiring entrants as the victim set. The three stories share the statutory text as their common kernel but diverge in operative premise, beneficiary/victim structure, and ε, and are linked via network edges rather than merged into one story with an averaged or hedged extraction value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
