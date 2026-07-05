% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Domain-Split Category Membership (Sex for Medical/Sports/Safety, Gender Identity for Social/Legal Recognition)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This story instantiates the hybrid contextual reading of the woman/female
 *   category kernel: institutions assign biological sex as the operative
 *   category in medical, sports, and safety contexts, and gender identity as
 *   the operative category in social and legal recognition contexts. This is
 *   not a synthesis that resolves the underlying dispute — it is an
 *   administrative allocation rule that lets institutions avoid committing to
 *   a single axis, at the cost of subordinating each advocacy position's
 *   preferred axis in whichever domain that position considers most material.
 *   The rule increasingly requires active boundary-policing (litigation over
 *   which domain a disputed case falls into, e.g. whether a given facility or
 *   record is 'social' or 'safety'-classified) as edge cases accumulate at
 *   the domain seams.
 *
 * KEY AGENTS:
 *   - institutional_conflict_managers: primary beneficiary (institutional/analytical) — collects reduced litigation and conflict exposure from having a defensible, purpose-matched rule
 *   - trans_women_in_medical_and_sports_contexts: primary target in clinical/competitive domains (powerless/trapped) — bears category subordination where stakes are highest for them
 *   - cis_women_in_legal_and_social_recognition_disputes: primary target in legal/social domains (moderate/constrained) — bears category subordination where stakes are highest for them under this framing
 *   - courts_and_legislatures: analytical observer and secondary agenda-setter — adjudicates the boundary disputes the split itself generates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.52).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.48).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Domain-Split Category Membership (Sex for Medical/Sports/Safety, Gender Identity for Social/Legal Recognition)").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '72514292-28f0-4c15-bb14-ba53d71b32c0').
narrative_ontology:cs_kernel_codification('72514292-28f0-4c15-bb14-ba53d71b32c0', distributed).
narrative_ontology:cs_authority_grounding('72514292-28f0-4c15-bb14-ba53d71b32c0', distributed).
narrative_ontology:cs_reading_relation('72514292-28f0-4c15-bb14-ba53d71b32c0', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('72514292-28f0-4c15-bb14-ba53d71b32c0', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('72514292-28f0-4c15-bb14-ba53d71b32c0', foundational, category_axis_is_purpose_relative).
narrative_ontology:cs_axiom_status(category_axis_is_purpose_relative, holdable).
narrative_ontology:cs_axiom_grounding('72514292-28f0-4c15-bb14-ba53d71b32c0', category_axis_is_purpose_relative, instrumental).
narrative_ontology:cs_axiom('72514292-28f0-4c15-bb14-ba53d71b32c0', secondary, institutional_function_determines_operative_definition).
narrative_ontology:cs_axiom_status(institutional_function_determines_operative_definition, holdable).
narrative_ontology:cs_axiom_grounding('72514292-28f0-4c15-bb14-ba53d71b32c0', institutional_function_determines_operative_definition, conventional).
narrative_ontology:cs_created_at('72514292-28f0-4c15-bb14-ba53d71b32c0', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, regulatory_bodies_seeking_liability_shields).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women_in_medical_and_sports_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cis_women_in_legal_and_social_recognition_disputes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sports_governing_bodies).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, gender_identity_advocacy_organizations).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocacy_organizations).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, context_sensitive_category_individuation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hospitals, sports federations, courts, and legislatures that must produce a working rule rather than resolve the underlying philosophical dispute. They draft context-specific eligibility and recognition policies (biological sex for clinical risk stratification and competitive sport, self-identified gender for legal documents and everyday social recognition) and defend the split as the only administratively survivable compromise. They bear reputational and litigation risk either way but control which contexts get which rule, and they collect the institutional benefit of a reduced-conflict operating rule even when neither advocacy side is satisfied.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers, beneficiary).

% In clinical settings, sports eligibility panels, and safety-classified spaces, the hybrid rule assigns them to their biological sex category regardless of their legally recognized gender elsewhere. They experience the split as a category demotion precisely where stakes are most acute (competition eligibility, risk-stratified medical protocols, sex-segregated safety facilities) even though the same institutions recognize their gender identity on IDs and in most social contexts. There is no exit from the medical or competitive domain that does not mean forfeiting participation.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_women_in_medical_and_sports_contexts, payer,
    powerless, biographical, trapped, national).

% In legal recognition contexts, single-sex space policy, and some social/organizational settings, the hybrid rule defers to self-identified gender, which these stakeholders argue subordinates their claim to sex-based protections precisely in domains (legal sex-segregated services, some prizes, some record categories) they consider adjacent to safety and fairness rather than purely social. They can litigate or lobby for domain reclassification but cannot exit the jurisdiction's legal category system.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, cis_women_in_legal_and_social_recognition_disputes, payer,
    moderate, biographical, constrained, national).

% Set eligibility criteria using biological sex categories under the hybrid rule, absorbing litigation and public backlash from both directions. They benefit from a defensible rule but pay in credibility and legal exposure regardless of where they draw the line, and international federations face inconsistent hybrid-rule application across national jurisdictions.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sports_governing_bodies, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, sports_governing_bodies, payer).

% Use biological sex for risk stratification (drug dosing, screening protocols, anatomical procedures) under the hybrid rule while using patients' gender identity for records, address, and interpersonal treatment. They generally regard this split as clinically necessary rather than political, but must navigate patient distress when the domain-switch itself is experienced as a status judgment.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, medical_providers_and_researchers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, medical_providers_and_researchers, observer).

% Argue the hybrid rule concedes their core claim in the domains that matter most for material outcomes (competition, medical dignity) while granting it only where it costs institutions little (documents, pronouns). They are consulted in some policy processes but their preferred single-axis resolution (gender identity governs everywhere) is not adopted; the hybrid compromise is presented to them as the achievable ceiling.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_identity_advocacy_organizations, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, gender_identity_advocacy_organizations, payer).

% Argue the hybrid rule concedes their core claim only in the domains institutions find easiest to defend (sport, clinical risk) while abandoning it in legal and many social recognition contexts they consider equally material (single-sex services, some legal protections). They are consulted in some policy processes but their preferred single-axis resolution (biological sex governs everywhere) is not adopted either.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocacy_organizations, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocacy_organizations, payer).

% Adjudicate disputes arising at the domain boundaries the hybrid rule creates — cases where a party disputes which category should govern a specific context (a locker room, a prize category, a medical record dispute). Their rulings incrementally redraw which domains count as 'medical/sports/safety' versus 'social/legal,' making the boundary itself contested terrain rather than a settled line.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, courts_and_legislatures, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides institutions (hospitals, sports bodies, courts, registries) with an administrable rule that avoids requiring a single metaphysical resolution of 'what a woman is' before any policy can be written — different institutional functions get different operative definitions matched to their stated purpose (risk management, competitive fairness, legal recognition).
% TRANSFER_FUNCTION: Moves the cost of definitional conflict from institutions (which get a workable, defensible split rule) onto whichever group's preferred category is subordinated in a given domain — trans women bear the cost in medical/sports/safety domains; cis women (per sex-based rights advocates) bear the cost in legal/social recognition domains.
% ABSENT_VOICES: Neither gender identity advocacy organizations nor sex-based rights advocacy organizations get their single-axis resolution; both are consulted but the hybrid outcome is presented to each as an imposed compromise rather than a negotiated agreement, and detransitioned individuals and intersex people are rarely given a distinct seat in either domain's rulemaking despite being materially affected by the boundary-drawing.
% DISAPPEARANCE_RATIONALE: Institutions dispute whether the hybrid rule's disappearance would collapse into administrative chaos (their view: no coherent single-axis replacement exists that avoids the same conflicts) or would simply force overdue adoption of one coherent axis (both advocacy sides' view, though they disagree on which axis). The rule's apparent stability may reflect institutional risk-aversion more than a genuinely settled resolution of the underlying category dispute.
% FOUNDING_PROBLEM: Institutions faced incompatible pressures: clinical and competitive contexts have measurable biological correlates relevant to their stated purposes (drug metabolism, athletic performance categories), while legal and social recognition contexts had shifted toward self-identification as the basis of dignity and non-discrimination protection. No single-axis rule satisfied the design goals institutions were separately accountable for.
% FOUNDING_PROBLEM_CORROBORATION: Medical and sports governing bodies attest the domain-split reflects genuine functional differences between contexts and defend it as principled. Both gender identity advocacy organizations and sex-based rights advocacy organizations, from opposing directions, attest the split is not a principled resolution but an institutional risk-management artifact that happens to subordinate their claim exactly where the stakes are highest for them. Independent legal scholarship on the incoherence of shifting category boundaries across adjacent domains (e.g., the same facility being classified differently for medical versus social purposes) provides some corroboration from outside either benefiting party that the split is administratively motivated rather than conceptually derived.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, contested).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) is moderate rather than high because the hybrid rule genuinely solves an institutional coordination problem (matching category to stated functional purpose) rather than existing purely to extract; but it is non-trivial because the domain-split itself becomes a lever institutions use to avoid harder commitments, and both major stakeholder groups experience real material subordination in their highest-stakes domain. Suppression (0.48) reflects growing enforcement of the domain boundary itself — courts and regulators increasingly have to police which category applies where, rather than settling on one axis. Theater ratio (0.42) is substantial: much of the domain-boundary litigation and policy revision activity manages the appearance of principled distinction-drawing rather than deriving the medical/social split from a coherent theory of why exactly these domains and not others get which axis.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional conflict managers sit near the beneficiary end: they get a workable rule and reduced aggregate conflict exposure even though individual cases remain contentious. Trans women in medical/sports/safety contexts sit near the target end for those domains specifically — trapped exit, powerless position, high stakes (competition eligibility, clinical dignity). Cis women in legal/social recognition contexts sit as targets for those domains under the sex-based-rights framing — moderate power, constrained exit, high stakes in legal protections and single-sex service access. Both advocacy organizations are structurally excluded from dictating the final rule even where they are consulted, which is why they carry the excluded role alongside payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid rule resists mislabeling as either pure coordination (it is not a neutral solution both sides embrace) or pure extraction (it does solve a genuine cross-domain administrability problem that a single rigid axis would not solve equally well). Classifying it tangled_rope captures both: a real coordination function (letting differently-purposed institutions operate without resolving a contested metaphysical question first) bundled with asymmetric extraction (each domain's losing party bears real costs) sustained by active enforcement (ongoing boundary litigation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_principled_or_administrative,
    'Is the medical/sports/safety versus social/legal domain split derived from a coherent principle about what each domain is actually tracking, or is it an administrative artifact that happens to distribute conflict in whichever way minimizes institutional liability?',
    'Examine whether the domain boundaries are stable and predictable from a stated functional theory (e.g., ''domains requiring physiological risk-relevant categorization use sex'') versus whether boundary placement shifts opportunistically in response to litigation pressure or public controversy over time.',
    'If principled, the hybrid rule is closer to a genuine rope-like coordination solution with incidental costs; if administrative/opportunistic, the coordination story is largely cover for a conflict-minimization mechanism that is substantially extractive of both flanks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_principled_or_administrative, conceptual, 'Whether the domain split reflects a principled theory or ad hoc institutional risk management.').

omega_variable(
    kernel_reading_selection_neutrality,
    'Is the choice to adopt a hybrid contextual reading itself neutral between the two single-axis positions, or does it structurally favor one side by default (e.g., because self-identification governs more of daily social life while sex governs comparatively rarer high-stakes events, or vice versa)?',
    'Comparative audit of how much lived time and how many material decisions fall under each domain category for the average person, weighted by stakes, to determine whether the hybrid split is actually balanced or tilts toward one reading''s preferred axis by sheer domain frequency and weight.',
    'If the split is not actually balanced, the ''compromise'' framing is itself misleading, and the tangled_rope classification would need to be revisited toward a reading closer to whichever single-axis position the hybrid rule functionally approximates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_neutrality, empirical, 'Whether the hybrid split is a genuine balance between the two single-axis readings or tilts toward one in practice.').

omega_variable(
    committer_framing_alternative,
    'Is the correct unit of analysis ''the woman/female category as adjudicated by institutions'' (the framing adopted here, which treats institutions as the kernel authority) or ''the woman/female category as a matter of individual metaphysical fact that institutions merely fail to track correctly'' (a framing under which the hybrid rule would not be a legitimate reading at all, but simply institutional error)?',
    'This is a framing choice rather than an empirically resolvable question; it depends on whether one takes institutional adjudication practices as constitutive of the operative category (a legal-positivist stance) or as fallible attempts to track an independent fact (a realist stance about sex or gender).',
    'Under the institutional-adjudication framing (adopted here), the hybrid rule is a coherent, classifiable constraint. Under the independent-fact framing, this entire story would be recharacterized as documenting sustained institutional error rather than a legitimate contextual reading, and the tangled_rope classification would shift toward snare (extraction dressed as coordination, with no genuine coordination function since the ''coordination problem'' itself is a symptom of institutional confusion about a settled fact).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether the hybrid reading is a legitimate framing or a symptom of institutional error under a realist metaphysics of the underlying category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__hybrid_contextual_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__hybrid_contextual_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__hybrid_contextual_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__hybrid_contextual_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__hybrid_contextual_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(woma_be_t4, woman_female_category__hybrid_contextual_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(woma_be_t8, woman_female_category__hybrid_contextual_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(woma_be_t12, woman_female_category__hybrid_contextual_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(woma_be_t16, woman_female_category__hybrid_contextual_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(woma_be_t20, woman_female_category__hybrid_contextual_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t4, woman_female_category__hybrid_contextual_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(woma_su_t8, woman_female_category__hybrid_contextual_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(woma_su_t12, woman_female_category__hybrid_contextual_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(woma_su_t16, woman_female_category__hybrid_contextual_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(woma_su_t20, woman_female_category__hybrid_contextual_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__hybrid_contextual_reading, 0.1).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% This story is the third member of the woman_female_category kernel family, alongside sex_biology_reading and gender_identity_reading. Each single-axis reading would produce a uniform beneficiary/victim structure across all contexts (biology-governs-all favors sex-based rights positions everywhere; identity-governs-all favors gender-identity positions everywhere). This hybrid reading instead fragments the victim set by domain and introduces a distinct beneficiary class (institutional conflict-minimizers) absent from either single-axis reading, plus a new extraction mechanism (domain-boundary litigation) that neither sibling generates on its own.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
