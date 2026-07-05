% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Gender-Identity Reading of the Category 'Woman'
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'woman' category
 *   kernel: the gender-identity reading, under which category membership
 *   follows internal identity rather than birth-sex biology or an
 *   intersex-accommodating biological spectrum. Under this reading, a person
 *   who identifies as a woman is a woman for legal and institutional purposes
 *   regardless of chromosomal or anatomical configuration. The ε profile is
 *   deliberately uneven across domains folded into one story only where the
 *   underlying transfer mechanism is the same rule (identity-attestation as
 *   the operative test): moderate extraction in identity-document policy
 *   (largely genuine coordination reducing gatekeeping harm) and
 *   substantially higher extraction where sex-segregated categories exist
 *   specifically because of birth-sex-linked physiology or vulnerability
 *   (elite sports, carceral housing, trauma-informed single-sex services).
 *   The sibling readings — sex_biology_reading and
 *   intersex_accommodation_reading — are separate constraints with their own
 *   ε, beneficiary/victim sets, and stakeholders; they are not described here
 *   beyond the omega and cs_structure fields that route the committer contest
 *   to its proper location.
 *
 * KEY AGENTS:
 *   - transgender_women: primary beneficiary (moderate/identity_locked) — gains recognition and access under this reading
 *   - identity_verification_administrators: agenda_setter (institutional/arbitrage) — writes and enforces the self-attestation standard
 *   - natal_female_athletes: payer (moderate/constrained) — bears competitive cost in sex-segregated sport
 *   - sex_based_service_providers: payer (moderate/constrained) — bears legal/funding risk maintaining birth-sex-only spaces
 *   - detained_natal_women: payer (powerless/trapped) — bears the highest-stakes cost with least voice
 *   - gender_critical_feminists: excluded — raises birth-sex-based objection but is frequently denied institutional standing
 *   - courts_and_legislatures: observer (institutional/analytical) — adjudicates across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.58).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.5).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Gender-Identity Reading of the Category 'Woman'").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '9f9eb789-75a0-4fc6-8584-538b1aa76b8a').
narrative_ontology:cs_kernel_codification('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', distributed).
narrative_ontology:cs_authority_grounding('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', distributed).
narrative_ontology:cs_reading_relation('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', foundational, gender_identity_is_the_determinative_criterion).
narrative_ontology:cs_axiom_status(gender_identity_is_the_determinative_criterion, holdable).
narrative_ontology:cs_axiom_grounding('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', gender_identity_is_the_determinative_criterion, deontological).
narrative_ontology:cs_axiom('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', secondary, birth_sex_based_exclusion_is_presumptively_discriminatory).
narrative_ontology:cs_axiom_status(birth_sex_based_exclusion_is_presumptively_discriminatory, holdable).
narrative_ontology:cs_axiom_grounding('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', birth_sex_based_exclusion_is_presumptively_discriminatory, conventional).
narrative_ontology:cs_reference_frame('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', self_attestation_recognition_standard).
narrative_ontology:cs_drift_state('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', post_sports_and_carceral_controversy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9f9eb789-75a0-4fc6-8584-538b1aa76b8a', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, identity_verification_administrators).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, natal_female_athletes).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_based_service_providers).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, detained_natal_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal, social, and institutional recognition as women consistent with lived identity, without being required to produce medical or surgical proof. Gain access to documents, facilities, and categories matching identity. Exit from the constraint would mean reverting to a legal sex marker and category exclusion they experience as a denial of who they are; this reading is the mechanism through which recognition is obtained, so their relationship to it is not merely instrumental — identity and legal standing are fused.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Government registries, HR departments, and sport/facility administrators who write and enforce self-identification policy for legal sex/gender markers. They set the evidentiary bar (self-attestation vs. medical proof), administer disputes, and bear reputational but not existential risk from either expanding or narrowing the rule.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, identity_verification_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Compete in sex-segregated categories designed around average natal-female physiology. Under this reading, category eligibility follows identity rather than birth sex, so they may face competitors with post-pubertal male-typical physiological advantages. Exit means leaving the sport or accepting reduced competitive prospects; they did not choose the rule and have limited institutional standing to contest it without being cast as discriminatory.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, natal_female_athletes, payer,
    moderate, biographical, constrained, national).

% Operate rape crisis centers, domestic violence shelters, and other single-sex spaces premised on excluding male-bodied individuals for safety and trauma-informed reasons. Under this reading, refusing entry to a self-identified woman on the basis of birth sex is treated as discriminatory, narrowing their ability to maintain a natal-sex-only space without legal or funding risk.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_service_providers, payer,
    moderate, biographical, constrained, regional).

% Incarcerated women housed in facilities that, under this reading, place self-identified transgender women in women's prisons regardless of birth sex or full transition status. They have essentially no exit — they cannot choose their housing assignment and have minimal voice in facility policy.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, detained_natal_women, payer,
    powerless, immediate, trapped, regional).

% Argue that sex-based protections exist because of birth-sex-linked vulnerabilities (physiology, socialization, reproductive capacity) that identity self-attestation cannot track, and that redefining 'woman' around identity erases the basis for those protections. Frequently characterized as bigoted for raising the objection, which limits their access to institutional venues (universities, media, policy consultations) where the rule is set.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_critical_feminists, excluded,
    organized, biographical, constrained, national).

% Adjudicate disputes between the competing readings — sports federations, prison policy, single-sex services, anti-discrimination statutes. Hear evidence and argument from all sides and can shift the operative definition of 'woman' within a given jurisdiction or context.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__gender_identity_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, low-friction rule for legal and institutional sex/gender classification that avoids requiring intrusive medical verification and allows transgender individuals to obtain documents and access matching their lived identity without protracted gatekeeping.
% TRANSFER_FUNCTION: Moves category membership and its attached rights (sports eligibility, single-sex space access, documentation) from a birth-sex-anchored basis to an identity-attestation basis, transferring standing within the 'woman' category to transgender women and transferring competitive/safety/space costs onto natal women in contexts where birth-sex-linked physiology or vulnerability was the basis for the original category boundary.
% ABSENT_VOICES: Natal female athletes, shelter operators, and incarcerated natal women rarely have direct standing in the policy-setting bodies (sports federations' inclusion committees, legislative drafting sessions) that adopt this reading; gender-critical feminists raising birth-sex-based objections are frequently excluded from platforms on the grounds that the objection itself is illegitimate, which suppresses the dissenting perspective before it reaches deliberation.
% DISAPPEARANCE_RATIONALE: If the gender-identity reading were reversed overnight, transgender women would lose legal and institutional recognition matching their lived identity in numerous jurisdictions — a significant rearrangement for that population. Natal-women-only spaces and sports categories would revert to birth-sex screening, which its proponents call a restoration and its critics call a loss. Both sides agree the world visibly rearranges; they dispute whether that rearrangement is a correction or a harm, which is why this is marked contested rather than a clean world_rearranges/world_unchanged split.
% FOUNDING_PROBLEM: Transgender people faced (and in many places still face) systemic denial of legal recognition, harassment, and exclusion from documents and spaces matching their lived identity, causing serious material and psychological harm; the identity-attestation standard was built to remove medicalized, invasive gatekeeping from that recognition process.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy organizations and many legal scholars attest the founding problem (discriminatory denial of recognition) remains live and the reading is a proportionate remedy. Independent voices outside the beneficiary set — including some longstanding feminist organizations, several elite sports governing bodies' own commissioned physiology reviews, and correctional oversight bodies investigating assault incidents in mixed facilities — corroborate that the reading, as currently implemented in sports and carceral contexts, has produced measurable adverse effects on natal women that the identity-recognition rationale does not by itself resolve. No single actor speaks for both harms simultaneously; this is a genuine, unresolved collision rather than a settled genealogy.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, contested).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects a domain-weighted average across a rule that is close to costless coordination in documentation contexts but substantially extractive in contexts (sport, carceral housing, trauma-informed services) where the excluded category boundary existed precisely to track birth-sex-linked physiology or vulnerability that identity attestation does not measure. Suppression (0.5) is moderate-high: the rule is enforced through institutional policy and increasingly through anti-discrimination law that treats birth-sex-based exclusion as unlawful, which forecloses the alternative (a birth-sex test) in jurisdictions that adopt this reading, but enforcement intensity and legal consequence vary substantially by jurisdiction and domain. Resistance (0.72) is high because natal-women's-space advocates and gender-critical feminists actively contest the rule in courts, legislatures, and sport governance — this is a live, high-salience dispute, not a settled constraint. Accessibility collapse (0.42) is moderate: alternative framings (sex-based, intersex-accommodating) remain legally and politically available in many jurisdictions, so alternatives have not collapsed globally, only within jurisdictions that have adopted this reading as binding law or policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women are the structural beneficiaries of this specific reading — the rule exists to secure their recognition, and their exit from the rule (its reversal) directly removes something they hold. Identity-verification administrators sit near the beneficiary end operationally (arbitrage exit, institutional power) though they do not personally capture rents from the rule the way, say, a fee-collecting institution would; their position is closer to symmetric administrator than beneficiary, which is why role is agenda_setter rather than beneficiary. Natal female athletes, sex-based service providers, and detained natal women are the targets: the rule transfers category-defined protections and eligibility away from a birth-sex boundary that they depend on, without their consent to the redefinition, and with constrained-to-trapped exit options depending on context (an athlete can leave a sport; a detained woman cannot leave a prison).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents this reading from being mislabeled as either pure coordination (ignoring the real costs imposed on natal women in physiology-linked and safety-linked contexts) or pure extraction (ignoring the genuine and serious harm the identity-attestation standard was built to solve for transgender people historically denied recognition). Both a genuine coordination function (removing invasive gatekeeping from identity recognition) and asymmetric extraction (shifting costs onto natal women in contexts where the category boundary tracked something identity attestation does not) are present simultaneously, which is exactly the tangled-rope signature — collapsing either dimension would misdescribe the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_attestation_vs_physiological_tracking,
    'Does self-attested gender identity track the physiological and vulnerability-based facts (average strength/speed differentials, birth-sex-linked trauma responses) that many sex-segregated categories were originally built to track, or is that tracking function severed once the category is redefined around identity?',
    'Domain-specific physiological and outcomes research (sports performance studies post-transition, incident data in mixed-sex carceral facilities, trauma-informed service outcome studies) compared across jurisdictions with different rules.',
    'If tracking is preserved (e.g., through additional eligibility criteria), the extraction in high-stakes domains would be lower than authored here; if severed, the extraction in sport/carceral contexts is structurally locked in regardless of policy tuning, since the category boundary and the thing it once tracked have diverged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_attestation_vs_physiological_tracking, empirical, 'Whether identity attestation preserves or severs the physiological/vulnerability tracking function of the original sex-based category boundary.').

omega_variable(
    kernel_framing_choice,
    'Is the ''woman'' category kernel best modeled as a single ambiguous commitment with three live readings (as done here), or does the very existence of intersex bodies show that the kernel was never binary and that the sex_biology_reading is not a coherent sibling but a category error?',
    'Philosophical and legal analysis of whether ''biological sex'' as a binary predicate is coherently specifiable prior to any policy choice, versus being itself already a contested construct.',
    'If the sex_biology_reading is itself incoherent as a binary claim, the kernel structure would need to be re-modeled as two readings (identity-based vs. spectrum-based) rather than three, changing which readings can coexist versus foreclose one another.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the three-reading kernel structure itself, or the binary sex_biology_reading within it, is the more defensible framing.').

omega_variable(
    victim_beneficiary_overlap,
    'Are natal female athletes and detained natal women genuinely disjoint from the beneficiary set, or do some transgender women also experience this reading''s downstream effects (e.g., transgender women excluded from women''s spaces by hostile implementation, or transgender women who are also incarcerated and face violence from either housing assignment) as victims rather than beneficiaries?',
    'Disaggregated incident and outcome data by transgender status within carceral and shelter populations under this reading''s implementation.',
    'If substantial overlap exists, the clean beneficiary/victim split authored here understates the internal heterogeneity of harm and the tangled-rope classification would need a more granular stakeholder set distinguishing implementation contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_beneficiary_overlap, empirical, 'Whether the beneficiary and victim sets are as cleanly disjoint as authored, or overlap in implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(woma_tr_t4, woman_category__gender_identity_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(woma_tr_t8, woman_category__gender_identity_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(woma_tr_t12, woman_category__gender_identity_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(woma_tr_t16, woman_category__gender_identity_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(woma_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(woma_be_t4, woman_category__gender_identity_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(woma_be_t8, woman_category__gender_identity_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(woma_be_t12, woman_category__gender_identity_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(woma_be_t16, woman_category__gender_identity_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(woma_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t4, woman_category__gender_identity_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(woma_su_t8, woman_category__gender_identity_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(woma_su_t12, woman_category__gender_identity_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(woma_su_t16, woman_category__gender_identity_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(woma_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'what determines category membership in woman' (the woman_category kernel). sex_biology_reading anchors membership to chromosomal/anatomical/reproductive facts; intersex_accommodation_reading anchors membership to a biological-sex spectrum inclusive of intersex variation; this story (gender_identity_reading) anchors membership to self-attested identity. Each has a distinct ε (this story's ε is domain-weighted and substantially higher in physiology/vulnerability-linked contexts than the sex_biology_reading's ε would be for the same contexts), a distinct beneficiary/victim structure, and is linked via affects_constraints rather than merged into one story, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
