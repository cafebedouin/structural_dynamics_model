% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Reading of Sex/Gender Category Membership
 *   domain: political_philosophy/bioethics/law
 *
 * SUMMARY:
 *   Across sport, medicine, and law, institutions increasingly apply
 *   biological-sex criteria in contexts framed around physical safety or
 *   competitive fairness, while applying gender-identity criteria in contexts
 *   framed around legal status, social services, and anti-discrimination
 *   protection. This produces the structurally distinctive feature of the
 *   hybrid reading: the same individual can be correctly categorized as
 *   'female' in one institutional context and 'not female' (for eligibility
 *   purposes) in another, by the same overarching policy framework, without
 *   the framework itself being incoherent by its own lights — because it
 *   never claims a single universal definition, only domain-relative ones.
 *
 * KEY AGENTS:
 *   - institutional_conflict_managers: agenda_setter/beneficiary (institutional/arbitrage) — administers the domain-split, reduces institutional litigation exposure
 *   - trans_women_in_sport_and_medicine: payer (powerless/trapped) — subordinated reading in medical/sport domains despite legal/social recognition elsewhere
 *   - sex_based_rights_advocates_in_legal_recognition_contexts: payer (organized/constrained) — subordinated reading in legal/social domains despite prevailing in medical/sport domains
 *   - courts_and_legislatures: observer/agenda_setter (institutional/analytical) — adjudicates disputes arising from the split's internal inconsistency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.52).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.58).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Reading of Sex/Gender Category Membership").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '13e15ff1-9c50-4234-9f05-df347dee7640').
narrative_ontology:cs_kernel_codification('13e15ff1-9c50-4234-9f05-df347dee7640', distributed).
narrative_ontology:cs_authority_grounding('13e15ff1-9c50-4234-9f05-df347dee7640', distributed).
narrative_ontology:cs_reading_relation('13e15ff1-9c50-4234-9f05-df347dee7640', woman_female_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('13e15ff1-9c50-4234-9f05-df347dee7640', woman_female_category__gender_identity_reading, influences).
narrative_ontology:cs_axiom('13e15ff1-9c50-4234-9f05-df347dee7640', foundational, category_criterion_is_domain_relative).
narrative_ontology:cs_axiom_status(category_criterion_is_domain_relative, holdable).
narrative_ontology:cs_axiom_grounding('13e15ff1-9c50-4234-9f05-df347dee7640', category_criterion_is_domain_relative, instrumental).
narrative_ontology:cs_axiom('13e15ff1-9c50-4234-9f05-df347dee7640', secondary, no_single_universal_definition_is_required_for_institutional_legitimacy).
narrative_ontology:cs_axiom_status(no_single_universal_definition_is_required_for_institutional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('13e15ff1-9c50-4234-9f05-df347dee7640', no_single_universal_definition_is_required_for_institutional_legitimacy, conventional).
narrative_ontology:cs_reference_frame('13e15ff1-9c50-4234-9f05-df347dee7640', pre_dispute_uncontested_biological_default).
narrative_ontology:cs_drift_state('13e15ff1-9c50-4234-9f05-df347dee7640', contemporary_multi_domain_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('13e15ff1-9c50-4234-9f05-df347dee7640', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, healthcare_administrators).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women_in_sport_and_medicine).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates_in_legal_recognition_contexts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, cisgender_female_athletes).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cisgender_female_athletes).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, context_sensitivity_of_category_terms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sports federations, hospital systems, and legislatures draw the line differently by domain: biological sex for medical dosing, athletic eligibility, and safety triage; gender identity for legal name/ID recognition, social services, and anti-discrimination protection. They administer the split, write the policy carve-outs, and are the ones who benefit from having a workable rule that reduces litigation and public controversy in each domain, even though the rule satisfies no single coherent theory of what the category 'woman' means.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers, beneficiary).

% Apply biological-sex criteria (testosterone levels, developmental puberty history) to eligibility for women's competitive categories, citing safety and competitive fairness. They gain a defensible, litigation-resistant rule and shift the site of contest onto athletes rather than onto themselves. They can revise eligibility criteria unilaterally between seasons.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sports_governing_bodies, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, sports_governing_bodies, agenda_setter).

% Use biological sex for clinical purposes — organ-specific screening, medication dosing, risk stratification — while using patients' stated gender identity for intake records, room assignment, and interpersonal address. They benefit from a rule that lets them avoid a single controversial ontological commitment; the split reduces malpractice exposure and patient-relations friction.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, healthcare_administrators, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, healthcare_administrators, agenda_setter).

% In athletic and clinical contexts, are categorized by biological sex history even where their legal identity documents and social recognition affirm womanhood. This produces exclusion from women's competitive categories and, in some clinical settings, administrative friction or disclosure demands they experience as a denial of the legal/social recognition the same institutional framework grants them elsewhere. Exit from sport or the healthcare system is not a real option — either forecloses participation or access to care.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_women_in_sport_and_medicine, payer,
    powerless, biographical, trapped, national).

% Argue that sex-based protections (single-sex spaces, data collection, safeguarding provisions) should track biological sex in legal/social recognition contexts too — domestic violence shelters, prisons, changing rooms. Under the hybrid rule, legal/social recognition contexts default to gender identity, which they experience as their reading being subordinated exactly where they believe safety and privacy interests are highest. They can lobby and litigate but cannot exit the jurisdiction's legal framework without relocating.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates_in_legal_recognition_contexts, payer,
    organized, generational, constrained, national).

% Benefit from the biological-sex carve-out in athletic eligibility, which they argue preserves competitive fairness. They bear reputational cost when framed as antagonists in the broader dispute and have limited power to set eligibility policy themselves — their interests are represented by the same governing bodies that administer the hybrid rule.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, cisgender_female_athletes, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, cisgender_female_athletes, payer).

% Adjudicate disputes arising from the hybrid rule's internal inconsistency — cases where a single person's status differs by which institutional context they stand in. Courts can affirm, strike down, or further complicate the domain-splitting rule; legislatures can codify it into statute or displace it entirely.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, courts_and_legislatures, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows institutions operating across incompatible domains (safety-critical medicine and sport vs. anti-discrimination and legal recognition) to avoid committing to a single universal definition of 'woman'/'female' that would create failure in at least one domain — reduces litigation volume and public controversy per-institution by letting each domain apply the criterion that domain's specific risk profile seems to require.
% TRANSFER_FUNCTION: Moves the cost of definitional incoherence from institutions (who avoid picking a single fight) onto individuals whose identity claims fall on the wrong side of the line in a given domain — trans women lose standing in sport/clinical contexts, sex-based-rights advocates lose standing in legal/social contexts — while institutions retain administrative flexibility and reduced exposure in both.
% ABSENT_VOICES: Neither trans advocacy organizations nor sex-based-rights organizations were the primary authors of the domain-split; it emerged from institutional risk management (sports federations, hospital counsel, legislative drafters) responding to litigation pressure from both sides rather than from a negotiated settlement either side endorses. Detransitioners and intersex individuals, whose situations fit neither side of either binary cleanly, are largely absent from the framework entirely.
% DISAPPEARANCE_RATIONALE: Sports and medical institutions would say the world rearranges badly without the split — either safety/fairness criteria disappear from sport and medicine, or legal/social recognition reverts to biology-only criteria that trans people say erases their standing. Sex-based-rights advocates would say a coherent biology-based rule across all domains simply restores clarity. Trans advocates would say a coherent identity-based rule across all domains restores their standing everywhere. Because the three camps disagree about what 'the world' even is in the counterfactual, the verdict is genuinely contested rather than resolvable by evidence.
% FOUNDING_PROBLEM: Institutions faced two irreconcilable litigation and legitimacy pressures simultaneously: safety/fairness claims requiring biology-based category lines in medicine and sport, and anti-discrimination/dignity claims requiring identity-based category lines in law and social recognition. The domain-split was built to let institutions satisfy both pressures without picking a single universal definition that would fail one pressure completely.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars studying regulatory pragmatism (writing from outside both advocacy camps) describe the domain-split as a genuine institutional risk-management response to unresolved conflicting mandates, corroborating that the founding problem is live in the sense that no settled underlying definition exists. However, advocates on both sides — from outside each other's camp but each still a benefiting-or-injured party rather than a neutral third party — describe the same split as an evasion that defers rather than resolves the definitional question, meaning the 'solution' does not eliminate the founding tension so much as distribute its cost unevenly by domain.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, contested).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.52 — lower than either monistic reading would likely score for its respective subordinated group in that reading's dominant domain, because the hybrid reading distributes rather than concentrates the cost: each contesting group 'wins' in some domains and 'loses' in others. Suppression (0.58) reflects the active enforcement required to hold two incompatible criteria simultaneously — courts, legislatures, and governing bodies must continually justify why domain X uses criterion A while domain Y uses criterion B, which requires ongoing institutional maintenance rather than resting on settled consensus. Theater ratio (0.42) captures that a meaningful share of the justificatory apparatus (position papers, expert panels, ethics reviews) exists to legitimate the split after the fact rather than to derive it from first principles. Accessibility collapse is moderate (0.40): the domain-split is well-documented and contestable, not naturalized as inevitable — this is not a mountain. Resistance is high (0.72) because both major camps actively contest the framework, unlike a stable coordination equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional conflict managers, sports governing bodies, and healthcare administrators are structural beneficiaries: they gain a workable administrative rule and reduced litigation exposure without needing to resolve the underlying definitional dispute — low d. Both advocacy-aligned payer groups experience high d, but in different domains: trans women in sport/medicine bear the cost where biological-sex criteria apply; sex-based-rights advocates bear the cost where gender-identity criteria apply in legal/social contexts. This domain-conditional victimhood is the structural signature the manifest predicted — the victim set literally shifts by which institutional context is under discussion, which is why a single override was not applied uniformly across both payer groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutions facing simultaneously irreconcilable safety and dignity mandates) has not disappeared, but its status is contested precisely because the 'solution' does not resolve the tension — it relocates it to different domains for different groups. This is not classic mandatrophy (a dead problem with a persisting institution) but a live-and-unresolved problem whose institutional response has calcified into a a default policy architecture that increasingly resists revision even as both advocacy camps continue to contest it. Classifying this as tangled_rope rather than snare prevents mislabeling the coordination function (institutions genuinely need SOME way to operate across domains with different risk profiles) as pure extraction, while still registering that the arrangement has identifiable victims and requires active enforcement to hold — a pure rope reading would erase the asymmetric costs each subordinated group bears in its disadvantaged domain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_kernel_reading_boundary,
    'Is the hybrid_contextual_reading a genuinely independent normative position, or is it better understood as an unprincipled compromise that inherits the weaknesses of both monistic readings (sex_biology_reading and gender_identity_reading) without inheriting either one''s internal coherence?',
    'Philosophical analysis of whether context-relative category membership can be given a principled unifying rationale (e.g., a theory of what work the category term is doing in each domain) versus documentation that the domain lines were drawn ad hoc, institution-by-institution, in response to litigation pressure rather than derived from a stated theory.',
    'If a principled unifying rationale exists, the hybrid reading is a genuine third position with its own coordination function; if the lines are ad hoc, the hybrid reading is better modeled as an unstable settlement that will continue drifting toward one monistic reading or the other as political pressure shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_kernel_reading_boundary, conceptual, 'Whether the hybrid reading has independent philosophical standing or is an unprincipled compromise between the two monistic readings.').

omega_variable(
    institutional_beneficiary_capture,
    'Do institutional conflict managers benefit from the hybrid rule merely incidentally (as an artifact of needing SOME rule), or do they have an active interest in perpetuating definitional ambiguity because ambiguity itself reduces their liability exposure relative to a settled universal rule?',
    'Track institutional responses when courts or legislatures propose settling the definition universally in either direction — if institutions actively lobby against settlement (preferring the ambiguous status quo) rather than merely accepting whichever settlement is imposed, that supports active interest in ambiguity.',
    'If institutions actively prefer ambiguity, the tangled_rope classification is strengthened (institutions are extracting reduced-liability benefit from an arrangement that imposes real costs on both payer groups); if institutions are neutral toward settlement, the coordination function is closer to incidental and the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_beneficiary_capture, empirical, 'Whether institutional beneficiaries have an active interest in perpetuating ambiguity versus merely operating within it.').

omega_variable(
    intersex_and_detransitioner_exclusion,
    'How does the hybrid reading treat individuals (intersex people, detransitioners) whose situations do not fit cleanly into either the biological-sex or gender-identity framework in any domain?',
    'Case-law and policy review of how domain-specific rules handle intersex conditions (which complicate the ''biological sex'' criterion itself) and detransition (which complicates the stability assumption underlying gender-identity criteria).',
    'If these groups are simply absent from the framework''s design (neither benefiting nor targeted, just unaddressed), that supports the absent_voices finding and suggests the hybrid reading''s domain lines were drawn without them in view at all, widening the excluded-parties set beyond the two organized advocacy camps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_and_detransitioner_exclusion, empirical, 'Whether intersex and detransitioner populations are addressed by or simply invisible to the hybrid framework.').


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
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(woma_be_t4, woman_female_category__hybrid_contextual_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(woma_be_t8, woman_female_category__hybrid_contextual_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(woma_be_t12, woman_female_category__hybrid_contextual_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(woma_be_t16, woman_female_category__hybrid_contextual_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(woma_be_t20, woman_female_category__hybrid_contextual_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(woma_su_t4, woman_female_category__hybrid_contextual_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(woma_su_t8, woman_female_category__hybrid_contextual_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(woma_su_t12, woman_female_category__hybrid_contextual_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(woma_su_t16, woman_female_category__hybrid_contextual_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(woma_su_t20, woman_female_category__hybrid_contextual_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__hybrid_contextual_reading, 0.1).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the woman_female_category kernel. sex_biology_reading claims a single chromosomal/reproductive-anatomy criterion across all domains; gender_identity_reading claims a single self-identification criterion across all domains; this hybrid_contextual_reading claims the criterion is domain-relative. Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct claimed_type — they are linked here rather than merged because merging would violate ε-invariance (a single constraint cannot honestly carry three different extraction profiles for three incompatible criteria).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
