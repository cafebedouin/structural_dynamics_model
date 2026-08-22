% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Hybrid Contextual Reading of the Woman/Female Category (Domain-Split Definition)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This story generates ONE reading within the contested woman/female
 *   category kernel: the hybrid contextual reading, which holds that category
 *   membership is properly determined by biological sex in medical, sporting,
 *   and safety contexts, and by gender identity in social and legal
 *   recognition contexts. This is not a synthesis or a middle path evaluated
 *   neutrally — it is itself a distinct, contestable claim with its own
 *   beneficiary and victim structure, structurally separate from the
 *   sex_biology_reading and gender_identity_reading constraints (other files,
 *   linked via network.affects_constraints). The hybrid reading's
 *   institutional attractiveness is that it lets conflict-averse
 *   administrators avoid endorsing either pure reading across the board, but
 *   this deferral itself becomes an extractive structure: whichever criterion
 *   is subordinated in a given domain, the subordinated party bears the cost
 *   with no compensating recognition, and the domain-boundary-drawing power
 *   sits entirely with institutions neither contesting group controls.
 *
 * KEY AGENTS:
 *   - institutional_conflict_managers: agenda-setting beneficiary (institutional/analytical) — administers the domain split, benefits from reduced political exposure
 *   - sports_governing_bodies: agenda-setting beneficiary (organized/arbitrage) — applies biological criteria while endorsing identity language elsewhere
 *   - medical_administrators: agenda-setting beneficiary (institutional/arbitrage) — applies biological criteria to clinical workflows, identity criteria to patient-facing records
 *   - trans_women_in_sex_stratified_contexts: payer (powerless/trapped) — recognition granted broadly, revoked specifically in sport/safety/some medical contexts
 *   - cis_women_in_legal_recognition_disputes: payer (powerless/constrained) — bear costs when identity criteria govern access to spaces calibrated to biological-sex risk profiles
 *   - intersex_individuals_navigating_dual_criteria: payer (powerless/trapped) — neither criterion was built with their anatomy or identity in mind
 *   - sex_biology_advocates and gender_identity_advocates: excluded (organized/constrained) — each group's preferred criterion is conceded nowhere it matters most to them
 *   - courts_and_regulators: observer (institutional/analytical) — adjudicate domain-boundary disputes without ruling on which criterion is correct
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.48).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.42).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Reading of the Woman/Female Category (Domain-Split Definition)").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '4b3162dc-06a5-4585-bc8b-fe6aa973ff63').
narrative_ontology:cs_kernel_codification('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', distributed).
narrative_ontology:cs_authority_grounding('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', distributed).
narrative_ontology:cs_reading_relation('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', foundational, category_criterion_is_context_indexed).
narrative_ontology:cs_axiom_status(category_criterion_is_context_indexed, holdable).
narrative_ontology:cs_axiom_grounding('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', category_criterion_is_context_indexed, instrumental).
narrative_ontology:cs_axiom('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', secondary, institutional_domain_authority_over_criterion_selection).
narrative_ontology:cs_axiom_status(institutional_domain_authority_over_criterion_selection, holdable).
narrative_ontology:cs_axiom_grounding('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', institutional_domain_authority_over_criterion_selection, conventional).
narrative_ontology:cs_reference_frame('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', pre_split_undifferentiated_legal_sex_category).
narrative_ontology:cs_drift_state('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', contemporary_multi_domain_dispute_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b3162dc-06a5-4585-bc8b-fe6aa973ff63', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, medical_administrators).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women_in_sex_stratified_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cis_women_in_legal_recognition_disputes).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, intersex_individuals_navigating_dual_criteria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures, regulatory bodies, hospital ethics boards, and sports federations that adopt domain-split definitions to avoid picking a single settled definition of 'woman'/'female' and to manage litigation risk from both directions. They draft the rules, decide which domain a given dispute falls into, and collect the benefit of reduced immediate political exposure without resolving the underlying contest.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers, beneficiary).

% Administer eligibility categories for competitive sport using biological criteria (testosterone levels, developmental sex characteristics) while publicly endorsing gender-identity language elsewhere in their organizations. They benefit from being able to invoke whichever criterion insulates them from the loudest current pressure, switching justificatory frames between competitions and jurisdictions.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sports_governing_bodies, agenda_setter,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, sports_governing_bodies, beneficiary).

% Hospitals and clinical guideline bodies that use biological sex for screening protocols, organ-specific care, and drug dosing while using self-identified gender for intake forms, wards, and patient-facing records. They administer the split and can adjust which criterion applies to which workflow, which reduces their own liability exposure.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, medical_administrators, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, medical_administrators, beneficiary).

% Have their legal and social identity as women recognized in most contexts but are reclassified by biological criteria specifically in sport, some medical screening pathways, and safety-sensitive settings (shelters, prisons). The domain-switch means their exit from the classifying institution's control is unavailable — they cannot simply choose which domain governs a given interaction, and the switch is often experienced as recognition being revoked precisely where stakes (competition eligibility, single-sex space access) are highest.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_women_in_sex_stratified_contexts, payer,
    powerless, biographical, trapped, national).

% In legal/social recognition contexts governed by self-identification, they bear costs when single-sex spaces, services, or protections calibrated to biological-sex-based risk profiles (domestic violence shelters, prison housing, changing rooms) admit members on the identity criterion instead. They have no formal channel to contest which domain's criterion applies to a given space, since the domain boundary itself is drawn by institutions, not by the affected parties.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, cis_women_in_legal_recognition_disputes, payer,
    powerless, biographical, constrained, national).

% Their biological presentation does not map cleanly onto either sex-binary sporting/medical criteria or a stable gender-identity self-report, so the hybrid framework's domain-switch mechanism produces inconsistent classification across contexts they cannot control — sometimes screened out by biological criteria never designed with their anatomy in mind, sometimes required to assert an identity category that does not match their felt experience of either domain's premise.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, intersex_individuals_navigating_dual_criteria, payer,
    powerless, biographical, trapped, national).

% Argue category membership should be settled by biology across all contexts including legal and social recognition; the hybrid reading concedes their criterion only in medical/sports/safety domains and treats their objection to identity-based legal recognition as already settled against them. They are consulted in sport and medicine but excluded from the legal/social recognition domain-setting process.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_biology_advocates, excluded,
    organized, generational, constrained, national).

% Argue category membership should be settled by self-identification across all contexts including sport and medicine; the hybrid reading concedes their criterion only in social/legal domains and treats their objection to biological criteria in sport and medicine as already settled against them. They are consulted in legal recognition contexts but excluded from the sport/medicine domain-setting process.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate disputes at the domain boundary itself — whether a given context is 'really' medical/sports/safety or 'really' social/legal — and thereby determine which criterion applies without ever having to rule on which criterion is correct in the abstract. They observe the whole structure and can shift the boundary case by case.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, institutional_conflict_managers).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows institutions operating across domains with genuinely different verification needs (anti-doping testing vs. identity documents) to avoid forcing a single universal definition of 'woman'/'female' onto contexts with different evidentiary requirements — a locker room dispute and a chromosome-based eligibility test are not obviously the same kind of question.
% TRANSFER_FUNCTION: Moves the cost of definitional ambiguity from the institutions that could resolve it onto whichever party's preferred criterion is subordinated in a given domain: trans individuals bear the cost where biological criteria govern (sport, some safety contexts), and cis women and biology-criterion advocates bear the cost where identity criteria govern (legal recognition, some safety contexts). No party is compensated for the domain in which they lose; the arrangement transfers the political cost of choosing away from the institutions and onto the domain-losers.
% ABSENT_VOICES: Neither sex_biology_advocates nor gender_identity_advocates are present when the domain boundary itself is drawn — the decision about which contexts count as 'medical/sports/safety' versus 'social/legal' is made by institutional_conflict_managers and courts_and_regulators, not by either contesting group. Intersex individuals are almost entirely absent from the design of either criterion, since the hybrid framework was built to mediate between the sex-biology and gender-identity camps, not to accommodate atypical biological presentation.
% DISAPPEARANCE_RATIONALE: If the hybrid domain-split vanished, every institution currently avoiding a single definition would be forced to adopt one criterion across all its functions, immediately reopening the underlying kernel contest in every domain simultaneously — sports federations, hospitals, and legal registries would each have to publicly commit to sex-biology or gender-identity as their sole standard, triggering the exact disputes the hybrid framework was built to defer.
% FOUNDING_PROBLEM: Institutions faced simultaneous political and legal pressure from two incompatible definitional camps and needed a way to grant recognition claims in some domains without conceding the same criterion in domains (competitive sport, clinical risk stratification, single-sex safety infrastructure) where the two camps' preferred criteria produce materially different real-world outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Sports federations and medical bodies attest the domain-split reflects genuine differences in evidentiary need (testosterone-mediated athletic advantage is not the same question as legal name-and-marker recognition) — this is corroborated by exercise physiologists and clinical researchers outside either advocacy camp. Both sex_biology_advocates and gender_identity_advocates attest, from outside the institutional beneficiary set, that the split is not a principled resolution but a political expedient that concedes nothing in the domain each group cares most about while appearing to concede something in the domain it cares less about.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.48, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.48) is moderate and rising: the hybrid framework did not begin as extractive — early domain splits (e.g., sport bodies using testosterone thresholds while hospitals used identity-neutral intake forms) plausibly reflected genuine differing evidentiary needs. Over the measured interval the split increasingly functions as a mechanism institutions use to avoid ever resolving the underlying contest, and the political cost of non-resolution shifts onto whichever party loses in a given domain. Suppression (0.42) is moderate: no single group is coercively barred from all recognition, but the domain-boundary is drawn entirely by institutions, and neither contesting camp has a formal channel to contest which domain applies to their case. Theater ratio (0.40) reflects that a meaningful share of the domain-split's public justification ("we respect gender identity AND biological reality") functions as legitimacy performance for institutions managing two audiences rather than as a principled resolution — but the underlying evidentiary-need differences between contexts are not entirely fictional, so theater is not dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   institutional_conflict_managers, sports_governing_bodies, and medical_administrators sit near the beneficiary end: they set which domain a dispute falls into and thereby control which criterion applies, collecting reduced political and legal exposure without ever having to defend a single principled standard. The three payer groups sit near the target end for structurally different reasons: trans_women_in_sex_stratified_contexts lose specifically where biological criteria are invoked despite broad identity-based recognition elsewhere; cis_women_in_legal_recognition_disputes lose specifically where identity criteria are invoked in spaces they expected biological-sex-based protections to govern; intersex_individuals_navigating_dual_criteria lose in both domains because neither criterion accommodates their case. All three payer groups are trapped or constrained because the domain-boundary that determines which criterion governs their situation is set by institutions, not by themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure snare matters because the domain-split does solve a real coordination problem in at least some contexts — anti-doping testing genuinely requires different evidentiary standards than identity-document issuance, and treating these as identical questions would itself misclassify a genuine coordination need as pure extraction. But the structure requires active enforcement (courts, regulators, and institutional policy continually re-drawing the domain boundary) and produces identifiable victims in every domain where their preferred criterion is subordinated, which is why it is not a clean rope. Declaring mandatrophy_resolved would be premature: the founding problem (differing evidentiary needs across contexts) remains partially live, but the current operation increasingly serves institutional conflict-avoidance rather than the evidentiary distinction that originally justified it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_neutrality,
    'Is the boundary between ''medical/sports/safety'' and ''social/legal recognition'' contexts itself a neutral, principled distinction, or is it drawn strategically by institutions to place the criterion most favorable to their liability position in each domain?',
    'Track how the domain boundary has shifted over time in specific institutions (e.g., whether prison housing classification has moved between ''safety'' and ''legal recognition'' framing in response to litigation pressure rather than in response to new evidentiary findings) and whether boundary-shifts correlate with legal exposure rather than with new physiological or social-science findings.',
    'If the boundary is drawn principally in response to liability pressure rather than genuine evidentiary distinctions, the hybrid reading''s coordination claim collapses and the constraint is better read as a pure snare on whichever party is subordinated in the boundary-shift; if the boundary tracks genuine evidentiary distinctions consistently over time, the tangled_rope reading with a real (if eroding) coordination function is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_neutrality, conceptual, 'Whether the domain-split boundary is principled or strategically drawn.').

omega_variable(
    intersex_accommodation_gap,
    'Is the hybrid framework''s poor fit for intersex individuals an oversight correctable within the hybrid structure, or a structural feature of building a compromise between two camps whose criteria (chromosomal/anatomical sex; self-identification) were both defined without reference to intersex variation?',
    'Examine whether institutions revising domain-split policy documents have added intersex-specific accommodation provisions over time, or whether intersex cases are consistently routed to ad hoc, case-by-case resolution outside the published criteria in both domains.',
    'If accommodation is being actively built, the extraction from intersex individuals may be transitional rather than structural; if intersex cases are persistently unaddressed by design, this is evidence the hybrid reading was constructed to mediate a two-camp dispute and treats a third affected population as a residual category, strengthening the tangled_rope reading over a rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_accommodation_gap, empirical, 'Whether intersex exclusion from both criteria is transitional or structural.').

omega_variable(
    committer_framing_alternative,
    'Could this constraint alternatively be framed not as a hybrid reading of the woman/female category kernel, but as a meta-level arbitration mechanism sitting ABOVE the kernel (i.e., a governance layer that decides, per-dispute, which of the two pure readings applies) rather than as a reading of the kernel itself?',
    'Compare institutional documents: do they present the domain-split as a definition of ''woman''/''female'' (a reading) or as a jurisdictional/choice-of-law-style rule about which definition applies where (a meta-rule)? Legal drafting conventions and internal institutional memos would show which self-understanding predominates.',
    'Under the reading framing (as authored here), this constraint competes directly with sex_biology_reading and gender_identity_reading for kernel occupancy and its axioms are first-order normative claims about category membership. Under the meta-rule framing, the constraint would instead be an authority-grounding mechanism ABOVE the kernel contest, with a different cs_structure entirely (authority_grounding likely shifting from extraction/distributed toward a pure jurisdictional mechanism) and would not carry its own axioms about what a woman IS, only about which criterion governs where. This story adopts the reading framing because institutional practice (sports federation bylaws, hospital policy, legal statute language) typically states the criterion directly rather than framing it as choice-of-law, but the alternative framing would change the classification meaningfully.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether the hybrid arrangement is best modeled as a kernel reading or as a meta-level jurisdictional rule above the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__hybrid_contextual_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__hybrid_contextual_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__hybrid_contextual_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__hybrid_contextual_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__hybrid_contextual_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(woma_be_t4, woman_female_category__hybrid_contextual_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(woma_be_t8, woman_female_category__hybrid_contextual_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(woma_be_t12, woman_female_category__hybrid_contextual_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(woma_be_t16, woman_female_category__hybrid_contextual_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(woma_be_t20, woman_female_category__hybrid_contextual_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t4, woman_female_category__hybrid_contextual_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(woma_su_t8, woman_female_category__hybrid_contextual_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(woma_su_t12, woman_female_category__hybrid_contextual_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(woma_su_t16, woman_female_category__hybrid_contextual_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(woma_su_t20, woman_female_category__hybrid_contextual_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__hybrid_contextual_reading, 0.08).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_contextual_reading member of the woman_female_category kernel family (3 stories: sex_biology_reading, gender_identity_reading, hybrid_contextual_reading). Each reading has its own ε and victim structure per the ε-invariance principle: sex_biology_reading concentrates victimhood on trans individuals across all domains; gender_identity_reading concentrates victimhood on cis women and biology-dependent institutions (sport, medicine) across all domains; hybrid_contextual_reading spreads victimhood across both camps but confined to the domain where their preferred criterion is subordinated, with a distinct beneficiary class (conflict-averse institutions) that neither pure reading names as a beneficiary at all. All three are linked bidirectionally so contamination/purity propagation across the kernel contest is traceable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
