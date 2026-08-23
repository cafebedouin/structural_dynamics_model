% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Woman/Female Category Membership by Biological Sex (XX/XY, Gamete Production Capacity)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint story captures the 'sex biology reading' of the contested
 *   kernel 'woman_female_category'. The constraint defines woman/female
 *   category membership strictly by chromosomal sex (XX), reproductive
 *   anatomy, and developmental biology (gamete production capacity). This
 *   definition operates as a gatekeeping mechanism for female-only spaces:
 *   prisons, domestic violence shelters, changing rooms, sports categories,
 *   and positive action provisions. The constraint is actively enforced
 *   through legislation (e.g., UK Equality Act 2010 sex-based exceptions, US
 *   state prison policies, World Athletics eligibility rules), institutional
 *   policy, and social norm enforcement. The claimed type is tangled_rope:
 *   the constraint performs a genuine coordination function (clear boundary
 *   for sex-based protections) while extracting asymmetrically from trans
 *   women (exclusion, denial of identity, exposure to violence). Extraction
 *   has risen sharply 2010-2025 as the constraint has been weaponized in
 *   culture war politics, requiring escalating suppression to maintain
 *   against legal challenges and shifting public opinion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.68).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.82).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.77).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Woman/Female Category Membership by Biological Sex (XX/XY, Gamete Production Capacity)").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '74b5a9c3-7142-4566-811e-3582f49907f3').
narrative_ontology:cs_kernel_codification('74b5a9c3-7142-4566-811e-3582f49907f3', distributed).
narrative_ontology:cs_authority_grounding('74b5a9c3-7142-4566-811e-3582f49907f3', distributed).
narrative_ontology:cs_reading_relation('74b5a9c3-7142-4566-811e-3582f49907f3', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('74b5a9c3-7142-4566-811e-3582f49907f3', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('74b5a9c3-7142-4566-811e-3582f49907f3', foundational, woman_category_requires_female_sex).
narrative_ontology:cs_axiom_status(woman_category_requires_female_sex, holdable).
narrative_ontology:cs_axiom_grounding('74b5a9c3-7142-4566-811e-3582f49907f3', woman_category_requires_female_sex, empirically_contingent).
narrative_ontology:cs_axiom('74b5a9c3-7142-4566-811e-3582f49907f3', foundational, gamete_production_capacity_defines_sex_class).
narrative_ontology:cs_axiom_status(gamete_production_capacity_defines_sex_class, holdable).
narrative_ontology:cs_axiom_grounding('74b5a9c3-7142-4566-811e-3582f49907f3', gamete_production_capacity_defines_sex_class, empirically_contingent).
narrative_ontology:cs_axiom('74b5a9c3-7142-4566-811e-3582f49907f3', secondary, sex_based_provisions_require_biological_boundary).
narrative_ontology:cs_axiom_status(sex_based_provisions_require_biological_boundary, holdable).
narrative_ontology:cs_axiom_grounding('74b5a9c3-7142-4566-811e-3582f49907f3', sex_based_provisions_require_biological_boundary, instrumental).
narrative_ontology:cs_reference_frame('74b5a9c3-7142-4566-811e-3582f49907f3', sex_based_feminist_framework).
narrative_ontology:cs_drift_state('74b5a9c3-7142-4566-811e-3582f49907f3', contemporary_gender_identity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('74b5a9c3-7142-4566-811e-3582f49907f3', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, biological_females).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, womens_rights_organizations).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_feminine_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, shelter_operators).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, biological_sex_binary).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, sex_based_rights_framework).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, material_reality_of_sex).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adult human females (XX) who rely on single-sex spaces (prisons, shelters, changing rooms, sports categories) for bodily privacy, dignity, and safety from male-pattern violence. The biological definition protects their access to these spaces by excluding trans women. They face constrained exit because sex-segregated spaces are often the only option for safety-critical needs; they cannot individually opt out of the need for such spaces.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, biological_females, beneficiary,
    organized, biographical, constrained, global).

% Trans women (XY, male-typical developmental biology) who identify as women and seek access to female-only spaces. Under this constraint they are categorically excluded from women's prisons, shelters, changing rooms, and sports categories. This exclusion exposes them to violence in male facilities, denies their gender identity, and creates material harm in housing, healthcare, and daily life. Their exit is identity-locked: they cannot cease being trans women without psychological devastation, and the constraint makes their identity legally and socially invisible in sex-segregated contexts.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, global).

% Non-binary and gender-nonconforming people assigned male at birth who are feminized in presentation or social role. They are swept into the same exclusion as trans women under a rigid biological definition, losing access to female-only spaces and services without necessarily identifying as women. Their situation mirrors trans women but with less institutional recognition.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_feminine_people, payer,
    powerless, biographical, identity_locked, global).

% State agencies responsible for prison placement policies. They enforce the biological definition by placing trans women in male facilities or separate units. They bear administrative costs of litigation and policy compliance but hold the coercive power to implement or resist the constraint. They can arbitrage between jurisdictions with different legal standards.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, prison_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Operators of domestic violence shelters and rape crisis centers. They enforce admission policies based on biological sex. They face funding pressures and legal liability from both sides: excluding trans women risks discrimination lawsuits and loss of government grants; including them risks losing women's trust and donor support. Their exit is constrained by mission commitment and regulatory environment.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, shelter_operators, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, shelter_operators, payer).

% Feminist organizations advocating for sex-based rights and single-sex provisions. They lobby for, litigate to defend, and publicly champion the biological definition. They benefit materially and politically from the constraint's enforcement (funding, membership, policy wins). They have mobile exit: they can shift focus to other feminist issues if the constraint collapses.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, womens_rights_organizations, beneficiary,
    organized, generational, mobile, global).

% Organizations advocating for gender-identity-based category membership. They are structurally excluded from the policy framework this constraint creates — their position is treated as illegitimate by the constraint's own logic. They would object to the exclusion of trans women from female spaces but have no seat at the table when the biological definition is treated as axiomatic. They can mobilize publicly and legally but cannot participate in the constraint's internal governance.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_rights_organizations, excluded,
    organized, generational, mobile, global).

% Judicial bodies adjudicating disputes over sex/gender definitions in anti-discrimination law, prison policy, shelter access, and sports eligibility. They interpret statutory language ("sex" vs "gender") and constitutional equal protection. They do not collect rents from the constraint but their rulings determine its enforcement scope. Their analytical seat means they evaluate the constraint's coherence without bearing its costs or collecting its benefits.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, legal_courts, observer,
    institutional, generational, analytical, national).

% Legislators and regulators who write laws defining "woman" and "female" in statutes (Equality Act, Title IX, prison regulations, shelter funding rules). They set the agenda by choosing which definition to codify. They benefit politically from aligning with organized constituencies on either side. They can arbitrage between state/federal levels and between different policy domains.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, policy_makers, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administratively legible boundary for sex-segregated spaces and provisions (prisons, shelters, sports, intimate care, positive action shortlists) that protects biological females from male-pattern violence and preserves fair competition in female sport. The biological definition solves the coordination problem of "who counts as a woman for sex-based protections" by anchoring it in an observable, immutable criterion.
% TRANSFER_FUNCTION: Transfers access to female-only spaces, legal protections, and sex-based provisions from trans women (who are categorically excluded) to biological females (whose access is secured). The transfer is enforced through state power (prison placement, shelter admission policies, sports governing body rules) and institutional gatekeeping.
% ABSENT_VOICES: Trans women and trans feminine people are the primary absent voices — they are the ones most directly harmed by the constraint but their perspective is excluded from the constraint's own definitional logic (they are defined out of the category "woman" and thus out of the conversation about women's spaces). Gender-critical feminists who support the biological definition but reject the feminist label are also marginalized in mainstream discourse. Detransitioners who regret medical transition but do not fit neatly into either advocacy camp are rarely heard.
% DISAPPEARANCE_RATIONALE: If the biological definition vanished overnight, prison systems would need new placement policies (likely gender-identity-based), shelters would lose their legal basis for single-sex admission, sports categories would require restructuring, and positive action shortlists would need new eligibility criteria. The entire architecture of sex-based provisions would reorganize around gender identity or a hybrid model. Trans women would gain access to female spaces; biological females would lose guaranteed single-sex provisions.
% FOUNDING_PROBLEM: The historical problem was male violence against females and the need for sex-segregated spaces as a protective measure. Early feminist and social reform movements established female-only prisons, shelters, changing rooms, and sports categories to address the material reality of sexual dimorphism and male-pattern offending. The biological definition was the unspoken premise that made these provisions coherent and enforceable.
% FOUNDING_PROBLEM_CORROBORATION: Crime statistics consistently show male-pattern violence (sexual assault, domestic violence) is overwhelmingly perpetrated by males against females, corroborated by law enforcement data, victimization surveys, and prison population data across jurisdictions. This is attested by criminologists, victim advocates, and prison administrators outside the feminist movement. The contested element is whether the biological definition remains the *only* or *best* way to address this problem given the existence of trans women who also face high rates of violence.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the severe material harm to trans women: placement in male prisons correlates with 13x higher sexual assault rates (UK Ministry of Justice data); shelter exclusion leaves trans women with no safe housing; sports bans end athletic careers. Suppression (0.82) is high because the constraint requires constant legal defense, policy litigation, and social enforcement against a growing trans rights movement. Theater ratio (0.28) is moderate: the coordination function (protecting biological females) is real but increasingly overshadowed by performative culture-war signaling (bathroom bills, sports bans affecting tiny numbers of athletes). Accessibility collapse (0.73) is high for trans women — no alternative female spaces exist; for biological females, alternatives (gender-neutral spaces) are emerging but remain marginal. Resistance (0.77) is high and rising: trans rights litigation, corporate policy divergence, international human rights pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the biological female seat, the constraint appears as a rope (genuine coordination, minimal extraction *for them*). From the trans woman seat, it appears as a snare (pure extraction, no coordination benefit, coercive enforcement). The engine computes this seat divergence from the structural data. The claimed type (tangled_rope) captures the hybrid reality: the constraint IS a coordination mechanism for one group that FUNCTIONS AS extraction for another. This is not a perspectival error — it is the structural signature of tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Biological females are structural beneficiaries (d ~0.15): the constraint secures their single-sex provisions. Directionality is dampened because they also bear some cost (rigid boundaries can harm gender-nonconforming biological females, and the culture war polarizes feminist movements). Trans women are structural targets (d ~0.95): identity-locked exit, powerless, bearing full exclusion. Prison authorities and shelter operators are agenda-setters with institutional power but constrained by litigation risk — their d sits near symmetric (0.45-0.55) as they both enforce and pay compliance costs. Women's rights orgs are beneficiaries with mobile exit (d ~0.2). Trans rights orgs are excluded (no d computation). Courts are analytical observers (d = 0.5 by definition). Policy makers are agenda-setters with arbitrage exit (d ~0.3-0.4 depending on political alignment).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (male violence against females, need for sex-segregated safety) remains live. The constraint has not atrophied into a piton — its coordination function is actively invoked and legally enforced. However, the extraction component has grown disproportionately: the constraint now operates in domains (youth sports, bathroom access) where the original safety rationale is attenuated. This is mandatrophy drift: the mandate (protect females) has expanded beyond its functional justification into a broader culture-war boundary maintenance. The constraint is not resolved mandatrophy — it is actively mutating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is the woman_female_category kernel a single commitment with multiple readings, or are these distinct constraints masquerading as readings of one kernel?',
    'Trace whether all three readings share a common institutional anchor (e.g., the Equality Act''s ''sex'' and ''gender reassignment'' protected characteristics, the UN CEDAW framework) that forces them into a single adjudicative space. If no shared anchor exists, they are separate constraints.',
    'If separate constraints, the network.affects_constraints links become cross-constraint influence rather than intra-kernel reading relations. The cs_structure block would be invalid for this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether the three declared readings genuinely share a kernel or are analytically distinct constraints.').

omega_variable(
    victim_set_ambiguity,
    'Does the constraint''s victim set include biological females harmed by the constraint''s rigid boundary (e.g., gender-nonconforming women, intersex women, women denied care due to sex-verification scrutiny), or only trans women?',
    'Document cases where biological females are adversely affected by sex-verification enforcement (e.g., Caster Semenya, women with DSDs, butch lesbians challenged in restrooms). If such cases are systematic, the victim set expands beyond trans women.',
    'If biological females are also victims, the constraint''s extraction is more diffuse and its coordination function more fragile — it may reclassify toward snare (extraction without clear beneficiary coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_ambiguity, empirical, 'Whether the victim set is exclusively trans women or includes biological females caught in enforcement collateral damage.').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function (protecting biological females in prisons/shelters) be separated from the extraction function (excluding trans women from all female spaces), or are they structurally inseparable?',
    'Examine jurisdictions that have implemented trans-inclusive policies with safeguards (e.g., individual risk assessment in prisons, gender-neutral shelter options with single-sex rooms). If female safety outcomes hold, the functions are separable.',
    'If separable, the constraint is a snare with a coordination cover story; if inseparable, it is a genuine tangled rope where extraction is the price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components can be institutionally decoupled.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the high suppression (0.82) primarily structural (legal bans, policy enforcement) or partially internalized (trans women self-excluding, biological females self-policing boundaries)?',
    'Measure trans women''s avoidance of female spaces even where legally permitted, and biological females'' informal enforcement of boundaries. Post-exit trajectory: if suppression persists after legal barriers are removed (e.g., in trans-inclusive jurisdictions), internalized component is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the structural measure — targets carry the suppression with them. This would increase effective extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs internalized suppression in the enforcement of biological sex boundaries.').

omega_variable(
    biological_criterion_coherence,
    'Is the biological criterion (XX/XY, gamete production capacity) a coherent, non-arbitrary boundary, or does it collapse under intersex conditions, DSDs, and the biological reality of sex as a cluster property?',
    'Assess whether the constraint''s operational definition (e.g., ''sex observed at birth'', ''chromosomal sex'', ''gamete production'') produces consistent classifications across all humans, or requires arbitrary line-drawing for the ~1.7% with intersex variations.',
    'If the criterion is incoherent, the constraint''s claimed naturalness (emerges_naturally = false but coordination function claims biological grounding) is undermined. The coordination function becomes a social choice masked as biological fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_criterion_coherence, empirical, 'Whether the biological sex binary holds as a clean classificatory boundary for legal/administrative purposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wfc_sbr_tr_t2010, woman_female_category__sex_biology_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(wfc_sbr_tr_t2013, woman_female_category__sex_biology_reading, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(wfc_sbr_tr_t2016, woman_female_category__sex_biology_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(wfc_sbr_tr_t2019, woman_female_category__sex_biology_reading, theater_ratio, 2019, 0.23).
narrative_ontology:measurement(wfc_sbr_tr_t2022, woman_female_category__sex_biology_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement(wfc_sbr_tr_t2025, woman_female_category__sex_biology_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(wfc_sbr_be_t2010, woman_female_category__sex_biology_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(wfc_sbr_be_t2013, woman_female_category__sex_biology_reading, base_extractiveness, 2013, 0.42).
narrative_ontology:measurement(wfc_sbr_be_t2016, woman_female_category__sex_biology_reading, base_extractiveness, 2016, 0.51).
narrative_ontology:measurement(wfc_sbr_be_t2019, woman_female_category__sex_biology_reading, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement(wfc_sbr_be_t2022, woman_female_category__sex_biology_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement(wfc_sbr_be_t2025, woman_female_category__sex_biology_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wfc_sbr_su_t2010, woman_female_category__sex_biology_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(wfc_sbr_su_t2013, woman_female_category__sex_biology_reading, suppression_requirement, 2013, 0.52).
narrative_ontology:measurement(wfc_sbr_su_t2016, woman_female_category__sex_biology_reading, suppression_requirement, 2016, 0.63).
narrative_ontology:measurement(wfc_sbr_su_t2019, woman_female_category__sex_biology_reading, suppression_requirement, 2019, 0.71).
narrative_ontology:measurement(wfc_sbr_su_t2022, woman_female_category__sex_biology_reading, suppression_requirement, 2022, 0.78).
narrative_ontology:measurement(wfc_sbr_su_t2025, woman_female_category__sex_biology_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__sex_biology_reading, 0.08).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, single_sex_prison_placement_policy).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, female_sport_eligibility_rules).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, domestic_violence_shelter_admission_policy).

% DUAL FORMULATION NOTE:
% This constraint (sex_biology_reading) and its siblings (gender_identity_reading, hybrid_contextual_reading) form the woman_female_category constraint family. They share the kernel 'who counts as a woman/female' but instantiate different constraints with different ε, different victim/beneficiary sets, and different type classifications. The sex_biology_reading has ε=0.68 (tangled_rope); the gender_identity_reading likely has lower ε for trans women but higher ε for biological females in some contexts; the hybrid_contextual_reading has context-variable ε. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__sex_biology_reading, powerless, 0.95).
constraint_indexing:directionality_override(woman_female_category__sex_biology_reading, organized, 0.15).
constraint_indexing:directionality_override(woman_female_category__sex_biology_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
