% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Patriarchal Personal Law Practices Contested Under Constitutional Equality Guarantees
 *   domain: legal/constitutional/gender
 *
 * SUMMARY:
 *   This constraint captures ONE reading of the marriage authority kernel:
 *   the gender-rights reading, which treats personal law legitimacy as
 *   conditional on intra-community equality between men and women, and treats
 *   judicial constitutional review as the appropriate lever for correcting
 *   specific discriminatory practices (unilateral divorce, maintenance
 *   ceilings, unequal inheritance) even where it does not touch the broader
 *   jurisdictional structure of legal pluralism. This is distinct from the
 *   secularist reading (which wants elimination of personal law pluralism
 *   entirely via a Uniform Civil Code), the communal autonomy reading (which
 *   denies the state any authority to adjudicate internal religious family
 *   law at all), the federalist/millet reading (which values fragmentation
 *   itself as an anti-majoritarian structural good, independent of gender
 *   outcomes), and the judicial harmonization reading (which frames the same
 *   case law as building a general constitutional floor rather than a
 *   gender-specific correction). The gender-rights reading's ε is authored
 *   high because the practice-by-practice litigation strategy, while
 *   producing real wins for specific litigants, leaves systemic extraction
 *   from women who are not litigants substantially intact, and the litigation
 *   apparatus itself accrues professional and institutional benefit
 *   independent of whether the underlying practice is actually eliminated in
 *   practice within communities.
 *
 * KEY AGENTS:
 *   - women_within_patriarchal_personal_law: Primary target (powerless/trapped) — bears the extraction of unequal marriage, maintenance, and inheritance terms
 *   - women_rights_advocates: Primary beneficiary (organized/mobile) — litigates strategically and gains standing, funding, and precedent from each win
 *   - reform_minded_judiciary: Secondary beneficiary (institutional/analytical) — gains authority and legacy from expansive constitutional interpretation without bearing social cost
 *   - community_religious_authorities: Excluded/payer (organized/constrained) — treated as respondent rather than co-author of the reform, loses jurisdiction incrementally
 *   - male_community_members: Payer (moderate/constrained) — loses legally sanctioned advantage with each ruling
 *   - the_state: Observer/agenda_setter (institutional/analytical) — enforces rulings, avoids legislative confrontation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.79).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.71).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Patriarchal Personal Law Practices Contested Under Constitutional Equality Guarantees").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal/constitutional/gender").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, '9b67c3dc-4a8b-43f7-a533-10391ccec09e').
narrative_ontology:cs_kernel_codification('9b67c3dc-4a8b-43f7-a533-10391ccec09e', distributed).
narrative_ontology:cs_authority_grounding('9b67c3dc-4a8b-43f7-a533-10391ccec09e', distributed).
narrative_ontology:cs_reading_relation('9b67c3dc-4a8b-43f7-a533-10391ccec09e', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('9b67c3dc-4a8b-43f7-a533-10391ccec09e', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b67c3dc-4a8b-43f7-a533-10391ccec09e', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('9b67c3dc-4a8b-43f7-a533-10391ccec09e', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('9b67c3dc-4a8b-43f7-a533-10391ccec09e', foundational, personal_law_legitimacy_conditional_on_gender_equality).
narrative_ontology:cs_axiom_status(personal_law_legitimacy_conditional_on_gender_equality, holdable).
narrative_ontology:cs_axiom_grounding('9b67c3dc-4a8b-43f7-a533-10391ccec09e', personal_law_legitimacy_conditional_on_gender_equality, deontological).
narrative_ontology:cs_axiom('9b67c3dc-4a8b-43f7-a533-10391ccec09e', secondary, constitutional_courts_may_correct_discrete_discriminatory_practices_without_displacing_pluralism).
narrative_ontology:cs_axiom_status(constitutional_courts_may_correct_discrete_discriminatory_practices_without_displacing_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('9b67c3dc-4a8b-43f7-a533-10391ccec09e', constitutional_courts_may_correct_discrete_discriminatory_practices_without_displacing_pluralism, conventional).
narrative_ontology:cs_reference_frame('9b67c3dc-4a8b-43f7-a533-10391ccec09e', colonial_era_personal_law_codification).
narrative_ontology:cs_drift_state('9b67c3dc-4a8b-43f7-a533-10391ccec09e', post_shah_bano_shayara_bano_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9b67c3dc-4a8b-43f7-a533-10391ccec09e', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, reform_minded_judiciary).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, community_religious_authorities).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, male_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under community personal law governing marriage, divorce, maintenance, and inheritance. Subject to unilateral divorce mechanisms (e.g. instant triple talaq prior to its curtailment), truncated maintenance entitlements, and unequal property/inheritance shares codified as religious obligation. Exit from the marriage frequently means exit from the community's social and economic support network as well; challenging the practice in court risks community ostracism even when the litigation succeeds.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, trapped, national).

% Litigate strategically, selecting individual practices (triple talaq, maintenance ceilings, discriminatory succession rules) for constitutional challenge rather than attacking personal law systems wholesale. Gain professional standing, funding, and policy influence from each successful reform; are not themselves subject to the personal law regimes they litigate against in the same way as their clients.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary).

% Reads constitutional equality guarantees (Articles 14, 15, 21) expansively to strike down or narrow specific personal law practices on a case-by-case basis. Gains institutional authority and precedent-setting stature from each ruling; bears none of the social cost imposed on litigants who must return to their communities after the case concludes.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, reform_minded_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, reform_minded_judiciary, beneficiary).

% Administer personal law as custodians of religious tradition and communal cohesion; treat judicial intervention into specific practices as an assault on communal jurisdiction. Not consulted in the framing of the equality challenge itself — their voice enters only as a defendant/respondent in litigation they did not choose to initiate, and each individual ruling erodes their claimed authority without their having conceded the underlying jurisdictional premise.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, community_religious_authorities, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, community_religious_authorities, payer).

% Benefit materially from the practices being challenged (unilateral divorce power, favorable maintenance terms, larger inheritance shares) and experience each successful equality ruling as a direct loss of legally sanctioned advantage. Have no formal standing to object beyond community-level political resistance or legislative lobbying.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, male_community_members, payer,
    moderate, biographical, constrained, national).

% Enforces judicial rulings once issued but has not legislated a comprehensive reform, preferring the political cover of piecemeal judicial intervention over a frontal legislative confrontation with communal politics. Benefits from appearing progressive on individual cases while avoiding the electoral cost of comprehensive personal law reform.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, the_state, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, the_state, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an incremental, court-supervised mechanism for individual women to challenge specific discriminatory provisions within their community's personal law without requiring a legislature to overhaul the entire personal law system at once — a real coordination function where legislative reform is politically blocked.
% TRANSFER_FUNCTION: Moves specific legal entitlements (maintenance amounts, divorce procedural protections, inheritance shares) from men and community religious authorities to individual women litigants, on a practice-by-practice, case-by-case basis rather than systemically.
% ABSENT_VOICES: Women within patriarchal personal law regimes who lack the resources, social capital, or safety to litigate are structurally absent from the judicial process that is nominally conducted in their interest — the litigants who reach the Supreme Court are not representative of the broader population the ruling is said to protect. Community religious authorities are also excluded from shaping the equality framework itself, entering only as respondents.
% DISAPPEARANCE_RATIONALE: If judicial equality review of personal law practices disappeared overnight, the specific curtailed practices (instant triple talaq, capped maintenance) would face no remaining check besides ordinary legislative politics; women currently relying on precedent-based protections would lose an active remedy pathway, and community authorities would regain uncontested jurisdiction over the practices in question. The arrangement is doing real structural work, not merely ratifying an existing equilibrium.
% FOUNDING_PROBLEM: Codified personal law practices produced systematically unequal outcomes for women within several religious communities — unilateral divorce without recourse, inadequate maintenance, unequal inheritance — while comprehensive legislative reform (a Uniform Civil Code or community-led reform) remained politically unattainable.
% FOUNDING_PROBLEM_CORROBORATION: Women's rights litigants and reform-minded judges attest the founding problem remains live, citing continuing incidence of the underlying practices in communities not yet reached by litigation. Community religious authorities and some feminist legal scholars outside the litigation apparatus (e.g. critiques from Muslim feminist scholars of the litigation-centric strategy) attest that the piecemeal judicial approach has become as much a vehicle for judicial institutional authority and majoritarian political signaling as a genuine remedy — that the case-by-case strategy leaves the systemic problem largely unaddressed while generating visible, symbolically potent wins.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.79 by 2025) because even as the gender-rights litigation strategy produces genuine constitutional wins, the practice-by-practice approach means most women within the affected communities remain governed by the un-litigated remainder of discriminatory personal law at any given time — the reading's own success stories (triple talaq curtailment, maintenance floor cases) coexist with un-remedied practices (unequal inheritance shares in several communities, procedural barriers to accessing even the reformed remedies). Suppression is moderately high (0.71) reflecting both the formal legal suppression women face within personal law regimes and the social suppression (ostracism, loss of support networks) that discourages the very litigation this reading depends on. Theater ratio is comparatively low (0.28) because the underlying judicial and advocacy activity is substantively functional rather than performative, though it is rising over the measured interval as high-profile individual cases increasingly serve as symbolic proof-of-reform substituting for systemic legislative change.
 *
 * DIRECTIONALITY LOGIC:
 *   Women within patriarchal personal law are the structural target: trapped exit options, powerless power atom, and the beneficiary of the extraction (unilateral divorce power, favorable maintenance/inheritance terms) accrues to male community members and is administered by community religious authorities. Women's rights advocates and reform-minded judiciary are structural beneficiaries of THIS READING specifically — not of the underlying personal law system, but of the reform apparatus itself: litigation wins accrue professional and institutional capital to the litigators and the judges independent of whether systemic conditions improve for the broader population of women the litigation is nominally conducted for. This is the seat divergence the omega below addresses: is this apparatus's ε correctly attributed to the practices themselves (in which case it is clearly extractive and the gender-rights reading is a genuine, if partial, remedy) or partly to the litigation-and-precedent industry that has formed around individual high-profile cases (in which case some of the measured 'reform benefit' is itself a form of extraction — visibility and institutional capital captured by advocates and judiciary without proportionate improvement for the excluded, non-litigating majority)?
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (systemic gender inequality in personal law with no comprehensive legislative remedy available) remains substantially live — this is not a resolved mandatrophy case. But the founding_problem_status is authored as contested rather than simply live because the piecemeal judicial strategy has, over four decades, itself become an institution with its own momentum: reform-minded judges and advocacy organizations have professional and reputational stakes in continuing the case-by-case approach rather than pressing for the comprehensive reform (legislative UCC, or community-led internal reform) that would obsolesce the litigation apparatus itself. The classification prevents mislabeling this as pure coordination (a naive rope reading would ignore that most affected women never benefit from any single ruling) or pure extraction with no coordination content (a naive framing would ignore that specific rulings have produced real, measurable improvement for litigants and, via precedent, some non-litigants) — snare is authored because the extraction from the non-litigating majority substantially exceeds the coordination benefit delivered to the reached minority, while acknowledging the coordination function is real, not fictional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    litigation_apparatus_capture_vs_genuine_remedy,
    'Does the measured extraction correctly attribute to the underlying discriminatory personal law practices, or does part of it belong to a litigation-and-precedent apparatus that has developed independent institutional interests in continuing case-by-case reform rather than pressing for comprehensive legislative or communal reform?',
    'Longitudinal tracking of outcomes for non-litigating women in communities where landmark rulings have been issued, compared to outcomes for litigants: if the gap remains wide and stable across decades despite successive rulings, this supports the capture reading; if the gap narrows as precedent diffuses into community practice, this supports the genuine remedy reading.',
    'If capture is significant, the gender-rights reading itself displays snare-like features (extraction accruing to the reform apparatus, symbolic wins substituting for systemic change) layered atop the genuinely extractive underlying personal law practices — a second-order extraction the current metrics may understate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(litigation_apparatus_capture_vs_genuine_remedy, empirical, 'Whether litigation-driven reform substantially benefits the broader excluded population or primarily the litigation apparatus itself.').

omega_variable(
    kernel_framing_alternative_reading,
    'Is the correct framing for this constraint ''gender equality vs. communal authority'' (the framing authored here) or ''individual constitutional rights vs. collective religious rights'' (a framing under which the same rulings would be read as vindicating individual liberty rather than specifically gender equality)?',
    'Compare judicial reasoning across the landmark rulings: opinions that ground the holding explicitly in Article 15 (sex discrimination) support the gender-rights framing; opinions grounding the same holding in Article 21 (individual liberty/dignity) generically would support the alternative individual-rights framing, which would produce a different beneficiary/victim structure (individual dissenters of any gender vs. collective religious authority, rather than women specifically vs. patriarchal practice).',
    'Under the alternative framing, some rulings currently coded as gender-rights wins would instead belong to a generic individual-rights-vs-communal-authority constraint with a different, non-gendered victim/beneficiary structure — this would shift some of the measured ε to a sibling constraint not currently modeled in this kernel''s declared reading set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_reading, conceptual, 'Whether the equality holdings are best modeled as gender-specific or as generic individual-rights claims against communal authority.').

omega_variable(
    class_representativeness_of_litigants,
    'Are the women who successfully litigate against personal law practices representative of the broader class of women subject to those practices, or are they systematically drawn from a subset with atypical resources, urban location, or social capital?',
    'Demographic and socioeconomic analysis of litigants in landmark personal law cases compared to census/survey data on the broader affected population.',
    'If litigants are systematically unrepresentative, the disappearance_verdict''s world_rearranges claim would need qualification — the arrangement may rearrange the world for a narrow, resourced subset while leaving the broader population''s arrangements largely unchanged, which would push the constraint''s classification toward a more concentrated-benefit, narrower-coordination-function reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_representativeness_of_litigants, empirical, 'Whether the litigation-accessible subset of the victim class is representative of the whole.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1985, marriage_authority__gender_rights_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(marr_tr_t1993, marriage_authority__gender_rights_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(marr_tr_t2001, marriage_authority__gender_rights_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(marr_tr_t2009, marriage_authority__gender_rights_reading, theater_ratio, 2009, 0.21).
narrative_ontology:measurement(marr_tr_t2017, marriage_authority__gender_rights_reading, theater_ratio, 2017, 0.25).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority__gender_rights_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t1985, marriage_authority__gender_rights_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(marr_be_t1993, marriage_authority__gender_rights_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(marr_be_t2001, marriage_authority__gender_rights_reading, base_extractiveness, 2001, 0.64).
narrative_ontology:measurement(marr_be_t2009, marriage_authority__gender_rights_reading, base_extractiveness, 2009, 0.68).
narrative_ontology:measurement(marr_be_t2017, marriage_authority__gender_rights_reading, base_extractiveness, 2017, 0.75).
narrative_ontology:measurement(marr_be_t2025, marriage_authority__gender_rights_reading, base_extractiveness, 2025, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1985, marriage_authority__gender_rights_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(marr_su_t1993, marriage_authority__gender_rights_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(marr_su_t2001, marriage_authority__gender_rights_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(marr_su_t2009, marriage_authority__gender_rights_reading, suppression_requirement, 2009, 0.62).
narrative_ontology:measurement(marr_su_t2017, marriage_authority__gender_rights_reading, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(marr_su_t2025, marriage_authority__gender_rights_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the marriage_authority kernel, decomposed per the ε-invariance principle: each reading evaluates the same underlying question (where does legitimate authority over marriage and family law reside) from a structurally distinct premise, producing a different ε, different beneficiary/victim structure, and potentially a different classification. This reading (gender_rights_reading) is authored as ε_high snare, cross-cutting the communal/secular divide and targeting specific discriminatory practices rather than the pluralism structure itself. The judicial_harmonization_reading describes overlapping case law from a different premise (constitutional floor-building) and should be expected to diverge in classification even though many of the same court rulings serve as evidence for both readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
