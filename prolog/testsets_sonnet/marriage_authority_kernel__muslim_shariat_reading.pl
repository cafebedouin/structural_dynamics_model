% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Muslim Personal Law (Shariat) Marriage/Family Authority
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint isolates the Muslim personal law reading of the shared
 *   Indian marriage-authority kernel: family law legitimacy is grounded in
 *   Shariat as interpreted through community-level qazis and defended
 *   institutionally by bodies such as the All India Muslim Personal Law
 *   Board, operating under the constitutional protection of Article 25/26
 *   minority religious freedom rather than under a codified civil statute.
 *   The reading provides genuine coordination value — a religiously
 *   legitimate, community-accessible forum that avoids forcing intimate
 *   family adjudication through a secular system perceived (with historical
 *   basis) as majoritarian — while simultaneously producing asymmetric
 *   outcomes for women under unilateral divorce, unequal inheritance shares,
 *   and unconsented polygamy. The 1985 Shah Bano controversy and subsequent
 *   Muslim Women (Protection of Rights on Divorce) Act 1986, and the 2017
 *   Shayara Bano triple-talaq judgment followed by the 2019 criminalization
 *   statute, mark points where suppression and extractiveness moved as state
 *   intervention was fought over and partially rolled back, then partially
 *   reasserted. This is a single reading among five siblings sharing the
 *   marriage-authority kernel (hindu_codified_reading,
 *   christian_canonical_reading, parsi_communal_reading,
 *   secular_civil_reading); ε here (0.58) is not directly comparable to the
 *   codified Hindu or secular civil readings' ε because the underlying
 *   mechanism — an uncodified, community-adjudicated jurisdiction with
 *   contested state reviewability — is structurally distinct, not a different
 *   observable angle on the same mechanism.
 *
 * KEY AGENTS:
 *   - muslim_personal_law_board: institutional agenda_setter defending interpretive monopoly
 *   - community_qazis: organized local adjudicators with fee/status stake in continued community jurisdiction
 *   - muslim_wives_under_unilateral_talaq: powerless payers, trapped exit, bear divorce and maintenance asymmetry
 *   - muslim_daughters_under_inheritance_shares: powerless payers under fixed fractional inheritance rule
 *   - indian_state_judiciary: institutional observer/excluded from routine adjudication, intervenes only at appellate/constitutional stage
 *   - muslim_womens_rights_organizations: organized excluded voice advocating codification or reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.62).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Personal Law (Shariat) Marriage/Family Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '85d5a187-1163-49b6-93e7-64819a331875').
narrative_ontology:cs_kernel_codification('85d5a187-1163-49b6-93e7-64819a331875', distributed).
narrative_ontology:cs_authority_grounding('85d5a187-1163-49b6-93e7-64819a331875', lineage).
narrative_ontology:cs_interpretation_layer_present('85d5a187-1163-49b6-93e7-64819a331875').
narrative_ontology:cs_reading_relation('85d5a187-1163-49b6-93e7-64819a331875', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('85d5a187-1163-49b6-93e7-64819a331875', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('85d5a187-1163-49b6-93e7-64819a331875', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('85d5a187-1163-49b6-93e7-64819a331875', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('85d5a187-1163-49b6-93e7-64819a331875', foundational, community_religious_tribunal_has_primary_adjudicative_authority).
narrative_ontology:cs_axiom_status(community_religious_tribunal_has_primary_adjudicative_authority, holdable).
narrative_ontology:cs_axiom_grounding('85d5a187-1163-49b6-93e7-64819a331875', community_religious_tribunal_has_primary_adjudicative_authority, theological).
narrative_ontology:cs_axiom('85d5a187-1163-49b6-93e7-64819a331875', secondary, unilateral_male_initiated_divorce_is_doctrinally_legitimate).
narrative_ontology:cs_axiom_status(unilateral_male_initiated_divorce_is_doctrinally_legitimate, overridden).
narrative_ontology:cs_axiom_grounding('85d5a187-1163-49b6-93e7-64819a331875', unilateral_male_initiated_divorce_is_doctrinally_legitimate, theological).
narrative_ontology:cs_reference_frame('85d5a187-1163-49b6-93e7-64819a331875', classical_shariat_community_jurisdiction).
narrative_ontology:cs_drift_state('85d5a187-1163-49b6-93e7-64819a331875', post_shayara_bano_2017_2019_criminalization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('85d5a187-1163-49b6-93e7-64819a331875', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_board).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, community_qazis).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_spouses_in_unilateral_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_wives_under_unilateral_talaq).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_daughters_under_inheritance_shares).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, women_in_polygamous_marriages).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, religious_community_self_governance_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, minority_cultural_autonomy_under_article_26).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Functions as the de facto interpretive authority over Shariat-derived family law, issuing model codes, lobbying against legislative codification, and defending qazi jurisdiction in courts and public discourse. It sets the terms under which community adjudication happens and benefits from the continued deference of the state to non-codification, which preserves its own interpretive monopoly.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_board, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicate marriage, divorce, and inheritance disputes at the community level, issuing talaq certificates and mediating settlements. Their authority and social standing depend on the community's continued reliance on religious rather than civil adjudication; they collect fees and status from this role and have no state salary tying them to codified outcomes.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, community_qazis, agenda_setter,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, community_qazis, beneficiary).

% Historically able to pronounce talaq unilaterally (triple talaq criminalized in 2019 but instant and other unilateral forms persist in practice in many community settings) and to contract polygamous marriages under the reading's interpretation, with minimal procedural burden compared to a fault-based civil divorce process. Their exit from the marriage is comparatively low-friction under this reading.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_spouses_in_unilateral_divorce, beneficiary,
    moderate, biographical, mobile, national).

% Face divorce pronounced against them with limited notice or negotiated settlement, inconsistent maintenance enforcement, and reliance on community mediation bodies that often lack coercive power to secure maintenance or matrimonial property. Many lack the economic independence or family support network to contest a qazi's or husband's determination and stay embedded in the community for social and economic survival.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_wives_under_unilateral_talaq, payer,
    powerless, biographical, trapped, local).

% Receive a fixed fractional share (typically half a son's share) under the classical inheritance reading applied by community bodies and, where uncontested, by courts applying Muslim personal law. Contesting the share means litigating against family and community consensus, which most do not do given social cost and unclear civil remedy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_daughters_under_inheritance_shares, payer,
    powerless, biographical, trapped, local).

% Share a husband's resources and legal standing with co-wives under a reading that permits polygamy without requiring the first wife's consent as a legal precondition. Economic dependency and social stigma around leaving typically foreclose exit; the arrangement is enforced by community and family expectation rather than formal coercion alone.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, women_in_polygamous_marriages, payer,
    powerless, biographical, trapped, local).

% Reviews personal-law disputes under a constitutional framework that has historically deferred to community personal law under Article 25/26 while intervening selectively (Shah Bano, Shayara Bano/triple talaq). It is structurally excluded from routine first-instance adjudication, which the community boards and qazis handle, and enters mainly at the appellate or constitutional-challenge stage — often after the community-level outcome has already been lived.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, indian_state_judiciary, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, indian_state_judiciary, excluded).

% Advocate for codification of Muslim family law or for a uniform civil code option, arguing the current reading is neither authentically fixed Shariat nor accountable to constitutional equality guarantees. They are not part of the personal law board's interpretive process and must litigate or legislate from outside it to be heard.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_womens_rights_organizations, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a community-internal, religiously legitimate forum for marriage, divorce, and inheritance adjudication without requiring believers to route intimate family matters through a secular civil court system perceived as culturally alien or historically distrusted by the minority community.
% TRANSFER_FUNCTION: Moves control over divorce timing, maintenance obligations, and inheritance shares disproportionately toward male family members and community religious authorities, and away from women whose economic and legal outcomes in divorce, maintenance, and inheritance are set largely without their negotiated consent.
% ABSENT_VOICES: Muslim women's rights organizations and reform-minded Islamic scholars who argue for ijtihad-based reinterpretation or codification are largely outside the personal law board's own interpretive process; individual women in maintenance disputes are frequently absent from the room where the qazi's determination is made.
% DISAPPEARANCE_RATIONALE: The personal law board and qazis maintain the arrangement is the only legitimate constitutional expression of community religious freedom and that its disappearance would mean state erosion of minority rights; women's rights groups and many jurists argue its disappearance (via codification or civil code option) would rearrange outcomes substantially for women's economic security while leaving genuine religious practice of marriage rites untouched, since the state would still recognize religious marriage ceremonies without deferring personal-law adjudication to community bodies.
% FOUNDING_PROBLEM: British colonial administration and post-independence India needed a way to govern family law for a religiously plural population without imposing a single code, and Muslim community leaders sought to preserve religious jurisdiction over family matters as a marker of protected minority identity following Partition-era anxieties about majoritarian assimilation.
% FOUNDING_PROBLEM_CORROBORATION: The personal law board attests the founding problem is fully live: protecting minority religious autonomy from majoritarian legislative override remains an active concern given ongoing uniform civil code debates. Independent legal scholars, the Law Commission of India (in its 2018 consultation paper on family law reform), and the Supreme Court in Shayara Bano v. Union of India attest from outside the board that specific practices (instant triple talaq) had drifted from defensible religious protection into unilateral extraction with no ijtihad basis even within classical Hanafi jurisprudence, and that broader codification questions remain genuinely unresolved rather than settled in the board's favor.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, contested).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high: the reading transfers real economic and legal control (divorce timing, maintenance terms, inheritance shares) from women to male relatives and community authorities, but the transfer coexists with a genuine coordination function (community-legitimate adjudication) that keeps ε below what a pure extraction mechanism would show. Suppression (0.62) reflects that exit from the community adjudicative structure carries real social and economic cost even though no formal state coercion compels a woman to use a qazi rather than a civil court — the suppression is substantially social/community-enforced rather than statutory. Accessibility_collapse (0.50) is mid-range because civil court avenues and, since 2019, criminal remedies for instant talaq nominally exist alongside the community forum, but are frequently inaccessible in practice due to cost, distance, and social pressure — alternatives are not fully collapsed, but are substantially constrained. Resistance (0.68) is high, reflecting decades of sustained litigation, legislative contest (1986 Act, 2019 Act), and organized advocacy by women's groups and reformist scholars — this is not a quiet, unresisted arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Male spouses and community authorities sit near the beneficiary end: they retain interpretive control, low-friction exit from marriage, and social/institutional standing under the current reading. Women across the three payer groups sit near the target end: their outcomes (divorce terms, maintenance, inheritance shares, co-wife status) are set substantially by others' unilateral action or by a fixed formula they cannot individually negotiate, and their exit options are trapped by economic dependency and community embeddedness rather than by formal law alone. The state judiciary is analytically positioned rather than benefiting or paying directly, though its selective, appellate-only intervention pattern itself shapes how much protection reaches payer seats in practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting minority religious jurisdiction from majoritarian legislative override — remains partly live (uniform civil code debates continue) but the specific mechanism of unilateral instant divorce was found by the Supreme Court itself (Shayara Bano, 2017) to lack grounding even in classical Hanafi jurisprudence, indicating a component of the reading had drifted from defensible religious protection into extraction with no religious-doctrinal basis, which the state partially corrected via the 2019 Act. This demonstrates why the classification must not collapse into pure snare or pure rope: the underlying coordination function (community-legitimate adjudication as minority protection) is real and contested reform remains ongoing, while specific sub-mechanisms (instant talaq, unequal inheritance defaults, unconsented polygamy) show asymmetric extraction requiring active social/institutional enforcement to persist — hence tangled_rope rather than either pure type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_autonomy_vs_extraction_reading,
    'Is deference to Muslim personal law board and qazi interpretation a genuine expression of constitutionally protected minority religious autonomy, or a constructed jurisdictional monopoly that extracts disproportionately from women within the community under cover of religious protection?',
    'Track outcomes of ongoing Law Commission consultations and any future codification or Uniform Civil Code legislative process; compare maintenance and inheritance outcomes for women under community adjudication versus civil court adjudication in comparable disputes.',
    'If autonomy framing dominates, the reading remains a defensible tangled_rope with a strong coordination component; if extraction framing dominates and further sub-mechanisms are found (as triple talaq was) to lack doctrinal grounding, the classification would shift toward snare for those specific sub-mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_autonomy_vs_extraction_reading, conceptual, 'Whether personal-law board authority is genuine minority protection or extraction under a protection label.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Where exactly does the muslim_shariat_reading''s jurisdiction end and the secular_civil_reading''s constitutional-override jurisdiction begin, given that Muslim couples may opt into the Special Marriage Act and courts intervene selectively (Shah Bano, Shayara Bano)?',
    'Map the actual case law boundary: which family-law questions the Supreme Court has treated as subject to constitutional equality review versus which it has left to community/personal-law deference, and how consistently opt-in secular civil marriage removes a couple from this reading''s jurisdiction in practice.',
    'A wide overlap zone would mean this reading''s ε is partly attenuated in practice by accessible exit into the secular_civil_reading; a narrow overlap zone would mean the reading''s structural extraction is closer to unmitigated for those who remain within it, which is the practical majority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, empirical, 'The practical boundary between this reading''s jurisdiction and the secular civil reading''s override authority.').

omega_variable(
    internal_reform_diversity_omission,
    'Does treating ''the muslim_shariat_reading'' as a single constraint obscure significant internal diversity — Hanafi, Shafi''i, Shia (Ithna Ashari), and reformist Muslim personal law positions differ substantially on divorce, inheritance, and consent requirements, and some already require judicial talaq or explicit maintenance protections?',
    'Would require decomposing this story further by fiqh school and by regional community practice if internal variance in ε across schools proves large enough to violate ε-invariance within this single reading.',
    'If internal variance is large, this single story may itself be an aggregation masking multiple structurally distinct sub-constraints, analogous to the BGS decomposition; if variance is modest, the single-reading treatment remains defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_reform_diversity_omission, conceptual, 'Whether school-level (fiqh) diversity within Muslim personal law requires further decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1937, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement_basis(marr_tr_t1937, observed).
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1955, 0.18).
narrative_ontology:measurement_basis(marr_tr_t1955, observed).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement_basis(marr_tr_t1985, observed).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(marr_tr_t2000, observed).
narrative_ontology:measurement(marr_tr_t2017, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2017, 0.26).
narrative_ontology:measurement_basis(marr_tr_t2017, observed).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(marr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1937, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1937, 0.55).
narrative_ontology:measurement_basis(marr_be_t1937, observed).
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1955, 0.56).
narrative_ontology:measurement_basis(marr_be_t1955, observed).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement_basis(marr_be_t1985, observed).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2000, 0.59).
narrative_ontology:measurement_basis(marr_be_t2000, observed).
narrative_ontology:measurement(marr_be_t2017, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2017, 0.53).
narrative_ontology:measurement_basis(marr_be_t2017, observed).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(marr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1937, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1937, 0.5).
narrative_ontology:measurement_basis(marr_su_t1937, observed).
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1955, 0.52).
narrative_ontology:measurement_basis(marr_su_t1955, observed).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement_basis(marr_su_t1985, observed).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement_basis(marr_su_t2000, observed).
narrative_ontology:measurement(marr_su_t2017, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2017, 0.45).
narrative_ontology:measurement_basis(marr_su_t2017, observed).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(marr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the marriage_authority_kernel operative in Indian personal law. Each sibling reading (hindu_codified_reading, christian_canonical_reading, parsi_communal_reading, secular_civil_reading) is authored as its own ε-invariant constraint with its own beneficiary/victim structure, adjudicative mechanism, and gender-equity profile, per the ε-invariance decomposition principle. They are linked here rather than merged because the underlying adjudicative mechanisms (community tribunal vs. codified civil court vs. constitutional civil registration) are structurally distinct, not different observables of one mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
