% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__muslim_shariat_reading, []).

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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Muslim Personal Law Marriage Contract (Nikah) under Shariat
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   The Muslim personal law marriage contract (nikah) operates as a civil
 *   contract under Quranic injunctions and hadith, governing marriage
 *   formation, dissolution, and spousal rights in India and other
 *   jurisdictions. The constraint presents itself as a balanced contractual
 *   framework with mutual obligations — mahr (dower) as women's financial
 *   protection, structured divorce procedures (talaq) with waiting periods,
 *   and polygyny permission with equity conditions. In practice, the
 *   constraint exhibits pronounced gender asymmetry: husbands hold unilateral
 *   dissolution power (talaq) while wives require judicial intervention
 *   (khula/faskh) with higher evidentiary burdens; mahr is frequently
 *   deferred and then contested at divorce; polygyny operates without
 *   meaningful equity enforcement. The 2019 legislative ban on instant triple
 *   talaq (talaq-e-biddat) modified one dissolution pathway but left the
 *   structural asymmetry intact — other talaq forms remain available to
 *   husbands. Religious authorities (ulema) administer the system and resist
 *   state intervention, positioning themselves as guardians of divine law.
 *   The constraint requires active enforcement through family courts, qazis,
 *   and community pressure to maintain its asymmetric operation.
 *
 * KEY AGENTS:
 *   - husbands: Primary beneficiary (institutional/identity_locked) — hold unilateral divorce power, polygyny permission, control over mahr payment timing
 *   - wives: Primary victim (powerless/identity_locked) — bear asymmetric divorce access, mahr enforcement burden, polygyny vulnerability
 *   - religious_authorities_ulema: Agenda setter (institutional/identity_locked) — administer marriage/dissolution, interpret texts, resist reform, derive authority from gatekeeping
 *   - patriarchal_kin_networks: Beneficiary (organized/constrained) — gain alliance control, property consolidation, lineage continuity through asymmetric rules
 *   - divorced_women_without_mahr: Victim (powerless/trapped) — bear concentrated costs of non-enforcement, limited exit options
 *   - women_in_polygynous_marriages: Victim (powerless/constrained) — bear resource dilution, emotional labor, unequal treatment
 *   - state_courts: Observer/partial agenda setter (institutional/analytical) — adjudicate disputes, enforce (or fail to enforce) mahr, apply statutory overrides (2019 ban)
 *   - womens_rights_activists: Excluded (organized/mobile) — advocate reform, challenge asymmetric provisions, propose alternative readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.68).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Muslim Personal Law Marriage Contract (Nikah) under Shariat").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, '8510ae61-8f06-4192-803c-bd1b67ae8ae1').
narrative_ontology:cs_kernel_codification('8510ae61-8f06-4192-803c-bd1b67ae8ae1', fixed_text).
narrative_ontology:cs_authority_grounding('8510ae61-8f06-4192-803c-bd1b67ae8ae1', lineage).
narrative_ontology:cs_interpretation_layer_present('8510ae61-8f06-4192-803c-bd1b67ae8ae1').
narrative_ontology:cs_reading_relation('8510ae61-8f06-4192-803c-bd1b67ae8ae1', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('8510ae61-8f06-4192-803c-bd1b67ae8ae1', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8510ae61-8f06-4192-803c-bd1b67ae8ae1', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('8510ae61-8f06-4192-803c-bd1b67ae8ae1', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('8510ae61-8f06-4192-803c-bd1b67ae8ae1', foundational, marriage_as_civil_contract_under_divine_law).
narrative_ontology:cs_axiom_status(marriage_as_civil_contract_under_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('8510ae61-8f06-4192-803c-bd1b67ae8ae1', marriage_as_civil_contract_under_divine_law, theological).
narrative_ontology:cs_axiom('8510ae61-8f06-4192-803c-bd1b67ae8ae1', foundational, husband_unilateral_talaq_as_quranic_right).
narrative_ontology:cs_axiom_status(husband_unilateral_talaq_as_quranic_right, holdable).
narrative_ontology:cs_axiom_grounding('8510ae61-8f06-4192-803c-bd1b67ae8ae1', husband_unilateral_talaq_as_quranic_right, theological).
narrative_ontology:cs_axiom('8510ae61-8f06-4192-803c-bd1b67ae8ae1', secondary, mahr_as_womens_exclusive_property).
narrative_ontology:cs_axiom_status(mahr_as_womens_exclusive_property, holdable).
narrative_ontology:cs_axiom_grounding('8510ae61-8f06-4192-803c-bd1b67ae8ae1', mahr_as_womens_exclusive_property, theological).
narrative_ontology:cs_axiom('8510ae61-8f06-4192-803c-bd1b67ae8ae1', secondary, polygyny_permitted_with_equity_condition).
narrative_ontology:cs_axiom_status(polygyny_permitted_with_equity_condition, holdable).
narrative_ontology:cs_axiom_grounding('8510ae61-8f06-4192-803c-bd1b67ae8ae1', polygyny_permitted_with_equity_condition, theological).
narrative_ontology:cs_reference_frame('8510ae61-8f06-4192-803c-bd1b67ae8ae1', classical_fiqh_marriage_governance).
narrative_ontology:cs_drift_state('8510ae61-8f06-4192-803c-bd1b67ae8ae1', post_colonial_personal_law_codification, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8510ae61-8f06-4192-803c-bd1b67ae8ae1', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, husbands).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, religious_authorities_ulema).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, patriarchal_kin_networks).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, wives).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, divorced_women_without_mahr).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, women_in_polygynous_marriages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, wives).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, quranic_divorce_procedure).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, mahr_as_womens_property_right).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, shariat_as_complete_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold unilateral dissolution right (talaq) in multiple forms; may take up to four wives simultaneously with theoretical equity requirement; control timing and payment of mahr (often deferred); benefit from community recognition and property rights within marriage. Exit is arbitrage-grade: can remarry instantly after talaq, access polygyny, and face minimal social sanction for exercising contractual rights.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, husbands, beneficiary,
    institutional, biographical, arbitrage, national).

% Enter marriage with mahr as theoretical financial protection but often deferred and unenforced; face asymmetric divorce access — khula requires husband's consent or judicial process with high evidentiary burden; vulnerable to polygyny without meaningful consent or equity enforcement; bear primary care burden for children. Exit is identity-locked: leaving the marriage framework means leaving the religious community, facing social ostracism, losing child custody presumptions, and navigating a legal system that privileges the husband's contractual rights.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, wives, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, wives, beneficiary).

% Administer marriage contracts (nikahnama), certify divorces, issue fatwas on family law, resist state legislative intervention, control interpretation of Quranic/hadith texts. Derive authority, livelihood, and social power from gatekeeping the constraint. Exit is identity-locked: their institutional role IS the constraint's enforcement mechanism; reform threatens their epistemic monopoly and material base.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, religious_authorities_ulema, agenda_setter,
    institutional, generational, identity_locked, national).

% Use marriage as alliance-building tool; control spouse selection, mahr negotiation, and post-marital residence; benefit from polygyny's expansion of kin networks; enforce community norms through social pressure. Exit is constrained: networks could shift to secular or reformed frameworks but lose the asymmetric leverage the current constraint provides.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, patriarchal_kin_networks, beneficiary,
    organized, generational, constrained, local).

% Bear concentrated costs of mahr non-enforcement: no financial cushion post-divorce, limited property rights, dependent on natal family or state support. Trapped by the intersection of deferred mahr norms, judicial delays, husband's non-compliance, and community stigma against divorced women. No meaningful exit from the constraint's consequences.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, divorced_women_without_mahr, payer,
    powerless, immediate, trapped, local).

% Experience resource dilution (time, money, emotional labor), unequal treatment despite theoretical equity requirement, vulnerability to further marriages. Constrained exit: khula difficult, social stigma high, economic dependence on husband, children's custody tied to marriage. Some accept polygyny as religious duty — identity-locked acceptance — others resist but lack alternatives.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_in_polygynous_marriages, payer,
    powerless, biographical, constrained, local).

% Adjudicate family law disputes under parallel personal law system; enforce (or fail to enforce) mahr orders; interpret statutory overrides (2019 triple talaq ban, 1986 Muslim Women Act); navigate constitutional equality guarantees vs. religious freedom claims. Analytical exit: can observe the constraint's operation without being subject to its personal law rules, but institutional role requires engaging with it.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, state_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, state_courts, agenda_setter).

% Advocate for reform within Muslim personal law (codification, mahr enforcement, khula access, polygyny restrictions) and for uniform civil code; challenge asymmetric provisions through litigation, legislative advocacy, and community mobilization. Mobile exit: operate in secular legal and civil society spaces; not personally subject to the constraint's personal law rules but structurally excluded from its interpretive authority.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, womens_rights_activists, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a complete, community-recognized framework for marriage formation, spousal obligations, financial protection (mahr), and dissolution procedures that operates without requiring state capacity — solves the coordination problem of marital stability and dispute resolution in a plural legal order.
% TRANSFER_FUNCTION: Moves unilateral dissolution power and polygyny permission from wives to husbands; moves interpretive authority and gatekeeping revenue from state to religious authorities; moves alliance control and property consolidation from individuals to kin networks; moves the cost of non-enforcement (unpaid mahr, unequal treatment) from husbands/authorities to wives.
% ABSENT_VOICES: Women in polygynous marriages who would challenge the equity requirement but cannot access the interpretive forums; divorced women denied mahr who lack legal resources to pursue enforcement; queer Muslims for whom the nikah framework has no recognition; reformist Islamic scholars who argue for gender-symmetric readings but are marginalized by institutional ulema.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, marriage regulation would default to the secular_contractual_reading (Special Marriage Act, state family courts) — existing marriages would need re-registration, mahr claims would become contractual debts, divorce would follow gender-symmetric procedures, polygyny would be criminalized. The religious authority structure (ulema gatekeeping) would lose its primary jurisdictional domain. Community dispute resolution would shift to state courts or new private mechanisms.
% FOUNDING_PROBLEM: In pre-colonial and early colonial India, Muslim communities needed a self-governing marriage framework that operated without state infrastructure, provided women's financial protection (mahr) in a patrilineal property system, and regulated dissolution in a context where women had no independent legal personality.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as dead by: (1) the Indian state's comprehensive family court system (established 1984) that now handles marriage, divorce, maintenance, custody for all communities; (2) the 2019 triple talaq ban itself — a legislative intervention that presupposes state capacity to regulate Muslim marriage; (3) women's rights organizations (BMMA, AIDWA) documenting that mahr, khula, and polygyny protections exist in Quranic text but are systematically nullified by practice; (4) the Law Commission of India's 2018 consultation paper on family law reform noting that personal laws no longer serve their original coordination function. No non-beneficiary source attests the founding problem as live — the ulema's claim that 'divine law is eternal' is a status claim, not a coordination-function claim.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers substantial decision-power and economic resources from wives to husbands and religious authorities without equivalent reciprocal transfer. Suppression (0.72) is high because the constraint's persistence depends on active enforcement: community pressure against khula seekers, religious authority resistance to reform, legal barriers to women's exit, and the identity-lock that makes leaving the constraint's framework equivalent to leaving the community. Theater ratio (0.38) is moderate — the mahr obligation and talaq procedures provide genuine coordination (marriage formation, financial protection framework, dissolution process) but a growing share of enforcement activity defends the asymmetric privileges rather than the coordination function. Accessibility collapse (0.65) reflects that alternatives (secular marriage, reformed personal law) exist but are structurally difficult to access due to identity-lock and community sanctions. Resistance (0.55) is significant — women's rights movements, judicial interventions, and legislative reforms (2019 ban) demonstrate active contestation, though the constraint adapts rather than breaks.
 *
 * PERSPECTIVAL GAP:
 *   From the husband/ulema seat, the constraint appears as divine coordination: a complete, balanced system where each party has defined rights and obligations. From the wife's seat, the same structure operates as enforced extraction: unilateral obligations (obedience, sexual access) with conditional rights (mahr if enforced, divorce if permitted). The engine computes this divergence from the declared beneficiaries/victims and exit_options — husbands have arbitrage-grade exit (can remarry, polygyny, unilateral talaq); wives have identity_locked exit (khula requires husband's consent or judicial process with community stigma). Religious authorities sit at the agenda_setter seat with institutional power and identity_locked exit — their authority derives from the constraint's persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: husbands (unilateral divorce, polygyny, mahr control), religious_authorities_ulema (interpretive monopoly, community authority), patriarchal_kin_networks (alliance/property control). Victims declared: wives (asymmetric divorce, mahr enforcement burden), divorced_women_without_mahr (concentrated non-enforcement costs), women_in_polygynous_marriages (resource dilution). Directionality derives from this structure: husbands/ulema/kin-networks are net beneficiaries (d low), wives/divorced-women/co-wives are net targets (d high). State courts and activists sit outside the core extraction loop — courts as partial enforcers with analytical distance, activists as excluded voices with mobile exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding coordination problem — regulating marriage formation and dissolution in a tribal/commercial society with weak state capacity — is largely dead (state courts now handle contracts, property, child welfare). Yet the constraint persists with its asymmetric extraction intact, maintained by religious authorities who extract authority from gatekeeping the kernel. This is classic mandatrophy: the coordination function has atrophied (or been absorbed by the state) but the extraction structure persists through identity-lock and institutional inertia. The 2019 ban shows the state CAN intervene but chose a narrow fix (one talaq form) rather than structural reform — suggesting the constraint's extraction is tolerated by the state as the price of communal peace.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the family_law_authority kernel, or a distinct constraint misidentified as a reading?',
    'Structural comparison of ε and beneficiary/victim profiles across all five declared readings of the kernel; if this reading''s profile is discontinuous with the kernel''s common referent, it is a separate constraint.',
    'If separate constraint, the committer frame is invalid and this story must stand alone without cs_structure.reading_relations; the network links to sibling readings would be reclassified as cross-constraint affects_constraints edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the committer-frame identification of this constraint as a kernel reading is structurally warranted.').

omega_variable(
    triple_talaq_ban_structural_break,
    'Does the 2019 legislative ban on instant triple talaq represent a structural break in the constraint, or a surface modification that leaves the asymmetric extraction intact?',
    'Track post-2019 divorce outcomes: if men''s unilateral dissolution power persists through other talaq forms (talaq-e-hasan, talaq-e-ahsan) with comparable ease, the extraction structure is continuous; if women''s access to khula and judicial divorce has meaningfully improved, the directionality may have shifted.',
    'If structural break: the interval should be split into pre-2019 and post-2019 stories with different ε and claimed_type. If continuous: single story with measurement series showing the ban''s limited effect on extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triple_talaq_ban_structural_break, empirical, 'Whether the 2019 triple talaq ban materially altered the constraint''s extraction profile or merely displaced it.').

omega_variable(
    mahr_enforcement_gap,
    'Is the mahr (dower) obligation a genuine coordination mechanism protecting women''s financial autonomy, or a theoretical right whose systemic non-enforcement makes it extractive theater?',
    'Empirical study of mahr payment rates at marriage, deferral enforcement at divorce, and judicial willingness to compel payment against recalcitrant husbands.',
    'If mahr functions as genuine protection, the constraint has a stronger coordination leg (supporting tangled_rope). If systematically unenforced, the coordination story is cover and the constraint trends toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mahr_enforcement_gap, empirical, 'Whether the mahr obligation operates as functional coordination or extractive theater.').

omega_variable(
    polygyny_coordination_vs_extraction,
    'Does permitted polygyny serve a genuine coordination function (widow/orphan care, social stability) or is it an extractive privilege for men that the constraint presents as religious mandate?',
    'Demographic analysis of polygynous marriage patterns: proportion driven by widow/orphan care vs. younger additional wives; economic outcomes for co-wives and children; stated motivations of polygynous husbands.',
    'If primarily extractive, polygyny is a beneficiary capture mechanism with no coordination justification — strengthens snare classification. If mixed, remains tangled_rope with a weaker coordination claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(polygyny_coordination_vs_extraction, conceptual, 'Whether polygyny''s persistence in the constraint is coordination-motivated or extraction-motivated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1937, family_law_authority__muslim_shariat_reading, theater_ratio, 1937, 0.25).
narrative_ontology:measurement(fami_tr_t1956, family_law_authority__muslim_shariat_reading, theater_ratio, 1956, 0.28).
narrative_ontology:measurement(fami_tr_t1973, family_law_authority__muslim_shariat_reading, theater_ratio, 1973, 0.32).
narrative_ontology:measurement(fami_tr_t1986, family_law_authority__muslim_shariat_reading, theater_ratio, 1986, 0.35).
narrative_ontology:measurement(fami_tr_t2001, family_law_authority__muslim_shariat_reading, theater_ratio, 2001, 0.37).
narrative_ontology:measurement(fami_tr_t2019, family_law_authority__muslim_shariat_reading, theater_ratio, 2019, 0.36).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__muslim_shariat_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(fami_be_t1937, family_law_authority__muslim_shariat_reading, base_extractiveness, 1937, 0.55).
narrative_ontology:measurement(fami_be_t1956, family_law_authority__muslim_shariat_reading, base_extractiveness, 1956, 0.58).
narrative_ontology:measurement(fami_be_t1973, family_law_authority__muslim_shariat_reading, base_extractiveness, 1973, 0.62).
narrative_ontology:measurement(fami_be_t1986, family_law_authority__muslim_shariat_reading, base_extractiveness, 1986, 0.65).
narrative_ontology:measurement(fami_be_t2001, family_law_authority__muslim_shariat_reading, base_extractiveness, 2001, 0.67).
narrative_ontology:measurement(fami_be_t2019, family_law_authority__muslim_shariat_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__muslim_shariat_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1937, family_law_authority__muslim_shariat_reading, suppression_requirement, 1937, 0.58).
narrative_ontology:measurement(fami_su_t1956, family_law_authority__muslim_shariat_reading, suppression_requirement, 1956, 0.62).
narrative_ontology:measurement(fami_su_t1973, family_law_authority__muslim_shariat_reading, suppression_requirement, 1973, 0.66).
narrative_ontology:measurement(fami_su_t1986, family_law_authority__muslim_shariat_reading, suppression_requirement, 1986, 0.69).
narrative_ontology:measurement(fami_su_t2001, family_law_authority__muslim_shariat_reading, suppression_requirement, 2001, 0.71).
narrative_ontology:measurement(fami_su_t2019, family_law_authority__muslim_shariat_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__muslim_shariat_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__muslim_shariat_reading, 0.08).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).

% DUAL FORMULATION NOTE:
% This reading and the secular_contractual_reading share the 'civil contract' surface framing but diverge on authority grounding (divine text vs. state law) and gender symmetry. The hindu_dharmashastra_reading shares the 'religious authority' grounding but differs on sacramental vs. contractual ontology. The 2019 triple talaq ban was a secular_contractual_reading intervention into this reading's operation — a cross-reading affects_constraints edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__muslim_shariat_reading, institutional, 0.15).
constraint_indexing:directionality_override(family_law_authority__muslim_shariat_reading, powerless, 0.88).
constraint_indexing:directionality_override(family_law_authority__muslim_shariat_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
