% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Muslim Personal Law (Shariat) Reading of Marriage/Family Authority
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the Muslim-Shariat reading of the contested
 *   marriage-authority kernel in Indian law: authority over Muslim marriage,
 *   divorce, and inheritance derives from Shariat as interpreted by community
 *   personal law boards and qazis, operating under constitutional protection
 *   for religious personal law (Articles 25, 26) alongside, and largely
 *   outside, the secular civil judiciary. The 1985 Shah Bano controversy, the
 *   2017 Shayara Bano triple-talaq judgment, and the 2019 Muslim Women Act
 *   mark points where the reading's boundary with state authority was
 *   actively contested and partially narrowed (hence the extractiveness dip
 *   after 2017). This is a single reading among five siblings sharing the
 *   marriage-authority kernel; the Hindu, Christian, Parsi, and secular-civil
 *   readings are separate constraints with their own ε values and are not
 *   modeled here — see kernel_context.
 *
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
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Personal Law (Shariat) Reading of Marriage/Family Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, 'a425b5b7-0d09-4229-afe1-d79427357265').
narrative_ontology:cs_kernel_codification('a425b5b7-0d09-4229-afe1-d79427357265', distributed).
narrative_ontology:cs_authority_grounding('a425b5b7-0d09-4229-afe1-d79427357265', lineage).
narrative_ontology:cs_interpretation_layer_present('a425b5b7-0d09-4229-afe1-d79427357265').
narrative_ontology:cs_reading_relation('a425b5b7-0d09-4229-afe1-d79427357265', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('a425b5b7-0d09-4229-afe1-d79427357265', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a425b5b7-0d09-4229-afe1-d79427357265', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('a425b5b7-0d09-4229-afe1-d79427357265', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('a425b5b7-0d09-4229-afe1-d79427357265', foundational, community_tribunal_interpretive_primacy).
narrative_ontology:cs_axiom_status(community_tribunal_interpretive_primacy, holdable).
narrative_ontology:cs_axiom_grounding('a425b5b7-0d09-4229-afe1-d79427357265', community_tribunal_interpretive_primacy, conventional).
narrative_ontology:cs_axiom('a425b5b7-0d09-4229-afe1-d79427357265', secondary, unilateral_male_prerogative_in_dissolution).
narrative_ontology:cs_axiom_status(unilateral_male_prerogative_in_dissolution, holdable).
narrative_ontology:cs_axiom_grounding('a425b5b7-0d09-4229-afe1-d79427357265', unilateral_male_prerogative_in_dissolution, conventional).
narrative_ontology:cs_reference_frame('a425b5b7-0d09-4229-afe1-d79427357265', classical_shariat_personal_law_autonomy).
narrative_ontology:cs_drift_state('a425b5b7-0d09-4229-afe1-d79427357265', post_shayara_bano_2017, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a425b5b7-0d09-4229-afe1-d79427357265', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazis).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_spouses_under_unilateral_talaq).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_wives_facing_unilateral_talaq).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_daughters_under_inheritance_shares).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, second_and_subsequent_wives_in_polygamous_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bodies such as the All India Muslim Personal Law Board articulate authoritative interpretations of Shariat for marriage, divorce, and inheritance, lobby against legislative codification, and coordinate community-wide resistance to judicial or statutory intervention. They set the interpretive agenda and face little formal accountability to those governed by their rulings.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards, agenda_setter,
    institutional, generational, arbitrage, national).

% Local religious adjudicators register nikah and talaq, issue fatwas on marital disputes, and administer inheritance distribution within their communities. They derive standing and income from being the recognized interpretive authority and can relocate their practice across regions without losing status.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, qazis, beneficiary).

% Husbands retain the structural capacity to dissolve marriage through unilateral pronouncement with minimal procedural burden, and to contract additional marriages under community-sanctioned polygamy, without needing court adjudication or spousal consent.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_spouses_under_unilateral_talaq, beneficiary,
    moderate, biographical, mobile, national).

% Wives can be divorced without prior notice or judicial process, lose marital home and maintenance security rapidly, and depend on community-administered iddat/mehr provisions that are inconsistently enforced. Exit into civil courts is legally available in principle but practically blocked by family and community pressure, cost, and social stigma attached to challenging religious authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_wives_facing_unilateral_talaq, payer,
    powerless, biographical, trapped, regional).

% Receive a fixed fractional share of inheritance smaller than a son's share under classical Shariat distribution rules applied by qazis and family consensus; challenging the distribution means contesting family, community, and religious authority simultaneously, and civil litigation is slow, costly, and socially penalized.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_daughters_under_inheritance_shares, payer,
    powerless, generational, constrained, national).

% Enter marriages that lack the legal and social protections of a sole marital relationship, face uncertain maintenance and inheritance standing relative to a first wife, and have limited practical recourse because the arrangement is validated by the same community authority that would need to adjudicate their grievance.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, second_and_subsequent_wives_in_polygamous_households, payer,
    powerless, biographical, trapped, regional).

% Constitutional courts have intervened episodically (Shah Bano, Shayara Bano/triple talaq judgment, Muslim Women Act 2019 criminalizing instant talaq) but are structurally kept at arm's length by Article 25/26 personal-law protections and political resistance to codification; each intervention triggers community mobilization framing it as external interference with religious autonomy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, indian_state_judiciary, excluded,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, indian_state_judiciary, observer).

% Groups such as the Bharatiya Muslim Mahila Andolan document abuses and advocate for codified, gender-equitable Muslim family law or a uniform civil code option, but are marginalized in the interpretive process controlled by the Personal Law Board and often cast by community leadership as inauthentic or state-aligned voices.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, reformist_muslim_womens_organizations, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Muslim communities with a self-administered, religiously legitimate mechanism for solemnizing marriage, resolving divorce, and distributing inheritance without requiring recourse to a secular judiciary many community members distrust or cannot readily access, and preserves a domain of communal self-governance under constitutional minority-rights protection.
% TRANSFER_FUNCTION: Moves control over marital dissolution, maintenance, and inheritance distribution disproportionately toward husbands, senior male relatives, and the qazi/board interpretive apparatus, and away from wives, daughters, and junior co-wives, who bear the material and status costs of decisions made largely without their structured participation.
% ABSENT_VOICES: Muslim wives subject to unilateral talaq, daughters receiving reduced inheritance shares, and junior wives in polygamous unions rarely sit on the boards or qazi councils that interpret Shariat for their cases; reformist women's organizations that would advocate codified gender-equitable reform are present in public discourse but structurally excluded from the interpretive authority itself.
% DISAPPEARANCE_RATIONALE: If Shariat-derived personal law authority disappeared overnight, Muslim marriage, divorce, and inheritance would default to whatever secular civil framework filled the vacuum (likely an extension of the Special Marriage Act or a uniform civil code), qazis and personal law boards would lose their adjudicatory function and associated standing, husbands would lose unilateral talaq and polygamy as available options, and daughters/wives would gain formal parity — a substantial rearrangement of family-law practice and community authority structures.
% FOUNDING_PROBLEM: Colonial and post-colonial India needed a way to accommodate religious pluralism in family law without imposing a single civil code on communities with distinct marital and inheritance traditions, preserving Muslim community self-governance as a matter of religious liberty and minority protection under Articles 25 and 26.
% FOUNDING_PROBLEM_CORROBORATION: The Muslim Personal Law Board and allied clergy attest the founding problem — protecting religious autonomy from majoritarian legal imposition — remains fully live. The Supreme Court (Shayara Bano v. Union of India, 2017) and Parliament (Muslim Women (Protection of Rights on Marriage) Act, 2019) have found specific components (instant unilateral talaq) constitutionally indefensible and no longer serving a legitimate religious-liberty function; reformist Muslim women's organizations and comparative-law scholars outside the Board's constituency corroborate that the coordination function persists while the gender-equity costs have become disproportionate to any live religious-liberty justification.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects a genuine coordination function (community-legitimate marriage/divorce/inheritance administration) coexisting with substantial asymmetric extraction concentrated on wives, daughters, and junior co-wives through unilateral talaq, unequal inheritance shares, and unregulated polygamy. Suppression (0.62) is high because exit into civil courts is nominally available but functionally blocked by social, financial, and familial pressure — suppression here is largely internalized/structural hybrid, not purely statutory. Theater ratio (0.28) is moderate-low: qazi adjudication performs real administrative work, but an increasing share of board activity (especially post-2017) defends the interpretive monopoly itself rather than adjudicating disputes. Accessibility collapse (0.6) is elevated because most Muslim women in practice do not experience civil courts as a live alternative even though one exists in law. Resistance (0.55) reflects active, organized pushback from reformist women's groups and periodic judicial intervention, but not yet a wholesale collapse of the arrangement's legitimacy within the community.
 *
 * DIRECTIONALITY LOGIC:
 *   Personal law boards and qazis are structural beneficiaries: they set interpretive terms, administer the system, and derive institutional and personal standing from being the recognized authority (d near beneficiary end). Male spouses benefit structurally from unilateral talaq and polygamy provisions without needing to justify decisions to any adjudicator outside the community (d low-moderate). Wives facing unilateral talaq, daughters under reduced inheritance shares, and junior co-wives are structural targets: the same interpretive apparatus that administers the coordination function extracts asymmetrically from them, and their exit options are trapped or constrained by social and economic dependency (d near target end). The state judiciary and reformist women's organizations are excluded rather than coordinated or extracted from directly — their absence from the interpretive process is the structural feature the omega below addresses.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the genuine coordination function (community self-governance, culturally legitimate dispute resolution, religious-liberty accommodation) that a pure-extraction label would erase, while classifying it as tangled_rope rather than rope refuses to launder the asymmetric extraction on wives, daughters, and junior wives as costless coordination. The founding_problem_status mismatch (contested; live for the Board, dead/superseded for specific components per Shayara Bano) is itself the diagnostic the mandatrophy check exists to surface: the arrangement's original religious-liberty justification remains partly live at the level of general community self-governance, but specific extractive mechanisms (instant unilateral talaq) have been found by the state's own highest court to have outlived any legitimate function — a partial mandatrophy resolution, not a wholesale one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_autonomy_vs_state_intervention_boundary,
    'Where does the constitutional boundary lie between legitimate religious personal-law autonomy under Articles 25/26 and impermissible gender-discriminatory extraction that the state is obligated to remedy under Article 14/15 equality guarantees?',
    'Track the trajectory of judicial and legislative intervention (Shah Bano 1985, Shayara Bano 2017, Muslim Women Act 2019, and any future uniform civil code litigation) to see whether the boundary is moving toward codified gender-equity floors within personal law, or holding at the current partial-intervention equilibrium.',
    'If the boundary continues moving toward mandated gender-equity floors, additional components of this reading (polygamy, inheritance shares) may reach the same live/dead founding-problem split that unilateral talaq already reached in 2017/2019, shifting the classification further toward snare for those components specifically. If political resistance halts further intervention, the tangled_rope classification remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_autonomy_vs_state_intervention_boundary, conceptual, 'Ongoing contest over the constitutional line between religious personal-law autonomy and state anti-discrimination obligation.').

omega_variable(
    board_authority_representativeness,
    'Do the Muslim personal law boards and qazi networks that set the interpretive agenda genuinely represent the interests of Muslim women and junior family members, or do they represent a self-selected male religious leadership whose interpretive choices are contestable even within Islamic jurisprudence (as reformist scholars and organizations like BMMA argue)?',
    'Comparative analysis of alternative Islamic jurisprudential traditions (including reformist and cross-national Muslim-majority country reforms to talaq, polygamy, and inheritance law) that reach different interpretive outcomes from the same textual sources, indicating the current reading is one contestable interpretation rather than the singular authoritative one.',
    'If the current interpretation is shown to be one contestable choice among viable alternatives within the tradition itself, the extraction is better characterized as a governance/representation failure within the reading rather than an irreducible feature of Shariat-derived authority as such — this would sharpen rather than dissolve the tangled_rope classification, since it would show the coordination function does not require the current extractive terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_authority_representativeness, conceptual, 'Whether the specific extractive terms are inherent to Shariat authority or a contestable interpretive choice within it.').

omega_variable(
    exit_option_practical_availability,
    'How much of the measured suppression on Muslim wives, daughters, and junior wives is structural (legal/economic barriers to civil court access) versus internalized (social and religious identity commitments that make invoking secular authority against family/community feel illegitimate even when legally available)?',
    'Post-exit trajectory studies: track women who did successfully invoke civil court remedies (e.g., under the 2019 Act) to see whether suppression (social sanction, family estrangement, community exclusion) persists after the legal barrier is formally removed.',
    'If suppression persists strongly post-exit, the effective suppression borne by targets is higher than the structural/legal measure alone suggests, since women carry social suppression with them even after successfully exiting through the available civil remedy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_option_practical_availability, empirical, 'Structural vs. internalized suppression mechanism affecting practical exit for Muslim wives and daughters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1937, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1955, 0.17).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(marr_tr_t2017, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t1937, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1937, 0.42).
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1955, 0.46).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1985, 0.54).
narrative_ontology:measurement(marr_be_t2005, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2005, 0.56).
narrative_ontology:measurement(marr_be_t2017, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1937, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1955, 0.45).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(marr_su_t2005, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(marr_su_t2017, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__muslim_shariat_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five siblings decomposing the natural-language concept 'marriage/family law authority in India' per the epsilon-invariance principle. Each sibling reading (Hindu codified, Christian canonical, Muslim Shariat, Parsi communal, secular civil) has a distinct epsilon, distinct beneficiary/victim structure, and its own classification because the underlying kernel-adjudicating institution and gender-equity outcomes differ structurally across readings. The secular_civil_reading is expected to show markedly lower extractiveness (individual-rights-grounded, gender-neutral by design) and functions as an available, though socially costly, exit option referenced in several stakeholder situations above.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__muslim_shariat_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
